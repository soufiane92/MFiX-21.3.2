'''
Simple text editor with line numbers, syntax highlighting, goto, and
search/replace.
Mostly follows the Qt C++ example:
    https://doc.qt.io/qt-5/qtwidgets-widgets-codeeditor-example.html
Inspiration from:
    https://www.binpress.com/building-text-editor-pyqt-3/

'''
import os
import re

from qtpy.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout, QPlainTextEdit,
    QLabel, QTextEdit, QFileDialog, QShortcut, QInputDialog, QListWidget,
    QAction, QMenu, QApplication)
from qtpy.QtGui import (QFont, QFontMetrics, QTextFormat, QPainter, QTextOption,
    QKeySequence, QTextCursor, QTextDocument, QFontDatabase)
from qtpy.QtCore import (QSize, QRect, Qt, Signal, QRegExp, QStringListModel,
    QTimer, QFileSystemWatcher)

from mfixgui.tools.qt import get_ui, get_icon, get_pixmap
from mfixgui.editor.constants import DEFAULT_COLOR_SCHEME, OPERATORS, DELIMITERS, LANGUAGE_DEFAULTS
from mfixgui.editor.syntax_highlighter import get_syntax_highlighter
from mfixgui.editor.completer import get_completer
from mfixgui.editor.widgets import (FindReplaceWidget, ReloadWidget,
    InfoBarWidget, LineNumberArea, CopyToProjectWidget)


def get_language(ext):
    if ext.startswith('.'):
        ext = ext[1:]
    for lang, value in LANGUAGE_DEFAULTS.items():
        if ext in value['exts']:
            return lang
    return 'text'

def insert_append_action(menu, action, insert=None):
    if insert:
        menu.insertAction(insert, action)
    else:
        menu.addAction(action)

class CodeEditorWidget(QWidget):
    def __init__(self, parent=None):
        QWidget.__init__(self, parent)

        self.ide_widget = parent
        self.editor = CodeEditor(self)
        self.editor.changed_on_disk.connect(self.show_reload)
        self.find_widget = FindReplaceWidget(self, self.editor)
        self.find_widget.setVisible(False)
        self.reload_widget = ReloadWidget(self, self.editor)
        self.reload_widget.setVisible(False)
        self.info_bar = InfoBarWidget(self, self.editor)
        self.copy_project_widget = CopyToProjectWidget(self)
        self.copy_project_widget.setVisible(False)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)
        layout.addWidget(self.reload_widget)
        layout.addWidget(self.copy_project_widget)
        layout.addWidget(self.editor)
        layout.addWidget(self.find_widget)
        layout.addWidget(self.info_bar)

        # signals
        self.editor.language_changed.connect(self.info_bar.set_language)
        self.editor.cursor_changed.connect(self.info_bar.set_position)

        # keyboard shortcuts
        shortcuts = [
            ('Ctrl+S', self.save),
            ('Ctrl+F', self.find),
            ('Ctrl+G', self.goto),
            ('Esc', self.close_dialogs),
            ]

        for key, callback in shortcuts:
            q = QShortcut(QKeySequence(key), self)
            q.activated.connect(callback)

    def open(self, fname=None, read_only=False):
        self.editor.open(fname, read_only)
        self.info_bar.set_path(os.path.abspath(fname))

    def save(self, fname=None):
        self.reload_widget.hide()
        # Emitting the signal here results in a double reload.  Our own
        # file watcher catches the file changed event, even when it's us
        # who is writing the file - cgw
        #self.ide_widget.file_changed.emit(self.editor.file_name)
        return self.editor.save(fname)

    def reload(self):
        self.editor.open(self.editor.file_name, self.editor.isReadOnly())
        self.ide_widget.file_changed.emit(self.editor.file_name)
        self.reload_widget.hide()

    def copy_to_project(self):
        self.ide_widget.copy_to_project(self.editor.file_name)

    def find(self):
        self.find_widget.set_focus(self.editor.isReadOnly())

    def goto(self):

        l, ok = QInputDialog.getText(self, "Goto line", "Enter a row to go there")
        if not ok:
            return

        try:
            l = int(l)
        except ValueError:
            return

        self.editor.goto(l)

    def show_reload(self):
        if self.editor.unsaved_flag:
            self.reload_widget.show()
        else:
            self.reload()

    def close_dialogs(self):

        if self.editor.completer.isVisible():
            self.editor.completer.hide()

        else:
            for w in [self.find_widget]:
                if w.isVisible():
                    w.hide()


class CodeEditor(QPlainTextEdit):
    needsSaved = Signal()
    saved = Signal()
    help_request = Signal() # Should we include the string?
    changed_on_disk = Signal()
    language_changed = Signal(str)
    cursor_changed = Signal(int, int)

    def __init__(self, parent=None):
        QPlainTextEdit.__init__(self, parent)

        self.unsaved_flag = False
        self.loading = False
        self.file_name = None
        self.watch_file_on_disk = True
        self.tab_char = '\t'  # change to spaces to replace tabs with spaces
        self.language = None

        self.line_number_area = LineNumberArea(self)

        # colors
        self.background_color = DEFAULT_COLOR_SCHEME.get('background')
        self.sidearea_color = DEFAULT_COLOR_SCHEME.get('sideareas')
        self.currentline_color = DEFAULT_COLOR_SCHEME.get('currentline')

        self.init_completer()

        # word wrap
        self.setWordWrapMode(QTextOption.NoWrap)

        # signals
        self.blockCountChanged.connect(self.update_line_number_area_width)
        self.updateRequest.connect(self.update_line_number_area)
        self.cursorPositionChanged.connect(self.highlight_current_line)
        self.cursorPositionChanged.connect(self.cursor_moved)
        self.modificationChanged.connect(self.modified)

        # file system watcher
        self.file_watcher = QFileSystemWatcher()
        self.file_watcher.fileChanged.connect(self.file_changed_on_disk)

        # setup language
        self.change_language()

        # context menu
        self.context_menu = self.createStandardContextMenu

    def contextMenuEvent(self, event):
        if self.context_menu is not None:
            menu = self.extend_context_menu()
            menu.exec_(event.globalPos())

    def extend_context_menu(self):
        menu = self.context_menu() # pylint: disable=not-callable
        first_default_action = menu.actions()

        if first_default_action:
            first_default_action = first_default_action[0]

        # help
        help_action = QAction(
            get_icon('question_mark.svg'), 'Help', menu)
        help_action.triggered.connect(self.help_request.emit)
        insert_append_action(menu, help_action, first_default_action)
        return menu

    def init_completer(self, language=None):
        Completer = get_completer(language)

        # Completer
        self.completer = Completer(self)
        self.completer.complete.connect(self.insert_completion)
        self.update_completer_timer = QTimer()
        self.update_completer_timer.setInterval(1*1000) # milliseconds
        self.update_completer_timer.timeout.connect(self.completer.update_local_list)
        self.update_completer_timer.start()

    def cursor_moved(self):
        tc = self.textCursor()
        y = tc.blockNumber() + 1
        x = tc.columnNumber() + 1
        self.cursor_changed.emit(y, x)

    def insert_completion(self, text):
        tc = self.textCursor()
        tc.clearSelection()
        tc.movePosition(QTextCursor.StartOfWord, QTextCursor.MoveAnchor)
        tc.movePosition(QTextCursor.EndOfWord, QTextCursor.KeepAnchor)
        tc.insertText(text)
        tc.movePosition(QTextCursor.EndOfWord)
        self.setTextCursor(tc)

    def calculate_real_position(self, point):
        """
        Add offset to a point, to take into account the Editor panels.
        This is reimplemented in CodeEditor, in other widgets it returns
        the same point.
        """
        return point

    def text_under_cursor(self):
        tc = self.textCursor()
        tc.select(QTextCursor.WordUnderCursor)
        return tc.selectedText()

    def at_beginning_of_word(self):
        tc = self.textCursor()
        pos = tc.position()
        tc.movePosition(QTextCursor.StartOfWord, QTextCursor.MoveAnchor)
        pos2 = tc.position()
        return pos == pos2

    def keyPressEvent(self, event):
        key = event.key()
        text = event.text().rstrip('\b\n\r\t')

        # determine prefix for the completer. This event is called before the
        # the new key is added to the text edit.
        tc = self.textCursor()
        selected_text = tc.selectedText().rstrip('\b\n\r\t')
        prefix = self.text_under_cursor()
        if key == Qt.Key_Backspace:
            prefix = prefix.replace(selected_text, '')
        else:
            prefix += text

        # determine modifiers
        alt = event.modifiers() & Qt.AltModifier
        shift = event.modifiers() & Qt.ShiftModifier
        ctrl = event.modifiers() & Qt.ControlModifier
        modifier = shift or ctrl or alt

        completer_visible = self.completer.isVisible()
        selection = tc.hasSelection()

        # if completer is visible
        if completer_visible:
            if key in [Qt.Key_Escape, Qt.Key_Tab, Qt.Key_Backtab]:
                event.ignore()
                return

        # cut line
        elif not selection and ctrl and key == Qt.Key_X:
            tc.select(QTextCursor.BlockUnderCursor)
            self.setTextCursor(tc)

        # handle back tab (Shift + Tab) since Qt does not "unindent"
        elif key == Qt.Key_Backtab:
            self.handle_back_tab()
            return

        # don't pop the completer up
        elif key in [Qt.Key_Return, Qt.Key_Enter, Qt.Key_Up, Qt.Key_Down,
                     Qt.Key_Left, Qt.Key_Right,
                     Qt.Key_PageUp, Qt.Key_PageDown, Qt.Key_Home, Qt.Key_End,
                     Qt.Key_CapsLock] or modifier:
            pass

        # pop the completer up!
        elif not completer_visible and len(prefix) and \
                (key in [Qt.Key_Tab] or (len(prefix) > 2 and len(text))) and \
                len(prefix) > 0 and prefix[-1] not in OPERATORS and \
                prefix[-1] not in DELIMITERS and not selection and \
                not self.at_beginning_of_word():

            self.completer.show_list(prefix)
            if key in [Qt.Key_Tab]:
                return

        # replace tab with tab_char
        elif key == Qt.Key_Tab:
            self.handle_tab()
            return

        # propagate event so text gets added
        QPlainTextEdit.keyPressEvent(self, event)

    def handle_tab(self):
        """Handle tabs"""
        tc = self.textCursor()
        if tc.hasSelection():
            tc.movePosition(QTextCursor.StartOfLine)
        tc.insertText(self.tab_char)

    def handle_back_tab(self):
        """
        The base QTextEdit does not handle backtabs correctly, this method will
        unindent the line if Shift + Tab (Backtab) is pressed.
        """
        # get current cursor
        tc = self.textCursor()
        tc.clearSelection()

        # move to beginning of line and select text to first word
        tc.movePosition(QTextCursor.StartOfLine)
        tc.movePosition(QTextCursor.NextWord, QTextCursor.KeepAnchor)
        sel_text = tc.selectedText()

        # if the text starts with the tab_char, replace it
        if sel_text.startswith(self.tab_char):
            text = sel_text.replace(self.tab_char, '', 1)
            tc.insertText(text)

    def modified(self):
        self.unsaved_flag = True
        if not self.loading:
            self.needsSaved.emit()

    def set_syntax_highlighter(self, language):

        # get the os default system font
        font = QFontDatabase.systemFont(QFontDatabase.FixedFont)

        sh = get_syntax_highlighter(language)
        self.syntax_highlighter = sh(self.document(), font)
        text_format = self.syntax_highlighter.formats['normal']
        self.setCurrentCharFormat(text_format)
        self.completer.language_list = self.syntax_highlighter.keywords + self.syntax_highlighter.builtins

        font = self.font = text_format.font()
        self.font_metrics = QFontMetrics(font)
        self.completer.setFont(font)

    def change_language(self, language=None):

        if language is None:
            if self.file_name is not None:
                language = get_language(os.path.splitext(self.file_name)[1])
            else:
                language = 'text'

        if language == self.language:
            # already set, return
            return

        self.language = language

        # syntax highlighter
        self.set_syntax_highlighter(language)
        self.tab_char = LANGUAGE_DEFAULTS[language]['tab_char']
        self.language_changed.emit(language)

    def line_number_area_width(self):
        m = max(1 , self.blockCount())
        digits = len(str(m))
        dpi = min(self.logicalDpiX(),
                  self.logicalDpiY())
        r =  3 + self.font_metrics.maxWidth() * digits
        r *= dpi / 96 # issues/939. disable this if using high-DPI scaling, see gui.py
        return r


    def update_line_number_area_width(self):
        self.setViewportMargins(self.line_number_area_width(), 0, 0, 0)

    def update_line_number_area(self, rect, dy):
        if dy:
            self.line_number_area.scroll(0, dy)
        else:
            self.line_number_area.update(0, rect.y(), self.line_number_area.width(), rect.height())

        if rect.contains(self.viewport().rect()):
            self.update_line_number_area_width()

    def resizeEvent(self, event):
        QPlainTextEdit.resizeEvent(self, event)
        cr = self.contentsRect()
        self.line_number_area.setGeometry(QRect(cr.left(), cr.top(), self.line_number_area_width(), cr.height()));

    def highlight_current_line(self):
        extraSelections = self.extraSelections()
        extraSelections.clear()
        if not self.isReadOnly():
            selection = QTextEdit.ExtraSelection()

            selection.format.setBackground(self.currentline_color)
            selection.format.setProperty(QTextFormat.FullWidthSelection, True)
            selection.cursor = self.textCursor()
            selection.cursor.clearSelection()
            extraSelections.append(selection)

        self.setExtraSelections(extraSelections)

    def line_number_area_paint(self, event):

        painter = QPainter(self.line_number_area)
        painter.fillRect(event.rect(), self.sidearea_color)

        block = self.firstVisibleBlock()
        blockNumber = block.blockNumber()
        top = self.blockBoundingGeometry(block).translated(self.contentOffset()).top()
        bottom = top + self.blockBoundingRect(block).height()

        while block.isValid() and top <= event.rect().bottom():
            if block.isVisible() and bottom >= event.rect().top():
                painter.setPen(self.syntax_highlighter.color_scheme['comment'])
                painter.setFont(self.font)
                painter.drawText(0, top, self.line_number_area.width(), self.fontMetrics().height(),
                                 Qt.AlignRight, str(blockNumber + 1))
            block = block.next()
            top = bottom
            bottom = top + self.blockBoundingRect(block).height()
            blockNumber += 1

    def goto(self, line, column=0):
        cursor = QTextCursor(self.document().findBlockByLineNumber(line-1))
        for x in range(column-1):
            cursor.movePosition(cursor.Right) #Note, passing count to movePosition doesn't work
        self.setTextCursor(cursor)
        self.setFocus()

    def open(self, fname, read_only=False):
        self.loading = True
        self.file_name = fname

        try:
            with open(fname, encoding='utf-8', errors='replace') as f:
                text = f.read()
            self.setPlainText(text)
            self.setReadOnly(read_only)
        except Exception as e:
            # TODO: handle read failures
            # log.error("error opening %s: %s" % (fname, e))
            text = ""

        # set syntax highlighter
        self.change_language()

        # add path to file watcher
        if os.path.exists(fname):
            self.file_watcher.addPath(fname)

        self.loading = False
        self.unsaved_flag = False

    def file_changed_on_disk(self, fname):
        if self.watch_file_on_disk:
            self.changed_on_disk.emit()

    def save(self, fname=None):

        if fname is None:
            fname = self.file_name
        if fname is None:
            fname = QFileDialog.getSaveFileName(self)[0]
            if not fname:
                return False
            self.file_name = fname

        # update file watcher
        watch_paths = self.file_watcher.files()
        if watch_paths:
            self.file_watcher.removePaths(watch_paths)

        with open(fname, 'w', encoding='utf-8', errors='ignore') as f:
            f.write(self.toPlainText())

        self.document().setModified(False)
        self.saved.emit()
        self.unsaved_flag = False

        # set syntax highlighter
        self.change_language()

        # update file watcher
        if os.path.exists(fname):
            self.file_watcher.addPath(fname)

        return True
