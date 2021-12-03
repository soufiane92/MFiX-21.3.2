"""
widgets for the code_editor.
"""

from qtpy.QtWidgets import (QWidget, QVBoxLayout)
from qtpy.QtGui import (QTextCursor, QTextDocument)
from qtpy.QtCore import (QSize, Qt, QRegExp)
from mfixgui.tools.qt import get_ui, get_icon, get_pixmap


class LineNumberArea(QWidget):
    def __init__(self, editor):
        QWidget.__init__(self, editor)
        self.editor = editor

    def sizeHint(self):
        return QSize(self.editor.line_number_area_width(), 0)

    def paintEvent(self, event):
        self.editor.line_number_area_paint(event)


class FindReplaceWidget(QWidget):
    def __init__(self, parent, editor):
        QWidget.__init__(self, parent)
        self.editor = editor

        self.prev_search_str = ''
        self.matches = False
        self.lastMatch = None

        ui = self.ui = get_ui('find_widget.ui')
        layout = QVBoxLayout(self)
        layout.addWidget(self.ui)
        layout.setContentsMargins(0, 0, 0, 0)

        ui.use_regex.setIcon(get_icon('regex'))
        ui.match_case.setIcon(get_icon('letter_case'))

        ui.close.setIcon(get_icon('close'))
        ui.close.clicked.connect(self.close)

        ui.find.clicked.connect(self.find)
        ui.replace.clicked.connect(self.replace)
        ui.replace_all.clicked.connect(self.replace_all)

        ui.find_text.returnPressed.connect(self.find)

    def close(self):
        self.setVisible(False)

    def set_focus(self, read_only=False):
        ui = self.ui
        self.setVisible(True)

        cursor = self.editor.textCursor()
        if cursor.hasSelection():
            text = cursor.selectedText()
            ui.find_text.setText(text)

        for w in [ui.replace_text, ui.replace, ui.replace_all]:
            w.setVisible(not read_only)

        ui.find_text.setFocus()

    def move_cursor(self, start, end):

        cursor = self.editor.textCursor()
        cursor.setPosition(start)
        cursor.movePosition(QTextCursor.Right, QTextCursor.KeepAnchor,end - start)
        self.editor.setTextCursor(cursor)

    def find(self):
        ui = self.ui

        text = ui.find_text.text()
        match_case = ui.match_case.isChecked()
        whole_word = ui.whole_word.isChecked()

        cursor_pos = self.editor.textCursor().position()

        # use regex
        if ui.use_regex.isChecked():
            text = QRegExp(text, Qt.CaseSensitive if match_case else Qt.CaseInsensitive)

        flags = None
        if match_case and whole_word:
            flags = QTextDocument.FindCaseSensitively|QTextDocument.FindWholeWords
        elif match_case:
            flags = QTextDocument.FindCaseSensitively
        elif whole_word:
            flags = QTextDocument.FindWholeWords

        def find(text, flags):
            if flags is not None:
                match = self.editor.find(text, flags)
            else:
                match = self.editor.find(text)
            return match
        match = find(text, flags)

        # check for a match
        if not match:
            if self.matches and self.prev_search_str == text:
                # go back to beginning
                self.editor.moveCursor(QTextCursor.Start)
                find(text, flags)
            elif self.prev_search_str != text:
                self.editor.moveCursor(QTextCursor.Start)
                if not find(text, flags):
                    cursor = self.editor.textCursor()
                    cursor.setPosition(cursor_pos, QTextCursor.MoveAnchor)
                    self.editor.setTextCursor(cursor)

        self.prev_search_str = text
        self.matches = match

    def replace(self):
        cursor = self.editor.textCursor()

        if cursor.hasSelection():
            cursor.insertText(self.ui.replace_text.text())
            self.editor.setTextCursor(cursor)
            self.find()

    def replace_all(self):

        self.editor.moveCursor(QTextCursor.Start)
        self.find()
        while self.matches:
            self.replace()


class ReloadWidget(QWidget):
    def __init__(self, parent=None, editor=None):
        QWidget.__init__(self, parent)

        self.editor = editor

        ui = self.ui = get_ui('reload_file_widget.ui')
        layout = QVBoxLayout(self)
        layout.addWidget(self.ui)
        layout.setContentsMargins(0, 0, 0, 0)

        ui.label_warning.setPixmap(get_pixmap('error_outline',  24, 24))

        ui.pushbutton_reload.clicked.connect(self.reload)
        ui.toolButton_cancel.clicked.connect(lambda:self.setVisible(False))
        ui.toolButton_cancel.setIcon(get_icon('close'))

    def show(self):
        text = 'This file has changed on disk. Reload file and lose current changes?'
        self.ui.label.setText(text)
        QWidget.show(self)

    def reload(self):
        self.parent().reload()
        self.hide()


class CopyToProjectWidget(QWidget):
    def __init__(self, parent=None):
        QWidget.__init__(self, parent)

        self.editor_widget = parent

        ui = self.ui = get_ui('copy_to_project.ui')
        layout = QVBoxLayout(self)
        layout.addWidget(self.ui)
        layout.setContentsMargins(0, 0, 0, 0)

        ui.label_info.setPixmap(get_pixmap('info',  24, 24))
        ui.pushbutton_copy.clicked.connect(self.copy)

    def copy(self):
        self.editor_widget.copy_to_project()


class InfoBarWidget(QWidget):
    def __init__(self, parent=None, editor=None):
        QWidget.__init__(self, parent)

        self.editor = editor

        ui = self.ui = get_ui('editor_info_bar.ui')
        layout = QVBoxLayout(self)
        layout.addWidget(self.ui)
        layout.setContentsMargins(0, 0, 0, 0)

        ui.toolButton_line.clicked.connect(parent.goto)

    def set_path(self, text):
        self.ui.label_path.setText(text)
        self.ui.label_path.setToolTip(text)

    def set_language(self, text):
        self.ui.label_language.setText(text)

    def set_position(self, row, col):
        self.ui.toolButton_line.setText('{}:{}'.format(row, col))
