"""
Completer widget for the code_editor.
"""

import re

from qtpy.QtWidgets import QListWidget
from qtpy.QtGui import QTextCursor
from qtpy.QtCore import Qt, Signal

from mfixgui.editor.constants import OPERATORS, DELIMITERS


def get_completer(language):
    return BaseCompleter


def fuzzyfinder(user_input, collection):
    """
    Stolen from here: https://blog.amjith.com/fuzzyfinder-in-10-lines-of-python
    """
    # remove operators; issue/921
    for char in ['.', '*', '+', '-', '\\', '(', ')']:
        user_input = user_input.replace(char, '')
    suggestions = []
    pattern = '.*?'.join(user_input)   # Converts 'djm' to 'd.*?j.*?m'
    regex = re.compile(pattern, re.IGNORECASE)  # Compiles a regex.
    for item in collection:
        match = regex.search(item)   # Checks if the current item matches the regex.
        if match:
            suggestions.append((len(match.group()), match.start(), item))
    return [x for _, _, x in sorted(suggestions)]


class BaseCompleter(QListWidget):
    complete = Signal(str)
    def __init__(self, text_edit):
        QListWidget.__init__(self, text_edit)

        self.textedit = text_edit
        self.setWindowFlags(Qt.SubWindow | Qt.FramelessWindowHint | Qt.NoDropShadowWindowHint)
        self.hide()
        self.itemActivated.connect(self.emit_complete)
        self.currentRowChanged.connect(self.row_changed)

        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

        self.prefix = ''
        self.local_list = []
        self.language_list = []
        self.match_list = []
        self.word_regex = re.compile("(\w+)")

    def get_matches(self, prefix):
        try:
            matches = fuzzyfinder(prefix, self.match_list)
        except:
            matches = []
        return matches

    def update(self):
        self.match_list = list(set(self.local_list + self.language_list))

    def update_current(self):
        """update current list of matches"""

        matches = self.get_matches(self.prefix)

        self.clear()
        self.addItems(matches)
        self.setCurrentRow(0)
        self.set_size()
        self.move_list()
        if not matches:
            self.hide()

    def update_local_list(self):
        self.local_list = self.word_regex.findall(self.textedit.toPlainText())

        if not self.isVisible():
            self.update()

    def emit_complete(self):
        self.complete.emit(self.current_text())
        self.hide()

    def current_text(self):
        item = self.currentItem()
        if item:
            return item.text()

    def row_changed(self):
        pass

    def set_size(self):
        self.setFixedSize(
            self.sizeHintForColumn(0)+2,
            self.sizeHintForRow(0)*min(self.count(), 10)+2
            )

    def focusOutEvent(self, event):
        self.hide()
        QListWidget.focusOutEvent(self, event)

    def hide(self):
        QListWidget.hide(self)
        self.textedit.setFocus()

    def show_list(self, prefix=None):
        """Show list corresponding to position."""
        self.show()
        self.setFocus()
        self.raise_()

        # update and match
        self.prefix = prefix
        self.update_current()

    def move_list(self):
        tc = self.textedit.textCursor()
        tc.clearSelection()
        tc.select(QTextCursor.WordUnderCursor)
        tc.setPosition(tc.selectionStart())

        textedit_geo = self.textedit.geometry()
        point = self.textedit.cursorRect(tc).bottomRight()

        # add line number area width
        point.setX(point.x() + self.textedit.line_number_area_width())

        # Computing completion widget and its parent right positions
        comp_right = point.x() + self.width()

        # Moving completion widget to the left
        # if there is not enough space to the right
        if comp_right > textedit_geo.width():
            point.setX(point.x() - (self.width() - (textedit_geo.width() - point.x())) - 2)

        # Computing completion widget and its parent bottom positions
        comp_bottom = point.y() + self.height()

        # Moving completion widget above if there is not enough space below
        if comp_bottom > textedit_geo.height():
            point.setY(self.textedit.cursorRect(tc).topRight().y() - self.height())

        self.move(point)

    def text_under_cursor(self):
        tc = self.textedit.textCursor()
        tc.select(QTextCursor.WordUnderCursor)
        return tc.selectedText()

    def keyPressEvent(self, event):
        """Process keypress."""
        text = event.text().rstrip(' \b\n\r\t')
        key = event.key()
        alt = event.modifiers() & Qt.AltModifier
        shift = event.modifiers() & Qt.ShiftModifier
        ctrl = event.modifiers() & Qt.ControlModifier
        modifier = shift or ctrl or alt

        prefix = self.text_under_cursor()
        if key == Qt.Key_Backspace:
            prefix = prefix[:-1]
        else:
            prefix += text
        self.prefix = prefix


        if not len(prefix) > 0:
            self.hide()
            self.textedit.keyPressEvent(event)
        elif key in [Qt.Key_Tab]:
            self.emit_complete()
        elif key == Qt.Key_Backspace:
            self.textedit.keyPressEvent(event)
            self.update_current()
        elif key in [Qt.Key_Escape, Qt.Key_Return, Qt.Key_Enter]:
            self.hide()
            self.textedit.keyPressEvent(event)
        elif key in (Qt.Key_Left, Qt.Key_Right) or (len(text)> 0 and (text[-1] in OPERATORS or text[-1] in DELIMITERS)):
            self.hide()
            self.textedit.keyPressEvent(event)
        elif key in (Qt.Key_Up, Qt.Key_Down, Qt.Key_PageUp, Qt.Key_PageDown,
                     Qt.Key_Home, Qt.Key_End,
                     Qt.Key_CapsLock) and not modifier:
            if key == Qt.Key_Up and self.currentRow() == 0:
                self.setCurrentRow(self.count() - 1)
            elif key == Qt.Key_Down and self.currentRow() == self.count()-1:
                self.setCurrentRow(0)
            else:
                QListWidget.keyPressEvent(self, event)
        elif len(text):
            self.textedit.keyPressEvent(event)
            self.update_current()
        elif modifier:
            self.textedit.keyPressEvent(event)
        else:
            self.hide()
            self.textedit.keyPressEvent(event)
            QListWidget.keyPressEvent(self, event)
