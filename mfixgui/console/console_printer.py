import logging
import os
import re

from qtpy.QtGui import QColor, QTextCharFormat, QTextCursor

from mfixgui.console import MessageScanner
from mfixgui.console.message_dialog import MessageDialog
from mfixgui.version import __version__

from mfixgui.console.solver_message import INFO, WARNING, ERROR

from mfixgui.tools import plural
from mfixgui.tools.qt import sub_icon_size

LOG = logging.getLogger("mfix")


class ConsolePrinter:
    """ ConsolePrinter class handles all printing of GUI and solver messages to
    the GUI console. Also responsible for the (Error) Message popup dialog. """

    def __init__(self, mfixgui):
        self.mfixgui = mfixgui
        ui = self.ui = mfixgui.ui
        self.console = self.ui.console
        self.message_scanner = MessageScanner(mfixgui, self)
        self.message_dialog = MessageDialog(mfixgui.project, mfixgui.keyword_doc, self)
        ui.toolbutton_errors.clicked.connect(self.show_errors)
        ui.toolbutton_warnings.clicked.connect(self.show_warnings)
        ui.toolbutton_messages.clicked.connect(self.show_messages)
        ui.toolbutton_clear.setIconSize(sub_icon_size())
        ui.toolbutton_clear.clicked.connect(self.clear_console)
        ui.toolbutton_scroll_top.setIconSize(sub_icon_size())
        ui.toolbutton_scroll_top.clicked.connect(self.scroll_top)
        ui.toolbutton_scroll_bottom.setIconSize(sub_icon_size())
        ui.toolbutton_scroll_bottom.clicked.connect(self.scroll_bottom)
        self.console.verticalScrollBar().actionTriggered.connect(self.stop_scrolling_maybe)
        self.console.verticalScrollBar().valueChanged.connect(self.slider_value_changed)
        self.errors = []
        self.warnings = []
        self.messages = []
        self.autoscroll = True
        self.update()
        self.console.mouseReleaseEvent = self.show_message_location_in_editor

    def show_message_location_in_editor(self, _event):
        """ Open up the Editor with the source of a solver message """
        cursor = self.console.textCursor()
        cursor.select(QTextCursor.LineUnderCursor)
        text = cursor.selectedText()
        result = self.find_path(text)
        if result is not None:
            path, lineno = result
            self.mfixgui.change_mode("editor")
            self.mfixgui.editor_widget.show_location(path, lineno)

    re_message_location = re.compile(r"(Error|Warning|Message) from ([^:]+):(\d+)(\s)*$", re.IGNORECASE)

    def find_path(self, text):
        """ Parse the location in the solver source where a message comes from """
        match = self.re_message_location.match(text)
        if match is None:
            return None
        return match.group(2), int(match.group(3))

    def show_errors(self):
        self.autoscroll = False
        self.message_dialog.show_errors()

    def show_warnings(self):
        self.autoscroll = False
        self.message_dialog.show_warnings()

    def show_messages(self):
        self.autoscroll = False
        self.message_dialog.show_messages()

    def stop_scrolling_maybe(self, arg):
        # handles key events like scroll to end
        sb = self.console.verticalScrollBar()
        if arg in (sb.SliderSingleStepSub,
                   sb.SliderToMinimum,
                   sb.SliderPageStepSub):
            self.autoscroll = False
        if arg in (sb.SliderToMaximum,):
            self.autoscroll = True
        else:
            self.autoscroll = (sb.sliderPosition() == sb.maximum())

    def slider_value_changed(self, arg):
        # Handles the case where scrollbar is adjusted directly,
        #  which does not cause actionTriggered
        sb = self.console.verticalScrollBar()
        self.autoscroll = (arg == sb.maximum())

    def scroll_top(self):
        scrollbar = self.console.verticalScrollBar()
        scrollbar.setValue(scrollbar.minimum())
        #self.autoscroll = False

    def scroll_bottom(self):
        scrollbar = self.console.verticalScrollBar()
        scrollbar.setValue(scrollbar.maximum())
        #self.autoscroll = True

    def print_welcome(self):
        self.print_internal("Welcome to MFiX - https://mfix.netl.doe.gov", color="blue")
        self.print_internal("MFiX-GUI version %s" % __version__, color="blue")

    def clear_console(self):
        self.console.clear()
        self.clear_messages()

    def clear_messages(self):
        self.message_dialog.close()
        self.message_dialog.clear_messages()
        self.errors.clear()
        self.warnings.clear()
        self.messages.clear()
        self.update()

    def append_messages(self, messages):
        for message in messages:
            if message.level is None:
                continue
            if message.level == ERROR:
                self.errors.append(message)
            elif message.level == WARNING:
                self.warnings.append(message)
            elif message.level == INFO:
                self.messages.append(message)

            self.message_dialog.add_message(message)
            if message.level == ERROR:
                self.message_dialog.show_errors()
        self.update()

    def update(self):
        """ Update state of the console buttons and error dialog """
        ui = self.mfixgui.ui
        tb = self.message_dialog.ui.tabwidget.tabBar()

        text = plural(len(self.errors), 'error')
        style = 'color: red' if self.errors else ''
        ui.toolbutton_errors.setText(text)
        ui.toolbutton_errors.setStyleSheet(style)
        ui.toolbutton_errors.setEnabled(bool(self.errors))
        tb.setTabText(0, text)
        tb.setTabTextColor(0, QColor('red' if self.errors else 'grey'))
        tb.setTabEnabled(0, bool(self.errors))

        text = plural(len(self.warnings), 'warning')
        style = 'color: orangered' if self.warnings else ''
        ui.toolbutton_warnings.setText(text)
        ui.toolbutton_warnings.setStyleSheet(style)
        ui.toolbutton_warnings.setEnabled(bool(self.warnings))
        tb.setTabText(1, text)
        tb.setTabTextColor(1, QColor('orangered' if self.warnings else 'grey'))
        tb.setTabEnabled(1, bool(self.warnings))

        text = plural(len(self.messages), 'message')
        style = 'color: blue' if self.messages else ''
        ui.toolbutton_messages.setText(text)
        ui.toolbutton_messages.setStyleSheet(style)
        ui.toolbutton_messages.setEnabled(bool(self.messages))
        tb.setTabText(2, text)
        tb.setTabTextColor(2, QColor('blue' if self.messages else 'grey'))
        tb.setTabEnabled(2, bool(self.messages))


    def handle_stdout(self, text):
        self.handle_print(text)
        self.mfixgui.update_logger_solver_output(text)

    def handle_stderr(self, text):
        self.print_internal(text, color="red", font="Courier")
        #self.message_scanner.add_line('<<<<TRACEBACK')
        for line in text.split():
            self.message_scanner.add_line(line)
        #self.message_scanner.add_line('>>>>>TRACEBACK')

    def handle_print(self, text):
        """ Collect output from mfix/pymfix process, format and display to the user. """
        lines = text.splitlines(True)
        for line in lines:
            if self.can_skip(line):
                continue
            self.message_scanner.add_line(line)

    def can_skip(self, line):
        """ Filter what text is printed to the GUI console """
        stripped = line.strip()
        lower = stripped.lower()
        if self.is_delim(line):
            return False
        # Skip routine messages that we are not going to trouble the user with
        return (lower == ""
                or all(c in ("*", "_", "<", "=", ">", " ") for c in lower)
                or lower.startswith(("please correct",
                                     "program terminated",
                                     "error stop 1")))

    def is_delim(self, line):
        return line.strip().startswith(('<<<<<', '>>>>>'))

    def print_internal(self, line, color=None, font=None, advance=True):
        if not (self.can_skip(line) or self.is_delim(line)):
            self.print_line(line, color, font, advance)

    def print_line(self, line, color=None, font=None, advance=True):
        """ Format and print line to console in GUI """
        line = strip_html(line)
        if self.can_skip(line):
            return
        # hack. TODO: real message types, map to font/color
        strikeout = font and font.lower() == "strikeout"
        underline = bool(self.re_message_location.match(line))
        log_line(("unset" if strikeout else "") + line.strip())

        if line.lower().startswith("info:"):
            color = "blue"
            line = line[5:].lstrip()

        if advance and not line.endswith(os.linesep):
            line += os.linesep

        cursor = self.get_cursor(line, font, color, strikeout, underline)
        scrollbar = self.console.verticalScrollBar()
        cursor.insertText(line)
        if self.autoscroll:
            scrollbar.setValue(scrollbar.maximum())

        return cursor.position()

    def get_cursor(self, line, font, color, strikeout, underline):
        """ get the cursor object for the console """
        cursor = self.console.textCursor()
        cursor.movePosition(cursor.End)
        char_format = get_char_format(line, font, color, strikeout, underline)
        # char_format.setFontFixedPitch(True) # ?
        cursor.setCharFormat(char_format)
        return cursor

    def scroll_to_message(self, message):
        """ Scroll to message in MessageDialog """
        if message.location is not None:
            cursor = QTextCursor(self.console.document())
            cursor.setPosition(message.location)
            self.console.setTextCursor(cursor)
            self.console.centerCursor()

def strip_html(line):
    """ avoid printing HTML tags """
    for (pat, rep) in (('&gt;', '>'),
                       ('&lt;', '<'),
                       ('<br>', ' '),
                       ('<b>', ''),
                       ('</b>', ''),
                       ('<i>', ''),
                       ('</i>', '')):
        line = line.replace(pat, rep)
    return line


def log_line(message):
    """ Send line to Python Log as well """
    lower = message.lower()
    if lower.startswith("error:"):
        LOG.error(message[6:].lstrip())
    elif lower.startswith("warning:"):
        LOG.warning(message[8:].lstrip())
    elif lower.startswith("info:"):
        LOG.info(message[5:].lstrip())
    else:
        LOG.info(message)


def get_char_format(line, font, color, strikeout=False, underline=False):
    """ create QTextCharFormat for the console cursor """
    char_format = QTextCharFormat()

    color = get_color(line, color)
    if color:
        char_format.setForeground(QColor(color))

    if font:
        if strikeout:  # hack
            char_format.setFontFamily("Monospace")
            char_format.setFontStrikeOut(True)
        else:
            char_format.setFontFamily(font)
            if underline:
                char_format.setFontItalic(True)
                char_format.setFontUnderline(True)

    return char_format


def get_color(line, color):
    lower = line.lower()
    if lower.startswith("error:"):
        return "red"
    if lower.startswith("warning:"):
        return "orangered"
    if lower.startswith("info:"):
        return "blue"
    return color
