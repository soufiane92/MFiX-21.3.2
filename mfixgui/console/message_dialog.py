from qtpy.QtCore import Qt
from qtpy.QtGui import QFontMetrics

from mfixgui.tools.qt import get_ui, get_icon
from mfixgui.console.solver_message import SolverLogMessage, INFO, WARNING, ERROR

def strip_html(line):
    for (pat, rep) in (('&gt;', '>'),
                       ('&lt;', '<'),
                       ('<br>', ' '),
                       ('<b>', ''),
                       ('</b>', ''),
                       ('<i>', ''),
                       ('</i>', ''),
                       ('<pre>', ''),
                       ('</pre>', '')):
        line = line.replace(pat, rep)
    return line



class MessageDialog:
    def __init__(self, project, keyword_doc, console_printer):
        self.project = project
        self.keyword_doc = keyword_doc
        self.ui = get_ui("message_dialog.ui")
        self.ui.min_height = None
        self.ui.setWindowTitle("MFiX solver messages") # This is in the .ui file, do we need to set it here?
        self.ui.drop_button.clicked.connect(self.drop_keyword)
        self.ui.edit_button.clicked.connect(self.edit_keyword)
        self.console_printer = console_printer
        self.ui.lineedit_keyword.textEdited.connect(lambda arg:
                                    self.ui.edit_button.setEnabled(True))

        self.error_tab = MessageTab(self,
                                    console_printer,
                                    "Error",
                                    (self.ui.error_tab,
                                     self.ui.label_error_source,
                                     self.ui.label_error_count,
                                     self.ui.first_error_button,
                                     self.ui.prev_error_button,
                                     self.ui.next_error_button,
                                     self.ui.last_error_button,
                                     self.ui.error_textbrowser))

        self.warning_tab = MessageTab(self,
                                      console_printer,
                                      "Warning",
                                      (self.ui.warning_tab,
                                       self.ui.label_warning_source,
                                       self.ui.label_warning_count,
                                       self.ui.first_warning_button,
                                       self.ui.prev_warning_button,
                                       self.ui.next_warning_button,
                                       self.ui.last_warning_button,
                                       self.ui.warning_textbrowser))

        self.message_tab = MessageTab(self,
                                      console_printer,
                                      "Message",
                                      (self.ui.message_tab,
                                       self.ui.label_message_source,
                                       self.ui.label_message_count,
                                       self.ui.first_message_button,
                                       self.ui.prev_message_button,
                                       self.ui.next_message_button,
                                       self.ui.last_message_button,
                                       self.ui.message_textbrowser))

        self.ui.tabwidget.currentChanged.connect(self.tab_changed)
        self.ui.keyReleaseEvent = self.handle_key_event
        self.current_tab = None
        self.ui.installEventFilter(self.ui)

    def tab_changed(self, idx):
        if idx == 0:
            self.current_tab = self.error_tab
        elif idx == 1:
            self.current_tab = self.warning_tab
        elif idx == 2:
            self.current_tab = self.message_tab
        else:
            self.current_tab = None
        self.update()
        if self.current_tab:
            self.current_tab.update()


    def handle_key_event(self, ev):
        if self.current_tab:
            self.current_tab.handle_key_event(ev)

    def show_errors(self):
        self.ui.tabwidget.setCurrentWidget(self.ui.error_tab)
        self.current_tab = self.error_tab
        self.current_tab.update()
        self.update()
        self.show()

    def show_warnings(self):
        self.ui.tabwidget.setCurrentWidget(self.ui.warning_tab)
        self.current_tab = self.warning_tab
        self.current_tab.update()
        self.update()
        self.show()

    def show_messages(self):
        self.ui.tabwidget.setCurrentWidget(self.ui.message_tab)
        self.current_tab = self.message_tab
        self.current_tab.update()
        self.update()
        self.show()

    def add_message(self, message: SolverLogMessage):
        if message.level == ERROR:
            self.error_tab.add(message)
        elif message.level == WARNING:
            self.warning_tab.add(message)
        elif message.level == INFO:
            self.message_tab.add(message)
        else:
            raise ValueError("Invalid log level")
        self.update()

    def clear_messages(self):
        self.error_tab.clear()
        self.warning_tab.clear()
        self.message_tab.clear()

    def show(self):
        self.ui.show()
        self.ui.activateWindow()
        self.ui.raise_()
        self.update()
        if self.current_tab:
            self.current_tab.update()

    def close(self):
        self.ui.close()

    def drop_keyword(self):
        et = self.error_tab
        err = et.current_message()
        if not err:
            return
        err.drop_keyword()
        et.messages.remove(err)
        self.console_printer.errors.remove(err)
        self.console_printer.update()
        if et.message_index is not None and et.message_index >= len(et.messages):
            if et.messages:
                et.message_index = len(et.messages) - 1
            else:
                et.message_index = None
                self.ui.close()
                return
        self.update()

    def edit_keyword(self):
        et = self.error_tab
        err = self.error_tab.current_message()
        if not err:
            return
        new_text = self.ui.lineedit_keyword.text().split("#", 1)[0]
        err.edit_keyword(new_text)
        et.messages.remove(err)
        self.console_printer.errors.remove(err)
        self.console_printer.update()
        if et.message_index is not None and et.message_index >= len(et.messages):
            if et.messages:
                et.message_index = len(et.messages) - 1
            else:
                et.message_index = None
                self.ui.close()
                return

        self.update()

    def update(self):
        if self.current_tab == self.error_tab:
            err = self.error_tab.current_message()
            if err is not None:
                self.show_keyword_error(err)
                self.ui.error_textbrowser.setText(err.text)

    def show_keyword_error(self, err):
        kwerr = err.keyword_error()
        if kwerr is None:
            self.ui.dropedit_widget.setVisible(False)
            return

        self.ui.dropedit_widget.setVisible(True)
        ignore, line = kwerr
        self.ui.lineedit_keyword.setText(line.strip())

        self.ui.edit_button.setEnabled(False) # enabled on edit



class MessageTab:
    def __init__(self, parent, console_printer, name, ui_widgets):
        (self.tab,
         self.source_label,
         self.count_label,
         self.first_button,
         self.prev_button,
         self.next_button,
         self.last_button,
         self.text_browser) = ui_widgets # FIXME

        self.parent = parent
        self.console_printer = console_printer
        self.name = name
        self.message_index = None
        self.messages = []
        self.first_button.clicked.connect(self.first_message)
        self.first_button.setIcon(get_icon('first.svg'))
        self.first_button.setText('')
        self.first_button.setToolTip("Go to first %s" % name.lower())
        self.prev_button.clicked.connect(self.prev_message)
        self.prev_button.setIcon(get_icon('left.svg'))
        self.prev_button.setText('')
        self.prev_button.setToolTip("Go to previous %s" % name.lower())
        self.next_button.clicked.connect(self.next_message)
        self.next_button.setIcon(get_icon('right.svg'))
        self.next_button.setText('')
        self.next_button.setToolTip("Go to next %s" % name.lower())
        self.last_button.clicked.connect(self.last_message)
        self.last_button.setIcon(get_icon("last.svg"))
        self.last_button.setText('')
        self.last_button.setToolTip("Go to last %s" % name.lower())
        self.source_label.mouseReleaseEvent = self.handle_mouse

    def handle_mouse(self, ev):
        gui = self.console_printer.mfixgui
        loc = self.source_label.text()
        if loc:
            file, line = loc.split(':', 1)
            try:
                line = int(line)
                gui.change_mode('editor')
                gui.editor_widget.show_location(file, int(line))
            except ValueError as e:
                print(e)

    def handle_key_event(self, ev):
        key = ev.key()
        mod = ev.modifiers()
        if key in (Qt.Key_Tab,): # Cycle through message types
            if self.parent.current_tab == self.parent.error_tab:
                if self.parent.warning_tab.messages:
                    self.parent.show_warnings()
                elif self.parent.message_tab.messages:
                    self.parent.show_messages()
            elif self.parent.current_tab == self.parent.warning_tab:
                if self.parent.message_tab.messages:
                    self.parent.show_messages()
                elif self.parent.error_tab.messages:
                    self.parent.show_errors()
            elif self.parent.current_tab == self.parent.message_tab:
                if self.parent.error_tab.messages:
                    self.parent.show_errors()
                elif self.parent.warning_tab.messages:
                    self.parent.show_warnings()
        elif key in (Qt.Key_Left, Qt.Key_Up):
            self.prev_message()
        elif key in (Qt.Key_Right, Qt.Key_Down):
            self.next_message()
        elif key in (Qt.Key_PageUp, Qt.Key_Home):
            self.first_message()
        elif key in (Qt.Key_PageDown, Qt.Key_End):
            self.last_message()
        ev.accept()


    def first_message(self):
        if not self.messages:
            return
        self.message_index = 0
        self.update()
        self.parent.update()

    def prev_message(self):
        if not self.messages:
            return
        if self.message_index > 0:
            self.message_index -= 1
            self.parent.autoscroll = False
            self.update()
            self.parent.update()

    def next_message(self):
        if self.message_index is None:
            return
        if self.message_index < len(self.messages)-1:
            self.message_index += 1
            self.parent.autoscroll = False
            self.update()
            self.parent.update()

    def last_message(self):
        if not self.messages:
            return
        self.message_index = len(self.messages) - 1
        self.parent.autoscroll = False
        self.update()
        self.parent.update()

    def add(self, message):
        self.messages.append(message)
        if self.message_index is None:
            self.message_index = len(self.messages) - 1
        self.update()

    def clear(self):
        self.messages.clear()
        self.message_index = None
        self.update()

    def current_message(self):
        if self.message_index is None:
            return None
        return self.messages[self.message_index]

    def update(self):
        message = self.current_message()
        tb = self.text_browser
        if message is not None:
            first_line = message.text.splitlines()[0].strip()
            src = first_line.split(' ')[-1]
            if src.startswith('<pre>'):
                src = src[5:]
            self.source_label.setText(src)
            text =  message.text[len(first_line)+1:]
            width = 200 + QFontMetrics(tb.currentFont(), tb).size(0, strip_html(text)).width()
            tb.setHtml('<pre>'+text) # Should we use plain-text?

            if self.parent.ui.minimumWidth() < width:
                self.parent.ui.setMinimumWidth(width)
            self.parent.ui.updateGeometry()
            self.console_printer.scroll_to_message(message)
            #self.console_printer.autoscroll = False

        message_count = len(self.messages)
        if message_count:
            self.count_label.setText(f"{self.name} {1+self.message_index} of {message_count}")
        self.tab.setEnabled(bool(message_count))
        self.next_button.setEnabled(self.message_index is not None and
                                    self.message_index < message_count-1)
        self.last_button.setEnabled(self.next_button.isEnabled())
        self.prev_button.setEnabled(self.message_index is not None and
                                    self.message_index > 0)
        self.first_button.setEnabled(self.prev_button.isEnabled())
