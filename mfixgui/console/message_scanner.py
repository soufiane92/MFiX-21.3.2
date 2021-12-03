"""Module for parsing solver messages from the console (solver output), and
dispatching them to MessageDialog when appropriate

"""

from PyQt5.QtCore import QTimer

from mfixgui.console.solver_message import parse_reinit_fail_message, SolverLogMessage


class MessageScanner:
    """ Class to parse text written to the console and display popups for solver error and warning messages """

    def __init__(self, mfixgui, console_printer):
        self.scan_linebuf = []
        self.mfixgui = mfixgui
        self.console_printer = console_printer
        self.timer = QTimer()
        self.timer.setSingleShot(True)
        self.timer.timeout.connect(self.handle_show_messages)

    def add_line(self, line):
        # Stop shouting!
        line = line.replace("INFO", "Info")
        line = line.replace("MESSAGE", "Message")
        line = line.replace("STATUS", "Status")
        line = line.replace("WARNING", "Warning")
        line = line.replace("ERROR", "Error")
        self.scan_linebuf.append(line)
        # Wait a little while for more lines, so we don't make too many popups
        if not self.timer.isActive():
            self.timer.start(300)
        # But don't wait to reset the mtime on a failed reinit (see comments in handle_reinit)
        if parse_reinit_fail_message(line):
            self.mfixgui.job_manager.reset_mtime()

    def handle_show_messages(self):
        messages = self.pop_messages()
        if messages:
            self.console_printer.append_messages(messages)


    def pop_messages(self):
        """ remove & return list of messages ready to be displayed"""
        message_list = []
        lines = self.scan_linebuf
        while True:
            pre_message_lines, message, lines = self.get_message(lines)
            for line in pre_message_lines:
                self.console_printer.print_internal(line, font="Courier")

            if message is not None:
                self.print_message(message)
                message_list.append(message)
                continue

            self.scan_linebuf = lines
            return message_list


    def print_message(self, message):
        """ Format and print line to console in GUI """
        lines = [line.rstrip() for line in message.lines]
        for line in lines:
            loc = self.console_printer.print_line(line, message.color, font="Courier")
            if message.location is None: # Record location so we can jump to message
                message.location = loc


    def get_message(self, lines):
        """ return (<lines before message>, <message>, <lines after message>) if there
        is a complete message, or ([], None, <lines>) if no message or a
        partial message. """

        open_delim = ">>>>>"
        close_delim = "<<<<<"

        for lineno, line in enumerate(lines):
            if line.lstrip().startswith(open_delim):
                pre_message_lines = lines[:lineno]
                for close_index in range(lineno + 1, len(lines)):
                    message_lines = lines[lineno + 1 : close_index]
                    if lines[close_index].lstrip().startswith(close_delim):
                        remaining_lines = lines[close_index + 1 :]
                        break
                    if lines[close_index].lstrip().startswith(open_delim):
                        self.mfixgui.error("Solver log message not formatted properly.\n"
                                           + ''.join(lines[lineno:]),
                                           popup=True)
                        remaining_lines = lines[close_index:]
                        break
                else:
                    continue
                return (pre_message_lines,
                        SolverLogMessage(self.mfixgui, message_lines),
                        remaining_lines)

        pre_message_lines = lines
        return pre_message_lines, None, []
