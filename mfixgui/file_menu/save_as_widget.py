from os.path import basename, dirname, exists, join, splitext

from PyQt5.QtWidgets import QWidget
from PyQt5 import QtGui, QtCore

from mfixgui.tools import find_project_file
from mfixgui.tools.qt import get_ui
from mfixgui.regexes import re_valid_run_name_qt

class SaveAsWidget(QWidget):
    def __init__(self, gui):
        super(SaveAsWidget, self).__init__()
        get_ui("save_as.ui", widget=self)
        self.gui = gui
        self.lineedit_run_name.textChanged.connect(self.run_name_text_changed)
        self.lineedit_run_name.setValidator(QtGui.QRegExpValidator(
            QtCore.QRegExp(re_valid_run_name_qt)))
        self.save_as_btn.clicked.connect(self.save_as)
        self.setObjectName("save_as")

    def create_save_as_project(self, item):
        if not self.gui.check_unsaved_abort():
            project_file = find_project_file(item.full_path)
            self.gui.open_save_as_from_template(project_file)

    def show(self):
        """Called when switching to the SaveAs widget"""
        current_run_name = self.gui.project.get_value('run_name')
        self.lineedit_run_name.setText(current_run_name)
        self.run_name_text_changed()

    def run_name_text_changed(self):
        """Called when new run_name text changes """
        new_run_name = self.lineedit_run_name.text()
        old_run_name = self.gui.project.get_value('run_name')
        can_save = bool(new_run_name) and new_run_name != old_run_name
        self.save_as_btn.setEnabled(can_save)

        project_dir = self.gui.get_project_dir()
        if new_run_name is not None and project_dir is not None:
            new_filename = join(project_dir, new_run_name + ".mfx")
            self.label_filename.setText(new_filename)

    def save_as(self):
        """Prompt user for new filename, save project to that file and make
        it the active project"""

        ### TODO what if there is a paused job?

        new_file = self.label_filename.text()
        if not new_file:
            return
        new_dir = dirname(new_file)
        if not self.gui.check_writable(new_dir):
            return

        ok_to_write = self.gui.check_writable(new_dir)
        if not ok_to_write:
            return

        if exists(new_file) and not self.gui.confirm_clobber(new_file):
            return

        # Force run name to file name.  Is this a good idea?
        run_name = splitext(basename(new_file))[0].replace(" ", "_")
        # See re_valid_run_name_qt
        # Don't allow '-' at start
        while run_name and run_name.startswith("-"):
            run_name = run_name[1:]
        banned_chars = """!#$&*?/<>{}[]()|~`'":;\\\n\t """
        for banned_char in banned_chars:
            run_name = run_name.replace(banned_char, "_")

        self.gui.set_project_file(new_file)
        self.gui.update_keyword("run_name", run_name)
        self.gui.save_project()

        # change file watcher
        self.gui.slot_rundir_timer()
        self.gui.signal_update_runbuttons.emit("")
        self.gui.hide_file_menu()
