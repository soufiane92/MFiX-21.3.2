import copy
import shutil
import warnings

from os import makedirs
from os.path import (basename, exists, expanduser, join)

from PyQt5.QtWidgets import QFileDialog, QWidget
from PyQt5 import QtGui, QtCore

from mfixgui.tools import find_project_file
from mfixgui.tools.qt import SETTINGS, get_ui, get_icon, main_icon_size
from mfixgui.regexes import re_valid_run_name_qt

class NewMeshWidget(QWidget):
    def __init__(self, gui):
        super(NewMeshWidget, self).__init__()
        get_ui("new_mesh.ui", widget=self)

        self.gui = gui
        self.button_create_mesh.clicked.connect(self.create_mesh)
        self.button_location.clicked.connect(self.get_new_location)
        self.setObjectName("new_mesh")
        self.lineedit_run_name.textChanged.connect(self.handle_update_run_name)
        self.lineedit_run_name.setValidator(QtGui.QRegExpValidator(
            QtCore.QRegExp(re_valid_run_name_qt)))

        self.button_location.setIcon(get_icon('folder.svg'))


    def show(self):
        """Called when switching to the new mesh widget.
        """
        home_dir = expanduser("~")
        proj_dir = self.gui.get_project_dir()
        if proj_dir:
            start_dir = join(home_dir, basename(proj_dir))
        else:
            start_dir = home_dir # ?
        self.lineedit_directory.setText(start_dir)
        self.lineedit_run_name.setText(self.gui.project.get_value('run_name'))
        self.handle_update_run_name()


    def handle_update_run_name(self):
        run_name = self.lineedit_run_name.text()
        self.button_create_mesh.setEnabled(bool(run_name))
        self.label_new_meshfile.setText(f'{run_name}.mfx')


    def get_new_location(self):
        project_dir = self.gui.get_project_dir()
        filename = QFileDialog.getExistingDirectory(self, 'Project location', project_dir)
        if filename:
            self.lineedit_directory.setText(filename)

    def create_mesh(self):
        if self.gui.check_unsaved_abort():
            return
        mesh_dir = self.lineedit_directory.text()
        run_name = self.lineedit_run_name.text()
        mesh_file = run_name + '.mfx' # This is correct.  .mfx is input, .msh is output
        makedirs(mesh_dir, exist_ok=True)
        if not self.gui.check_writable(mesh_dir):
            return
        fullpath = join(mesh_dir, mesh_file)
        if exists(fullpath) and not self.gui.confirm_clobber(fullpath):
            return
        try:
            with open(fullpath, 'w') as f:
                f.write("units='SI'\nppo=.True.\nrun_name='%s'\n" % run_name)
                f.close()
        except Exception as e:
            self.gui.error("Error creating mesh: %s" % str(e))
        self.gui.open_project(fullpath)
