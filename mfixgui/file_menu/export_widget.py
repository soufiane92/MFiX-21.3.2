import copy
import shutil
import warnings

from os import listdir, makedirs, symlink
from os.path import (
    basename,
    dirname,
    expanduser,
    exists,
    isdir,
    isfile,
    islink,
    join,
    normpath,
    splitext,
    realpath,
    relpath
)

from PyQt5.QtWidgets import QFileDialog, QWidget
from PyQt5 import QtGui, QtCore

from mfixgui.tools import find_project_file
from mfixgui.tools.qt import SETTINGS, get_ui, get_icon, main_icon_size
from mfixgui.regexes import re_valid_run_name_qt

class ExportWidget(QWidget):
    def __init__(self, gui):
        super(ExportWidget, self).__init__()
        get_ui("export.ui", widget=self)

        self.gui = gui
        self.export_btn.clicked.connect(self.export_project)
        self.export_location_btn.clicked.connect(self.get_new_location)
        self.setObjectName("export")
        self.lineedit_export_directory.textChanged.connect(self.handle_update_exportdir)
        self.lineedit_export_run_name.textChanged.connect(self.handle_update_run_name)
        self.lineedit_export_run_name.setValidator(QtGui.QRegExpValidator(
            QtCore.QRegExp(re_valid_run_name_qt)))

        self.export_location_btn.setIcon(get_icon('folder.svg'))

        self._files = []
        self.do_export_btn.clicked.connect(self._handle_do_export)
        self.dont_export_btn.clicked.connect(self._handle_dont_export)

        # set the icon size
        for btn in [self.do_export_btn, self.dont_export_btn]:
            btn.setIconSize(main_icon_size())

    def show(self):
        """Called when switching to the Export widget.
        New export directory is in home
        directory by default.

        """

        home_dir = expanduser("~")
        start_dir = join(home_dir, basename(self.gui.get_project_dir()))
        self.lineedit_export_directory.setText(start_dir)

        self.lineedit_export_run_name.setText(self.gui.project.get_value("run_name"))
        self.handle_update_run_name()
        self.handle_update_exportdir()

    def handle_update_run_name(self):
        run_name = self.lineedit_export_run_name.text()
        self.export_btn.setEnabled(bool(run_name))
        self.label_new_projectfile.setText(f"{run_name}.mfx") # Should we show full path here?

    def handle_update_exportdir(self):
        project_file = basename(self.gui.get_project_file())

        files_ = [f for f in self.projectdir_files()
                  if basename(f) != project_file and isfile(f)]

        self.update_files(files_)

    def projectdir_files(self):
        """ Returns list of output and project files under the project directory """
        project_dir = self.gui.get_project_dir()
        if project_dir is None:
            return []
        return [f
            for f in listdir(project_dir)
            if f and basename(f) and basename(f) != "build"]


    def get_new_location(self):
        project_dir = self.gui.get_project_dir()
        filename = QFileDialog.getExistingDirectory(
            self, "Export location", project_dir
        )
        if filename:
            self.lineedit_export_directory.setText(filename)

    def create_export_project(self, item):
        if self.gui.check_unsaved_abort():
            return
        project_file = find_project_file(item.full_path)
        self.gui.open_export_from_template(project_file)

    def export_project(self):
        """Copy project files to new directory, but do not switch to new project"""

        project_file = self.gui.get_project_file()
        if not project_file:
            return

        if self.gui.unsaved_flag:
            response = self.gui.message(
                title="Save?",
                icon="question",
                text="Save project before exporting?",
                buttons=["yes", "no", "cancel"])

            if response == "cancel":
                return

            if response == "yes":
                self.gui.handle_save(autostart=False)

        export_dir = self.lineedit_export_directory.text()
        makedirs(export_dir, exist_ok=True)

        if not self.gui.check_writable(export_dir):
            return

        run_name = self.lineedit_export_run_name.text()
        export_file = join(export_dir, run_name + ".mfx")

        if exists(export_file) and not self.gui.confirm_clobber(export_file):
            return

        self.write_exported_file(export_file)
        self.copy_output_files(export_dir)

        self.gui.hide_file_menu()
        response = self.gui.message(
            title="Export success",
            text=f"Successfully exported to:\n {export_file}\n Open exported project?",
            buttons=["cancel", "ok"],
        )
        if response == "ok":
            self.gui.open_project(export_file)

    def write_exported_file(self, export_file):
        """ Copy Project object and write to new location """

        new_project_run_name, _ = splitext(basename(export_file))
        self.gui.print_internal("Info: Exporting %s" % new_project_run_name)

        exported_project = copy.deepcopy(self.gui.project)
        exported_project.updateKeyword("run_name", new_project_run_name)
        template = SETTINGS.value("template", "standard").lower()
        with warnings.catch_warnings(record=True) as ws:
            try:
                exported_project.writeDatFile(
                    join(dirname(export_file), new_project_run_name + ".mfx"),
                    template=template)
            except Exception as e:
                self.error(str(e), popup=True)
                return
            for w in ws:
                self.gui.warn(str(w.message))

    def copy_output_files(self, export_dir):
        """ Copy output files *.RES, *.SP?, *.vtu, etc... """

        files_to_copy = self.selected_files()

        for path in files_to_copy:
            export_src = join(self.gui.get_project_dir(), path)
            export_dest = join(export_dir, basename(path))
            try:
                if islink(path):
                    linkdest = normpath(export_src)
                    symlink(linkdest, export_dest)

                elif isdir(export_src):
                    shutil.copytree(export_src, export_dir, symlinks=True)

                else:
                    shutil.copyfile(export_src, export_dest, follow_symlinks=False)
            except Exception as exc:
                self.gui.error(
                    f"Error copying {export_src} to {export_dest}:\n{exc}", popup=True
                )

    def selected_files(self):
        """ Returns selected set of files """
        widget = self.listwidget_copied
        selected_filenames = [widget.item(ii).text() for ii in range(widget.count())]
        return [
            file_
            for file_ in self._files
            if self._format_filename(basename(file_)) in selected_filenames
        ]

    def update_files(self, files_):
        """Called when showing the OutputFileWidget """
        self._files = files_
        self._update_listboxes()

    def _handle_do_export(self):
        for item in self.listwidget_uncopied.selectedItems():
            item = self.listwidget_uncopied.takeItem(self.listwidget_uncopied.row(item))
            self.listwidget_copied.addItem(item)

    def _handle_dont_export(self):
        for item in self.listwidget_copied.selectedItems():
            item = self.listwidget_copied.takeItem(self.listwidget_copied.row(item))
            self.listwidget_uncopied.addItem(item)

    def _update_listboxes(self):
        """ Update checkboxes and listboxes with current list of filenames """

        self.listwidget_copied.clear()
        self.listwidget_uncopied.clear()
        for file_ in sorted(self._files):
            text = self._format_filename(file_)
            self.listwidget_uncopied.addItem(text)

    def _format_filename(self, filename):
        """ Text displayed in the listboxes for this filename """
        name = basename(filename)
        name = relpath(filename, self.gui.get_project_dir())
        return (
            "{} -> {}".format(name, realpath(filename))
            if islink(filename)
            else "{}/".format(name)
            if isdir(filename)
            else name
        )
