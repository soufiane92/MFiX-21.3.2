# -*- coding: utf-8 -*-
"""
Dialog to build the mfixsolver from the GUI
On windows, the following paths need to be added to PATH:
  - ANACONDA_HOME/Library/mingw-w64/bin
  - ANACONDA_HOME/Library/usr/bin
"""

import enum
import os
from os.path import dirname, join
import platform
import re
import shutil
import sys

from qtpy.QtCore import QProcess, QProcessEnvironment, QSize, Qt, QUrl
from qtpy.QtGui import QColor, QTextCharFormat, QFont, QFontMetrics, QMouseEvent
from qtpy.QtWidgets import QApplication, QDialog

from mfixgui.tools import (SOURCE_DIR, SCRIPT_DIRECTORY,
                           conda_prefix, plural, safe_int)
from mfixgui.tools.qt import get_ui, SETTINGS
import mfixgui.build_mfixsolver


#RE_HIGHLIGHT_ERROR = re.compile(r"error[: ]", re.IGNORECASE)
RE_PERCENT_DONE = re.compile(r"^\[([ 1][0-9][0-9])%\]")
RE_COMPILER_ERROR = re.compile(r"^([A-Za-z/.0-9_]+):([0-9]+):([0-9]+):")
RE_LINK_ERROR = re.compile(r"^([A-Za-z/.0-9_]+):([0-9]+):")

WINDOWS = platform.system() == "Windows"


class BuildType(enum.Enum):
    """ enum.Enum for -DCMAKE_BUILD_TYPE """

    RELWITHDEBINFO = "Release with debug information"
    DEBUG = "Debug"
    CUSTOM = "Custom (specify compiler flags)"


class Server(enum.Enum):
    """ enum.Enum for -DCMAKE_BUILD_TYPE """

    PYMFIX = "Python implementation"
    NONE = "None (batch)"


class BuildPopup(QDialog):
    """ Dialog box for running build_mfixsolver to build the solver """



    def __init__(self, parent):
        QDialog.__init__(self, parent)
        gui = self.gui = parent
        self.cwd = os.getcwd()
        ui = self.ui = get_ui("build_popup.ui", self)
        tb = ui.textbrowser
        le = ui.lineedit_command
        m = QFontMetrics(tb.font())
        ui.min_width = m.width(" " * 120)
        tb.setMinimumSize(QSize(ui.min_width, 20 * m.height()))
        le.setMinimumSize(QSize(m.width(" " * 40), m.height()))

        tb.contextMenuEvent = self.handle_context_menu
        tb.anchorClicked.connect(self.handle_anchor)
        ui.button_first_error.clicked.connect(self.goto_first_error)
        ui.button_next_error.clicked.connect(self.goto_next_error)
        ui.button_prev_error.clicked.connect(self.goto_prev_error)
        self.finished.connect(gui.handle_compile_finished)
        self.build_proc = QProcess()
        ui.min_height = None
        #ui.setModal(True)
        ui.setWindowTitle("Build solver")
        ui.combobox_server.addItems([s.value for s in Server])
        ui.combobox_buildtype.addItems([type_.value for type_ in BuildType])
        ui.adjustSize()
        self.init_handlers()

        #desktop = QApplication.instance().desktop()
        #screen = desktop.screenNumber(desktop.cursor().pos())
        #geo = self.ui.frameGeometry()
        #geo.moveCenter(desktop.screenGeometry(screen).center())
        #self.ui.move(geo.center())


    def popup(self):
        # only show server option in dev mode
        self.gui.ui.toolbutton_compile.setEnabled(False)
        self.ui.progressbar.setRange(0,100)
        self.ui.progressbar.setValue(0)
        dev_mode = int(SETTINGS.value("developer_mode", 0))
        self.init_visibility(dev_mode)
        if self.gui.project:
            self.load_settings(self.gui.project.mfix_gui_comments)

        self.set_output_visible(False)
        self.update_build_cmd()
        self.set_build_enabled(True)
        self.update_compiler_box()
        self.ui.checkbox_dmp.stateChanged.connect(self.update_compiler_box)

        self.cwd = os.getcwd()
        self.show()
        self.raise_()
        self.activateWindow()
        #desktop = QApplication.instance().desktop()
        #screen = desktop.screenNumber(desktop.cursor().pos())
        #geo = self.ui.frameGeometry()
        #geo.moveCenter(desktop.screenGeometry(screen).center())
        #self.ui.move(geo.center())


    def handle_context_menu(self, event):
        # Avoid exposing internal urls
        menu = self.ui.textbrowser.createStandardContextMenu(event.globalPos())
        for ac in menu.actions():
            if ac.text() == 'Copy &Link Location':
                menu.removeAction(ac)
        menu.exec_(event.globalPos())


    def goto_first_error(self):
        self.goto_error(1)

    def goto_prev_error(self):
        self.goto_error(self.current_error-1)

    def goto_next_error(self):
        self.goto_error(self.current_error+1)

    def update_compiler_box(self):
        self.ui.combobox_compiler.clear()
        compilers = (
            [] if self.ui.checkbox_dmp.isChecked() else ["gfortran", "ifort"]
        ) + ["mpifort", "mpiifort", "mpif90"]
        self.ui.combobox_compiler.addItems([fc for fc in compilers if shutil.which(fc)])


    def intercept_close(self):
        if self.build_proc.state() > 0:
            self.kill_build()
        if self.gui.project:
            self.save_build_settings(self.gui.project.mfix_gui_comments)
        self.close()

    def handle_anchor(self, url):
        url = url.url() # it's a QUrl
        n = int(url.split('#error_',1)[1])
        self.current_error = n
        self.goto_error(n)


    def goto_error(self, n):
        if n < 1:
            n = 1
        elif n > self.error_count:
            n = self.error_count
        self.current_error = n
        self.ui.textbrowser.setSource(QUrl("#error_%s"%n))
        fname, line, column = self.errors[n-1]
        tab = self.gui.editor_widget.open(fname)
        tab.editor.goto(line, column)
        self.gui.change_mode('editor')
        self.update_error_buttons()
        #self.gui.raise_()


    def cancel(self):
        """ if build is running, stop build
            if build is not running, close dialog box """
        if self.build_proc.state() > 0:
            self.kill_build()
            self.set_build_enabled(True)
            self.ui.label_progress.setText("Build canceled.")
        else:
            if self.gui.project:
                self.save_build_settings(self.gui.project.mfix_gui_comments)
            self.ui.close()

    def kill_build(self):
        if self.build_proc.state() > 0:
            self.build_proc.kill()
            self.build_proc.waitForFinished(300)

    def toggle_output(self, show=False):
        """ hide or show the build output TextBrowser """
        if not self.ui.textbrowser.isVisible() or show:
            self.set_output_visible(True)
            self.scroll_to_end()
        else:
            self.set_output_visible(False)

    def scroll_to_end(self):
        sb = self.ui.textbrowser.verticalScrollBar()
        sb.setValue(sb.maximum())

    def set_output_visible(self, show):
        ui = self.ui
        for b in (ui.button_first_error, ui.button_prev_error, ui.button_next_error):
            b.setVisible(show)
        ui.textbrowser.setVisible(show)
        if show:
            ui.pushbutton_show_out.setText("Hide build output")
        else:
            ui.pushbutton_show_out.setText("Show build output")
            if self.ui.min_height is None:
                self.ui.min_height = self.ui.height()
        ui.adjustSize()

    def size_hint(self):
        height = self.ui.height()
        width = self.ui.width()

        tb_vis = self.ui.textbrowser.isVisible()
        tb_width = self.ui.min_width + 10
        if self.ui.min_height is not None and not tb_vis:
            height = self.ui.min_height
        if tb_vis and tb_width > width:
            width = tb_width
        size = QSize(width, height)
        return size

    def update_build_cmd(self):
        ui = self.ui
        ui.lineedit_command.setText(" ".join(self.get_build_cmd()))
        can_build = (
            bool(ui.combobox_compiler.currentText())
            or not ui.checkbox_dmp.isChecked()
        )
        status = (
            'Press "Build solver" to compile.'
            if can_build
            else "Specify compiler [wrapper] for DMP"
        )

        self.pushbutton_build.setEnabled(can_build)
        self.label_progress.setText(status)
        self.label_progress.setStyleSheet("color: None;")

        build_type = self.ui.combobox_buildtype.currentText()
        custom = (build_type == BuildType.CUSTOM.value)
        self.ui.label_flags.setEnabled(custom)
        self.ui.lineedit_compiler_flags.setEnabled(custom)

    def clean(self):
        ui = self.ui
        ui.label_progress.setText("Cleaning build directory...")
        ui.label_progress.setStyleSheet("color: None;")

        ui.progressbar.setValue(0)
        ui.progressbar.setRange(0, 0)

        self.print_to_output("Cleaning directory: %s\n" % self.cwd, color='blue')
        self.set_build_enabled(False)

        removed_stuff = mfixgui.build_mfixsolver.do_clean(self.cwd)

        ui.progressbar.setValue(0)
        self.print_to_output(removed_stuff or "Nothing to clean.\n")
        ui.label_progress.setText("Cleaning build directory... Done")
        self.set_build_enabled(True)

        ui.progressbar.setValue(0)
        ui.progressbar.setRange(0, 100)

    def build(self):
        """ Start a build_mfixsolver process """
        ui = self.ui
        tb = ui.textbrowser
        tb.clear()
        self.error_count = 0
        self.errors = []
        self.current_error = None
        self.update_error_buttons()
        self.scroll_to_end()
        ui.label_progress.setText("Checking requirements...")
        ui.label_progress.setStyleSheet("color: None;")
        ui.progressbar.setRange(0, 0)

        if self.gui.unsaved_flag:
            confirm = self.gui.message(text="Save project before building?",
                                   title="Save?",
                                   icon='question',
                                   buttons=['yes', 'no'],
                                   default='no')
            if confirm == 'yes':
                self.gui.save_project()

        if self.gui.editor_widget.check_needs_saved():
            confirm = self.gui.message(text="Save editor files before building?",
                                   title="Save?",
                                   icon='question',
                                   buttons=['yes', 'no'],
                                   default='no')
            if confirm == 'yes':
                self.gui.save_editor_files()

        self.build_proc = QProcess()
        self.build_proc.setWorkingDirectory(str(self.cwd))
        cmd = self.get_build_cmd()
        self.print_to_output("Running %s\n" % ' '.join(cmd), color='blue')
        self.build_proc.setProcessEnvironment(get_environment())
        self.build_proc.readyReadStandardOutput.connect(self.read_out)
        self.build_proc.readyReadStandardError.connect(self.read_err)
        self.build_proc.finished.connect(self.finished_building)
        self.build_proc.error.connect(self.error)
        self.set_build_enabled(False)
        self.build_proc.start(cmd[0], cmd[1:])


    def update_error_buttons(self):
        ui = self.ui
        if self.error_count == 0:
            for b in (ui.button_first_error, ui.button_next_error, ui.button_prev_error):
                b.setEnabled(False)
        else:
            ui.button_first_error.setEnabled(True)
            ui.button_next_error.setEnabled(self.current_error is not None
                                            and self.current_error < self.error_count)
            ui.button_prev_error.setEnabled(self.current_error is not None
                                            and self.current_error > 1)



    def finished_building(self, exit_code, exit_status):
        if exit_code == 0 and exit_status == 0:
            self.ui.progressbar.setRange(0, 100)
            self.ui.progressbar.setValue(100)
            self.ui.label_progress.setText("Build succeeded.")
            self.ui.label_progress.setStyleSheet("color: blue;")
        else:
            if self.error_count > 0:
                msg = "Build failed with %s." % plural(self.error_count, 'error')
            else:
                msg = "Build failed."
            #self.ui.progressbar.reset()
            self.ui.progressbar.setRange(0,100)
            self.ui.label_progress.setText(msg)
            self.ui.label_progress.setStyleSheet("color: red;")
            self.toggle_output(show=True)
        self.set_build_enabled(True)
        self.ui.pushbutton_show_out.setEnabled(True)

    def set_build_enabled(self, enabled):
        self.ui.pushbutton_cancel.setText("Close" if enabled else "Cancel")
        self.ui.pushbutton_build.setEnabled(enabled)
        self.ui.pushbutton_clean.setEnabled(enabled)
        self.ui.compiler_groupbox.setEnabled(enabled)
        self.ui.checkbox_groupbox.setEnabled(enabled)

    def error(self, error):
        cmd = " ".join(self.get_build_cmd())
        if error == QProcess.FailedToStart:
            msg = "Process failed to start: " + cmd
        elif error == QProcess.Crashed:
            msg = "Process exit: " + cmd
        elif error == QProcess.Timedout:
            msg = "Process timeout: " + cmd
        elif error in (QProcess.WriteError, QProcess.ReadError):
            msg = "Process communication error " + cmd
        else:
            msg = "Unknown error: " + cmd

        self.ui.label_progress.setText("Process Error")
        self.print_to_output(msg, color='red')
        self.toggle_output(show=True)

    def read_out(self):
        if self.build_proc is None:
            return
        output = str(self.build_proc.readAllStandardOutput(),
                     encoding='ascii', errors='ignore')
        self.check_progress(output, is_err=False)

    def read_err(self):
        if self.build_proc is None:
            return
        output = str(self.build_proc.readAllStandardError(),
                     encoding='ascii', errors='ignore')
        self.check_progress(output, is_err=True)

    def check_progress(self, output, is_err):
        for line in output.splitlines():
            self.check_progress1(line, is_err)

    def check_progress1(self, line, is_err):
        ui = self.ui
        color = 'black'
        err = None
        if is_err:
            color = 'red'
            match = RE_COMPILER_ERROR.search(line)
            if match:
                fname = match.group(1)
                lno = int(match.group(2))
                col = int(match.group(3))
                err = (fname, lno, col)
            else:
                match = RE_LINK_ERROR.match(line)
                if match:
                    fname = match.group(1)
                    lno = int(match.group(2))
                    err = (fname, lno, 0)
            if err:
                self.errors.append(err)
                text = '%s:%s:%s:' % err
                self.error_count += 1
                anchor = 'error_%s' % self.error_count
                self.print_to_output('<a href=#%s name=%s>%s</a>'
                                     %(anchor, anchor, text))
                line = line[match.span()[1]:]
                self.update_error_buttons()


        else:
            match = RE_PERCENT_DONE.match(line)
            if match:
                percent = int(match.group(1))
                ui.label_progress.setText("Compiling...")
                ui.progressbar.setRange(0, 100)
                ui.progressbar.setValue(percent)
            elif self.ui.progressbar.value() == 100:
                ui.label_progress.setText("Linking...")
                ui.progressbar.setRange(0, 0)
            if 'build succ' in line.lower():
                color = 'blue'
            elif 'build fail' in line.lower():
                color = 'red'
        #if not line.endswith('\n'):
        #    line += '\n'
        line = line.replace('&', '&amp;')
        line = line.replace('>', '&gt;')
        line = line.replace('<', '&lt;')
        self.print_to_output(line+'\n', color)


    def print_to_output(self, msg, color='black'):
        """ display message in the build output TextBrowser """
        ui = self.ui
        tb = ui.textbrowser

        cursor = tb.textCursor()
        cursor.movePosition(cursor.End)

        sb = tb.verticalScrollBar()
        at_end = sb.value() == sb.maximum()

        if "<a href=" in msg:
            html = msg # Already html, don't change a thing
        else:
            msg = msg.replace('\r', '') # Windows output from build_mfixsolver
            msg = msg.replace('\n', '<br>')
            msg = msg.replace(' ', '&nbsp;')
            html = '<span style="color:%s">%s</span>' % (color, msg) # add color
        cursor.insertHtml(html)
        if at_end:
            self.scroll_to_end()


    def init_visibility(self, dev_mode):
        self.ui.textbrowser.setVisible(False)

        nonwindows_only = [
            self.ui.checkbox_dmp,
            #self.ui.checkbox_smp,
            self.ui.combobox_buildtype,
            self.ui.combobox_compiler,
            self.ui.compiler_groupbox,
            self.ui.lineedit_compiler_flags,
        ]
        for widget in nonwindows_only:
            widget.setVisible(not WINDOWS)

        devmode_only = [
            self.ui.checkbox_verbose,
            self.ui.combobox_server,
            self.ui.label_server,
        ]
        for widget in devmode_only:
            widget.setVisible(dev_mode)

    def init_handlers(self):
        signals = [
            self.ui.checkbox_dmp.toggled,
            self.ui.checkbox_parallel.toggled,
            self.ui.checkbox_smp.toggled,
            self.ui.checkbox_verbose.toggled,
            self.ui.combobox_buildtype.currentIndexChanged,
            self.ui.combobox_compiler.currentIndexChanged,
            self.ui.combobox_compiler.editTextChanged,
            self.ui.combobox_server.currentIndexChanged,
            self.ui.combobox_server.editTextChanged,
            self.ui.lineedit_compiler_flags.textChanged,
        ]
        for signal in signals:
            signal.connect(self.update_build_cmd)

        self.ui.finished.connect(self.intercept_close)
        self.ui.pushbutton_build.clicked.connect(self.build)
        self.ui.pushbutton_cancel.clicked.connect(self.cancel)
        self.ui.pushbutton_clean.clicked.connect(self.clean)
        self.ui.pushbutton_show_out.clicked.connect(self.toggle_output)

    def set_server(self, interactive):
        set_combobox(self.ui.combobox_server, interactive)

    def set_buildtype(self, buildtype):
        set_combobox(self.ui.combobox_buildtype, buildtype)

    def get_build_cmd(self):
        """ return command argument list for building the solver """
        return (
            ["build_mfixsolver"]
            if not SOURCE_DIR
            else [sys.executable, "-m", "mfixgui.build_mfixsolver"]
        ) + self.get_args()

    def get_args(self):
        args = []
        build_type = self.ui.combobox_buildtype.currentText()
        if build_type == BuildType.DEBUG.value:
            args += [f"-DCMAKE_BUILD_TYPE={build_type}"]
        elif build_type == BuildType.CUSTOM.value:
            fcflags = self.ui.lineedit_compiler_flags.text()
            args += [f"-DCMAKE_Fortran_FLAGS={fcflags}"]


        if self.ui.checkbox_parallel.isChecked():
            args += ["-j"]

        if self.ui.checkbox_smp.isChecked():
            args += ["--smp"]

        compiler = self.ui.combobox_compiler.currentText()
        if self.ui.checkbox_dmp.isChecked():
            args += ["--dmp"]
            args += [f"-DMPI_Fortran_COMPILER={compiler}"]
        elif compiler:
            args += [f"-DCMAKE_Fortran_COMPILER={compiler}"]

        if int(SETTINGS.value("developer_mode", 0)):
            server = self.ui.combobox_server.currentText()
            if server == Server.NONE.value:
                args += ["--server=none"]

        if self.ui.checkbox_verbose.isChecked():
            args += ["--verbose"]

        return args

    def load_settings(self, comments):
        ui = self.ui
        ui.checkbox_dmp.setChecked(safe_int(comments.get("BUILD_DMP", "0")))
        ui.checkbox_parallel.setChecked(safe_int(comments.get("BUILD_PARALLEL", "1")))
        ui.checkbox_smp.setChecked(safe_int(comments.get("BUILD_SMP", "0")))
        ui.checkbox_verbose.setChecked(safe_int(comments.get("BUILD_VERBOSE", "0")))
        ui.combobox_compiler.setEditText(comments.get("BUILD_FC", ""))
        ui.lineedit_compiler_flags.setText(comments.get("BUILD_FC_FLAGS", ""))

        buildtype = comments.get("BUILD_TYPE", BuildType.RELWITHDEBINFO.name)
        if buildtype in BuildType.__members__:
            self.set_buildtype(BuildType[buildtype].value)

        server = comments.get("BUILD_INTERACTIVE", Server.PYMFIX.name)
        if server in Server.__members__:
            self.set_server(Server[server].value)

    def save_build_settings(self, comments):
        """ Save Build dialog settings in gui_comments """
        ui = self.ui
        def update(key, val):
            old = comments.get(key)
            if isinstance(val, int):
                if old is not None:
                    old = int(old)
            if val != old:
                comments[key] = val
                self.gui.set_unsaved_flag()
        for (key, val) in (("BUILD_DMP", int(ui.checkbox_dmp.isChecked())),
                           ("BUILD_INTERACTIVE", Server(ui.combobox_server.currentText()).name),
                           ("BUILD_PARALLEL", int(ui.checkbox_parallel.isChecked())),
                           ("BUILD_SMP", int(ui.checkbox_smp.isChecked())),
                           ("BUILD_TYPE", BuildType(ui.combobox_buildtype.currentText()).name)):
            update(key, val)
        flags = ui.lineedit_compiler_flags.text().strip()
        if flags:
            update("BUILD_FC_FLAGS", flags)
        else:
            if comments.pop("BUILD_FC_FLAGS", None):
                self.gui.set_unsaved_flag()


def get_environment():
    PYTHONPATH = dirname(SCRIPT_DIRECTORY)
    if "PYTHONPATH" in os.environ:
        PYTHONPATH += os.pathsep + os.environ["PYTHONPATH"]
    env = QProcessEnvironment.systemEnvironment()
    env.insert("PYTHONPATH", PYTHONPATH)
    env.insert("PYTHONUNBUFFERED", "y")
    if WINDOWS:
        anaconda_home = conda_prefix()
        path = env.value("PATH")
        new_path = os.pathsep.join([
            join(anaconda_home, "Library", "mingw-w64", "bin"),
            join(anaconda_home, "Library", "usr", "bin"),
            path])
        env.insert("PATH", new_path)
    return env


def set_combobox(cb, value):
    """ Set current index of combobox by value """
    for i in range(cb.count()):
        if value == cb.itemText(i):
            cb.setCurrentIndex(i)
