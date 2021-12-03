""" Module for representing Solver executables """

import os
from os.path import abspath, dirname, isdir, isfile, join, realpath, relpath

import logging
import subprocess
import sys

from collections import OrderedDict
from distutils import spawn
from enum import Enum
from threading import Thread

from mfixgui.tools.qt import SETTINGS
from mfixgui.tools import get_mfix_home

RECENT_EXE_LIMIT = 5

MFIXSOLVER_NAMES = ["mfixsolver",
                    "mfixsolver.exe",
                    "mfixsolver.bat",
                    "mfixsolver.sh"]


def find_default_solver_path():
    d = dirname(sys.executable)
    search_path = (d, join(d, 'Scripts'))
    for d in search_path:
        for name in MFIXSOLVER_NAMES:
            solver = join(d, name)
            if os.path.exists(solver):
                return solver
    return None


class SolverStatus(Enum):
    """ Enum for -DCMAKE_BUILD_TYPE """
    PENDING = "Waiting for 'mfixsolver --print-flags' to finish"
    GOOD = "--print-flags succeeded"
    ERROR = "--print-flags ran with an error"


class Solver:
    """ Represents a builtin or custom solver """

    def __init__(self, path, solvers_updated):
        self.name = str(path)
        self.path = path
        self.flags = ""
        self.stderror = None
        self.status = SolverStatus.PENDING
        self.solvers_updated = solvers_updated
        if solvers_updated:
            self._thread = Thread(target=self.run_print_flags)
            self._thread.start()

    def run_print_flags(self):
        try:
            logging.debug("Feature testing MFiX %s", self.path)
            self.flags = subprocess.check_output(
                [str(self.path), "--print-flags"], stderr=subprocess.STDOUT
            ).decode("utf-8", "ignore")
            self.status = SolverStatus.GOOD
            logging.debug("stdout: %s --print-flags: %s", self.path, self.flags)
        except subprocess.CalledProcessError as proc_err:
            self.stderror = proc_err.output
            self.status = SolverStatus.ERROR
            logging.debug(
                "could not run %s --print-flags: %s", self.path, self.stderror
            )
        logging.debug("finished: %s --print-flags: ", self.path)
        if self.solvers_updated:
            self.solvers_updated.emit()

    def dmp_enabled(self):
        """ Whether solver supports DMP """
        return "dmp" in self.flags

    def smp_enabled(self):
        """ Whether solver supports SMP """
        return "smp" in self.flags

    def python_enabled(self):
        """ Whether solver supports interactivity """
        return "python" in self.flags

    def get_error(self):
        """ Error message if solver failed """
        return self.stderror.decode("utf-8", "replace")

    def get_status(self):
        """ Current SolverStatus """
        return self.status

    def get_status_text(self):
        """ Current SolverStatus """
        if self.status == SolverStatus.PENDING:
            return "Checking Solver."

        if self.status == SolverStatus.GOOD:
            return ""

        if self.status == SolverStatus.ERROR:
            return "Solver Error:"

        raise ValueError(self.status)

    def icon(self):
        if self.status == SolverStatus.PENDING:
            return "timelapse.svg"

        if self.status == SolverStatus.GOOD:
            return "check_outline.svg"

        if self.status == SolverStatus.ERROR:
            return "error_outline.svg"

        raise ValueError(self.status)


class SolverManager:
    """ Represent the set of solvers available.  After constructor runs, it has no solvers """

    def __init__(self, mfixgui, solvers_updated):
        self.mfixgui = mfixgui
        self._solvers = OrderedDict()
        self.solvers_updated = solvers_updated
        self.project_dir = self.mfixgui.get_project_dir()

    def get_solver_from_text(self, text):
        return self.solvers.get(text, None)

    def solver_keys(self):
        """ Return the combobox display names of all solvers """
        return [key for key in self._solvers.keys()]

    def add(self, solver_path):
        """ Add a new solver (if path is not already present)"""
        solver = Solver(solver_path, self.solvers_updated)
        key = self.display_name(solver.path)
        if key in self._solvers:
            logging.debug("Solver already contains key %s for %s", key, solver.path)
        else:
            self._solvers[key] = solver
            self._solvers_updated()

        return solver

    @property
    def solvers(self):
        """ Dictionary of solvers display_name -> Solver """
        return self._solvers

    def remove(self, key):
        """ Remove solver """
        del self._solvers[key]
        self._solvers_updated()

    @classmethod
    def check_if_invalid_exe(cls, path):
        """ return error message if not a valid executable, else None """

        if not isfile(path):
            return "{} is not a file.".format(path)

        # try executable
        if not os.access(str(path), os.X_OK):
            return "{} is not an executable.".format(path)

        # windows, check extension
        if os.name == "nt":
            ext = os.path.splitext(path)[-1]
            if ext not in (".exe", ".bat"):
                return f"Extension {ext} is not recognized, must be .exe or .bat"

        return None

    def _solvers_updated(self):
        SETTINGS.setValue(
            "queue_solvers",  # Why is the key 'queue_solvers' ?
            "|".join([str(solver.path) for solver in list(self._solvers.values())]),
        )
        if self.solvers_updated:
            self.solvers_updated.emit()

    def display_name(self, solver_path):
        """ Given a solver_path, show the string to be displayed in Run dialog """
        real_path = realpath(solver_path)
        prj_dir = abspath(self.project_dir)
        if real_path.startswith(prj_dir):
            return join("[project]", relpath(real_path, prj_dir))

        default_dir = dirname(sys.executable)
        if real_path.startswith(default_dir):
            return join("[default]", relpath(real_path, default_dir))

        return real_path


class SolverManagerFactory:
    """ Creates a SolverManager and initializes it with solvers from various sources """

    def __init__(self, mfixgui, solvers_updated, cmdline_solver):
        self.commandline_option_exe = cmdline_solver
        self.mfixgui = mfixgui
        self.gui_comments = self.mfixgui.project.mfix_gui_comments
        self.project_dir = self.mfixgui.get_project_dir()

        self._solver_manager = SolverManager(mfixgui, solvers_updated)
        exe_list_order = [
            self.command_line_option,
            self.project_file_executable,
            self.project_directory_executables,
            self.os_path,
            self.recently_used_executables,
            self.mfix_build_directories,
            get_saved_exe,
        ]

        for exe_spec in exe_list_order:
            for exe in exe_spec():
                if isfile(exe):
                    self._solver_manager.add(exe)

    def solver_manager(self):
        return self._solver_manager

    def command_line_option(self):
        if self.commandline_option_exe is not None:
            yield self.commandline_option_exe

    def project_file_executable(self):
        project_exe = self.gui_comments.get("mfix_exe", None)
        if project_exe:
            yield project_exe

    def project_directory_executables(self):
        for name in MFIXSOLVER_NAMES:
            exe = join(self.project_dir, name)
            if os.path.exists(exe):
                yield exe

    def os_path(self):
        path_env_var = os.environ.get("PATH", "")
        for d in path_env_var.split(os.pathsep):
            if d != os.getcwd() and isdir(d):
                for name in MFIXSOLVER_NAMES:
                    exe = os.path.join(d, name)
                    if os.path.exists(exe):
                        yield exe

    def recently_used_executables(self):
        recent_list = SETTINGS.value("recent_executables")
        if recent_list:
            recent_solvers = [
                path for path in recent_list.split(os.pathsep)[:RECENT_EXE_LIMIT]
            ]
            for recent_exe in recent_solvers:
                if isfile(recent_exe):
                    yield recent_exe

    def mfix_build_directories(self):
        for name in MFIXSOLVER_NAMES:
            exe = join(get_mfix_home(), name)
            if os.path.exists(exe):
                yield exe


def get_saved_exe():
    """ Find last used solver from settings """
    last_exe = SETTINGS.value("mfix_exe")
    if last_exe and isfile(last_exe):
        yield last_exe
