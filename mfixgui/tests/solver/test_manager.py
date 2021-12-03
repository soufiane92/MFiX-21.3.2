# -*- coding: utf-8 -*-
"""
Test the Solver Manager dialog
"""

import os
import subprocess

from os.path import abspath
from unittest.mock import patch

import pytest

from mfixgui.solver.manager import (
    Solver,
    SolverStatus,
    SolverManager,
    SolverManagerFactory,
)


@pytest.fixture(scope="module")
def all_paths_exe():
    """ Make all paths executable """
    with patch("mfixgui.solver.manager.isfile") as is_file:
        is_file.return_value = True
        with patch("mfixgui.solver.manager.os.access") as access:
            access.return_value = True
            yield


@pytest.fixture
def no_flags():
    """ Mock solver with no flags """
    with patch("mfixgui.solver.manager.subprocess.check_output") as check_output:
        check_output.return_value = b""
        yield


@pytest.fixture
def solver_err():
    """ Mock executable that does not run with '--print-flags """
    with patch("mfixgui.solver.manager.subprocess.check_output") as check_output:
        check_output.side_effect = subprocess.CalledProcessError(
            "mfixsolver", "Problem running mfixsolver --print-flags"
        )
        yield


@pytest.fixture
def mfix_gui(mocker):
    """ Mock the MfixGui and its project_dir """
    gui = mocker.Mock()
    gui.get_project_dir.return_value = "/fake_projectdir"
    yield gui


class TestSolver:
    """ Test creating a Solver given a path """

    def test_solver_pending(self, all_paths_exe, mocker):
        """ Add a custom solvers """
        with patch("mfixgui.solver.manager.Thread") as thread:
            solver = Solver("my_featureful_solver", mocker.Mock())

            assert solver.status == SolverStatus.PENDING
            assert solver.icon() == "timelapse.svg"

    def test_solver_ok(self, all_paths_exe, no_flags, mocker):
        """ Add a custom solvers """
        solver = Solver("my_featureful_solver", mocker.Mock())
        solver._thread.join()

        assert solver.status == SolverStatus.GOOD
        assert solver.icon() == "check_outline.svg"

    def test_solver_error(self, all_paths_exe, solver_err, mocker):
        """ Add a custom solvers """
        solver = Solver("my_featureful_solver", mocker.Mock())
        solver._thread.join()

        assert solver.status == SolverStatus.ERROR
        assert solver.icon() == "error_outline.svg"

    @pytest.mark.parametrize(
        "solver_output,flags",
        [
            (b"", (False, False, False)),
            (b"python", (True, False, False)),
            (b"smp", (False, True, False)),
            (b"dmp", (False, False, True)),
            (b"python smp dmp", (True, True, True)),
        ],
    )
    def test_solver_flags(self, all_paths_exe, solver_output, flags, mocker):
        """ Add a custom solvers """
        with patch("mfixgui.solver.manager.subprocess.check_output") as check_output:
            check_output.return_value = solver_output
            solver = Solver("my_featureful_solver", mocker.Mock())

            assert solver.python_enabled() == flags[0]
            assert solver.smp_enabled() == flags[1]
            assert solver.dmp_enabled() == flags[2]


class TestSolverManager:
    """ Test adding and removing Solvers from SolverManager """

    def test_add_solver(self, all_paths_exe, mfix_gui, no_flags, mocker):
        """ Add a solver """
        solver_update = mocker.Mock()
        solver_manager = SolverManager(mfix_gui, solver_update)

        solver_manager.add(abspath("/my/custom/solver"))

        assert sorted(solver_manager.solver_keys()) == ["/my/custom/solver"]
        solver_update.emit.assert_called()

    def test_add_project_solver(self, all_paths_exe, mfix_gui, no_flags, mocker):
        """ Add a custom solver in project directory """
        solver_update = mocker.Mock()
        solver_manager = SolverManager(mfix_gui, solver_update)

        solver_manager.add("/fake_projectdir/solver")

        assert sorted(solver_manager.solver_keys()) == ["[project]/solver"]
        solver_update.emit.assert_called()

    @patch("mfixgui.solver.manager.sys")
    def test_add_default_solver(self, sys, all_paths_exe, mfix_gui, no_flags, mocker):
        """ Add a solver in same directory as the current Python executable """
        sys.executable = abspath("/my/python")
        solver_update = mocker.Mock()
        solver_manager = SolverManager(mfix_gui, solver_update)

        solver_manager.add(abspath("/my/custom/solver"))

        assert sorted(solver_manager.solver_keys()) == ["[default]/custom/solver"]
        solver_update.emit.assert_called()

    def test_duplicate_solver(self, all_paths_exe, mfix_gui, no_flags, mocker):
        """ Can only add one solver for each fully resolved path """
        solver_update = mocker.Mock()
        solver_manager = SolverManager(mfix_gui, solver_update)

        solver_manager.add(abspath("/my/custom/solver"))
        solver_manager.add(abspath("/my/custom/solver"))
        solver_manager.add(abspath("/my/custom/../custom/solver"))

        assert sorted(solver_manager.solver_keys()) == ["/my/custom/solver"]
        solver_update.emit.assert_called()

    def test_remove_solver(self, all_paths_exe, mfix_gui, no_flags, mocker):
        """ Remove a solver """
        solver_update = mocker.Mock()
        solver_manager = SolverManager(mfix_gui, solver_update)
        solver_manager.add(abspath("/my/custom/solver"))

        solver_manager.remove(abspath("/my/custom/solver"))

        assert sorted(solver_manager.solver_keys()) == []
        solver_update.emit.assert_called()


class TestSolverManagerFactory:
    """ Test initializing SolverManager from various sources """

    @pytest.fixture
    def solver_settings(self):
        with patch("mfixgui.solver.manager.SETTINGS") as settings:
            settings.value = lambda x: {
                "recent_executables": "/prev/solver" + os.pathsep + "/prev2/solver3",
                "mfix_exe": "/saved/exe",
            }[x]
            yield

    @pytest.fixture
    def all_sources(self, all_paths_exe, no_flags, solver_settings):
        """ Combine all sources in one fixture """
        yield

    def test_solver_order(self, all_sources, mfix_gui, mocker):
        """ Check the prescribed order that SolverManagerFactory initializes the Solvers in SolverManager """
        mfix_gui.project.mfix_gui_comments = {"mfix_exe": "/gui/comment_solver"}
        factory = SolverManagerFactory(mfix_gui, mocker.Mock(), "/cmdline/solver")

        solver_manager = factory.solver_manager()

        assert list(solver_manager.solvers.keys()) == [
            abspath(path)
            for path in (
                "/cmdline/solver",
                "/gui/comment_solver",
                "/prev/solver",
                "/prev2/solver3",
                "/saved/exe",
            )
        ]

    @patch("mfixgui.solver.manager.isfile")
    def test_solver_order_paths_missing(self, is_file, all_sources, mfix_gui, mocker):
        """ Check the prescribed order that SolverManagerFactory initializes the Solvers in SolverManager """
        mfix_gui.project.mfix_gui_comments = {"mfix_exe": "/gui/comment_solver"}
        is_file.return_value = False
        factory = SolverManagerFactory(mfix_gui, mocker.Mock(), "/cmdline/solver")

        solver_manager = factory.solver_manager()

        assert list(solver_manager.solvers.keys()) == []
