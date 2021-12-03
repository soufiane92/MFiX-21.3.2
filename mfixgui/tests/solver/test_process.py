# -*- coding: utf-8 -*-
"""
Test Solver Process
"""

import os

from unittest.mock import patch

import pytest

import mfixgui.solver.process


class TestLocalRuns:
    @pytest.fixture
    def process_manager(self, mocker, tmpdir):
        gui = mocker.Mock()
        gui.get_project_dir.return_value = str(tmpdir)
        gui.get_project_file.return_value = "some_file"
        procman = mfixgui.solver.process.ProcessManager(gui)
        with patch("mfixgui.solver.process.QProcess"):
            yield procman

    def test_local_run(self, process_manager, mocker):
        """ Test launching a local job """
        mysolver = mocker.Mock()
        mysolver.path = "my_solver"

        with patch.object(mfixgui.solver.process.os, "unlink") as unlink:
            process_manager.start_solver(mysolver, None, 1, (1, 1, 1))

            unlink.assert_not_called()
            process_manager.mfix_process.start.assert_called_with(
                "env", ["OMP_NUM_THREADS=1", "my_solver", "-s", "-f", "some_file"]
            )

    def test_rm_mfixstop(self, process_manager, mocker, tmpdir):
        mfix_stop = os.path.join(tmpdir, "MFIX.STOP")
        open(mfix_stop, 'a').close()
        mysolver = mocker.Mock()
        mysolver.path = "my_solver"

        with patch.object(mfixgui.solver.process.os, "unlink") as unlink:
            process_manager.start_solver(mysolver, None, 1, (1, 1, 1))

            unlink.assert_called_with(mfix_stop)
            process_manager.mfix_process.start.assert_called_with(
                "env", ["OMP_NUM_THREADS=1", "my_solver", "-s", "-f", "some_file"]
            )


@patch("mfixgui.solver.process.get_mfix_home")
@patch("mfixgui.solver.process.QProcess")
def test_queue_job(qprocess, get_mfix_home, mocker):
    """ Test submitting a job to the queue """
    gui = mocker.Mock()
    get_mfix_home.return_value = "MFIX_HOME_DIRECTORY"
    gui.get_project_dir.return_value = "some_dir"
    gui.get_project_file.return_value = "some_file"
    gui.project.get_value.return_value = "projname"
    template = mocker.Mock()
    template.template_values.return_value = {}
    template.get_script.return_value = "some_script"
    template.delete.return_value = "DELETE"
    template.status.return_value = "STATUS"
    template.submit.return_value = "SUBMIT"
    template.job_id_regex.return_value = "REGEX"
    mysolver = mocker.Mock()
    mysolver.path = "my_solver"
    procman = mfixgui.solver.process.ProcessManager(gui)

    procman.start_solver(mysolver, template, 1, (1, 1, 1))

    gui.job_manager.submit_command.assert_called_with(
        "some_script",
        "SUBMIT",
        "DELETE",
        "STATUS",
        "REGEX",
        {
            "COMMAND": (
                "env MFIX_RUN_CMD="
                + '"env OMP_NUM_THREADS=1 my_solver -s -f some_file"'
                + " env OMP_NUM_THREADS=1 my_solver -s -f some_file"
            ),
            "MFIX_HOME": "MFIX_HOME_DIRECTORY",
            "PROJECT_NAME": "projname",
        },
    )
