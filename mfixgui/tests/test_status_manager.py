# -*- coding: utf-8 -*-

import pytest

from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import QMainWindow, QLabel

from mfixgui.gui import MfixGui
from mfixgui.status_manager import StatusManager
from mfixgui.tools.qt import get_ui


@pytest.fixture
def mfix(mocker, qtbot):
    mfixgui_fixture = mocker.Mock()
    mfixgui_fixture.ui = get_ui("gui.ui")

    mfixgui_fixture.ui.label_status = QLabel("Ready")
    return mfixgui_fixture


@pytest.fixture
def status_manager(mfix):
    status_manager = StatusManager(mfix)
    status_manager.mfixgui.get_project_file.return_value = None
    return status_manager


def test_no_job(status_manager):
    status_manager.mfixgui.get_res_files.return_value = None
    status_manager.mfixgui.job_manager.is_job_pending.return_value = False
    status_manager.mfixgui.job_manager.job = None

    status_manager.update_status_if_changed()

    assert not status_manager.mfixgui.ui.toolbutton_run_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_pause_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_stop_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_reset_mfix.isEnabled()
    assert status_manager.mfixgui.ui.toolbutton_compile.isEnabled()


def test_job_pending(status_manager):
    status_manager.mfixgui.job_manager.job.status = {"running": True}
    status_manager.mfixgui.job_manager.job.is_paused.return_value = False
    status_manager.mfixgui.job_manager.is_job_pending.return_value = True
    status_manager.mfixgui.get_project_file.return_value = "foobar.mfx"

    status_manager.update_status_if_changed()

    assert (
        status_manager.mfixgui.ui.label_status.text()
        == "MFiX stopping: elapsed time 0:00:00  "
    )

    assert not status_manager.mfixgui.ui.toolbutton_run_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_pause_mfix.isEnabled()
    assert status_manager.mfixgui.ui.toolbutton_stop_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_reset_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_compile.isEnabled()


def test_job_running(status_manager):
    status_manager.mfixgui.job_manager.job.status = {"running": True}
    status_manager.mfixgui.job_manager.job.is_paused.return_value = False
    status_manager.mfixgui.job_manager.pausing = False
    status_manager.mfixgui.job_manager.stopping = False
    status_manager.mfixgui.job_manager.is_job_pending.return_value = False
    status_manager.mfixgui.get_project_file.return_value = "foobar.mfx"

    status_manager.update_status_if_changed()

    assert (
        status_manager.mfixgui.ui.label_status.text()
        == "MFiX running: elapsed time 0:00:00  "
    )
    assert not status_manager.mfixgui.ui.toolbutton_run_mfix.isEnabled()
    assert status_manager.mfixgui.ui.toolbutton_pause_mfix.isEnabled()
    assert status_manager.mfixgui.ui.toolbutton_stop_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_reset_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_compile.isEnabled()


def test_job_paused(status_manager):
    status_manager.mfixgui.get_project_file.return_value = "foobar.mfx"
    status_manager.mfixgui.job_manager.is_job_pending.return_value = False
    status_manager.mfixgui.job_manager.job.is_paused.return_value = True
    status_manager.mfixgui.job_manager.job.status = {"running": True}

    status_manager.update_status_if_changed()

    assert (
        status_manager.mfixgui.ui.label_status.text()
        == "MFiX paused: elapsed time 0:00:00  "
    )
    assert status_manager.mfixgui.ui.toolbutton_run_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_pause_mfix.isEnabled()
    assert status_manager.mfixgui.ui.toolbutton_stop_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_reset_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_compile.isEnabled()


def test_job_finished(status_manager):
    status_manager.mfixgui.get_project_file.return_value = "foobar.mfx"
    status_manager.mfixgui.get_res_files.return_value = []
    status_manager.mfixgui.job_manager.is_job_pending.return_value = False
    status_manager.mfixgui.job_manager.job = None

    status_manager.update_status_if_changed()

    assert status_manager.mfixgui.ui.label_status.text() == "Ready"

    assert status_manager.mfixgui.ui.toolbutton_run_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_pause_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_stop_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_reset_mfix.isEnabled()
    assert status_manager.mfixgui.ui.toolbutton_compile.isEnabled()


def test_job_resumable(status_manager):
    status_manager.mfixgui.get_project_file.return_value = "foobar.mfx"
    status_manager.mfixgui.get_res_files.return_value = ["foo.log", "bar.spx"]
    status_manager.mfixgui.job_manager.is_job_pending.return_value = False
    status_manager.mfixgui.job_manager.job = None

    status_manager.update_status_if_changed()

    assert (
        status_manager.mfixgui.ui.label_status.text()
        == "Previous MFiX run is resumable.  Reset job to edit model  "
    )

    assert status_manager.mfixgui.ui.toolbutton_run_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_pause_mfix.isEnabled()
    assert not status_manager.mfixgui.ui.toolbutton_stop_mfix.isEnabled()
    assert status_manager.mfixgui.ui.toolbutton_reset_mfix.isEnabled()
    assert status_manager.mfixgui.ui.toolbutton_compile.isEnabled()
