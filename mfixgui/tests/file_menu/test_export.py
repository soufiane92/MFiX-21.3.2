# -*- coding: utf-8 -*-
"""
Test the Export file menu item
"""

import os.path

from unittest.mock import call, patch

import pytest

import mfixgui.file_menu.export_widget
from mfixgui.tools import get_mfix_templates, get_run_name_from_file


@pytest.fixture
def mfix_mock(mocker, qtbot):
    """" Mock of MfixGui instance """
    mfix_mock = mocker.Mock()
    mfix_mock.unsaved = False
    mfix_mock.get_project_file.return_value = "/original/project/filename"
    mfix_mock.output_selection_popup.get_output_files.return_value = ["selected.file"]
    mfix_mock.get_output_files.return_value = ["output.file"]
    return mfix_mock


@pytest.fixture
def export_widget(mfix_mock, mocker):
    """" Mock of MfixGui instance """

    with patch("mfixgui.file_menu.export_widget.makedirs") as makedirs:
        widget = mfixgui.file_menu.export_widget.ExportWidget(mfix_mock)
        yield widget


@pytest.mark.skip
def test_export_new_directory(export_widget, mocker):
    """ Test successful export to another directory """
    shutil = mocker.patch("mfixgui.file_menu.export_widget.shutil")

    export_widget.export_project()

    shutil.copyfile.assert_has_calls(
        [
            call("selected.file", "/new/exported/selected.file", follow_symlinks=True),
            call("output.file", "/new/exported/output.file", follow_symlinks=True),
        ]
    )


def test_export_same_directory(export_widget, mfix_mock, mocker):
    """ exporting to same directory not possible run popup and run it """
    shutil = mocker.patch("mfixgui.file_menu.export_widget.shutil")
    mfix_mock.get_project_file.return_value = "/same/filename"
    # mfix_mock.get_save_filename.return_value = "/same/filename"

    export_widget.export_project()

    shutil.copyfile.assert_not_called()


def test_no_project(export_widget, mfix_mock, mocker):
    """ export when no project is open """
    shutil = mocker.patch("mfixgui.file_menu.export_widget.shutil")
    mfix_mock.get_project_file.return_value = None

    export_widget.export_project()

    shutil.copyfile.assert_not_called()


def test_cancel_export_filename(export_widget, mfix_mock, mocker):
    """ users cancels when selecting an export filename """
    # mfix_mock.get_save_filename.return_value = None
    shutil = mocker.patch("mfixgui.file_menu.export_widget.shutil")

    export_widget.export_project()

    shutil.copyfile.assert_not_called()


def test_unwritable_directory(export_widget, mfix_mock, mocker):
    """ the user does not have permission to write to the directory of the export filename """
    mfix_mock.check_writable.return_value = False
    shutil = mocker.patch("mfixgui.file_menu.export_widget.shutil")

    export_widget.export_project()

    shutil.copyfile.assert_not_called()
    mfix_mock.check_writable.assert_called()


def test_cancel_confirmation_dialog(export_widget, mfix_mock, mocker):
    """ users cancels at the Copy? dialog """
    shutil = mocker.patch("mfixgui.file_menu.export_widget.shutil")
    mfix_mock.output_selection_popup.exec_.return_value = False

    export_widget.export_project()

    shutil.copyfile.assert_not_called()


def test_cancel_unsaved(export_widget, mfix_mock, mocker):
    """ user cancels when prompted to save """
    shutil = mocker.patch("mfixgui.file_menu.export_widget.shutil")
    mfix_mock.unsaved = True
    mfix_mock.message.return_value = "cancel"

    export_widget.export_project()

    shutil.copyfile.assert_not_called()


@pytest.mark.xfail()
def test_save_then_export(export_widget, mfix_mock, mocker):
    """ user saves before exporting """
    shutil = mocker.patch("mfixgui.file_menu.export_widget.shutil")
    mfix_mock.unsaved = True
    mfix_mock.message.return_value = "yes"

    export_widget.export_project()

    shutil.copyfile.assert_has_calls(
        [
            call("selected.file", "/new/exported/selected.file", follow_symlinks=True),
            call("output.file", "/new/exported/output.file", follow_symlinks=True),
        ]
    )


@pytest.mark.xfail()
def test_copy_exception(export_widget, mfix_mock, mocker):
    """ error occurs when copying saves before exporting """
    shutil = mocker.patch("mfixgui.file_menu.export_widget.shutil")
    shutil.copyfile.side_effect = Exception("hard drive crashed")

    export_widget.export_project()

    mfix_mock.error.assert_has_calls(
        [call("Error copying file:\nhard drive crashed", popup=True)]
    )


@pytest.fixture
def mock_dialogs(tmpdir):
    with patch("mfixgui.gui.NewProjectDialog.get_name_and_location") as name_loc:
        name_loc.return_value = ("THE_RUN_NAME", tmpdir)
        yield


@pytest.mark.xfail(run=False)
def test_run_name(mock_dialogs, tmpdir, mocker):
    """ test that RUNNAME of exported file matches original RUNNAME """

    mfix_gui = mocker.Mock()
    template_mfx = "tutorials/dem/fluid_bed_2d/fluid_bed_dem_2d.mfx"
    template = os.path.join(get_mfix_templates(), template_mfx)
    mfix_gui.open_new_from_template(template)

    # with patch.object(mfix_gui, "get_save_filename") as get_save_filename:
    with patch("mfixgui.file_menu.export_widget.makedirs"):
        with patch.object(mfix_gui, "message"):
            new_filename = str(tmpdir) + "/new_run_name.mfx"
            # get_save_filename.return_value = new_filename

            mfix_gui.export_widget.export_project()

            run_name = get_run_name_from_file(new_filename)
            assert run_name == "new_run_name"
