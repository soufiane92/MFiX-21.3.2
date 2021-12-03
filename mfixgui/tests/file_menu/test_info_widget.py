import json

from os.path import join
from unittest.mock import patch

import pytest
from PyQt5.QtWidgets import QMainWindow

from mfixgui.gui import MfixGui
import mfixgui.file_menu.info_widget


@pytest.fixture
def gui(mocker, qtbot):
    """ MfixGui fixture """
    app = QMainWindow()
    mocker.patch("mfixgui.gui.os.chdir")
    gui_fixture = MfixGui(app)
    return gui_fixture


def test_update_project_info(mocker, qtbot):
    gui = mocker.Mock()
    gui.project.mfix_gui_comments = {"project_notes": json.dumps("my notes")}
    gui.get_project_file.return_value = "the_project_file"
    info_widget = mfixgui.file_menu.info_widget.InfoWidget(gui)

    info_widget.update_info()

    assert info_widget.project_notes() == "my notes"


@pytest.mark.xfail(run=False, reason="FIXME")
@patch("mfixgui.gui.datetime")
@patch("mfixgui.gui.getuser")
@patch("mfixgui.gui.time")
def test_comments_new_project(time, getuser, datetime, gui, mocker, tmpdir):
    """ tests reading new comments from new project from template """
    getuser.return_value = "curr_user"
    datetime.datetime.strftime.return_value = "2018-12-34 56:78"
    time.strftime.return_value = "2000-01-02 03:04"
    project_filename = join(tmpdir, "myfile.mfx")
    with open(project_filename, "w") as project_file:
        test_project = """ RUN_NAME="blank" """
        project_file.write(test_project)
    get_new_project_info = mocker.patch.object(gui, "get_new_project_info")
    get_new_project_info.return_value = (str(tmpdir), "the_run_name")

    gui.open_new_from_template(str(project_filename))

    assert gui.project.mfix_gui_comments.get("author") == "curr_user"
    assert gui.project.mfix_gui_comments.get("modified_by") == "curr_user"
    assert gui.project.mfix_gui_comments.get("created_date") == "2000-01-02 03:04"
    assert gui.project.mfix_gui_comments.get("modified_time") == "2018-12-34 56:78"


@pytest.mark.xfail(run=False, reason="FIXME")
@patch("mfixgui.gui.datetime")
@patch("mfixgui.gui.time")
def test_comments_existing(time, datetime, gui, tmpdir):
    """ tests reading existing comments from existing project file """
    datetime.datetime.strftime.return_value = "1918-11-11 11:11"
    project_filename = join(tmpdir, "myfile.mfx")
    with open(project_filename, "w") as project_file:
        test_project = """
RUN_NAME="testing123"
#!MFIX-GUI author = first_user
#!MFIX-GUI created_date = 2018-11-09 10:45
#!MFIX-GUI modified_by = later_user
#!MFIX-GUI modified_time = 2018-11-09 22:45
        """
        project_file.write(test_project)

    gui.open_project(str(project_filename))

    assert gui.project.mfix_gui_comments.get("author") == "first_user"
    assert gui.project.mfix_gui_comments.get("modified_by") == "later_user"
    assert gui.project.mfix_gui_comments.get("created_date") == "2018-11-09 10:45"
    assert gui.project.mfix_gui_comments.get("modified_time") == "1918-11-11 11:11"


@pytest.mark.xfail(run=False, reason="FIXME")
@patch("mfixgui.gui.datetime")
@patch("mfixgui.gui.getuser")
@patch("mfixgui.gui.time")
def test_comments_missing(time, getuser, datetime, gui, tmpdir):
    """ tests reading existing comments from existing project file """
    getuser.return_value = "curr_user"
    datetime.datetime.strftime.return_value = "1918-11-11 11:11"
    time.strftime.return_value = "1776-07-04 02:30"

    project_filename = join(tmpdir, "myfile.mfx")
    with open(project_filename, "w") as project_file:
        test_project = """
RUN_NAME="testing123"
        """
        project_file.write(test_project)

    gui.open_project(str(project_filename))

    assert gui.project.mfix_gui_comments.get("author") == "curr_user"
    assert gui.project.mfix_gui_comments.get("created_date") == "1776-07-04 02:30"
    assert gui.project.mfix_gui_comments.get("modified_by") is None
    assert gui.project.mfix_gui_comments.get("modified_time") == "1918-11-11 11:11"
