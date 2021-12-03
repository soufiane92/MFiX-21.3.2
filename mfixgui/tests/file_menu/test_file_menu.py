# -*- coding: utf-8 -*-
"""
Test the Export file menu item
"""

import os

from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import QMainWindow

from unittest.mock import call, patch

import pytest

from mfixgui.gui import MfixGui


@pytest.fixture
def mfix(mocker, qtbot):
    """ MfixGui without a project open """
    app = QMainWindow()
    mocker.patch("mfixgui.gui.os.chdir")
    gui = MfixGui(app)
    return gui


@pytest.fixture
def mfix_proj(mfix, tmpdir):
    """ MfixGui with a project open """
    project_filename = os.path.join(tmpdir, "myfile.mfx")
    with open(project_filename, "w") as project_file:
        test_project = """ RUN_NAME="blank" """
        project_file.write(test_project)
        mfix.open_project(project_filename)
    return mfix


@pytest.mark.xfail(run=False, reason="FIXME")
@patch("mfixgui.file_menu.SETTINGS")
def test_no_project_open(settings, mfix, mocker):
    """ What file menu items are enabled when no project is open """
    settings.value.return_value = ""
    mfix.set_project_file(None)

    mfix.open_file_menu()

    assert mfix.ui.new_project.enabled()
    assert mfix.ui.open_project.enabled()
    assert not mfix.ui.file_menu_back.isEnabled()
    assert not mfix.ui.export_project.enabled()
    assert not mfix.ui.info_item.enabled()
    assert not mfix.ui.save.enabled()
    assert not mfix.ui.save_as.enabled()

    assert mfix.new_project_widget == mfix.ui.stackedwidget_file_menu.currentWidget()


@pytest.mark.xfail(run=False, reason="FIXME")
def test_unsaved_project(mfix_proj, mocker):
    """ What file menu items are enabled when project not saved """
    mfix_proj.open_file_menu()

    assert mfix_proj.ui.export_project.enabled()
    assert mfix_proj.ui.new_project.enabled()
    assert mfix_proj.ui.open_project.enabled()
    assert mfix_proj.ui.info_item.enabled()
    assert mfix_proj.ui.file_menu_back.isEnabled()
    assert mfix_proj.ui.save.enabled()
    assert mfix_proj.ui.save_as.enabled()

    assert mfix_proj.info_widget == mfix_proj.ui.stackedwidget_file_menu.currentWidget()


@pytest.mark.xfail(run=False, reason="FIXME")
def test_saved_project(mfix_proj, mocker):
    """ What file menu items are enabled when project saved """
    mfix_proj.save_project()
    mfix_proj.open_file_menu()

    assert mfix_proj.ui.export_project.enabled()
    assert mfix_proj.ui.new_project.enabled()
    assert mfix_proj.ui.open_project.enabled()
    assert mfix_proj.ui.info_item.enabled()
    assert mfix_proj.ui.file_menu_back.isEnabled()
    assert not mfix_proj.ui.save.enabled()
    assert mfix_proj.ui.save_as.enabled()

    assert mfix_proj.info_widget == mfix_proj.ui.stackedwidget_file_menu.currentWidget()


@pytest.mark.xfail(run=False, reason="FIXME")
def test_ctrl_o_for_open(mfix, qtbot):
    mfix.open_file_menu()
    mfix.handle_open_shortcut()

    assert mfix.open_widget == mfix.ui.stackedwidget_file_menu.currentWidget()


@pytest.mark.xfail(run=False, reason="FIXME")
def test_ctrl_n_for_new(mfix, qtbot):
    mfix.open_file_menu()
    mfix.handle_new_shortcut()

    assert mfix.new_project_widget == mfix.ui.stackedwidget_file_menu.currentWidget()
