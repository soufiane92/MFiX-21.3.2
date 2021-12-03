# -*- coding: utf-8 -*-
"""
run with pytest
"""

import pytest

from mfixgui.gui import MfixGui


@pytest.fixture
def mfix_gui(mocker, qtbot):
    app = mocker.Mock()
    gui = MfixGui(app)
    return gui


@pytest.mark.xfail(run=False, reason="Hangs")
def test_setup_current_pane(mocker, mfix_gui):
    def make_mock(txt):
        mock = mocker.Mock()
        mock.text.return_value = txt
        return [mock]

    tree_nav = mfix_gui.ui.treewidget_navigation
    selectedItems = mocker.patch.object(tree_nav, "selectedItems")

    selectedItems.return_value = make_mock("regions")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("model_setup")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("solids")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("initial_conditions")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("boundary_conditions")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("point_sources")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("internal_surfaces")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("chemistry")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("numerics")
    mfix_gui.setup_current_pane()
    selectedItems.return_value = make_mock("output")
    mfix_gui.setup_current_pane()
