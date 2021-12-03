# -*- coding: utf-8 -*-
"""
run with pytest
"""

import pytest

from unittest.mock import patch

from mfixgui.monitors import Monitors


@pytest.fixture
def monitors(mocker):
    monitors = Monitors()

    monitors.ui = None
    mocker.patch.object(monitors, 'ui')

    monitors.project = None
    mocker.patch.object(monitors, 'project')

    monitors.fluid_solver_disabled = None
    mocker.patch.object(monitors, 'fluid_solver_disabled')

    monitors.fluid_species = None
    mocker.patch.object(monitors, 'fluid_species')

    monitors.update_nav_tree = None
    mocker.patch.object(monitors, 'update_nav_tree')

    monitors.update_graphics_tab = None
    mocker.patch.object(monitors, 'update_graphics_tab')

    monitors.ui.monitors.tablewidget_regions.rowCount.return_value = 5

    monitors.init_monitors()
    return monitors


@patch('mfixgui.monitors.get_selected_row')
def test_monitors(get_selected_row, monitors, mocker):
    monitors.update_monitors = None

    monitors.monitors_delete_regions()
