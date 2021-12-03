import pytest

from PyQt5.QtWidgets import QTabWidget

from mfixgui.graphic_tabs import GraphicTabs


@pytest.fixture
def graphics_tabs(qtbot, mocker):
    graphics_tabs = GraphicTabs()

    graphics_tabs.ui = None
    mocker.patch.object(graphics_tabs, 'ui')
    graphics_tabs.ui.tabWidgetGraphics = QTabWidget()

    graphics_tabs.set_unsaved_flag = None
    mocker.patch.object(graphics_tabs, 'set_unsaved_flag')

    graphics_tabs.monitor_reader = None
    mocker.patch.object(graphics_tabs, 'monitor_reader')

    graphics_tabs.init_graphics_tabs(False)
    return graphics_tabs


def test_dt_plot(graphics_tabs):
    """ test adding each type of plot tab """
    new_tab = graphics_tabs.add_tab()

    new_tab.create_plot_widget("DT")

    assert len(graphics_tabs.plot_dict) == 1
    assert (
        graphics_tabs.graphics_to_str()
        == '{"order": ["DT"], "data": {"DT": {"plot": true, "vtk": false, "monitors": false}}}'
    )


def test_time_plot(graphics_tabs):
    new_tab = graphics_tabs.add_tab()

    new_tab.create_plot_widget("Simulation time")

    assert len(graphics_tabs.plot_dict) == 1
    assert (
        graphics_tabs.graphics_to_str()
        == '{"order": ["Simulation time"], "data": {"Simulation time": {"plot": true, "vtk": false, "monitors": false}}}'
    )


def test_nit_plot(graphics_tabs):
    new_tab = graphics_tabs.add_tab()

    new_tab.create_plot_widget("NIT")

    assert len(graphics_tabs.plot_dict) == 1
    assert (
        graphics_tabs.graphics_to_str()
        == '{"order": ["NIT"], "data": {"NIT": {"plot": true, "vtk": false, "monitors": false}}}'
    )


def test_residuals_plot(graphics_tabs):
    new_tab = graphics_tabs.add_tab()

    new_tab.create_plot_widget("Residuals")

    assert len(graphics_tabs.plot_dict) == 1
    assert (
        graphics_tabs.graphics_to_str()
        == '{"order": ["Residuals"], "data": {"Residuals": {"plot": true, "vtk": false, "monitors": false}}}'
    )


def test_remove_plot_widget(graphics_tabs):
    new_tab = graphics_tabs.add_tab()
    new_tab.create_plot_widget("Residuals")
    assert len(graphics_tabs.plot_dict) == 1
    # new_tab.ui.tabWidgetGraphics.tab.monitors = some_monitor

    graphics_tabs.remove_tab(0)

    assert len(graphics_tabs.plot_dict) == 1
    assert (
        graphics_tabs.graphics_to_str()
        == '{"order": ["New"], "data": {"New": {"plot": false, "vtk": false, "monitors": false}}}'
    )


def test_load_plot_widget(graphics_tabs):
    """ test loading plots from saved JSON """
    json_str = '{"order": ["toolButton_dt"], "data": {"toolButton_dt": {"plot": true, "vtk": false, "monitors": false}}}'
    graphics_tabs.graphics_from_str(json_str)


@pytest.mark.skip
def test_create_new_tab(mfix_gui, mocker):
    tab = mfix_gui.add_tab(name="New")
    tab.create_vtk_widget(False)


def test_monitor_plot(graphics_tabs):
    graphics_tabs._tab_changed(0)
    graphics_tabs.ui.tabWidgetGraphics.widget(0).create_monitor_plot_widget()
