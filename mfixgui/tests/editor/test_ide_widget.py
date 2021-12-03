import pytest

from mfixgui.gui import MfixGui


@pytest.fixture
def mfix_gui(mocker, qtbot):
    app = mocker.Mock()
    gui = MfixGui(app)
    return gui


@pytest.mark.xfail(run=False, reason="FIXME")
def test_show_location(mfix_gui):
    mfix_gui.editor_widget.show_location("mfix.f", 10)
