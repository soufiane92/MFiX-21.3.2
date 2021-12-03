import os
import types
from unittest.mock import patch

import pytest

import mfixgui.file_menu.save_as_widget
from mfixgui.gui import MfixGui
from mfixgui.tools import get_mfix_templates, get_run_name_from_file


@pytest.fixture
def mock_dialogs(tmpdir):
    with patch("mfixgui.gui.NewProjectDialog.get_name_and_location") as name_loc:
        name_loc.return_value = ("THE_RUN_NAME", tmpdir)
        yield


@pytest.fixture
def mfix_gui(mocker, qtbot):
    """ MfixGui without a project open """
    app = mocker.Mock()
    gui = MfixGui(app)
    yield gui


@pytest.fixture
def save_as_widget(mfix_gui):
    """" Mock of MfixGui instance """
    widget = mfixgui.file_menu.save_as_widget.SaveAsWidget(mfix_gui)
    yield widget


@pytest.mark.xfail(run=False, reason="FIXME")
def test_save_as(mfix_gui, mock_dialogs, tmpdir, save_as_widget):
    template_mfx = "tutorials/dem/fluid_bed_2d/fluid_bed_dem_2d.mfx"
    template = os.path.join(get_mfix_templates(), template_mfx)
    mfix_gui.open_new_from_template(template)
    new_save_as_filename = str(tmpdir) + "/saved_as_project.mfx"
    save_as_widget.label_filename.setText(new_save_as_filename)

    save_as_widget.save_as()

    run_name = get_run_name_from_file(new_save_as_filename)
    assert run_name == "saved_as_project"
