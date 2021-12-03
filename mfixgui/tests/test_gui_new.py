import os.path
from unittest.mock import patch

import pytest

from mfixgui.gui import MfixGui
from mfixgui.tools import find_project_file, get_mfix_templates


@pytest.fixture
def mfix_gui(mocker, qtbot):
    app = mocker.Mock()
    gui = MfixGui(app)
    return gui


@pytest.fixture
def mock_dialogs(tmpdir):
    with patch("mfixgui.gui.NewProjectDialog.get_name_and_location") as name_loc:
        name_loc.return_value = ("THE_RUN_NAME", tmpdir)
        yield


@pytest.mark.xfail(run=False, reason="Hangs")
@pytest.mark.parametrize(
    "template_mfx",
    [
        "tutorials/dem/fluid_bed_2d/fluid_bed_dem_2d.mfx",
        "tutorials/dem/hopper_3d/hopper_dem_3d.mfx",
        "tutorials/dem/hopper_pseudo_2d/hopper_dem_pseudo_2d.mfx",
        "tutorials/dem/loop_seal_pseudo_2d/loop_seal_dem_pseudo_2d.mfx",
        "tutorials/dem/spouted_bed_pseudo_2d/spouted_bed_dem_pseudo_2d.mfx",
        "tutorials/fld/vortex_shedding_2d/vortex_shedding_fld_2d.mfx",
        "tutorials/pic/cyclone_3d/cyclone_pic_3d.mfx",
        "tutorials/pic/hopper_3d/hopper_pic_3d.mfx",
        "tutorials/pic/loop_seal_3d/loop_seal_pic_3d.mfx",
        "tutorials/pic/silane_pyrolysis_3d/silane_pyrolysis_pic_3d.mfx",
        "tutorials/pic/spouted_bed_3d/spouted_bed_pic_3d.mfx",
        "tutorials/tfm/fluid_bed_2d/fluid_bed_tfm_2d.mfx",
        "tutorials/tfm/fluid_bed_3d/fluid_bed_tfm_3d.mfx",
        "tutorials/tfm/hopper_3d/hopper_tfm_3d.mfx",
        "tutorials/tfm/hopper_pseudo_2d/hopper_tfm_pseudo_2d.mfx",
        "tutorials/tfm/silane_pyrolysis_2d/silane_pyrolysis_tfm_2d.mfx",
    ],
)
def test_new_from_template(template_mfx, mock_dialogs, mfix_gui):
    template = os.path.join(get_mfix_templates(), template_mfx)

    mfix_gui.open_new_from_template(template)
