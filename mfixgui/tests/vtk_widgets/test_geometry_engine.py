from unittest.mock import patch

import pytest

from mfixgui.vtk_widgets import geometry_engine
from mfixgui.gui import MfixGui


@pytest.fixture
def mfix_gui(mocker, qtbot):
    """ MfixGui without a project open """
    app = mocker.Mock()
    gui = MfixGui(app)
    with patch.object(gui, "get_project_dir") as get_project_dir:
        get_project_dir.return_value = "/fake_projectdir"
        yield gui


@pytest.mark.xfail(run=False, reason="Hangs")
class TestGeometryEngineAddStl:
    """ Test the add_stl() method for GeometryEngine """

    @pytest.fixture
    def geom_engine(self, mfix_gui, mocker, add_stl):
        """ Mock the GeometryEngine and its project_dir """

        widget = geometry_engine.GeometryEngine(mfix_gui)
        with patch("mfixgui.vtk_widgets.geometry_engine.vtk") as vtk:
            yield widget

    @pytest.fixture
    def add_stl(self):
        with patch("mfixgui.vtk_widgets.geometry_engine.is_stl_ascii") as is_ascii:
            is_ascii.return_value = True
            with patch(
                "mfixgui.vtk_widgets.geometry_engine.purge_multi_solids"
            ) as purge:
                purge.return_value = "STL_Filename"
                yield

    @pytest.mark.parametrize(
        "stl_fname,orig_fname",
        [
            ("/abc/geometry.stl", "/abc/geometry.stl.original"),
            ("${proj_dir}/geometry.stl", "${proj_dir}/geometry.stl.original"),
        ],
    )
    @patch("mfixgui.vtk_widgets.geometry_engine.shutil.copy")
    def test_add_stl_geometry_stl(self, shutil_cp, stl_fname, orig_fname, geom_engine):
        """ Test adding an STL file as an argument """
        name = geom_engine.add_stl(stl_fname)

        shutil_cp.assert_called_with(stl_fname, orig_fname)
        assert name == "stl_filename"

    @pytest.mark.parametrize(
        "stl_fname,orig_fname",
        [
            ("/abc/some_geometry.stl", "/abc/some_geometry.stl"),
            ("${proj_dir}/some_geometry.stl", "/fake_projectdir/some_geometry.stl"),
        ],
    )
    @patch("mfixgui.vtk_widgets.geometry_engine.shutil.copy")
    def test_add_stl(self, shutil_cp, stl_fname, orig_fname, geom_engine):
        """ Test adding an STL file as an argument """

        name = geom_engine.add_stl(stl_fname)

        shutil_cp.assert_not_called()
        assert name == "stl_filename"
