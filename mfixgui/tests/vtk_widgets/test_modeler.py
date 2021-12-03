import os.path

import pytest
from unittest.mock import patch

from mfixgui.vtk_widgets import modeler
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
class TestModelerAddStl:
    """ Test the add_stl() method for VtkWidget """

    @pytest.fixture
    def mock_vtk(self):
        with patch("mfixgui.vtk_widgets.base.QVTKRenderWindowInteractor") as qvtk_rend:
            with patch("mfixgui.vtk_widgets.base.QtWidgets.QGridLayout") as qgridlayout:
                with patch("mfixgui.vtk_widgets.base.vtk") as base_vtk:
                    with patch("mfixgui.vtk_widgets.modeler.vtk") as vtk:
                        yield

    @pytest.fixture
    def vtk_widget(self, mfix_gui, mock_vtk, mocker):
        """ Mock the VtkWidget and its project_dir """
        widget = modeler.VtkWidget(mfix_gui)
        trans_filt = mocker.Mock()
        widget.geometrydict["new_stl_name"] = {
            "transformfilter": trans_filt,
            "visible": False,
        }
        yield widget

    @pytest.fixture
    def add_stl(self):
        with patch("mfixgui.vtk_widgets.modeler.GeometryEngine.add_stl") as _add_stl:
            _add_stl.return_value = "new_stl_name"
            yield _add_stl

    def test_add_stl(self, add_stl, vtk_widget, tmpdir):
        """ Test adding an STL file as an argument """
        with patch.object(modeler.os.path, "exists", return_value=True):

            vtk_widget.add_stl(os.path.join(tmpdir, "some_geometry.stl"))

            add_stl.assert_called_with(
                vtk_widget,
                os.path.join(tmpdir, "some_geometry.stl"),
                None,
                None,
                False,
                "/fake_projectdir",
            )

    def test_add_stl_projdir(self, add_stl, vtk_widget, tmpdir):
        """ Test adding an STL file as an argument """
        with patch.object(modeler.os.path, "exists", return_value=True):

            vtk_widget.add_stl(os.path.join(tmpdir, "${proj_dir}/some_geom.stl"))

            add_stl.assert_called_with(
                vtk_widget,
                os.path.join(tmpdir, "fake_projectdir", "some_geom.stl"),
                None,
                None,
                False,
                "/fake_projectdir",
            )

    @patch("mfixgui.vtk_widgets.modeler.QFileDialog.getOpenFileName")
    @patch("mfixgui.vtk_widgets.modeler.GUI.message")
    @patch("mfixgui.vtk_widgets.modeler.shutil.copy")
    def test_user_add_stl(self, shutil_copy, message, get_open, add_stl, vtk_widget):
        """ Test None as the STL file argument, and the user selecting the STL file """
        get_open.return_value = ("/abc/some_geom.stl", None)
        message.return_value = "yes"

        with patch.object(modeler.os.path, "exists", return_value=True):
            vtk_widget.add_stl()

            shutil_copy.assert_called_with(
                "/abc/some_geom.stl", "/fake_projectdir/some_geom.stl"
            )
            add_stl.assert_called_with(
                vtk_widget,
                "/fake_projectdir/some_geom.stl",
                None,
                None,
                False,
                "/fake_projectdir",
            )
