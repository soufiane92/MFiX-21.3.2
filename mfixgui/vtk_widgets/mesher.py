"""
This is an experiment, trying to use methods in vtk to mesh an stl.
"""

import os
import vtk

from vtk.constants import CELL_TYPE_ENUM

from mfixgui.vtk_widgets.tools import safe_float
from mfixgui.vtk_widgets.constants import DEFAULT_MESH_NAME

GUI = None

class Mesher(object):
    def __init__(self):
        self.mesh_actor = None
        self.mesh_mapper = None

    def export_unstructured(self, fname, grid):
        """export an unstructured grid"""
        gw = vtk.vtkXMLUnstructuredGridWriter()
        gw.SetFileName(fname)
        gw.SetInputConnection(grid)
        gw.Write()

    def vtk_calc_distance_from_geometry(self):
        """for every point in the mesh, calculate the distance from the
        toplevel geometry"""
        source = self.collect_toplevel_geometry()

        if source:
            signed_distances = vtk.vtkFloatArray()
            signed_distances.SetNumberOfComponents(1)
            signed_distances.SetName("SignedDistances")

            implicit_poly_data_distance = vtk.vtkImplicitPolyDataDistance()

            implicit_poly_data_distance.SetInput(source.GetOutput())

            # Evaluate the signed distance function at all of the grid points
            for point_id in range(self.rectilinear_grid.GetNumberOfPoints()):
                p = self.rectilinear_grid.GetPoint(point_id)
                signed_distance = implicit_poly_data_distance.EvaluateFunction(p)
                signed_distances.InsertNextValue(signed_distance)

            self.rectilinear_grid.GetPointData().SetScalars(signed_distances)

    def vtk_mesher_table_based(self):
        """use vtkTableBasedClipDataSet to slice the background mesh with the
        toplevel geometry"""
        self.vtk_calc_distance_from_geometry()

        clipper = vtk.vtkTableBasedClipDataSet()
        clipper.SetInputData(self.rectilinear_grid)
        if self.ui.mesh.checkbox_mesh_inside.isChecked():
            clipper.InsideOutOn()
        else:
            clipper.InsideOutOff()

        clipper.SetMergeTolerance(
            safe_float(self.ui.mesh.lineedit_vtk_mesh_merge.text())
            )
        clipper.SetValue(0.0)
        clipper.Update()
        self.export_mesh(clipper)

        self.mesh = clipper.GetOutput()
        self.show_mesh()
        self.mesh_stats()

    def vtk_mesher_threshold(self):
        """use vtkThreshold to slice the background mesh with the
        toplevel geometry"""
        self.vtk_calc_distance_from_geometry()

        thresh = vtk.vtkThreshold()
        thresh.SetInputData(self.rectilinear_grid)

        if self.ui.mesh.checkbox_threshold_inside.isChecked():
            thresh.ThresholdByLower(0)
        else:
            thresh.ThresholdByUpper(0)

        if self.ui.mesh.checkbox_threshold_interface.isChecked():
            thresh.AllScalarsOff()
        else:
            thresh.AllScalarsOn()

        thresh.Update()
        self.export_mesh(thresh)

        self.mesh = thresh.GetOutput()
        self.show_mesh()
        self.mesh_stats()

    def export_mesh(self, mesh, name=DEFAULT_MESH_NAME):
        """export the mesh"""
        global GUI
        project_dir = GUI.get_project_dir()
        self.export_unstructured(os.path.join(project_dir, name),
                                 mesh.GetOutputPort())

    def mesh_stats(self):
        """calculate mesh statistics"""
        cell_count = self.mesh.GetNumberOfCells()
        cell_types = self.mesh.GetCellTypesArray()

        if cell_types is None or cell_count is None:
            return

        cell_list = []
        for i in range(cell_types.GetNumberOfTuples()):
            cell_list.append(cell_types.GetTuple(i))

        cell_type_counts = []
        for cell in set(cell_list):
            cell_type_counts.append([
                CELL_TYPE_ENUM[int(cell[0])],
                cell_list.count(cell)
                ])

        self.ui.mesh.lineedit_mesh_cells.setText(str(cell_count))

        self.ui.mesh.plaintextedit_mesh_cell_types.setPlainText(
            '\n'.join([':\t'.join([str(cell_type), str(cells)])
                       for cell_type, cells in cell_type_counts])
            )

    def do_mesh(self):
        """mesh the geometry"""
        mesher = str(self.ui.mesh.combobox_mesher.currentText())

        if mesher == 'vtkTableBasedClipDataSet':
            self.vtk_mesher_table_based()
        elif mesher == 'vtkThreshold':
            self.vtk_mesher_threshold()

    def remove_mesh(self, name=DEFAULT_MESH_NAME):
        """remove the mesh from the vtk scene as well as the file"""
        global GUI
        project_dir = GUI.get_project_dir()
        path = os.path.join(project_dir, name)
        if not os.path.exists(path):
            return
        key = GUI.message(text='Remove %s?' % name, buttons=['yes', 'no'], default='no')
        if key == 'no':
            return

        os.remove(path)

        if self.mesh_actor is not None:
            self.vtkrenderer.RemoveActor(self.mesh_actor)
            self.mesh_actor = None

    def set_mesh_actor_props(self):
        """change the actor properties of the mesh"""
        props = self.visual_props['mesh']
        self.set_representation(self.mesh_actor, props['rep'])
        self.mesh_actor.GetProperty().SetColor(props['color'].getRgbF()[:3])
        self.mesh_actor.GetProperty().SetEdgeColor(props['edge'].getRgbF()[:3])
        self.mesh_actor.SetVisibility(int(props['visible']))

    def show_mesh(self):
        """show the mesh"""
        if self.mesh is None:
            return

        if self.mesh_actor is not None:
            self.vtkrenderer.RemoveActor(self.mesh_actor)
        self.mesh_mapper = vtk.vtkDataSetMapper()
        self.mesh_mapper.ScalarVisibilityOff()
        self.mesh_mapper.SetInputData(self.mesh)
        self.mesh_actor = vtk.vtkActor()
        self.mesh_actor.SetMapper(self.mesh_mapper)
        self.vtkrenderer.AddActor(self.mesh_actor)
        self.set_mesh_actor_props()
        self.render()
