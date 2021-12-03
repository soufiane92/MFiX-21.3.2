"""
This is the vtk widget. It handles displaying the 3D graphics in the GUI as
well as a simple geometry creation tool and region selection.
"""

import copy
import os.path
import shutil
import logging
import decimal

from functools import partial
from qtpy import QtCore, QtGui
from qtpy.QtWidgets import (
    QFileDialog,
    QColorDialog,
    QCheckBox,
    QTreeWidgetItem,
    QAction,
    QMenu,
    QLineEdit,
    QToolButton,
)

from mfixgui.animations import animate_stacked_widget
from mfixgui.constants import CONVERSION_TO_METERS
from mfixgui.tools.qt import (widget_iter, get_icon, sub_icon_size,
                              get_image_path, deepcopy_dict, get_ui,
                              get_separator)
from mfixgui.tools import get_unique_string
from mfixgui.widgets.base import LineEdit, ComboBox
from mfixgui.project import Equation, ExtendedJSON
from mfixgui.vtk_widgets import VTK_IMPORT_INFO
from mfixgui.vtk_widgets.base import BaseVtkWidget, vtk
from mfixgui.vtk_widgets.tools import (
    safe_float, safe_int, clean_visual_dict,
    is_stl_ascii, purge_multi_solids, remove_vtk_objects)
from mfixgui.vtk_widgets.geometry_engine import GeometryEngine
from mfixgui.colormaps.color_maps import build_vtk_lookup_tables, build_qicons

LOOKUP_TABLES = build_vtk_lookup_tables()


try:
    from mfixgui.vtk_widgets.wizards import (
        DistributionWizard, CycloneWizard, ReactorWizard, HopperWizard)
except ImportError:
    logging.getLogger(__name__).warning("Unable to import VTK wizards")

try:
    from mfixgui.vtk_widgets.constants import (
        DEFAULT_IMPLICIT_PARAMS, DEFAULT_VISUAL_PROPS, FILTER_DICT,
        IMPLICIT_DICT, PARAMETRIC_DICT, PRIMITIVE_DICT,
        PROCEDURAL_DICT)
except ImportError:
    DEFAULT_MESH_NAME = 'geometry.stl'
    VTK_IMPORT_INFO.append('Can not import VTK constants.')

GUI = None
LOG = logging.getLogger(__name__)
CONVERSION = copy.deepcopy(CONVERSION_TO_METERS)
CONVERSION['custom'] = 0


def custom_round(value, decimals, rounding='ceiling'):
    with decimal.localcontext() as ctx:
        d = decimal.Decimal(value)
        if rounding == 'ceiling':
            ctx.rounding = decimal.ROUND_CEILING
        else:
            ctx.rounding = decimal.ROUND_FLOOR
        return round(d, decimals)


def dict_iter(d):
    """iterator to recursively iterate over dict of dicts"""
    for key, val in d.items():
        if isinstance(val, dict):
            for (key2, dep) in dict_iter(val):
                yield key2, dep
        yield key, val


def rec_key_replace(new, old, obj):
    if isinstance(obj, dict):
        return dict((new if key == old else key, rec_key_replace(new, old, val)) for key, val in obj.items())
    return obj


def rec_val_replace(obj, key, replace_value):
    for k, v in obj.items():
        if isinstance(v, dict):
            obj[k] = rec_val_replace(v, key, replace_value)
    if key in obj:
        obj[key] = replace_value
    return obj


class VtkWidget(BaseVtkWidget, GeometryEngine):
    """the vtk widget"""

    value_updated = QtCore.Signal(object, object, object)

    def __init__(self, parent=None):
        BaseVtkWidget.__init__(self, parent)
        GeometryEngine.__init__(self, parent, parent.project)

        global GUI
        GUI = parent

        self.ui = parent.ui
        ui = self.ui.geometry
        self.geometrytree_widget = self.ui.geometry.treeWidgetGeometry

        # --- data ---
        self.animate = True
        self.region_dict = {}
        self.needs_saved = False
        self.geometrydict = {}
        self.boundary_dict = {}
        self.mesh_dict = {}
        self.bc_id_lookup_table = None

        self.booleanbtndict = {
            'union':        self.ui.geometry.toolbutton_geometry_union,
            'intersection': self.ui.geometry.toolbutton_geometry_intersect,
            'difference':   self.ui.geometry.toolbutton_geometry_difference,
            }
        self.visual_props = deepcopy_dict(DEFAULT_VISUAL_PROPS, qobjects=True)

        self.rectilinear_grid = vtk.vtkRectilinearGrid()
        self.grid_viewer_dict = {
            'filters': [],
            'mappers': [],
            'actors':  [],
            }

        self.cell_spacing_widgets = [
            self.ui.mesh.lineedit_mesh_cells_size_x,
            self.ui.mesh.lineedit_mesh_cells_size_y,
            self.ui.mesh.lineedit_mesh_cells_size_z
            ]

        # enable parameters
        for widget in widget_iter(self.ui.geometry.groupBoxGeometryParameters):
            if isinstance(widget, LineEdit):
                widget.allow_parameters = True

        # ui file widget tweaks
        ui.combobox_stl_units.clear()
        ui.combobox_stl_units.addItems(list(CONVERSION.keys()))
        ui.combobox_stl_units.setCurrentText('m')
        ui.combobox_stl_units.currentIndexChanged.connect(self.handle_stl_units)

        # --- setup vtk mappers/actors ---
        self.glyph = self.glyph_actor = None

        # set types on lineedits
        for widget in widget_iter(self.ui.geometry.stackedWidgetGeometryDetails):
            if isinstance(widget, LineEdit):
                parameter = str(widget.objectName()).lower()
                if any([s in parameter for s in ['divisions', 'resolution',
                                                 'nhills', 'iterations']]):
                    widget.dtype = int
                else:
                    widget.dtype = float

        self.init_base_toolbar()
        self.__add_toolbar_btns()
        self.__connect_events()
        self.__add_sidebar()
        self.__setup_wizards()

        # file system watcher
        self.dir_watcher = QtCore.QFileSystemWatcher()
        self.dir_watcher.directoryChanged.connect(self.file_watcher_dir_changed)
        self.file_watcher = QtCore.QFileSystemWatcher()
        self.file_watcher.fileChanged.connect(self.file_watcher_file_changed)

        self.timer = QtCore.QTimer()
        self.watching_file = False

        # make sure widget stack is at 0
        self.ui.geometry.stackedWidgetGeometryDetails.setCurrentIndex(0)

    # --- setup ---
    def __add_toolbar_btns(self):

        sep = get_separator(parent=self.button_bar)
        self.button_bar_layout.addWidget(sep)

        self.toolbat_geo_add_btns = []

        # stl
        tb = QToolButton()
        tb.clicked.connect(lambda _ignore: self.add_stl())
        tb.setIcon(get_icon('geometry.svg'))
        tb.setIconSize(sub_icon_size())
        tb.setToolTip('Add a STL file to the scene')
        tb.setAutoRaise(True)
        tb.setFocusPolicy(QtCore.Qt.ClickFocus)
        self.toolbat_geo_add_btns.append(tb)
        self.button_bar_layout.addWidget(tb)

        # implicit functions
        for name, geo in [
                ('sphere', 'sphere'),
                ('box', 'box'),
                ('cylinder', 'cylinder'),
                ('cone', 'cone'),
                ('torus', 'superquadric'),
                ]:
            tb = QToolButton()
            tb.clicked.connect(partial(self.add_implicit, implicittype=geo))
            tb.setIcon(get_icon(name + '.svg'))
            tb.setIconSize(sub_icon_size())
            tb.setToolTip('Add a {} to the scene'.format(name))
            tb.setAutoRaise(True)
            tb.setFocusPolicy(QtCore.Qt.ClickFocus)
            self.toolbat_geo_add_btns.append(tb)
            self.button_bar_layout.addWidget(tb)

    def __connect_events(self):

        # --- Geometry Tree ---
        self.geometrytree_widget.setStyleSheet(
            "QTreeView::indicator:unchecked {image: url(%s);}"
            "QTreeView::indicator:checked {image: url(%s);}"
            % (get_image_path('visibilityofflight.svg'),
               get_image_path('visibility.svg'))
            )
        self.geometrytree_widget.itemSelectionChanged.connect(
            self.selected_geometry_changed)
        self.geometrytree_widget.itemClicked.connect(self.geometry_clicked)

        # --- geometry button ---
        self.add_geometry_menu = QMenu(self)
        self.ui.geometry.toolbutton_add_geometry.setMenu(
            self.add_geometry_menu)

        action = QAction('STL File', self.add_geometry_menu)
        action.triggered.connect(lambda _ignore: self.add_stl())
        action.setIcon(get_icon('geometry.svg'))
        action.setIconVisibleInMenu(True)
        self.add_geometry_menu.addAction(action)

        # --- procedural functions ---
        proc_menu = self.add_geometry_procedural= QMenu(self)
        proc_menu.setTitle('Procedural')
        proc_menu.setIcon(get_icon('geometry.svg'))
        a = self.add_geometry_menu.addMenu(proc_menu)
        a.setIconVisibleInMenu(True)
        for geo in PROCEDURAL_DICT.keys():
            action = QAction(geo.replace('_', ' '), proc_menu)
            action.triggered.connect(partial(self.add_procedural, proceduraltype=geo))
            action.setIcon(get_icon('geometry.svg'))
            action.setIconVisibleInMenu(True)
            proc_menu.addAction(action)

        # --- implicit functions ---
        p_menu = self.add_geometry_implicit = QMenu(self)
        p_menu.setTitle('Implicits')
        p_menu.setIcon(get_icon('function.svg'))
        a = self.add_geometry_menu.addMenu(p_menu)
        a.setIconVisibleInMenu(True)
        for geo in IMPLICIT_DICT.keys():
            action = QAction(geo.replace('_', ' '), p_menu)
            action.triggered.connect(partial(self.add_implicit, implicittype=geo))
            action.setIcon(get_icon('function.svg'))
            action.setIconVisibleInMenu(True)
            p_menu.addAction(action)

        # --- primitives ---
        p_menu = self.add_geometry_primitive = QMenu(self)
        p_menu.setTitle('Primitives')
        p_menu.setIcon(get_icon('geometry.svg'))
        a = self.add_geometry_menu.addMenu(p_menu)
        a.setIconVisibleInMenu(True)
        for geo in PRIMITIVE_DICT.keys():
            action = QAction(geo, p_menu)
            action.triggered.connect(partial(self.add_primitive, primtype=geo))
            action.setIcon(get_icon('geometry.svg'))
            action.setIconVisibleInMenu(True)
            p_menu.addAction(action)

        # --- parametric ---
        p_menu = self.add_geometry_parametric = QMenu(self)
        p_menu.setTitle('Parametrics')
        p_menu.setIcon(get_icon('geometry.svg'))
        a = self.add_geometry_menu.addMenu(p_menu)
        a.setIconVisibleInMenu(True)
        for geo in PARAMETRIC_DICT.keys():
            action = QAction(geo.replace('_', ' '), p_menu)
            action.triggered.connect(partial(self.add_parametric, paramtype=geo))
            action.setIcon(get_icon('geometry.svg'))
            action.setIconVisibleInMenu(True)
            p_menu.addAction(action)

        # --- filter button ---
        self.add_filter_menu = QMenu(self)
        self.ui.geometry.toolbutton_add_filter.setMenu(self.add_filter_menu)
        self.add_filter_menu.aboutToShow.connect(self.enable_filters)
        self.filter_actions = []
        for geo in FILTER_DICT.keys():
            action = QAction(geo.replace('_', ' '), self.add_filter_menu)
            action.triggered.connect(partial(self.add_filter, filtertype=geo))
            action.setIcon(get_icon('filter.svg'))
            action.setIconVisibleInMenu(True)
            self.filter_actions.append(action)
            self.add_filter_menu.addAction(action)

        # setup signals
        self.ui.geometry.toolbutton_remove_geometry.clicked.connect(
            self.remove_geometry)
        self.ui.geometry.toolbutton_copy_geometry.clicked.connect(
            self.handle_copy_geometry)

        # connect boolean
        for key, btn in self.booleanbtndict.items():
            btn.clicked.connect(lambda ignore, k=key: self.boolean_operation(k))

        # connect parameter widgets
        for widget in widget_iter(
                self.ui.geometry.stackedWidgetGeometryDetails):
            if isinstance(widget, QLineEdit):
                widget.editingFinished.connect(partial(self.handle_widget_value_changed, widget))
            elif isinstance(widget, QCheckBox):
                widget.stateChanged.connect(lambda i, w=widget: self.handle_widget_value_changed(w))

        # connect object name change
        self.ui.geometry.lineedit_object_name.editingFinished.connect(self.change_object_name)

        # boolean flip order
        self.ui.geometry.pushbutton_boolean_fliporder.clicked.connect(self.boolean_flip_order)

        # --- mesh ---
        self.ui.geometry.pushbutton_mesh_autosize.clicked.connect(
            self.auto_size_mesh_extents)

    def hide_show_sidebar(self, visible=None):
        """hide/show the sidebar widgets"""
        sb = self.sidebar
        if visible is None:
            visible = not sb.widget_geometry.isVisible()

        tb = sb.toolButton_hide_show
        tb.setIcon(get_icon('right.svg' if visible else 'left.svg'))
        tb.setToolTip('Collapse sidebar' if visible else 'Open sidebar')
        tb.setIconSize(sub_icon_size())
        tb.setDown(False)

        for wid in [sb.widget_geometry, sb.widget_bkgr_mesh, sb.widget_regions,
                    sb.label_axes, sb.widget_boundary, sb.widget_mesh]:
            wid.setVisible(visible)

    def __add_sidebar(self):
        """build the sidebar widgets"""

        sb = self.sidebar = get_ui('modeler_sidebar.ui')
        self.grid_layout.addWidget(self.sidebar, 1, 1)

        # hide/Show
        sb.toolButton_hide_show.setIcon(get_icon('right.svg'))
        sb.toolButton_hide_show.clicked.connect(lambda ignore: self.hide_show_sidebar())
        sb.toolButton_hide_show.setIconSize(sub_icon_size())

        # vis btns
        sb.toolButton_geometry_vis.setIcon(get_icon('geometry.svg'))
        sb.toolButton_bkgr_mesh_vis.setIcon(get_icon('grid.svg'))
        sb.toolButton_regions_vis.setIcon(get_icon('all_region.svg'))
        sb.toolButton_axes_vis.setIcon(get_icon('axes.svg'))
        sb.toolButton_boundary_vis.setIcon(get_icon('border_outer.svg'))
        sb.toolButton_mesh_vis.setIcon(get_icon('mesh.svg'))

        # toggles
        def hide_show_options_widget(tb, opt_widget):
            visible = not opt_widget.isVisible()
            opt_widget.setVisible(visible)
            tb.setArrowType(QtCore.Qt.UpArrow if visible else QtCore.Qt.DownArrow)

        for tb, opt in [
                (sb.toolButton_geometry_toggle, sb.widget_geometry_opts),
                (sb.toolButton_bkgr_mesh_toggle, sb.widget_bkgr_mesh_opts),
                (sb.toolButton_regions_toggle, sb.widget_regions_opts),
                (sb.toolButton_boundary_toggle, sb.widget_boundary_opts),
                (sb.toolButton_mesh_toggle, sb.widget_mesh_opts),
                ]:
            tb.clicked.connect(
                lambda down, tb=tb, w=opt: hide_show_options_widget(tb, w))

        # visible buttons
        geo_list = ['geometry', 'background_mesh', 'regions', 'boundary', 'mesh',
                    'normals', 'axes']
        vbtns = self.visual_btns = dict((k, {}) for k in geo_list)
        for tb, geo in zip([sb.toolButton_geometry_vis,
                            sb.toolButton_bkgr_mesh_vis,
                            sb.toolButton_regions_vis,
                            sb.toolButton_boundary_vis,
                            sb.toolButton_mesh_vis,
                            sb.checkBox_geometry_normals,
                            sb.toolButton_axes_vis,
                            ], geo_list):
            tb.clicked.connect(lambda ignore, g=geo, t=tb: self.change_visibility(g, t))
            tb.setCheckable(True)
            tb.setChecked(True)
            tb.setIconSize(sub_icon_size())
            vbtns[geo]['visible'] = tb

        for cb, geo in zip([sb.comboBox_geometry_style,
                            sb.comboBox_bkgr_mesh_style,
                            sb.comboBox_regions_style,
                            sb.comboBox_boundary_style,
                            sb.comboBox_mesh_style,
                            ], geo_list):
            cb.activated.connect(lambda ignore, g=geo, c=cb: self.change_representation(g, c))
            vbtns[geo]['rep'] = cb

        for ct, geo in zip([sb.toolButton_geometry_color,
                            sb.toolButton_bkgr_mesh_color], geo_list):
            ct.clicked.connect(lambda ignore, g=geo, t=ct: self.change_color(g, t))
            vbtns[geo]['color'] = ct

        for sp, geo in zip([sb.doubleSpinBox_geomtry_opactiy,
                            sb.doubleSpinBox_bkgr_mesh_opactity,
                            sb.doubleSpinBox_regions_opacity,
                            sb.doubleSpinBox_boundary_opacity,
                            sb.doubleSpinBox_mesh_opacity,
                            ], geo_list):
            sp.valueChanged.connect(lambda _, o=sp, g=geo: self.change_opacity(o, g))
            vbtns[geo]['opacity'] = sp

        # Normal widgets
        sb.lineEdit_normal_scale.editingFinished.connect(self.scale_normals)
        vbtns['normals']['scale'] = sb.lineEdit_normal_scale

        ntb = sb.toolButton_normal_color
        ntb.clicked.connect(lambda ignore: self.change_color('normals', ntb))
        vbtns['normals']['color'] = ntb

        nc = sb.lineEdit_normal_count
        nc.editingFinished.connect(self.change_normal_count)
        vbtns['normals']['count'] = nc

        # Boundary color
        sb.comboBox_boundary_colorby.activated.connect(self.change_boundary_color)
        vbtns['boundary']['color_by'] = sb.comboBox_boundary_colorby

        self.hide_show_sidebar(False)
        self.set_visual_btn_values()

        self.button_bar_layout.addStretch()

        # Mesh button
        sb.pushButton_reload_mesh.pressed.connect(self.read_mesh_file)
        sb.comboBox_mesh_colorby.activated.connect(self.change_mesh_color)
        sb.comboBox_mesh_color_map.activated.connect(self.change_mesh_colormap)
        vbtns['mesh']['color_by'] = sb.comboBox_mesh_colorby

        cb = sb.comboBox_mesh_color_map
        for name, icons in build_qicons().items():
            if not name.endswith('_reversed'):
                cb.addItem(icons.get('bar', QtGui.QIcon()), name)
        cb.setCurrentText('pinks')

        # reset mesh vis widgets
        self.reset_boundary_mesh_widgets()
        self.reset_mesh_widgets()


    def reset_boundary_mesh_widgets(self, enable=False):
        sb = self.sidebar
        # disable boundary widgets
        sb.widget_boundary_opts.setEnabled(enable)
        sb.toolButton_boundary_vis.setChecked(enable)
        sb.toolButton_boundary_vis.setEnabled(enable)


    def reset_mesh_widgets(self, enable=False):
        sb = self.sidebar
        # disable boundary widgets
        sb.comboBox_mesh_colorby.setEnabled(enable)
        sb.comboBox_mesh_color_map.setEnabled(enable)
        sb.comboBox_mesh_style.setEnabled(enable)
        sb.doubleSpinBox_mesh_opacity.setEnabled(enable)
        sb.toolButton_mesh_vis.setChecked(enable)
        sb.toolButton_mesh_vis.setEnabled(enable)

    def __setup_wizards(self):

        self.cyclone_popup = None
        self.distribution_popup = None
        self.reactor_popup = None
        self.hopper_popup = None

        # --- wizards ---
        wizard_menu = QMenu('wizards', self)

        action = QAction('distributed', wizard_menu)
        action.triggered.connect(self.handle_distributed_wizard)
        wizard_menu.addAction(action)

        action = QAction('cyclone', wizard_menu)
        action.triggered.connect(self.handle_cyclone_wizard)
        wizard_menu.addAction(action)

        action = QAction('reactor', wizard_menu)
        action.triggered.connect(self.handle_reactor_wizard)
        wizard_menu.addAction(action)

        action = QAction('hopper', wizard_menu)
        action.triggered.connect(self.handle_hopper_wizard)
        wizard_menu.addAction(action)

        self.ui.geometry.toolbutton_wizard.setMenu(wizard_menu)

    def set_visual_btn_values(self):
        """change the visual btns to be in sync with self.visual_props"""
        for geo, info in self.visual_props.items():
            for key, value in info.items():
                if geo in self.visual_btns and key in self.visual_btns[geo]:
                    wid = self.visual_btns[geo][key]
                    if key == 'rep':
                        wid.setCurrentIndex(wid.findText(value))
                    elif key == 'visible':
                        wid.setChecked(value)
                        name = 'boundary mesh' if geo == 'boundary' else geo.replace('_', ' ')
                        wid.setToolTip(('Hide ' if value else 'Show ') + name)
                    elif key == 'color':
                        wid.setStyleSheet("QToolButton{{ background: {};}}".format(value.name()))
                    elif key == 'opacity':
                        wid.setValue(value)
                    elif key in ['scale', 'count']:
                        wid.setText(str(value))
                    elif key == 'color_by':
                        wid.setCurrentIndex(wid.findText(value))

    def handle_stl_units(self):
        unit = str(self.ui.geometry.combobox_stl_units.currentText())
        enable = unit == 'custom'
        if not enable:
            self.ui.geometry.lineedit_stl_scale.setText(str(CONVERSION[unit]))
            self.handle_widget_value_changed(self.ui.geometry.lineedit_stl_scale)
        self.handle_widget_value_changed(self.ui.geometry.combobox_stl_units)
        self.ui.geometry.lineedit_stl_scale.setEnabled(enable)

    def emitUpdatedValue(self, key, value, args=None):
        """emit an updates value"""
        self.value_updated.emit(self, {key: value}, args)

    def updateValue(self, key, newValue, args=None):
        """receive keyword changed from project manager"""
        if key == 'no_k':
            self.change_interaction(newValue)

    def objectName(self):
        """return the name of this object"""
        return 'VTK Widget'

    def set_unsaved_flag(self):
        """Tell the GUI that something changed that is worth saving"""
        GUI.set_unsaved_flag()
        GUI.clear_mesh_accepted()
        self.needs_saved = True

    def enable(self, enabled=True, partial=False):
        """enable/disable widgets based on run state"""
        for w in self.toolbat_geo_add_btns:
            w.setEnabled(enabled)

    def default(self):
        """reset to defaults"""
        self.ui.geometry.lineedit_keyword_z_max.setEnabled(True)
        self.ui.mesh.lineedit_keyword_kmax.setEnabled(True)
        self.vtkrenderer.RemoveAllViewProps()
        self.clear_all_geometry()
        self.change_interaction()
        self.reset_boundary_mesh_widgets()
        self.reset_mesh_widgets()
        self.glyph = self.glyph_actor = None
        self.render()

    def geometry_from_str(self, string):
        GeometryEngine.geometry_from_str(self, string)
        self.update_normals()

        # setup file watcher to read *_boundary.vtk
        fs = self.file_watcher.files()
        if fs:
            self.file_watcher.removePaths(fs)
        ds = self.file_watcher.directories()
        if ds:
            self.dir_watcher.removePaths(ds)

        b_name = self.get_boundary_name()
        if b_name is not None and os.path.exists(b_name):
            self.file_watcher.addPath(b_name)
            self.read_boundary_file(b_name, loading=True)
        elif b_name is not None:
            self.dir_watcher.addPath(os.path.dirname(b_name))
        else:
            prj_dir = GUI.get_project_dir()
            if prj_dir is not None:
                self.dir_watcher.addPath(prj_dir)

    def file_watcher_dir_changed(self, path):
        b_name = self.get_boundary_name()
        if b_name is None:
            return
        if os.path.exists(b_name):
            self.file_watcher.addPath(b_name)
            self.dir_watcher.removePath(path)
            self.file_watcher_file_changed(b_name)

    def file_watcher_file_changed(self, path):
        if not self.watching_file:
            self.watching_file = True
            self.timer.singleShot(1000, lambda:self.read_boundary_file(path))

    def read_boundary_file(self, path, loading=False):
        self.watching_file = False

        # remove existing actor
        actor = self.boundary_dict.get('actor', None)
        if actor is not None:
            self.vtkrenderer.RemoveActor(actor)

        if not os.path.exists(path):
            # boundary file deleted, probably from project reset
            self.reset_boundary_mesh_widgets(False)
            # add the project dir back to the file watcher
            self.dir_watcher.addPath(os.path.dirname(path))
            self.render()
            return

        GUI.print_internal(f"Reading mesh boundary: {path}", color='blue')

        # read the file and figure out if it is polydata or unstructured
        boundary_file_reader = vtk.vtkDataReader()
        boundary_file_reader.SetFileName(path)
        poly = False
        if boundary_file_reader.IsFilePolyData():
            boundary_file_reader = vtk.vtkPolyDataReader()
            poly = True
        elif boundary_file_reader.IsFileUnstructuredGrid():
            boundary_file_reader = vtk.vtkUnstructuredGridReader()
        else:
            # something else:
            return

        boundary_file_reader.SetFileName(path)
        boundary_file_reader.ReadAllScalarsOn()
        boundary_file_reader.Update()

        # collect array info
        data = boundary_file_reader.GetOutput()
        cell_data = data.GetCellData()

        array_info = {}
        for i in range(cell_data.GetNumberOfArrays()):
            array = cell_data.GetArray(i)
            arange = array.GetRange(0)
            array_info[cell_data.GetArrayName(i)] = {
                'i': i,
                'range': arange,
                'from': min(arange),
                'to': max(arange)}

        # set colorby
        sb = self.sidebar
        current = sb.comboBox_boundary_colorby.currentText()
        if current == '' or current not in array_info.keys():
            current = self.visual_props.get('boundary', {}).get('color_by', 'bc_id')

        sb.comboBox_boundary_colorby.clear()
        sb.comboBox_boundary_colorby.addItems(list(array_info.keys()))

        current_idx = sb.comboBox_boundary_colorby.findText(current)
        sb.comboBox_boundary_colorby.setCurrentIndex(current_idx)

        # Create a mapper
        if poly:
            mapper = vtk.vtkPolyDataMapper()
        else:
            mapper = vtk.vtkDataSetMapper()
        mapper.SetInputConnection(boundary_file_reader.GetOutputPort())
        mapper.ScalarVisibilityOn()
        mapper.SetScalarModeToUseCellFieldData()

        # Create an actor
        actor = vtk.vtkActor()
        actor.SetMapper(mapper)
        self.set_representation(actor, self.visual_props.get('boundary', {}).get('rep', 'edges'))
        actor.GetProperty().SetOpacity(self.visual_props.get('boundary', {}).get('opacity', 1.0))
        visible = self.visual_props.get('boundary', {}).get('visible', True)
        actor.SetVisibility(int(visible))

        self.vtkrenderer.AddActor(actor)

        self.boundary_dict.update({'filename': path,
                                   'reader': boundary_file_reader,
                                   'mapper': mapper,
                                   'actor': actor,
                                   'array_info': array_info})

        self.update_region_color_map()
        self.set_boundary_color(current)

        if not loading:
            self.render()

        self.reset_boundary_mesh_widgets(True)
        sb.toolButton_boundary_vis.setChecked(visible)

    def get_boundary_name(self):
        """Return the current project <run_name>_boundary.vtk path"""
        prj_dir = GUI.get_project_dir()
        run_name = GUI.project.get_value('run_name', None)
        if run_name is None:
            return None
        fname = os.path.join(prj_dir, run_name.upper()+"_boundary.vtk")
        return fname

    def read_mesh_file(self, loading=False):

        # remove existing actor
        actor = self.mesh_dict.get('actor', None)
        if actor is not None:
            self.vtkrenderer.RemoveActor(actor)

        path = self.get_mesh_name()

        if not os.path.exists(path):
            self.reset_mesh_widgets()
            GUI.print_internal(f"Could not find mesh file: {path}", color='red')
            return
        GUI.print_internal(f"Reading mesh: {path}", color='blue')

        reader = vtk.vtkXMLUnstructuredGridReader()
        reader.SetFileName(path)
        reader.Update()

        # collect array info
        data = reader.GetOutput()
        cell_data = data.GetCellData()

        array_info = {}
        for i in range(cell_data.GetNumberOfArrays()):
            array = cell_data.GetArray(i)
            arange = array.GetRange(0)
            array_info[cell_data.GetArrayName(i)] = {
                'i': i,
                'range': arange,
                'from': min(arange),
                'to': max(arange)}

        # set colorby
        sb = self.sidebar
        current = sb.comboBox_mesh_colorby.currentText()
        if current == '' or current not in array_info.keys():
            current = self.visual_props.get('mesh', {}).get('color_by', 'Volume')

        sb.comboBox_mesh_colorby.clear()
        sb.comboBox_mesh_colorby.addItems(list(array_info.keys()))

        current_idx = sb.comboBox_mesh_colorby.findText(current)
        sb.comboBox_mesh_colorby.setCurrentIndex(current_idx)

        mapper = vtk.vtkDataSetMapper()
        mapper.SetInputConnection(reader.GetOutputPort())
        mapper.SetScalarVisibility(True)
        mapper.SetScalarModeToUseCellFieldData()

        actor = vtk.vtkActor()
        actor.SetMapper(mapper)
        self.set_representation(actor, self.visual_props.get('mesh', {}).get('rep', 'edges'))
        actor.GetProperty().SetOpacity(self.visual_props.get('mesh', {}).get('opacity', 1.0))
        visible = self.visual_props.get('mesh', {}).get('visible', True)
        actor.SetVisibility(int(visible))

        self.vtkrenderer.AddActor(actor)

        self.mesh_dict.update(
            {'filename': path,
             'reader': reader,
             'mapper': mapper,
             'actor': actor,
             'array_info': array_info})

        self.set_mesh_color(current)

        if not loading:
            self.render()

        self.reset_mesh_widgets(True)
        sb.toolButton_mesh_vis.setChecked(visible)

    def get_mesh_name(self):
        """Return the current project <run_name>_mesh.vtu path"""
        prj_dir = GUI.get_project_dir()
        run_name = GUI.project.get_value('run_name', None)
        vtu_dir = GUI.project.get_value('vtu_dir', None)
        mesh_name = run_name.upper()+"_MESH.vtu"

        if run_name is None:
            return None
        if vtu_dir is not None:
            fname = os.path.join(prj_dir, vtu_dir.upper(), mesh_name)
        else:
            fname = os.path.join(prj_dir, mesh_name)
        return fname

    def visual_props_to_str(self):
        """convert visual props to str"""
        return ExtendedJSON.dumps(clean_visual_dict(self.visual_props))

    def visual_props_from_str(self, string):
        """convert string to visual props"""
        data = ExtendedJSON.loads(string)

        for geo, geo_dict in data.items():
            for key, value in geo_dict.items():
                if key in ['color', 'edge']:
                    if isinstance(value, (list, tuple)):
                        data[geo][key] = QtGui.QColor(*value)
                    else:
                        data[geo][key] = QtGui.QColor(value)

        self.visual_props = deepcopy_dict(DEFAULT_VISUAL_PROPS, qobjects=True)
        self.visual_props.update(data)

        # axes visibility
        self.axes.SetVisibility(self.visual_props.get('axes', {}).get('visible'))

        self.set_visual_btn_values()
        for actor in self.grid_viewer_dict['actors']:
            self.set_background_mesh_actor_props(actor)

        self.scale_normals()
        self.set_normal_color()

    def process_quadrics(self, proj):
        # hack to look for a value in a list of values
        def get(keys, default, args):
            for key in keys:
                v = proj.get_value(key, None, args)
                if v is not None:
                    break
            return default if v is None else v

        for qid in proj.get_key_indices('quadric_form'):
            qtype = proj.get_value('quadric_form', '', qid).lower()

            data = copy.deepcopy(DEFAULT_IMPLICIT_PARAMS)
            data['radius'] = proj.get_value('radius', 0, qid)
            data['centerx'] = proj.get_value('t_x', 0, qid)
            data['centery'] = proj.get_value('t_y', 0, qid)
            data['centerz'] = proj.get_value('t_z', 0, qid)
            xe = [get(['piece_xmax', 'clip_xmax'], float(Equation('xmax')), qid),
                  get(['piece_xmin', 'clip_xmin'], float(Equation('xmin')), qid)]
            ye = [get(['piece_ymax', 'clip_ymax'], float(Equation('ymax')), qid),
                  get(['piece_ymin', 'clip_ymin'], float(Equation('ymin')), qid)]
            ze = [get(['piece_zmax', 'clip_zmax'], float(Equation('zmax')), qid),
                  get(['piece_zmin', 'clip_zmin'], float(Equation('zmin')), qid)]

            dx = xe[0] - xe[1]
            dy = ye[0] - ye[1]
            dz = ze[0] - ze[1]

            mx = (xe[0] + xe[1])/2
            my = (ye[0] + ye[1])/2
            mz = (ze[0] + ze[1])/2

            itype = None
            if 'sphere' in qtype:
                itype = 'sphere'
            elif 'cyl' in qtype:
                itype = 'cylinder'
                if qtype.startswith('x'):
                    data['rotationz'] = 90
                    data['height'] = dx
                    data['centerx'] += mx
                elif qtype.startswith('y'):
                    data['height'] = dy
                    data['centery'] += my
                elif qtype.startswith('z'):
                    data['rotationx'] = 90
                    data['height'] = dz
                    data['centerz'] += mz

                # if the height is zero, most likely a 2D simulation, make the
                # height 1 so we can see it.
                if data['height'] == 0:
                    data['height'] = 1
            elif 'cone' in qtype:
                itype = 'cone'

            if itype is not None:
                data['type'] = itype
                self.add_implicit(data=data, name=qtype+'_'+str(qid[0]))

    # --- geometry ---
    def selected_geometry_changed(self):
        """The selected geometry changed, update UI"""
        current_selection = self.geometrytree_widget.selectedItems()
        top_level_items = [self.geometrytree_widget.indexOfTopLevelItem(select) > -1
                           for select in current_selection]

        implicits = ['implicit' in self.geometrydict.get(select.text(0), {}).get('geo_type', '')
                     for select in current_selection]

        # boolean btns
        enableboolbtn = False
        if len(current_selection) == 2 and all(top_level_items) and not any(implicits):
            enableboolbtn = True
        elif len(current_selection) >= 2 and all(top_level_items) and all(implicits):
            enableboolbtn = True
        for btn in self.booleanbtndict.values():
            btn.setEnabled(enableboolbtn)

        # enable/disable delete/copy/filter button
        enables = [False]*3
        if len(current_selection) == 1:
            if all(top_level_items):
                enables = [True]*3
            else:
                enables[2] = True
        elif all(top_level_items):
            enables[0] = True

        for enable, widget in zip(enables, [self.ui.geometry.toolbutton_remove_geometry,
                                            self.ui.geometry.toolbutton_add_filter,
                                            self.ui.geometry.toolbutton_copy_geometry,]):
            widget.setEnabled(enable)

        text = None
        data = None
        if current_selection:
            text = str(current_selection[-1].text(0)).lower()
            data = self.geometrydict.get(text)

        if text is not None and data is not None:
            type_ = data['type']
            geo_type = data.get('geo_type', None)

            # check for boolean types
            if type_ in ['union', 'intersect', 'difference']:
                type_ = 'boolean'

            # add implicit if object is an implicit
            elif geo_type == 'implicit':
                type_ += '_implicit'
            elif geo_type == 'procedural':
                type_ += '_procedural'

            new_index = 0
            for i in range(
                    self.ui.geometry.stackedWidgetGeometryDetails.count()):
                widget = self.ui.geometry.stackedWidgetGeometryDetails.widget(i)
                if str(widget.objectName()) == type_:
                    new_index = i
                    break

            # set the widget values
            for child in widget_iter(widget):
                name = str(child.objectName()).lower().split('_')[-1]

                value = data.get(name, None)
                if value is None:
                    continue

                if isinstance(child, LineEdit):
                    child.updateValue(None, value)
                elif isinstance(child, QCheckBox):
                    child.setChecked(value)
                elif isinstance(child, ComboBox):
                    child.updateValue(None, value)

            self.ui.geometry.lineedit_object_name.setText(text)

        else:
            new_index = 0
            self.ui.geometry.lineedit_object_name.setText('')
            self.ui.geometry.toolbutton_remove_geometry.setEnabled(False)

        current_index = self.ui.geometry.stackedWidgetGeometryDetails.currentIndex()
        self.ui.geometry.lineedit_object_name.setEnabled(new_index!=0)

        if self.animate:
            animate_stacked_widget(
                GUI,
                self.ui.geometry.stackedWidgetGeometryDetails,
                (current_index, new_index))

    def change_object_name(self):
        # get the current name
        current_selection = self.geometrytree_widget.selectedItems()
        tree_item = None
        if current_selection:
            tree_item = current_selection[-1]
            name = str(tree_item.text(0)).lower()
        else:
            return

        # get the new name
        new_name = self.ui.geometry.lineedit_object_name.text()

        # make sure the name actually changed, an item is selected
        if name == new_name or tree_item is None:
            return

        # make sure the name is unique
        new_name_unq = get_unique_string(new_name, self.geometrydict.keys())
        # if the name changes, update the lineedit
        if new_name != new_name_unq:
            new_name = new_name_unq
            self.ui.geometry.lineedit_object_name.setText(new_name)

        # change the item name
        tree_item.setText(0, new_name)

        # change the goemetry dict name
        self.geometrydict[new_name] = self.geometrydict.pop(name)

        # change the name in the geometry tree
        self.geometrytree = rec_key_replace(new_name, name, self.geometrytree)

        # Change parameter map
        self.change_parameter_geo_name(new_name, name)

        self.set_unsaved_flag()

    def get_tree_item(self, name):
        """return the tree item with name"""
        items = self.geometrytree_widget.findItems(name, QtCore.Qt.MatchRecursive, 0)
        if items:
            return items[0]

        return None

    def geometry_clicked(self, item):
        """Hide/Show the clicked geometry"""

        self.change_item_visibility(None, item, item.checkState(0) == QtCore.Qt.Checked)
        # update visibility of visual tabs
        GUI.update_geometry_visibility()

    def change_item_visibility(self, name=None, item=None, visible=True):
        if name is None and item is not None:
            name = str(item.text(0)).lower()
        else:
            return

        geo = self.geometrydict.get(name)
        if geo is None:
            return

        actor = geo.get('actor')
        if not actor:
            return

        if visible:
            if self.visual_props['geometry']['visible']:
                actor.VisibilityOn()
            geo['visible'] = True
        else:
            actor.VisibilityOff()
            geo['visible'] = False

        self.update_normals()

        # send toplevel geometry to regions
        self.ui.regions.update_toplevelgeo(self.get_toplevel_geomtry_names(visible_only=False))

        self.render()

    def get_stl_extents(self, filename):
        '''given an stl file, return the extents of that file'''

        # purge solids
        if is_stl_ascii(filename):
            filename = purge_multi_solids(filename)

        # reader
        reader = vtk.vtkSTLReader()
        reader.SetFileName(filename)
        reader.MergingOn()
        reader.Update()

        ret = [round(x,6) for x in reader.GetOutput().GetBounds()]
        return ret

    def add_to_tree(self, name, icon='geometry.svg', children=()):
        '''
        Add a geometry object to the tree. Over-rides GeometryEngine def to add
        entries to the tree widget.

        name (str): name to show in the tree
        icon (str): name of icon to show in tree
        '''

        GeometryEngine.add_to_tree(self, name=name, children=children)

        toplevel = QTreeWidgetItem([name])
        toplevel.setIcon(0, get_icon(icon))
        toplevel.setFlags(toplevel.flags() | QtCore.Qt.ItemIsUserCheckable)

        # collect and move any children
        self.animate = False
        for child in children:
            child_item = self.get_tree_item(child)
            toplevelindex = self.geometrytree_widget.indexOfTopLevelItem(child_item)
            item = self.geometrytree_widget.takeTopLevelItem(toplevelindex)
            if item:
                self.change_item_visibility(None, item, False)
                toplevel.addChild(item)
                item.setCheckState(0, QtCore.Qt.Unchecked)
        self.animate = True

        self.geometrytree_widget.addTopLevelItem(toplevel)
        self.gui.change_pane('geometry')
        self.geometrytree_widget.setCurrentItem(toplevel)

        # check and set visibility
        geo = self.geometrydict.get(name)
        visible = geo['visible']
        self.change_item_visibility(None, toplevel, visible)
        if visible:
            toplevel.setCheckState(0, QtCore.Qt.Checked)
        else:
            toplevel.setCheckState(0, QtCore.Qt.Unchecked)

        # send toplevel geometry to regions
        self.ui.regions.update_toplevelgeo(self.get_toplevel_geomtry_names(visible_only=False))

    def add_to_render(self, name, obj='transformfilter', scalars=False):
        '''
        Add a geometry object to the render.

        name (str): name of the geometry object
        obj (str): name of vtk.vtkPolyData object to build mapper for
        '''

        # add to render
        geo_data = self.geometrydict.get(name)
        transform_filter = geo_data[obj]

        # mapper
        mapper = vtk.vtkPolyDataMapper()
        mapper.SetInputConnection(transform_filter.GetOutputPort())
        if not scalars:
            mapper.ScalarVisibilityOff()

        # actor
        actor = vtk.vtkActor()
        actor.SetMapper(mapper)

        geo_data['mapper'] = mapper
        geo_data['actor'] = actor

        self.set_geometry_actor_props(actor, name)
        self.vtkrenderer.AddActor(actor)

    def handle_widget_value_changed(self, widget, name=None, value=None, key=None):
        """Update the value of edited parameter in the geometrydict"""

        if name is None:
            current_selection = self.geometrytree_widget.selectedItems()

            if current_selection:
                name = str(current_selection[-1].text(0)).lower()
            value = None

            geo_data = self.geometrydict.get(name)
            if geo_data is None:
                return

            widget_name = str(widget.objectName()).lower().split('_')[-1]

            if widget_name in geo_data:
                key = widget_name
            else:
                return

            # if widget is a lineedit
            if isinstance(widget, LineEdit):
                value = widget.value
            elif isinstance(widget, QCheckBox):
                value = widget.isChecked()
            elif isinstance(widget, ComboBox):
                value = widget.value
        else:
            geo_data = self.geometrydict.get(name)

        prev_value = geo_data.get(key, None)
        if value is not None and (prev_value != value or str(prev_value) != str(value)):
            self.update_geometry_value(name, value, key)

    def update_geometry_value(self, name, value, key):
        geo_data = self.geometrydict.get(name)
        self.update_parameter_map(value, name, key)
        geo_data[key] = value
        type_ = geo_data['type']
        geo_type = geo_data.get('geo_type')

        if type_ in PRIMITIVE_DICT and geo_type == 'primitive':
            self.update_primitive(name)
        elif type_ in FILTER_DICT:
            self.update_filter(name)
        elif type_ in PROCEDURAL_DICT and geo_type == 'procedural':
            self.update_procedural(name)
        elif type_ in IMPLICIT_DICT and geo_type == 'implicit':
            self.update_implicit(name)
        elif type_ == 'stl':
            self.update_stl(name)

        if 'transform' in geo_data and not geo_type in ['filter', 'implicit']:
            self.update_transform(name)

        self.set_unsaved_flag()
        self.update_region_facet_selection()
        self.render()

    def update_transform(self, name):

        transform_filter = GeometryEngine.update_transform(self, name)

        geo = self.geometrydict.get(name)
        geo_type = geo['type']

        # update stl extents widgets
        if geo_type == 'stl':
            bounds = [round(x,6) for x in transform_filter.GetOutput().GetBounds()]

            ui = self.ui.geometry
            for key, bound, widget in zip(
                    ['extentxmin', 'extentxmax', 'extentymin', 'extentymax',
                     'extentzmin', 'extentzmax'],
                    bounds,
                    [ui.lineedit_stl_extentxmin, ui.lineedit_stl_extentxmax,
                     ui.lineedit_stl_extentymin, ui.lineedit_stl_extentymax,
                     ui.lineedit_stl_extentzmin, ui.lineedit_stl_extentzmax]):
                geo[key] = bound
                # manually update stl bounds widgets
                widget.updateValue(None, bound)

        return transform_filter

    def add_stl(self, filename=None, name=None, data=None, loading=False,
                proj_dir=None):
        """Open browse dialog and load selected stl file"""

        proj_dir = os.path.realpath(GUI.get_project_dir())
        if filename is None:
            filename, _ = QFileDialog.getOpenFileName(
                self, 'Select an STL File',
                proj_dir,
                'STL File (*.stl)',)

            filename = str(filename)

            if not filename:
                return None

            # ask to copy stl to current project
            if proj_dir not in os.path.realpath(filename):
                btn = GUI.message(text='The STL file is not in the current project\n'
                    'directory. Copy file to project directory?', buttons=['yes', 'no'],
                    default='no')
                if btn == 'yes':
                    basename = os.path.basename(filename)
                    copied_filename = os.path.join(proj_dir, basename)
                    shutil.copy(filename, copied_filename)
                    filename = copied_filename

        # look for '${proj_dir}' and replace with the project dir
        filename = filename.replace('${proj_dir}', proj_dir)

        # normalize the path issues/504
        filename = os.path.realpath(filename.replace('\\', '/'))

        # check to make sure the file exists
        if not os.path.exists(filename):
            msg = 'Could not find {}.\nBrowse to new location?'.format(filename)
            btn = GUI.message(text=msg, buttons=['yes', 'no'], default='no')
            new_name = None
            if btn == 'yes':
                new_name = self.add_stl(filename=None, name=name, data=data, loading=loading)
            else:
                GUI.warn('Could not find {}, please check geometry'.format(filename))
            return new_name

        # create the geometry object
        name = GeometryEngine.add_stl(self, filename, name, data, loading, proj_dir)

        # add to render
        self.add_to_render(name)

        if not loading:
            self.set_unsaved_flag()
            self.update_normals()
            self.update_region_facet_selection()
            self.render()
        return name

    def add_primitive(self, primtype='sphere', name=None, data=None,
                      loading=False):
        """Add the specified primitive"""

        # create primitive
        name = GeometryEngine.add_primitive(
            self, primtype=primtype, name=name, data=data, loading=loading)
        # add to render
        self.add_to_render(name)

        if not loading:
            self.set_unsaved_flag()
            self.update_normals()
            self.update_region_facet_selection()
            self.render()
        return name

    def add_procedural(self, proceduraltype=None, name=None, data=None,
                       loading=False):
        """Add a procedural object"""

        # create implicit
        name = GeometryEngine.add_procedural(self, proceduraltype, name, data,
                                             loading)
        # add to render
        self.add_to_render(name)

        if not loading:
            self.set_unsaved_flag()
            self.update_normals()
            self.render()
        return name

    def add_implicit(self, implicittype=None, name=None, data=None,
                     loading=False):
        """Add an implicit function"""

        # create implicit
        name = GeometryEngine.add_implicit(self, implicittype, name, data,
                                           loading)
        # add to render
        self.add_to_render(name, obj='surface')

        if not loading:
            self.set_unsaved_flag()
            self.update_normals()
            self.render()
        return name

    def add_parametric(self, paramtype=None, name=None, data=None,
                       loading=False):
        """Add the specified parametric object"""

        # create parametric
        name = GeometryEngine.add_parametric(self, paramtype, name, data,
                                             loading)
        # add to render
        self.add_to_render(name)

        if not loading:
            self.set_unsaved_flag()
            self.update_normals()
            self.update_region_facet_selection()
            self.render()
        return name

    def add_filter(self, filtertype=None, name=None, data=None, child=None,
                   loading=False):
        """add the selected filter with the input being the currently selected
        toplevel item"""

        if child is None:
            current_selection = self.geometrytree_widget.selectedItems()
            if current_selection:
                child = str(current_selection[-1].text(0)).lower()

        # create filter
        name = GeometryEngine.add_filter(self, filtertype, name, data, child,
                                         loading)
        # add to render
        self.add_to_render(name, 'filter')

        if not loading:
            self.set_unsaved_flag()
            self.update_normals()
            self.update_region_facet_selection()
            self.render()

        # hide the source
        self.geometrydict[child]['actor'].VisibilityOff()
        self.geometrydict[child]['visible'] = False

        return name

    def boolean_operation(self, booltype=None, boolname=None, data=None,
                          children=None, loading=False):
        """Apply a boolean operation with the currently selected toplevel
        items."""

        if children is None:
            children = []
            for selection in self.geometrytree_widget.selectedItems():
                name = str(selection.text(0)).lower()
                children.append(name)

        boolname = GeometryEngine.boolean_operation(
            self, booltype, boolname, data, children, loading)

        bool_data = self.geometrydict.get(boolname)
        implicit = 'boolean_implicit' == bool_data.get('geo_type', None)

        mapper = vtk.vtkPolyDataMapper()

        if implicit:
            surface = bool_data.get('surface', None)
            mapper.SetInputConnection(surface.GetOutputPort())
        else:
            boolean_operation = bool_data.get('booleanoperation', None)
            mapper.SetInputConnection(boolean_operation.GetOutputPort())
        mapper.ScalarVisibilityOff()

        actor = vtk.vtkActor()
        actor.SetMapper(mapper)

        self.set_geometry_actor_props(actor, boolname)
        self.vtkrenderer.AddActor(actor)

        # save references
        bool_data['mapper'] = mapper
        bool_data['actor'] = actor

        if not loading:
            self.set_unsaved_flag()
            self.update_normals()
            self.update_region_facet_selection()

        self.render()

        return boolname

    def boolean_flip_order(self):
        """Flip the order of the geometry objects in the currently selected
        boolean object"""

        # get the current name
        current_selection = self.geometrytree_widget.selectedItems()
        tree_item = None
        if current_selection:
            tree_item = current_selection[-1]
            name = str(tree_item.text(0)).lower()
        else:
            return

        data = self.geometrydict.get(name)
        bool_type = data.get('type', None)
        if bool_type is None or bool_type in ['union', 'intersection']:
            return

        # flip the order in the geometrydict and the geometrytree
        data['children'] = data.get('children', [])[::-1]
        for key, dep in dict_iter(self.geometrytree):
            if key == name:
                break
        self.geometrytree = rec_val_replace(self.geometrytree, name, dict((k, dep[k]) for k in list(dep.keys())[::-1]))


        bool_obj = data.get('booleanoperation')
        if data.get('geo_type') == 'boolean_implicit':
            functions = bool_obj.GetFunction()
            functions = [functions.GetItemAsObject(i) for i in range(functions.GetNumberOfItems())]
            [bool_obj.RemoveFunction(f) for f in functions]
            [bool_obj.AddFunction(f) for f in functions[::-1]]
        else:
            connections = [bool_obj.GetInputConnection(i, 0) for i in range(bool_obj.GetTotalNumberOfInputConnections())]
            [bool_obj.RemoveAllInputConnections(i) for i in range(bool_obj.GetTotalNumberOfInputConnections())]
            [bool_obj.SetInputConnection(i, conn) for i, conn in enumerate(connections[::-1])]

        self.set_unsaved_flag()
        self.update_region_facet_selection()
        self.render()

    def clear_all_geometry(self):
        """remove all geometry"""
        self.geometrytree_widget.clear()
        for name, geo in self.geometrydict.items():
            self.remove_from_parameter_map(name, geo)
        self.geometrydict = {}
        self.geometrytree = {}

    def remove_geometry(self):
        """Remove the currently selected geometry, filter, or boolean operation
        """
        currentSelection = self.geometrytree_widget.selectedItems()
        self.animate = False
        for selection in currentSelection:
            text = str(selection.text(0)).lower()

            GeometryEngine.remove_geometry(self, text)

            # remove tree item
            toplevelindex = self.geometrytree_widget.indexOfTopLevelItem(
                selection)
            item = self.geometrytree_widget.takeTopLevelItem(toplevelindex)

            # move children to toplevel, make visible
            children = item.takeChildren()
            for child in children:
                self.geometrytree_widget.addTopLevelItem(child)
                if self.visual_props['geometry']['visible']:
                    self.change_item_visibility(None, child, True)
                child.setCheckState(0, QtCore.Qt.Checked)

            # remove graphics
            geo = self.geometrydict.pop(text)
            self.remove_from_parameter_map(text, geo)
            self.vtkrenderer.RemoveActor(geo['actor'])

        self.animate = True
        if children:
            self.geometrytree_widget.setCurrentItem(children[0])
        else:
            i = self.geometrytree_widget.topLevelItemCount() - 1
            if i >= 0:
                item = self.geometrytree_widget.topLevelItem(i)
                self.geometrytree_widget.setCurrentItem(item)
            else:
                self.selected_geometry_changed()

        self.set_unsaved_flag()
        self.update_normals()
        self.update_region_facet_selection()
        self.render()

        # update region list
        self.ui.regions.update_toplevelgeo(self.get_toplevel_geomtry_names(visible_only=False))

    def handle_copy_geometry(self):
        """duplicate the selected geometry"""
        currentSelection = self.geometrytree_widget.selectedItems()
        if currentSelection:
            text = str(currentSelection[-1].text(0)).lower()
            self.copy_geometry(text)

    def copy_geometry(self, name, center=None, rotation=None):
        """given a geometry name, copy it and optionally change the center or
        rotation"""

        # clear selection to make sure values are not changed
        self.geometrytree_widget.selectionModel().clearSelection()

        data = copy.deepcopy(remove_vtk_objects(self.geometrydict[name]))
        parent = self.get_tree_item(name)

        if center is not None:
            for key, val in zip(['centerx', 'centery', 'centerz'], center):
                data[key] = val

        if rotation is not None:
            for key, val in zip(['rotationx', 'rotationy', 'rotationz'], rotation):
                data[key] = val

        name = None
        geo_type = data['geo_type']
        if geo_type == 'primitive':
            name = self.add_primitive(data=data)
        elif geo_type == 'procedural':
            name = self.add_procedural(data=data)
        elif geo_type == 'parametric':
            name = self.add_parametric(data=data)
        elif geo_type == 'filter':
            # copy the child first
            copy_child = self.copy_geometry(parent.child(0).text(0))
            name = self.add_filter(data=data, child=copy_child)
        elif geo_type == 'boolean' or geo_type == 'boolean_implicit':
            # copy children first
            copy_children = []
            children = data.get('children', [])
            for child in children:
                copy_children.append(self.copy_geometry(child))
            name = self.boolean_operation(booltype=data['type'], data=data,
                                          children=copy_children)
        elif geo_type == 'stl':
            name = self.add_stl(filename=data['filename'], data=data)
        elif geo_type == 'implicit':
            name = self.add_implicit(data=data)

        # update parameter mapping
        for key, value in data.items():
            self.update_parameter_map(value, name, key, check_old=False)

        self.geometrytree_widget.setCurrentItem(self.get_tree_item(name))

        self.set_unsaved_flag()
        return name

    def enable_filters(self):
        """filter menu about to show, enable/disable based on geo selected"""

        current_selection = self.geometrytree_widget.selectedItems()
        if current_selection:
            name = str(current_selection[-1].text(0)).lower()
        else:
            return

        geo_data = self.geometrydict.get(name)
        geo_type = geo_data.get('geo_type')

        enable = [False] + [True]*(len(self.filter_actions)-1)
        if 'implicit' in geo_type:
            enable = [False]*len(self.filter_actions)
            enable[0] = True

        for en, act in zip(enable, self.filter_actions):
            act.setEnabled(en)

    def set_geometry_actor_props(self, actor, name):
        """set the geometry proprerties to the others in the scene"""

        props = self.visual_props['geometry']

        self.set_representation(actor, props['rep'])
        actor.GetProperty().SetColor(props['color'].getRgbF()[:3])
        actor.GetProperty().SetEdgeColor(props['edge'].getRgbF()[:3])
        actor.GetProperty().SetOpacity(props['opacity'])

        # check visibility
        geo = self.geometrydict.get(name)
        if not props['visible'] or not geo['visible']:
            actor.VisibilityOff()

    # --- regions ---
    def update_region_source(self, name):
        """Update the specified primitive"""
        primtype = self.region_dict[name]['type']
        props = self.region_dict[name]

        if 'source' in self.region_dict[name]:
            source = self.region_dict[name]['source']
        else:
            source = None

        lengths = [abs(safe_float(to) - safe_float(from_)) for
                   from_, to in zip(props['from'], props['to'])]
        center = [min(map(safe_float, ft)) + l / 2.0 for ft, l in
                  zip(zip(props['from'], props['to']),
                      lengths)]

        # update source
        if primtype == 'sphere':
            source.SetRadius(min(lengths)/2.0)
        elif primtype == 'point':
            source.SetRadius(.01)
            center = props['from']
        elif primtype == 'box':
            source.SetXLength(lengths[0])
            source.SetYLength(lengths[1])
            source.SetZLength(lengths[2])
        elif primtype == 'XY-plane':
            source.SetXLength(lengths[0])
            source.SetYLength(lengths[1])
            source.SetZLength(0)
            center[2] = props['from'][2]
        elif primtype == 'XZ-plane':
            source.SetXLength(lengths[0])
            source.SetYLength(0)
            source.SetZLength(lengths[2])
            center[1] = props['from'][1]
        elif primtype == 'YZ-plane':
            source.SetXLength(0)
            source.SetYLength(lengths[1])
            source.SetZLength(lengths[2])
            center[0] = props['from'][0]
        elif primtype == 'STL':
            source.SetXLength(lengths[0])
            source.SetYLength(lengths[1])
            source.SetZLength(lengths[2])
        else:
            return None

        # common props
        if source is not None:
            if all(isinstance(x, (float, int, Equation)) for x in center):
                source.SetCenter(*center)
                source.Update()

        return source

    def new_region(self, name, region):
        """create a new region"""
        self.region_dict[name] = deepcopy_dict(region)
        self.region_dict[name]['name'] = name

        if region['type'] == 'point':
            shape = 'sphere'
        else:
            shape = 'box'

        source = PRIMITIVE_DICT[shape]()
        self.region_dict[name]['source'] = source
        self.update_region_source(name)

        # Create a mapper
        mapper = vtk.vtkPolyDataMapper()
        mapper.SetInputConnection(source.GetOutputPort())

        # Create an actor
        actor = vtk.vtkActor()
        actor.SetMapper(mapper)
        self.set_region_actor_props(actor, name, region['color'].color_float)

        self.vtkrenderer.AddActor(actor)
#        self.balloon_widget.AddBalloon(actor, name, None)

        self.region_dict[name]['actor'] = actor
        self.region_dict[name]['mapper'] = mapper
        self.select_facets(name)

        self.change_region_visibility(
            name, self.region_dict[name]['visibility'],)

        self.update_region_color_map()

    def delete_region(self, name):
        """delete a region"""
        region = self.region_dict.pop(name)
        self.vtkrenderer.RemoveActor(region['actor'])
        if 'clip_actor' in region:
            self.vtkrenderer.RemoveActor(region['clip_actor'])

    def update_region(self, name, region):
        """update a region"""
        self.region_dict[name].update(deepcopy_dict(region))
        self.update_region_source(name)
        self.select_facets(name)
        self.render()

    def update_region_facet_selection(self):
        for name in self.region_dict.keys():
            self.select_facets(name)

    def change_region_color(self, name, color):
        """change the color of a region"""
        reg = self.region_dict.get(name)
        reg['color'] = copy.deepcopy(color)

        actor = reg['actor']
        actor.GetProperty().SetColor(*reg['color'].color_float)

        if 'clip_actor' in self.region_dict[name]:
            actor = reg['clip_actor']
            actor.GetProperty().SetColor(*reg['color'].color_float)

        self.render()
        self.update_region_color_map()

    def change_region_type(self, name, region):
        """change the type of a region"""

        self.region_dict[name].update(deepcopy_dict(region))

        if region['type'] == 'point':
            shape = 'sphere'
        else:
            shape = 'box'

        source = PRIMITIVE_DICT[shape]()
        region_data = self.region_dict.get(name)
        region_data['source'] = source
        self.update_region_source(name)
        self.select_facets(name)
        region_data['mapper'].SetInputConnection(source.GetOutputPort())

        self.render()

    def change_region_name(self, old_name, new_name):
        """change the name of a region"""
        region = self.region_dict.pop(old_name)
        self.region_dict[new_name] = region

    def change_region_visibility(self, name, visible):
        """change the visibility of a region"""
        reg = self.region_dict.get(name)
        if visible and self.visual_props['regions']['visible']:
            reg['actor'].VisibilityOn()
            if 'clip_actor' in reg:
                reg['clip_actor'].VisibilityOn()
        else:
            reg['actor'].VisibilityOff()
            if 'clip_actor' in reg:
                reg['clip_actor'].VisibilityOff()
        reg['visible'] = visible

        self.render()

    def set_region_actor_props(self, actor, name, color=None):
        """set the geometry properties to the others in the scene"""

        props = self.visual_props['regions']
        self.set_representation(actor, props['rep'])
        actor.GetProperty().SetOpacity(props['opacity'])

        if color:
            actor.GetProperty().SetColor(*color)
        else:
            actor.GetProperty().SetColor(props['color'].getRgbF()[:3])

        # check visibility
        if not props['visible']:
            actor.VisibilityOff()

    def update_region_color_map(self):

        # {1: {'region': 'top outlet'}, 2: {'region': 'Wall', 'fluid_wall_type': 1}}
        lookup_table = self.region_color_map = vtk.vtkLookupTable()

        idxs = list(GUI.bcs.keys())
        if not idxs:
            return

        min_idx = min(idxs)
        max_idx = max(idxs)
        lookup_table.SetNumberOfTableValues(max_idx-min_idx+1)
        lookup_table.SetTableRange(min_idx, max_idx)
        lookup_table.Build()
        for i, idx in enumerate(range(min_idx, max_idx+1)):
            if idx in GUI.bcs:
                reg = self.region_dict.get(GUI.bcs[idx].get('region'))
                if reg is not None:
                    color = reg.get('color', None)
                    if color is not None:
                        c = color.color_float
            else:
                c = [1.0, 1.0, 1.0] # default color

            lookup_table.SetTableValue(i, c[0], c[1], c[2], 1.0)

        self.bc_id_lookup_table = lookup_table
        if self.sidebar.comboBox_boundary_colorby.currentText() == 'bc_id':
            self.set_boundary_color('bc_id')

    def change_boundary_color(self, index):
        text = self.sidebar.comboBox_boundary_colorby.currentText()
        self.visual_props.get('boundary', {})['color_by'] = text
        self.set_boundary_color(text)
        self.render()

    def set_boundary_color(self, name):

        array_info = self.boundary_dict.get('array_info', {})
        array = array_info.get(name, {})

        # set the color map
        mapper = self.boundary_dict.get('mapper')
        if mapper is None:
            return

        if name == 'bc_id' and self.bc_id_lookup_table is not None:
            lut = self.bc_id_lookup_table
        else:
            lut = vtk.vtkLookupTable()
            lut.DeepCopy(LOOKUP_TABLES.get('pinks'))
            lut.Build()

        mapper.SelectColorArray(name)
        mapper.SetLookupTable(lut)
        mapper.SetScalarRange(safe_float(array.get('from', 0.0), 0.0),
                              safe_float(array.get('to', 1.0), 1.0))

    def change_mesh_color(self, index):
        text = self.sidebar.comboBox_mesh_colorby.currentText()
        self.visual_props.get('mesh', {})['color_by'] = text
        self.set_mesh_color(text)
        self.render()

    def change_mesh_colormap(self, index):
        text = self.sidebar.comboBox_mesh_color_map.currentText()
        self.visual_props.get('mesh', {})['color_map'] = text
        self.set_mesh_color()
        self.render()

    def set_mesh_color(self, name=None):

        if name is None:
            name = self.visual_props.get('mesh', {}).get('color_by')
        if name is None:
            return

        array_info = self.mesh_dict.get('array_info', {})
        array = array_info.get(name, {})

        # set the color map
        mapper = self.mesh_dict.get('mapper')
        if mapper is None:
            return
        lut = vtk.vtkLookupTable()
        cmap = self.visual_props.get('mesh', {}).get('color_map', 'pinks')
        lut.DeepCopy(LOOKUP_TABLES.get(cmap))
        lut.Build()

        mapper.SelectColorArray(name)
        mapper.SetLookupTable(lut)
        mapper.SetScalarRange(safe_float(array.get('from', 0.0), 0.0),
                              safe_float(array.get('to', 1.0), 1.0))

    def select_facets(self, name):
        """select facets with an implicit function"""
        region = self.region_dict[name]
        # remove old objects
        if 'clip_actor' in region:
            self.vtkrenderer.RemoveActor(region['clip_actor'])
            for key in ['clip_actor', 'clip_mapper', 'output']:
                region.pop(key)

        if region['type'].lower() != 'stl':
            self.ui.regions.set_facet_number(name, 0)
            self.set_representation(region['actor'], self.visual_props['regions']['rep'])
            return

        # need to save since facet selection could have changed
        self.needs_saved = True

        # check for geometry
        geo = self.collect_toplevel_geometry()
        if geo is not None:
            output, n_facets = GeometryEngine.select_facets(self, region)
            clip_mapper = vtk.vtkPolyDataMapper()
            clip_mapper.SetInputData(output)
            clip_mapper.ScalarVisibilityOff()
            clip_actor = vtk.vtkActor()
            clip_actor.SetMapper(clip_mapper)

            # save
            region['output'] = output
            region['clip_mapper'] = clip_mapper
            region['clip_actor'] = clip_actor
            region['actor'].GetProperty().SetRepresentationToWireframe()

            self.set_region_actor_props(clip_actor, name,
                                        color=region['color'].color_float)

            self.vtkrenderer.AddActor(clip_actor)
            self.ui.regions.set_facet_number(name, n_facets)

    # --- output files ---
    def export_stl(self, file_name):
        """export visible toplevel geometry"""
        GeometryEngine.export_stl(self, file_name, GUI.bcs, GUI.iss)
        self.needs_saved = False

    # --- mesh ---
    def auto_size_mesh_extents(self):
        """collect and set the extents of the visible geometry"""
        extents = self.get_geometry_extents()

        if extents:
            for key, extent, rounding in zip(['x_min', 'x_max', 'y_min', 'y_max',
                                             'z_min', 'z_max'],
                                             extents,
                                             ['floor', 'ceiling']*3):
                self.emitUpdatedValue(key, float(custom_round(extent, 3, rounding)))

    def update_background_mesh(self, spacing):
        """update the background mesh"""
        cells = [len(c) for c in spacing]
        self.rectilinear_grid.SetDimensions(*cells)

        x_coords = vtk.vtkFloatArray()
        for i in spacing[0]:
            x_coords.InsertNextValue(i)

        y_coords = vtk.vtkFloatArray()
        for i in spacing[1]:
            y_coords.InsertNextValue(i)

        z_coords = vtk.vtkFloatArray()
        for i in spacing[2]:
            z_coords.InsertNextValue(i)

        self.rectilinear_grid.SetXCoordinates(x_coords)
        self.rectilinear_grid.SetYCoordinates(y_coords)
        self.rectilinear_grid.SetZCoordinates(z_coords)

        # remove existing
        for actor in self.grid_viewer_dict['actors']:
            self.vtkrenderer.RemoveActor(actor)

        self.grid_viewer_dict['filters'] = []
        self.grid_viewer_dict['actors'] = []
        self.grid_viewer_dict['mappers'] = []

        # add new actors
        for i in range(3):
            filter_ = vtk.vtkRectilinearGridGeometryFilter()
            filter_.SetInputData(self.rectilinear_grid)

            if i == 0:
                filter_.SetExtent(0, 0, 0, cells[1], 0, cells[2])
            elif i == 1:
                filter_.SetExtent(0, cells[0], 0, 0, 0, cells[2])
            else:
                filter_.SetExtent(0, cells[0], 0, cells[1], 0, 0)
            filter_.Update()

            self.grid_viewer_dict['filters'].append(filter_)
            mapper = vtk.vtkPolyDataMapper()
            mapper.SetInputConnection(filter_.GetOutputPort())
            mapper.ScalarVisibilityOff()
            self.grid_viewer_dict['mappers'].append(mapper)

            actor = vtk.vtkActor()
            actor.SetMapper(mapper)
            self.set_background_mesh_actor_props(actor)
            self.grid_viewer_dict['actors'].append(actor)

            self.vtkrenderer.AddActor(actor)

        self.render()

    def set_background_mesh_actor_props(self, actor):
        """set the background mesh properties"""
        props = self.visual_props['background_mesh']
        self.set_representation(actor, props['rep'])
        actor.GetProperty().SetColor(props['color'].getRgbF()[:3])
        actor.GetProperty().SetEdgeColor(props['color'].getRgbF()[:3])
        actor.GetProperty().SetOpacity(props['opacity'])
        actor.SetVisibility(int(props['visible']))

    def init_normals(self):

        sphere = vtk.vtkSphereSource()

        # normal generator
        normal_generator = self.normal_generator = vtk.vtkPolyDataNormals()
        normal_generator.SetInputConnection(sphere.GetOutputPort())
        normal_generator.ComputePointNormalsOff()
        normal_generator.ComputeCellNormalsOn()
        normal_generator.AutoOrientNormalsOff()
        normal_generator.FlipNormalsOff()
        normal_generator.ConsistencyOff()
        normal_generator.SplittingOff()

        # cell centers
        cell_centers = self.cell_centers = vtk.vtkCellCenters()
        cell_centers.SetInputConnection(normal_generator.GetOutputPort())

        # limit glyph count
        glyph_mask = self.glyph_mask = vtk.vtkMaskPoints()
        glyph_mask.SetInputConnection(cell_centers.GetOutputPort())
        glyph_mask.RandomModeOn()
        glyph_mask.SetRandomModeType(2) # setting to type 1 crashes on windows
        n = self.visual_props.get('normals', {}).get('count', 1000)
        glyph_mask.SetMaximumNumberOfPoints(safe_int(n))

        arrow = self.normal_arrow = vtk.vtkArrowSource()
        arrow.SetTipResolution(8)
        arrow.SetTipLength(0.3)
        arrow.SetTipRadius(0.1)

        # Create transformer
        transform = self.glyph_transform = vtk.vtkTransform()
        transform.Scale(.1, .1, .1)
        scalled_arrow = self.normal_scaled_arrow = vtk.vtkTransformPolyDataFilter()
        scalled_arrow.SetTransform(transform)
        scalled_arrow.SetInputConnection(arrow.GetOutputPort())

        glyph = self.glyph = vtk.vtkGlyph3D()
        glyph.SetInputConnection(glyph_mask.GetOutputPort())
        glyph.SetSourceConnection(scalled_arrow.GetOutputPort())
        glyph.SetVectorModeToUseNormal()
        glyph.ScalingOff()
        glyph.OrientOn()

        self.glyph_mapper = vtk.vtkPolyDataMapper()
        self.glyph_mapper.SetInputConnection(glyph.GetOutputPort())
        self.glyph_mapper.ScalarVisibilityOff()
        self.glyph_actor = vtk.vtkActor()
        self.glyph_actor.SetMapper(self.glyph_mapper)
        if self.visual_props['normals'].get('visible', False) and self.visual_props['geometry'].get('visible', False):
            self.glyph_actor.VisibilityOn()
        else:
            self.glyph_actor.VisibilityOff()
        self.vtkrenderer.AddActor(self.glyph_actor)
        self.set_normal_color()

    def scale_normals(self):
        if self.glyph is None:
            self.init_normals()
        scale = safe_float(self.sidebar.lineEdit_normal_scale.text(), 0.1)
        self.visual_props['normals']['scale'] = scale

        self.glyph_transform.Identity()
        self.glyph_transform.Scale(scale, scale, scale)
        self.update_normals()
        self.render()

    def change_normal_count(self):

        n = self.sidebar.lineEdit_normal_count.text()
        self.visual_props.get('normals', {})['count'] = n
        self.glyph_mask.SetMaximumNumberOfPoints(safe_int(n))
        self.render()

    def update_normals(self):
        if self.glyph is None:
            self.init_normals()
        geo = self.collect_all_geometry()
        if geo is None:
            self.glyph_actor.VisibilityOff()
            return

        if self.visual_props['normals'].get('visible', False) and self.visual_props['geometry'].get('visible', False):
            self.glyph_actor.VisibilityOn()

        self.normal_generator.SetInputConnection(geo.GetOutputPort())

    def set_normal_color(self):
        if self.glyph_actor is None:
            return
        color = self.visual_props['normals']['color']
        self.glyph_actor.GetProperty().SetColor(color.getRgbF()[:3])

    # --- view ---
    def change_visibility(self, name, toolbutton):
        """change the visibility of one of the scene objects"""
        actors = None
        visible = self.visual_props[name]['visible'] = toolbutton.isChecked()
        sb = self.sidebar
        toolbutton.setToolTip(('Hide ' if visible else 'Show ') + name.replace('_', ' '))
        if name == 'background_mesh':
            actors = self.grid_viewer_dict['actors']
        elif name == 'geometry':
            actors = [geo['actor'] for geo in self.geometrydict.values()
                      if geo['visible']]
            if sb.checkBox_geometry_normals.isChecked():
                actors += [self.glyph_actor]
        elif name == 'regions':
            actors = [geo['actor'] for geo in self.region_dict.values()
                      if geo['visible']]

            actors += [geo['clip_actor'] for geo in self.region_dict.values()
                       if 'clip_actor' in geo and geo['visible']]
        elif name == 'normals' and sb.toolButton_geometry_vis.isChecked():
            actors = [self.glyph_actor]
        elif name == 'axes':
            actors = [self.axes]
        elif name == 'boundary':
            actors = [self.boundary_dict.get('actor', None)]
        elif name == 'mesh':
            actors = [self.mesh_dict.get('actor', None)]

        if actors is not None:
            for actor in actors:
                if actor is not None:
                    if visible:
                        actor.VisibilityOn()
                    else:
                        actor.VisibilityOff()
            self.render()

    def get_actors(self, name):
        """given a scene actor type, return the actors"""
        actors = []
        if name == 'background_mesh':
            actors = self.grid_viewer_dict['actors']
        elif name == 'geometry':
            actors = [geo['actor'] for geo in self.geometrydict.values()]
        elif name == 'regions':
            actors = []
            for geo in self.region_dict.values():
                clip = geo.get('clip_actor', None)
                if clip is not None:
                    actors.append(clip)
                else:
                    actors.append(geo.get('actor', None))

        elif name == 'normals':
            actors = [self.glyph_actor]
        elif name == 'boundary':
            a = self.boundary_dict.get('actor', None)
            if a is not None:
                actors = [a]
        elif name == 'mesh':
            a = self.mesh_dict.get('actor', None)
            if a is not None:
                actors = [a]

        for actor in actors:
            yield actor

    def change_representation(self, name, combobox):
        """given a scene actor type, change the representation"""
        representation = str(combobox.currentText())

        self.visual_props[name]['rep'] = representation

        for actor in self.get_actors(name):
            self.set_representation(actor, representation)
        self.render()

    def change_color(self, name, button):
        """given a scene actor type, change the color"""
        col = QColorDialog.getColor(parent=self)
        if not col.isValid():
            return

        button.setStyleSheet("QToolButton{{ background: {};}}".format(
            col.name()))

        self.visual_props[name]['color'] = col
        dark = self.visual_props[name]['edge'] = col.darker()

        for actor in self.get_actors(name):
            if actor is not None:
                actor.GetProperty().SetColor(col.getRgbF()[:3])
                actor.GetProperty().SetEdgeColor(dark.getRgbF()[:3])

        self.render()

    def change_opacity(self, opacity, name):
        """given a scene actor type, change the opacity"""
        opacity = opacity.value()
        self.visual_props[name]['opacity'] = opacity

        for actor in self.get_actors(name):
            if actor is not None:
                actor.GetProperty().SetOpacity(opacity)
        self.render()

    def set_visible_btn_image(self, btn, checked):
        """given a button, change the icon"""
        if not checked:
            btn.setIcon(get_icon('visibilityofflight.svg'))
        else:
            btn.setIcon(get_icon('visibility.svg'))

    # --- wizards ---
    def handle_cyclone_wizard(self):
        """show the cyclone wizard"""
        if self.cyclone_popup is None:
            self.cyclone_popup = CycloneWizard(self)
        self.cyclone_popup.popup()

    def handle_distributed_wizard(self):
        """show the distributed wizard"""
        if self.distribution_popup is None:
            self.distribution_popup = DistributionWizard(self)
        self.distribution_popup.popup()

    def handle_reactor_wizard(self):
        """show the reactor wizard"""
        if self.reactor_popup is None:
            self.reactor_popup = ReactorWizard(self)
        self.reactor_popup.popup()

    def handle_hopper_wizard(self):
        """show the hopper wizard"""
        if self.hopper_popup is None:
            self.hopper_popup = HopperWizard(self)
        self.hopper_popup.popup()
