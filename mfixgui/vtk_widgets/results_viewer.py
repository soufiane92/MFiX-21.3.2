# -*- coding: utf-8 -*-
import copy
import glob
import os
from bisect import bisect_left
from collections import OrderedDict
from collections.abc import Mapping
from qtpy import QtCore, QtGui, QtWidgets

from mfixgui.colormaps.color_maps import build_qicons
from mfixgui.tools.qt import get_icon, sub_icon_size, get_ui, SETTINGS
from mfixgui.vtk_widgets.base import BaseVtkWidget
from mfixgui.vtk_widgets.constants import SPLAT_SHADERS
from mfixgui.vtk_widgets.tools import safe_float, safe_int, safe_combo, parse_pvd_file
from mfixgui.vtk_widgets.dialogs import CreateMoviePopUp

# graphics libraries
try:
    import vtk
    from mfixgui.colormaps.color_maps import build_vtk_lookup_tables
    VTK_AVAILABLE = True
    LOOKUP_TABLES = build_vtk_lookup_tables()
    GLYPHS = {
        'sphere':   vtk.vtkSphereSource,
        'cube':     vtk.vtkCubeSource,
        'cylinder': vtk.vtkCylinderSource,
        'cone':     vtk.vtkConeSource}

    # check for point gaussian, vtk 7+
    POINT_GAUSSIAN = hasattr(vtk, 'vtkPointGaussianMapper')
except ImportError:
    vtk = None
    VTK_AVAILABLE = False
    LOOKUP_TABLES = {}
    POINT_GAUSSIAN = False
    GLYPHS = {}

DEFAULT_MAXIMUM_POINTS = 10000
MAX_PLAYBACK_DELAY = 1000  # ms
DEFAULT_PLAYBACK_DELAY = 100  # ms
DEFAULT_GEO_COLOR = QtGui.QColor(224, 224, 224)
DEFAULT_TEXT_COLOR = QtGui.QColor(0, 0, 0)
GEOMETRY = ['cells', 'points', 'geometry', 'color_bar', 'time_label', 'axes']
INDEX_DICT = {'x': 0, 'y': 1, 'z': 2, 'mag': -1}


def get_closest(my_list, my_number):
    """
    Assumes my_list is sorted. Returns closest value index to my_number.

    If two numbers are equally close, return the smallest number index.

    altered from: https://stackoverflow.com/questions/12141150/from-list-of-integers-get-number-closest-to-a-given-value/12141511#12141511
    """
    pos = bisect_left(my_list, my_number)
    if pos == 0:
        return 0
    if pos == len(my_list):
        return -1
    before = my_list[pos - 1]
    after = my_list[pos]
    if after - my_number < my_number - before:
       return my_list.index(after)
    else:
       return my_list.index(before)


def clean_dict(dirty_dict):
    """remove qcolor objects save the hex values"""
    cd = {}
    for k, v in dirty_dict.items():
        if isinstance(v, (dict, OrderedDict)):
            cd[k] = clean_dict(v)
        elif isinstance(v, QtGui.QColor):
            cd[k] = v.name()
        else:
            cd[k] = v
    return cd


def qcolor_dict(d):
    """the reverse of clean_dict, change hex back to qcolor"""
    qd = {}
    for k, v in d.items():
        if isinstance(v, (dict, OrderedDict)):
            qd[k] = qcolor_dict(v)
        elif isinstance(v, str) and v.startswith('#'):
            qd[k] = QtGui.QColor(v)
        else:
            qd[k] = v
    return qd


def build_time_dict(search_str):
    '''given a glob search string, return a dictionary of
    time (float) : file_name'''
    f_dict = OrderedDict()

    files = glob.glob(search_str)
    for f in sorted(files):
        time = None
        with open(f, 'r', encoding='utf-8', errors='replace') as xml:
            for i, line in enumerate(xml):
                if '<!-- Time =' in line:
                    try:
                        time = float(line.replace('<!-- Time =', '').replace('sec. -->', ''))
                    except:
                        pass
                    break

                if i > 4:
                    break
        if time is not None:
            f_dict[time] = os.path.basename(f)
    return f_dict


def update(d, u):
    for k, v in u.items():
        if isinstance(v, Mapping):
            r = update(d.get(k, {}), v)
            d[k] = r
        else:
            d[k] = u[k]
    return d


class ColorMapPopUp(QtWidgets.QDialog):
    applyEvent = QtCore.Signal(object, object, object)

    def __init__(self, parent=None):
        QtWidgets.QDialog.__init__(self, parent)

        self.color = None
        ui = self.ui = get_ui('color_map.ui', self)

        self.setWindowTitle('Color Map')

        ui.toolbutton_select_color.clicked.connect(self.select_color)
        ui.toolbutton_select_color.setStyleSheet(
            "QToolButton{{ background: {};}}".format(DEFAULT_GEO_COLOR.name()))
        ui.lineedit_from.dtype = float
        ui.lineedit_to.dtype = float

        for name, icons in build_qicons().items():
            if not name.endswith('_reversed'):
                ui.combobox_color_map.addItem(
                    icons.get('bar', QtGui.QIcon()), name)
        self.set_color_map('viridis')

        btn = ui.buttonBox.button(QtWidgets.QDialogButtonBox.Apply)
        btn.clicked.connect(self.emit_apply_event)
        btn = ui.buttonBox.button(QtWidgets.QDialogButtonBox.Ok)
        btn.clicked.connect(self.emit_apply_event)

        ui.toolbutton_auto_scale.clicked.connect(self.auto_scale)

    def emit_apply_event(self):
        self.applyEvent.emit(self.geo, self.button, self.array)

    def set_(self, array, comp):
        self.array = array
        self.comp = comp
        self.color = self.array.get('color', DEFAULT_GEO_COLOR)
        self.set_color(self.color)
        self.set_color_map(self.array.get('color_map', 'viridis'))
        self.ui.checkbox_reversed.setChecked(self.array.get('reversed', False))

        d_range = self.array.get('range', [[0, 1]])
        if len(d_range) == 3:
            if comp >= 0:
                d_range = d_range[comp]
            else:
                d_range = self.array.get('magnitude', [0, 1])
        else:
            d_range = d_range[0]
        self.ui.lineedit_from.updateValue(
            None, self.array.get('from', '{:.3g}'.format(d_range[0])))
        self.ui.lineedit_to.updateValue(
            None, self.array.get('to', '{:.3g}'.format(d_range[1])))

        single_color = self.array.get('single_color', False)
        self.ui.checkbox_single_color.setChecked(single_color)
        self.ui.widget_color_map.setEnabled(not single_color)

    def get(self):
        color_map = self.ui.combobox_color_map.currentText()
        reverse = self.ui.checkbox_reversed.isChecked()

        rng = [safe_float(self.ui.lineedit_from.value, 0),
               safe_float(self.ui.lineedit_to.value, 1)]

        if reverse:
            color_map += '_reversed'
        return {
            'color':        self.color,
            'single_color': self.ui.checkbox_single_color.isChecked(),
            'color_map':    color_map,
            'reversed':     reverse,
            'from':         min(rng),
            'to':           max(rng),
            }

    def select_color(self):
        col = QtWidgets.QColorDialog.getColor()
        if not col.isValid():
            return
        self.color = col
        self.set_color(col)

    def set_color(self, color):
        if isinstance(color, QtGui.QColor):
            self.ui.toolbutton_select_color.setStyleSheet("QToolButton{{ background: {};}}".format(
                color.name()))

    def set_color_map(self, color_map):
        color_map = color_map.replace('_reversed', '')
        self.ui.combobox_color_map.setCurrentText(color_map)

    def auto_scale(self):
        comp = self.comp
        d_range = self.array.get('range', [[0, 1]])
        if len(d_range) == 3:
            if comp >= 0:
                d_range = d_range[comp]
            else:
                d_range = self.array.get('magnitude', [0, 1])
        else:
            d_range = d_range[0]
        self.ui.lineedit_from.updateValue(None, '{:.3g}'.format(d_range[0]))
        self.ui.lineedit_to.updateValue(None, '{:.3g}'.format(d_range[1]))

    def popup(self, text):

        self.setWindowTitle('Change {} color map'.format(text))

        self.show()
        self.raise_()
        self.activateWindow()


class GraphicsVtkWidget(BaseVtkWidget):
    """vtk widget for showing results"""
    def __init__(self, parent=None, load=False):
        BaseVtkWidget.__init__(self, parent)

        self.loading = False
        self.cell_arrays = {}
        self.vtu_pattern = None
        self.node_arrays = {}
        self.point_arrays = {}
        self.vtp_pattern = None
        self.pvd_files = {}
        self.frame_index = -1
        self.vtp_files = {}
        self.vtu_files = {}
        self.update_color_by = False
        self.time = 0.0
        self.time_format = '{:.2f} s'
        self.time_label_color = DEFAULT_TEXT_COLOR
        self.color_bar_color = DEFAULT_TEXT_COLOR
        self.geometry_color = DEFAULT_GEO_COLOR
        self.particle_mapper_str = 'point gaussian' if POINT_GAUSSIAN else 'glyphs'
        self.particle_render_options = {
            'max_points': DEFAULT_MAXIMUM_POINTS,
            'glyph': 'sphere',
            'mapper': self.particle_mapper_str,
            'splat': 'sphere',
            }

        self.play_timer = QtCore.QTimer()
        self.play_timer.timeout.connect(self.forward)

        # look for vtu files
        self.file_timer = QtCore.QTimer()
        self.file_timer.timeout.connect(self.look_for_files)
        self.file_timer.start(1000)

        # dialogs
        self.color_dialog = ColorMapPopUp(self)
        self.color_dialog.applyEvent.connect(self.change_color)

        self.movie_dialog = CreateMoviePopUp(self)

        self.init_toolbar()
        self.init_vtk()
        self.init_geometry()

        if not load:
            # look for files
            self.look_for_files()
            self.change_frame(0)
            self.reset_view()
            if self.vtu_pattern is not None:
                self.change_color_by('cells')
            if self.vtp_pattern is not None:
                self.change_color_by('points')

    @property
    def project_dir(self):
        return self.gui.get_project_dir()

    def set_state(self, state):
        '''load a saved vtk state'''
        self.defer_render = True # defer rendering vtk until load complete
        self.loading = True
        sb = self.sidebar

        state = qcolor_dict(state)

        # set particle mapper
        if not POINT_GAUSSIAN:
            self.particle_mapper_str = 'glyphs'
        else:
            self.particle_mapper_str = state.get('particle_options', {}).get('mapper', 'glyphs')

        # look for files
        self.look_for_files()

        # update colorbar/range/etc. data
        self.cell_arrays.update(state.get('cell_arrays', {}))
        self.node_arrays.update(state.get('node_arrays', {}))
        self.point_arrays.update(state.get('point_arrays', {}))

        # set the file patterns
        for geo, key in [('cells', 'vtu_pattern'), ('points', 'vtp_pattern')]:
            cb = self.visual_btns[geo]['file_pattern']
            val = state.get(key, None)
            if val is not None:
                val = safe_combo(cb, val)
                self.change_file_pattern(geo, cb, new=val)

        # set the array names
        for geo in ['cells', 'points']:
            cb = self.visual_btns[geo]['color_by']
            color_button = self.visual_btns[geo]['color']
            component = self.visual_btns[geo]['component']

            array_name = state.get('_'.join([geo, 'color_by']), None)
            if array_name is not None:
                safe_combo(cb, array_name)
                self.handle_change_color(geo, color_button, popup=False)

                val = state.get('_'.join([geo, 'component']), None)
                safe_combo(component, val)
                self.change_color_by(geo)

        # particle render options
        self.set_particle_options(state.get('particle_options', {}))
        self.handle_particle_mapper_changed()

        # visibility and opacity for all actors
        visible = state.get('visible', {})
        opacity = state.get('opacity', {})
        for geo_name in GEOMETRY:
            geo = geo_name.lower().replace(' ', '_')
            geo_btns = self.visual_btns.get(geo)

            # change visibility
            vis = visible.get(geo, True)
            geo_btns['visible'].setChecked(vis)
            self.change_visibility(geo, vis)

            # change opacity
            op = opacity.get(geo, 1)
            if 'opacity' in geo_btns:
                geo_btns['opacity'].setValue(op)
                self.change_opacity(geo, op)

        # geometry
        sb.comboBox_regions_style.setCurrentText(state.get('geometry_style', 'solid'))
        self.change_geo_color(color=state.get('geometry_color',  DEFAULT_GEO_COLOR))

        # color_bar
        safe_combo(sb.comboBox_color_bar_field,
                   state.get('color_bar_mapper', 'cells/nodes').lower())
        sb.spinBox_color_bar_n_labels.setValue(state.get('color_bar_n_labels', 10))
        sb.lineEdit_color_bar_format.setText(state.get('color_bar_label_fmt', '%.2f'))
        sb.comboBox_color_bar_position.setCurrentText(state.get('color_bar_pos', 'right').lower())
        self.change_color_bar_color(color=state.get('color_bar_color',  DEFAULT_TEXT_COLOR))

        # time label
        sb.lineEdit_time_label_format.setText(state.get('time_label_format', '{:.2f}'))
        sb.comboBox_time_label_loc.setCurrentText(state.get('time_label_pos', 'top right'))
        sb.spinBox_time_label_size.setValue(state.get('time_label_text_size', 24))
        self.change_time_label_color(color=state.get('time_label_color',  DEFAULT_TEXT_COLOR))

        # camera
        camera_state = state.get('camera', None)
        if camera_state is not None:
            self.set_camera_state(camera_state)

        # image stack
        project_path = os.path.dirname(self.gui.get_project_file())
        sb.lineEdit_image_stack_dir.setText(state.get('image_stack_dir', project_path))
        sb.lineEdit_image_stack_prefix.setText(state.get('image_stack_prefix', 'frame_'))
        sb.lineEdit_image_stack_width.setText(state.get('image_stack_width', '1920'))
        sb.lineEdit_image_stack_height.setText(state.get('image_stack_height', '1080'))
        safe_combo(sb.comboBox_image_stack_type, state.get('image_stack_type', 'png'))
        sb.checkBox_trans_back.setChecked(state.get('image_stack_trans', False))

        # set the current frame
        self.change_frame(state.get('frame', 0))

        self.render(defer_render=False)  # render
        self.loading = False

    def set_unsaved_flag(self):
        """don't call set_unsaved_flag when loading"""
        if not self.loading:
            self.gui.set_unsaved_flag()

    def get_state(self):
        '''collect a dictionary of values to save'''
        sb = self.sidebar
        state = {
            'vtu_pattern': self.vtu_pattern,
            'vtp_pattern': self.vtp_pattern,
            'cell_arrays': self.cell_arrays,
            'node_arrays': self.node_arrays,
            'point_arrays': self.point_arrays,
            'frame': self.frame_index,
            'camera': self.get_camera_state(),
            'particle_options': self.particle_render_options,

            # geometry
            'geometry_style': sb.comboBox_regions_style.currentText(),
            'geometry_color': self.geometry_color,

            # color bar
            'color_bar_mapper': sb.comboBox_color_bar_field.currentText(),
            'color_bar_pos': sb.comboBox_color_bar_position.currentText(),
            'color_bar_color': self.color_bar_color,
            'color_bar_n_labels': sb.spinBox_color_bar_n_labels.value(),
            'color_bar_label_fmt': sb.lineEdit_color_bar_format.text(),

            # time label
            'time_label_format': sb.lineEdit_time_label_format.text(),
            'time_label_pos': sb.comboBox_time_label_loc.currentText(),
            'time_label_color': self.time_label_color,
            'time_label_text_size': sb.spinBox_time_label_size.value(),

            # image stack
            'image_stack_dir': sb.lineEdit_image_stack_dir.text(),
            'image_stack_prefix': sb.lineEdit_image_stack_prefix.text(),
            'image_stack_width': sb.lineEdit_image_stack_width.text(),
            'image_stack_height': sb.lineEdit_image_stack_height.text(),
            'image_stack_type': sb.comboBox_image_stack_type.currentText(),
            'image_stack_trans': sb.checkBox_trans_back.isChecked(),
            }

        # save array and component selection
        for geo in ['cells', 'points']:
            for key in ['color_by', 'component']:
                val = self.visual_btns[geo][key].currentText()
                if val:
                    state['_'.join([geo, key])] = val

        # opacity/visible
        visible = state['visible'] = {}
        opacity = state['opacity'] = {}
        for geo_name in GEOMETRY:
            geo = geo_name.lower().replace(' ', '_')
            geo_btns = self.visual_btns.get(geo)
            visible[geo] = geo_btns['visible'].isChecked()
            if 'opacity' in geo_btns:
                opacity[geo] = geo_btns['opacity'].value()

        return clean_dict(state)

    def reset(self):
        self.play_timer.stop()
        self.file_timer.stop()
        self.vtkRenderWindow.Finalize()

    def init_vtk(self):

        self.actors = {'time_label': self.time_label,
                       'color_bar':  self.scalar_bar,
                       'axes':       self.axes}
        self.mappers = {}
        self.lookuptables = {}

        self.ugrid_cell_mapper = None
        self.ugrid_node_mapper = None
        self.particle_mapper_glyph = None
        self.particle_mapper_pg = None
        self.particle_reader = None

        self.time_label.SetVisibility(True)
        self.time_label.SetInput(self.time_format.format(self.time))

    @property
    def particle_mapper(self):
        return self.particle_mapper_glyph if self.particle_mapper_str == 'glyphs' else self.particle_mapper_pg

    @property
    def points_str(self):
        return 'points_glyph' if self.particle_mapper_str == 'glyphs' else 'points_pg'

    @property
    def cells_str(self):
        return self.sidebar.comboBox_cells_style.currentText()

    def init_ugrid(self, parallel=False):
        '''setup the cell/point vtk stuff'''
        # cells
        self.enable_toolbar_geo('cells', visible=False)
        self.enable_colorbar_select(0)
        if parallel:
            self.ugrid_reader = vtk.vtkXMLPUnstructuredGridReader()
        else:
            self.ugrid_reader = vtk.vtkXMLUnstructuredGridReader()

        # filter out unused arrays
        self.pass_array = vtk.vtkPassArrays()
        self.pass_array.SetInputConnection(self.ugrid_reader.GetOutputPort())
        # self.pass_array.AddCellDataArray(var)

        # extract surface
        self.surface_extract = vtk.vtkDataSetSurfaceFilter()
        self.surface_extract.SetInputConnection(self.pass_array.GetOutputPort())

        self.ugrid_cell_mapper = self.mappers['cells'] = vtk.vtkDataSetMapper()
        self.ugrid_cell_mapper.SetInputConnection(self.surface_extract.GetOutputPort())
        self.ugrid_cell_mapper.SetScalarVisibility(True)
        self.ugrid_cell_mapper.SetScalarModeToUseCellFieldData()
        self.change_color_map('cells', 'viridis')

        actor = self.actors['cells'] = vtk.vtkActor()
        actor.SetMapper(self.ugrid_cell_mapper)

        self.vtkrenderer.AddActor(actor)
        actor.SetVisibility(False)  # hide cells by default

        # nodes
        self.ugrid_cell_to_points = vtk.vtkCellDataToPointData()
        self.ugrid_cell_to_points.SetInputConnection(self.surface_extract.GetOutputPort())

        self.ugrid_node_mapper = self.mappers['nodes'] = vtk.vtkDataSetMapper()
        self.ugrid_node_mapper.SetInputConnection(self.ugrid_cell_to_points.GetOutputPort())
        self.ugrid_node_mapper.SetScalarVisibility(True)
        self.ugrid_node_mapper.SetScalarModeToUsePointFieldData()
        #self.ugrid_node_mapper.UseLookupTableScalarRangeOn()
        #self.ugrid_node_mapper.InterpolateScalarsBeforeMappingOn()
        self.change_color_map('nodes', 'viridis')

        actor = self.actors['nodes'] = vtk.vtkActor()
        actor.SetMapper(self.ugrid_node_mapper)
        actor.SetVisibility(False)  # hide nodes by default

        self.vtkrenderer.AddActor(actor)

    def init_particles(self, parallel):
        '''setup the particle vtk stuff'''
        self.enable_toolbar_geo('points')
        self.enable_colorbar_select(1)

        if self.particle_reader is None:
            if parallel:
                self.particle_reader = vtk.vtkXMLPPolyDataReader()
            else:
                self.particle_reader = vtk.vtkXMLPolyDataReader()

        if POINT_GAUSSIAN and self.particle_mapper_str == 'point gaussian':
            self.init_point_gaussian()
        else:
            self.particle_mapper_str = 'glyphs'
            self.init_glyph_mapper()

    def init_glyph_mapper(self):
        # glyph mapper
        self.glyph_mask = vtk.vtkMaskPoints()
        self.glyph_mask.SetInputConnection(self.particle_reader.GetOutputPort())
        self.glyph_mask.RandomModeOn()
        self.glyph_mask.SetRandomModeType(2) # setting to type 1 crashes on windows
        self.glyph_mask.SetMaximumNumberOfPoints(self.particle_render_options.get('max_points', DEFAULT_MAXIMUM_POINTS))

        self.glyph = vtk.vtkGlyph3D()
        self.glyph.SetInputConnection(self.glyph_mask.GetOutputPort())
        self.glyph.SetColorModeToColorByVector()
        self.set_glyph_source(self.particle_render_options.get('glyph', 'sphere'))

        self.particle_mapper_glyph = self.mappers['points_glyph'] = vtk.vtkPolyDataMapper()
        self.particle_mapper_glyph.SetInputConnection(self.glyph.GetOutputPort())
        self.change_color_map('points', 'viridis')

        actor = self.actors['points_glyph'] = vtk.vtkActor()
        actor.SetMapper(self.particle_mapper_glyph)
        if hasattr(actor, 'SetForceOpaque'):
            actor.SetForceOpaque(True)
        self.vtkrenderer.AddActor(actor)

    def init_point_gaussian(self):

        # setup the point gaussian mapper
        pg = vtk.vtkPointGaussianMapper()
        pg.EmissiveOff()
        pg.SetScaleArray('Diameter')
        pg.SetScaleFactor(.5)
        pg.SetScalarVisibility(True)
        pg.SetScalarModeToUsePointFieldData()
        pg.SetInputConnection(self.particle_reader.GetOutputPort())
        pg.SetSplatShaderCode(SPLAT_SHADERS.get('sphere'))
        self.particle_mapper_pg = self.mappers['points_pg'] = pg
        self.change_color_map('points', 'viridis')

        actor= self.actors['points_pg'] = vtk.vtkActor()
        actor.SetMapper(self.particle_mapper_pg)
        actor.SetForceOpaque(True)

        self.vtkrenderer.AddActor(actor)

    def init_geometry(self):
        self.enable_toolbar_geo('geometry')
        poly_data = self.gui.vtkwidget.collect_toplevel_geometry(visible_only=True)

        # Create a mapper
        mapper = self.mappers['geometry'] = vtk.vtkPolyDataMapper()
        if poly_data is not None:
            mapper.SetInputConnection(poly_data.GetOutputPort())
        mapper.ScalarVisibilityOff()

        # Create an actor
        actor = self.actors['geometry'] = vtk.vtkActor()
        actor.SetMapper(mapper)
        actor.GetProperty().SetColor(DEFAULT_GEO_COLOR.getRgbF()[:3])
        actor.GetProperty().SetOpacity(0.4)

        self.vtkrenderer.AddActor(actor)

    def update_geometry(self):
        poly_data = self.gui.vtkwidget.collect_toplevel_geometry(visible_only=True)
        mapper = self.mappers['geometry']
        mapper.RemoveAllInputConnections(0)
        if poly_data is not None:
            mapper.SetInputConnection(poly_data.GetOutputPort())
        self.render()

    def enable_toolbar_geo(self, geo, visible=True):
        """enable/disable widgets for a particular item"""
        sb = self.sidebar
        btn, wid = None, None
        if geo == 'cells' or geo == 'nodes':
            btn, wid = sb.toolButton_cells_nodes, sb.widget_cells_nodes
        elif geo == 'points':
            btn, wid = sb.toolButton_points, sb.widget_points
        elif geo == 'geometry':
            btn, wid = sb.toolButton_geometry, sb.widget_geometry
        elif geo == 'color_bar':
            btn, wid = sb.toolButton_color_bar, sb.widget_color_bar
        elif geo == 'time_label':
            btn, wid = sb.toolButton_time_label, sb.widget_time_label
        elif geo == 'image_stack':
            btn, wid = sb.toolButton_camera, sb.widget_image_stack
        else:
            print('Unknown geo:', geo)

        if btn is not None:
            btn.setChecked(visible)
            btn.setEnabled(True)
        if wid is not None:
            wid.setEnabled(True)

    def enable_colorbar_select(self, index, enable=True):
        cb = self.sidebar.comboBox_color_bar_field
        model = cb.model()

        item = model.item(index)
        if not enable:
            flags = QtCore.Qt.NoItemFlags
        else:
            flags = QtCore.Qt.ItemIsSelectable | QtCore.Qt.ItemIsEnabled
        item.setFlags(flags)

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

        for wid in [sb.widget_cells_nodes, sb.widget_points, sb.widget_geometry,
                    sb.widget_color_bar, sb.widget_time_label, sb.label_axes,
                    sb.widget_playback, sb.widget_image_stack]:
            wid.setVisible(visible)

        size_hint = sb.scrollAreaWidgetContents.minimumSizeHint()
        width = size_hint.width()
        sb.setMaximumWidth(width if not visible else 16777215)

    def init_toolbar(self):
        """ build the toolbar and sidebar"""

        # build to sidebar
        sb = self.sidebar = get_ui('results_viewer_sidebar.ui')
        self.grid_layout.addWidget(self.sidebar, 1, 1)
        self.hide_show_sidebar(False)

        # tweak scrollarea
        # monkey patching
        def customResize(event):
            sc = sb.scrollAreaWidgetContents
            sa = sb.scrollArea
            sbar = sa.verticalScrollBar()
            size_hint = sc.minimumSizeHint()
            width = size_hint.width()
            if size_hint.height() > sa.height():
                width += sbar.width()
            sa.setMinimumWidth(width)

        sb.scrollAreaWidgetContents.resizeEvent = customResize

        # hide/Show
        tb = sb.toolButton_hide_show
        tb.setIcon(get_icon('left.svg'))
        tb.setToolTip('Open sidebar')
        tb.clicked.connect(lambda ignore: self.hide_show_sidebar())
        tb.setIconSize(sub_icon_size())

        def hide_show_options_widget(tb, opt_widget, visible=None):
            if visible is None:
                visible = not opt_widget.isVisible()
            opt_widget.setVisible(visible)
            tb.setArrowType(QtCore.Qt.UpArrow if visible else QtCore.Qt.DownArrow)

        # toggles
        for tb, opt in [
                (sb.toolButton_cells_nodes_toggle, sb.widget_cells_nodes_opts),
                (sb.toolButton_points_toggle, sb.widget_points_opts),
                (sb.toolButton_geometry_toggle, sb.widget_geometry_opts),
                (sb.toolButton_color_bar_toggle, sb.widget_color_bar_opts),
                (sb.toolButton_time_label_toggle, sb.widget_time_label_opts),
                (sb.toolButton_playback_toggle, sb.widget_playback_opts),
                (sb.toolButton_image_stack_toggle, sb.widget_image_stack_opts),
                ]:
            hide_show_options_widget(tb, opt, False)
            tb.clicked.connect(
                lambda down, tb=tb, w=opt: hide_show_options_widget(tb, w))

        # vis btns
        icon_size = sub_icon_size()
        sb.toolButton_cells_nodes.setIcon(get_icon('grid.svg'))
        sb.toolButton_points.setIcon(get_icon('dem.svg'))
        sb.toolButton_geometry.setIcon(get_icon('geometry.svg'))
        sb.toolButton_color_bar.setIcon(get_icon('gradient.svg'))
        sb.toolButton_time_label.setIcon(get_icon('time.svg'))
        sb.toolButton_axes_vis.setIcon(get_icon('axes.svg'))
        sb.toolButton_camera.setIcon(get_icon('camera_stack.svg'))
        sb.toolButton_camera.setIconSize(icon_size)
        sb.toolButton_playback.setIcon(get_icon('speed.svg'))
        sb.toolButton_playback.setIconSize(icon_size)

        vbtns = self.visual_btns = dict((k, {}) for k in GEOMETRY)
        tb_list = [sb.toolButton_cells_nodes, sb.toolButton_points,
                   sb.toolButton_geometry, sb.toolButton_color_bar,
                   sb.toolButton_time_label, sb.toolButton_axes_vis]
        for tb, geo in zip(tb_list, GEOMETRY):
            tb.clicked.connect(lambda down, g=geo: self.change_visibility(g, down))
            vbtns[geo]['visible'] = tb
            if geo not in ['color_bar', 'time_label', 'axes']:
                tb.setChecked(False)
            tb.setIconSize(icon_size)

        # cells/points
        # file patterns
        for cb, geo in [(sb.comboBox_cells_file_pattern, 'cells'),
                        (sb.comboBox_points_file_pattern, 'points')]:
            cb.activated.connect(lambda item, g=geo, c=cb: self.change_file_pattern(g, c))
            vbtns[geo]['file_pattern'] = cb

        # cell color
        for wid in [sb.comboBox_cells_style, sb.comboBox_cells_variable,
                    sb.comboBox_cells_component]:
            wid.activated.connect(self.change_cell_color)

        # point color
        for wid in [sb.comboBox_points_variable, sb.comboBox_points_component]:
            wid.activated.connect(self.change_point_color)

        # variable
        for cb, geo in [(sb.comboBox_cells_variable, 'cells'),
                        (sb.comboBox_points_variable, 'points')]:
            vbtns[geo]['color_by'] = cb

        # component
        for cb, geo in [(sb.comboBox_cells_component, 'cells'),
                        (sb.comboBox_points_component, 'points')]:
            vbtns[geo]['component'] = cb

        # colors
        for tb, geo in [(sb.toolButton_cells_color, 'cells'),
                        (sb.toolButton_points_color, 'points')]:
            tb.clicked.connect(lambda ignore, g=geo,
                               t=tb: self.handle_change_color(g, t))
            tb.setIcon(build_qicons().get('viridis', {}).get('icon', QtGui.QIcon))

        # -- points --
        for wid in [sb.comboBox_partilce_mapper, sb.comboBox_particle_shader,
                    sb.comboBox_glyph, sb.lineEdit_maximum_glyph_points]:
            wid.value_updated.connect(self.handle_particle_mapper_changed)
        sb.comboBox_particle_shader.addItems(SPLAT_SHADERS.keys())
        sb.comboBox_glyph.addItems(GLYPHS.keys())
        sb.label_glyph.setVisible(False)
        sb.comboBox_glyph.setVisible(False)
        sb.label_max_points.setVisible(False)
        sb.lineEdit_maximum_glyph_points.setVisible(False)

        # -- geometry --
        sb.comboBox_regions_style.currentIndexChanged.connect(self.change_geo_style)
        sb.toolButton_geometry_color.clicked.connect(self.change_geo_color)
        sb.toolButton_geometry_color.setStyleSheet(
            "QToolButton{{ background: {};}}".format(DEFAULT_GEO_COLOR.name()))

        # -- color bar --
        cb = sb.comboBox_color_bar_field
        cb.currentIndexChanged.connect(self.change_colorbar_mapper)
        vbtns['color_bar']['mapper'] = cb
        sb.spinBox_color_bar_n_labels.valueChanged.connect(self.change_color_bar_n_labels)
        sb.lineEdit_color_bar_format.textChanged.connect(self.change_color_bar_format)
        sb.comboBox_color_bar_position.currentIndexChanged.connect(self.change_color_bar_loc)
        sb.toolButton_color_bar_color.clicked.connect(self.change_color_bar_color)
        sb.toolButton_color_bar_color.setStyleSheet(
            "QToolButton{{ background: {};}}".format(DEFAULT_TEXT_COLOR.name()))

        # -- time label --
        le = sb.lineEdit_time_label_format
        le.setText(self.time_format)
        le.textChanged.connect(self.handle_label_format)
        le.setToolTip('Format to be used in the display of the time label. '
                      'Needs to be a valid python format string such as '
                      '"{:.2f}", "{:.3g}", or "{:2E}".')
        vbtns['time_label']['label_format'] = le

        sb.comboBox_time_label_loc.currentIndexChanged.connect(self.change_time_label_loc)
        sb.toolButton_time_label_color.clicked.connect(self.change_time_label_color)
        sb.toolButton_time_label_color.setStyleSheet(
            "QToolButton{{ background: {};}}".format(DEFAULT_TEXT_COLOR.name()))
        sb.spinBox_time_label_size.valueChanged.connect(self.change_time_label_text_size)

        size = QtCore.QSize(25, 25)
        for tb, geo in zip([sb.toolButton_cells_color,
                            sb.toolButton_points_color,
                            sb.toolButton_geometry_color,
                            sb.toolButton_color_bar_color,
                            sb.toolButton_time_label_color,
                            ], GEOMETRY):
            tb.setMinimumSize(size)
            tb.setMaximumSize(size)
            tb.setIconSize(size)
            vbtns[geo]['color'] = tb

        # opacity
        for spb, geo in zip([sb.doubleSpinBox_cells_opacity,
                             sb.doubleSpinBox_points_opacity,
                             sb.doubleSpinBox_geometry_opacity,
                             sb.doubleSpinBox_color_bar_opacity,
                             sb.doubleSpinBox_time_label_opacity,
                             ], GEOMETRY):
            spb.valueChanged.connect(lambda o=spb, g=geo: self.change_opacity(g, o))
            vbtns[geo]['opacity'] = spb

        # playback
        slider = self.speed_slider = sb.horizontalSlider_playback_speed
        slider.setRange(0, MAX_PLAYBACK_DELAY) # delay = MAX - speed
        slider.setValue(MAX_PLAYBACK_DELAY - DEFAULT_PLAYBACK_DELAY)
        slider.sliderReleased.connect(self.handle_speed_changed)

        # image stack
        project_path = os.path.dirname(self.gui.get_project_file())
        sb.lineEdit_image_stack_dir.setText(project_path)
        sb.toolButton_image_stack_browse.setIcon(get_icon('folder.svg'))
        sb.toolButton_image_stack_browse.clicked.connect(self.browse_image_stack)

        # hide resolution widgets
        if not int(SETTINGS.value('enable_screenshot_res', 0)):
            for wid in [sb.label_image_stack_width, sb.lineEdit_image_stack_width,
                        sb.label_image_stack_height, sb.lineEdit_image_stack_height]:
                wid.setVisible(False)

        # create and add buttons to the toolbar
        self.init_base_toolbar()

        # --- video capture ---
        self.toolbutton_create_movie = QtWidgets.QToolButton()
        self.toolbutton_create_movie.clicked.connect(self.create_movie)
        self.toolbutton_create_movie.setIcon(get_icon('videocam.svg'))
        self.toolbutton_create_movie.setIconSize(sub_icon_size())
        self.toolbutton_create_movie.setToolTip('Create a video')

        # --- play/stop/forward/backward controls ---
        self.toolbutton_first = QtWidgets.QToolButton()
        self.toolbutton_first.clicked.connect(self.handle_first)
        self.toolbutton_first.setIcon(get_icon('first.svg'))
        self.toolbutton_first.setIconSize(sub_icon_size())
        self.toolbutton_first.setToolTip('First')

        self.toolbutton_back = QtWidgets.QToolButton()
        self.toolbutton_back.clicked.connect(self.handle_back)
        self.toolbutton_back.setIcon(get_icon('back.svg'))
        self.toolbutton_back.setIconSize(sub_icon_size())
        self.toolbutton_back.setToolTip('Previous')

        self.toolbutton_play = QtWidgets.QToolButton()
        self.toolbutton_play.clicked.connect(self.handle_play_stop)
        self.toolbutton_play.setIcon(get_icon('play.svg'))
        self.toolbutton_play.setIconSize(sub_icon_size())
        self.toolbutton_play.setToolTip('Play')

        self.toolbutton_next = QtWidgets.QToolButton()
        self.toolbutton_next.clicked.connect(self.handle_next)
        self.toolbutton_next.setIcon(get_icon('next.svg'))
        self.toolbutton_next.setIconSize(sub_icon_size())
        self.toolbutton_next.setToolTip('Next')

        self.toolbutton_last = QtWidgets.QToolButton()
        self.toolbutton_last.clicked.connect(self.handle_last)
        self.toolbutton_last.setIcon(get_icon('last.svg'))
        self.toolbutton_last.setIconSize(sub_icon_size())
        self.toolbutton_last.setToolTip('Last')

        self.toolbutton_repeat = QtWidgets.QToolButton()
        self.toolbutton_repeat.setIcon(get_icon('autorenew.svg'))
        self.toolbutton_repeat.setIconSize(sub_icon_size())
        self.toolbutton_repeat.setCheckable(True)
        self.toolbutton_repeat.setToolTip('Repeat from beginning')

        hspacer = QtWidgets.QSpacerItem(99999, 10, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Maximum,)

        self.frame_spinbox = QtWidgets.QSpinBox()
        self.frame_spinbox.editingFinished.connect(lambda: self.change_frame(self.frame_spinbox.value()))
        self.frame_spinbox.setMaximum(9999999)
        self.frame_spinbox.setButtonSymbols(QtWidgets.QAbstractSpinBox.NoButtons)

        self.checkbox_snap = QtWidgets.QCheckBox('Save Snapshots')

        for btn in [self.toolbutton_create_movie,
                    self.toolbutton_first, self.toolbutton_back,
                    self.toolbutton_play,
                    self.toolbutton_next, self.toolbutton_last,
                    self.toolbutton_repeat,
                    hspacer, self.frame_spinbox,
                    ]:
            if btn == hspacer:
                self.button_bar_layout.addSpacerItem(btn)
            else:
                self.button_bar_layout.addWidget(btn)
            if isinstance(btn, QtWidgets.QToolButton):
                btn.setAutoRaise(True)
                btn.setFocusPolicy(QtCore.Qt.ClickFocus)

        self.button_bar_layout.addStretch()

    def create_movie(self):

        self.movie_dialog.popup()

    def showEvent(self, event):
        # has to be called after the widget is visible
        self.vtkiren.Initialize()

    def hideEvent(self, event):
        self.stop()

    def close(self):
        BaseVtkWidget.close(self)

        # clean up timer(s)
        self.play_timer.stop()
        self.file_timer.stop()

    def browse_image_stack(self):
        sb = self.sidebar
        path = sb.lineEdit_image_stack_dir.text()

        filename = QtWidgets.QFileDialog.getExistingDirectory(
            self, "Select a image stack directory", path)
        if isinstance(filename, (tuple, list)):
            filename = filename[0]
        if not filename:
            return
        sb.lineEdit_image_stack_dir.setText(filename)

    def handle_speed_changed(self):
        if self.play_timer.isActive():
            self.play_timer.stop()
            self.handle_play_stop()

    def stop(self):
        self.toolbutton_play.setIcon(get_icon('play.svg'))
        self.toolbutton_play.setToolTip("Play")
        self.play_timer.stop()

    def handle_play_stop(self):
        if self.play_timer.isActive():
            self.stop()
        else:
            self.toolbutton_play.setIcon(get_icon('stop.svg'))
            self.toolbutton_play.setToolTip("Stop")
            delay_ms = max(0, MAX_PLAYBACK_DELAY-self.speed_slider.value())
            self.play_timer.start(delay_ms)

    def handle_first(self):
        self.change_frame(0)

    def handle_back(self):
        self.change_frame(self.frame_index - 1)

    def handle_next(self):
        self.change_frame(self.frame_index + 1)

    def handle_last(self):
        self.change_frame(max(len(self.vtu_files), len(self.vtp_files))-1)

    def forward(self):
        self.change_frame(self.frame_index + 1)

    def change_frame(self, index, force=False):

        # assume that whatever one is bigger has the smaller time step
        n_vtp = len(self.vtp_files)
        n_vtu = len(self.vtu_files)
        n_max = max(n_vtp, n_vtu)
        sb = self.sidebar

        if self.play_timer.isActive() and self.toolbutton_repeat.isChecked() and index == n_max:
            index = 0
        elif index >= n_max:
            index = n_max-1
        elif index < 0:
            index = 0

        if index == self.frame_index and not force:
            return
        else:
            self.frame_index = index

        vtp_list = list(self.vtp_files.keys())
        vtu_list = list(self.vtu_files.keys())

        if n_max > 0:
            vtp_success = vtu_success = False
            if n_vtp > n_vtu:
                time = vtp_list[index]
                vtp_success = self.read_vtp(self.vtp_files[time])
                if n_vtu:
                    vtu_files = list(self.vtu_files.values())
                    vtu_success = self.read_vtu(vtu_files[get_closest(vtu_list, time)])
            else:
                time = vtu_list[index]
                vtu_success = self.read_vtu(self.vtu_files[time])
                if n_vtp:
                    vtp_files = list(self.vtp_files.values())
                    vtp_success = self.read_vtp(vtp_files[get_closest(vtp_list, time)])
            self.time = time

            if vtu_success or vtp_success:
                self.frame_spinbox.setValue(index)
                self.set_timelabel(text=self.time_format.format(time))
                self.render()

            if sb.toolButton_camera.isChecked():
                path = sb.lineEdit_image_stack_dir.text()
                prefix = sb.lineEdit_image_stack_prefix.text()
                width = safe_int(sb.lineEdit_image_stack_width.text(), 1920)
                height = safe_int(sb.lineEdit_image_stack_height.text(), 1080)
                type_ = sb.comboBox_image_stack_type.currentText()
                trans = sb.checkBox_trans_back.isChecked()
                if os.path.exists(path):
                    self.screenshot(
                        True,
                        fname=os.path.join(path, '{}{:06d}.{}'.format(prefix, index, type_)),
                        size=(width, height),
                        transparent=trans)

    def look_for_files(self):
        if self.project_dir is None:
            return
        pvd_files = glob.glob(os.path.join(self.project_dir, '*.pvd'))
        new_pattern = False
        for pvd in pvd_files:
            base_name = os.path.basename(pvd).replace('.pvd', '')
            files = parse_pvd_file(pvd) # returns OrderedDict keyed by time
            if files:
                if base_name in self.pvd_files:
                    f_dict = self.pvd_files[base_name]['files']
                    f_dict.update(files)
                    # remove missing files
                    remove_list = []
                    for key, path in f_dict.items():
                        if not os.path.exists(os.path.join(self.project_dir, path)):
                            remove_list.append(key)
                    for key in remove_list:
                        f_dict.pop(key)
                else:
                    # new file pattern
                    new_pattern = True
                    key = list(files.keys())[0] # files is nonempty
                    filename = files[key]
                    t = 'vtp' if filename and filename.endswith('vtp') else 'vtu'
                    self.pvd_files[base_name] = {'files':files, 'type':t}

        # update the combo_boxes
        vtp = []
        vtu = []
        for k, v in self.pvd_files.items():
            if v.get('type') == 'vtp':
                vtp.append(k)
            else:
                vtu.append(k)

        if self.vtp_pattern is None and vtp:
            self.vtp_pattern = vtp[0]
        if self.vtu_pattern is None and vtu:
            self.vtu_pattern = vtu[0]

        # update file pattern comboboxes
        def update_cb(cb, items):
            cur = cb.currentText()
            cb.clear()
            cb.addItems(items)
            if not len(cur) > 0 and len(items) > 0:
                cur = items[0]
            if cur:
                cb.setCurrentText(cur)
            else:
                cur = None
            return cur

        sb = self.sidebar

        # update cell/node
        cb = sb.comboBox_cells_file_pattern
        cur = update_cb(cb, vtu)
        if cur in self.pvd_files:
            self.vtu_files = self.pvd_files[cur]['files']

        # update particles
        cb = sb.comboBox_points_file_pattern
        cur = update_cb(cb, vtp)
        if cur in self.pvd_files:
            self.vtp_files = self.pvd_files[cur]['files']

        # render if new file pattern
        if new_pattern:
            self.change_frame(self.frame_index, force=True)

    def change_file_pattern(self, geo, combo, new=None):
        if new is None:
            new = combo.currentText()
        if not new:
            return
        pvd_file = self.pvd_files.get(new, None)
        if pvd_file is None:
            return
        if geo == 'points':
            self.vtp_pattern = new
            self.vtp_files = pvd_file['files']
        else:
            self.vtu_pattern = new
            self.vtu_files = pvd_file['files']
        self.update_color_by = True
        self.change_frame(self.frame_index, True)
        self.change_color_by(geo)
        self.set_unsaved_flag()

    # --- vtk functions ---
    def read_vtu(self, vtu_file):
        init = False
        if self.ugrid_cell_mapper is None:
            self.init_ugrid(parallel=vtu_file.endswith('.pvtu'))
            init = True

        path = os.path.join(self.project_dir, vtu_file)
        if not os.path.exists(path):
            return False

        self.ugrid_reader.SetFileName(path)
        self.ugrid_reader.Update()

        # TODO: Build Once
        data = self.ugrid_reader.GetOutput()
        cell_data = data.GetCellData()
        new_array_info = {}
        for i in range(cell_data.GetNumberOfArrays()):
            array = cell_data.GetArray(i)
            n_comp = array.GetNumberOfComponents()
            new_array_info[cell_data.GetArrayName(i)] = {
                'i': i,
                'components': n_comp,
                'range': [array.GetRange(i) for i in range(n_comp)],
                'magnitude': array.GetRange(-1)}


        cell_info = self.cell_arrays.get(self.vtu_pattern, {})
        node_info = self.node_arrays.get(self.vtu_pattern, {})
        cell_info = update(cell_info, copy.deepcopy(new_array_info))
        node_info = update(node_info, copy.deepcopy(new_array_info))

        self.cell_arrays[self.vtu_pattern] = cell_info
        self.node_arrays[self.vtu_pattern] = node_info

        # update combobox
        combo = self.visual_btns['cells']['color_by']
        items = cell_info.keys()
        name = combo.currentText()
        if not name or name not in items:
            name = cell_data.GetArrayName(0)  # can return None if there is no array
        combo.clear()
        combo.addItems(items)

        # do we have items?
        enabled = bool(items)
        combo.setEnabled(enabled)
        if enabled and name is not None:
            combo.setCurrentText(name)

        if init or self.update_color_by:
            # array filter
            self.pass_array.ClearArrays()
            if name is not None:
                self.pass_array.AddCellDataArray(name)
                self.ugrid_cell_mapper.SelectColorArray(name)

            if init:
                self.change_color_by('cells')
                self.reset_view()
                self.sidebar.toolButton_cells_nodes.setChecked(True)
                self.set_colorbar(mapper=self.ugrid_node_mapper, label=name)
                self.visual_btns['color_bar']['mapper'].setCurrentIndex(0)
            self.update_color_by = False

        return True

    def read_vtp(self, vtp_file):
        init = False
        if self.particle_mapper is None:
            self.init_particles(parallel=vtp_file.endswith('.pvtp'))
            init = True

        path = os.path.join(self.project_dir, vtp_file)
        if not os.path.exists(path):
            return False

        self.particle_reader.SetFileName(path)
        self.particle_reader.Update()

        data = self.particle_reader.GetOutput()
        point_data = data.GetPointData()
        new_array_info = {}
        n_tuples = None
        for i in range(point_data.GetNumberOfArrays()):
            array = point_data.GetArray(i)
            n_comp = array.GetNumberOfComponents()
            n_tuples = array.GetNumberOfTuples()
            new_array_info[point_data.GetArrayName(i)] = {
                'i': i,
                'number_of_tuples': n_tuples,
                'components': n_comp,
                'range': [array.GetRange(i) for i in range(n_comp)],
                'magnitude': array.GetRange(-1)}
        point_info = self.point_arrays.get(self.vtp_pattern, {})
        point_info = update(point_info, copy.deepcopy(new_array_info))
        self.point_arrays[self.vtp_pattern] = point_info
        if n_tuples:
            self.sidebar.lineEdit_point_count.setText('{}'.format(n_tuples))

        diameter = self.gui.project.get_value('d_p0', args=[1], default=0.5)
        if 'Diameter' in point_info:
            if self.particle_mapper_glyph is not None:
                self.glyph.SetScaleModeToScaleByScalar()
                self.glyph.SetScaleFactor(1)
                self.glyph.SetInputArrayToProcess(0, 0, 0, 0, 'Diameter')
            elif self.particle_mapper_pg is not None:
                self.particle_mapper_pg.SetScaleArray('Diameter')
                self.particle_mapper_pg.SetScaleFactor(.5)
        elif self.particle_mapper_glyph is not None:
            self.glyph.SetScaleFactor(diameter*2)
        elif self.particle_mapper_pg is not None:
            self.particle_mapper_pg.SetScaleFactor(diameter)

        # update combo box
        combo = self.visual_btns['points']['color_by']
        items = point_info.keys()
        name = combo.currentText()
        if not name or name not in items:
            name = point_data.GetArrayName(0)
        combo.clear()
        combo.addItems(items)
        combo.setEnabled(bool(items))
        combo.setCurrentIndex(combo.findText(name))

        if init or self.update_color_by:
            if self.particle_mapper_glyph is not None:
                self.glyph.SetInputArrayToProcess(1, 0, 0, 0, name)
            elif self.particle_mapper_pg is not None:
                self.particle_mapper_pg.SelectColorArray(name)

            if init:
                self.change_color_by('points')
                self.reset_view()
                self.sidebar.toolButton_points.setChecked(True)
                self.set_colorbar(mapper=self.particle_mapper, label=name)
                self.visual_btns['color_bar']['mapper'].setCurrentIndex(1)
            self.update_color_by = False
        return True

    def change_visibility(self, geo, visible):

        geo = self.points_str if geo == 'points' else geo
        geo = self.cells_str if geo == 'cells' else geo
        actor = self.actors.get(geo, None)
        if actor is not None:
            actor.SetVisibility(visible)
            self.render()
        self.set_unsaved_flag()

    def change_cell_color(self):
        ''' change the cell color '''
        sb = self.sidebar
        style = self.cells_str
        var = sb.comboBox_cells_variable.currentText()
        comp = sb.comboBox_cells_component.currentText()
        index = INDEX_DICT[comp]

        # make sure the style is visible
        if sb.toolButton_cells_nodes.isChecked():
            actor = self.actors.get(style)
            if actor is not None:
                actor.SetVisibility(True)

        # make sure the other one is hidden
        hide_actor = self.actors.get('nodes' if style == 'cells' else 'cells')
        if hide_actor is not None:
            hide_actor.SetVisibility(False)

        if self.ugrid_cell_mapper is None or self.ugrid_node_mapper is None:
            return

        # array filter
        self.pass_array.ClearArrays()
        self.pass_array.AddCellDataArray(var)

        if style == 'cells':
            self.ugrid_cell_mapper.SelectColorArray(var)
            array = self.cell_arrays.get(self.vtu_pattern, {}).get(var)
        else:
            self.ugrid_node_mapper.SelectColorArray(var)
            array = self.node_arrays.get(self.vtu_pattern, {}).get(var)
        if array is None:
            return

        mapper = self.mappers.get(style)
        mapper.SetScalarRange(safe_float(array.get('from', 0.0), 0.0),
                              safe_float(array.get('to', 1.0), 1.0))

        single_color = array.get('single_color', False)
        if single_color:
            mapper.ScalarVisibilityOff()
            color = array.get('color', QtCore.Qt.white)
            btn = sb.toolButton_cells_color
            btn.setStyleSheet("QToolButton{{ background: {};}}".format(color.name()))
            btn.setIcon(QtGui.QIcon())
        else:
            self.change_color_map(style, array.get('color_map', 'viridis'), index)
            mapper.ScalarVisibilityOn()

        sb.comboBox_cells_component.setEnabled(array['components'] == 3)
        self.change_colorbar_mapper()

        # make sure opacity is correct
        self.change_opacity(style, sb.doubleSpinBox_cells_opacity.value())

        self.render()
        self.set_unsaved_flag()

    def change_point_color(self):
        ''' change the point color '''
        sb = self.sidebar
        var = sb.comboBox_points_variable.currentText()
        comp = sb.comboBox_points_component.currentText()
        index = INDEX_DICT[comp]

        if self.particle_mapper_str == 'glyphs':
            self.glyph.SetInputArrayToProcess(1, 0, 0, 0, var)
        else:
            if self.particle_mapper is None:
                return
            self.particle_mapper.SelectColorArray(var)
        array = self.point_arrays.get(self.vtp_pattern, {}).get(var)
        if array is None:
            return

        mapper = self.mappers.get(self.points_str)
        mapper.SetScalarRange(safe_float(array.get('from', 0.0), 0.0),
                              safe_float(array.get('to', 1.0), 1.0))

        single_color = array.get('single_color', False)
        if single_color:
            mapper.ScalarVisibilityOff()
            color = array.get('color', QtCore.Qt.white)
            btn = sb.toolButton_points_color
            btn.setStyleSheet("QToolButton{{ background: {};}}".format(color.name()))
            btn.setIcon(QtGui.QIcon())
        else:
            self.change_color_map('points', array.get('color_map', 'viridis'), index)
            mapper.ScalarVisibilityOn()

        sb.comboBox_points_component.setEnabled(array['components'] == 3)
        self.change_colorbar_mapper()

        self.render()
        self.set_unsaved_flag()

    def change_color_by(self, geo):
        if geo == 'points':
            self.change_point_color()
        else:
            self.change_cell_color()

    def change_color_map(self, geo, colormap, component=None):
        """change the color map"""
        mapper = self.mappers.get(
            self.points_str if geo == 'points' else geo, None)
        if mapper is None:
            return
        lut = self.lookuptables.get(geo, None)

        if colormap is not None:
            new_lut = vtk.vtkLookupTable()
            new_lut.DeepCopy(LOOKUP_TABLES.get(colormap, 'viridis'))

            # Fix for bug introduced in VTK7, bug fixed in VTK8
            # https://gitlab.kitware.com/vtk/vtk/issues/16966
            if hasattr(new_lut, 'BuildSpecialColors'):
                new_lut.BuildSpecialColors()

            new_lut.Build()
            # check component in old lut
            if lut is not None and component is None and lut.GetVectorMode() != 0:
                component = lut.GetVectorComponent()
        else:
            new_lut = lut

        if component is not None and component >= 0:
            new_lut.SetVectorModeToComponent()
            new_lut.SetVectorComponent(component)
        else:
            new_lut.SetVectorModeToMagnitude()

        mapper.SetLookupTable(new_lut)
        self.lookuptables[geo] = new_lut

        if colormap is not None:
            btn = self.visual_btns['cells' if geo == 'nodes' else geo]['color']
            btn.setIcon(build_qicons().get(colormap).get('icon', QtGui.QIcon()))
            btn.setStyleSheet("QToolButton{{ background: {};}}".format(None))

    def handle_change_color(self, geo, button, popup=True):
        """popup the color bar dialog"""

        sb = self.sidebar

        # get the array name. Note: cells and nodes share the same widget
        array_name = self.visual_btns[geo]['color_by'].currentText()

        # check for nodes
        geo = self.cells_str if geo == 'cells' else geo

        if len(array_name) == 0:
            return
        if geo == 'points':
            array = self.point_arrays.get(self.vtp_pattern, {}).get(array_name)
            comp = sb.comboBox_points_component.currentText()
        elif geo == 'cells':
            array = self.cell_arrays.get(self.vtu_pattern, {}).get(array_name)
            comp = sb.comboBox_cells_component.currentText()
        elif geo == 'nodes':
            array = self.node_arrays.get(self.vtu_pattern, {}).get(array_name)
            comp = sb.comboBox_cells_component.currentText()
        else:
            return

        if array is None:
            return
        comp = INDEX_DICT.get(comp)
        self.color_dialog.set_(array, comp)
        self.color_dialog.geo = geo
        self.color_dialog.button = button
        if popup:
            self.color_dialog.popup('Points' if geo == 'points' else 'Cells/Nodes')

    def change_color(self, geo, button, array):
        """change the color or color map of an actor"""
        mapper = self.mappers.get(self.points_str if geo == 'points' else geo)
        actor = self.actors.get(self.points_str if geo == 'points' else geo)

        params = self.color_dialog.get()

        array.update(params)

        color = params.get('color', QtCore.Qt.white)
        color_map = params.get('color_map', 'viridis')

        self.change_color_map(geo, color_map)

        single_color = params.get('single_color', False)

        if single_color:
            button.setStyleSheet(
                "QToolButton{{ background: {};}}".format(color.name()))
            actor.GetProperty().SetColor(color.getRgbF()[:3])
            button.setIcon(QtGui.QIcon())
            mapper.ScalarVisibilityOff()
        else:
            button.setStyleSheet("QToolButton{{ background: {};}}".format(None))
            mapper.ScalarVisibilityOn()

        mapper.SetScalarRange(
            safe_float(array.get('from', 0.0), 0.0),
            safe_float(array.get('to', 1.0), 1.0))
        map_text = self.visual_btns['color_bar']['mapper'].currentText().lower()
        if geo in map_text:
            label = self.visual_btns['points' if map_text=='points' else 'cells']['color_by'].currentText()
            self.set_colorbar(mapper=mapper, label=label)

        self.render()

    def change_geo_color(self, checked=False, color=None):
        """Change the color of the geometry actor"""
        if color is None:
            color = QtWidgets.QColorDialog.getColor(parent=self)
            if not color.isValid():
                return
        self.geometry_color = color

        self.sidebar.toolButton_geometry_color.setStyleSheet(
            "QToolButton{{ background: {};}}".format(color.name()))

        self.actors['geometry'].GetProperty().SetColor(color.getRgbF()[:3])
        self.render()
        self.set_unsaved_flag()

    def change_geo_style(self):
        style = self.sidebar.comboBox_regions_style.currentText()
        self.set_representation(self.actors.get('geometry', None), style)

        self.render()
        self.set_unsaved_flag()

    def change_color_bar_color(self, checked=False, color=None):
        """Change the color of the geometry actor"""
        if color is None:
            color = QtWidgets.QColorDialog.getColor(parent=self)
            if not color.isValid():
                return
        self.color_bar_color = color

        self.sidebar.toolButton_color_bar_color.setStyleSheet(
            "QToolButton{{ background: {};}}".format(color.name()))
        self.set_colorbar(color=color.getRgbF()[:3])
        self.set_unsaved_flag()

    def change_time_label_color(self, checked=False, color=None):
        """Change the color of the geometry actor"""
        if color is None:
            color = QtWidgets.QColorDialog.getColor(parent=self)
            if not color.isValid():
                return
        self.time_label_color = color

        self.sidebar.toolButton_time_label_color.setStyleSheet(
            "QToolButton{{ background: {};}}".format(color.name()))

        self.set_timelabel(color=color.getRgbF()[:3])
        self.render()
        self.set_unsaved_flag()

    def change_opacity(self, geo, opacity):
        """change the opactiy of an actor"""
        if isinstance(opacity, QtWidgets.QDoubleSpinBox):
            opacity = opacity.value()

        # point gaussian / glyphs
        geo = self.points_str if geo == 'points' else geo
        # check for nodes / cells
        geo = self.cells_str if geo == 'cells' else geo

        if geo in self.actors:
            actor = self.actors.get(geo, None)
            if actor is not None:
                actor.GetProperty().SetOpacity(opacity)
                self.render()
        self.set_unsaved_flag()

    def handle_particle_mapper_changed(self):
        '''
        Particle mapper changed, hide/show widgets
        '''
        sb = self.sidebar
        mapper = sb.comboBox_partilce_mapper.value

        glyph = mapper == 'glyphs'

        # hide/show
        # point gaussian
        sb.label_shader.setVisible(not glyph)
        sb.comboBox_particle_shader.setVisible(not glyph)
        # glyph
        sb.label_glyph.setVisible(glyph)
        sb.comboBox_glyph.setVisible(glyph)
        sb.label_max_points.setVisible(glyph)
        sb.lineEdit_maximum_glyph_points.setVisible(glyph)

        self.set_unsaved_flag()
        self.change_particle_options()

    def collect_particle_options(self):
        # collect widget values
        data = {}
        sb = self.sidebar
        data['mapper'] = sb.comboBox_partilce_mapper.value
        data['splat'] = sb.comboBox_particle_shader.value
        data['glyph'] = sb.comboBox_glyph.value
        data['max_points'] = safe_int(sb.lineEdit_maximum_glyph_points.value, 10000)
        return data

    def set_particle_options(self, data):
        # set widget values
        sb = self.sidebar
        sb.comboBox_partilce_mapper.updateValue(
            None, data.get('mapper', 'point gaussian'))
        sb.comboBox_particle_shader.updateValue(
            None, data.get('splat', 'sphere'))
        sb.comboBox_glyph.updateValue(None, data.get('glyph', 'sphere'))
        sb.lineEdit_maximum_glyph_points.updateValue(
            None, data.get('max_points', 10000))

    def change_particle_options(self):
        # collect particle options from widgets
        data = self.collect_particle_options()
        mpr = data.get('mapper', 'point gaussian')
        if not POINT_GAUSSIAN:
            mpr = data['mapper'] = 'glyphs'

        # save a copy
        self.particle_render_options = data

        if self.particle_mapper is None:
            return

        # change particle mapper
        if mpr != self.particle_mapper_str:
            self.particle_mapper_str = mpr

            if self.particle_mapper is None:
                return

            ga = self.actors.get('points_glyph')
            pa = self.actors.get('points_pg')

            # remove one actor, add the other
            if mpr == 'glyphs':
                if ga is not None:
                    self.vtkrenderer.AddActor(ga)
                if pa is not None:
                    self.vtkrenderer.RemoveActor(pa)
            else:
                if ga is not None:
                    self.vtkrenderer.RemoveActor(ga)
                if pa is not None:
                    self.vtkrenderer.AddActor(pa)

            self.change_point_color()

            # make sure opacity is correct
            self.change_opacity('points', self.sidebar.doubleSpinBox_points_opacity.value())

        if POINT_GAUSSIAN and self.particle_mapper_pg is not None:
            self.particle_mapper_pg.SetSplatShaderCode(
                SPLAT_SHADERS.get(data.get('splat', 'sphere'), 'sphere'))
        if self.particle_mapper_glyph is not None:
            self.glyph_mask.SetMaximumNumberOfPoints(
                data.get('max_points', DEFAULT_MAXIMUM_POINTS))
            self.set_glyph_source(data.get('glyph', 'sphere'))

        self.render()

    def set_glyph_source(self, name):
        gs = GLYPHS[name]()
        self.glyph.SetSourceConnection(gs.GetOutputPort())

    def set_colorbar(self, mapper=None, label=None, position=None, color=None,
                     shadow=None, italic=None, n_labels=None, label_fmt=None):
        BaseVtkWidget.set_colorbar(self, mapper, label, position, color, shadow,
                                   italic, n_labels, label_fmt)
        self.enable_toolbar_geo(
            'color_bar', visible=self.sidebar.toolButton_color_bar.isChecked())
        self.actors['color_bar'] = self.scalar_bar

        self.render()
        self.set_unsaved_flag()

    def change_color_bar_n_labels(self):
        n_labels = self.sidebar.spinBox_color_bar_n_labels.value()
        self.set_colorbar(n_labels=n_labels)

    def change_color_bar_format(self):
        le = self.sidebar.lineEdit_color_bar_format
        fmt = le.text()

        try:
            fmt % 3.14
            color = 'black'
            self.set_colorbar(label_fmt=fmt)
        except:
            color = 'red'
        le.setStyleSheet("color: " + color)

    def change_color_bar_loc(self):
        pos = self.sidebar.comboBox_color_bar_position.currentText()
        self.set_colorbar(position=pos)

    def change_colorbar_mapper(self):
        map_text = self.sidebar.comboBox_color_bar_field.currentText()
        sb = self.sidebar
        label = ''
        if map_text == 'points':
            mapper = self.particle_mapper
            label = sb.comboBox_points_variable.currentText()
        else:
            if sb.comboBox_cells_style.currentText() == 'nodes':
                mapper = self.ugrid_node_mapper
            else:
                mapper = self.ugrid_cell_mapper
            label = sb.comboBox_cells_variable.currentText()

        self.set_colorbar(mapper=mapper, label=label)

    def handle_label_format(self, text):

        try:
            text.format(1.34)
            self.time_format = text
            self.set_timelabel(text=self.time_format.format(self.time))
            self.render()
            color = 'black'
        except:
            color = 'red'
        self.sidebar.lineEdit_time_label_format.setStyleSheet("color: " + color)
        self.set_unsaved_flag()

    def change_time_label_loc(self):
        cb = self.sidebar.comboBox_time_label_loc.currentText()
        self.set_timelabel(pos=cb)

        self.render()
        self.set_unsaved_flag()

    def change_time_label_text_size(self):
        size = self.sidebar.spinBox_time_label_size.value()
        self.set_timelabel(fontsize=size)

        self.render()
        self.set_unsaved_flag()
