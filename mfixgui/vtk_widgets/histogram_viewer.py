"""
Widget to show histograms.
"""
import os
import copy
import glob
import numpy as np

import vtk
from vtk.util.numpy_support  import vtk_to_numpy

from qtpy.QtWidgets import (
    QHBoxLayout, QVBoxLayout, QGridLayout, QWidget, QToolButton, QLabel, QComboBox,
    QSizePolicy, QSpacerItem, QSpinBox, QAbstractSpinBox)
from qtpy.QtCore import QFileSystemWatcher, QTimer, Qt

from mfixgui.tools.qt import get_icon, sub_icon_size
from mfixgui.tools.util import safe_int
from mfixgui.vtk_widgets.tools import update_combo,  parse_pvd_file, safe_combo

# graphics libraries
from mfixgui.tools.pyqtgraph import (pg, PYQTGRAPH_AVAILABLE, PlotWidget,
    DEFAULT_PENS, DEFAULT_BRUSHS)


class HistogramViewer(QWidget):

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)

        self.name = 'histogram'

        self.ugrid_reader = None
        self.particle_reader = None
        self.pvd_files = {}
        self.frame_index = 0
        self.time = 0.0
        self.repeat = False
        self.data = None
        self.init = True

        layout = QVBoxLayout(self)
        layout.setContentsMargins(5, 5, 5, 5)

        # controls
        btn_bar = QWidget()
        self.button_bar_layout = QGridLayout(btn_bar)
        layout.addWidget(btn_bar)
        self.build_toolbar()

        # plot
        self.histogram_plot = PlotWidget()
        layout.addWidget(self.histogram_plot)

        # play timer
        self.play_timer = QTimer()
        self.play_timer.timeout.connect(self.forward)

        # look for vtu files
        self.file_timer = QTimer()
        self.file_timer.timeout.connect(self.look_for_files)
        self.file_timer.start(1000)

    def get_state(self):
        state = {
            'pvd':   self.combox_pvd.currentText(),
            'var':   self.combox_variable.currentText(),
            'comp':  self.combobox_component.currentText(),
            'bins':  self.spinbox_bins.value(),
            'style': self.combobox_style.currentText(),
        }

        return state

    def set_state(self, state):
        for combo, key, d in [
                (self.combox_pvd, 'pvd', ''),
                (self.combox_variable, 'var', ''),
                (self.combobox_component, 'comp', 'x'),
                (self.combobox_style, 'style', 'bar'),
                ]:
            safe_combo(combo, state.get(key, d))

        self.spinbox_bins.setValue(safe_int(state.get('bins', 100), 100))

    def build_toolbar(self):
        layout = self.button_bar_layout
        layout.setContentsMargins(5, 5, 5, 5)
        hspacer = QSpacerItem(99999, 10, QSizePolicy.Expanding, QSizePolicy.Maximum)

        # PVD select
        lbl = QLabel('File pattern')
        layout.addWidget(lbl, 0, 0)
        self.combox_pvd = QComboBox()
        self.combox_pvd.activated.connect(lambda:self.change_frame(self.frame_index, force=True))
        layout.addWidget(self.combox_pvd, 0, 1)

        lbl = QLabel('Variable')
        layout.addWidget(lbl, 0, 2)
        self.combox_variable = QComboBox()
        self.combox_variable.activated.connect(lambda:self.change_frame(self.frame_index, force=True))
        layout.addWidget(self.combox_variable, 0, 3)

        lbl = QLabel('Component')
        layout.addWidget(lbl, 0, 4)
        self.combobox_component = QComboBox()
        self.combobox_component.addItems(['x', 'y', 'z', 'mag'])
        self.combobox_component.activated.connect(self.change_histogram)
        layout.addWidget(self.combobox_component, 0, 5)

        self.button_bar_layout.addItem(hspacer, 0, 6)

        lbl = QLabel('Bins')
        layout.addWidget(lbl, 1, 0)
        self.spinbox_bins = QSpinBox()
        self.spinbox_bins.setRange(2, 9999)
        self.spinbox_bins.setValue(10)
        self.spinbox_bins.valueChanged.connect(self.change_histogram)
        layout.addWidget(self.spinbox_bins, 1, 1)

        lbl = QLabel('Style')
        layout.addWidget(lbl, 1, 2)
        self.combobox_style = QComboBox()
        self.combobox_style.addItems(['bar', 'line'])
        self.combobox_style.activated.connect(self.change_histogram)
        layout.addWidget(self.combobox_style, 1, 3)

        # --- play/stop/forward/backward controls ---
        for i, (icon, callback, tooltip) in enumerate([
                ('first.svg',     self.handle_first,     'First'),
                ('back.svg',      self.handle_back,      'Previous'),
                ('play.svg',      self.handle_play_stop, 'Play'),
                ('next.svg',      self.handle_next,      'Next'),
                ('last.svg',      self.handle_last,      'Last'),
                ('autorenew.svg', self.handle_repeat,    'Repeat from beginning'),
                ]):
            btn = QToolButton()
            if tooltip == 'Play':
                self.toolbutton_play = btn
            btn.clicked.connect(callback)
            btn.setIcon(get_icon(icon))
            btn.setIconSize(sub_icon_size())
            btn.setToolTip(tooltip)
            btn.setAutoRaise(True)
            btn.setFocusPolicy(Qt.ClickFocus)
            layout.addWidget(btn, 0, 7 + i)

        # last btn is the repeat, set setCheckable
        btn.setCheckable(True)

        self.button_bar_layout.addItem(hspacer, 0, 15)

        self.frame_spinbox = QSpinBox()
        self.frame_spinbox.editingFinished.connect(lambda: self.change_frame(self.frame_spinbox.value()))
        self.frame_spinbox.setMaximum(9999999)
        self.frame_spinbox.setButtonSymbols(QAbstractSpinBox.NoButtons)

        layout.addWidget(self.frame_spinbox, 0, 16)


    def handle_first(self):
        self.change_frame(0)

    def handle_back(self):
        self.change_frame(self.frame_index - 1)

    def handle_play_stop(self):
        if self.play_timer.isActive():
            self.stop()
        else:
            self.toolbutton_play.setIcon(get_icon('stop.svg'))
            delay_ms = max(0, 100)
            self.play_timer.start(delay_ms)

    def stop(self):
        self.toolbutton_play.setIcon(get_icon('play.svg'))
        self.play_timer.stop()

    def handle_next(self):
        self.change_frame(self.frame_index + 1)

    def handle_last(self):
        self.change_frame(-1)

    def handle_repeat(self, checked):
        self.repeat = checked

    def forward(self):
        self.change_frame(self.frame_index + 1)

    def change_frame(self, index, force=False):
        name = self.combox_pvd.currentText()

        f_dict = self.pvd_files.get(name, None)
        if f_dict is None:
            return

        fnames = f_dict.get('files', [])
        if not fnames:
            return

        n_max = len(fnames) - 1

        if self.play_timer.isActive() and self.repeat and index == n_max:
            index = 0
        elif index >= n_max:
            index = n_max-1
        elif index == -1:
            index = n_max
        elif index < 0:
            index = 0

        if index == self.frame_index and not force:
            return
        else:
            self.frame_index = index

        time = list(fnames.keys())[index]
        fname = fnames[time]

        if f_dict.get('type', '') == 'vtp':
            self.read_vtp(fname)
        else:
            self.read_vtu(fname)

        self.frame_spinbox.setValue(index)

    def change_histogram(self):
        data = self.data
        if data is None:
            return

        array = vtk_to_numpy(data)
        if len(array.shape) > 1:
            comp = self.combobox_component.currentText()
            if comp == 'mag':
                array = np.sqrt((array*array).sum(axis=1))
            else:
                array = array[:, ['x', 'y', 'z'].index(comp)]

        min_ = array.min()
        max_ = array.max()
        n_points = len(array)

        counts, edges = np.histogram(array, self.spinbox_bins.value())
        centers = (edges[1:]+edges[:-1])/2
        width = edges[1]-edges[0]

        style = self.combobox_style.currentText()

        pw = self.histogram_plot
        pw.clear()
        pw.showGrid(True, True, 0.5)
        if style == 'line':
            pw.plot(centers, counts, pen=DEFAULT_PENS[0])
        else:
            bg = pg.BarGraphItem(
                x=centers,
                width=width,
                height=counts,
                pen=pg.mkPen(color=[255,255,255], width=1),
                brush=DEFAULT_BRUSHS[0])
            pw.addItem(bg)

        # pw.setXRange(min_, max_)
        pw.setLabel('bottom', self.combox_variable.currentText())
        pw.setLabel('left', 'Count')

    @property
    def project_dir(self):
        return self.gui.get_project_dir()

    def look_for_mesh_file(self):
        prj = self.gui.project
        prj_dir = self.project_dir
        run_name = prj.get_value('run_name', None)
        vtu_dir = prj.get_value('vtu_dir', None)
        mesh_name = run_name.upper()+"_MESH.vtu"

        if run_name is None:
            return None

        if vtu_dir is not None:
            fname = os.path.join(prj_dir, vtu_dir.upper(), mesh_name)
        else:
            fname = os.path.join(prj_dir, mesh_name)

        if os.path.exists(fname):
            return {0: fname}
        return None

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
                    self.pvd_files[base_name] = {'files': files, 'type': t}

        # look for mesh file
        fdict = self.look_for_mesh_file()
        if fdict is not None:
            self.pvd_files['MESH'] = {'files': fdict, 'type': 'vtu'}

        # update the combo boxes
        update_combo(self.combox_pvd, self.pvd_files.keys())

        # trigger read/plot event on first file
        if self.init:
            self.change_frame(0, True)
            self.init = False

    def init_readers(self, parallel=False):
        '''setup the cell/point vtk stuff'''
        # cells
        if parallel:
            self.ugrid_reader = vtk.vtkXMLPUnstructuredGridReader()
            self.particle_reader = vtk.vtkXMLPPolyDataReader()
        else:
            self.ugrid_reader = vtk.vtkXMLUnstructuredGridReader()
            self.particle_reader = vtk.vtkXMLPolyDataReader()

    def read_vtp(self, vtp_file):
        if self.particle_reader is None:
            self.init_readers(parallel=vtp_file.endswith('.pvtp'))

        path = os.path.join(self.project_dir, vtp_file)
        if not os.path.exists(path):
            return False

        self.particle_reader.SetFileName(path)
        self.particle_reader.Update()

        data = self.particle_reader.GetOutput()
        point_data = data.GetPointData()
        array_info = {}
        n_tuples = None
        for i in range(point_data.GetNumberOfArrays()):
            array = point_data.GetArray(i)
            n_comp = array.GetNumberOfComponents()
            n_tuples = array.GetNumberOfTuples()
            array_info[point_data.GetArrayName(i)] = {
                'i': i,
                'number_of_tuples': n_tuples,
                'components': n_comp,
                'range': [array.GetRange(i) for i in range(n_comp)],
                'magnitude': array.GetRange(-1)}

        var = update_combo(self.combox_variable, array_info.keys())
        comp = array_info.get(var, {}).get('components', 1) > 1
        self.combobox_component.setEnabled(comp)
        points = point_data.GetAbstractArray(var)
        self.data = points
        self.change_histogram()

    def read_vtu(self, vtu_file):
        if self.ugrid_reader is None:
            self.init_readers(parallel=vtu_file.endswith('.pvtu'))

        path = os.path.join(self.project_dir, vtu_file)
        if not os.path.exists(path):
            return False

        self.ugrid_reader.SetFileName(path)
        self.ugrid_reader.Update()

        data = self.ugrid_reader.GetOutput()
        cell_data = data.GetCellData()
        array_info = {}
        for i in range(cell_data.GetNumberOfArrays()):
            array = cell_data.GetArray(i)
            n_comp = array.GetNumberOfComponents()
            array_info[cell_data.GetArrayName(i)] = {
                'i': i,
                'components': n_comp,
                'range': [array.GetRange(i) for i in range(n_comp)],
                'magnitude': array.GetRange(-1)}

        var = update_combo(self.combox_variable, array_info.keys())
        comp = array_info.get(var, {}).get('components', 1) > 1
        self.combobox_component.setEnabled(comp)
        cells = cell_data.GetAbstractArray(var)
        self.data = cells
        self.change_histogram()

    def reset(self):
        self.histogram_plot.clear()
        self.play_timer.stop()
        self.file_timer.stop()

    def close(self):
        # clean up timer
        self.play_timer.stop()
        self.file_timer.stop()

    def hideEvent(self, event):
        self.stop()
