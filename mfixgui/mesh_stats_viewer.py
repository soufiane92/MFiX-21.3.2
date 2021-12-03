"""
Mixin to gui for showing mesh stats.
"""
import os
import copy

from qtpy.QtWidgets import (QHBoxLayout, QListWidget, QWidget, QSplitter)
from qtpy.QtCore import QFileSystemWatcher, QTimer, Qt

# graphics libraries
from mfixgui.tools.pyqtgraph import (pg, PYQTGRAPH_AVAILABLE, PlotWidget,
    DEFAULT_PENS, DEFAULT_BRUSHS)

from mfixgui.tools.read_mesh_stats import read_mesh_stats

class MeshStatsViewer(object):

    def init_mesh_stats_viewer(self):

        self.mesh_stats_data = {}

        self.mesh_stats_widget = QWidget()
        self.mesh_stats_widget.monitors = False
        self.mesh_stats_widget.name = 'mesh stats'

        layout = QHBoxLayout(self.mesh_stats_widget)
        layout.setContentsMargins(5, 5, 5, 5)

        splitter = QSplitter()
        layout.addWidget(splitter)

        self.mesh_stats_list = QListWidget()
        self.mesh_stats_list.currentTextChanged.connect(self.change_mesh_stats)
        self.mesh_stats_plot = PlotWidget()
        splitter.addWidget(self.mesh_stats_list)
        splitter.addWidget(self.mesh_stats_plot)

        # file system watcher
        self.mesh_stats_dir_watcher = QFileSystemWatcher()
        self.mesh_stats_dir_watcher.directoryChanged.connect(self.mesh_stats_dir_changed)
        self.mesh_stats_file_watcher = QFileSystemWatcher()
        self.mesh_stats_file_watcher.fileChanged.connect(self.mesh_stats_file_changed)

        self.timer = QTimer()
        self.watching_mesh_stats_file = False

    def get_mesh_stats_name(self):
        """Return the current project MESH_STATS.LOG path"""
        prj_dir = self.get_project_dir()
        if prj_dir is None:
            return None
        fname = os.path.join(prj_dir, "MESH_STATS.LOG")
        return fname

    def look_for_mesh_stats(self):

        b_name = self.get_mesh_stats_name()
        # if b_name is None:
        #     return
        if os.path.exists(b_name):
            self.mesh_stats_file_watcher.addPath(b_name)
            self.read_mesh_stats_file(b_name)
        else:
            self.mesh_stats_dir_watcher.addPath(os.path.dirname(b_name))

    def show_mesh_stats(self, show=False):

        tab_w = self.ui.tabWidgetGraphics
        index = tab_w.indexOf(self.mesh_stats_widget)

        # check if the widget has a tab
        if index == -1:
            index = tab_w.count()-1
            tab_w.insertTab(index, self.mesh_stats_widget, "Mesh Stats")

        if show:
            tab_w.setCurrentIndex(index)

    def mesh_stats_dir_changed(self, path):
        ms_name = self.get_mesh_stats_name()
        if ms_name is None:
            return
        if os.path.exists(ms_name):
            self.mesh_stats_file_watcher.addPath(ms_name)
            self.mesh_stats_dir_watcher.removePath(path)
            self.mesh_stats_file_changed(ms_name)

    def mesh_stats_file_changed(self, path):
        if not self.watching_mesh_stats_file:
            self.watching_mesh_stats_file = True
            self.timer.singleShot(1000, lambda:self.read_mesh_stats_file(path))

    def read_mesh_stats_file(self, path):
        self.watching_mesh_stats_file = False
        if not os.path.exists(path):
            # add the project dir back to the file watcher
            self.mesh_stats_dir_watcher.addPath(os.path.dirname(path))
            return

        self.show_mesh_stats()

        self.print_internal(f"Reading mesh stats: {path}", color='blue')
        self.mesh_stats_data = read_mesh_stats(path)

        cur_item = self.mesh_stats_list.currentItem()
        if cur_item is not None:
            cur_text = cur_item.text()
        else:
            cur_text = "Aspect Ratio (SCALAR CUT CELLS)"

        self.mesh_stats_list.clear()
        self.mesh_stats_list.addItems(self.mesh_stats_data.keys())
        items = self.mesh_stats_list.findItems(cur_text, Qt.MatchExactly)
        if len(items) > 0:
            self.mesh_stats_list.setCurrentItem(items[0])
            self.change_mesh_stats(cur_text)

    def change_mesh_stats(self, name):

        data = self.mesh_stats_data.get(name, None)
        if data is None:
            return

        fr = data.get('from')
        to = data.get('to')
        min_ = min(fr)
        max_ = max(to)
        n_points = len(data.get('from'))

        # Check for single value
        if n_points == 1:
            fr = copy.deepcopy(fr)
            to = copy.deepcopy(to)
            fr[0] = fr[0] - 0.01
            to[0] = to[0] + 0.01
            min_ = fr[0] - 4*0.02
            max_ = to[0] + 4*0.02

        pw = self.mesh_stats_plot
        pw.clear()
        y0 = [0]*n_points
        bg = pg.BarGraphItem(
            x0=fr,
            x1=to,
            height=data.get('n'),
            pen=pg.mkPen(color=[255,255,255], width=2),
            brush=DEFAULT_BRUSHS[0])

        pw.addItem(bg)
        pw.setXRange(min_, max_)
        pw.setLabel('bottom', name)
        pw.setLabel('left', 'Cells')

    def reset_mesh_stats_viewer(self):

        self.mesh_stats_list.clear()
        self.mesh_stats_plot.clear()
