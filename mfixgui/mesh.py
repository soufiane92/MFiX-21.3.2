# -*- coding: utf-8 -*-
import sys
import os
import glob
import tempfile

from collections import OrderedDict
from itertools import groupby
from functools import reduce

from qtpy import QtCore, QtWidgets
from qtpy.QtCore import QFileSystemWatcher

from mfixgui.tools import sort_dict, safe_float, safe_int, format_key_with_args
from mfixgui.tools.qt import widget_iter
from mfixgui.widgets.base import BaseWidget

# These are needed for running the Mesher in SMS mode
from mfixgui.solver.manager import Solver, MFIXSOLVER_NAMES

MESH_EXTENT_KEYS = ['x_min', 'x_max', 'y_min', 'y_max', 'z_min', 'z_max']
MESH_CELL_KEYS = ['imax', 'jmax', 'kmax']
TABLE_MFIXKEY_MAP = {'position': 'cp', 'cells': 'nc', 'stretch': 'er',
                     'first': 'first_d', 'last': 'last_d'}
CELL_MFIX_KEYS = ['imax', 'jmax', 'kmax']


def ctrl_pts_to_loc(ctrl, min_loc):
    """given control points, convert to location"""
    loc = [min_loc]
    last = min_loc
    for pt in ctrl.values():
        er = safe_float(pt['stretch'], 1.0)
        cp = safe_float(pt['position'], 0.0)
        nc = safe_int(pt['cells'], 1)

        # uniform dx
        dx = (cp-last)/nc

        # first dx?
        fdx = safe_float(pt['first'], 0.0)
        if fdx < 0 and len(loc) > 2:
            fdx = loc[-1] - loc[-2]

        # expansion ratio
        ratio = 1
        prev_cell_w = dx
        if nc > 1 and er != 1:
            ratio = er**(1/(nc-1))
            prev_cell_w = (cp-loc[-1])*(1-ratio)/(1-ratio**nc)  # current cell width
            prev_cell_w /= ratio  # backup one cell for the loop below

        # add cell positions to list
        for i in range(nc):
            cell_w = dx
            if er != 1:
                cell_w = prev_cell_w*ratio
            loc.append(loc[-1] + cell_w)
            prev_cell_w = cell_w
        last = loc[-1]

    return loc


def linspace(f, t, c):
    """copy of numpy's linspace"""
    if int(c) <= 1:
        return [f, t]
    dx = (t-f)/float(c)
    l = [f]
    for i in range(c):
        l.append(l[-1]+dx)
    l[-1] = t # make sure the last value is the one given
    return l


def build_context_menu(actions):
    """given a list of (text, method), generate a menu"""
    menu = QtWidgets.QMenu()
    for s, method in actions:
        action = QtWidgets.QAction(s, menu)
        action.triggered.connect(method)
        menu.addAction(action)
    return menu


class KeyHandler(QtCore.QObject):
    value_updated = QtCore.Signal(object, object, object)
    update_mesh_extents = QtCore.Signal(object, object, object)
    update_mesh_cells = QtCore.Signal(object, object, object)
    def updateValue(self, key, val, args):
        if key in MESH_EXTENT_KEYS:
            self.update_mesh_extents.emit(key, val, args)
        elif key in MESH_CELL_KEYS + ['no_k']:
            self.update_mesh_cells.emit(key, val, args)


class Mesh(object):
    # Mesh Task Pane Window: This section allows a user to define the mesh
    # Methods that need vtk are in the vtkwidget
    def init_mesh(self):
        self.mesh_extents = []
        self.mesh_cells = []
        self.mesh_spacing = [[],[],[]] # x, y, z
        ui = self.ui.mesh
        self.cell_spacing_widgets = [ui.lineedit_mesh_cells_size_x,
                                     ui.lineedit_mesh_cells_size_y,
                                     ui.lineedit_mesh_cells_size_z]
        self.mesh_tables = [ui.table_mesh_control_points_x,
                            ui.table_mesh_control_points_y,
                            ui.table_mesh_control_points_z]
        self.cell_count_widgets = [ui.lineedit_keyword_imax,
                                   ui.lineedit_keyword_jmax,
                                   ui.lineedit_keyword_kmax]
        self.mesh_delete_btns = [ui.toolbutton_mesh_remove_x,
                                 ui.toolbutton_mesh_remove_y,
                                 ui.toolbutton_mesh_remove_z]
        self.mesh_add_btns = [ui.toolbutton_mesh_add_x,
                              ui.toolbutton_mesh_add_y,
                              ui.toolbutton_mesh_add_z]

        # connect delete btns
        for (i, btn) in enumerate(self.mesh_delete_btns):
            btn.clicked.connect(lambda ignore, i=i: self.mesh_delete(i))

        # connect add btns
        for (i, btn) in enumerate(self.mesh_add_btns):
            btn.clicked.connect(lambda ignore, i=i: self.mesh_add(i))

        # PPO/SMS
        self.mesh_dir_watcher = QFileSystemWatcher()
        self.mesh_dir_watcher.directoryChanged.connect(self.mesh_dir_changed)
        ui.pushbutton_delete_mesh.clicked.connect(self.delete_mesh) # TODO disable button if nothing to delete (need file watcher)
        ui.pushbutton_generate_mesh.clicked.connect(self.generate_mesh)
        ui.pushbutton_accept_mesh.clicked.connect(self.accept_mesh)
        ui.pushbutton_reset_tol.clicked.connect(self.reset_tol)
        ui.pushbutton_reset_tol.setToolTip("""Resets keywords <b>tol_small_cell</b>, <b>tol_small_area</b>,
<b>tol_merge</b>, <b>tol_snap</b>, <b>tol_stl</b>, <b>tol_stl_dp</b>, <b>tol_delh</b>,
and <b>stl_small_angle</b> to their default values.""")
        # key handler
        self.mesh_key_handler = KeyHandler()
        self.project.register_widget(self.mesh_key_handler, MESH_EXTENT_KEYS + MESH_CELL_KEYS + ['no_k'], [])
        self.mesh_key_handler.update_mesh_extents.connect(self.update_background_mesh_extents)
        self.mesh_key_handler.update_mesh_cells.connect(self.update_background_mesh_cells)

        # connect mesh tab btns
        for (i, btn) in enumerate([ui.pushbutton_mesh_uniform, ui.pushbutton_mesh_mesher]):
            btn.clicked.connect(lambda ignore, i=i:self.change_mesh_tab(i))

        ui.combobox_mesher.currentIndexChanged.connect(
            self.change_mesher_options)

        column_delegate = {0: {'widget': 'lineedit',
                               'dtype':  'dp'},
                           1: {'widget': 'lineedit',
                               'dtype':  'i'},
                           2: {'widget': 'lineedit',
                               'dtype':  'dp'},
                           3: {'widget': 'lineedit',
                               'dtype':  'dp'},
                           4: {'widget': 'lineedit',
                               'dtype':  'dp'},
                           }

        # setup tables
        for (i, (d, table)) in enumerate(zip(['x', 'y', 'z'], self.mesh_tables)):
            table.keys = [k+d for k in TABLE_MFIXKEY_MAP.values()] # Locatability
            table.dtype = OrderedDict
            table._setModel() # FIXME: Should be in __init__
            table.set_selection_model()
            table.set_value(OrderedDict())
            table.set_columns(['position', 'cells', 'stretch', 'first', 'last'])
            table.show_vertical_header(True)
            table.auto_update_rows(True)
            table.new_selection.connect(lambda f, t, i=i, d=d, table=table:
                                        self.mesh_new_selection(f, t, i, d, table))
            table.default_value = OrderedDict()
            table.value_changed.connect(lambda row, col, val, d=d, t=table:
                                        self.mesh_table_changed(row, col, val, d, t))
            #table.fit_to_contents()
            # TODO squash table height?  see 'fixup_table'
            table.menu = build_context_menu([
                ('split', lambda ignore, t=table, d=d: self.mesh_split(t, d))])
            table.auto_update_rows(True)
            table.set_delegate(col=column_delegate, row=None)

        # update total cell count
        self.cell_counts = {}
        for le in self.cell_count_widgets:
            le.value_updated.connect(self.calc_total_cells)

        # SMS/PPO mode:  invalidate mesh if anything changes
        for w in widget_iter(ui):
            if isinstance(w, BaseWidget):
                w.post_update = self.clear_mesh_accepted # TODO implement chained post_updates


    def calc_total_cells(self, wid, dict, args):
        self.cell_counts.update(dict)
        vals = [safe_int(v, 1) for v in self.cell_counts.values()]
        tot = reduce(lambda x, y: x*y, vals)
        self.ui.mesh.lineEdit_total_background_cells.setText(str(tot))

    def setup_mesh(self, allow_disabled_tab=False):
        ### Set widgets to reflect keyword settings.
        # All of these are handled automatically by the project_manager
        # Issues 656, 977
        #ui = self.ui.mesh
        #cb = ui.checkbox_flip_stl_normals
        pass


    def mesh_dir_changed(self, d):
        ui = self.ui.mesh
        run_name = self.project.get_value('run_name')
        if not run_name:
            return
        mesh_file = run_name.upper() + '.msh'
        mesh_exists = os.path.exists(mesh_file)
        for b in (ui.pushbutton_accept_mesh,
                  ui.pushbutton_delete_mesh):
            b.setEnabled(mesh_exists)

    def accept_mesh(self):
        self.set_mesh_accepted()

    def delete_mesh(self):
        run_name = self.project.get_value('run_name')
        if run_name:
            RUN_NAME = run_name.upper()
            files = [f for f in (RUN_NAME+'_boundary.vtk',
                                 RUN_NAME+'_MESH.vtu',
                                 RUN_NAME+'.msh')
                     if os.path.exists(f)]
            if files:
                if self.remove_output_files(output_files=files):
                    self.clear_mesh_accepted()
            else:
                self.print_internal("Nothing to delete")
                self.clear_mesh_accepted()

    def generate_mesh(self):
        self.update_keyword('ppo', True)
        if self.unsaved_flag:
            self.save_project()
        solver = None
        exec_dir = os.path.dirname(sys.executable)
        search_dirs = (exec_dir,
                       os.path.join(exec_dir, 'Scripts'),
                       self.get_project_dir())
        for d in search_dirs:
            for name in MFIXSOLVER_NAMES:
                if os.path.exists(os.path.join(d, name)):
                    solver = os.path.join(d, name)
                    break
            if solver:
                break
        if solver is None:
            self.error("No MFiX solver found.\nSearched:\n%s"
                       % '\n'.join(search_dirs), popup=True)
            return

        run_name = self.project.get_value('run_name')
        mesh_file = run_name.upper() + '.msh'
        if os.path.exists(mesh_file):
            if not self.confirm_clobber(mesh_file):
                return
            # Invalidate mesh, in case run fails (?)
            os.unlink(mesh_file)

        self.remove_mesh_tempfiles()
        solver = Solver(solver, None)
        # Issues/1084 create temporary project file omitting user keys
        # Would be nice to do this in /tmp but the solver does a 'chdir'
        tmp = tempfile.NamedTemporaryFile(dir=os.getcwd(), suffix='.mfx.tmp',
                                          prefix='mesh_', encoding='utf-8',
                                          mode='w+', delete=False)
        for kw in self.project.keywordItems():
            key = kw.key
            if (key in self.project.usr_keyword_doc or
                key not in self.keyword_doc):
                # It's a user keyword or an undocumented key, skip it
                continue
            # Issues/1152 run mesher serial
            if key in ('nodesi', 'nodesj', 'nodesk'):
                continue
            line = kw.line()
            line = line.split('#!MFIX-GUI')[0].strip() # Simplify, remove expressions
            tmp.write(line+'\n')

        self.process_manager.start_solver(solver, None, 1, (1,1,1), project_file=tmp.name)
        #self.handle_run() # pops up run_popup which we don't want here

    def remove_mesh_tempfiles(self):
        for f in glob.glob('mesh_*.mfx.tmp'):
            try:
                os.unlink(f)
            except Exception as e:
                self.error(str(e), popup=False)

    def reset_tol(self):
        for key in ('tol_small_cell', 'tol_small_area',
                    'tol_merge', 'tol_stl', 'tol_delh',
                    'tol_stl_dp', 'stl_small_angle'):
            default = self.keyword_doc.get(key,{}).get('initpython')
            self.update_keyword(key, default)
        key = 'tol_snap'
        default = self.keyword_doc.get(key,{}).get('initpython')
        for i in range(1,4):
            self.update_keyword(key, default, args=[i])
        self.setup_mesh()
        self.clear_mesh_accepted()

    def change_mesh_tab(self, tabnum):
        """switch mesh stacked widget based on selected"""
        ui = self.ui.mesh
        to_btn = [ui.pushbutton_mesh_uniform, ui.pushbutton_mesh_mesher][tabnum]
        self.animate_stacked_widget(
            ui.stackedwidget_mesh,
            ui.stackedwidget_mesh.currentIndex(),
            tabnum,
            line=ui.line_mesh,
            to_btn=to_btn,
            btn_layout=ui.gridlayout_mesh_tab_btns)

        for btn in [ui.pushbutton_mesh_uniform, ui.pushbutton_mesh_mesher]:
            btn.setChecked(btn == to_btn)
            font = btn.font()
            font.setBold(btn == to_btn)
            btn.setFont(font)

    def change_mesher_options(self):
        """switch the mesh options stacked widget"""
        ui = self.ui.mesh
        mesher = str(ui.combobox_mesher.currentText()).lower()

        current_index = 0
        for i in range(ui.stackedwidget_mesher_options.count()):
            widget = ui.stackedwidget_mesher_options.widget(i)
            if mesher == str(widget.objectName()).lower():
                current_index = i
                break

        self.animate_stacked_widget(
            ui.stackedwidget_mesher_options,
            ui.stackedwidget_mesher_options.currentIndex(),
            current_index)

        enable = mesher != 'cutcell'
        ui.pushbutton_generate_mesh.setEnabled(enable)
        ui.pushbutton_remove_mesh.setEnabled(enable)

    def mesh_new_selection(self, from_, to, index, dir_, table):
        """handle a new selection"""
        self.mesh_delete_btns[index].setEnabled(len(to)>0)

    def update_mesh_keyword(self, key, value, args=None):
        """check the keywords and correct the index"""
        # erx, ery, erz, ncx, ncy, ncz, first_dx, first_dy, first_dz, last_dx, last_dy, last_dz all start at 1
        if args is not None and key[:-1] != 'cp':
            args += 1
        self.clear_mesh_accepted()
        self.update_keyword(key, value, args)

    def init_background_mesh(self):
        """init the background mesh"""
        project = self.project

        self.mesh_extents = [safe_float(project.get_value(key, default=0.0))
                             for key in MESH_EXTENT_KEYS]
        self.mesh_cells = [safe_int(project.get_value(key, default=1))
                           for key in MESH_CELL_KEYS]
        self.cell_counts = dict((k,v) for k, v in zip(MESH_CELL_KEYS, self.mesh_cells))
        self.calc_total_cells(self, {}, ())
        self.mesh_spacing = [[], [], []]

        # disable delete btns
        for btn in self.mesh_delete_btns:
            btn.setEnabled(False)

        # collect dx, dy, dz
        for (i, (s, c)) in enumerate(zip(['dx', 'dy', 'dz'], MESH_CELL_KEYS)):
            d = [project.get_value(s, args=args) for args in sorted(project.get_key_indices(s))]
            l = len(d)

            # if there are spacing, update the keywords.
            if l > 0:
                self.mesh_cells[i] = l
                self.update_mesh_keyword(c, l)
                self.mesh_spacing[i] = d
                self.extract_mesh_spacing(i, d)

        # collect variable grid spacing keywords
        for (i, k) in enumerate(['x', 'y', 'z']):
            indices = project.get_key_indices('cp' + k)
            if indices:
                table_dic = OrderedDict()
                for j, ind in enumerate(sorted(indices)):
                    ind = ind[0]
                    table_dic[j] = {
                        'position': project.get_value('cp' + k, 0, args=ind),
                        'cells': project.get_value('nc' + k, 1, args=ind+1),
                        'stretch': project.get_value('er' + k, 1, args=ind+1),
                        'first': project.get_value('first_d' + k, 0, args=ind+1),
                        'last': project.get_value('last_d' + k, 0, args=ind+1)}
                self.mesh_tables[i].set_value(table_dic)
                self.mesh_tables[i].fit_to_contents()

        self.update_background_mesh()

    def extract_mesh_spacing(self, index, spacing):
        """given a list of cell spacing, convert to control points,
        also update [xyz]_max"""

        start = 0
        table_dic = OrderedDict()
        d = ['x', 'y', 'z'][index]

        for (i, (val, count)) in enumerate((k, sum(1 for _i in g))
                                         for (k,g) in groupby(spacing)):
            loc = count*val + start
            start = loc
            self.update_mesh_keyword('cp' + d, loc, args=i)
            self.update_mesh_keyword('nc' + d, count, args=i)
            table_dic[i] = {'position': loc, 'cells': count, 'stretch': 1.0,
                            'first': 0.0, 'last': 0.0}
        #Use location of final control point for [xyz]_max
        self.mesh_extents[index*2+1] = loc
        self.update_mesh_keyword(MESH_EXTENT_KEYS[1::2][index], loc)

        #Unset d[xyz] keys
        for i in range(len(spacing)):
            self.unset_keyword('d' + d, args=i)

        self.mesh_tables[index].set_value(table_dic)
        self.mesh_tables[index].fit_to_contents()

    def mesh_table_changed(self, row, col, val, d, table):
        """a value in the table was edited, update"""
        data = table.value
        if col == 'position':
            sort = sort_dict(data, 'position')
            table.set_value(sort, block=False) # unblock because the table is currently in an "edit" state
            table.fit_to_contents()
            for i, val in sort.items():
                self.mesh_update_mfixkeys(val, i, d)
        else:
            mfix_key = TABLE_MFIXKEY_MAP[col] + d

            self.update_mesh_keyword(mfix_key, val, args=row)

        self.update_background_mesh()

    def mesh_delete(self, index):
        """delete the selected control point"""
        table = self.mesh_tables[index]
        data = table.value
        rows = table.current_rows()
        if not rows: return
        max_i = max(data.keys())
        min_row = min(rows)
        d = ['x', 'y', 'z'][index]

        # remove rows
        for row in rows:
            data.pop(row)

        # rebuild dict
        # TODO: better way?
        new = OrderedDict()
        for (i, ctrl) in enumerate(data.values()):
            new[i] = ctrl
            if i >= min_row:
                self.mesh_update_mfixkeys(ctrl, i, d)
        table.set_value(new)
        table.fit_to_contents()
        self.update_background_mesh()

        nrows = len(new)
        if rows[-1] == nrows: # We deleted the last row,
            if nrows > 0:
                table.selectRow(nrows-1)

        # remove trailing keywords
        k = new.keys()
        if k:
            m = max(k)
        else:
            m = 0
        for i in range(m, max_i):
            ind = i + 1
            for key in TABLE_MFIXKEY_MAP.values():
                self.update_mesh_keyword(key+d, None, args=ind)

        # the last control point
        if m == max_i == 0:
            for key in TABLE_MFIXKEY_MAP.values():
                self.update_mesh_keyword(key+d, None, args=0)

    def mesh_add(self, index):
        """add a control point to the end"""
        table = self.mesh_tables[index]
        data = table.value
        d = ['x', 'y', 'z'][index]
        k = data.keys()
        i = 0
        if k:
            i = max(k) + 1
            loc = safe_float(list(data.values())[-1]['position']) + 1
            c = 1
        else:
            loc = self.project.get_value(d + '_max', 1)
            c = self.project.get_value(CELL_MFIX_KEYS[index], 1)
        ctrl = data[i] = {'position': loc, 'cells': c, 'stretch': 1.0, 'first': 0.0, 'last': 0.0}

        self.mesh_update_mfixkeys(ctrl, i, d)
        table.set_value(data)
        table.fit_to_contents()
        self.update_background_mesh()

    def mesh_split(self, table, d):
        """split the selected control point"""
        row, col = table.get_clicked_cell()
        data = table.value
        split_data = data[row]
        prev_data_loc = self.project.get_value(d + '_min', 0)
        if row >= 2:
            prev_data_loc = safe_float(data[row-1]['position'])

        # find the midpoint, and slit the cells evenly
        midpoint = (safe_float(split_data['position']) - prev_data_loc)/2.0 + prev_data_loc
        cells = max(int(safe_int(split_data['cells'], 1)/2), 1)
        split_data['cells'] = cells

        # insert the cell
        # TODO: better way?
        new = OrderedDict()
        for i, ctrl in data.items():
            if i < row:
                new[i] = ctrl
            elif i == row:
                new[i] = {'position': midpoint, 'cells': cells, 'stretch': 1.0,
                          'first': 0.0, 'last': 0.0}
                self.mesh_update_mfixkeys(new[i], i, d)
                new[i+1] = ctrl
                self.mesh_update_mfixkeys(ctrl, i+1, d)
            else:
                new[i+1] = ctrl
                self.mesh_update_mfixkeys(ctrl, i+1, d)

        table.set_value(new)
        table.fit_to_contents()
        self.update_background_mesh()

    def mesh_update_mfixkeys(self, ctrl, index, dir_):
        for key, value in ctrl.items():
            mfix_key = TABLE_MFIXKEY_MAP[key] + dir_
            self.update_mesh_keyword(mfix_key, value, args=index)

    def update_background_mesh_cells(self, key, val, args):
        """collect cells changes, check if value is different"""
        if not self.mesh_cells: # Nothing to do
            return
        if key == 'no_k':
            self.update_background_mesh()
        else:
            val = safe_int(val, 1)
            ind = MESH_CELL_KEYS.index(key)
            old_val = self.mesh_cells[ind]
            if old_val != val:
                self.mesh_cells[ind] = val
                self.update_background_mesh()

    def update_background_mesh_extents(self, key, val, args):
        """collect extents changes, check if value is different"""
        if not self.mesh_extents: return
        val = safe_float(val, 0)
        ind = MESH_EXTENT_KEYS.index(key)
        old_val = self.mesh_extents[ind]
        if old_val != val:
            self.mesh_extents[ind] = val
            self.update_background_mesh()

    def update_background_mesh(self):
        """update the background mesh"""
        extents = self.mesh_extents
        cells = self.mesh_cells
        project = self.project

        # average cell width
        for (f, t), c, wid in zip(zip(extents[::2], extents[1::2]), cells, self.cell_spacing_widgets):
            if c > 0:
                w = (t-f)/c
            else:
                w = t-f
            wid.setText('{0:.2e}'.format(w))

        # determine cell spacing
        spacing = []
        for (i, table) in enumerate(self.mesh_tables):
            axis='xyz'[i]
            val = table.value
            if val:
                s = ctrl_pts_to_loc(val, project.get_value('%s_min'%axis, 0))
                spacing.append(s)
                # disable imax, jmax, kmax
                self.cell_count_widgets[i].setEnabled(False)
                # update imax, jmax, kmax
                self.update_mesh_keyword(CELL_MFIX_KEYS[i], len(s)-1) # TODO: fix redundant update calls
            else:
                spacing.append(linspace(extents[i*2], extents[i*2+1], cells[i]))
                # enable imax, jmax, kmax
                if i == 2:
                    self.cell_count_widgets[i].setEnabled(not project.get_value('no_k', False))
                else:
                    self.cell_count_widgets[i].setEnabled(True)

        self.vtkwidget.update_background_mesh(spacing)

    def reset_mesh(self):
        # Reset directory watcher
        for d in self.mesh_dir_watcher.directories():
            self.mesh_dir_watcher.removePath(d)
        # Anything else?
