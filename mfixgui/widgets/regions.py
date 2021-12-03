# -*- coding: utf-8 -*-
#!/usr/bin/env python

import os
import glob
import re
from qtpy import QtWidgets, QtCore
UserRole = QtCore.Qt.UserRole

from mfixgui.tools import get_unique_string
from mfixgui.tools.qt import (widget_iter, CellColor, get_ui,
                              get_pixmap, deepcopy_dict, get_icon,
                              sub_icon_size, sub_icon_height)

from mfixgui.project import Equation, ExtendedJSON, pretty_eq_rep
from mfixgui.constants import *

DEFAULT_REGION_DATA = {
    'equilibrant': False,
    'invert': False,
    'facet_count': 0,
    'filter': [0, 1, 0],
    'filter_facets': False,
    'to': [0, 0, 0],
    'deviation_angle': 10,
    'slice': False, # slice is deprecated in 20.2, replace with 'method' = 'Slice'
    'method': 'Full',
    'from': [0, 0, 0],
    'color': CellColor([1, 0, 0]),
    'stl_shape': 'all',
    'geometry_pool': [],
    'type': 'box',
    'visibility': True}

COLUMN_NAME, COLUMN_VISIBILITY, COLUMN_COLOR, COLUMN_TYPE, COLUMN_USED_BY = range(5)
def safe_int(obj, default=None):
    try:
        return int(obj)
    except:
        return default

def clean_region_dict(region_dict):
    """remove qt objects and values that are equal to the default"""
    clean_dict = {}
    for key, value in region_dict.items():
        if key in ['visible', 'used by', 'name']:
            pass
        elif key == 'color':
            clean_dict['color'] = region_dict['color'].color_hex
        elif isinstance(value, list) and any(isinstance(v, Equation) for v in value):
            clean_dict[key] = value
        elif value != DEFAULT_REGION_DATA.get(key, None):
            clean_dict[key] = value
    return clean_dict

# Need to match "min" and "max" in region inputs, but only when they are
# not part of a longer word ("xmin"/"xmax") or a function  call ("min(")
RE_MIN = re.compile(r'\bmin\b($|\s*[^\(])')
RE_MAX = re.compile(r'\bmax\b($|\s*[^\(])')

class RegionsWidget(QtWidgets.QWidget):

    def __init__(self, gui=None):
        QtWidgets.QWidget.__init__(self, gui)
        self.gui = gui
        self.parameter_key_map = {}

        get_ui('regions.ui', self)

        self.extent_lineedits = [self.lineedit_regions_from_x,
                                 self.lineedit_regions_to_x,
                                 self.lineedit_regions_from_y,
                                 self.lineedit_regions_to_y,
                                 self.lineedit_regions_from_z,
                                 self.lineedit_regions_to_z]
        keys = ['x_w', 'x_e', 'y_s', 'y_n', 'z_b', 'z_t']

        for (le, key) in zip(self.extent_lineedits, keys):
            le.keys = ['%s_%s'%(base, key) for base in ('bc', 'ic', 'is', 'ps', 'monitor', 'vtk', 'usr')] # Findability
            le.allow_parameters = True
            le.dtype = float
            le.help_text = 'Physical coordinates describing the bounds of the region.'
            le.setToolTip(le.help_text)

        self.lineedit_region_name.help_text = 'Name of the region. Used throughout the GUI to reference the region'
        self.lineedit_region_name.setToolTip(self.lineedit_region_name.help_text)
        self.lineedit_region_name.post_update = self.update_region_parameters
        self.combobox_stl_shape.help_text = 'Shape to be used to select facets.'
        self.combobox_stl_shape.setToolTip(self.combobox_stl_shape.help_text)
        self.combobox_stl_shape.currentTextChanged.connect(lambda v: self.comboBox_selection_method.setEnabled(v != 'all'))
        self.comboBox_selection_method.setEnabled(False)

        for wid in [self.lineedit_filter_x, self.lineedit_filter_y, self.lineedit_filter_z]:
            wid.allow_parameters = True
            wid.help_text = ('Vector to filter facets with. If the facet '
                            'normal is sufficiently close, then the facet'
                            ' will be added to the region, else discarded.')
            wid.setToolTip(wid.help_text)

        self.lineedit_deviation_angle.allow_parameters = True
        self.lineedit_deviation_angle.help_text = ('Angle to provide a '
            'tolerence to the filtering of facets based on the facet normal.')
        self.lineedit_deviation_angle.setToolTip(self.lineedit_deviation_angle.help_text)
        self.toolbutton_region_add.clicked.connect(self.new_region)
        self.toolbutton_region_delete.clicked.connect(self.delete_region)
        self.toolbutton_region_delete.setEnabled(False) #Need a selection
        self.toolbutton_region_copy.clicked.connect(self.copy_region)
        self.toolbutton_region_copy.setEnabled(False) #Need a selection
        self.toolbutton_row_up.clicked.connect(self.row_up)
        self.toolbutton_row_up.setEnabled(False) #Need a selection
        self.toolbutton_row_down.clicked.connect(self.row_down)
        self.toolbutton_row_down.setEnabled(False) #Need a selection

        self.toolbutton_color.clicked.connect(self.change_color)

        tw = self.tablewidget_regions
        tw.dtype = dict
        tw._setModel()
        tw.set_selection_model()
        tw.set_value({})
        tw.set_columns(['name', 'visible', 'color', 'type', 'used by'])
        tw.show_vertical_header(False)
        tw.auto_update_rows(True)
        tw.new_selection.connect(self.update_region_parameters)
        tw.clicked.connect(self.cell_clicked)
        tw.default_value = {}
        self.inhibit_toggle = True

        self.widget_region_parameters.setEnabled(False)
        for widget in widget_iter(self.widget_region_parameters):
            if hasattr(widget, 'value_updated'):
                widget.value_updated.connect(self.region_value_changed)

                # example <name>: lineedit_regions_to_x
                name = str(widget.objectName())

                # set extent limits
                if '_to_' in name or '_from_' in name:
                    kinfo = name.split('_')
                    widget.key = '_'.join(kinfo[-2:])
                    widget.dtype = float
                    widget.setValInfo(max=Equation(kinfo[-1]+'max'),
                                      min=Equation(kinfo[-1]+'min'))
                elif 'name' in name:
                    widget.key = 'name'
                    widget.dtype = str
                elif 'stl_shape' in name:
                    widget.key = 'stl_shape'
                    widget.dtype = str
                elif 'selection_method' in name:
                    widget.key = 'method'
                    widget.dtype = str
                elif 'filter' in name:
                    widget.key = '_'.join(name.split('_')[-2:])
                    widget.dtype = float
                elif 'deviation_angle' in name:
                    widget.key = 'deviation_angle'
                    widget.dtype = float
                elif 'equilibrant' in name:
                    widget.key = 'equilibrant'
                    widget.dtype = bool
                elif 'invert' in name:
                    widget.key = 'invert'
                    widget.dtype = bool
                elif 'geometry_pool' in name:
                    widget.key = 'geometry_pool'
                elif 'bc_type' in name:
                    widget.key = 'bc_type'

        self.error = self.gui.error
        self.warning = self.warn = self.gui.warn

        self.checkbox_selectfacets.clicked.connect(self.stl_type_changed)
        self.checkbox_selectfacets.toggled.connect(lambda c: self.groupbox_stl.setEnabled(c))
        self.groupbox_filter_facets.clicked.connect(self.filter_facets_changed)

        # default region buttons
        for btn, region in [(self.toolbutton_region_a, 'all'),
                            (self.toolbutton_region_l, 'left'), (self.toolbutton_region_r, 'right'),
                            (self.toolbutton_region_t, 'top'), (self.toolbutton_region_b, 'bottom'),
                            (self.toolbutton_region_f, 'front'), (self.toolbutton_region_back, 'back')]:
            btn.clicked.connect(lambda ignore, r=region: self.new_default_region(r))
            btn.setIcon(get_icon(region + '_region.svg'))
            btn.setToolTip(region)
            btn.setIconSize(sub_icon_size())


    def update_toplevelgeo(self, toplvl):
        lw = self.listwidget_regions_geometry_pool
        v = lw.value
        lw.clear()
        lw.add_items(toplvl)
        lw.updateValue(None, v)

    def reset_regions(self):
        self.tablewidget_regions.value.clear()
        self.parameter_key_map = {}

    def get_visibility_image(self, visible=True):
        return get_pixmap('visibility.svg' if visible else 'visibilityofflight.svg',
                          16, 16)

    def cell_clicked(self, index):
        if self.inhibit_toggle: # Don't toggle visibility on a row selection event
            self.inhibit_toggle = False
            return
        self.inhibit_toggle = False
        if index.column() == COLUMN_VISIBILITY:
            data = self.tablewidget_regions.value
            name = list(data.keys())[index.row()]

            vis = data[name]['visibility'] = not data[name]['visibility']
            self.vtkwidget.change_region_visibility(name, vis)

            data[name]['visible'] = self.get_visibility_image(vis)

            self.tablewidget_regions.set_value(data)
        elif index.column() == COLUMN_COLOR:
            self.change_color()

    def new_default_region(self, region):
        f = ['xmin', 'ymin', 'zmin']
        t = ['xmax', 'ymax', 'zmax']
        typ = 'box'
        if region == 'left':
            t[0] = 'xmin'
            typ = 'YZ-plane'
        elif region == 'right':
            f[0] = 'xmax'
            typ = 'YZ-plane'
        elif region == 'top':
            f[1] = 'ymax'
            typ = 'XZ-plane'
        elif region == 'bottom':
            t[1] = 'ymin'
            typ = 'XZ-plane'
        elif region == 'front':
            f[2] = 'zmax'
            typ = 'XY-plane'
        elif region == 'back':
            t[2] = 'zmin'
            typ = 'XY-plane'

        # convert strings to equations
        extents = [[Equation(e) for e in f], [Equation(e) for e in t]]
        self.new_region(region, extents, typ)

    def new_region(self, name=None, extents=None, rtype=None, defer_update=False):
        """create a new region"""
        # This is used both as a signal callback and an API function,
        # so there's some complexity with default args/
        if name in (None, True, False): # 'clicked' signal arguments
            name =  'R_1' # shorter than 'region', better than 'new'
        # Would be nice to name a box 'box', plane 'plane', etc but
        #  they all start as 'box' and then are changed to new type

        data = self.tablewidget_regions.value
        name = get_unique_string(name, list(data.keys()))

        reg_dat = deepcopy_dict(DEFAULT_REGION_DATA, qobjects=True)
        if rtype is not None and extents is not None:
            reg_dat['type'] = rtype
            reg_dat['from'] = extents[0]
            reg_dat['to'] = extents[1]
        reg_dat['visible'] = self.get_visibility_image(True)
        reg_dat['color'].rand()
        reg_dat['name'] = name

        # check all top level geometry by default
        reg_dat['geometry_pool'] = self.vtkwidget.get_toplevel_geomtry_names()

        # update parameters before the new region is added to the dictionary
        self.update_parameter_name(None, name, reg_dat)

        data[name] = reg_dat
        self.tablewidget_regions.set_value(data)
        self.vtkwidget.new_region(name, reg_dat)
        if defer_update:
            return

        #self.tablewidget_regions.fit_to_contents()
        self.fixup_regions_table(self.tablewidget_regions)
        self.tablewidget_regions.selectRow(len(data)-1) # Select new row
        if self.gui.mode == 'mesher':
            self.gui.clear_mesh_accepted()
        self.gui.set_unsaved_flag()
        if self.gui.sms_mode and self.gui.mode == 'mesher':
            self.gui.bcs_region_dict = self.get_region_dict()
            # If we add the region as a BC at creation, it's immediately 'in use'
            #  and we can't select facets
            #self.gui.bcs_add_regions_1([name], bc_type='MI')
            self.update_region_parameters()
            self.combobox_bc_type.setCurrentIndex(0) # None

        self.gui.update_nav_tree() # Enable/disable ICs/BCs etc

    def delete_region(self):
        'remove the currently selected region'
        rows = self.tablewidget_regions.current_rows()
        if rows:
            data = self.tablewidget_regions.value
            for row in rows:
                name = list(data.keys())[row]
                users = self.gui.get_region_users(name)
                bc_flag = (self.gui.sms_mode
                           and self.gui.mode == 'mesher'
                           and users == ['BC'])
                if users and not bc_flag:
                    self.gui.message(text="Region %s is in use" % name)
                    return
                if bc_flag:
                    for (idx, bc_data) in self.gui.bcs.items():
                        if bc_data.get('region') == name:
                            break
                    else:
                        idx = None
                    if idx is not None: # TODO: handle multi-region BCs (unlink)
                        bc_tab = self.gui.ui.boundary_conditions.tablewidget_regions
                        for r in range(bc_tab.rowCount()):
                            item_data = bc_tab.item(r,0).data(UserRole)
                            if item_data and idx in item_data[0]:
                                bc_tab.removeRow(r)
                                break

                        del self.gui.bcs[idx]
                        # Unset keywords
                        kwlist = list(self.gui.project.keywordItems())
                        for kw in kwlist:
                            if kw.key.startswith('bc_') and kw.args and kw.args[0]==idx:
                                self.gui.unset_keyword(kw.key, args=kw.args)

                deleted_region = data.pop(name)
                self.vtkwidget.delete_region(name)
                self.remove_from_parameter_map(name, deleted_region)

            self.tablewidget_regions.set_value(data)
            self.vtkwidget.render()
            if self.gui.mode == 'mesher':
                self.gui.clear_mesh_accepted()
            self.gui.set_unsaved_flag()
            self.gui.update_nav_tree()

        nrows = len(data)
        if rows[-1] == nrows: # We deleted the last row,
            #https://mfix.netl.doe.gov/gitlab/develop/mfix/issues/99
            if nrows > 0:
                self.tablewidget_regions.selectRow(nrows-1)

        self.update_region_parameters()
        self.fixup_regions_table(self.tablewidget_regions)

    def copy_region(self):
        'copy the currently selected region'
        rows = self.tablewidget_regions.current_rows()

        if rows:
            data = self.tablewidget_regions.value
            for row in rows:
                name = list(data.keys())[row]
                new_region = deepcopy_dict(data[name], qobjects=True)

                new_name = get_unique_string(name, list(data.keys()))
                # update parameters before the new region is added to the dictionary
                self.update_parameter_name(None,new_name, new_region)
                data[new_name] = new_region
                data[new_name]['visible'] = self.get_visibility_image(
                    data[new_name]['visibility'])
                self.vtkwidget.new_region(new_name, data[new_name])

            self.tablewidget_regions.set_value(data)
            #self.tablewidget_regions.fit_to_contents()
            self.fixup_regions_table(self.tablewidget_regions)
            self.tablewidget_regions.selectRow(len(data)-1)
            if self.gui.mode == 'mesher':
                self.gui.clear_mesh_accepted()
            self.gui.set_unsaved_flag()
            self.gui.update_nav_tree()


    def row_up(self):
        tw = self.tablewidget_regions
        rows = tw.current_rows()
        if not rows:
            return
        row = rows[0]
        p = list(range(len(tw.value)))
        p[row], p[row-1] = row-1, row
        self.permute_rows(p)
        tw.selectRow(row-1)

    def row_down(self):
        tw = self.tablewidget_regions
        rows = tw.current_rows()
        if not rows:
            return
        row = rows[0]
        p = list(range(len(tw.value)))
        p[row], p[row+1] = row+1, row
        self.permute_rows(p)
        tw.selectRow(row+1)

    def permute_rows(self, p):
        tw = self.tablewidget_regions
        data = tw.value
        # Death to OrderedDicts!
        keys = list(data.keys())
        vals = list(data.values())
        tw.set_value(dict((keys[p[i]], vals[p[i]])
                                 for i in range(len(data))))


    def update_region_parameters(self):
        'a new region was selected, update region widgets'
        rows = self.tablewidget_regions.current_rows()
        # This should be a single-selection table
        self.inhibit_toggle = True
        enabled = bool(rows)

        #self.toolbutton_region_delete.setEnabled(enabled) # Handled below
        self.toolbutton_region_copy.setEnabled(enabled)
        self.widget_region_parameters.setEnabled(enabled)
        row = rows[0] if enabled else None
        nrows = len(self.tablewidget_regions.value) if enabled else 0
        self.toolbutton_row_up.setEnabled(enabled and row > 0)
        self.toolbutton_row_down.setEnabled(enabled and row < nrows-1)

        if enabled:
            data = self.tablewidget_regions.value
            name = list(data.keys())[rows[-1]]
            data = data[name]
            data['name'] = name
            # enable widgets
            self.enable_disable_widgets(name)
        else:
            data = deepcopy_dict(DEFAULT_REGION_DATA, qobjects=True)
            name = ''

        # color
        self.toolbutton_color.setStyleSheet(
            "QToolButton{{ background: rgb({},{},{});}}".format(
                *data['color'].color_int))

        self.lineedit_region_name.updateValue(None, name)

        # Disable widgets for regions that are in use
        users = self.gui.get_region_users(name)
        bc_flag = (self.gui.sms_mode
                   and self.gui.mode == 'mesher'
                   and users == ['BC'])
        self.enable_disable_widgets(name, enable_all=bool(bc_flag or not users))

        self.toolbutton_region_delete.setEnabled(bool(enabled and (bc_flag or not users)))
        self.checkbox_selectfacets.setEnabled(bool(enabled and (bc_flag or not users)))
        base = 'Select Facets (STL)'
        self.checkbox_selectfacets.setText(base + ' - region in use' if (users and not bc_flag) else base)
        if self.gui.sms_mode and self.gui.mode=='mesher':
            #for w in (self.label_bc_type, self.combobox_bc_type):
            #    w.setEnabled('BC' in users)
            if 'BC' in users:
                for (idx, bc_data) in self.gui.bcs.items():
                    if bc_data.get('region') == name:
                        break
                else:
                    idx = None
                if idx is not None:
                    # This is simpler but does not respect mixed wall
                    #bc_type = self.gui.project.get_value('bc_type', args=idx, default='')
                    #if bc_type.startswith('CG_'):
                    #    bc_type = bc_type[3:]
                    #if bc_type not in BC_TYPES:
                    #    self.error("Unknown bc_type %s" % str(bc_type))
                    #else:
                    #    # offset 1 for 'None' in combobox
                    #    self.combobox_bc_type.setCurrentIndex(1+BC_TYPES.index(bc_type))
                    #
                    # The only place we keep track of 'mixed', when we are in PPO/SMS
                    #  mode, is in the BCS table.  So look in there.
                    # (use gui.bcs dictionary instead?)
                    bc_tab = self.gui.ui.boundary_conditions.tablewidget_regions
                    for r in range(bc_tab.rowCount()):
                        item = bc_tab.item(r, 0) # COLUMN_TYPE in bcs.py
                        item_data = item.data(UserRole)
                        if item_data and name in item_data[1]:
                            item = bc_tab.item(r, 1)
                            self.combobox_bc_type.setCurrentText(item.text())
                            break
                    else:
                        self.error("No BC found for %s" % name)
                else:
                    self.error("No BC found for %s" % name)
            else:
                self.combobox_bc_type.setCurrentIndex(0) # None

        users_str = ', '.join(users)
        data['used by'] = users_str
        # type
        r_type = data.get('type', 'box')
        self.checkbox_selectfacets.setChecked(r_type == 'STL')
        for wid in self.extent_lineedits:
            wid.error_check = r_type != 'STL'

        # from
        for widget, value in zip(self.extent_lineedits[::2], data['from']):
            widget.updateValue(None, value)

        # to
        for widget, value in zip(self.extent_lineedits[1::2], data['to']):
            widget.updateValue(None, value)

        # stl
        self.combobox_stl_shape.updateValue(None, data['stl_shape'])
        self.comboBox_selection_method.updateValue(None, data['method'])
        self.listwidget_regions_geometry_pool.updateValue(None, data['geometry_pool'])

        self.checkbox_equilibrant.updateValue(None, data['equilibrant'])
        self.checkbox_invert.updateValue(None, data['invert'])
        self.groupbox_filter_facets.setChecked(data['filter_facets'])
        self.label_numbeoffacets.setText(str(data['facet_count']))
        self.lineedit_deviation_angle.updateValue(None, data['deviation_angle'])
        for widget, value in zip([self.lineedit_filter_x,
                                  self.lineedit_filter_y,
                                  self.lineedit_filter_z],
                                  data['filter']):
            widget.updateValue(None, value)


    def setup_regions(self, allow_disabled_tab=False):
        # Set up all widgets in pane to current state,
        # including updates we deferred during extract_region

        data = self.tablewidget_regions.value
        for (k,v) in data.items():
            users = self.gui.get_region_users(k)
            users_str = ', '.join(users)
            data[k]['used by'] = users_str
        self.update_region_parameters()
        #self.tablewidget_regions.fit_to_contents()
        self.fixup_regions_table(self.tablewidget_regions)

    def stl_type_changed(self):
        stl = self.checkbox_selectfacets.isChecked()
        val = 'STL' if stl else None
        for wid in self.extent_lineedits:
            wid.error_check = not stl
        self.region_value_changed(self.checkbox_selectfacets, {'type': val}, [])

    def filter_facets_changed(self):
        val = self.groupbox_filter_facets.isChecked()
        self.region_value_changed(self.groupbox_filter_facets, {'filter_facets': val}, [])

    def set_facet_number(self, name, number):
        """call back from vtkwidget to update facet count"""
        data = self.tablewidget_regions.value
        row_data = data.get(name, None)
        if row_data is None:
            return
        row_data['facet_count'] = number
        self.update_region_parameters()

    def region_value_changed(self, widget, value, args, name=None, update_param=True):
        '''
        This method is called both directly and by widget value changes:
         - one of the region widgets values changed, update
         - a parameter changed that effects the region
        '''
        first_value = list(value.values())[0]
        data = self.tablewidget_regions.value
        if name is None:
            names = list(data.keys())
            if not names:
                return
            # issues/838 - only get the current row if name is None, else this
            # method fails silently when no row is selected.
            rows = self.tablewidget_regions.current_rows()
            if not rows:
                return
            name = names[rows[-1]]
        elif name not in data:
            self.error("region %s not defined" % name)
            return
        key = list(value.keys())[0]
        row_data = data[name]

        if self.gui.mode == 'mesher':
            self.gui.clear_mesh_accepted()
        self.gui.set_unsaved_flag()

        shape = row_data['type']
        to = 'to' in key
        from_ = 'from' in key
        stl = self.checkbox_selectfacets.isChecked()
        if key == 'bc_type':
            i = self.combobox_bc_type.currentIndex()
            bc_type = None if i==0 else BC_TYPES[i-1]
            if bc_type and shape=='STL':
                bc_type = 'CG_' + bc_type
            mixed_wall = 'mixed' in self.combobox_bc_type.currentText().lower()
            for (idx, bc_data) in self.gui.bcs.items():
                if bc_data.get('region') == name:
                    break
            else:
                idx = None
            if idx is not None: # Region is a BC
                self.gui.update_keyword('bc_type', bc_type, args=idx)
                bc_tab = self.gui.ui.boundary_conditions.tablewidget_regions
                for r in range(bc_tab.rowCount()):
                    item_data = bc_tab.item(r,0).data(UserRole)
                    if item_data and idx in item_data[0]:
                        if bc_type is None: # TODO: handle multi-region BCs (unlink)
                            bc_tab.removeRow(r)
                        else:
                            bc_tab.item(r,1).setText(self.combobox_bc_type.currentText())
                        break
                if bc_type is None: # Type has been set to None
                    del self.gui.bcs[idx]
                    # Unset keywords
                    kwlist = list(self.gui.project.keywordItems())
                    for kw in kwlist:
                        if kw.key.startswith('bc_') and kw.args and kw.args[0]==idx:
                            self.gui.unset_keyword(kw.key, args=kw.args)
            elif bc_type: # If type changed from None, (re)create the BC object
                #self.error("No BC found for %s" % name)
                #BC was deleted, or not yet defined.
                self.gui.bcs_region_dict = self.get_region_dict()
                self.gui.bcs_add_regions_1([name], bc_type=bc_type,
                                           mixed_wall=mixed_wall)
            self.update_region_parameters()

        elif to or from_:
            item = key.split('_')
            axis = item[1]
            index = ['x', 'y', 'z'].index(axis)

            # check for equation and special parameters max, min
            if isinstance(first_value, Equation):
                used = first_value.get_used_parameters()
                if 'min' in used or 'max' in used:
                    eq = first_value.eq
                    for r in RE_MIN, RE_MAX:
                        eq = r.sub(lambda m: axis+m[0], eq)
                    first_value.eq = eq
                    widget.updateValue(None, first_value)
            if update_param:
                self.update_parameter_map(value[key], name, key)

            # update data dict
            row_data[item[0]][index] = first_value

            users = self.gui.get_region_users(name)
            # Infer shape from extents
            if not users:
                old_shape = row_data.get('type', 'box')
                if stl:
                    shape = 'STL'
                else:
                    shape = row_data['type'] = self.get_region_type([row_data['from'], row_data['to']])
                if old_shape != shape:
                    self.vtkwidget.change_region_type(name, row_data)

            # check for and update point and plane extents
            shape = data[name]['type']
            if users and any(s in shape for s in ['plane', 'point']) and item[1] not in shape.lower():
                self.update_parameter_map(first_value, name, 'to_'+str(item[1]))
                row_data['to'][index] = first_value
                self.extent_lineedits[index*2+1].updateValue(None, first_value)

            # propagate values
            for update in (self.vtkwidget.update_region,
                           self.gui.update_region):
                update(name, row_data)

        elif 'name' in key and name != first_value:
            new_name = get_unique_string(first_value,
                                         list(data.keys()))
            if not new_name:
                self.gui.error('Invalid name "%s"' % value)
                return

            data = dict(((new_name, v) if k == name else (k, v) for
                                (k, v) in data.items()))
            data[new_name]['name'] = new_name

            # update parameter map (?)
            self.update_parameter_name(name, new_name, row_data)
            self.vtkwidget.change_region_name(name, new_name)

            # Unfortunately gui.change_region_name needs the table data to be set
            self.tablewidget_regions.set_value(data)
            self.gui.change_region_name(name, new_name)

        elif 'type' in key:
            shape = first_value
            if shape is None:
                shape = self.get_region_type([row_data['from'], row_data['to']])
            data[name]['type'] = shape
            self.vtkwidget.change_region_type(name, data[name])
            self.enable_disable_widgets(name)

            # Handle 'CG_' prefix on bc_type key
            for (idx, bc_data) in self.gui.bcs.items():
                if bc_data.get('region') == name:
                    break
            else:
                idx = None
            if idx is not None:
                bc_type = self.gui.project.get_value('bc_type', args=idx)
                if bc_type:
                    if shape == 'STL' and not bc_type.startswith('CG_'):
                        self.gui.update_keyword('bc_type', 'CG_'+bc_type, args=idx)
                    elif bc_type.startswith('CG_') and shape != 'STL':
                        self.gui.update_keyword('bc_type', bc_type[3:], args=idx)

            # check for plane, update extents
            if 'plane' in shape:
                shape = shape.lower()
                index = [d not in shape for d in 'xyz'].index(True)
                f_value = data[name]['from'][index]
                self.update_parameter_map(f_value, name, 'to_'+'xyz'[index])
                row_data['to'][index] = f_value
                self.extent_lineedits[index*2+1].updateValue(None, f_value)
            # check for point, update extents
            elif shape == 'point':
                for index in (0,1,2):
                    f_value = row_data['from'][index]
                    row_data['to'][index] = f_value
                    self.update_parameter_map(f_value, name, 'to_'+'xyz'[index])
                    self.extent_lineedits[index*2+1].updateValue(None, f_value)

        #TODO this seems like lots of copy/paste
        elif 'stl_shape' in key:
            s = row_data['stl_shape'] = first_value
            if s == 'all':
                self.comboBox_selection_method.updateValue(None, 'Full')
            row_data['slice'] = False
            self.vtkwidget.update_region(name, row_data)

        elif 'method' in key:
            row_data['method'] = first_value
            self.vtkwidget.update_region(name, row_data)

        elif 'filter_facets' in key:
            row_data['filter_facets'] = first_value
            self.vtkwidget.update_region(name, row_data)

        elif 'equilibrant' in key:
            row_data['equilibrant'] = first_value
            self.vtkwidget.update_region(name, row_data)

        elif 'invert' in key:
            row_data['invert'] = first_value
            self.vtkwidget.update_region(name, row_data)

        elif 'filter' in key:
            item = key.split('_')
            index = ['x', 'y', 'z'].index(item[1])
            row_data[item[0]][index] = first_value
            self.vtkwidget.update_region(name, row_data)

            if update_param:
                self.update_parameter_map(value[key], name, key)

        elif 'deviation_angle' in key:
            row_data['deviation_angle'] = first_value
            self.vtkwidget.update_region(name, row_data)

            if update_param:
                self.update_parameter_map(value[key], name, key)

        elif 'geometry_pool' in key:
            row_data['geometry_pool'] = first_value
            self.vtkwidget.update_region(name, row_data)

        self.tablewidget_regions.set_value(data)

        if key == 'type':
            self.gui.update_nav_tree() # ICs/BCs availability depends on region types

    def enable_disable_widgets(self, name, enable_all=False):
        data = self.tablewidget_regions.value
        users = self.gui.get_region_users(name)
        # Allow changing a region, if it only controls VTK, Monitors, or PSs
        input_enabled = self.input_enabled or not any(x in users for x in ('BCs', 'ICs', 'ISs'))
        enable_list = [input_enabled]*6
        if not enable_all:
            rtype = data.get('name',{}).get('type')
            if rtype == 'point':
                enable_list[1::2] = [False]*3
            elif rtype == 'XY-plane':
                enable_list[5] = False
            elif rtype == 'XZ-plane':
                enable_list[3] = False
            elif rtype == 'YZ-plane':
                enable_list[1] = False

        for widget, enable in zip(self.extent_lineedits, enable_list):
            widget.setEnabled(enable)

    def change_color(self):
        rows = self.tablewidget_regions.current_rows()
        data = self.tablewidget_regions.value
        name = list(data.keys())[rows[-1]]

        color = QtWidgets.QColorDialog.getColor(data[name]['color'].color, parent=self, title='Select region color')

        if color.isValid():
            data[name]['color'].color = color

            self.toolbutton_color.setStyleSheet(
                "QToolButton{{ background: rgb({},{},{});}}".format(
                    *data[name]['color'].color_int))

            self.gui.set_unsaved_flag()
            self.vtkwidget.change_region_color(name, data[name]['color'])

    def regions_to_str(self):
        """ convert regions data to a string for saving """
        data = {'order': list(self.tablewidget_regions.value.keys()),
                'regions': {}
                }
        for region in data['order']:
            data['regions'][region] = clean_region_dict(self.tablewidget_regions.value[region])
        return ExtendedJSON.dumps(data)

    def regions_from_str(self, string):
        """ load regions data from a saved string """
        loaded_data = ExtendedJSON.loads(string) # Order of dict has been lost

        if 'order' not in loaded_data:
            return

        data = {}  # Rebuild data dict

        for region in loaded_data['order']:
            # Copy dictionary entry to ordered dict
            region_data = data[region] = deepcopy_dict(DEFAULT_REGION_DATA, qobjects=True)
            loaded_region = loaded_data['regions'].get(region, {})

            # issue/1156
            if loaded_region.get('geometry_pool', None) is None:
                # most likely created with a GUI prior to 19.1, select all geometry
                loaded_region['geometry_pool'] = self.vtkwidget.get_toplevel_geomtry_names()

            region_data.update(loaded_region)
            if 'visibility' in region_data:
                # Create pixmap for 'visible' column
                region_data['visible'] = self.get_visibility_image(region_data['visibility'])
            if 'color' in region_data:
                # Convert to a CellColor object
                region_data['color'] = CellColor(region_data['color'])

            # slice is deprecated in 20.2, replace with 'method' = 'Slice'
            if region_data.get('slice', False):
                region_data['method'] = 'Slice'
                region_data['slice'] = False

            region_data['name'] = region # For table display only

            #build parameter map
            self.update_parameter_name(None, region, region_data)

            # add to vtk widget
            self.vtkwidget.new_region(region, region_data)

        self.tablewidget_regions.set_value(data)
        #self.tablewidget_regions.fit_to_contents()
        self.fixup_regions_table(self.tablewidget_regions)

    def extract_regions(self, proj, proj_dir=None):
        """ extract regions from IC, BC, PS, IS, VTK, MONITOR"""
        if self.tablewidget_regions.value:
            # We assume regions_dict has been initialized correctly
            # from mfix_gui_comments.
            return

        stl_files = []
        stl_nums = []
        if proj_dir:
            # look for geometry_#####.stl files
            stl_files = glob.glob(os.path.join(proj_dir, 'geometry_*.stl'))
            # extract numbers
            stl_nums = [safe_int(f.split('.')[0].split('_')[-1]) for f in stl_files]

        for prefix, conds in (('ic_', proj.ics), ('bc_', proj.bcs),
                              ('is_', proj.iss), ('ps_', proj.pss),
                              ('vtk_', proj.vtks), ('monitor_', proj.monitors)):
            for cond in conds:
                extents = []
                extents_keys = False
                for key in ('x_w', 'x_e', 'y_s', 'y_n', 'z_b', 'z_t'):
                    key = prefix + key
                    if key in cond:
                        extents.append(float(cond[key]))
                        extents_keys = True
                    else:
                        extents.append(0.0)

                # reformat extents
                extents = [extents[::2], extents[1::2]]

                # infer region type from extents
                rtype = self.get_region_type(extents)

                # create a name
                name = prefix.upper() + str(cond.ind) # "BC_1"

                add = False
                # handle CG_* regions
                if ('bc_type' in cond and cond['bc_type'].value.lower().startswith('cg')):
                    rtype = 'STL'
                    add = True
                    # single stl bc, assume region fills domain
                    if cond.ind == proj.get_value('stl_bc_id'):
                        ext = [Equation(s) for s in ['xmin', 'xmax', 'ymin', 'ymax', 'zmin', 'zmax']]
                        extents = [ext[::2], ext[1::2]]
                        for key, value in [('from', ext[::2]), ('to', ext[1::2])]:
                            for v, k in zip(value, ['x', 'y', 'z']):
                                self.update_parameter_map(v, name, '_'.join([key,k]))
                    else:
                        if cond.ind in stl_nums:
                            extents = self.vtkwidget.get_stl_extents(stl_files[stl_nums.index(cond.ind)])
                            # reformat extents
                            extents = [extents[::2], extents[1::2]]

                # if extents are not already used by a region, add it
                elif not self.check_extents_in_regions(extents) and extents_keys:
                    add = True
                elif not extents_keys:
                    self.warn('{} does not have extents defined and is not a cartesian grid, ignoring'.format(name))
                #else: # XXX FIXME this happens when regions are shared
                #    self.warn('could not infer region from {}'.format(name))

                if add:
                    self.new_region(name, extents, rtype, defer_update=True)

    def fixup_regions_table(self, tw, stretch_column=3):
        ui = self # !!
        hv = QtWidgets.QHeaderView
        resize = tw.horizontalHeader().setSectionResizeMode
        ncols = tw.model().columnCount()
        for n in range(0, ncols):
            resize(n, hv.Stretch if n==stretch_column else hv.ResizeToContents)

        # trim excess vertical space - can't figure out how to do this in designer
        header_height = tw.horizontalHeader().height()

        # Note - scrollbar status can change outside of this function.
        # Do we need to call this every time window geometry changes?
        scrollbar_height = tw.horizontalScrollBar().isVisible() * (4+tw.horizontalScrollBar().height())
        nrows = tw.model().rowCount()
        if nrows==0:
            row_height = 0
            height = header_height+scrollbar_height
        else:
            row_height = tw.rowHeight(0)
            height =  (header_height+scrollbar_height
                       + nrows*row_height + 4) # extra to avoid unneeded scrollbar

        if tw == ui.tablewidget_regions:
            icon_height = sub_icon_height() + 8
            ui.top_frame.setMaximumHeight(height+icon_height)
            ui.top_frame.setMinimumHeight(header_height+icon_height+row_height*min(nrows,5))
            ui.top_frame.updateGeometry()
            tw.setMaximumHeight(height)
            tw.setMinimumHeight(header_height)
        else:
            tw.setMaximumHeight(height)
            tw.setMinimumHeight(height)
        tw.updateGeometry() #? needed?

    def check_extents_in_regions(self, extents):
        """ check to see if the extents are already in a region """
        region_dict = self.tablewidget_regions.value
        for key in region_dict.keys():
            region = region_dict.get(key)
            region_extent = [region['from'], region['to']]
            if extents == region_extent:
                return True
        return False

    def get_region_type(self, extents):
        """ given the extents, guess the region type """
        rtype = 'box'
        if extents[0] == extents[1]:
            rtype = 'point'
        else:
            for r, f, t in zip(['YZ-plane', 'XZ-plane', 'XY-plane'],
                               extents[0], extents[1]):
                if f == t:
                    rtype = r
                    break
        return rtype

    def get_region_dict(self):
        """return region dict, for use by clients"""
        region_dict = self.tablewidget_regions.value
        return deepcopy_dict(region_dict) # Allow clients to modify dict

    def get_value(self, name, key):
        """given a region name and value key, return the value"""

        data = self.tablewidget_regions.value.get(name)
        # safe to assume key is always in data?
        if data is None:
            val = None
        elif 'to_' in key or 'from_' in key or 'filter_' in key:
            item = key.split('_')
            if len(item)>1 and item[1] in 'xyz':
                index = 'xyz'.index(item[1])
                val = data[item[0]][index]
            else:
                val = data[key]
        else:
            val = data[key]
        return val

    def update_parameter_map(self, new_value, name, key):
        """update the mapping of parameters and keywords"""
        name_key = ','.join([name, key])
        # new params
        new_params = []
        if isinstance(new_value, Equation):
            new_params = new_value.get_used_parameters()

        # old params
        old_value = self.get_value(name, key)

        old_params = []
        if isinstance(old_value, Equation):
            old_params = old_value.get_used_parameters()

        add = set(new_params)-set(old_params)
        for param in add:
            self.add_namekey(param, name_key)

        remove = set(old_params)-set(new_params)
        for param in remove:
            self.remove_namekey(param, name_key)

    def add_namekey(self, param, name_key):
        """add the name_key to the parameter list"""
        if param not in self.parameter_key_map:
            self.parameter_key_map[param] = set()
        self.parameter_key_map[param].add(name_key)

    def remove_namekey(self, param, name_key):
        """remove the name_key from the parameter list"""
        self.parameter_key_map[param].remove(name_key)
        if len(self.parameter_key_map[param]) == 0:
            self.parameter_key_map.pop(param)

    def update_parameters(self, params):
        """parameters have changed, update regions"""
        for param in params:
            if param in self.parameter_key_map:
                for var in self.parameter_key_map[param].copy():
                    name, key = var.split(',')
                    self.region_value_changed(
                        None, {key: self.get_value(name, key)}, None,
                        name=name, update_param=False)

    def remove_from_parameter_map(self, name, del_region):
        """a region was deleted, make sure to remove from parameter map"""
        for key, value in del_region.items():
            name_key = ','.join([name, key])
            if isinstance(value, list):
                for xyz, item in zip(['x', 'y', 'z'], value):
                    self._remove_key('_'.join([name_key, xyz]), item)
            else:
                self._remove_key(key, value)

    def update_parameter_name(self, old_name, new_name, region):
        """a region name changed, update map"""
        if old_name:
            self.remove_from_parameter_map(old_name, region)
        for k, v in region.items():
            if isinstance(v, list):
                for xyz, item in zip(['x', 'y', 'z'], v):
                    self.update_parameter_map(item, new_name, '_'.join([k, xyz]))
            else:
                self.update_parameter_map(v, new_name, k)

    def _remove_key(self, name_key, value):
        if not isinstance(value, Equation): return

        for param in value.get_used_parameters():
            keys = self.parameter_key_map.get(param, None)
            if keys:
                keys.remove(name_key)
                if len(self.parameter_key_map[param]) == 0:
                    self.parameter_key_map.pop(param)
            else:
                self.warn("couldn't remove {} from {}".format(param, keys))
