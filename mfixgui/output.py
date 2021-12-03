# -*- coding: utf-8 -*-

"""Output Task Pane Window"""

import os
import glob

from json import JSONDecoder, JSONEncoder

from qtpy.QtCore import Qt

from qtpy.QtWidgets import (QComboBox, QGroupBox, QHeaderView, QLabel,
                            QLineEdit, QPushButton, QTableWidgetItem, QWidget)

from qtpy.QtGui import QPixmap, QPalette

from mfixgui.animations import animate_stacked_widget

from mfixgui.constants import *

from mfixgui.tools.qt import (get_combobox_item, get_selected_row,
                              set_item_enabled, set_item_noedit,
                              sub_icon_height, widget_iter)

from mfixgui.widgets.base import (BaseWidget, LineEdit, CheckBox, ComboBox)

UserRole = Qt.UserRole

#Top tabset
BASIC_TAB, VTK_TAB, SPX_TAB, RESIDUALS_TAB, LOG_TAB, USER_TAB = range(6)
PAGE_CELL, PAGE_PARTICLE, PAGE_FORCE, PAGE_GEOMETRY = range(4)

#Columns in VTK output region table
COLUMN_REGION, COLUMN_FILENAME, COLUMN_TYPE, COLUMN_ID = range(4)

#Bottom subpage tabset, for VTK cell data
(FLUID_TAB, SOLIDS_TAB_DUMMY_L, SOLIDS_TAB, SOLIDS_TAB_DUMMY_R,
 SCALAR_TAB, REACTIONS_TAB, OTHER_TAB) = range(7) # bottom tabset

MAX_SP = 11

VTK_DATA_TYPES = ['C', 'P', 'F', 'G']

USR_KEYS = ['usr_' + x for x in ('x_w', 'x_e', 'y_s', 'y_n', 'z_b', 'z_t',
                                 'dt',  'var', 'format', 'ext', 'type')]

class Output(object):
    #Output Task Pane Window:
    #The output input is split into tabs.

    def init_output(self):
        ui = self.ui.output

        self.output_current_tab = BASIC_TAB
        self.vtk_outputs = {} # key: index.  value: data dictionary for VTK output region
        # Note, SingleSelection enabled so 'indices' and 'regions' are really just 'index' and 'region'
        self.vtk_current_indices = [] # List of VTK output indices
        self.vtk_current_regions = [] # And the names of the regions which define them
        self.vtk_current_solid = self.P = None

        # Dynamically created items
        ui.dynamic_widgets = {}
        ui.groupbox_filter_particles_checked = None # Tristate
        self.usr_outputs = {}
        self.usr_current_index = None
        self.output_region_dict = None
        # connect tab buttons
        self.output_pushbuttons = (ui.pushbutton_basic,
                                   ui.pushbutton_vtk,
                                   ui.pushbutton_spx,
                                   ui.pushbutton_residuals,
                                   ui.pushbutton_log,
                                   ui.pushbutton_usr)

        for (i, btn) in enumerate(self.output_pushbuttons):
            btn.pressed.connect(lambda i=i: self.output_change_tab(i))

        self.init_output_basic_tab()
        self.init_output_vtk_tab()
        self.init_output_spx_tab()
        self.init_output_residuals_tab()
        self.init_output_log_tab()
        self.init_output_usr_tab()
        self.output_logs = {} # Key:  item being logged, value:  (enable, filename, overwrite/append/increment)

        cb = ui.combobox_vtk_select_mode
        key = 'vtk_select_mode'
        cb.key = key
        for (i, value) in enumerate('CPI'):
            item = get_combobox_item(cb, i)
            self.add_tooltip(item, key, value=value)
        cb.activated.connect(self.handle_combobox_vtk_select_mode)
        # Make the table update when user changed VTK_FILEBASE
        le = ui.lineedit_keyword_vtk_filebase_args_VTK
        le.post_update = self.setup_output_vtk_tab
        # Cannot save solid inventory if we are not reporting it
        cb = ui.checkbox_keyword_report_solid_inventory
        cb.post_update = self.enable_disable_solid_inventory


    def enable_disable_solid_inventory(self):
        # Can't save it if it's not being reported!
        ui = self.ui.output
        val = bool(self.project.get_value('report_solid_inventory', False))
        gb = ui.groupbox_save_solid_inventory
        gb.setEnabled(val)
        ui.widget_solid_inventory_frequency.setEnabled(val)
        ui.checkbox_keyword_breakdown_solid_inventory_by_phase.setEnabled(val)


    def handle_combobox_vtk_select_mode(self, index):
        ui = self.ui.output
        key = 'vtk_select_mode'
        val = 'CPI'[index]
        for v in self.vtk_current_indices:
            self.update_keyword(key, val, args=[v])
        cb = ui.combobox_vtk_select_mode
        cb.setToolTip(get_combobox_item(cb,index).toolTip())

    def handle_combobox_usr_region(self, index):
        ui = self.ui.output
        id = self.usr_current_index
        cb = ui.combobox_usr_region
        if id is None:
            return
        if index == 0: # None
            text = "None"
            for k in 'x_w', 'x_e', 'y_s', 'y_n', 'z_b', 'z_t':
                self.unset_keyword('usr_'+k, args=[id])
        else:
            if self.output_region_dict is None:
                self.output_region_dict = self.ui.regions.get_region_dict()
            text = cb.currentText()
            if text not in self.output_region_dict:
                self.error("Unknown region %s" % text, popup=True)
                return
            data = self.output_region_dict[text]
            self.output_set_usr_region_keys(id, data)

        ui.tablewidget_usr_regions.item(id-1, 0).setText(text)
        self.usr_outputs[id]['region'] = text


    # Output sub-pane navigation
    def output_change_tab(self, tabnum):
        ui = self.ui.output
        to_btn = self.output_pushbuttons[tabnum]
        self.output_current_tab = tabnum
        animate_stacked_widget(
            self,
            ui.stackedwidget_output,
            (ui.stackedwidget_output.currentIndex(), tabnum),
            line=ui.line_output,
            to_btn=to_btn,
            btn_layout=ui.gridlayout_tab_btns)
        self.setup_output_tab(tabnum)
        for btn in self.output_pushbuttons:
            btn.setChecked(btn == to_btn)
            font = btn.font()
            font.setBold(btn == to_btn)
            btn.setFont(font)


    def init_output_basic_tab(self):
        ui = self.ui.output

        for item in widget_iter(ui.page_basic):
            if isinstance(item, BaseWidget):
                item.post_update = self.setup_output_basic_tab

        gb = ui.groupbox_write_vtk_files
        key = 'write_vtk_files'
        gb.clicked.connect(self.output_enable_vtk)
        self.add_tooltip(gb, key)

        gb = ui.groupbox_write_part_out
        key = 'write_part_out'
        gb.clicked.connect(self.handle_write_part_out)
        self.add_tooltip(gb, key)
        gb.tooltip0 = gb.toolTip()

        ui.groupbox_filter_particles.clicked.connect(
            self.output_handle_groupbox_filter_particles)

        # Set up widgets for _exclude keys
        for w in widget_iter(ui.groupbox_filter_particles):
            w.hidden_ctrl = ui.groupbox_filter_particles
            if not isinstance(w, (QComboBox, QLabel)):
                continue
            name = w.objectName()
            if name.startswith('combobox_filter_'):
                w.key = 'part_out_' +  name.split('combobox_filter_', 1)[-1] + '_exclude'
                w.args = None
                self.add_tooltip(w, w.key)
                w.currentIndexChanged.connect(lambda idx, w=w:
                                              self.output_handle_part_out_exclude(w, idx))
            if name.startswith('label_filter_'):
                if name.endswith('_units'):
                    name = name[:-6]
                w.key = 'part_out_' +  name.split('label_filter_', 1)[-1] + '_max'
                w.args = None
                self.add_tooltip(w, w.key)

        cb = ui.checkbox_spx
        cb.clicked.connect(self.output_enable_spx)

    def handle_write_part_out(self, val):
        ui = self.ui.output
        if val:
            self.update_keyword('write_part_out', True)
            r = self.get_retained_keyword('part_out_zero_vel')
            if val is not None:
                self.update_keyword('part_out_zero_vel', r)
        else:
            self.unset_keyword('write_part_out')
            zero_vel = self.project.get_value('part_out_zero_vel')
            if zero_vel:
                self.retain_keyword('part_out_zero_vel')
            else:
                self.clear_retained_keyword('part_out_zero_vel')
            self.unset_keyword('part_out_zero_vel')
            ui.checkbox_keyword_part_out_zero_vel.setChecked(False)
        check = val and any((k[0].startswith('part_out') and k[0] != 'part_out_zero_vel')
                            for k in self.retained_keys)
        ui.groupbox_filter_particles.setChecked(check)
        self.output_handle_groupbox_filter_particles(check, use_retained=True)


    def init_output_vtk_tab(self):
        #VTK (tab)
        ui = self.ui.output

        self.output_saved_fluid_species_names = []
        self.output_saved_solids_names = []
        self.output_saved_solids_species_names = []

        # Set up subtabs
        self.output_pushbuttons_bottom = (ui.pushbutton_fluid,
                                          #ui.pushbutton_solid,
                                          ui.pushbutton_scalar,
                                          ui.pushbutton_reactions,
                                          ui.pushbutton_other)

        self.output_current_subtab = FLUID_TAB
        self.vtk_current_solid = self.P = self.usr_current_index = None
        ui.pushbutton_fluid.pressed.connect(lambda: self.output_change_subtab(FLUID_TAB, None))
        ui.pushbutton_scalar.pressed.connect(lambda: self.output_change_subtab(SCALAR_TAB, None))
        ui.pushbutton_reactions.pressed.connect(lambda: self.output_change_subtab(REACTIONS_TAB, None))
        ui.pushbutton_other.pressed.connect(lambda: self.output_change_subtab(OTHER_TAB, None))

        # Trim width of "Fluid" and "Scalar" buttons, like we do for
        # dynamically-created "Solid #" buttons
        for b in self.output_pushbuttons_bottom:
            w = b.fontMetrics().boundingRect(b.text()).width() + 20
            b.setMaximumWidth(w)

        # Icons and table similar to IC/BC/PS/IS for adding VTK regions. This
        # section requires WRITE_VTK_FILES = .TRUE. Icons to
        # add/remove/duplicate regions are given at the top Clicking the 'add'
        # and 'duplicate' buttons triggers a popup window where the user must
        # select a VTK region.

        ui.toolbutton_add.clicked.connect(self.output_show_regions_popup)
        ui.toolbutton_delete.clicked.connect(self.output_delete_regions)
        ui.toolbutton_delete.setEnabled(False) # Need a selection
        ui.tablewidget_vtk_regions.itemSelectionChanged.connect(self.handle_output_vtk_region_selection)

        ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_CELL)
        # no per-subpage init, yet, so there's some per-page init here
        ui.checkbox_keyword_vtk_vorticity_args_VTK.post_update = self.set_vtk_lambda_2

        # Show cell data groupboxes without a title (title is separate
        # label_select_cell_data, displayed outside tab set)

        # Show groupbox without title  (title is empty string)
        height = ui.checkbox_keyword_vtk_ep_g_args_VTK.sizeHint().height()
        tweak = 3
        # Fix gap where title would be, with negative margin.
        # This is somewhat quesionable (i.e. a total hack)
        # Furthermore setting the style in the subpage no longer works as of Qt 5.12
        #ui.subpage_cell.setStyleSheet(
        #    'QGroupBox{padding-top: %spx; margin-top: %spx;}' %
        #    (height, tweak-height))
        for w in widget_iter(ui.subpage_cell):
            if isinstance(w, QGroupBox):
                if w.title():
                    continue
                w.setStyleSheet(
                    'QGroupBox{padding-top: %spx; margin-top: %spx;}' %
                    (height, tweak-height))

        # Locatability
        ui.groupbox_cell_reactions.keys = ['vtk_des_rrate', 'vtk_fluid_rrate']
        ui.groupbox_cell_reactions_deprecated.keys = ['vtk_rrate', 'vtk_rrate_label']

    def init_output_usr_tab(self):
        ui = self.ui.output
        ui.toolbutton_usr_add.clicked.connect(self.output_add_usr_row)
        ui.toolbutton_usr_delete.clicked.connect(self.output_delete_usr_row)
        ui.toolbutton_usr_up.clicked.connect(self.output_up_usr_row)
        ui.toolbutton_usr_down.clicked.connect(self.output_down_usr_row)
        ui.tablewidget_usr_regions.itemSelectionChanged.connect(self.handle_output_usr_region_selection)
        ui.lineedit_keyword_usr_dt_args_USR.post_update = self.update_usr_regions_table # update table when usr_dt modified
        ui.combobox_usr_region.activated.connect(self.handle_combobox_usr_region)
        ui.lineedit_keyword_usr_var_args_USR.setMaxLength(60)
        ui.lineedit_keyword_usr_format_args_USR.setMaxLength(60)
        ui.lineedit_keyword_usr_ext_args_USR.setMaxLength(16)
        ui.lineedit_keyword_usr_type_args_USR.setMaxLength(6)


    def output_change_subtab(self, subtab, solid):
        ui = self.ui.output
        index = (0 if subtab == FLUID_TAB
                 else len(self.solids)+1 if subtab == SCALAR_TAB
                 else len(self.solids)+2 if subtab == REACTIONS_TAB
                 else len(self.solids)+3 if subtab == OTHER_TAB
                 else solid)

        for i in range(ui.bottom_tab_layout.columnCount()):
            item = ui.bottom_tab_layout.itemAtPosition(0, i)
            if not item:
                continue
            widget = item.widget()
            if not widget:
                continue
            font = widget.font()
            font.setBold(i == index)
            widget.setFont(font)

        current_index = ui.stackedwidget_cell.currentIndex()
        # If we're switching from solid m to solid n, we need some
        # special handling, because both tabs are really the same
        # widget.  We make a picture of the current tab, display that
        # in a dummy pane, then slide back to the solids tab
        if subtab == current_index == SOLIDS_TAB:
            if solid == self.vtk_current_solid:
                return # nothing to do

            if solid > (self.vtk_current_solid or 0):
                dummy_label = ui.label_dummy_solids_L
                dummy_tab = SOLIDS_TAB_DUMMY_L
            else:
                dummy_label = ui.label_dummy_solids_R
                dummy_tab = SOLIDS_TAB_DUMMY_R

            p = QPixmap(ui.subpage_cell_solids.size())
            p.fill(ui.scrollarea_cell_contents.palette().color(QPalette.Window))
            ui.subpage_cell_solids.render(p, flags=QWidget.DrawChildren) # avoid rendering bg
            dummy_label.setPixmap(p)
            ui.stackedwidget_cell.setCurrentIndex(dummy_tab)

        self.output_current_subtab = subtab
        self.vtk_current_solid = self.P = solid if subtab == SOLIDS_TAB else None
        self.setup_output_current_subtab()

        # change stackedwidget contents
        animate_stacked_widget(
            self,
            ui.stackedwidget_cell,
            (ui.stackedwidget_cell.currentIndex(), subtab),
            line=ui.line_subtab,
            to_btn=ui.bottom_tab_layout.itemAtPosition(0, index),
            btn_layout=ui.bottom_tab_layout)
        # Scroll to top
        ui.scrollarea_cell.ensureVisible(0, 0)


    def handle_output_vtk_region_selection(self):
        ui = self.ui.output
        tw = ui.tablewidget_vtk_regions
        row = get_selected_row(tw)
        if row is None:
            indices = []
            regions = []
        else:
            (indices, regions) = tw.item(row, 0).data(UserRole)
        self.vtk_current_indices, self.vtk_current_regions = indices, regions
        enabled = (row is not None)
        ui.toolbutton_delete.setEnabled(enabled and ui.input_enabled)
        # This disables the bottom set of tabs, so navigation is limited
        #ui.bottom_frame_vtk.setEnabled(enabled and ui.input_enabled)

        # Allow the bottom tab set to still function during run
        for w in (ui.cell_common,
                  ui.subpage_particle,
                  ui.scrollarea_cell):
            w.setEnabled(enabled and ui.input_enabled)
        ui.bottom_tab_frame.setEnabled(enabled) # Allow sub-tab navigation while locked
        ui.label_select_cell_data.setEnabled(enabled)

        ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_CELL) # Why?
        if not enabled: # No selection, clear inputs
            for widget in widget_iter(ui.bottom_frame_vtk):
                if isinstance(widget, LineEdit):
                    widget.setText('')
                elif isinstance(widget, CheckBox):
                    widget.setChecked(False)
            return
        self.setup_output_vtk_tab() # reinitialize all widgets in current tab


    def handle_output_usr_region_selection(self):
        ui = self.ui.output
        tw = ui.tablewidget_usr_regions
        row = get_selected_row(tw)
        if row is None:
            index = region = None
        else:
            index, region = tw.item(row, 0).data(UserRole)
        self.usr_current_index, self.usr_current_regions = index, region
        self.setup_output_usr_tab() # reinitialize all widgets in current tab


    def fixup_output_table(self, tw, stretch_column=1):
        ui = self.ui.output
        hv = QHeaderView
        resize = tw.horizontalHeader().setSectionResizeMode
        ncols = tw.columnCount()
        for n in range(0, ncols):
            resize(n, hv.Stretch if n == stretch_column else hv.ResizeToContents)

        # trim excess vertical space - can't figure out how to do this in designer
        header_height = tw.horizontalHeader().height()

        # Note - scrollbar status can change outside of this function.
        # Do we need to call this every time window geometry changes?
        scrollbar_height = tw.horizontalScrollBar().isVisible() * (4+tw.horizontalScrollBar().height())
        nrows = tw.rowCount()
        if nrows == 0:
            row_height = 0
            height = header_height+scrollbar_height
        else:
            row_height = tw.rowHeight(0)
            height = (header_height+scrollbar_height
                      + nrows*row_height + 4) # extra to avoid unneeded scrollbar (?)
        icon_height = sub_icon_height() + 8
        top_frame = ui.top_frame_vtk if tw == ui.tablewidget_vtk_regions else ui.top_frame_usr
        top_frame.setMaximumHeight(height+icon_height)
        top_frame.setMinimumHeight(header_height+icon_height+row_height*min(nrows, 5))
        top_frame.updateGeometry()
        tw.setMaximumHeight(height)
        tw.setMinimumHeight(header_height)
        tw.updateGeometry()


    def output_show_regions_popup(self):
        # Users cannot select inapplicable regions.
        # VTK regions can be points, planes, or volumes (not STLs)
        # Regions can define multiple VTK regions.
        # per 1130 now STL regions can have output: vtk_geo
        ui = self.ui.output
        rp = self.regions_popup
        rp.clear()
        for (name, data) in self.output_region_dict.items():
            shape = data.get('type', '---')
            # Assume available if unmarked
            available = (data.get('available', True))
                          #and (shape in ('point', 'box')
                          #    or 'plane' in shape))
            row = (name, shape, available)
            rp.add_row(row)
        #Select Output type
        # Selection is required
        # Available selections:
        #  Cell data
        #    Selection always available
        #    Set keyword VTK_DATA(#) to 'C'
        #  Particle data
        #    Selection only available with DEM, CGP or PIC solids
        #    Sets keyword VTK_DATA(#) to 'P'
        solids_models = set(self.project.get_value('solids_model', args=[i])
                            for i in range(1, len(self.solids)+1))

        rp.reset_signals()
        rp.save.connect(self.output_add_regions)
        rp.cancel.connect(self.output_cancel_add)
        for item in (ui.tablewidget_vtk_regions,
                     ui.bottom_frame_vtk, # Disable to prevent user input when dialog active.
                     ui.toolbutton_add,
                     ui.toolbutton_delete):
            item.setEnabled(False)
        rp.popup('Select region for VTK output   ') # XXX padding
        enabled = any(x in solids_models for x in ('DEM','PIC','CGP'))
        item = get_combobox_item(rp.combobox_2, 1)
        if not hasattr(item, 'tooltip0'):
            item.tooltip0 = item.toolTip()
        if not enabled:
            item.setToolTip(item.tooltip0 + "<br>Not available for TFM solids")
        else:
            item.setToolTip(item.tooltip0)
        set_item_enabled(item, enabled) # Particle
        if not enabled:
            rp.combobox_2.setCurrentIndex(0)

    def output_delete_solids_phase(self, phase_index):
        """adjust vtk_current_solid when solids phase deleted"""
        if (self.vtk_current_solid is not None
            and self.vtk_current_solid >= phase_index):
            self.vtk_current_solid -= 1
            if self.vtk_current_solid == 0:
                self.vtk_current_solid = None


    def output_cancel_add(self):
        ui = self.ui.output

        for item in (ui.toolbutton_add,
                     ui.tablewidget_vtk_regions):
            item.setEnabled(True)

        if get_selected_row(ui.tablewidget_vtk_regions) is not None:
            ui.toolbutton_delete.setEnabled(True)

        # always renable - used to block all widgets when the region selection
        # popup is active.
        ui.bottom_frame_vtk.setEnabled(True)


    def output_add_regions(self):
        # Interactively add regions to define VTK Output
        ui = self.ui.output
        rp = self.regions_popup
        self.output_cancel_add() # Re-enable input widgets
        selections = rp.get_selection_list()
        if not selections:
            return
        output_type = VTK_DATA_TYPES[rp.combobox_2.currentIndex()]
        self.output_add_regions_1(selections, output_type=output_type, indices=None, autoselect=True)
        self.output_setup_current_tab() # Update the widgets


    def output_add_regions_1(self, selections, output_type=None, indices=None, autoselect=False):
        # Used by both interactive and load-time add-region handlers
        ui = self.ui.output

        if output_type is None:
            output_type = 'C' #Cell data is default

        if self.output_region_dict is None:
            self.output_region_dict = self.ui.regions.get_region_dict()

        tw = ui.tablewidget_vtk_regions
        nrows = tw.rowCount()
        tw.setRowCount(nrows+1)
        def make_item(val):
            item = QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item
        item = make_item('+'.join(selections))

        if indices is None: # interactive
            indices = [None] * len(selections)
        else: # loading file
            assert len(selections) == len(indices)

        for (i, region_name) in enumerate(selections):
            idx = indices[i]
            if idx is None:
                idx = self.output_find_index()
                indices[i] = idx
            self.vtk_outputs[idx] = {'region': region_name}
            region_data = self.output_region_dict.get(region_name)
            if region_data is None: # ?
                self.warn("no data for region %s" % region_name)
                continue
            self.output_set_vtk_region_keys(idx, region_data, output_type)
            #self.output_region_dict[region_name]['available'] = False # Mark as in-use
        item.setData(UserRole, (tuple(indices), tuple(selections)))
        tw.setItem(nrows, COLUMN_REGION, item)

        filebase = None if indices is None else self.project.get_value('vtk_filebase',
                                                                       args=[indices[0]],
                                                                       default='')
        item = make_item(filebase)
        tw.setItem(nrows, COLUMN_FILENAME, item)
        self.add_tooltip(item, key='vtk_filebase')

        name = ('Cell data' if output_type == 'C'
                else 'Particle data' if output_type == 'P'
                else 'Geometry data' if output_type == 'G'
                else 'Force chain data' if output_type == 'F'
                else '???')
        item = make_item(name)
        item.setToolTip('Cell data (VTU file)'  if output_type == 'C'
                        else 'Particle data (VTP file)' if output_type == 'P'
                        else 'Geometry data (VTU file)' if output_type == 'G'
                        else 'Force chain data' if output_type == 'F'
                        else '')
        tw.setItem(nrows, COLUMN_TYPE, item)

        item = make_item(','.join(map(str, indices)))
        tw.setItem(nrows, COLUMN_ID, item)
        #self.fixup_output_table(tw) # avoid dup. call

        if autoselect:
            tw.setCurrentCell(nrows, 0)


    def output_find_index(self):
        # Fill holes (?)
        n = 1
        while n in self.vtk_outputs:
            n += 1
        return n


    def output_delete_regions(self):
        ui = self.ui.output
        tw = ui.tablewidget_vtk_regions
        row = get_selected_row(tw)
        if row is None: # No selection
            return

        # Unset keywords
        kwlist = list(self.project.keywordItems())
        for kw in kwlist:
            key, args = kw.key, kw.args
            if key.startswith('vtk_') and args and args[0] in self.vtk_current_indices:
                self.unset_keyword(key, args=args)

        for r in self.vtk_current_regions:
            if r in self.output_region_dict:
                self.output_region_dict[r]['available'] = True

        for i in self.vtk_current_indices:
            del self.vtk_outputs[i]

        self.vtk_current_regions = []
        self.vtk_current_indices = []

        tw.removeRow(row)
        #self.fixup_output_table(tw)
        self.setup_output_vtk_tab()
        #self.update_nav_tree()


    def vtk_regions_to_str(self):
        """convert VTK output region definitions to saveable form"""
        ui = self.ui.output
        tw = ui.tablewidget_vtk_regions
        data = [tw.item(i, 0).data(UserRole)
                for i in range(tw.rowCount())]
        return JSONEncoder().encode(data)


    def vtk_regions_from_str(self, s):
        if not s:
            return
        data = JSONDecoder().decode(s)
        for (indices, regions) in data:
            if not indices:
                continue # should not get empty tuple
            # vtk_data (output type) keyword should be set already when we call this
            output_type = self.project.get_value('vtk_data', args=[indices[0]], default='C')
            self.output_add_regions_1(regions, output_type=output_type,
                                      indices=indices, autoselect=False)


    def vtk_extract_regions(self):
        if self.vtk_outputs:
            # We assume that output regions have been initialized correctly
            # from mfix_gui_comments.
            # TODO: verify that there is an output region for each output
            return

        if self.output_region_dict is None:
            self.output_region_dict = self.ui.regions.get_region_dict()

        # TODO: if we wanted to be fancy, we could find regions where
        # output values matched, and merge into a new output region.  That
        # is only needed for projects created outside the GUI (otherwise
        # we have already stored the output regions).  Also would be nice
        # to offer a way to split compound regions.
        for vtk in self.project.vtks:

            d = vtk.keyword_dict
            extent = [d.get('vtk_'+k, None) for k in ('x_w', 'y_s', 'z_b',
                                                      'x_e', 'y_n', 'z_t')]
            extent = [0 if x is None else x.value for x in extent]
            #if any (x is None for x in extent):
            #    self.warn("vtk output %s: invalid extents %s" %
            #               (vtk.ind, extent))
            #    continue

            output_type = d.get('vtk_data') # Returns a keyword object or None, not a value
            if output_type is None:
                output_type = 'C' # Default

            else:
                output_type = output_type.value # get value from keyword object
                if output_type not in ('C', 'P'):
                    self.error("Invalid type '%s' for VTK output %s"
                               % (output_type, vtk.ind), popup=True)
                    continue

            for (region_name, data) in self.output_region_dict.items():
                ext2 = [0 if x is None else x for x in
                        (data.get('from', []) + data.get('to', []))]
                if ext2 == extent: # Don't need to check 'available' here since
                            # Regions can define multiple VTK regions.
                    self.output_add_regions_1([region_name], indices=[vtk.ind],
                                              output_type=output_type,
                                              autoselect=False)
                    break
            else:
                self.warn("vtk output %s: could not match defined region %s" %
                          (vtk.ind, extent))
                kwlist = list(self.project.keywordItems())
                for kw in kwlist:
                    key, args = kw.key, kw.args
                    if key.startswith('vtk_') and args and args[0]==vtk.ind:
                        self.unset_keyword(key, args=args)

    def usr_regions_to_str(self):
        """convert USR output region definitions to saveable form"""
        ui = self.ui.output
        tw = ui.tablewidget_usr_regions
        data = [tw.item(i, 0).data(UserRole)
                for i in range(tw.rowCount())]
        return JSONEncoder().encode(data)


    def usr_regions_from_str(self, s):
        if not s:
            return
        ui = self.ui.output
        tw = ui.tablewidget_usr_regions
        data = JSONDecoder().decode(s)
        for (index, region) in data:
            if 1 <= index <= 5:
                self.usr_outputs[index] = {'region': region}
            else:
                self.error("Invalid index %s" % index, popup=True)
        self.update_usr_regions_table()


    def usr_extract_regions(self):
        ui = self.ui.output
        tw = ui.tablewidget_usr_regions
        if self.usr_outputs:
            # We assume that output regions have been initialized correctly
            # from mfix_gui_comments.
            # TODO: verify that there is an output region for each output
            return

        if self.output_region_dict is None:
            self.output_region_dict = self.ui.regions.get_region_dict()

        indices = [i for i in range(1,6) if any(self.project.get_value(k,args=[i]) is not None
                                                for k in USR_KEYS)]
        for i in indices:

            extent = [self.project.get_value('usr_'+k, args=[i])
                      for k in ('x_w', 'y_s', 'z_b',
                                'x_e', 'y_n', 'z_t')]
            if extent == [None, None, None, None, None, None]:
                region_name = 'None'
            else:
                for (region_name, data) in self.output_region_dict.items():
                    ext2 = [0 if x is None else x for x in
                            (data.get('from', []) + data.get('to', []))]
                    if ext2 == extent:
                        break
            self.usr_outputs[i] = {'region': region_name}
        self.update_usr_regions_table()


    def update_usr_regions_table(self):
        ui = self.ui.output
        tw = ui.tablewidget_usr_regions
        tw.setRowCount(len(self.usr_outputs))
        def make_item(val):
            item = QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item
        row = 0
        for i in self.usr_outputs:
            region = self.usr_outputs[i].get('region', 'None')
            tw.setItem(row, 0, make_item(region))
            usr_dt = self.project.get_value('usr_dt', args=[i])
            tw.setItem(row, 1, make_item(usr_dt))
            tw.setItem(row, 2, make_item(i))
            tw.item(row, 0).setData(UserRole, (i, region))
            row += 1
        self.fixup_output_table(tw)


    def output_logs_to_str(self):
        return JSONEncoder().encode(self.output_logs)

    def output_logs_from_str(self, s):
        if not s:
            return
        data = JSONDecoder().decode(s)
        for (k, v) in data.items():
            self.output_logs[k] = v


    def init_output_spx_tab(self):
        ui = self.ui.output
        gb = ui.groupbox_print_des_data # aka "Write ASCII particle data"
        gb.clicked.connect(lambda val: self.update_keyword('print_des_data', val))
        self.add_tooltip(gb, key='print_des_data')

        cb = ui.combobox_des_output_type
        cb.currentIndexChanged.connect(lambda val: self.update_keyword('des_output_type', DES_OUTPUT_TYPES[val]))
        self.add_tooltip(cb, key='des_output_type')


    def update_resid_string(self):
        ui = self.ui.output
        enable = not(self.project.get_value('group_resid', default=False))
        ui.groupbox_residuals.setEnabled(enable)
        # Only allow user to add resid_strings in sequential order
        key = 'resid_string'
        if enable:
            indices = [getattr(ui, 'combobox_resid_string_%s'%i).currentIndex()
                       for i in range(1, 9)]
            i1 = [i for i in indices if i>0]
            for (i,j) in enumerate(i1, 1):
                cb = getattr(ui, 'combobox_resid_string_%s'%i)
                cb.setCurrentIndex(j)
                cb.setEnabled(True)
                val = cb.currentText().split(':')[0]
                self.update_keyword(key, val, args=[i])
            i = 1+len(i1)
            if i < 9:
                cb = getattr(ui, 'combobox_resid_string_%s'%i)
                cb.setCurrentIndex(0)
                self.unset_keyword(key, args=[i])
                cb.setEnabled(True)
            i += 1
            while i < 9:
                cb = getattr(ui, 'combobox_resid_string_%s'%i)
                cb.setCurrentIndex(0)
                self.unset_keyword(key, args=[i])
                cb.setEnabled(False)
                i += 1
            used = set(getattr(ui, 'combobox_resid_string_%s'%i).currentIndex()
                       for i in range(1,9))
            for i in range(1, 9):
                cb = getattr(ui, 'combobox_resid_string_%s'%i)
                for j in range(len(cb)):
                    get_combobox_item(cb, j).setEnabled(j==0 # <None> always enabled
                                                        or j==cb.currentIndex()
                                                        or j not in used)

        else:
            for i in range(1, 9):
                self.unset_keyword("resid_string", args=[i])

    def init_output_log_tab(self):
        ui = self.ui.output
        for which in 'solid_inventory', 'solver_output', 'dt', 'nit':
            gb = getattr(ui, 'groupbox_save_%s' % which)
            gb.clicked.connect(lambda checked, which=which: self.handle_log_file_enable(which, checked))

            le = getattr(ui, 'lineedit_%s_filename' % which)
            le.key = which
            le.value_updated.connect(self.handle_log_file_name)

            for mode in 'overwrite', 'append', 'increment':
                rb = getattr(ui, 'radiobutton_%s_%s' % (which, mode))
                rb.clicked.connect(lambda checked, which=which, mode=mode: self.handle_log_file_mode(which, mode))


    def handle_log_file_enable(self, which, enable):
        if which not in self.output_logs:
            self.output_logs[which] = [enable, '', 'overwrite']
        else:
            self.output_logs[which][0] = enable
        self.set_unsaved_flag()

    def handle_log_file_name(self, widget, value_dict, args):
        # TODO check for valid filename
        k, v = value_dict.popitem()
        if k not in self.output_logs:
            self.output_logs[k] = [True, v, 'overwrite']
        else:
            self.output_logs[k][1] = v
        self.set_unsaved_flag()

    def handle_log_file_mode(self, which, mode):
        if which not in self.output_logs:
            self.output_logs[which] = [True, '', mode]
        else:
            self.output_logs[which][2] = mode
        self.set_unsaved_flag()

    def init_output_residuals_tab(self):
        ui = self.ui.output
        ui.checkbox_keyword_group_resid.post_update = self.update_resid_string
        key = 'resid_string'
        for i in range(1, 9):
            cb = getattr(ui, 'combobox_resid_string_%s'%i)
            cb.activated.connect(self.update_resid_string)
            self.add_tooltip(cb, key)
        gb = ui.groupbox_save_residuals
        gb.clicked.connect(lambda checked: self.handle_log_file_enable('residuals', checked))

        le = ui.lineedit_residuals_filename
        le.key = 'residuals'
        le.value_updated.connect(self.handle_log_file_name)
        for mode in 'overwrite', 'append', 'increment':
            rb = getattr(ui, 'radiobutton_residuals_%s' % mode)
            rb.clicked.connect(lambda checked, mode=mode: self.handle_log_file_mode('residuals', mode))


    def output_enable_vtk(self, enabled):
        self.update_keyword('write_vtk_files', enabled)
        self.setup_output() # enable/disable gui widgets


    def output_enable_spx(self, enabled):
        ui = self.ui.output
        if enabled:
            ui.pushbutton_spx.setEnabled(True)

            res_dt = self.project.get_value('res_dt', default=1.0)
            default = max(1.0, res_dt)
            for i in range(1, MAX_SP+1): # Only set keys not already set
                if self.project.get_value('spx_dt', args=[i]) is None:
                    # Try to restore value from disabled lineedit (use retained_key here?)
                    le = getattr(ui, 'lineedit_keyword_spx_dt_args_%s' % i)
                    val = le.value
                    if val == '' or val is None or val == 0:
                        val = default
                    self.update_keyword('spx_dt', val, args=[i])
        else:
            ui.pushbutton_spx.setEnabled(False)
            for i in range(1, MAX_SP+1):
                self.unset_keyword('spx_dt', args=[i])

    def setup_output(self, allow_disabled_tab=False):
        ui = self.ui.output
        # Grab a fresh copy, may have been updated
        self.output_region_dict = self.ui.regions.get_region_dict()

        # Mark regions which are in use (this gets reset each time we get here)
        #for (i, data) in self.vtk_outputs.items():
        #    region = data['region']
        #    if region in self.output_region_dict:
        #        self.output_region_dict[region]['available'] = False


        #Write binary Single Precision files (SPx)
        #    No keyword association
        #    Enables SPx tab
        #    Backwards compatibility: Enabled if any SPx time values are specified

        spx_dt_specified = any(self.project.get_value('spx_dt', args=[i]) is not None
                               for i in range(1, MAX_SP+1)) # Note, enabled in template! XXX Jordan?

        activate_spx = spx_dt_specified
        ui.checkbox_spx.setEnabled(True)
        ui.checkbox_spx.setChecked(activate_spx)
        ui.pushbutton_spx.setEnabled(activate_spx)

        vtk_enabled = self.project.get_value('write_vtk_files', default=False)
        ui.pushbutton_vtk.setEnabled(vtk_enabled)

        # TODO don't stay on disabled tab
        self.output_setup_current_tab()


    def setup_output_tab(self, tabnum):
        if tabnum == BASIC_TAB:
            self.setup_output_basic_tab()
        elif tabnum == VTK_TAB:
            self.setup_output_vtk_tab()
        elif tabnum == SPX_TAB:
            self.setup_output_spx_tab()
        elif tabnum == RESIDUALS_TAB:
            self.setup_output_residuals_tab()
        elif tabnum == LOG_TAB:
            self.setup_output_log_tab()
        elif tabnum == USER_TAB:
            self.setup_output_usr_tab()
        else:
            raise ValueError(tabnum)


    def output_setup_current_tab(self):
        self.setup_output_tab(self.output_current_tab)


    def setup_output_basic_tab(self):
        #Basic (tab)
        ui = self.ui.output
        #
        #Specify Restart/checkpoint write interval
        #    Specification always available (required)
        #    Sets keyword RES_DT
        #    DEFAULT 1.0
        key = 'res_dt'
        res_dt = self.project.get_value(key, default=1.0)

        # 'reverse constraint', res_dt must be less than a bunch of other keys
        # (which must be greater than it)
        #m = min(safe_float(self.project.get_value('spx_dt', default=1.0, args=[i]))
        #        for i in range(1,MAX_SP+1))
        #if self.project.get_value('res_backups', default=0) > 0:
        #    m = min(m, safe_float(self.project.get_value('res_backup_dt', default=1.0)))
        #ui.lineedit_keyword_res_dt.max = m
        #  Note, this seems like a good idea but makes it hard to change values

        #if res_dt > m:
        #    res_dt = m

        #    self.update_keyword(key, res_dt)

        #Specify the number of backup copies
        #    Specification always available
        #    Sets keyword RES_BACKUPS
        #    DEFAULT 0
        #    Error check: value is greater than or equal to 0

        #Specify the backup interval
        #    Specification only available if RES_BACKUPS > 0
        enabled = self.project.get_value('res_backups', default=0) > 0
        #    Sets keyword RES_BACKUP_DT
        key = 'res_backup_dt'
        #    DEFAULT 1.0
        default = 1.0
        #    Error check: value must be greater than or equal to RES_DT
        ui.lineedit_keyword_res_backup_dt.min = res_dt
        for item in (ui.label_res_backup_dt,
                     ui.lineedit_keyword_res_backup_dt,
                     ui.label_res_backup_dt_units):
            item.setEnabled(enabled)
            if enabled:
                val = self.project.get_value(key)
                if val is None:
                    val = self.get_retained_keyword(key, default=default)
                    if val < res_dt:
                        val = res_dt
                    self.update_keyword(key, val)
            else:
                self.retain_keyword(key)
                ui.lineedit_keyword_res_backup_dt.setText('')
                self.unset_keyword(key)

        #Enable VTK output
        #    Specification always available
        #    Sets keyword WRITE_VTK_FILES
        #    DEFAULT .FALSE.
        #    Enables VTK tab
        # (handled by keyword widget and post_update)
        write_vtk_files = self.project.get_value('write_vtk_files', default=False)
        ui.groupbox_write_vtk_files.setChecked(write_vtk_files)
        ui.pushbutton_vtk.setEnabled(write_vtk_files)

        #Enable time-dependent VTK files
        #    Specification only if WRITE_VTK_FILES = .TRUE.
        enabled = bool(write_vtk_files)

        #    Sets keyword TIME_DEPENDENT_FILENAME
        key = 'time_dependent_filename'
        #    DEFAULT value .TRUE.
        default = True
        cb = ui.checkbox_keyword_time_dependent_filename
        #cb.setEnabled(enabled)        #(handled by checkable groupbox)
        value = self.project.get_value(key)
        self.add_tooltip(cb, key, value=True if value is None else value)
        if enabled:
            if value is None:
                value = default
                self.update_keyword(key, value)
        else:
            self.unset_keyword(key)
        ui.pushbutton_vtk.setEnabled(enabled)

        #Specify VTK Directory
        #    Specification only if WRITE_VTK_FILES = .TRUE.
        #    Sets keyword VTU_DIR
        #    No default (empty string)
        key = 'vtu_dir'
        enabled = bool(write_vtk_files)
        value = self.project.get_value(key, default='')
        #for item in (ui.label_vtu_dir, ui.lineedit_keyword_vtu_dir):
        #    item.setEnabled(enabled)        #(handled by checkable groupbox)
        if enabled:
            if value is None or value == '':
                value = ui.lineedit_keyword_vtu_dir.value # saved value in GUI
                if value is None:
                    value = default
                self.update_keyword(key, value)
        else:
            self.unset_keyword(key)
        ui.pushbutton_vtk.setEnabled(enabled)

        gb = ui.groupbox_write_part_out
        enabled = self.project.solver in (DEM,CGP)
        write_part_out = enabled and self.project.get_value('write_part_out', default=False)
        gb.setEnabled(enabled)
        gb.setChecked(write_part_out)
        if not enabled:
            gb.setToolTip(gb.tooltip0 + '<br>Available for DEM/CGP models only.')
        else:
            gb.setToolTip(gb.tooltip0)

        # Filter particle data in particle_data.out
        gb = ui.groupbox_filter_particles
        # Set groupbox to correct state
        if ui.groupbox_filter_particles_checked is None:
            ui.groupbox_filter_particles_checked = any((k.key.startswith('part_out_') and k.key != 'part_out_zero_vel')
                                                       for k in self.project.keywordItems())
        gb.setChecked(enabled and ui.groupbox_filter_particles_checked)
        energy_eq = self.project.get_value('energy_eq', default=True)
        for w in (ui.combobox_filter_temp,
                  ui.label_filter_temp,
                  ui.label_filter_temp_units,
                  ui.lineedit_keyword_part_out_temp_min,
                  ui.lineedit_keyword_part_out_temp_max):
            if not hasattr(w, 'tooltip0'):
                w.tooltip0 = w.toolTip()
            w.setEnabled(energy_eq)
            w.setToolTip(w.tooltip0 + "<br>Requires energy equations." if not w.isEnabled()
                         else w.tooltip0)

        last = ui.lineedit_keyword_part_out_temp_max
        # Dynamic widgets for phase, species and usr vars
        layout = ui.groupbox_filter_particles.layout()
        n_species = max((self.project.get_value('nmax_s', default=0, args=[p])
                         for p in range(1, 1+len(self.solids))),
                        default=0)
        key = 'part_out_x_s_exclude'
        if key not in ui.dynamic_widgets.keys():
            ui.dynamic_widgets[key] = []
        dws = ui.dynamic_widgets[key]
        while len(dws) < n_species:
            n = 1+len(dws)
            cb = QComboBox()
            cb.addItem("Include")
            cb.addItem("Exclude")
            cb.key = key
            cb.args = [n]
            cb.currentIndexChanged.connect(lambda idx, w=cb:
                                           self.output_handle_part_out_exclude(w, idx))
            self.add_tooltip(cb, cb.key)
            layout.addWidget(cb)
            QWidget.setTabOrder(last, cb)
            last = cb

            l1 = QLabel("Species %s mass fraction" % n)
            l1.key = 'part_out_x_s_min'
            l1.args = [n]
            self.add_tooltip(l1, l1.key)
            layout.addWidget(l1)
            le1 = LineEdit()
            le1.dtype = float
            le1.key = 'part_out_x_s_min'
            le1.args = [n]
            le1.min = 0.0
            le1.max = 1.0
            le1.value_updated.connect(self.project.submit_change)
            le1.post_update = self.setup_output_basic_tab
            self.add_tooltip(le1, le1.key)
            layout.addWidget(le1)
            QWidget.setTabOrder(last, le1)
            last = le1
            le2 = LineEdit()
            le2.dtype = float
            le2.key = 'part_out_x_s_max'
            le2.args = [n]
            le2.min = 0.0
            le2.max = 1.0
            le2.value_updated.connect(self.project.submit_change)
            le2.post_update = self.setup_output_basic_tab
            self.add_tooltip(le2, le2.key)
            layout.addWidget(le2)
            QWidget.setTabOrder(last, le2)
            last = le2
            l2 = QLabel('') # Unit placeholder
            layout.addWidget(l2)
            dws.append((cb, l1, le1, le2, l2))

        while len(dws) > n_species:
            for kw in ('part_out_x_s_min', 'part_out_x_s_max', 'part_out_x_s_exclude'):
                self.unset_keyword(kw, args=[len(dws)])
            row = dws.pop()
            for w in row:
                layout.removeWidget(w)
                w.deleteLater()

        def as_str(x):
            return '' if x is None else str(x)

        for i in range(1, n_species+1):
            val = self.project.get_value('part_out_x_s_min', args=[i])
            dws[i-1][2].setText(as_str(val))
            val = self.project.get_value('part_out_x_s_max', args=[i])
            dws[i-1][3].setText(as_str(val))

        # User vars
        n_vars = self.project.get_value('des_usr_var_size', default=0)
        key = 'part_out_usr_var_exclude'
        if key not in ui.dynamic_widgets.keys():
            ui.dynamic_widgets[key] = []
        dws = ui.dynamic_widgets[key]
        while len(dws) < n_vars:
            n = 1+len(dws)
            cb = QComboBox()
            cb.addItem("Include")
            cb.addItem("Exclude")
            cb.key = key
            cb.args = [n]
            cb.currentIndexChanged.connect(lambda idx, w=cb:
                                           self.output_handle_part_out_exclude(w, idx))
            self.add_tooltip(cb, cb.key)
            layout.addWidget(cb)
            QWidget.setTabOrder(last, cb)
            last = cb
            l1 = QLabel("User scalar %s" % n)
            l1.key = 'part_out_usr_var_min'
            l1.args = [n]
            self.add_tooltip(l1, l1.key)
            layout.addWidget(l1)
            le1 = LineEdit()
            le1.dtype = float
            le1.key = 'part_out_usr_var_min'
            le1.args = [n]
            le1.min = 0.0
            le1.max = 1.0
            le1.value_updated.connect(self.project.submit_change)
            le1.post_update = self.setup_output_basic_tab
            self.add_tooltip(le1, le1.key)
            layout.addWidget(le1)
            QWidget.setTabOrder(last, le1)
            last = le1
            self.add_tooltip(le1, le1.key)
            le2 = LineEdit()
            le2.dtype = float
            le2.key = 'part_out_usr_var_max'
            le2.args = [n]
            le2.min = 0.0
            le2.max = 1.0
            le2.value_updated.connect(self.project.submit_change)
            le2.post_update = self.setup_output_basic_tab
            self.add_tooltip(le2, le2.key)
            layout.addWidget(le2)
            QWidget.setTabOrder(last, le2)
            last = le2
            l2 = QLabel('') # Unit placeholder
            layout.addWidget(l2)

            dws.append((cb, l1, le1, le2, l2))

        while len(dws) > n_vars:
            for kw in ('part_out_usr_var_min', 'part_out_usr_var_max', 'part_out_usr_var_exclude'):
                self.unset_keyword(kw, args=[len(dws)])
            row = dws.pop()
            for w in row:
                layout.removeWidget(w)
                w.deleteLater()

        for i in range(1, n_vars+1):
            val = self.project.get_value('part_out_usr_var_min', args=[i])
            dws[i-1][2].setText(as_str(val))
            val = self.project.get_value('part_out_usr_var_max', args=[i])
            dws[i-1][3].setText(as_str(val))

        key = 'part_out_phase'
        n_phases = len(self.solids)
        if key not in ui.dynamic_widgets.keys():
            ui.dynamic_widgets[key] = []
        dws = ui.dynamic_widgets[key]
        while len(dws) < n_phases:
            n = 1+len(dws)
            cb = QComboBox()
            cb.addItem("Include")
            cb.addItem("Exclude")
            cb.key = key
            cb.args = [n]
            cb.currentIndexChanged.connect(lambda idx, w=cb:
                               self.output_handle_part_out_phase(w, idx))
            self.add_tooltip(cb, cb.key)
            row = layout.rowCount()
            layout.addWidget(cb, row, 0, 1, 1)
            QWidget.setTabOrder(last, cb)
            last = cb

            l = QLabel('')
            l.key = key
            l.args = [n]
            self.add_tooltip(l, key)
            layout.addWidget(l, row, 1, 1, -1)
            dws.append((cb, l))

        while len(dws) > n_phases:
            self.unset_keyword('part_out_phase', args=[len(dws)])
            row = dws.pop()
            for w in row:
                layout.removeWidget(w)
                w.deleteLater()

        for (i, name) in enumerate(self.solids):
            dws[i][1].setText(name)

        # Set comboboxes to correct state
        for w in widget_iter(ui.groupbox_filter_particles):
            if not isinstance(w, QComboBox):
                continue
            key = w.key
            args = w.args
            default = not key.endswith('_exclude')
            val = self.project.get_value(key, args=args, default=default)
            w.setCurrentIndex(int(val != default))

        # Set groupbox to correct state, do this last to handle dynamic_widgets
        self.output_handle_groupbox_filter_particles(gb.isChecked(), use_retained=False)
        ui.groupbox_filter_particles.setToolTip('Select which particles to write in particle_output.dat')


    def output_handle_groupbox_filter_particles(self, idx, use_retained=True):
        ui = self.ui.output
        ui.groupbox_filter_particles_checked = idx
        for w in widget_iter(ui.groupbox_filter_particles):
            if isinstance(w, (QComboBox, QLabel, QLineEdit)):
                w.setVisible(idx)
                if isinstance(w, QLabel):
                    continue
                if not idx:
                    if use_retained:
                        val = self.project.get_value(w.key, args=w.args)
                        if val is not None:
                            self.retain_keyword(w.key, args=w.args)
                        else:
                            self.clear_retained_keyword(w.key, args=w.args)
                        self.unset_keyword(w.key, args=w.args)
                else:
                    if use_retained:
                        val = self.get_retained_keyword(w.key, args=w.args)
                        if val is not None:
                            self.update_keyword(w.key, val, args=w.args)
                    default=self.keyword_doc.get(w.key, {}).get('initpython')
                    val = self.project.get_value(w.key, args=w.args, default=default)
                    if isinstance(w, QComboBox):
                        w.setCurrentIndex(int(val != default))
                    elif isinstance(w, LineEdit):
                        w.updateValue(w.key, val, args=w.args)


    def output_handle_part_out_exclude(self, w, idx):
        key = w.key
        args = getattr(w, 'args', None)
        if idx == 0: # Include
            self.unset_keyword(key, args=args) # Don't save default False
            #self.update_keyword(key, False, args=args)
        else:
            self.update_keyword(key, True, args=args)


    def output_handle_part_out_phase(self, w, idx):
        key = w.key
        args = getattr(w, 'args', None)
        if idx: # Exclude
            self.update_keyword(key, False, args=args)
        else: # Don't save default True
            #self.update_keyword(key, True, args=args)
            self.clear_retained_keyword(key, args=args)
            self.unset_keyword(key, args=args)

    def setup_output_spx_tab(self):
        """SPx (tab) Note: Technically, MFiX will now permit a user to mix-and-match
        the SPx output files meaning that some can be written and others not.
        However, this is likely to break the ParaView reader. Therefore, if the
        “Write binary SPx” checkbox is enabled, output is required for all SPx
        files. Otherwise, all should remain unspecified to skip writing the SPx
        files.
        """

        ui = self.ui.output

        #Write interval for gas volume fraction
        #    Sets keyword SPX_DT(1)
        #    DEFAULT 1.0
        #    Required if SPx data is enabled.
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for gas and solids pressure
        #    Sets keyword SPX_DT(2)
        #    Required if SPx data is enabled.
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for gas velocity
        #    Sets keyword SPX_DT(3)
        #    Required if SPx data is enabled.
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for solids velocity
        #    Sets keyword SPX_DT(4)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for solids bulk density
        #    Sets keyword SPX_DT(5)
        #    Required if SPx data is enabled.
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for gas and solids temperature
        #    Sets keyword SPX_DT(6)
        #    Required if SPx data is enabled and solving any energy equations.
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for gas and solids mass fractions
        #    Sets keyword SPX_DT(7)
        #    Required if SPx data is enabled and solving any species equations.
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for granular temperature
        #    Sets keyword SPX_DT(8)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for user defined scalars
        #    Sets keyword SPX_DT(9)
        #    Required if SPx data is enabled and solving any user defined scalar equations
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for reaction rates
        #    Required if SPx data is enabled and NRR > 0 (see below)
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        #Write interval for turbulence quantities
        #    Sets keyword SPX_DT(11)
        #    Required if SPx data is enabled and TURBULENCE_MODEL = “K_EPSILON”
        #    DEFAULT 1.0
        #    Error check: value must be greater than or equal to RES_DT

        res_dt = self.project.get_value('res_dt', default=1.0)

        key = 'spx_dt'
        default = 1.0
        for sp in range(1, MAX_SP+1):
            le = getattr(ui, 'lineedit_keyword_spx_dt_args_%s' % sp)
            le.min = res_dt
            val = self.project.get_value(key, args=[sp])
            if val is None:
                val = default
                if val < res_dt:
                    val = res_dt
                self.update_keyword(key, val, args=[sp])

        #Write ASCII particle data
        #    Selection only available if DEM, CGP or PIC solids
        #    Sets keyword PRINT_DES_DATA
        #    DEFAULT .TRUE.
        key = 'print_des_data'
        solids_models = set(self.project.get_value('solids_model', args=[i])
                            for i in range(1, len(self.solids)+1))
        enabled = any(x in solids_models for x in ('DEM','PIC','CGP'))
        gb = ui.groupbox_print_des_data
        if enabled:
            checked = self.project.get_value(key, default=True)
        else:
            checked = self.project.get_value(key, default=False)
        gb.setEnabled(enabled)
        gb.setChecked(checked)

        #Specify VTP Directory
        #    Specification only if PRINT_DES_DATA = .TRUE.
        #    Sets keyword VTP_DIR
        #    No default (empty string)
        #Select particle data format
        #
        #Selection only available if DEM, CGP or PIC solids and PRINT_DES_DATA = .TRUE.
        #  (note, containing groupbox is disabled if this condition is not met)
        #Sets keyword DES_OUTPUT_TYPE
        #Available Selections
        #  ParaView - VTK/.vtp [DEFAULT]
        #  Tecplot - .dat
        cb = ui.combobox_des_output_type
        key = 'des_output_type'
        default = DES_OUTPUT_TYPES[0] # "PARAVIEW"
        val = self.project.get_value(key, default)
        if val not in DES_OUTPUT_TYPES:
            val = default
            self.update_keyword(key, val)
        cb.setCurrentIndex(DES_OUTPUT_TYPES.index(val))



    def output_default_vtk_filebase(self, region_names):
        # Construct default value for VTK_FILEBASE,
        # replacing possibly troublesome characters
        key = 'vtk_filebase'
        val = '+'.join(region_names)
        for c in ': /*?': # is this enough?
            val = val.replace(c, '_')
        indices = self.project.get_key_indices(key)
        names = set(self.project.get_value(key, args=i) for i in indices)
        val0 = val
        count = 0
        while val in names:
            count += 1
            val = '%s_%s' % (val0, count)
        return val


    def setup_output_vtk_tab(self):
        ui = self.ui.output
        self.fixup_output_table(ui.tablewidget_vtk_regions)

        row = get_selected_row(ui.tablewidget_vtk_regions)
        # Autoselect if only 1 row
        if row is None and ui.tablewidget_vtk_regions.rowCount() == 1:
            row = 0
            ui.tablewidget_vtk_regions.setCurrentCell(row, 0)
        enabled = (row is not None and ui.input_enabled)
        ui.toolbutton_delete.setEnabled(enabled)
        ui.bottom_frame_vtk.setEnabled(enabled)
        ui.bottom_tab_frame.setEnabled(row is not None) # Allow sub-tab navigation while locked
        for w in (ui.cell_common,
                  ui.subpage_particle,
                  ui.scrollarea_cell):
            w.setEnabled(enabled)

        indices = self.vtk_current_indices
        if not indices:
            ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_CELL)
            #Construct the GUI, even though disabled (species checkboxes)
            self.setup_output_current_subtab()
            return

        V0 = indices[0]
        vtk_data = self.project.get_value('vtk_data', args=[V0], default='C')

        # Note, filebase through nzs are common to cell/particle

        #Specify filename base
        # Specification is required.
        # Sets keyword VTK_FILEBASE(#)
        # DEFAULT region name
        # TODO make sure it's a valid filename
        key = 'vtk_filebase'
        le = ui.lineedit_keyword_vtk_filebase_args_VTK
        tw = ui.tablewidget_vtk_regions
        val = self.project.get_value(key, args=[V0])
        if val is None: # Construct from region name
            val = self.output_default_vtk_filebase(self.vtk_current_regions)
            for V in self.vtk_current_indices:
                self.update_keyword(key, val, args=[V])
        le.updateValue(key, val)
        for i in range(tw.rowCount()):
            data = tw.item(i, 0).data(UserRole)
            indices, names = data
            if V0 in indices:
                tw.item(i, COLUMN_FILENAME).setText(val)

        #Specify write interval
        # Specification is required
        # Sets keyword VTK_DT(#)
        # DEFAULT 1.0 (must write)
        key = 'vtk_dt'
        default = 1.0
        le = ui.lineedit_keyword_vtk_dt_args_VTK
        val = self.project.get_value(key, args=[V0])
        if val is None:
            val = default
            for V in self.vtk_current_indices:
                self.update_keyword(key, val, args=[V])
        le.updateValue(key, val)

        if vtk_data in ('G', 'F'):
            for w in (ui.label_vtk_nxs, ui.label_vtk_nys, ui.label_vtk_nzs,
                      ui.lineedit_keyword_vtk_nxs_args_VTK,
                      ui.lineedit_keyword_vtk_nys_args_VTK,
                      ui.lineedit_keyword_vtk_nzs_args_VTK,
                      ui.label_vtk_slice_tol,
                      ui.lineedit_keyword_vtk_slice_tol_args_VTK,
                      ui.checkbox_keyword_vtk_cutcell_only_args_VTK):
                w.setEnabled(False)
                if isinstance(w, LineEdit):
                    w.setText('')
                if isinstance(w, CheckBox):
                    w.setChecked(False)
        else:
            for w in (ui.label_vtk_nxs, ui.label_vtk_nys, ui.label_vtk_nzs,
                      ui.lineedit_keyword_vtk_nxs_args_VTK,
                      ui.lineedit_keyword_vtk_nys_args_VTK,
                      ui.lineedit_keyword_vtk_nzs_args_VTK,
                      ui.label_vtk_slice_tol,
                      ui.lineedit_keyword_vtk_slice_tol_args_VTK):
                w.setEnabled(True)

            for c in 'xyz':
                #Specify region x[yz]-axis slices
                # Specification always available
                # Sets keyword VTK_NXS(#)
                # DEFAULT 0
                key = 'vtk_n%ss' % c
                default = 0
                le = getattr(ui, 'lineedit_keyword_vtk_n%ss_args_VTK'%c)
                val = self.project.get_value(key, args=[V0])
                if val is None:
                    val = default
                    for V in self.vtk_current_indices:
                        self.update_keyword(key, val, args=[V])
                le.updateValue(key, val)

            #  Specify slice tolerance
            #  Specification always available
            #  Sets keyword VTK_SLICE_TOL(#)
            key = 'vtk_slice_tol'
            le = ui.lineedit_keyword_vtk_slice_tol_args_VTK
            val = self.project.get_value(key, args=[V0])
            le.updateValue(key, val)

            #  Check box: "Only save data in cut cells"
            #  Specification only available for cell data
            #  Sets keyword VTK_CUTCELL_ONLY(#) = .TRUE.
            key = 'vtk_cutcell_only'
            cb = ui.checkbox_keyword_vtk_cutcell_only_args_VTK
            val = self.project.get_value(key, args=[V0], default=False)
            if vtk_data == 'C':
                cb.updateValue(key, val)
                cb.setEnabled(True)
            else:
                cb.setChecked(False)
                cb.setEnabled(False)

        if vtk_data == 'C':
            self.setup_output_vtk_cell()
            ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_CELL)
            ui.scrollarea_cell.ensureVisible(0, 0)
        elif vtk_data == 'P':
            self.setup_output_vtk_particle()
            ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_PARTICLE)
            ui.scrollarea_particle.ensureVisible(0, 0)
        elif vtk_data == 'F':
            #self.setup_output_vtk_force() nothing to set up
            ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_FORCE)
        elif vtk_data == 'G':
            self.setup_output_vtk_geometry()
            ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_GEOMETRY)
        else:
            self.error("Unknown vtk_data %s" % vtk_data)
            ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_CELL)


    def setup_output_vtk_cell(self):
        #Cell data sub-pane

        #There is a need for some hand waving here. Many mfix.dat files may use a different specification
        #for VTK input. There will need to be a way of catching the 'old' format and converting it to this
        #input style.

        ui = self.ui.output
        b = ui.pushbutton_fluid
        b.setText(self.fluid_phase_name)
        font = b.font()
        font.setBold(self.output_current_subtab == FLUID_TAB)
        b.setFont(font)
        w = b.fontMetrics().boundingRect(b.text()).width() + 20
        b.setMaximumWidth(w)

        solids_names = list(self.solids.keys())
        if self.output_saved_solids_names != solids_names:
            # Clear out the old ones
            n_cols = ui.bottom_tab_layout.columnCount()
            for i in range(n_cols-1, 0, -1):
                item = ui.bottom_tab_layout.itemAtPosition(0, i)
                if not item:
                    continue
                widget = item.widget()
                if not widget:
                    continue
                if widget in self.output_pushbuttons_bottom:
                    continue
                ui.bottom_tab_layout.removeWidget(widget)
                widget.setParent(None)
                widget.deleteLater()
            # And make new ones
            for (i, solid_name) in enumerate(solids_names, 1):
                b = QPushButton(text=solid_name)
                w = b.fontMetrics().boundingRect(solid_name).width() + 20
                b.setMaximumWidth(w)
                b.setFlat(True)
                font = b.font()
                font.setBold(self.output_current_subtab == SOLIDS_TAB and i == self.vtk_current_solid)
                b.setFont(font)
                b.pressed.connect(lambda i=i: self.output_change_subtab(SOLIDS_TAB, i))
                ui.bottom_tab_layout.addWidget(b, 0, i)

        # Move the 'Scalar' and other buttons to the right of all solids, if needed
        if len(self.solids) != len(self.output_saved_solids_names):
            for (i, b) in enumerate(self.output_pushbuttons_bottom):
                if b == ui.pushbutton_fluid:
                    continue
                ui.bottom_tab_layout.removeWidget(b)
                ui.bottom_tab_layout.addWidget(b, 0, i+len(self.solids))

        b = ui.pushbutton_scalar
        font = b.font()
        font.setBold(self.output_current_subtab == SCALAR_TAB)
        b.setFont(font)
        nscalar = self.project.get_value('nscalar', default=0)
        enabled = (nscalar > 0)
        b.setEnabled(enabled)

        b = ui.pushbutton_reactions
        font = b.font()
        font.setBold(self.output_current_subtab == REACTIONS_TAB)
        b.setFont(font)
        nrr = self.project.get_value('nrr', default=0)
        enabled = bool((nrr > 0) or self.project.reactions)
        b.setEnabled(enabled)

        b = ui.pushbutton_other
        font = b.font()
        font.setBold(self.output_current_subtab == OTHER_TAB)
        b.setFont(font)
        enabled = True
        b.setEnabled(enabled)

        self.output_saved_solids_names = solids_names

        for (i, solid_name) in enumerate(self.solids.keys(), 1):
            model = self.project.get_value('solids_model', args=[i])
            # All vtk solids keywords require TFM solids, so disable tab completely if not applicable
            b = ui.bottom_tab_layout.itemAtPosition(0, i).widget()
            if model == 'TFM':
                b.setEnabled(True)
                b.setToolTip(None)
            else:
                b.setEnabled(False)
                b.setToolTip("VTK output only supported for TFM solids")

        # make sure underline is in the right place, as # of solids may
        # have changed (lifted from animate_stacked_widget, which we
        # don't want to call here)
        tab = self.output_current_subtab
        line_to = self.output_tab_to_index(tab, self.vtk_current_solid)
        line = ui.line_subtab
        btn_layout = ui.bottom_tab_layout
        if line_to is not None:
            btn_layout.addItem(btn_layout.takeAt(
                btn_layout.indexOf(line)), 1, line_to)

        # Don't stay on disabled tab TODO

        indices = self.vtk_current_indices
        if not indices:
            # Clear inputs?  should have been done in handle_selection
            return

        self.setup_output_current_subtab()


    def output_tab_to_index(self, tab, solid):
        return (0 if tab == FLUID_TAB
                else len(self.solids)+1 if tab == SCALAR_TAB
                else len(self.solids)+2 if tab == REACTIONS_TAB
                else len(self.solids)+3 if tab == OTHER_TAB
                else solid)

    def setup_output_current_subtab(self):
        self.setup_output_subtab(self.output_current_subtab,
                                 self.vtk_current_solid)


    def setup_output_subtab(self, subtab, solid):
        if subtab == FLUID_TAB:
            self.setup_output_fluid_subtab()
        elif subtab == SOLIDS_TAB:
            self.setup_output_solids_subtab(solid)
        elif subtab == SCALAR_TAB:
            self.setup_output_scalar_subtab()
        elif subtab == REACTIONS_TAB:
            self.setup_output_reactions_subtab()
        elif subtab == OTHER_TAB:
            self.setup_output_other_subtab()
        else:
            raise ValueError(subtab)


    def setup_output_fluid_subtab(self):
        # Fluid Phase (tab?)
        ui = self.ui.output
        layout = ui.groupbox_cell_fluid.layout()
        indices = self.vtk_current_indices
        V0 = indices[0] if indices else None
        # NB normally we bail out if indices is None but we want to construct
        #  the fluid species checkboxes (for visual apperarance sake)

        #Enable writing gas volume fraction
        # Selection always available
        # Sets keyword VTK_EP_G(#)
        # DEFAULT value .FALSE.

        #Enable writing gas pressure
        # Requires fluid solver (RO_G0 /= 0.0)
        # Sets keyword VTK_P_G(#)
        # DEFAULT value .FALSE.

        #Enable writing gas velocity vector
        # Requires fluid solver (RO_G0 /= 0.0)
        # Sets keyword VTK_VEL_G(#)
        # DEFAULT value .FALSE.

        #Enable writing gas velocity x-component
        # Requires fluid solver (RO_G0 /= 0.0)
        # Sets keyword VTK_U_G(#)
        # DEFAULT value .FALSE.

        #Enable writing gas velocity y-component
        # Requires fluid solver (RO_G0 /= 0.0)
        # Sets keyword VTK_V_G(#)
        # DEFAULT value .FALSE.

        #Enable writing gas velocity z-component
        # Requires fluid solver (RO_G0 /= 0.0)
        # Sets keyword VTK_W_G(#)
        # DEFAULT value .FALSE.

        #Enable writing gas temperature
        # Requires fluid solver (RO_G0 /= 0.0) and ENERGY_EQ = .TRUE.
        # Sets keyword VTK_T_G(#)
        # DEFAULT value .FALSE.

        #Enable writing gas mixture molecular weight
        # Requires fluid solver (RO_G0 /= 0.0) and defined species
        # Sets keyword VTK_MW_MIX_G(#)
        # DEFAULT value .FALSE.

        #Enable writing turbulent kinetic energy
        #    Requires fluid solver (RO_G0 /= 0.0) and TURBULENCE_MODEL='K_EPSILON'
        #    Sets keyword VTK_K_TURB_G(#)
        #    DEFAULT .FALSE.

        #Enable writing turbulent dissipation
        #    Requires fluid solver (RO_G0 /= 0.0) and TURBULENCE_MODEL='K_EPSILON'
        #    Sets keyword VTK_E_TURB_G(#)
        #    DEFAULT .FALSE.

        for key in ('vtk_ep_g',
                    'vtk_p_g',
                    'vtk_vel_g',
                    'vtk_u_g',
                    'vtk_v_g',
                    'vtk_w_g',
                    'vtk_k_turb_g',
                    'vtk_e_turb_g'):
            cb = getattr(ui, 'checkbox_keyword_%s_args_VTK' % key)
            val = False if V0 is None else self.project.get_value(key, args=[V0], default=False)
            cb.setChecked(val)
            cb.setEnabled(not self.fluid_solver_disabled  or key == 'vtk_ep_g')

        # disable temp if energy_eq disabled
        cb = ui.checkbox_keyword_vtk_t_g_args_VTK
        key = 'vtk_t_g'
        enabled = not self.fluid_solver_disabled and self.project.get_value('energy_eq', default=True)
        if not hasattr(cb, 'tooltip0'):
            cb.tooltip0 = cb.toolTip()
        cb.setEnabled(enabled)
        if not enabled:
            cb.setChecked(False)
            cb.setToolTip(cb.tooltip0 + '<br>Requires energy equations.')
            for V in self.vtk_current_indices:
                self.unset_keyword(key, args=[V])
        else:
            cb.setToolTip(cb.tooltip0)

        # disable mix molecular weight if fluid solver disabled or no species
        cb = ui.checkbox_keyword_vtk_mw_mix_g_args_VTK
        key = 'vtk_mw_mix_g'
        enabled = bool(self.fluid_species and not self.fluid_solver_disabled)
        if not hasattr(cb, 'tooltip0'):
            cb.tooltip0 = cb.toolTip()
        cb.setEnabled(enabled)
        if not enabled:
            cb.setChecked(False)
            cb.setToolTip(cb.tooltip0 + '<br>Fluid solver disabled, or no fluid species.')
            for V in self.vtk_current_indices:
                self.unset_keyword(key, args=[V])
        else:
            cb.setToolTip(cb.tooltip0)

        # disable turbulences if model != K_EPSILON
        turbulence_model = self.project.get_value('turbulence_model', default=DEFAULT_TURBULENCE_MODEL)
        enabled = not self.fluid_solver_disabled and (turbulence_model == 'K_EPSILON')
        for key in ('vtk_k_turb_g', 'vtk_e_turb_g'):
            cb = getattr(ui, 'checkbox_keyword_%s_args_VTK' % key)
            if not hasattr(cb, 'tooltip0'):
                cb.tooltip0 = cb.toolTip()
            cb.setEnabled(enabled)
            if not enabled:
                cb.setChecked(False)
                cb.setToolTip(cb.tooltip0 + '<br>Requires K_EPSILON turbulence model.')
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V])
            else:
                cb.setToolTip(cb.tooltip0)

        #Enable writing gas species N mass fractions (an entry for each defined species)
        #  Requires fluid solver (RO_G0 /= 0.0) and SPECIES_EQ(0) = .TRUE.
        # Requires defined gas phase species
        # Sets keyword VTK_X_G(#,N)
        # DEFAULT value .FALSE.
        key_x = 'vtk_x_g' # mass fraction
        key_y = 'vtk_y_g' # molar fraction
        for key in (key_x, key_y):
            if key not in ui.dynamic_widgets:
                ui.dynamic_widgets[key] = []

        # Interleave two lists
        def interleave(l1, l2):
            l3 = [None] * (len(l1) + len(l2))
            l3[::2] = l1
            l3[1::2] = l2
            return l3

        cbs = interleave(ui.dynamic_widgets[key_x], ui.dynamic_widgets[key_y])

        fluid_species_names = list(self.fluid_species.keys())
        if self.output_saved_fluid_species_names != fluid_species_names:
            self.output_saved_fluid_species_names = fluid_species_names
            n_fluid_species = len(fluid_species_names)
            if len(cbs) > 2*n_fluid_species:
                for (i, cb) in enumerate(cbs[2*n_fluid_species:], 2*n_fluid_species):
                    for V in self.vtk_current_indices:
                        self.unset_keyword(cb.key, args=[V, 1+i//2]) # Should have been done in delete_species
                    layout.removeWidget(cb)
                    cb.setParent(None)
                    cb.deleteLater()
                cbs =  cbs[:2*n_fluid_species]
            while len(cbs) < 2*n_fluid_species:
                n = len(cbs)//2
                for (key, desc) in ((key_x,'mass'), (key_y, 'molar')):
                    cb = CheckBox('%s %s fraction' % (fluid_species_names[n], desc))
                    cb.key = key
                    cb.args = ['VTK', n+1]
                    cb.value_updated.connect(self.project.submit_change)
                    cbs.append(cb)
                    self.add_tooltip(cb, key=cb.key)
                    layout.addWidget(cb)
            ui.dynamic_widgets[key_x] = cbs[::2]
            ui.dynamic_widgets[key_y] = cbs[1::2]

            for i,cb in enumerate(cbs):
                cb.setText('%s %s fraction' % (fluid_species_names[i//2], ['mass','molar'][i%2]))

        # Set checkboxes to correct state
        species_eq = self.project.get_value('species_eq', default=True, args=[0])
        enabled = bool(species_eq and not self.fluid_solver_disabled)
        for (i, cb) in enumerate(cbs):
            if not hasattr(cb, 'tooltip0'):
                cb.tooltip0 = cb.toolTip()
            cb.setEnabled(enabled)
            if enabled:
                val = False if V0 is None else self.project.get_value(cb.key, args=[V0, 1+i//2], default=False)
                cb.setChecked(bool(val))
                cb.setToolTip(cb.tooltip0)
            else:
                cb.setChecked(False)
                for V in self.vtk_current_indices:
                    self.unset_keyword(cb.key, args=[V, 1+i//2])
                cb.setToolTip(cb.tooltip0 + '<br>Requires species equations.')

    def setup_output_reactions_subtab(self):
        ui = self.ui.output
        V0 = self.vtk_current_indices[0] if self.vtk_current_indices else None
        # Dynamically created GUI items
        layout = ui.groupbox_cell_reactions.layout()

        #Enable writing reaction rates - issues/1272
        rxn_info = []  # Name, is_des, index
        des_count = 0
        fluid_count = 0
        rxn_count = 0
        # We're handling fluid and DES reactions in one block,  could
        #  separate them but this seems more like an implementation detail
        #  than something we should expose to users
        for (name,rxn) in self.project.reactions.items():
            des = any(self.project.get_value('solids_model', default='TFM', args=[p]) != 'TFM'
                      for p in rxn.get('phases', []))
            if des:
                des_count += 1
                n = des_count
            else:
                fluid_count += 1
                n = fluid_count
            rxn_info.append((name, des, n))
        rxn_count = len(rxn_info)
        key = 'vtk_fluid_des_rrate' # Not an actual MFIX key
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        if len(cbs) > rxn_count:
            for (i, cb) in enumerate(cbs[rxn_count:], 1+rxn_count):
                for V in self.vtk_current_indices:
                    self.unset_keyword(cb.key, args=[V, cb.idx])
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:rxn_count]
        # Add widgets as needed
        while len(cbs) < rxn_count:
            n = 1+len(cbs)
            (name, des, idx) = rxn_info[n-1]
            cb = CheckBox(name + " rate")
            cb.key = 'vtk_des_rrate' if des else 'vtk_fluid_rrate'
            cb.args = ['VTK', idx]
            cb.idx = idx
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb, n-1, 0)
        # Set all checkboxes to correct state
        for (i, cb) in enumerate(cbs):
            name, des, idx = rxn_info[i]
            cb.setText(name + " rate")
            cb.key = 'vtk_des_rrate' if des else 'vtk_fluid_rrate'
            cb.args = ['VTK', idx]
            cb.idx = idx
            self.add_tooltip(cb, key=cb.key)
            cb.setChecked(self.project.get_value(cb.key, args=[V0, idx], default=False))

        # This section is legacy/deprecated - see issues/1272
        # Requires nRR > 0
        # Sets keyword VTK_RRATE(#, #)
        # DEFAULT value .FALSE.
        nrr = self.project.get_value('nrr', default=0)
        # issues/1329
        enable = (nrr > 0)
        #enable = (nrr > 0) and any(k.value == True and k.key.startswith('vtk_rrate')
        #                           for k in self.project.keywordItems())
        ui.groupbox_cell_reactions_deprecated.setVisible(enable)
        layout = ui.groupbox_cell_reactions_deprecated.layout()
        key = 'vtk_rrate'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        # Remove extra widgets if number decreased
        if len(cbs) > nrr:
            for (i, cb) in enumerate(cbs[nrr:], 1+nrr):
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, i]) # Should have been done already
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:nrr]
        key = 'vtk_rrate_label'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        ws = ui.dynamic_widgets[key] # Values are 2-tuples, (label,lineedit)
        if len(ws) > nrr:
            for (i, (label,le)) in enumerate(ws[nrr:], 1+nrr):
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, i])
                for w in (label, le):
                    layout.removeWidget(w)
                    layout.removeWidget(w)
                    w.setParent(None)
                    w.deleteLater()
            ui.dynamic_widgets[key] = ws = ws[:nrr]
        # Add widgets as needed
        while len(cbs) < nrr:
            key = 'vtk_rrate'
            n = 1+len(cbs)
            cb = CheckBox("Reaction rate %s" % n)
            cb.key = key
            cb.args = ['VTK', n]
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb, n-1, 0)
            key = 'vtk_rrate_label'
            label = QLabel('Label')
            label.args = ['VTK', n]
            self.add_tooltip(label, key=key)
            layout.addWidget(label, n-1, 1)
            le = LineEdit()
            le.key = key
            le.args = ['VTK', n]
            le.dtype = str
            le.value_updated.connect(self.project.submit_change)
            ws.append((label,le))
            self.add_tooltip(le, key=le.key)
            layout.addWidget(le, n-1, 2)
        # Set checkboxes to correct state
        key = 'vtk_rrate'
        for (i, cb) in enumerate(cbs, 1):
            val = self.project.get_value(key, args=[V0, i], default=False)
            cb.setChecked(bool(val))
        # Set lineedits to correct state
        key = 'vtk_rrate_label'
        for (i, (label, le)) in enumerate(ws, 1):
            val = self.project.get_value(key, args=[V0, i], default='')
            le.setText(val)


    def setup_output_solids_subtab(self, P):
        #Solids Phase (tab?)
        ui = self.ui.output
        self.vtk_current_solid = self.P = P
        if P is None: # Nothing to do (?)
            return
        if not self.vtk_current_indices: # No region selected
            # Clear inputs?
            return
        indices = self.vtk_current_indices
        V0 = indices[0]

        #Enable writing solids pressure
        # Requires TFM solids
        # Sets keyword VTK_P_S
        # DEFAULT value .FALSE.

        #Enable writing solids velocity vector
        # Requires TFM solids
        # Sets keyword VTK_VEL_S(#,#)
        # DEFAULT value .FALSE.

        #Enable writing solids velocity x/y/z-component
        # Requires TFM solids
        # Sets keyword VTK_U/V/W_S(#,#)
        # DEFAULT value .FALSE.

        #Enable writing solids volume fraction
        # Requires TFM solids
        # Sets keyword VTK_EP_S(#,#)
        # DEFAULT value .FALSE.

        #Enable writing solids (material) density
        # Requires TFM solids
        # Sets keyword VTK_RO_S(#,#)
        # DEFAULT value .FALSE.

        #Enable writing solids bulk density
        # Requires TFM solids
        # Sets keyword VTK_ROP_S(#,#)
        # DEFAULT value .FALSE.

        #Enable writing solids temperature
        # Requires TFM solids and ENERGY_EQ = .TRUE.
        # Sets keyword VTK_S_G(#,#)  # VTK_T_S
        # DEFAULT value .FALSE.
        key = 'vtk_t_s'
        cb = ui.checkbox_keyword_vtk_t_s_args_VTK_P
        energy_eq = self.project.get_value('energy_eq', default=True)
        enabled = bool(energy_eq)
        cb.setEnabled(enabled)
        if not enabled:
            cb.setChecked(False)
            for V in self.vtk_current_indices:
                self.unset_keyword(key, args=[V, P])

        #Enable writing solids phase granular temperature
        # Requires TFM solids and KT_TYPE /= “ALGEBRAIC”
        # Sets keyword VTK_THETA_M(#,#)
        # DEFAULT value .FALSE.
        key = 'vtk_theta_m'
        cb = ui.checkbox_keyword_vtk_theta_m_args_VTK_P
        kt_type = self.project.get_value('kt_type', default=DEFAULT_KT_TYPE)
        enabled = (kt_type != 'ALGEBRAIC')
        cb.setEnabled(enabled)
        if not enabled:
            cb.setChecked(False)
            for V in self.vtk_current_indices:
                self.unset_keyword(key, args=[V, P])

        for key in ('vtk_p_s', 'vtk_vel_s', 'vtk_u_s', 'vtk_v_s', 'vtk_w_s',
                    'vtk_ro_s', 'vtk_ep_s', 'vtk_rop_s', 'vtk_t_s', 'vtk_theta_m'):
            cb = getattr(ui, 'checkbox_keyword_%s_args_VTK_P'%key)
            val = self.project.get_value(key, default=False, args=[V0, P])
            cb.setChecked(bool(val))

        layout = ui.groupbox_cell_solids.layout()

        #Enable writing solids phase M, species N
        # Requires TFM solids and SPECIES_EQ(#) = .TRUE.
        # Sets keyword VTK_X_S(#,M,N)
        # DEFAULT value .FALSE.
        key = 'vtk_x_s'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        solids_species_names = list(self.solids_species.get(P, {}).keys())
        if self.output_saved_solids_species_names != solids_species_names:
            self.output_saved_solids_species_names = solids_species_names
            n_solids_species = len(solids_species_names)
            if len(cbs) > n_solids_species:
                for (i, cb) in enumerate(cbs[n_solids_species:], 1+n_solids_species):
                    for V in self.vtk_current_indices:
                        self.unset_keyword(key, args=[V, P, i]) # Should have been done already
                    layout.removeWidget(cb)
                    cb.setParent(None)
                    cb.deleteLater()
                ui.dynamic_widgets[key] = cbs = cbs[:n_solids_species]

            while len(cbs) < n_solids_species:
                n = 1+len(cbs)
                cb = CheckBox('%s mass fraction' % solids_species_names[n-1])
                cb.key = key
                cb.args = ['VTK', 'P', n]
                cb.value_updated.connect(self.project.submit_change)
                cbs.append(cb)
                self.add_tooltip(cb, key=cb.key)
                layout.addWidget(cb)
            for (cb, name) in zip(cbs, solids_species_names):
                cb.setText(name)

        # Set checkboxes to correct state
        species_eq = self.project.get_value('species_eq', default=True, args=[P])
        enable = bool(species_eq)
        for (i, cb) in enumerate(cbs, 1):
            cb.setEnabled(enable)
            if enable:
                val = self.project.get_value(key, args=[V0, P, i], default=False)
                cb.setChecked(bool(val))
            else:
                cb.setChecked(False)
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, P, i])


    def setup_output_scalar_subtab(self):
        #Scalar (tab?)
        # Note, this is nearly identical to output_reactions_tab
        ui = self.ui.output
        V0 = self.vtk_current_indices[0] if self.vtk_current_indices else None
        layout = ui.groupbox_cell_scalar.layout()
        #Enable writing user defined scalar
        # Requires NSCALAR > 0
        # Sets keyword VTK_SCALAR(#, #) # requires Scalar index
        # DEFAULT value .FALSE.
        key = 'vtk_scalar'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        nscalar = self.project.get_value('nscalar', default=0)
        # Remove extra widgets if number decreased
        if len(cbs) > nscalar:
            for (i, cb) in enumerate(cbs[nscalar:], 1+nscalar):
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, i]) # Should have been done already
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:nscalar]
        # Add widgets as needed
        while len(cbs) < nscalar:
            n = 1+len(cbs)
            cb = CheckBox(self.scalar_names.get(n, "Scalar %s" % n))
            cb.key = key
            cb.args = ['VTK', n]
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb)

        # Set checkboxes to correct state
        for (i, cb) in enumerate(cbs, 1):
            val = False if V0 is None else self.project.get_value(key, args=[V0, i], default=False)
            cb.setChecked(bool(val))
            name = self.scalar_names.get(i, 'Scalar %s'%i) # Names may have changed
            if cb.text() != name:
                cb.setText(name)


    def setup_output_other_subtab(self):
        #Other (tab?)
        ui = self.ui.output

        if not self.vtk_current_indices:
            return
        V0 = self.vtk_current_indices[0]

        # Enable writing solids pressure preventing overpacking
        # Requires TFM solids
        enabled = any(self.project.get_value('solids_model', default='TFM', args=[P]) == 'TFM'
                      for P in range(1, 1+len(self.solids)))
        cb = ui.checkbox_keyword_vtk_p_star_args_VTK
        if not hasattr(cb, 'tooltip0'):
            cb.tooltip0 = cb.toolTip()
        cb.setEnabled(enabled)
        if not enabled:
            cb.setChecked(False)
            cb.setToolTip(cb.tooltip0 + "<br>Requires TFM solids.")
            for V in self.vtk_current_indices:
                self.unset_keyword('vtk_p_star', args=[V])
        else:
            cb.setToolTip(cb.tooltip0)

        #Enable writing vorticity
        # Requires fluid solver (RO_G0 /= 0.0)
        # Sets keyword VTK_VORTICITY (#)
        # Sets keyword VTK_LAMBDA_2(#)
        # DEFAULT value .FALSE.
        keys = ['vtk_vorticity', 'vtk_lambda_2']
        cb = ui.checkbox_keyword_vtk_vorticity_args_VTK
        if not hasattr(cb, 'tooltip0'):
            cb.tooltip0 = cb.toolTip()
        enabled = not self.fluid_solver_disabled
        cb.setEnabled(enabled)
        if not enabled:
            cb.setChecked(False)
            for V in self.vtk_current_indices:
                for k in keys:
                    self.unset_keyword(k, args=[V])
            cb.setToolTip(cb.tooltip0 + "<br>Requires fluid solver enabled.")
        else:
            val = self.project.get_value(keys[0], default=False, args=[V0])
            cb.setChecked(bool(val))
            cb.setToolTip(cb.tooltip0)

        #Enable writing partition
        # Sets keyword VTK_PARTITION(#)
        # DEFAULT value .FALSE.

        #Enable writing boundary ID
        # Sets keyword VTK_BC_ID(#)
        # DEFAULT value .FALSE.

        #Enable writing wall distance
        # Sets keyword VTK_DWALL(#)
        # DEFAULT value .FALSE.

        #Enable writing cell index
        # Sets keyword VTK_IJK(#)
        # DEFAULT value .FALSE.
        for key in ('vtk_partition', 'vtk_bc_id',
                    'vtk_dwall', 'vtk_ijk'):
            cb = getattr(ui, 'checkbox_keyword_%s_args_VTK'%key)
            val = self.project.get_value(key, default=False, args=[V0])
            cb.setChecked(bool(val))


    def set_vtk_lambda_2(self):
        #vtk_lambda_2 follows setting of vtk_vorticity
        if not self.vtk_current_indices:
            return
        V0 = self.vtk_current_indices[0]
        val = self.project.get_value('vtk_vorticity', args=[V0])
        for V in self.vtk_current_indices:
            self.update_keyword('vtk_lambda_2', val, args=[V])


    def setup_output_vtk_particle(self):
        """Particle data sub-pane There is a need for some hand waving here. Many
        mfix.dat files may use a different specification for VTK input. There
        will need to be a way of catching the 'old' format and converting it to
        this input style.
        """

        # Note 1, filebase through nzs are common to cell/particle
        # Note 2, this groupbox is disabled completely if not DEM, CGP or PIC
        ui = self.ui.output
        ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_PARTICLE)

        indices = self.vtk_current_indices
        if not indices:
            # Clear inputs?  should have been done in handle_selection
            return
        V0 = indices[0]

        # SRS update June 2017 add VTK_SELECT_MODE
        key = 'vtk_select_mode'
        vtk_select_mode = self.project.get_value(key, args=[V0], default='C')
        if vtk_select_mode not in ['C', 'P', 'I']:
            self.warn("Invalid vtk_select_mode %s" % vtk_select_mode)
            vtk_select_mode = 'C'
        cb = ui.combobox_vtk_select_mode
        index = ['C', 'P', 'I'].index(vtk_select_mode)
        cb.setCurrentIndex(index)
        cb.setToolTip(get_combobox_item(cb,index).toolTip())

        # NB The following all require PIC or DEM/CGP solids and default to False
        #
        #Enable writing particle diameter
        # Sets keyword VTK_PART_DIAMETER(#)

        #Enable writing particle translational velocity
        # Sets keyword VTK_PART_VEL(#)

        #Enable writing particle rotational velocity
        # Sets keyword VTK_PART_ANGULAR_VEL

        #Enable writing particle orientation
        # Sets keyword VTK_PART_ORIENTATION

        #Enable writing particle temperature
        # Requires and ENERGY_EQ=.TRUE.
        # Sets keyword VTK_PART_TEMP(#)

        #Enable writing particle density
        # Sets keyword VTK_PART_DENSITY(#)

        #Enable writing particle cohesive force
        # Requires USE_COHESION=.TRUE.
        # Sets keyword VTK_PART_COHESION(#)

        #Enable writing particle user variable
        # Requires DES_USR_VAR_SIZE > 0
        # Sets keyword VTK_PART_USR_VAR(#,#)
        # (handled below)

        #Enable writing particle species composition
        # (below other controls)

        #Enable writing particle MPI rank
        # Sets keyword VTK_PART_RANK(#)

        #Enable writing particle global ID
        # Sets keyword VTK_PART_ID(#)

        #Enable writing particle phase ID
        # Sets keyword VTK_PHASE_ID(#)

        for key in ('vtk_part_diameter',
                    'vtk_part_vel',
                    'vtk_part_angular_vel',
                    'vtk_part_orientation',
                    'vtk_part_temp',
                    'vtk_part_density',
                    'vtk_part_cohesion',
                    'vtk_part_residence_time',
                    'vtk_part_cgp_stat_wt',
                    'vtk_part_physical_diameter',
                    'vtk_part_rank',
                    'vtk_part_id',
                    'vtk_part_phase_id'):
            cb = getattr(ui, 'checkbox_keyword_%s_args_VTK' % key)
            val = self.project.get_value(key, args=[V0], default=False)
            cb.setChecked(val)

        # disable temp if energy_eq disabled
        cb = ui.checkbox_keyword_vtk_part_temp_args_VTK
        key = 'vtk_part_temp'
        enabled = self.project.get_value('energy_eq', default=True)
        if not hasattr(cb, 'tooltip0'):
            cb.tooltip0 = cb.toolTip()
        cb.setEnabled(enabled)
        if not enabled:
            cb.setChecked(False)
            cb.setToolTip(cb.tooltip0 + '<br>Requires energy equations.')
            for V in self.vtk_current_indices:
                self.unset_keyword(key, args=[V])
        else:
            cb.setToolTip(cb.tooltip0)

        # disable cohesion if use_cohesion disabled
        cb = ui.checkbox_keyword_vtk_part_cohesion_args_VTK
        key = 'vtk_part_cohesion'
        enabled = self.project.get_value('use_cohesion', default=False)
        cb.setEnabled(enabled)
        if not hasattr(cb, 'tooltip0'):
            cb.tooltip0 = cb.toolTip()
        if not enabled:
            cb.setToolTip(cb.tooltip0 + '<br>Requires <b>USE_COHESION</b>.')
        else:
            cb.setToolTip(cb.tooltip0)
        if not enabled:
            cb.setChecked(False)
            for V in self.vtk_current_indices:
                self.unset_keyword(key, args=[V])

        # disable vtk_part_cgp_stat_wt if not CGP solid
        cb = ui.checkbox_keyword_vtk_part_cgp_stat_wt_args_VTK
        key = 'vtk_part_cgp_stat_wt'
        enabled = (self.project.solver == CGP)
        if not hasattr(cb, 'tooltip0'):
            cb.tooltip0 = cb.toolTip()
        cb.setEnabled(enabled)
        if not enabled:
            cb.setToolTip(cb.tooltip0 + '<br>CGP model only.')
            cb.setChecked(False)
            for V in self.vtk_current_indices:
                self.unset_keyword(key, args=[V])

        # disable vtk_part_physical_diameter if not CGP solid
        cb = ui.checkbox_keyword_vtk_part_physical_diameter_args_VTK
        key = 'vtk_part_physical_diameter'
        enabled = (self.project.solver == CGP)
        if not hasattr(cb, 'tooltip0'):
            cb.tooltip0 = cb.toolTip()
        cb.setEnabled(enabled)
        if not enabled:
            cb.setToolTip(cb.tooltip0 + '<br>CGP model only.')
            cb.setChecked(False)
            for V in self.vtk_current_indices:
                self.unset_keyword(key, args=[V])

        layout = ui.groupbox_particle_data.layout()
        #Enable writing particle user variable
        # Requires DEM, CGP or PIC solids and DES_USR_VAR_SIZE > 0
        # Sets keyword VTK_PART_USR_VAR(#,#)
        # DEFAULT value .FALSE.
        key = 'vtk_part_usr_var'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        des_usr_var_size = self.project.get_value('des_usr_var_size', default=0)
        # Remove extra widgets if number decreased
        if len(cbs) > des_usr_var_size:
            for (i, cb) in enumerate(cbs[des_usr_var_size:], 1+des_usr_var_size):
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, i])
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:des_usr_var_size]
        # Add widgets as needed
        while len(cbs) < des_usr_var_size:
            n = 1+len(cbs)
            cb = CheckBox("DES user scalar %s" % n)
            cb.key = key
            cb.args = ['VTK', n]
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb)
        # Set checkboxes to correct state
        for (i, cb) in enumerate(cbs, 1):
            val = self.project.get_value(key, args=[V0, i], default=False)
            cb.setChecked(bool(val))

        #Enable writing particle species composition
        # Requires DEM, CGP or PIC solids and any SPECIES_EQ=.TRUE.
        # Sets keyword VTK_PART_X_S(#,N)
        # NOTE, VTK_PART_X_S(#,N) where N ranges from 1 to max(nmax_s)
        # DEFAULT value .FALSE.
        key = 'vtk_part_x_s'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        enabled = any(self.project.get_value('species_eq', default=True, args=[P])
                      for P in range(1, len(self.solids)+1))
        max_n = 0 if not enabled else max(self.project.get_value('nmax_s', args=[N], default=0)
                                          for N in range(1, len(self.solids)+1))
        # Remove extra widgets if number decreased
        if len(cbs) > max_n:
            for (i, cb) in enumerate(cbs[max_n:], 1+max_n):
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, i])
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:max_n]
        # Add widgets as needed
        while len(cbs) < max_n:
            n = 1+len(cbs)
            cb = CheckBox("Species %s mass fraction" % n)
            cb.key = key
            cb.args = ['VTK', n]
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb)
        # Set checkboxes to correct state
        for (i, cb) in enumerate(cbs, 1):
            val = self.project.get_value(key, args=[V0, i], default=False)
            cb.setChecked(bool(val))

        # Issues/1272 add VTK_PART_RRATE
        rxn_info = []  # Name, is_des, index
        des_count = 0
        for (name,rxn) in self.project.reactions.items():
            des = any(self.project.get_value('solids_model', default='TFM', args=[p]) != 'TFM'
                      for p in rxn.get('phases', []))
            if des:
                des_count += 1
                rxn_info.append((name, des_count))
            else:
                pass # Don't care about fluid reactions here
        key = 'vtk_part_rrate'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        # Remove extra widgets if number decreased
        if len(cbs) > des_count:
            for (i, cb) in enumerate(cbs[des_count:], 1+des_count):
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, cb.idx])
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:des_count]
        # Add widgets as needed
        while len(cbs) < des_count:
            n = 1+len(cbs)
            (name, idx) = rxn_info[n-1]
            cb = CheckBox(name + " rate")
            cb.key = key
            cb.args = ['VTK', idx]
            cb.idx = idx
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb)
        # Set checkboxes to correct state
        for (i, cb) in enumerate(cbs, 1):
            (name, idx) = rxn_info[i-1]
            val = self.project.get_value(cb.key, args=[V0, idx], default=False)
            cb.setText(name + " rate")
            cb.idx = idx
            cb.args = ['VTK', idx]
            cb.setChecked(bool(val))

        # SRS update Jul 2017
        # May 2018 move to separate groupbox
        layout = ui.groupbox_select_particle_data.layout()
        #Enable writing per-phase particle data
        # Requires DEM/CGP solids
        # Sets keyword VTK_PART_PHASE(#,#)
        # DEFAULT value .TRUE.
        key = 'vtk_part_phase'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        # Remove extra widgets if number decreased
        n_solids = len(self.solids)
        solids_names = list(self.solids.keys())
        if len(cbs) > n_solids:
            for (i, cb) in enumerate(cbs[n_solids:], 1+n_solids):
                for V in self.vtk_current_indices:
                    self.unset_keyword(key, args=[V, i]) # Should have been done already
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:n_solids]
        # Add widgets as needed
        while len(cbs) < n_solids:
            n = len(cbs) + 1
            cb = CheckBox(solids_names[n-1])
            cb.key = key
            cb.args = ['VTK', n]
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb)
        # Set checkboxes to correct state
        for (i, cb) in enumerate(cbs, 1):
            cb.setText(solids_names[i-1])
            val = self.project.get_value(key, args=[V0, i], default=True)
            cb.setChecked(bool(val))

    def setup_output_vtk_geometry(self):
        ui = self.ui.output
        key = 'vtk_geo'
        gb = ui.groupbox_geometry_data
        layout = gb.layout()
        for w in ui.dynamic_widgets.get(key, []):
            layout.removeWidget(w)
            w.setParent(None)
            w.deleteLater()
        ui.dynamic_widgets[key] = []
        files = glob.glob('geometry_*.stl') + glob.glob('is_*.stl')
        files = [f for f in files
                 if f.split('_')[1].split('.')[0].isdigit()]
        files.sort()
        V0 = self.vtk_current_indices[0] if self.vtk_current_indices else None
        if V0 is not None:
            keys = set(self.project.get_value(key, args=[V0,i]) for i in range(1,101))
            keys.discard(None)
        else:
            keys = set()
        for i,f in enumerate(files):
            cb = CheckBox(f)
            cb.key = key
            cb.args = ['VTK', i]
            ui.dynamic_widgets[key].append(cb)
            layout.addWidget(cb, i, 0)
            cb.setChecked(f in keys)
            cb.clicked.connect(lambda checked,fname=f: self.output_toggle_vtk_geo(checked, fname))

    def output_toggle_vtk_geo(self, checked, fname):
        if not self.vtk_current_indices:
            return
        V0 = self.vtk_current_indices[0]
        key = 'vtk_geo'
        files = set(self.project.get_value(key, args=[V0,i]) for i in range(1,101)) #VTK_GEO_MAX
        files.discard(None)
        if checked:
            files.add(fname)
        else:
            files.discard(fname)
        files = sorted(list(files))
        for i, f in enumerate(files, 1):
            self.update_keyword(key, f, args=[V0,i])
        for i in range(len(files)+1, 101):
            self.unset_keyword(key, args=[V0,i])

    def output_set_vtk_region_keys(self, idx, data, output_type=None):
        # Update the keys which define the region the vtk output applies to
        if output_type is not None:
            self.update_keyword('vtk_data', output_type, args=[idx])
        if self.project.get_value('vtk_data', args=[idx]) == 'G': # Geo
            for k in ('x_w', 'x_e', 'y_n', 'y_s', 'z_b', 'z_t'):
                self.unset_keyword('vtk_'+k) # Do we need to do this?
            return
        no_k = self.project.get_value('no_k')
        for (key, val) in zip(('x_w', 'y_s', 'z_b',
                               'x_e', 'y_n', 'z_t'),
                              data['from']+data['to']):
            # vtk_z_t and vtk_z_b keywords should not be added when no_k=True
            if no_k and key in ('z_t', 'z_b'):
                continue
            self.update_keyword('vtk_'+key, val, args=[idx])


    def output_check_region_in_use(self, name):
        return (any(data.get('region') == name for data in self.vtk_outputs.values())
                or any(data.get('region') == name for data in self.usr_outputs.values()))

    def output_update_region(self, name, data):
        for (i, output) in self.vtk_outputs.items():
            if output.get('region') == name:
                self.output_set_vtk_region_keys(i, data)
        for (i, output) in self.usr_outputs.items():
            if output.get('region') == name:
                self.output_set_usr_region_keys(i, data)



    def output_change_region_name(self, old_name, new_name):
        ui = self.ui.output
        # VTK outputs
        for (key, val) in self.vtk_outputs.items():
            if val.get('region') == old_name:
                self.vtk_outputs[key]['region'] = new_name
                tw = ui.tablewidget_vtk_regions
                for i in range(tw.rowCount()):
                    data = tw.item(i, 0).data(UserRole)
                    indices, names = data
                    if key in indices:
                        item = tw.item(i, COLUMN_REGION)
                        new_names = [new_name if n == old_name else n for n in names]
                        item.setData(UserRole, (indices, new_names))
                        item.setText('+'.join(new_names))
                        # Also update vtk_filebase, if it is at the default setting
                        vtk_filebase = self.project.get_value('vtk_filebase', args=[key],
                                                              default='')
                        default_name = '+'.join(names)
                        if vtk_filebase and (vtk_filebase==default_name
                                             or vtk_filebase.startswith(default_name+'_')):
                            vtk_filebase = self.output_default_vtk_filebase(new_names)
                            for idx in indices:
                                self.update_keyword('vtk_filebase', vtk_filebase, args=[idx])
                            tw.item(i, COLUMN_FILENAME).setText(vtk_filebase)
        # USR outputs
        for (key, val) in self.usr_outputs.items():
            if val.get('region') == old_name:
                self.usr_outputs[key]['region'] = new_name
                tw = ui.tablewidget_usr_regions
                for i in range(tw.rowCount()):
                    data = tw.item(i, 0).data(UserRole)
                    index, name = data
                    if key == index:
                        item = tw.item(i, COLUMN_REGION)
                        item.setData(UserRole, (index, new_name))
                        item.setText(new_name)


    def setup_output_residuals_tab(self):
        ui = self.ui.output
        indices = []
        for n in range(1,9):
            cb = getattr(ui, 'combobox_resid_string_%s'%n)
            indices.append(cb.currentIndex())
            cb.clear()

        def item(key, name):
            for n in range(1,9):
                cb = getattr(ui, 'combobox_resid_string_%s'%n)
                if key is None:
                    cb.addItem(name)
                else:
                    cb.addItem('%s: %s' % (key, name))

        item(None, '<None>')
        # P0 Gas pressure
        item('P0', '%s pressure'%self.fluid_phase_name)
        # PM Solids phase M pressure
        for (i,k) in enumerate(self.solids.keys(), 1):
            item('P%s'%i, '%s pressure'%k)
        # R0 Gas density
        item('R0', '%s density'%self.fluid_phase_name)
        # RM Solids phase M density
        for (i,k) in enumerate(self.solids.keys(), 1):
            item('R%s'%i, '%s density'%k)
        # U/V/W0 Gas phase U/V/W-velocity
        for c in 'UVW':
            item('%s0'%c, '%s %s-velocity' % (self.fluid_phase_name, c))

        # U/V/WM Solids phase M U/V/W-velocity
        for (i,k) in enumerate(self.solids.keys(), 1):
            for c in 'UVW':
                item('%s%s'%(c,i), '%s %s-velocity' % (k, c))

        # T0 Gas temperature
        item('T0', '%s temperature'%self.fluid_phase_name)
        # TM Solids phase M temperature
        for (i,k) in enumerate(self.solids.keys(), 1):
            item('T%s'%i, '%s temperature'%k)
        # GM Solids phase M granular temperature
        for (i,k) in enumerate(self.solids.keys(), 1):
            item('G%s'%i, '%s granular temperature'%k)
        # X0NN Gas phase species NN mass fraction
        for (i,k) in enumerate(self.fluid_species.keys(), 1):
            item('X0%02d'%i, '%s mass fraction'%k)
        # XMNN Solids phase M species NN mass fraction
        for i in range(1, 1+len(self.solids)):
            for (j,k) in enumerate(self.solids_species[i].keys(), 1):
                item('X%s%02d'%(i,j), '%s mass fraction'%k)
        # K0 K-Epsilon model residuals
        item('K0', 'K-ε model residuals')

        if self.project.get_value('group_resid', default=False):
            # Attempt to preserve settings even though groupbox is disabled
            for (i,j) in enumerate(indices, 1):
                cb = getattr(ui, 'combobox_resid_string_%s'%i)
                if j < len(cb):
                    cb.setCurrentIndex(j)
        for n in range(1,9):
            cb = getattr(ui, 'combobox_resid_string_%s'%n)
            val = self.project.get_value('resid_string', args=[n])
            if val:
                for i in range(0, len(cb)):
                    if get_combobox_item(cb, i).text().split(':')[0] == val:
                        cb.setCurrentIndex(i)
                        break
                else:
                    self.warning('Invalid resid_string(%s) %s ignored'%
                                 (n, val))
                    cb.setCurrentIndex(0)
            else:
                cb.setCurrentIndex(0)

        group_resid = self.project.get_value('group_resid', default=False)
        ui.groupbox_residuals.setEnabled(not group_resid)

        enable, filename, mode = self.output_logs.get('residuals', [False, '', 'overwrite'])
        gb = ui.groupbox_save_residuals
        gb.setChecked(bool(enable))
        ui.lineedit_residuals_filename.setText(filename)
        if mode not in ('overwrite', 'append', 'increment'):
            mode = 'overwrite'
        rb = getattr(ui, 'radiobutton_residuals_%s' % mode)
        rb.setChecked(True)


    def setup_output_log_tab(self):
        ui = self.ui.output
        for which in 'solid_inventory', 'solver_output', 'dt', 'nit':
            enable, filename, mode = self.output_logs.get(which, [False, '', 'overwrite'])
            gb = getattr(ui, 'groupbox_save_%s' % which)
            gb.setChecked(bool(enable))
            le = getattr(ui, 'lineedit_%s_filename' % which)
            le.setText(filename)
            if mode not in ('overwrite', 'append', 'increment'):
                mode = 'overwrite'
            rb = getattr(ui, 'radiobutton_%s_%s' % (which, mode))
            rb.setChecked(True)
        self.enable_disable_solid_inventory()


    def setup_output_usr_tab(self):
        ui = self.ui.output
        tw = ui.tablewidget_usr_regions
        self.fixup_output_table(tw)
        row = get_selected_row(tw)
        # Autoselect if only 1 row
        if row is None and len(self.usr_outputs) == 1:
            row = 0
            tw.setCurrentCell(row, 0)

        enabled = (row is not None)
        ui.toolbutton_usr_delete.setEnabled(enabled and ui.input_enabled)
        ui.toolbutton_usr_add.setEnabled(ui.input_enabled and len(self.usr_outputs)<5)
        ui.bottom_frame_usr.setEnabled(enabled and ui.input_enabled)

        if not enabled: # No selection, clear inputs
            for widget in widget_iter(ui.bottom_frame_usr):
                if isinstance(widget, LineEdit):
                    widget.setText('')
                elif isinstance(widget, CheckBox):
                    widget.setChecked(False)
                elif isinstance(widget, ComboBox):
                    widget.setCurrentIndex(0)
            for widget in ui.toolbutton_usr_delete, ui.toolbutton_usr_up, ui.toolbutton_usr_down:
                widget.setEnabled(False)
            return

        ui.toolbutton_usr_up.setEnabled(row > 0)
        ui.toolbutton_usr_down.setEnabled(row < tw.rowCount()-1)

        data = tw.item(row, 0).data(UserRole)
        id, region = data
        def get_widget(key):
            return getattr(ui, 'lineedit_keyword_%s_args_USR'%key)
        for key in 'usr_dt', 'usr_var', 'usr_type', 'usr_format', 'usr_ext':
            val = self.project.get_value(key, args=[id])
            get_widget(key).updateValue(key, val, args=[id])
        cb = ui.combobox_usr_region
        if self.output_region_dict is None:
            self.output_region_dict = self.ui.regions.get_region_dict()
        regions = ['None'] + [k for (k,v) in self.output_region_dict.items()
                              if v.get('type') in ('point', 'box')
                              or 'plane' in v.get('type', '----')]
        cb.clear()
        cb.addItems(regions)
        region = self.usr_outputs[id].get('region', 'None')
        if region not in regions:
            self.error("Unknown region %s" % region, popup=True)
            return
        cb.setCurrentIndex(regions.index(region))


    def output_add_usr_row(self):
        ui = self.ui.output
        tw = ui.tablewidget_usr_regions
        for n in range(1,6):
            if n not in self.usr_outputs:
                break
        else:
            return # should not be here
        self.usr_outputs[n] = {'region': 'None'}
        # Maintain order of indices
        self.usr_outputs = dict(sorted(self.usr_outputs.items()))
        self.update_usr_regions_table()
        for row in range(tw.rowCount()):
            data = tw.item(row,0).data(UserRole)
            if data:
                id, region = data
                if id==n:
                    tw.setCurrentCell(row, 0) # Select the new row
                    break
        ui.toolbutton_usr_add.setEnabled(len(self.usr_outputs)<5)


    def output_set_usr_region_keys(self, idx, data):
        # Update the keys which define the region the usr output applies to
        no_k = self.project.get_value('no_k')
        for (key, val) in zip(('x_w', 'y_s', 'z_b',
                               'x_e', 'y_n', 'z_t'),
                              data['from']+data['to']):
            # usr_z_t and usr_z_b keywords should not be added when no_k=True
            if no_k and key in ('z_t', 'z_b'):
                continue
            self.update_keyword('usr_'+key, val, args=[idx])



    def output_delete_usr_row(self):
        ui = self.ui.output
        tw = ui.tablewidget_usr_regions
        row = get_selected_row(tw)
        if row is None: # No selection
            return
        data = tw.item(row, 0).data(UserRole)
        if not data:
            return
        id, region = data
        if id not in self.usr_outputs:
            return
        del self.usr_outputs[id]
        for key in USR_KEYS:
            self.unset_keyword(key, args=[id])
        ui.toolbutton_usr_delete.setEnabled(len(self.usr_outputs)>0)
        ui.toolbutton_usr_add.setEnabled(len(self.usr_outputs)<5)
        self.update_usr_regions_table()


    def output_up_usr_row(self):
        ui = self.ui.output
        tw = ui.tablewidget_usr_regions
        row = get_selected_row(tw)
        if row is None or row==0:
            return
        data = tw.item(row-1,0).data(UserRole)
        if not data:
            return
        id_a, region_a = data
        data = tw.item(row, 0).data(UserRole)
        if not data:
            return
        id_b, region_b = data
        a,b = self.usr_outputs.get(id_a), self.usr_outputs.get(id_b)
        self.usr_outputs[id_a] = b
        self.usr_outputs[id_b] = a

        for key in USR_KEYS:
            a, b = self.project.get_value(key, args=[id_a], default=None), self.project.get_value(key, args=[id_b],default=None)
            self.update_keyword(key, b, args=[id_a])
            self.update_keyword(key, a, args=[id_b])
        self.update_usr_regions_table()
        tw.setCurrentCell(row-1,0)


    def output_down_usr_row(self):
        ui = self.ui.output
        tw = ui.tablewidget_usr_regions
        row = get_selected_row(tw)
        if row is None or row==tw.rowCount()-1:
            return
        data = tw.item(row,0).data(UserRole)
        if not data:
            return
        id_a, region_a = data
        data = tw.item(row+1, 0).data(UserRole)
        if not data:
            return
        id_b, region_b = data
        a,b = self.usr_outputs.get(id_a), self.usr_outputs.get(id_b)
        self.usr_outputs[id_a] = b
        self.usr_outputs[id_b] = a

        for key in USR_KEYS:
            a, b = self.project.get_value(key, args=[id_a], default=None), self.project.get_value(key, args=[id_b],default=None)
            self.update_keyword(key, b, args=[id_a])
            self.update_keyword(key, a, args=[id_b])
        self.update_usr_regions_table()
        tw.setCurrentCell(row+1,0)


    def reset_output(self):
        ui = self.ui.output
        # Set all output-related state back to default
        ui.pushbutton_vtk.setEnabled(False)
        self.output_change_tab(BASIC_TAB)
        self.vtk_outputs.clear()
        self.usr_outputs.clear()
        self.output_logs.clear()
        self.vtk_current_indices = []
        self.vtk_current_regions = []
        self.usr_current_index = None
        self.usr_current_region = None
        self.output_region_dict = None
        self.vtk_current_solid = self.P = None
        ui.tablewidget_vtk_regions.clearContents()
        ui.tablewidget_vtk_regions.setRowCount(0)
        ui.tablewidget_usr_regions.clearContents()
        ui.tablewidget_usr_regions.setRowCount(0)
        ui.groupbox_filter_particles.setChecked(False)
        self.output_handle_groupbox_filter_particles(False, use_retained=False)
        ui.groupbox_filter_particles_checked = None
        self.output_current_tab = BASIC_TAB
        ui.stackedwidget_output.setCurrentIndex(BASIC_TAB)
        ui.stackedwidget_cell_particle.setCurrentIndex(PAGE_CELL)
        self.output_current_subtab = FLUID_TAB
        ui.stackedwidget_cell.setCurrentIndex(FLUID_TAB)
        # Clean up dynamic widgets
        for i1 in ui.dynamic_widgets.values():
            for i2 in i1:
                if isinstance(i2, (list, tuple)):
                    for i3 in i2:
                        i3.deleteLater()
                else:
                    i2.deleteLater()
        ui.dynamic_widgets.clear()
