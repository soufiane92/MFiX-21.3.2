# -*- coding: utf-8 -*-
"""Monitors pane"""

from json import JSONDecoder, JSONEncoder

from qtpy import QtCore
from qtpy.QtWidgets import (QHeaderView, QPushButton, QGroupBox,
                            QTableWidgetItem, QWidget)

from qtpy.QtGui import QPixmap, QPalette

from mfixgui.animations import animate_stacked_widget
from mfixgui.widgets.base import CheckBox

from mfixgui.tools.qt import (set_item_noedit, get_combobox_item,
                              get_selected_row, sub_icon_height,
                              set_combobox_tooltip,
                              widget_iter)

from mfixgui.tools.keyword_args import mkargs, keyword_args
from mfixgui.constants import *

UserRole = QtCore.Qt.UserRole

(FLUID_TAB, SOLIDS_TAB_DUMMY_L, SOLIDS_TAB, SOLIDS_TAB_DUMMY_R,
 SCALAR_TAB, REACTIONS_TAB, OTHER_TAB, PHASE_TAB) = range(8) # bottom tabset for Cell data

PAGE_PARTICLE = 8

# Columns in tablewidget_regions
COLUMN_REGION, COLUMN_FILENAME, COLUMN_OUTPUT_TYPE, COLUMN_DATA_TYPE, COLUMN_ID = range(5)

class Monitors(object):
    # Monitors task pane window:

    def init_monitors(self):
        ui = self.ui.monitors

        self.monitors = {} # key: index.  value: data dictionary for monitor
        self.monitors_current_index = None
        self.monitors_current_region = None # And the names of the regions which define them
        self.monitors_region_dict = None
        self.monitors_saved_solids_names = []
        ui.dynamic_widgets = {}
        ui.toolbutton_add.clicked.connect(self.monitors_show_regions_popup)
        ui.toolbutton_delete.clicked.connect(self.monitors_delete_regions)
        for tb in (ui.toolbutton_delete,):
            tb.setEnabled(False) # Need a selection

        tw = ui.tablewidget_regions
        tw.itemSelectionChanged.connect(self.handle_monitors_region_selection)
        tw.horizontalHeaderItem(COLUMN_REGION).setToolTip("Region monitor is defined over.")
        self.add_tooltip(tw.horizontalHeaderItem(COLUMN_FILENAME), key='monitor_name')
        tw.horizontalHeaderItem(COLUMN_OUTPUT_TYPE).setToolTip("Cell or Particle data")
        self.add_tooltip(tw.horizontalHeaderItem(COLUMN_DATA_TYPE), key='monitor_type')
        tw.horizontalHeaderItem(COLUMN_ID).setToolTip("Monitor ID")

        self.monitors_current_tab = FLUID_TAB # If fluid is disabled, we will switch
        self.monitors_current_solid = self.P = None
        ui.pushbutton_fluid.pressed.connect(lambda: self.monitors_change_tab(FLUID_TAB,None))
        ui.pushbutton_scalar.pressed.connect(lambda: self.monitors_change_tab(SCALAR_TAB,None))
        ui.pushbutton_reactions.pressed.connect(lambda: self.monitors_change_tab(REACTIONS_TAB,None))
        ui.pushbutton_other.pressed.connect(lambda: self.monitors_change_tab(OTHER_TAB,None))
        # Phase tab is not user-selectable

        # Trim width of "Fluid", "Scalar", "Reactions", and "Phase" like we do for
        # dynamically-created "Solid #" buttons
        for b in (ui.pushbutton_fluid, ui.pushbutton_scalar,
                  ui.pushbutton_reactions, ui.pushbutton_other,
                  ui.pushbutton_phase):
            w = b.fontMetrics().boundingRect(b.text()).width() + 20
            b.setMaximumWidth(w)

        # Show groupbox without title  (title is empty string)
        height = ui.checkbox_keyword_monitor_ep_g_args_MONITOR.sizeHint().height()
        tweak = 3
        # Fix gap where title would be, with negative margin.
        # This is somewhat quesionable (i.e. a total hack)
        # Furthermore setting the style in the subpage no longer works as of Qt 5.12
        #ui.subpage_cell.setStyleSheet(
        #    'QGroupBox{padding-top: %spx; margin-top: %spx;}' %
        #    (height, tweak-height))
        for w in widget_iter(ui.stackedwidget_detail):
            if isinstance(w, QGroupBox):
                if w.title():
                    continue
                w.setStyleSheet(
                    'QGroupBox{padding-top: %spx; margin-top: %spx;}' %
                    (height, tweak-height))

        cb = ui.combobox_monitor_type_cell
        key = 'monitor_type'
        cb.key = key
        for i in range(len(cb)):
            self.add_tooltip(get_combobox_item(cb, i), key, value=i)
        set_combobox_tooltip(cb)
        cb.activated.connect(self.handle_combobox_monitor_type_cell)
        cb = ui.combobox_monitor_type_particle
        key = 'monitor_type'
        cb.key = key
        for i in range(len(cb)):
            self.add_tooltip(get_combobox_item(cb, i), key, value=101+i) #NB
        set_combobox_tooltip(cb)
        cb.activated.connect(self.handle_combobox_monitor_type_particle)

        # Make the table update when user changed MONITOR_NAME
        le = ui.lineedit_keyword_monitor_name_args_MONITOR
        le.setMaxLength(64) # TODO:  ensure valid & unique file name
        le.post_update = self.setup_monitors

        # pmass checkbox gets special treatment
        cb = ui.checkbox_keyword_monitor_pmass_args_MONITOR
        cb.disabled = False


    def handle_combobox_monitor_type_cell(self, index):
        ui = self.ui.monitors
        cb = ui.combobox_monitor_type_cell
        cb.setToolTip(get_combobox_item(cb,index).toolTip())
        mon = self.monitors_current_index
        if mon is None: # No selection
            return
        key = 'monitor_type'
        val = index
        prev_phase_required = self.monitor_requires_phase(mon)
        self.update_keyword(key, val, args=[mon])
        new_phase_required = self.monitor_requires_phase(mon)

        if prev_phase_required != new_phase_required:
            # Unset keys when 'phase_required' changes..
            kwlist = list(self.project.keywordItems())
            for kw in kwlist:
                key, args = kw.key, kw.args
                if key.startswith('monitor_') and args and args[0]==mon:
                    if key in ('monitor_x_w', 'monitor_y_s', 'monitor_z_b',
                               'monitor_x_e', 'monitor_y_n', 'monitor_z_t',
                               'monitor_name', 'monitor_type', 'monitor_dt'):
                        continue
                    self.unset_keyword(key, args=args)


        self.handle_monitors_region_selection() # Will update tabset & ensure valid tab


    def handle_combobox_monitor_type_particle(self, index):
        ui = self.ui.monitors
        cb = ui.combobox_monitor_type_particle
        set_combobox_tooltip(cb)
        mon = self.monitors_current_index
        if mon is None: # No selection
            return
        key = 'monitor_type'
        val = index + 101 # NB
        self.update_keyword(key, val, args=[mon])
        if val == MONITOR_PARTICLE_MASS_FLOW_RATE:
            # Disable all booleans except MONITOR_PMASS
            kwlist = list(self.project.keywordItems())
            for kw in kwlist:
                key, args = kw.key, kw.args
                if key.startswith('monitor_') and args and args[0]==mon:
                    if key in ('monitor_x_w', 'monitor_y_s', 'monitor_z_b',
                               'monitor_x_e', 'monitor_y_n', 'monitor_z_t',
                               'monitor_name', 'monitor_type', 'monitor_dt'):
                        continue
                    self.unset_keyword(key, args=args)
            self.update_keyword('monitor_pmass', True, args=[mon])

        self.setup_monitors_particle() # Update widgets
        #self.handle_monitors_region_selection() # Will update widgets


    def monitors_show_regions_popup(self):
        #  Monitor regions can be points, planes, or volumes
        #  STL regions can not be used to defined a monitor region
        #  Multiple regions can not be combined to define a monitor
        #  Invalid regions can not be selected in the menu

        ui = self.ui.monitors
        rp = self.regions_popup
        rp.clear()

        # Only allow particle data for DEM, CGP or PIC solids
        enabled = any(self.project.get_value('solids_model',
                                             default='TFM',
                                             args=[i]) in ('DEM','CGP','PIC')
                      for i in range(1, 1+len(self.solids)))

        get_combobox_item(rp.combobox_2, 1).setEnabled(enabled)

        if not enabled:
            rp.combobox_2.setCurrentIndex(0)

        for (name,data) in self.monitors_region_dict.items():
            shape = data.get('type', '---')
            # Assume available if unmarked
            available = (data.get('available', True)
                         and (shape in ('box', 'point')
                              or 'plane' in shape))
            row = (name, shape, available)
            rp.add_row(row)
        rp.reset_signals()
        rp.save.connect(self.monitors_add_regions)
        rp.cancel.connect(self.monitors_cancel_add)
        for item in (ui.tablewidget_regions,
                     ui.bottom_frame,
                     ui.toolbutton_add,
                     ui.toolbutton_delete):
            item.setEnabled(False)
        # Force popup to be wider.  This is a hack.
        rp.popup('Select region for monitor                       ')


    def monitors_cancel_add(self):
        ui = self.ui.monitors
        for item in (ui.toolbutton_add,
                     ui.tablewidget_regions):
            item.setEnabled(True)
        tw = ui.tablewidget_regions
        row = get_selected_row(tw)
        if row is not None:
            for item in (ui.bottom_frame,
                         ui.toolbutton_delete):
                item.setEnabled(True)


    def monitors_add_regions(self):
        # Interactively add regions to define Monitors
        ui = self.ui.monitors
        rp = self.regions_popup

        self.monitors_cancel_add() # Re-enable input widgets
        selections = rp.get_selection_list()
        if not selections:
            return
        output_type = rp.combobox_2.currentIndex() # 0: cell 1: particle
        monitor_type = rp.combobox.currentIndex() - 1 # <choose type>
        if output_type == 1: # Particle data
            monitor_type += 101 #  no 'value' for particle data
        self.monitors_add_regions_1(selections, monitor_type=monitor_type, autoselect=True)
        if monitor_type == MONITOR_PARTICLE_MASS_FLOW_RATE:
            self.update_keyword('monitor_pmass', True, args=[self.monitors_current_index])
        self.setup_monitors() # Update the widgets


    def monitors_add_regions_1(self, selections, monitor_type=None, index=None, autoselect=False):
        # Used by both interactive and load-time add-region handlers
        ui = self.ui.monitors

        if not selections:
            return
        if monitor_type is None:
            self.error("No type for monitor %s" % index)
            return
        if self.monitors_region_dict is None:
            self.monitors_region_dict = self.ui.regions.get_region_dict()

        tw = ui.tablewidget_regions
        nrows = tw.rowCount()
        tw.setRowCount(nrows+1)

        def make_item(val):
            item = QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item

        region_name = selections[0]
        item = make_item(region_name)

        if index is None: # interactive
            index = self.monitors_find_index()

        self.monitors[index] = {'region': region_name}
        region_data = self.monitors_region_dict.get(region_name)
        if region_data is None: # ?
            self.warn("no data for region %s" % region_name)
            return
        self.monitors_set_region_keys(region_name, index, region_data, monitor_type=monitor_type)
        #self.monitors_region_dict[region_name]['available'] = False # Mark as in-use
        item.setData(UserRole, (index, region_name))
        tw.setItem(nrows, COLUMN_REGION, item)

        name=self.project.get_value('monitor_name', args=[index], default='')
        item = make_item(name)
        item.args=[index]
        self.add_tooltip(item, key="monitor_name", value=name)
        tw.setItem(nrows, COLUMN_FILENAME, item)

        monitor_type = self.project.get_value('monitor_type',
                                              default=0,
                                              args=[index])
        item = make_item('Particle' if monitor_type > 100 else 'Cell')
        tw.setItem(nrows, COLUMN_OUTPUT_TYPE, item)

        item = make_item(MONITOR_TYPE_PARTICLE_NAMES[monitor_type-101] if monitor_type>100
                         else MONITOR_TYPE_CELL_NAMES[monitor_type])
        item.args=[index]
        self.add_tooltip(item, key='monitor_type', value=monitor_type)

        tw.setItem(nrows, COLUMN_DATA_TYPE, item)
        item = make_item(str(index))

        tw.setItem(nrows, COLUMN_ID, item)

        self.fixup_monitors_table()
        if autoselect:
            tw.setCurrentCell(nrows, COLUMN_REGION)


    def monitors_find_index(self):
        # Always add new monitor at end
        return 1 if not self.monitors else 1 + max(self.monitors)


    def monitors_delete_regions(self):
        ui = self.ui.monitors
        tw = ui.tablewidget_regions
        row = get_selected_row(tw)
        if row is None: # No selection
            return

        # Unset keywords
        kwlist = list(self.project.keywordItems())
        for kw in kwlist:
            key, args = kw.key, kw.args
            if key.startswith('monitor_') and args and args[0]==self.monitors_current_index:
                self.unset_keyword(key, args=args)

        r = self.monitors_current_region
        if r and r in self.monitors_region_dict:
            self.monitors_region_dict[r]['available'] = True

        i = self.monitors_current_index
        if i in self.monitors:
            del self.monitors[i]

        self.monitors_current_region = None
        self.monitors_current_index = None

        tw.removeRow(row)
        self.fixup_monitors_table()
        self.monitors_setup_current_tab()
        self.update_nav_tree()


    def monitors_delete_solids_phase(self, phase_index):
        """adjust monitors_current_solid when solids phase deleted"""
        if (self.monitors_current_solid is not None and
            self.monitors_current_solid >= phase_index):
            self.monitors_current_solid -= 1
            if self.monitors_current_solid == 0:
                self.monitors_current_solid = None


    def handle_monitors_region_selection(self):
        ui = self.ui.monitors
        table = ui.tablewidget_regions
        row = get_selected_row(table)
        nrows = table.rowCount()
        if row is None:
            index = None
            region = None
        else:
            (index, region) = table.item(row,COLUMN_REGION).data(UserRole)
        self.monitors_current_index, self.monitors_current_region = index, region
        enabled = (row is not None)
        for item in (ui.toolbutton_delete,
                     ui.bottom_frame):
            item.setEnabled(enabled)
        if not enabled:
            return
        # Set combobox to only allow appropriate monitor_type
        mon = index
        monitor_type = self.project.get_value('monitor_type', default=0, args=[mon])
        particle = monitor_type > 100
        offset = 101 if particle else 0
        cb = ui.combobox_monitor_type_particle if particle else ui.combobox_monitor_type_cell

        region_type = self.monitors_region_dict.get(region)
        if region_type is None:
            self.error("Invalid region %s" % region)
            return
        region_type = region_type.get('type')
        if region_type == 'point':
            valids = MONITOR_TYPES_PARTICLE_POINT if particle else MONITOR_TYPES_CELL_POINT
        elif 'plane' in region_type:
            valids = MONITOR_TYPES_PARTICLE_PLANE if particle else MONITOR_TYPES_CELL_PLANE
        elif 'box' in region_type:
            valids = MONITOR_TYPES_PARTICLE_VOLUME if particle else MONITOR_TYPES_CELL_VOLUME
        else:
            valids = []
        for i in range(len(cb)):
            get_combobox_item(cb, i).setEnabled(i+offset in valids)
        if monitor_type is None or monitor_type not in valids:
            # Default not specified in SRS, but 1 (sum) is valid for planes and volumes
            monitor_type = 0 if region_type == 'point' else 1
            self.update_keyword('monitor_type', monitor_type, args=[mon])
        cb.setCurrentIndex(monitor_type-offset)
        set_combobox_tooltip(cb)
        if particle:
            # Done in setup_monitors_particle
            #ui.stackedwidget_detail.setCurrentIndex(PAGE_PARTICLE)
            pass
        else:
            enable_phase = self.monitor_requires_phase(mon)
            if enable_phase: # Disable all tabs except 'Phase'
                for i in range(ui.tab_layout.columnCount()-1): # Skip 'Phase'
                    item = ui.tab_layout.itemAtPosition(0, i)
                    if not item:
                        continue
                    widget = item.widget()
                    if not widget:
                        continue
                    widget.setEnabled(False)
                    widget.setToolTip('')
                    ui.pushbutton_phase.setEnabled(True)
            else:
                ui.pushbutton_fluid.setEnabled(True)
                for i in range(1, 1+len(self.solids)): # Enable tabs for TFM solids
                    item = ui.tab_layout.itemAtPosition(0, i)
                    if not item:
                        continue
                    widget = item.widget()
                    if not widget:
                        continue
                    enabled = self.project.get_value('solids_model', default='TFM', args=[i])=='TFM'
                    widget.setEnabled(enabled)
                    widget.setToolTip('TFM solids required.' if not enabled else '')

            enabled = self.project.get_value('nscalar', default=0) > 0
            ui.pushbutton_scalar.setEnabled(enabled)
            ui.pushbutton_scalar.setToolTip('No scalars defined.' if not enabled else '')

        self.setup_monitors() # reinitialize all widgets
        ui.scrollarea_detail.ensureVisible(0, 0)  # scroll to top


    def fixup_monitors_table(self, stretch_column=1):
        ui = self.ui.monitors
        hv = QHeaderView
        tw = ui.tablewidget_regions # main table, adjust top splitter
        resize = tw.horizontalHeader().setSectionResizeMode
        ncols = tw.columnCount()
        for n in range(0, ncols):
            resize(n, hv.Stretch if n==stretch_column else hv.ResizeToContents)

        # trim excess vertical space - can't figure out how to do this in designer
        header_height = tw.horizontalHeader().height()

        # Note - scrollbar status can change outside of this function.
        # Do we need to call this every time window geometry changes?
        scrollbar_height = tw.horizontalScrollBar().isVisible() * (4+tw.horizontalScrollBar().height())
        nrows = tw.rowCount()
        if nrows==0:
            row_height = 0
            height = header_height+scrollbar_height
        else:
            row_height = tw.rowHeight(0)
            height =  (header_height+scrollbar_height
                       + nrows*row_height + 4) # extra to avoid unneeded scrollbar
        icon_height = sub_icon_height() + 8
        ui.top_frame.setMaximumHeight(height+icon_height)
        ui.top_frame.setMinimumHeight(header_height+icon_height+row_height*min(nrows,5))
        ui.top_frame.updateGeometry()
        tw.setMaximumHeight(height)
        tw.setMinimumHeight(header_height)
        tw.updateGeometry() #? needed?


    def monitors_update_enabled(self):
        if self.monitors:
            # Never disable if there are Monitors defined
            disabled = False
        else:
            # If there are no solids, no scalar equations, and the fluid solver is disabled,
            # then we have no input tabs on the Monitors pane, so disable it completely
            regions = self.ui.regions.get_region_dict()
            nregions = sum(1 for (name, r) in regions.items()
                           if r.get('type') in ('box', 'point')
                           or 'plane' in r.get('type'))
            disabled = (nregions==0
                        or (self.project.get_value('nrr', default=0) == 0
                            and len(self.project.reactions) == 0
                            and self.project.get_value('nscalar',default=0)==0
                            and len(self.solids)==0))
        self.find_navigation_tree_item("Monitors").setDisabled(disabled)


    def monitors_tab_to_index(self, tab, solid):
        return (0 if tab==FLUID_TAB
                else len(self.solids)+1 if tab==SCALAR_TAB
                else len(self.solids)+2 if tab==REACTIONS_TAB
                else len(self.solids)+3 if tab==OTHER_TAB
                else len(self.solids)+4 if tab==PHASE_TAB
                else solid)


    def monitors_change_tab(self, tab, solid):
        ui = self.ui.monitors
        index = self.monitors_tab_to_index(tab, solid)

        for i in range(ui.tab_layout.columnCount()):
            item = ui.tab_layout.itemAtPosition(0, i)
            if not item:
                continue
            widget = item.widget()
            if not widget:
                continue
            font = widget.font()
            font.setBold(i==index)
            widget.setFont(font)

        current_index = ui.stackedwidget_detail.currentIndex()
        # If we're switching from solid m to solid n, we need some
        # special handling, because both tabs are really the same
        # widget.  We make a picture of the current tab, display that
        # in a dummy pane, then slide back to the solids tab
        if tab == current_index == SOLIDS_TAB:
            if solid == self.monitors_current_solid:
                return # Really nothing to do

            if solid > (self.monitors_current_solid or 0):
                dummy_label = ui.label_dummy_solids_L
                dummy_tab = SOLIDS_TAB_DUMMY_L
            else:
                dummy_label = ui.label_dummy_solids_R
                dummy_tab = SOLIDS_TAB_DUMMY_R
            p = QPixmap(ui.page_solids.size())
            p.fill(ui.detail_pane.palette().color(QPalette.Window))
            ui.page_solids.render(p, flags=QWidget.DrawChildren)  #avoid rendering bg
            dummy_label.setPixmap(p)
            ui.stackedwidget_detail.setCurrentIndex(dummy_tab)

        self.monitors_current_tab = tab
        self.monitors_current_solid = self.P = solid if tab==SOLIDS_TAB else None
        self.monitors_setup_current_tab()

        # change stackedwidget contents
        animate_stacked_widget(
            self,
            ui.stackedwidget_detail,
            (ui.stackedwidget_detail.currentIndex(), tab),
            line=ui.tab_underline,
            to_btn=ui.tab_layout.itemAtPosition(0, index),
            btn_layout=ui.tab_layout)
        # Scroll to top
        ui.scrollarea_detail.ensureVisible(0, 0)


    def monitors_check_region_in_use(self, name):
        return any(data.get('region')==name for data in self.monitors.values())


    def monitors_update_region(self, name, data):
        for (i,mon) in self.monitors.items():
            if mon.get('region') == name:
                self.monitors_set_region_keys(name, i, data)


    def monitors_set_region_keys(self, name, idx, data, monitor_type=None):
        # Update the keys which define the region the monitor applies to
        if monitor_type is not None:
            self.update_keyword('monitor_type', monitor_type, args=[idx])
        no_k = self.project.get_value('no_k')
        for (key, val) in zip(('x_w', 'y_s', 'z_b',
                               'x_e', 'y_n', 'z_t'),
                              data['from']+data['to']):
            # monitor_z_t and monitor_z_b keywords should not be added when no_k=True
            if no_k and key in ('z_t', 'z_b'):
                continue
            self.update_keyword('monitor_'+key, val, args=[idx])


    def monitors_change_region_name(self, old_name, new_name):
        ui = self.ui.monitors
        for (key, val) in self.monitors.items():
            if val.get('region') == old_name:
                self.monitors[key]['region'] = new_name
                tw = ui.tablewidget_regions
                for i in range(tw.rowCount()):
                    data = tw.item(i,COLUMN_REGION).data(UserRole)
                    index, name = data
                    if key == index:
                        item = tw.item(i,COLUMN_REGION)
                        item.setData(UserRole, (index, new_name))
                        item.setText(new_name)
                        # Also update monitor_name, if it is at the default setting
                        monitor_name = self.project.get_value('monitor_name', args=[key])
                        if monitor_name and (monitor_name==name
                                             or monitor_name.startswith(name+'_')):
                            monitor_name = self.monitor_default_name(new_name)
                            self.update_keyword('monitor_name', monitor_name, args=[index])
                            item = tw.item(i, COLUMN_FILENAME)
                            item.setText(monitor_name)
                            item.args = [index]
                            self.add_tooltip(item, key='monitor_name', value=monitor_name)

    def reset_monitors(self):
        self.monitors.clear()
        self.monitors_current_index = None
        self.monitors_current_region = None
        self.monitors_region_dict = None
        self.monitors_current_solid = self.P = None
        ui = self.ui.monitors
        ui.tablewidget_regions.clearContents()
        ui.tablewidget_regions.setRowCount(0)
        # anything else to do here?
        # TODO remove dynamically created input widgets, although this should
        #  get handled next time we call 'setup'

    def monitor_regions_to_str(self):
        ui = self.ui.monitors
        tw = ui.tablewidget_regions
        data = [tw.item(i,COLUMN_REGION).data(UserRole)
                for i in range(tw.rowCount())]
        return JSONEncoder().encode(data)


    def monitors_regions_from_str(self, s):
        if not s:
            return
        data = JSONDecoder().decode(s)
        for (index, region) in data:
            monitor_type = self.project.get_value('monitor_type', args=[index])
            self.monitors_add_regions_1([region], monitor_type=monitor_type,
                                        index=index, autoselect=False)


    def setup_monitors(self, allow_disabled_tab=False):
        ui = self.ui.monitors
        # Grab a fresh copy, may have been updated
        self.monitors_region_dict = self.ui.regions.get_region_dict()

        # Mark regions which are in use (this gets reset each time we get here)
        #for (i, data) in self.monitors.items():
        #    region = data['region']
        #    if region in self.monitors_region_dict:
        #        self.monitors_region_dict[region]['available'] = False
        self.fixup_monitors_table()
        row = get_selected_row(ui.tablewidget_regions)
        # Autoselect if only 1 row
        if row is None and ui.tablewidget_regions.rowCount() == 1:
            row = 0
            ui.tablewidget_regions.setCurrentCell(row, COLUMN_REGION)
        enabled = (row is not None)
        for item in (ui.toolbutton_delete,
                     ui.bottom_frame):
            item.setEnabled(enabled)

        mon = self.monitors_current_index
        mon_type = self.project.get_value('monitor_type', default=1, args=[mon])
        if mon is not None:
            key = 'monitor_name'
            le = ui.lineedit_keyword_monitor_name_args_MONITOR
            val = self.project.get_value(key, args=[mon])
            if val is None: # Construct from region name
                val = self.monitor_default_name(self.monitors_current_region)
                self.update_keyword(key, val, args=[mon])
            le.updateValue(key, val)
            # Update table too
            tw = ui.tablewidget_regions
            for i in range(tw.rowCount()):
                data = tw.item(i, 0).data(UserRole)
                index, name = data
                if index == mon:
                    item = tw.item(i, COLUMN_FILENAME)
                    item.setText(val)
                    item.args = [mon]
                    self.add_tooltip(item, key='monitor_name', value=val)

            #Specify write interval
            key = 'monitor_dt'
            default = 0.05
            le = ui.lineedit_keyword_monitor_dt_args_MONITOR
            val = self.project.get_value(key, args=[mon])
            if val is None:
                val = default
                self.update_keyword(key, val, args=[mon])
            le.updateValue(key, val)

        if mon_type > 100: # Particle data
            self.setup_monitors_particle()
        else:
            self.setup_monitors_cell(allow_disabled_tab)


    def setup_monitors_particle(self):
        ui = self.ui.monitors
        mon = self.monitors_current_index
        if mon is None:
            return
        monitor_type = self.project.get_value('monitor_type', args=[mon])

        layout = ui.groupbox_particle.layout()

        ui.stackedwidget_detail.setCurrentIndex(PAGE_PARTICLE)
        ui.label_monitor_data.setText("Select particle data to write")
        #ui.label_monitor_data.hide()
        ui.combobox_monitor_type_particle.show()
        ui.combobox_monitor_type_cell.hide()
        ui.tab_frame.hide()

        mass_flow = (monitor_type==MONITOR_PARTICLE_MASS_FLOW_RATE)

        #  Enable monitoring particle rotational velocity about [xyz]
        #  Rotational velocities require DEM/CGP solids.  This is not per-phase,
        #  so enable if any phase is DEM/CGP
        enabled = (not mass_flow
                   and any(self.project.get_value('solids_model',
                                             default='TFM',
                                             args=[i]) in ('DEM','CGP')
                      for i in range(1, 1+len(self.solids))))
        for c in ('x','y','z'):
            item = getattr(ui, 'checkbox_keyword_monitor_rot_%s_args_MONITOR'%c)
            item.setEnabled(enabled)

        #Enable monitoring particle temperature
        #  Requires DEM, CGP or PIC solids and ENERGY_EQ=.TRUE.
        enabled = (not mass_flow
                   and self.project.get_value('energy_eq', default=True))
        ui.checkbox_keyword_monitor_t_p_args_MONITOR.setEnabled(enabled)

        # Set all checkboxes to correct state
        for key in ('radius', 'pmass', 'pvol', 'ro_p',
                    't_p',
                    'vel_x', 'vel_y', 'vel_z',
                    'rot_x', 'rot_y', 'rot_z',
                    'part_residence_time'):
            key = 'monitor_' + key
            item = getattr(ui, 'checkbox_keyword_%s_args_MONITOR' % key)
            item.setChecked(bool(self.project.get_value(key,
                                                        default=False,
                                                        args=[mon])))

        #  Enable monitoring particle species composition
        #  Requires DEM, CGP or PIC solids and any SPECIES_EQ=.TRUE.
        #  Sets keyword MONITOR_X_P(#,#)
        #  Note, MONITOR_X_P(#,N) where N ranges from 1 to max(nmax_s)
        #  Default value .FALSE.
        key = 'monitor_x_p'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        n = max(self.project.get_value('nmax_s', default=0, args=[i])
                for i in range(1, 1+len(self.solids)))
        # Remove extra widgets if number decreased
        if len(cbs) > n:
            for (i, cb) in enumerate(cbs[n:], 1+n):
                self.unset_keyword(key, args=[mon, i])
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:n]
        # Add widgets as needed
        while len(cbs) < n:
            cb = CheckBox('Species %d composition' % (1+len(cbs)))
            cb.key = key
            cb.args = ['MONITOR', 1+len(cbs)]
            assert len(cb.args)==len(keyword_args[key])
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            cb.tooltip_0 = cb.toolTip()
            layout.addWidget(cb)
        # Set checkboxes to correct state
        enabled = (not mass_flow
                   and any(self.project.get_value('species_eq',
                                                  default=True,
                                                  args=[i]) == True
                           for i in range(1, 1+len(self.solids))))
        for (i, cb) in enumerate(cbs, 1):
            val = self.project.get_value(key, args=[mon, i], default=False)
            cb.setChecked(bool(val))
            cb.setEnabled(enabled)
            if not enabled:
                cb.setToolTip(cb.tooltip_0 + '\nRequires species equations.')
            else:
                cb.setToolTip(cb.tooltip_0)

        # Enable monitoring particle user variable
        #  Requires DEM, CGP or PIC solids and DES_USR_VAR_SIZE > 0
        #  Sets keyword MONITOR_DES_USR_VAR(#,#)
        key = 'monitor_des_usr_var'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        n = self.project.get_value('des_usr_var_size', default=0)
        # Remove extra widgets if number decreased
        if len(cbs) > n:
            for (i, cb) in enumerate(cbs[n:], 1+n):
                self.unset_keyword(key, args=[mon, i])
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:n]
        # Add widgets as needed
        while len(cbs) < n:
            cb = CheckBox("DES user scalar %d" % (1+len(cbs)))
            cb.key = key
            cb.args = ['MONITOR', 1+len(cbs)]
            assert len(cb.args)==len(keyword_args[key])
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            cb.tooltip_0 = cb.toolTip()
            layout.addWidget(cb)
        # Set checkboxes to correct state
        for (i, cb) in enumerate(cbs, 1):
            val = self.project.get_value(key, args=[mon, i], default=False)
            cb.setChecked(bool(val))

        # Enable monitoring particle reaction rates
        #  Requires DEM, CGP or PIC solids
        #  Sets keyword MONITOR_PART_RRATE(#,#)
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
        key = 'monitor_part_rrate'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        # Remove extra widgets if number decreased
        if len(cbs) > des_count:
            for cb in cbs[des_count:]:
                self.unset_keyword(key, args=[mon, cb.idx])
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:des_count]
        # Add widgets as needed
        while len(cbs) < des_count:
            cb = CheckBox("reaction rate") # will be replaced by rxn name
            cb.key = key
            cb.idx = 1+len(cbs)
            cb.args = ['MONITOR', cb.idx]
            assert len(cb.args)==len(keyword_args[key])
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            cb.tooltip_0 = cb.toolTip()
            layout.addWidget(cb)
        # Set checkboxes to correct state
        for (i, cb) in enumerate(cbs, 1):
            (name, idx) = rxn_info[i-1]
            val = self.project.get_value(cb.key, args=[mon, idx], default=False)
            cb.setText(name + " rate")
            cb.idx = idx
            cb.args = ['MONITOR', idx]
            cb.setChecked(bool(val))


        # Filter particle data by phase
        layout = ui.groupbox_particle_filter.layout()
        #Enable writing per-phase particle data
        # Requires DEM, CGP or PIC solids
        # Sets keyword MONITOR_PART_PHASE(#,#)
        # DEFAULT value .TRUE.
        key = 'monitor_part_phase'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        n_solids = len(self.solids)
        solids_names = list(self.solids.keys())
        # Remove extra widgets if number decreased
        if len(cbs) > n_solids:
            for (i, cb) in enumerate(cbs[n_solids:], 1+n_solids):
                self.unset_keyword(key, args=[mon, i]) # Should have been done in delete_solids_phase
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:n_solids]
        # Add widgets as needed
        while len(cbs) < n_solids:
            n = len(cbs) + 1
            cb = CheckBox(solids_names[n-1])
            cb.key = key
            cb.args = ['MONITOR', n]
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb)
        # Set checkboxes to correct state
        for (i, cb) in enumerate(cbs, 1):
            cb.setText(solids_names[i-1])
            val = self.project.get_value(key, args=[mon, i], default=True)
            cb.setChecked(bool(val))
            enabled = self.project.get_value('solids_model',
                                             default='TFM',
                                             args=[i]) in ('DEM','CGP','PIC')
            cb.setEnabled(enabled)

        # Do this part after updating dynamic widgets - see issues/1313
        if mass_flow:
            cb = ui.checkbox_keyword_monitor_pmass_args_MONITOR
            cb.setChecked(True)
            # Force checkbox to stay selected, without greying it out
            if not cb.disabled:
                cb.disabled = True
                cb.value_updated.disconnect()
                cb.clicked.connect(lambda *args, cb=cb: cb.setChecked(True)) # short-circuit

            for cb in widget_iter(ui.groupbox_particle):
                if isinstance(cb, CheckBox):
                    # avoid operating on the 'layout' child
                    cb.setVisible(cb == ui.checkbox_keyword_monitor_pmass_args_MONITOR)

        else:
            cb = ui.checkbox_keyword_monitor_pmass_args_MONITOR
            if cb.disabled:
                cb.disabled = False
                cb.clicked.disconnect()
                cb.value_updated.connect(self.project.submit_change) # standard handler
            for cb in widget_iter(ui.groupbox_particle):
                if isinstance(cb, CheckBox):
                    # avoid operating on the 'layout' child
                    cb.setVisible(True)


    def setup_monitors_cell(self, allow_disabled_tab=False):
        ui = self.ui.monitors
        mon = self.monitors_current_index
        enable_phase = self.monitor_requires_phase(mon)

        ui.label_monitor_data.setText("Select cell data to write")
        ui.label_monitor_data.show()
        ui.combobox_monitor_type_cell.show()
        ui.combobox_monitor_type_particle.hide()
        ui.tab_frame.show()

        #**Fluid phase tab**
        b = ui.pushbutton_fluid
        b.setText(self.fluid_phase_name)
        font = b.font()
        font.setBold(self.monitors_current_tab == FLUID_TAB)
        b.setFont(font)
        w = b.fontMetrics().boundingRect(b.text()).width() + 20
        b.setMaximumWidth(w)

        #**Solids Phase Tab** *(Requires TFM Solids)*
        #Each solid phase will have its own tab. The tab name should be the name of the solid
        solids_names = list(self.solids.keys())
        if self.monitors_saved_solids_names != solids_names:
            # Clear out the old ones
            n_cols = ui.tab_layout.columnCount()
            for i in range(n_cols-1, 0, -1):
                item = ui.tab_layout.itemAtPosition(0, i)
                if not item:
                    continue
                widget = item.widget()
                if not widget:
                    continue
                if widget in (ui.pushbutton_fluid, ui.pushbutton_scalar,
                              ui.pushbutton_reactions, ui.pushbutton_other,
                              ui.pushbutton_phase):
                    continue
                ui.tab_layout.removeWidget(widget)
                widget.setParent(None)
                widget.deleteLater()
            # And make new ones
            for (i, solid_name) in enumerate(solids_names, 1):
                b = QPushButton(text=solid_name)
                w = b.fontMetrics().boundingRect(solid_name).width() + 20
                b.setMaximumWidth(w)
                b.setFlat(True)
                font = b.font()
                font.setBold(self.monitors_current_tab==SOLIDS_TAB and i==self.monitors_current_solid)
                b.setFont(font)
                b.pressed.connect(lambda i=i: self.monitors_change_tab(SOLIDS_TAB, i))
                ui.tab_layout.addWidget(b, 0, i)

        # Only TFM solids
        for i in range(1, 1+len(self.solids)):
            enabled = (self.project.get_value('solids_model', args=[i])=='TFM'
                       and not enable_phase)
            item = ui.tab_layout.itemAtPosition(0, i)
            if item:
                widget = item.widget()
            if widget:
                widget.setEnabled(enabled)
                if enabled:
                    widget.setToolTip('') # Clear disabled message

        #Scalar (tab) - Tab only available if scalar equations are solved
        # Move the 'Scalar' button to the right of all solids, if needed
        b = ui.pushbutton_scalar
        font = b.font()
        font.setBold(self.monitors_current_tab==SCALAR_TAB)
        b.setFont(font)
        nscalar = self.project.get_value('nscalar', default=0)
        enabled = (nscalar > 0) and not enable_phase
        b.setEnabled(enabled)
        if len(self.solids) != len(self.monitors_saved_solids_names):
            ui.tab_layout.removeWidget(b)
            ui.tab_layout.addWidget(b, 0, 1+len(self.solids))

        #Reactions (tab) - Tab only available if  nrr > 0
        # Move the 'Reactions' button to the right of all solids, if needed
        b = ui.pushbutton_reactions
        font = b.font()
        font.setBold(self.monitors_current_tab==REACTIONS_TAB)
        b.setFont(font)
        nrr = self.project.get_value('nrr', default=0)
        enabled = bool((nrr > 0 or self.project.reactions) and not enable_phase)
        if not enabled:
            b.setToolTip("Requires reactions or nrr > 0.")
        else:
            b.setToolTip('')
        b.setEnabled(enabled)
        if len(self.solids) != len(self.monitors_saved_solids_names):
            ui.tab_layout.removeWidget(b)
            ui.tab_layout.addWidget(b, 0, 2+len(self.solids))

        #Other (tab)
        # Move to the right of all solids, if needed
        b = ui.pushbutton_other
        font = b.font()
        font.setBold(self.monitors_current_tab==OTHER_TAB)
        b.setFont(font)
        enabled = not enable_phase
        b.setEnabled(enabled)
        if len(self.solids) != len(self.monitors_saved_solids_names):
            ui.tab_layout.removeWidget(b)
            ui.tab_layout.addWidget(b, 0, 3+len(self.solids))


        # Move the 'Phase' button to the right of all solids, if needed
        b = ui.pushbutton_phase
        font = b.font()
        font.setBold(self.monitors_current_tab==PHASE_TAB)
        b.setFont(font)
        b.setEnabled(enable_phase)
        if len(self.solids) != len(self.monitors_saved_solids_names):
            ui.tab_layout.removeWidget(b)
            ui.tab_layout.addWidget(b, 0, 4+len(self.solids))

        self.monitors_saved_solids_names = solids_names
        self.P = self.monitors_current_solid

        if mon is None:
            #Construct the GUI, even though disabled (species checkboxes, etc)
            self.monitors_setup_current_tab()
            return

        particle = self.project.get_value('monitor_type', default=0, args=[mon]) > 100
        if particle:
            # Shouldn't be here
            return

        # Don't stay on a disabled tab
        index = self.monitors_tab_to_index(self.monitors_current_tab, self.monitors_current_solid)
        item = None if index is None else ui.tab_layout.itemAtPosition(0, index)
        b = item.widget() if item else None
        if ui.isEnabled() and not (b and b.isEnabled()) and not allow_disabled_tab:
            self.monitors_change_tab(*self.monitors_find_valid_tab())
        else:
            if ui.stackedwidget_detail.currentIndex() == PAGE_PARTICLE:
                # Go back to cell monitor layout
                ui.stackedwidget_detail.setCurrentIndex(self.monitors_current_tab)
            self.monitors_setup_current_tab()

        # make sure underline is in the right place, as # of solids may
        # have changed (lifted from animate_stacked_widget, which we
        # don't want to call here)
        tab = self.monitors_current_tab
        line_to = self.monitors_tab_to_index(tab, self.monitors_current_solid)
        line = ui.tab_underline
        btn_layout = ui.tab_layout
        if line_to is not None:
            btn_layout.addItem(btn_layout.takeAt(
                btn_layout.indexOf(line)), 1, line_to)

    def monitor_requires_phase(self, mon):
        if mon is None:
            return False
        val = self.project.get_value('monitor_type', args=[mon])
        return val in (MONITOR_CELL_VOLUME_FLOW_RATE,
                       MONITOR_CELL_MASS_FLOW_RATE)


    def monitors_find_valid_tab(self):
        mon = self.monitors_current_index
        if self.monitor_requires_phase(mon):
            return (PHASE_TAB, None)
        else:
            return (FLUID_TAB, None) # Always available, even if fluid solver disabled


    def monitor_default_name(self, region_name):
        # Construct default value for MONITOR_NAME,
        # replacing possibly troublesome characters
        key = 'monitor_name'
        val = region_name
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


    def monitors_setup_current_tab(self):
        # Only called for cell monitors
        if self.monitors_current_tab == FLUID_TAB:
            self.setup_monitors_fluid_tab()
        elif self.monitors_current_tab == SOLIDS_TAB:
            self.setup_monitors_solids_tab(self.monitors_current_solid)
        elif self.monitors_current_tab == SCALAR_TAB:
            self.setup_monitors_scalar_tab()
        elif self.monitors_current_tab == REACTIONS_TAB:
            self.setup_monitors_reactions_tab()
        elif self.monitors_current_tab == OTHER_TAB:
            self.setup_monitors_other_tab()
        elif self.monitors_current_tab == PHASE_TAB:
            self.setup_monitors_phase_tab()


    def monitors_extract_regions(self):
        # Note, "monitors" is not a ConditionCollection
        # like BCs, VTKs, etc.  Should it be?
        if self.monitors:
            # We assume that monitor regions have been initialized correctly
            # from mfix_gui_comments.
            # TODO: verify that there is an monitor region for each monitor
            return

        if self.monitors_region_dict is None:
            self.monitors_region_dict = self.ui.regions.get_region_dict()

        for mon in self.project.get_key_indices('monitor_name'):
            mon = mon[0]
            extent = [self.project.get_value('monitor_'+k,
                                             args=[mon],
                                             default=0)
                      for k in ('x_w', 'y_s', 'z_b',
                                'x_e', 'y_n', 'z_t')]

            for (region_name, data) in self.monitors_region_dict.items():
                ext2 = [0 if x is None else x for x in
                        (data.get('from',[]) + data.get('to',[]))]
                if ext2 == extent:
                    if data.get('available', True):
                        monitor_type = self.project.get_value('monitor_type', args=[mon])
                        self.monitors_add_regions_1([region_name], monitor_type=monitor_type,
                                                    index=mon, autoselect=False)
                        break
            else:
                self.warn("monitor %s: could not match defined region %s" %
                          (mon, extent))
                kwlist = list(self.project.keywordItems())
                for kw in kwlist:
                    key, args = kw.key, kw.args
                    if key.startswith('monitor_') and args and args[0]==mon:
                        self.unset_keyword(key, args=args)


    def setup_monitors_fluid_tab(self):
        #Fluid (tab)
        ui = self.ui.monitors
        mon = self.monitors_current_index
        layout = ui.groupbox_fluid.layout()

        def get_widget(key):
            return getattr(ui, 'checkbox_keyword_%s_args_MONITOR'%key)

        # NB normally we bail out if index is None but we want to construct
        #  the fluid species checkboxes (for visual apperarance sake)
        for key in ('monitor_ep_g',
                    'monitor_p_g',
                    'monitor_u_g',
                    'monitor_v_g',
                    'monitor_w_g'):
            val = False if mon is None else self.project.get_value(key, default=False, args=[mon])
            widget = get_widget(key)
            widget.setChecked(bool(val))

        # Temperature requires fluid solver and ENERGY_EQ = .TRUE.
        key = 'monitor_t_g'
        enabled = self.project.get_value('energy_eq', default=True) and not self.fluid_solver_disabled
        val = False if mon is None else self.project.get_value(key, default=False, args=[mon])
        if val and not enabled:
            val = False
            self.update_keyword(key, val, args=[mon])
        widget = get_widget(key)
        widget.setChecked(bool(val))
        widget.setEnabled(enabled)

        # Turbulent kinetic energy and dissipation require TURBULENCE_MODEL='K_EPSILON'
        enabled = self.project.get_value('turbulence_model', default=DEFAULT_TURBULENCE_MODEL) == 'K_EPSILON'
        for key in ('monitor_k_turb_g',
                    'monitor_e_turb_g'):
            val = False if mon is None else self.project.get_value(key, default=False, args=[mon])
            if val and not enabled:
                val = False
                self.unset_keyword(key, args=[mon])
            widget = get_widget(key)
            widget.setChecked(bool(val))
            widget.setEnabled(enabled)

        key_x = 'monitor_x_g'
        key_y = 'monitor_y_g'
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
        names = list(self.fluid_species.keys())
        n_species = len(names)
        # Remove extra widgets if number decreased
        if len(cbs) > 2*n_species:
            for (i, cb) in enumerate(cbs[2*n_species:], 2*n_species):
                self.unset_keyword(cb.key, args=[mon, 1+i//2]) # Should have been done already by fluid_delete_species_keys
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            cbs = cbs[:2*n_species]
        # Add widgets as needed
        while len(cbs) < 2*n_species:
            n = len(cbs)//2
            for (key, desc) in ((key_x,'mass'), (key_y, 'molar')):
                cb = CheckBox('%s %s fraction' % (names[n], desc))
                cb.key = key
                cb.args = ['MONITOR', n+1]
                cb.value_updated.connect(self.project.submit_change)
                cbs.append(cb)
                self.add_tooltip(cb, key=cb.key)
                layout.addWidget(cb)
        # Set checkboxes to correct state
        species_eq = self.project.get_value('species_eq', default=True, args=[0])
        enabled = bool(species_eq and not self.fluid_solver_disabled)
        for (i, cb) in enumerate(cbs):
            cb.setEnabled(enabled)
            if enabled:
                val = self.project.get_value(cb.key, args=[mon, 1+(i//2)], default=False)
                cb.setChecked(bool(val))
            else:
                cb.setChecked(False)
                self.unset_keyword(cb.key, args=[mon, 1+i//2])
            cb.setText('%s %s fraction' % (names[i//2], ['mass','molar'][i%2]))
        ui.dynamic_widgets[key_x] = cbs[::2]
        ui.dynamic_widgets[key_y] = cbs[1::2]

    def setup_monitors_solids_tab(self, P):
        # Solid-# (tab) - Rename tab to user provided solids name.
        # Note, solids phases are numbered 1-N
        ui = self.ui.monitors
        self.monitors_current_solid = self.P = P
        if P is None: # Nothing to do
            return
        mon = self.monitors_current_index
        if mon is None: # No region selected
            return
        if self.project.get_value('solids_model', args=[P]) != 'TFM':
            return

        layout = ui.groupbox_solids.layout()

        def get_widget(key):
            for pat in ('checkbox_keyword_%s_args_MONITOR',
                        'checkbox_keyword_%s_args_MONITOR_P',
                        'checkbox_keyword_%s_args_MONITOR_P_S'):
                widget = getattr(ui, pat % key, None)
                if widget:
                    return widget
            self.error('no widget for key %s' % key)

        def setup_key_widget(key):
            args = mkargs(key, monitor=mon, phase=P)
            val = self.project.get_value(key, args=args)
            get_widget(key).setChecked(bool(val))

        for key in ('monitor_u_s', 'monitor_v_s', 'monitor_w_s',
                    'monitor_rop_s', 'monitor_p_s'):
            setup_key_widget(key)

        # Solids temperature requires ENERGY_EQ = .TRUE.
        key = 'monitor_t_s'
        enabled = self.project.get_value('energy_eq', default=True)
        val = False if mon is None else self.project.get_value(key, default=False, args=[mon,P])
        if val and not enabled:
            val = False
            self.update_keyword(key, val, args=[mon,P])
        widget = get_widget(key)
        widget.setChecked(bool(val))
        widget.setEnabled(enabled)

        # Granular temperature requires KT_TYPE != 'ALGEBRAIC'
        key = 'monitor_theta_m'
        enabled = self.project.get_value('kt_type', default=DEFAULT_KT_TYPE) != 'ALGEBRAIC'
        val = False if mon is None else self.project.get_value(key, default=False, args=[mon,P])
        if val and not enabled:
            val = False
            self.update_keyword(key, val, args=[mon,P])
        widget = get_widget(key)
        widget.setChecked(bool(val))
        widget.setEnabled(enabled)

        key = 'monitor_x_s'
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        names = list(self.solids_species[P].keys())
        n = len(names)
        # Remove extra widgets if number decreased
        if len(cbs) > n:
            for (i, cb) in enumerate(cbs[n:], 1+n):
                self.unset_keyword(key, args=[mon, P, i]) # Should have been done already by solids_delete_species_keys
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:n]
        # Add widgets as needed
        while len(cbs) < n:
            cb = CheckBox()
            cb.key = key
            cb.args = ['MONITOR', P, 1+len(cbs)]
            assert len(cb.args)==len(keyword_args[key])
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb)
        # Set checkboxes to correct state
        for (i, cb) in enumerate(cbs, 1):
            val = self.project.get_value(key, args=[mon, P, i], default=False)
            cb.setChecked(bool(val))
            cb.setText(names[i-1]) #Species name may have changed
            cb.args = ['MONITOR', P, i]
            self.add_tooltip(cb, key=cb.key)

    def setup_monitors_scalar_tab(self):
        mon = self.monitors_current_index
        if mon is None:
            return # No selection

        ui = self.ui.monitors
        nscalar = self.project.get_value('nscalar', default=0)
        old_nscalar = getattr(ui, 'nscalar', None)
        ui.nscalar = nscalar

        key = 'monitor_scalar'
        if nscalar == old_nscalar:
            # Update the checkboxes and names
            for i in range(1, nscalar+1):
                # TODO use ui.dynamic_widgets instead of getattr/setattr
                cb = getattr(ui, "checkbox_monitor_scalar_%s"%i, None)
                if not cb:
                    continue
                name = self.scalar_names.get(i, 'Scalar %s'%i)
                if cb.text() != name:
                    cb.setText(name)
                val = self.project.get_value(key, args=[mon, i], default=False)
                cb.setChecked(bool(val))

            return

        layout = ui.groupbox_scalar.layout()

        for i in range(layout.rowCount()-1, -1, -1):
            item = layout.itemAtPosition(i,0)
            if not item:
                continue
            widget = item.widget()
            if not widget:
                continue
            if isinstance(widget, CheckBox):
                self.unset_keyword(widget.key, args=widget.args)
            widget.setParent(None)
            widget.deleteLater()

        #Sets keyword MONITOR_SCALAR(#,#)
        #DEFAULT False
        key = 'monitor_scalar'
        row = 0
        for i in range(1, nscalar+1):
            cb = CheckBox(self.scalar_names.get(i, "Scalar %s" % i))
            cb.key = key
            cb.args = ['MONITOR', i]
            assert len(cb.args)==len(keyword_args[key])
            cb.value_updated.connect(self.project.submit_change)
            setattr(ui, 'checkbox_monitor_scalar_%s'%i, cb)
            self.add_tooltip(cb, key)
            val = self.project.get_value(key, args=[mon, i])
            cb.setChecked(bool(val))
            layout.addWidget(cb, row, 0)
            row += 1

    def setup_monitors_reactions_tab(self):
        mon = self.monitors_current_index
        if mon is None:
            return # No selection
        ui = self.ui.monitors

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
        key = 'monitor_fluid_des_rrate' # Not an actual MFIX key
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        layout = ui.groupbox_reactions.layout()
        if len(cbs) > rxn_count:
            for (i, cb) in enumerate(cbs[rxn_count:], 1+rxn_count):
                for M in self.monitor_current_indices:
                    self.unset_keyword(cb.key, args=[M, cb.idx])
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:rxn_count]
        # Add widgets as needed
        while len(cbs) < rxn_count:
            n = 1+len(cbs)
            (name, des, idx) = rxn_info[n-1]
            cb = CheckBox(name + " rate")
            cb.key = 'monitor_des_rrate' if des else 'monitor_fluid_rrate'
            cb.args = ['MONITOR', idx]
            cb.idx = idx
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb, n-1, 0)
        # Set all checkboxes to correct state
        for (i, cb) in enumerate(cbs):
            name, des, idx = rxn_info[i]
            cb.setText(name + " rate")
            cb.key = 'monitor_des_rrate' if des else 'monitor_fluid_rrate'
            cb.args = ['MONITOR', idx]
            cb.idx = idx
            self.add_tooltip(cb, key=cb.key)
            cb.setChecked(self.project.get_value(cb.key, args=[mon, idx], default=False))


        # This section is deprecated, see issues/1272
        nrr = self.project.get_value('nrr', default=0)
        enable = (nrr > 0) and any(k.value == True and k.key == 'monitor_rrate'
                                   for k in self.project.keywordItems())
        ui.groupbox_reactions_deprecated.setVisible(enable)
        layout = ui.groupbox_reactions_deprecated.layout()
        key = 'monitor_rrate'  # Note there is no monitor_rrate_label
        if key not in ui.dynamic_widgets:
            ui.dynamic_widgets[key] = []
        cbs = ui.dynamic_widgets[key]
        # Remove extra widgets if number decreased
        if len(cbs) > nrr:
            for (i, cb) in enumerate(cbs[nrr:], 1+nrr):
                for M in self.monitor_current_indices:
                    self.unset_keyword(key, args=[M, i]) # Should have been done already
                layout.removeWidget(cb)
                cb.setParent(None)
                cb.deleteLater()
            ui.dynamic_widgets[key] = cbs = cbs[:nrr]
        # Add widgets as needed
        while len(cbs) < nrr:
            n = 1+len(cbs)
            cb = CheckBox("Reaction rate %s" % n)
            cb.key = key
            cb.args = ['MONITOR', n]
            cb.value_updated.connect(self.project.submit_change)
            cbs.append(cb)
            self.add_tooltip(cb, key=cb.key)
            layout.addWidget(cb, n-1, 0)
        # Set checkboxes to correct state
        key = 'monitor_rrate'
        for (i, cb) in enumerate(cbs, 1):
            val = self.project.get_value(key, args=[mon, i], default=False)
            cb.setChecked(bool(val))


    def setup_monitors_other_tab(self):
        mon = self.monitors_current_index
        if mon is None:
            return # No selection
        ui = self.ui.monitors
        cb = ui.checkbox_keyword_monitor_p_star_args_MONITOR
        if not hasattr(cb, 'tooltip0'):
            cb.tooltip0 = cb.toolTip()
        enabled = any(self.project.get_value('solids_model', args=[i])=='TFM'
                       for i in range(1, 1+len(self.solids)))
        cb.setEnabled(enabled)
        if not enabled:
            cb.setChecked(False)
            cb.setToolTip(cb.tooltip0 + "<br>Requires TFM solids.")
            self.unset_keyword('monitor_p_star', args=[mon])
        else:
            cb.setToolTip(cb.tooltip0)


    def setup_monitors_phase_tab(self):
        mon = self.monitors_current_index
        if mon is None:
            return # No selection
        ui = self.ui.monitors
        layout = ui.groupbox_phase.layout()
        phase_names = [self.fluid_phase_name] + list(self.solids.keys())

        for i in range(layout.rowCount()-1, -1, -1):
            item = layout.itemAtPosition(i,0)
            if not item:
                continue
            widget = item.widget()
            if not widget:
                continue
            widget.setParent(None)
            widget.deleteLater()
        row = 0
        for (i,name) in enumerate(phase_names):
            key = 'monitor_rop_s' if i>0 else 'monitor_ep_g'
            args = ['MONITOR', i] if i else ['MONITOR']
            cb = CheckBox("%s flow rate" % name)
            cb.key = key
            cb.args = args
            self.add_tooltip(cb, key)
            assert len(cb.args)==len(keyword_args[key])
            cb.value_updated.connect(self.project.submit_change)
            args = [mon, i] if i else [mon]
            enabled = i==0 or self.project.get_value('solids_model', default='TFM', args=[i])=='TFM'
            val = self.project.get_value(key, args=args)

            if val and not enabled:
                val = False
                self.unset_keyword(key, args=args)
            cb.setEnabled(enabled)
            cb.setToolTip('TFM solids required.' if not enabled
                          else 'Write flow rate for %s phase.' % name)

            cb.setChecked(bool(val))
            layout.addWidget(cb, row, 0)
            row += 1
