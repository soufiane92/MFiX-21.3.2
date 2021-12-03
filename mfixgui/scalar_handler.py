# -*- coding: utf-8 -*-
"""Scalars pane"""

from mfixgui.tools import keyword_args

from qtpy import QtCore

from qtpy.QtWidgets import QHeaderView, QTableWidgetItem

from mfixgui.tools.qt import (set_item_noedit, get_combobox_item,
                              get_selected_row, sub_icon_height)

UserRole = QtCore.Qt.UserRole

COLUMN_NAME = 0
COLUMN_PHASE = 1
COLUMN_ID = 2

class ScalarHandler:
    def init_scalar_handler(self):
        #  Data members
        self.scalar_names = {}
        self.current_scalar = None
        #  Connect Qt callbacks
        ui = self.ui.scalars
        ui.toolbutton_add.clicked.connect(self.add_scalar)
        ui.toolbutton_add.key = 'nscalar'
        ui.toolbutton_delete.clicked.connect(self.delete_scalar)
        ui.toolbutton_delete.key = 'nscalar'
        for tb in (ui.toolbutton_delete,):
            tb.setEnabled(False) # Need a selection
        tw = ui.tablewidget_scalars
        tw.itemSelectionChanged.connect(self.handle_scalars_selection)
        cb = ui.combobox_scalar_phase
        cb.activated.connect(self.set_scalar_phase)
        self.add_tooltip(cb, key='phase4scalar')
        le = ui.lineedit_scalar_name
        for w in (le, ui.label_scalar_name):
            self.add_tooltip(w, key=None, description="Optional name for scalar equation")
        le.key = 'scalar_name' #Not a real mfix key, used in set_scalar_name
        le.value_updated.connect(self.set_scalar_name)

    def reset_scalars(self):
        self.scalar_names.clear()
        self.current_scalar = None

    def setup_scalars(self, allow_disabled_tab=False):
        # Set up the scalar pane
        ui = self.ui.scalars
        tw = ui.tablewidget_scalars
        nscalar = self.project.get_value('nscalar', default=0)
        avail_phases = self.scalars_get_avail_phases()
        tw.setRowCount(nscalar)

        def make_item(val):
            item = QTableWidgetItem(str(val))
            set_item_noedit(item)
            return item

        phase_names = [self.fluid_phase_name] + list(self.solids.keys())
        for i in range(1, 1+nscalar):
            name = self.scalar_names.get(i, 'Scalar %s'%i)
            tw.setItem(i-1, COLUMN_NAME, make_item(name))
            tw.setItem(i-1, COLUMN_ID, make_item(i))
            phase = self.project.get_value('phase4scalar', args=[i])
            if phase is None:
                self.error("phase4scalar %s is not set" % i)
                if avail_phases:
                    phase = avail_phases[0]
                    self.update_keyword('phase4scalar', phase, args=[i])
            if phase is not None and not(isinstance(phase, int)
                                         and 0<=phase<=len(self.solids)):
                self.error("Invalid phase4scalar(%s) %s" % (i, phase),
                           popup=True)
                self.unset_keyword('phase4scalar', args=[i])
                phase = None
            tw.setItem(i-1, COLUMN_PHASE, make_item('None' if phase is None
                                                    else phase_names[phase]))
        cb = ui.combobox_scalar_phase
        # Number or name of phases may have changed
        cb.clear()
        for i in avail_phases:
            cb.addItem(phase_names[i], i)

        self.fixup_scalars_table()
        row = get_selected_row(tw)
        # Autoselect if only 1 row
        if row is None and tw.rowCount() == 1:
            row = 0
            tw.setCurrentCell(row, COLUMN_NAME)
        enabled = (row is not None)
        for item in (ui.toolbutton_delete,
                     ui.bottom_frame):
            item.setEnabled(enabled)

        if row is not None:
            phase = self.project.get_value('phase4scalar', args=[row+1])
            if phase in avail_phases:
                cb.setCurrentIndex(avail_phases.index(phase))
        # Can't define scalars if there are no valid phases
        #  We really shouldn't be here in that case, the pane
        #  should be disabled
        ui.toolbutton_add.setEnabled(bool(avail_phases))


    def scalars_update_enabled(self):
        if self.project.get_value('nscalar', default=0) > 0:
            # Never disable if there are scalars defined
            disabled = False
        else:
            disabled = not bool(self.scalars_get_avail_phases())
        self.find_navigation_tree_item("Scalars").setDisabled(disabled)


    def scalars_get_avail_phases(self):
        #-  The fluid phase should only be available when the fluid solver
        #is enabled (RO_g0 /= 0).
        #-  TFM solids phases should be listed when defined.
        #-  DEM and PIC solids phases should never be listed.
        avail_phases = []
        ro_g0 = self.project.get_value('ro_g0')
        if ro_g0 != 0:
            avail_phases.append(0)
        for i in range(1, 1+len(self.solids)):
            if self.project.get_value('solids_model', args=[i]) == 'TFM':
                avail_phases.append(i)
        return avail_phases


    def fixup_scalars_table(self, stretch_column=COLUMN_NAME):
        ui = self.ui.scalars
        hv = QHeaderView
        tw = ui.tablewidget_scalars # main table, adjust top splitter
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


    def add_scalar(self):
        # Define a new scalar, increment nscalar and set phase4scalar to default
        # New row is always added at end
        ui = self.ui.scalars
        tw = ui.tablewidget_scalars
        nscalar = self.project.get_value('nscalar', default=0)
        nscalar += 1
        self.update_keyword('nscalar', nscalar)
        tw.setRowCount(nscalar)
        row = nscalar - 1
        name = "Scalar %s" % nscalar
        def make_item(val):
            item = QTableWidgetItem(str(val))
            set_item_noedit(item)
            return item
        tw.setItem(row, COLUMN_NAME, make_item(name))
        tw.setItem(row, COLUMN_ID, make_item(nscalar))
        avail_phases = self.scalars_get_avail_phases()
        if avail_phases:
            phase = avail_phases[0]
        else:
            phase = None
        if phase is not None:
            self.update_keyword('phase4scalar', avail_phases[0], args=[nscalar])
        phase_names = [self.fluid_phase_name] + list(self.solids.keys())
        tw.setItem(row, COLUMN_PHASE, make_item("None" if phase is None else phase_names[phase]))
        self.fixup_scalars_table()
        # Autoselect the new addition
        tw.setCurrentCell(row, COLUMN_NAME)

        # Need to set scalar-related defaults for BCs and ICs
        for BC in self.bcs.keys():
            self.bcs_set_default_keys(BC)
        for IC in self.ics.keys():
            self.ics_set_default_keys(IC)


    def delete_scalar(self):
        if not self.current_scalar:
            return # Nothing to do
        nscalar = self.project.get_value('nscalar', default=0)
        if nscalar <= 0:
            return
        nscalar -= 1
        self.update_keyword('nscalar', nscalar)
        self.scalar_delete_indices(self.current_scalar, 1)
        if self.current_scalar in self.scalar_names:
            del self.scalar_names[self.current_scalar]
        for (k,v) in list(self.scalar_names.items()):
            if k > self.current_scalar:
                self.scalar_names[k-1] = self.scalar_names[k]
                del self.scalar_names[k]

        # Just recreate the table
        self.setup_scalars()


    def set_scalar_name(self, widget, vals, args):
        if not self.current_scalar:
            return
        i = self.current_scalar
        name = vals['scalar_name']
        if not name:
            name = "Scalar %s" % i
        self.scalar_names[i] = name
        row = i - 1
        self.ui.scalars.tablewidget_scalars.item(row,COLUMN_NAME).setText(name)
        key = 'scalar_name(%s)' % i
        if self.project.mfix_gui_comments.get(key) != name:
            self.project.mfix_gui_comments[key] = name
            self.set_unsaved_flag()


    def set_scalar_phase(self, phase):
        if not self.current_scalar:
            return
        i = self.current_scalar
        row = self.current_scalar - 1
        self.update_keyword('phase4scalar', phase, args=[i])
        phase_names = [self.fluid_phase_name] + list(self.solids.keys())
        self.ui.scalars.tablewidget_scalars.item(row,COLUMN_PHASE).setText(phase_names[phase])


    def handle_scalars_selection(self):
        ui = self.ui.scalars
        tw = ui.tablewidget_scalars
        row = get_selected_row(tw)
        enabled = (row is not None)
        for item in (ui.toolbutton_delete, ui.bottom_frame):
            item.setEnabled(enabled)
        name = tw.item(row, COLUMN_NAME).text() if enabled else ''
        ui.lineedit_scalar_name.setText(name)
        if not enabled: # Nothing more to do
            self.current_scalar = None
            return
        self.current_scalar = row + 1
        phase = self.project.get_value('phase4scalar',
                                       args=[self.current_scalar])
        cb = ui.combobox_scalar_phase
        for i in range(cb.count()):
            item = get_combobox_item(cb, i)
            if item.data(UserRole) == phase:
                cb.setCurrentIndex(i)
                break
        else:
            self.error("Invalid phase4scalar(%s) = %s" % (self.current_scalar,
                                                          phase),
                       popup=True)


    def scalar_delete_indices(self, start_index, num_indices=1):
        """Delete all keywords associated with specified scalar index (1-based),
        fixing up the resulting gap in sequence"""
        # We assume that nscalar is already decremented
        nscalar = self.project.get_value('nscalar', default=0)
        prev_size = nscalar + num_indices # Size before deletion
        for key in keyword_args.keys_by_type['scalar']:
            arg_types = keyword_args.keyword_args[key]
            indices = self.project.get_key_indices(key)
            if not indices:
                continue
            if arg_types == ('scalar',):
                # Copy/slide/trim
                vals = [self.project.get_value(key, args=i)
                        for i in range(1, 1+prev_size)]
                del vals[start_index-1:start_index+num_indices-1] # Slide (1-based)
                for (i, val) in enumerate(vals, 1):
                    self.update_keyword(key, val, args=i)
                for i in range(nscalar+1, prev_size+1):
                    self.unset_keyword(key, args=[i]) #Trim off the end

            else:
                scalar_pos = arg_types.index('scalar')
                new_vals = {}
                for args in indices:
                    args_scalar = args[scalar_pos]
                    if args_scalar < start_index: # Unaffected
                        continue
                    if start_index <= args_scalar <= start_index + num_indices:
                        continue # skip the scalars we're deleting
                    new_args = list(args)
                    if args_scalar > start_index + num_indices:
                        new_args[scalar_pos] -= num_indices #Slide along 'scalar' axis
                    new_vals[tuple(new_args)] = self.project.get_value(key, args=args)
                for (args, val) in new_vals.items():
                    self.update_keyword(key, val, args=args)
                for args in indices: # Trim
                    key_scalar = args[scalar_pos]
                    if key_scalar > nscalar:
                        self.unset_keyword(key, args)
        # Deal with memoized BC scalar_eq_type data
        # This should perhaps be done in bcs.py since it depends on BC internals
        for (region, data) in self.bcs.items():
            memo_dict = data.get('scalar_eq_type')
            if memo_dict:
                for (index, eq_type) in list(memo_dict.items()): # Going to modify dict
                    if index < start_index: # Unaffected
                        continue
                    if index > start_index+num_indices:
                        memo_dict[index-num_indices] = eq_type
                    del memo_dict[index]


    def scalar_insert_indices(self, start_index, num_indices=1):
        """Re-index keywords which depend on a scalar index after adjusting
        number of scalar equations.  Dual to scalar_delete_indices"""

        # We assume that nscalar is already incremented
        nscalar = self.project.get_value('nscalar', default=0)
        for key in keyword_args.keys_by_type['scalar']:
            arg_types = keyword_args.keyword_args[key]
            indices = self.project.get_key_indices(key)
            if not indices:
                continue

            if arg_types == ('scalar',):
                for i in range(nscalar, start_index-1, -1):
                    # Bump index up by num_indices
                    val = self.project.get_value(key, args=[i])
                    self.update_keyword(key, val, args=[i+num_indices])
                # Clear out the new range.  Special-case 'phase4scalar' since
                # that will get set right away
                if key != 'phase4scalar':
                    for i in range(start_index, start_index+num_indices):
                        self.unset_keyword(key, args=[i])

            else:
                scalar_pos = arg_types.index('scalar')
                new_vals = {}
                for args in indices:
                    args_scalar = args[scalar_pos]
                    if args_scalar < start_index: # Unaffected by insertion
                        continue
                    new_args = list(args)
                    new_args[scalar_pos] += num_indices #Slide along 'scalar_pos' axis
                    new_vals[tuple(new_args)] = self.project.get_value(key, args=args)
                for (args, val) in new_vals.items():
                    self.update_keyword(key, val, args=args)
                for args in indices: # Trim
                    args_scalar = args[scalar_pos]
                    if start_index <= args_scalar < start_index+num_indices:
                        self.unset_keyword(key, args)

        # Deal with memoized BC scalar_eq_type data
        # This should perhaps be done in bcs.py since it depends on BC internals
        for (region, data) in self.bcs.items():
            memo_dict = data.get('scalar_eq_type')
            if memo_dict:
                for (index, eq_type) in list(memo_dict.items()): # Going to modify dict
                    if index >= start_index:
                        memo_dict[index+num_indices] = eq_type
                        del memo_dict[index]


    def delete_scalars_of_phase(self, phase):
        nscalar = self.project.get_value('nscalar', default=0)
        if nscalar <= 0:
            return
        for index in range(nscalar, 0, -1): # reverse order since we're deleting
            phase4scalar = self.project.get_value('phase4scalar', args=[index])
            if phase4scalar == phase:
                nscalar -= 1
                self.update_keyword('nscalar', nscalar)
                self.scalar_delete_indices(index, 1)
                if index in self.scalar_names:
                    del self.scalar_names[index]
                for (k,v) in list(self.scalar_names.items()):
                    if k > index:
                        self.scalar_names[k-1] = self.scalar_names[k]
                        del self.scalar_names[k]
        if self.current_scalar is not None:
            if self.current_scalar > nscalar:
                self.current_scalar = nscalar or None # 0 is not valid
