"""Regions popup for MFIX-GUI
used for initial & boundary conditions"""

# Note, throughout this module, the abbreviation 'is_' means "internal surface", not "is"

import os
from collections import OrderedDict

from qtpy.QtCore import Qt, Signal

from qtpy.QtWidgets import (QAbstractItemView, QDialog,
                            QHeaderView, QTableWidgetItem)

from mfixgui.constants import *

from mfixgui.tools.qt import (get_combobox_item, get_selected_rows,
                              item_enabled, set_combobox_tooltip, get_ui,
                              set_item_enabled, set_item_noedit)

UserRole = Qt.UserRole

CELL, PARTICLE, FORCE, GEOMETRY = range(4)

def resize_column(table, col, flags):
    table.horizontalHeader().setSectionResizeMode(col, flags)

class RegionsPopup(QDialog):
    """Popup widget for selecting region, used by BCs/ICs/PSs/ISs/VTK/Monitors"""
    save = Signal()
    cancel = Signal()

    def handle_regions_selection(self):
        ui = self.ui
        tw = ui.table
        cb = ui.combobox
        cb2 = ui.combobox_2

        selections = get_selected_rows(tw)

        buttonbox = ui.buttonbox

        ok = bool(selections)
        if self.monitor and cb.currentIndex() == 0: # <Select type>
            ok = False
        buttonbox.button(buttonbox.Ok).setEnabled(ok)

        region_types = [tw.item(x, 1).text() for x in selections]
        types_match = len(set(region_types)) < 2

        if self.boundary:
            #  Pressure Inflow
            #     Not available for STL regions
            #     Not available for volume regions
            #  (also, chained regions must match type)
            disable = any(x in ('STL', 'box') for x in region_types) or not types_match
            item = get_combobox_item(cb, PRESSURE_INFLOW)
            set_item_enabled(item, not disable)

            # Volume BCs are only allowed for walls
            disable = any(x == 'box' for x in region_types) or not types_match
            for idx in (MASS_INFLOW, PRESSURE_OUTFLOW, MASS_OUTFLOW):
                item = get_combobox_item(cb, idx)
                set_item_enabled(item, not disable)

            # Don't stay on disabled item
            if not item_enabled(get_combobox_item(cb, cb.currentIndex())):
                cb.setCurrentIndex(BC_TYPES.index(DEFAULT_BC_TYPE))

            bc_type = BC_TYPES[cb.currentIndex()]
            if bc_type == 'CYCLIC':
                for i in range(cb.count()):
                    item = get_combobox_item(cb, i)
                    item.setEnabled('cyclic' in item.text().lower())
            else:
                for i in range(cb.count()):
                    item = get_combobox_item(cb, i)
                    if 'cyclic' in item.text().lower():
                        item.setEnabled(False)

            # Wall type boundary
            if bc_type.endswith('W'):
                self.handle_type(cb.currentIndex())
            # For inflows/outflows, only allow compatible orientation
            else:
                if len(selections) == 1:
                    region_type = tw.item(selections[0], 1).text()
                    for i in range(0, tw.rowCount()):
                        if i == selections[0]:
                            continue
                        enable = (tw.item(i, 1).text() == region_type)
                        self.enable_row(i, enable)
                elif len(selections) == 0:
                    self.handle_type(cb.currentIndex())
                else:
                    pass

        elif self.surface:
            # IS regions can be planes or volumes (not points)
            # *-Axis permeable/semipermeable not available for planes
            # STL allowed for moveable surface
            is_plane = selections and any('plane' in tw.item(x, 1).text()
                                          for x in selections)
            is_stl = selections and any (tw.item(x,1).text()=='STL'
                                         for x in selections)
            is_box = selections and any (tw.item(x,1).text()=='box'
                                         for x in selections)
            for index in (X_SEMIPERMEABLE, Y_SEMIPERMEABLE, Z_SEMIPERMEABLE,
                          X_IMPERMEABLE, Y_IMPERMEABLE, Z_IMPERMEABLE):
                item = get_combobox_item(cb, index)
                set_item_enabled(item, is_box or not selections)
            for index in (SEMIPERMEABLE, IMPERMEABLE):
                item = get_combobox_item(cb, index)
                set_item_enabled(item, is_plane or not selections)
            item = get_combobox_item(cb, MOVEABLE)
            set_item_enabled(item, is_stl or not selections)


        elif self.monitor:
            cell = ui.combobox_2.currentIndex() == 0
            if cell:
                monitor_types = MONITOR_TYPES_CELL
                monitor_types_point = MONITOR_TYPES_CELL_POINT
                monitor_types_plane = MONITOR_TYPES_CELL_PLANE
                monitor_types_volume = MONITOR_TYPES_CELL_VOLUME
                offset = 0
            else:
                monitor_types = MONITOR_TYPES_PARTICLE
                monitor_types_point = MONITOR_TYPES_PARTICLE_POINT
                monitor_types_plane = MONITOR_TYPES_PARTICLE_PLANE
                monitor_types_volume = MONITOR_TYPES_PARTICLE_VOLUME
                offset = 101
            if len(selections) == 1:
                region_type = tw.item(selections[0], 1).text()
                for monitor_type in monitor_types:
                    point = monitor_type in monitor_types_point
                    plane = monitor_type in monitor_types_plane
                    volume = monitor_type in monitor_types_volume
                    enable = (point and region_type == 'point'
                              or plane and 'plane' in region_type
                              or volume and region_type == 'box')

                    item = get_combobox_item(cb, 1+monitor_type-offset)
                    set_item_enabled(item, enable)
                set_item_enabled(get_combobox_item(cb, 0), False)

            elif len(selections) == 0:
                set_item_enabled(get_combobox_item(cb, 0), True)
                for monitor_type in monitor_types:
                    item = get_combobox_item(cb, 1+monitor_type-offset)
                    set_item_enabled(item, True)
                set_item_enabled(get_combobox_item(cb, 0), True)
                self.handle_type(self.ui.combobox.currentIndex())


    def handle_type(self, val):
        if val == -1:
            return
        tw = self.ui.table
        selections = get_selected_rows(tw)
        target = tw.item(selections[0], 1).text() if selections else None
        buttonbox = self.ui.buttonbox
        cb = self.ui.combobox
        set_combobox_tooltip(cb)
        if self.boundary:
            if not selections:
                for i in range(cb.count()):
                    get_combobox_item(cb, i).setEnabled(True)
            bc_type = BC_TYPES[val]
            if bc_type.endswith('W'):
                for i in range(tw.rowCount()):
                    text = tw.item(i, 1).text()
                    enable = 'cyclic' not in text
                    self.enable_row(i, enable)

            elif bc_type == 'PI':
                #    Not available for volume regions
                #    Not available for STL regions
                for i in range(tw.rowCount()):
                    text = tw.item(i, 1).text()
                    enable = ('plane' in text) and (target is None or text == target)
                    self.enable_row(i, enable)

            elif bc_type in ('MI', 'PO', 'MO'):
                #    Not available for volume regions
                for i in range(tw.rowCount()):
                    text = tw.item(i, 1).text()
                    enable = (('plane' in text or text == 'STL')
                              and (target is None or text==target))
                    self.enable_row(i, enable)
            elif bc_type == 'CYCLIC':
                tw.clearSelection()
                for i in range(tw.rowCount()):
                    text = tw.item(i, 1).text()
                    enable = 'cyclic' in text
                    self.enable_row(i, enable)
            else:
                self.error("Unknown bc_type %s" % bc_type)
            tw.setSelectionMode(QAbstractItemView.SingleSelection if bc_type == 'CYCLIC'
                                else QAbstractItemView.MultiSelection)

        elif self.surface:
            tw.setSelectionMode(QAbstractItemView.MultiSelection)
            is_type = IS_TYPES[val]
            need_plane = is_type in ('IMPERMEABLE', 'SEMIPERMEABLE')
            need_box = is_type.startswith(('X_','Y_','Z_'))
            need_stl = is_type == 'STL'
            for i in range(tw.rowCount()):
                shape = tw.item(i, 1).text()
                enable = ((need_box and shape == 'box')
                          or (need_plane and ('plane' in shape))
                          or (need_stl and shape=='STL'))
                self.enable_row(i, enable)

        elif self.monitor:
            buttonbox.button(buttonbox.Ok).setEnabled(val != 0 and bool(selections))
            enable_all = (val == 0) # <Select type>
            monitor_type = val - 1
            output_type = self.ui.combobox_2.currentIndex()
            if output_type == 0: # Cell
                point = monitor_type in MONITOR_TYPES_CELL_POINT
                plane = monitor_type in MONITOR_TYPES_CELL_PLANE
                volume = monitor_type in MONITOR_TYPES_CELL_VOLUME
            elif output_type == 1: #Particle
                monitor_type += 101
                point = monitor_type in MONITOR_TYPES_PARTICLE_POINT
                plane = monitor_type in MONITOR_TYPES_PARTICLE_PLANE
                volume = monitor_type in MONITOR_TYPES_PARTICLE_VOLUME
            else:
                return
            for i in range(tw.rowCount()):
                text = tw.item(i, 1).text()
                enable = enable_all or (point and text == 'point'
                                        or plane and 'plane' in text
                                        or volume and text == 'box')
                self.enable_row(i, enable)

    def handle_type_2(self, val): #Cell/Particle select for Monitor & VTK
        gui = self.parent
        if self.monitor:
            cb = self.ui.combobox
            cb.clear()
            cb.addItem('<Select type>')
            if val == 0:
                names = MONITOR_TYPE_CELL_NAMES
                offset = 0
            elif val == 1:
                names = MONITOR_TYPE_PARTICLE_NAMES
                offset = 101
            elif val > 1: #GEO/FORCE
                # Shouldn't get here
                return

            cb.addItems(names)
            for i in range(1, len(cb)):
                gui.add_tooltip(get_combobox_item(cb, i), key='monitor_type', value=i-1+offset)

        elif self.vtk:
            tw = self.ui.table
            for i in range(0, tw.rowCount()):
                shape = tw.item(i,1).text()
                self.enable_row(i, (shape=='STL' and val==GEOMETRY)
                                or (shape!='STL' and val in (CELL, PARTICLE))
                                or (shape=='box' and val==FORCE))


    def enable_row(self, i, enable):
        tw = self.ui.table
        enable = enable and tw.item(i, 2).data(UserRole) # Initial enable setting
        tw.item(i, 2).setText('Yes' if enable else 'No')
        for j in (0, 1, 2):
            set_item_enabled(tw.item(i, j), enable)


    def reset_signals(self):
        for sig in (self.cancel, self.save):
            try:
                sig.disconnect()
            except:
                pass


    def __init__(self, parent=None):
        super(RegionsPopup, self).__init__(parent)
        self.parent = parent
        self.ui = get_ui('regions_popup.ui', self)

        # key=region name, val=data dict
        self.defined_regions = OrderedDict()

        buttons = self.ui.buttonbox.buttons()
        buttons[0].clicked.connect(self.save.emit)
        buttons[1].clicked.connect(self.cancel.emit)
        #self.ui.table.doubleClicked.connect(self.save.emit) # Too easy to double-click accidentally
        #self.ui.table.doubleClicked.connect(self.accept)
        self.ui.table.itemSelectionChanged.connect(self.handle_regions_selection)
        self.ui.combobox.currentIndexChanged.connect(self.handle_type)
        self.ui.combobox_2.currentIndexChanged.connect(self.handle_type_2) # Cell/particle/geo
        self.rejected.connect(self.cancel.emit)
        self._mousePressEvent = self.ui.table.mousePressEvent
        self.ui.table.mousePressEvent = self.mousePressEvent
        self.combobox_height = None # Update on first show

    def clear(self):
        self.ui.table.clearContents()
        self.ui.table.setRowCount(0)


    def add_row(self, row):
        table = self.ui.table
        nrows = table.rowCount()
        table.setRowCount(nrows+1)
        def make_item(val, enabled):
            item = QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            set_item_enabled(item, enabled)
            return item
        (name, shape, available) = row
        table.setItem(nrows, 0, make_item(name, available))
        table.setItem(nrows, 1, make_item(shape, available))
        table.setItem(nrows, 2, make_item('Yes' if available else 'No', available))
        # Keep track of original 'available' so we can restore
        table.item(nrows, 2).setData(UserRole, available)


    def popup(self, label_text):
        # Widget is shared by ICs/BCs/PSs/ISs/VTK output
        # NB, we distinguish the caller based on the label text
        ui = self.ui
        ui.label_top.setText(label_text)

        buttonbox = self.ui.buttonbox
        buttonbox.button(buttonbox.Ok).setEnabled(False)

        self.boundary = boundary = ('boundary condition' in label_text)
        self.surface = surface = ('internal surface' in label_text)
        self.vtk = vtk = ('VTK output' in label_text)
        self.monitor = monitor = ('monitor' in label_text)
        ui.combobox_2.view().setRowHidden(2, not self.vtk) # Force data
        ui.combobox_2.view().setRowHidden(3, not self.vtk) # Geometry data
        if ui.combobox_2.currentIndex() > 1 and not self.vtk:
            ui.combobox_2.setCurrentIndex(0)
        tw = ui.table

        if (monitor or vtk): # Output filenames will collide if we allow multiple regions
            tw.setSelectionMode(QAbstractItemView.SingleSelection)
            self.setWindowTitle("Select region")
        else:
            tw.setSelectionMode(QAbstractItemView.MultiSelection)
            self.setWindowTitle("Select regions")

        # setup the combobox appropriately
        cb = ui.combobox
        cb2 = ui.combobox_2
        label = ui.label_type
        gui = self.parent
        show_type = False
        if boundary or surface:
            label.setText('Boundary type' if boundary else 'Surface type')
            cb.clear()
            cb.addItems(BC_NAMES if boundary else IS_NAMES)
            if boundary:
                for i in range(len(cb)):
                    gui.add_tooltip(get_combobox_item(cb, i), key='bc_type', value=BC_TYPES[i])
                gui.add_tooltip(get_combobox_item(cb, BC_TYPES.index('CYCLIC')),
                                key=None, description='Cyclic boundary condition in specified direction')
            if surface:
                for i in range(len(cb)):
                    gui.add_tooltip(get_combobox_item(cb, i), key='is_type', value=IS_TYPES[i])

            index = BC_TYPES.index(DEFAULT_BC_TYPE) if boundary else IS_TYPES.index(DEFAULT_IS_TYPE)
            cb.setCurrentIndex(index)
            self.handle_type(index)
            show_type = True
        elif vtk:
            # slight hack to set tooltips
            get_combobox_item(cb2, 0).setToolTip('Cell data (VTU file).')
            get_combobox_item(cb2, 1).setToolTip('Particle data (VTP file).' )
            get_combobox_item(cb2, 2).setToolTip('Force chain data.')
            get_combobox_item(cb2, 3).setToolTip('Geometry data (VTU file).  Requires STL region.')
            show_type = True
            self.handle_type_2(cb2.currentIndex())
        elif monitor:
            label.setText('Monitor type')
            cb.clear()
            cb.addItem('<Select type>')
            if cb2.currentIndex() == 0: # Cell
                cb.addItems(MONITOR_TYPE_CELL_NAMES)
                for i in range(1, len(cb)):
                    gui.add_tooltip(get_combobox_item(cb, i), key='monitor_type', value=i-1)
            elif cb2.currentIndex() == 1:# Particle
                cb.addItems(MONITOR_TYPE_PARTICLE_NAMES)
                for i in range(1, len(cb)):
                    gui.add_tooltip(get_combobox_item(cb, i), key='monitor_type', value=i+100)

            get_combobox_item(cb2, 0).setToolTip('Cell data.')
            get_combobox_item(cb2, 1).setToolTip(
                'Particle data.' if get_combobox_item(cb2,1).isEnabled()
                else 'Particle data.<br>Not available for TFM solids.')
            show_type = True

        if show_type:
            ui.frame_object_type.show()
            set_combobox_tooltip(cb)
            nrows = 2
        else:
            ui.frame_object_type.hide()
            nrows = 1

        if monitor:
            nrows += 1
        for item in (ui.combobox_2, ui.label_2):
            item.setVisible(bool(monitor or vtk))
        for item in (ui.combobox, ui.label_type):
            item.setVisible(not vtk)

        self.show()
        # Can't get the combobox to the size I want.  For now
        # I'm just padding the label above it to make the window
        # wide enough
        #cb.setMinimumWidth(cb.minimumSizeHint().width())
        #ui.adjustSize()
        # Table fixup
        tw.resizeColumnToContents(0)
        #tw.resizeColumnToContents(1)
        #tw.resizeColumnToContents(2)
        resize = tw.horizontalHeader().setSectionResizeMode
        for i in (0, 1, 2):
            resize(i, QHeaderView.Fixed)
        #resize(2, QHeaderView.ResizeToContents)

        table_height = 4 + tw.horizontalHeader().height() + (tw.rowHeight(0)*tw.rowCount())

        min_table_height = 150
        tw.setMinimumHeight(min(min_table_height, table_height))
        tw.setMaximumHeight(table_height)

        # Height of all stuff other than the main table - try to prevent scrollbars
        margins = 10 + 5 # Top + bottom
        line_spacing = 6 # Should get this from UI
        if self.combobox_height is None: # Cache this because it changes (?)
            self.combobox_height = self.combobox.size().height()
        stuff_height = (nrows+1) * self.combobox_height + margins + (nrows+2)*line_spacing
        height = (table_height + stuff_height)

        self.setMinimumHeight(min(min_table_height+stuff_height, height))
        self.setMaximumHeight(height)

        width = (sum(tw.columnWidth(i) for i in (0,1,2))
                 + tw.verticalScrollBar().isVisible() * (4+tw.verticalScrollBar().width()))
        extra = ui.label_top.width() + 10 - width
        if extra > 0:
            width += extra

        self.setMaximumWidth(width)
        self.setMinimumWidth(width)
        self.resize(width, min(height, 600)) # Don't make popup too tall for screen
        # Allocate any extra space to first column of table
        if extra > 0:
            tw.setColumnWidth(0, tw.columnWidth(0)+extra)
        self.raise_()
        self.activateWindow()


    def get_selection_list(self):
        """return list of selected region names"""
        rows = list(set([i.row() for i in self.ui.table.selectedItems()]))
        rows.sort()
        names = [self.ui.table.item(r, 0).text() for r in rows]
        return names


    def mousePressEvent(self, ev):
        # Allow deselecting selected row when in SingleSelection mode
        # by clicking again, on selected row, or on any inactive area
        tw = self.ui.table
        prev = list(set([i.row() for i in tw.selectedItems()]))
        r = self._mousePressEvent(ev)
        if tw.selectionMode() == QAbstractItemView.SingleSelection:
            rows = list(set([i.row() for i in tw.selectedItems()]))
            if len(rows)==1 and rows==prev:
                tw.setCurrentCell(-1,-1)
        return r
