# -*- coding: utf-8 -*-

from qtpy.QtCore import QTimer, QPoint, Qt
from qtpy.QtWidgets import (QToolTip, QComboBox, QLineEdit,
                            QCheckBox, QSpinBox, QLabel,
                            QStackedWidget, QScrollArea,
                            QTreeWidgetItem)

from qtpy.QtGui import QCursor, QColor

from mfixgui.constants import *
from mfixgui.advanced_keys import advanced_keys
from mfixgui.tools.qt import get_selected_row, widget_iter
from mfixgui.tools.keyword_args import keyword_args


UserRole = Qt.UserRole

# BCs/ICs etc
FLUID_TAB = 0
#SOLIDS_TAB_DUMMY_L = 1
SOLIDS_TAB = 2
#SOLIDS_TAB_DUMMY_R = 3
SCALAR_TAB = 4
CYCLIC_TAB = 5
TRANSIENT_TAB = 6

# Output pane
BASIC_TAB = 0

# combobox_bc_type
(MASS_INFLOW, PRESSURE_OUTFLOW,
 NO_SLIP_WALL, FREE_SLIP_WALL, PARTIAL_SLIP_WALL, MIXED_WALL,
 PRESSURE_INFLOW, MASS_OUTFLOW, CYCLIC_BOUNDARY) = range(9)

match_color = 'lightgreen'
err_color = 'orangered'

def locate_keyword(self, key, args=None):
    global gui
    gui = self
    return simple_search(key, args)


bcs_solids_keys_nophase = {'bc_dt_0', 'bc_jj_ps', 'e_w', 'phi_w', 'phip', 'phip0'}
ics_solids_keys_nophase = {'ic_des_fit_to_region', 'ic_p_star'}
vtk_keys_nophase = {'vtk_p_star'}
monitor_keys_nophase = {'monitor_p_star'}
# *_keys_nophase found by doing:
# [w.key for w in widget_iter(_.page_solids) if hasattr(w,'key') and
#       w.key in keyword_args and 'phase' not in keyword_args[w.key]]

solids_keys_nophase = set().union(bcs_solids_keys_nophase,
                                  ics_solids_keys_nophase,
                                  vtk_keys_nophase,
                                  monitor_keys_nophase)

def simple_search(key, args):
    # Return a 2-tuple, status:bool, err_msg:string or Noned
    found = False
    first = True
    err = None
    arg_types = keyword_args.get(key) or ()
    if not key:
        return False, "No key specified"

    # Advanced keys
    if key in gui.project.usr_keyword_doc or key in advanced_keys:
        gui.change_pane('advanced')
        # TODO highlight row or '+' button
        return True, None

    prefix = key.split('_',1)[0]
    # These panes have per-phase solids sub-panes
    if ('phase' in arg_types and prefix in ('bc', 'ic', 'ps', 'is',
                                            'vtk', 'monitor')
        or key in solids_keys_nophase):

        if not gui.solids:
            gui.change_pane('solids') # XXX Is this going too far?
            return False, "No solids phases defined"

    if 'scalar' in arg_types and prefix in ('bc', 'ic', 'vtk',
                                            'monitor'):
        nscalar = gui.project.get_value('nscalar', default=0)
        if nscalar == 0:
            gui.change_pane('scalars') # XXX Is this going too far?
            return False, "No scalar equations defined"
        # Could use some better OOP here.  This suggests how the classes might look
        if prefix == 'ic':
            gui.setup_ics_scalar_tab() # gui.ics.setup_scalar_tab, or even:
                                       # gui.ics.setup_tab(SCALAR_TAB)
                                       # This requires changing current meaning of .ics, .bcs etc
        elif prefix == 'bc':
            gui.setup_bcs_scalar_tab() # gui.bcs. etc.
        elif prefix == 'monitor':
            gui.setup_monitors_scalar_tab()
        elif prefix == 'vtk':
            gui.setup_output_scalar_subtab() # NB subtab

    if 'vtk' in arg_types:
        if not gui.project.get_value('write_vtk_files', default=False):
            pane = gui.ui.output
            gb = pane.groupbox_write_vtk_files
            gui.change_pane('output')
            gui.output_change_tab(BASIC_TAB)
            focus_widget(pane, gb)
            blink(gb, err_color)
            return False, "VTK outputs are disabled"

    # TODO look for fluid phase argument
    matches = []
    for pane in gui.ui.panes:
        for w in widget_iter(pane):
            wkeys = getattr(w, 'keys', None)
            if wkeys is None:
                wkeys = [getattr(w, 'key', None)]
            wname = w.objectName()
            if key in wkeys:
                matches.append(w)
            elif wname.startswith('label_'+key):
                # Blink labels but avoid false leading-substring matches
                # should we just set .keys for labels too (?)
                rest = wname[6+len(key):]
                if rest.startswith('_'):
                    rest = rest[1:].split('_')
                    while rest and rest[0].isdigit():
                        rest.pop(0)
                if not rest or rest=='units':
                    matches.append(w)
        if matches:
            # FIXME this assumes each key is in a single pane
            #  This is mostly true except for:  momentum_*_eq, species_eq, species_name (depr)
            break

    if not matches:
        return False, err

    # Don't flash e.g. both a groupbox and a widget in that groupbox
    for a in matches[:]:
        for b in matches[:]:
            if b==a:
                continue
            if a.isVisible() and is_ancestor(b, a):
                matches.remove(b)

    labels = [w for w in matches if isinstance(w, QLabel)]
    widgets = [w for w in matches if not w in labels]
    # Prefer widgets in open/current subtab (ep_star)
    if pane == gui.ui.solids:
        def which_tab(w):
            for (i, t) in enumerate([pane.materials, pane.TFM, pane.DEM, pane.PIC]):
                if is_ancestor(t, w):
                    return i
            return 0
        widgets.sort(key=lambda w: gui.solids_pushbuttons[which_tab(w)].isEnabled(), reverse=True)

    if pane == gui.ui.boundary_conditions:
        # Prefer currently selected row if type matches
        if gui.bcs_current_indices:
            BC0 = gui.bcs_current_indices[0]
            bc_type = gui.project.get_value('bc_type', args=[BC0])
            def page_name(w):
                return ('fluid' if is_ancestor(pane.page_fluid, w)
                        else 'solid' if is_ancestor(pane.page_solids, w)
                        else '')
            def prefer_selection(w):
                if not bc_type: # Cyclic BC?
                    return False #
                t1 = page_name(w)
                if not t1:
                    return False
                for t2 in 'W', 'I', 'PO', 'MO':
                    subpage = getattr(pane, 'page_%s_%s' % (t1, t2), None)
                    if is_ancestor(subpage, w):
                        if bc_type.endswith(t2):
                            return True
                return False
            widgets.sort(key=prefer_selection, reverse=True)

    if len(widgets) > 1:
        #print("Multiple widgets found", ', '.join(w.objectName() or str(w) for w in widgets))
        for w in widgets[:]:
            if w.isHidden():
                widgets.remove(w)

    if widgets:
        w = widgets[0]
    else:
        w = labels[0]
    err = setup_pane(pane, w, key, args)
    if err:
        return False, err
    for w in matches:
        blink(w)
    if widgets:
        w = widgets[0]
        focus_widget(pane, w)
    return bool(matches), err


def focus_widget(pane, widget):
    if gui.mode != 'modeler':
        gui.change_mode('modeler')
    w = widget
    w.setFocus(0)
    # Handle scrolling first.  This handles scrolling but not scrollable tables
    while w.parent():
        par = w.parent()
        if isinstance(par, QScrollArea):
            par.ensureWidgetVisible(widget)
            break
        #elif isinstance(par, QTableWidgetItem):
        #    par.ensureItemVisible()
        w = par

    # widget.mapToGlobal(0,0) returns coordinates which are offset,
    #  due to stacked widget animation (bug in Qt?)
    #  The following works around this bug
    xoff, yoff = 0, 0
    w = widget
    while w.parent():
        par = w.parent()
        if isinstance(par, QStackedWidget):
            p = w.mapTo(par, QPoint(0,0))
            xoff += p.x()
            yoff += p.y()
        w = par

    p0 = widget.mapToGlobal(QPoint(2, 2))
    p = QPoint(p0.x() - xoff,
               p0.y() - yoff)

    # Allow time for animation to complete.  This results
    # in tooltip being displayed, do we want that?
    #QTimer.singleShot(5000, lambda w=w: QCursor.setPos(p))
    QCursor.setPos(p)

    # Excessive!
    #if isinstance(w, QComboBox):
    #    QTimer.singleShot(1000, w.showPopup)


def set_style(widget, style, name):
    try:
        widget.setStyleSheet(style)
    except RuntimeError as e:
        #print(name, e)
        pass


def blink(w, color=match_color):
    # TODO:  if widget is a clickable groupbox, only blink
    #  the label/checkbox

    if isinstance(w, QTreeWidgetItem):
        # For blinking items in nav_tree
        bg = w.background(1) # Get bg color from column 1, in case column 0 color has changed already
        qcolor = QColor(color)
        w.setBackground(0, qcolor)
        for t in range(1,8):
            QTimer.singleShot(500*t, lambda w=w, c=bg if t%2 else qcolor:
                              w.setBackground(0, c))
    else:
        # All other widgets
        style = 'background: ' + color
        w.setStyleSheet(style)
        for t in range(1,8):
            QTimer.singleShot(500*t, lambda w=w, name=w.objectName(), s='' if t%2 else style:
                          set_style(w, s, name))



def is_ancestor(w1, w2):
    """Is widget w1 in the .parent() hierarchy of widget w2?"""
    if w1 == w2:
        return True
    while w2.parent():
        w2 = w2.parent()
        if w2 == w1:
            return True
    return False


def setup_pane(pane, widget, key, args=None):
    # args not handled yet
    if gui.mode != 'modeler':
        gui.change_mode('modeler')

    pane_name = pane.objectName()
    widget_name = widget.objectName()
    arg_types = keyword_args.get(key) or ()

    # Check for fluid solver disabled
    if gui.fluid_solver_disabled and (pane_name == 'fluid'):

        # Should we allow navigating to disabled pane?
        gui.change_pane('model setup')
        pane = gui.ui.model_setup
        cb = pane.checkbox_disable_fluid_solver
        focus_widget(pane, cb)
        blink(cb)
        return "Fluid solver is disabled"

    if not gui.change_pane(pane_name, allow_disabled_tab=True):
        item = gui.find_navigation_tree_item(pane_name)
        blink(item, err_color)
        return pane_name.title() + " pane is locked"

    if (gui.fluid_solver_disabled
        and pane_name in ('initial_conditions',
                          'boundary_conditions',
                          'point_sources')
        and is_ancestor(pane.page_fluid, widget)):
        pass # Fluid sub-tab disabled

    # Make sure we are on the correct subtab
    if pane_name == 'model':
        pass # No subtabs
    elif pane_name == 'geometry':
        pass
    elif pane_name == 'mesh':
        if is_ancestor(pane.background, widget):
            gui.change_mesh_tab(0)
        elif is_ancestor(pane.mesher, widget):
            gui.change_mesh_tab(1)
        else:
            return "no tab found"
    elif pane_name == 'regions':
        pass
    elif pane_name == 'fluid':
        # TODO args for fluid species, select row in table
        pass
    elif pane_name == 'solids':
        # TODO select row for phase if needed
        for (i,p) in enumerate([pane.materials, pane.TFM, pane.DEM, pane.PIC]):
            if is_ancestor(p, widget):
                gui.solids_change_tab(i)
                break
    elif pane_name == 'scalars':
        pass

    elif pane_name == 'initial_conditions':
        if 'scalar' in arg_types:
            gui.ics_change_tab(SCALAR_TAB)
        elif is_ancestor(pane.page_solids, widget):
            P = gui.ics_current_solid
            if P is None: # We need to change to solids tab
                # Use the first solid. We already checked for len(solids)>0
                gui.ics_change_tab(SOLIDS_TAB, 1)
            if key in ('ic_des_lattice',
                       'ic_des_rand',
                       'ic_des_rand_factor_x',
                       'ic_des_rand_factor_y',
                       'ic_des_rand_factor_z',
                       'ic_des_spacing',
                       'ic_des_space_factor_x',
                       'ic_des_space_factor_y',
                       'ic_des_space_factor_z'):
                gui.ui.initial_conditions.groupbox_dem_seeding.setChecked(True)
                gui.handle_ic_dem_seeding(True)

        elif is_ancestor(pane.page_fluid, widget):
            gui.ics_change_tab(FLUID_TAB)
        else:
            return "no tab found"

    elif pane_name == 'boundary_conditions':
        # The is the most difficult case.
        #  We are really reaching into the BCS pane here

        # Look for which tab a key belongs to
        ## Solids tab
        ####  XXX1 Not all solids keys have 'phase' argument, eg BC_JJ_PS
        ####  XXX2 Some BC_ keys are in solid and fluid, eg bc_dt_0
        if key == 'bc_type':
            pass # Not in a tab
        elif is_ancestor(pane.page_solids, widget):
            for (idx, target) in enumerate(['W', 'I', 'PO', 'MO']):
                subpage = getattr(pane, 'page_solids_%s'%target)
                if is_ancestor(subpage, widget):
                    break
            else:
                return "no subtab found"
            valid = False
            if (gui.bcs_current_solid is not None
                and idx == pane.page_solids.currentIndex()):
                valid = True  # Already on correct subtab
            else:
                # First try to find a row which is compatible type
                tw = pane.tablewidget_regions
                for row in range(tw.rowCount()):
                    indices, regions = tw.item(row,0).data(UserRole)
                    if indices:
                        bc_type = gui.project.get_value('bc_type', args=indices[0], default='X')
                        if bc_type.endswith(target):
                            valid = True
                            tw.setCurrentCell(row, 0) # TODO Ensure selection visible
                            if not gui.bcs_current_solid:
                                gui.bcs_change_tab(SOLIDS_TAB, 1)
                            break

            if not (valid and gui.bcs_current_solid):
                cb = pane.combobox_bc_type
                tw.clearSelection() # Do not stay on an invalid row, eg Cyclic
                cb.setCurrentIndex({'W': PARTIAL_SLIP_WALL,
                                    'I': MASS_INFLOW,
                                    'MO': MASS_OUTFLOW,
                                    'PO': PRESSURE_OUTFLOW}.get(target, cb.currentIndex()))
                pane.page_solids.setCurrentIndex(idx)
                gui.bcs_change_tab(SOLIDS_TAB, 1) # We already checked that there is at least 1 solids phase

        ## Scalar tab
        elif 'scalar' in arg_types:
            gui.bcs_change_tab(SCALAR_TAB)

        # Transient tab
        elif key in ('bc_mi_start_time', 'bc_mi_end_time'):
            gui.bcs_change_tab(TRANSIENT_TAB)

        ## Cyclic tab
        elif (key=='flux_g'
              or key.startswith('cyclic_')
              or key.startswith('delp_')):
            axis = key.split('_')[1]
            tw = pane.tablewidget_regions
            sel = get_selected_row(tw)
            rows = list(range(tw.rowCount()))
            if sel:
                rows.remove(sel)
                rows = [sel] + rows # Put selected row first to prefer it
            for row in rows:
                indices, regions = tw.item(row,0).data(UserRole)
                idx = indices[0]
                if gui.bc_is_cyclic(idx):
                    bc_axis = idx[0].lower()
                    if key=='flux_g' or axis==bc_axis:
                        tw.setCurrentCell(row, 0)
                        break
            else:
                tw.clearSelection()
                gui.bcs_change_tab(CYCLIC_TAB) # Show disabled tab

        ## Fluid tab (everything else)
        elif is_ancestor(pane.page_fluid, widget):
            for (idx, target) in enumerate(['W', 'I', 'PO', 'MO']):
                subpage = getattr(pane, 'page_fluid_%s'%target)
                if is_ancestor(subpage, widget):
                    break
            else:
                return "no subtab found"
            if gui.fluid_solver_disabled:
                return "fluid solver is disabled"
            valid = False
            if idx == pane.page_fluid.currentIndex():
                valid = True  # Already on correct subtab
            else:
                # First try to find a row which is compatible type
                tw = pane.tablewidget_regions
                for row in range(tw.rowCount()):
                    indices, regions = tw.item(row,0).data(UserRole)
                    if indices:
                        bc_type = gui.project.get_value('bc_type', args=indices[0], default='X')
                        if bc_type.endswith(target):
                            valid = True
                            tw.setCurrentCell(row, 0) # TODO Ensure selection visible
                            gui.bcs_change_tab(FLUID_TAB)
                            break

            if not valid:
                cb = pane.combobox_bc_type
                tw.clearSelection() # Do not stay on an invalid row, eg Cyclic
                cb.setCurrentIndex({'W': PARTIAL_SLIP_WALL,
                                    'I': MASS_INFLOW,
                                    'MO': MASS_OUTFLOW,
                                    'PO': PRESSURE_OUTFLOW}.get(target, cb.currentIndex()))
                pane.page_fluid.setCurrentIndex(idx)
                gui.bcs_change_tab(FLUID_TAB)
        else:
            return "no tab found"

    elif pane_name == 'point_sources':
        if 'phase' in arg_types:
            P = gui.pss_current_solid
            if P is None:
                gui.pss_change_tab(SOLIDS_TAB, 1)
        else:
            gui.pss_change_tab(FLUID_TAB)

    elif pane_name == 'numerics':
        for (i,p) in enumerate((pane.residuals, pane.discretization,
                                pane.linear_solver, pane.preconditioner,
                                pane.advanced)):
            if is_ancestor(p, widget):
                gui.numerics_change_tab(i)
                break

    elif pane_name == 'output':
        for (i, p) in enumerate((pane.page_basic,
                                 pane.page_vtk,
                                 pane.page_spx,
                                 pane.page_residuals,
                                 pane.page_log,
                                 pane.page_usr)):
            if is_ancestor(p, widget):
                gui.output_change_tab(i)
                break

    # Check for hidden widget
    if widget.isHidden():
        msg = getattr(widget, 'hidden_msg', 'control is hidden')
        ctrl = getattr(widget, 'hidden_ctrl', None)
        if ctrl:
            blink(ctrl, err_color)
            focus_widget(pane, ctrl)
        return msg
