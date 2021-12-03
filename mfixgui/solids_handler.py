# methods to deal with solids, split off from mfixgui.py

from copy import deepcopy
from collections import OrderedDict

from json import JSONDecoder

from qtpy import QtWidgets
from qtpy.QtCore import Qt, QTimer

from mfixgui.animations import animate_stacked_widget
from mfixgui.constants import *
from mfixgui.widgets.base import LineEdit

from mfixgui.tools import (append_row_column_triangular, drop_row_column_triangular,
                           format_key_with_args, keyword_args, safe_float)
from mfixgui.tools.qt import (get_combobox_item, get_selected_row,
                              set_combobox_tooltip, set_item_enabled,
                              set_item_noedit, widget_iter, get_icon,
                              set_alignment, sub_icon_size, main_icon_size)

from mfixgui.species_handler import SpeciesHandler
from mfixgui.solids_tfm import SolidsTFM
from mfixgui.solids_dem import SolidsDEM
from mfixgui.solids_pic import SolidsPIC

from mfixgui import default_values

TAB_MATERIALS, TAB_TFM, TAB_DEM, TAB_CGP, TAB_PIC = range(5)

class SolidsHandler(SolidsTFM, SolidsDEM, SolidsPIC, SpeciesHandler):

    def init_solids_default_models(self):
        self.solids_density_model = CONSTANT
        self.solids_viscosity_model = OTHER
        self.solids_mol_weight_model = MIXTURE
        self.solids_specific_heat_model = CONSTANT


    def init_solids_handler(self):
        self.solids = OrderedDict() # keyed by name of solids phase
        self.solids_current_phase = None
        self.solids_species = {} #dict of OrderedDict, keyed by phase num
        self.psd = {}
        #  The value dict  is keyed by species, but probably should
        #  be keyed by alias.
        #  Note we now enforce Alias=Species (alias-species integration)

        self.solids_current_tab = TAB_MATERIALS

        ui = self.ui.solids
        ui.dynamic_widgets = {}

        self.solids_pushbuttons = (ui.pushbutton_solids_materials,
                                   ui.pushbutton_solids_TFM,
                                   ui.pushbutton_solids_DEM,
                                   ui.pushbutton_solids_CGP,
                                   ui.pushbutton_solids_PIC)

        tb = ui.toolbutton_solids_add_phase
        tb.key = 'mmax' # locatability
        tb.clicked.connect(lambda : self.solids_add_phase()) # clicked emits a boolean, ignore it
        tb = ui.toolbutton_solids_delete_phase
        tb.key = 'mmax' # locatability
        tb.clicked.connect(self.solids_delete_phase)
        tb.setEnabled(False)

        # solids database
        tb = ui.toolbutton_solids_search
        tb.clicked.connect(self.handle_solids_search)
        enable = bool(self.particle_popup.db)
        tb.setEnabled(enable)
        tb.setToolTip("Import particle properties from database." if enable else
                      "Could not load particle database.")

        ui.tablewidget_psd.itemSelectionChanged.connect(self.handle_solids_psd_selection)

        tw_solids, tw_solids_species = ui.tablewidget_solids, ui.tablewidget_solids_species
        # Note, with find-by-args this will have to select a table row, not the
        # groupbox
        # Should nmax_s be on the +/- buttons or on the groupbox?
        ui.groupbox_species.keys = ['species_alias_s', 'species_s', 'ro_xs0', 'nmax_s']# Locatability

        # force summary table to update on kw updates
        class TableWidgetProxy:
            def objectName(self):
                return "proxy"
            def updateValue(*args): # pylint: disable=no-method-argument
                self.update_solids_table()
        # TODO maybe the solids_table should just be directly editable?
        self.project.register_widget(TableWidgetProxy(),
                             ['solids_model', 'd_p0', 'ro_s0'], args='*')
        tw_solids.itemSelectionChanged.connect(self.handle_solids_table_selection)

        cb = ui.combobox_solids_model
        cb.key = 'solids_model' # Locatability
        cb.activated.connect(self.handle_combobox_solids_model)
        cb.setToolTip(cb.currentText())

        ui.lineedit_solids_phase_name.value_updated.connect(self.handle_solids_phase_name)

        cb = ui.combobox_solids_tc_model
        ui.label_solids_tc_model.keys = cb.keys = ['k_s0', 'ks_model']

        for (i, val) in enumerate(KS_MODELS):
            self.add_tooltip(get_combobox_item(cb,i), key='ks_model', value=val)
        cb.setToolTip(get_combobox_item(cb, cb.currentIndex()).toolTip())

        self.init_solids_default_models()
        cb = ui.checkbox_species_eq
        cb.clicked.connect(self.handle_solids_species_eq)
        self.add_tooltip(cb, key='species_eq')
        cb.keys = ['species_eq', 'bc_hw_x_s', 'bc_c_x_s', 'bc_xw_s']

        for c in 'xyz':
            key = 'momentum_%s_eq' % c
            cb = getattr(ui, 'checkbox_'+key)
            self.add_tooltip(cb, key=key)
            cb.clicked.connect(lambda enabled, key=key: self.handle_solids_momentum_eq(key, enabled))

        # REQUIRED keys
        le = ui.lineedit_keyword_d_p0_args_P
        le.required = True
        le.saved_value = default_values.d_p0
        le.initpython = default_values.d_p0
        le.post_update = self.solids_check_2d_DEM

        le = ui.lineedit_keyword_des_em_args_P
        le.post_update = self.update_solids_detail_pane
        le = ui.lineedit_keyword_k_s0_args_P
        le.post_update = self.update_solids_detail_pane

        # ro_s0 is not required for VSD
        #le = ui.lineedit_keyword_ro_s0_args_P
        #le.required = True
        #le.saved_value = default_values.ro_s0
        #le.initpython = default_values.ro_s0

        # Handle a number of cases which are essentially the same
        # avoid repetition in set_solids_*_model methods
        def make_solids_model_setter(self, name, key):
            def setter(model=None): # If not specified, determine correct model from keywords
                ui = self.ui.solids
                phase = self.solids_current_phase
                key_s0 = 'c_ps0' if key=='c_p' else key + '_s0'
                key_usr = 'usr_cps' if key=='c_p' else 'usr_' + key + 's'
                lineedit = getattr(ui, 'lineedit_keyword_%s_args_P' % key_s0)
                units_label = getattr(ui, 'label_%s_units' % key_s0)

                if model is None and phase is not None:
                    val_s0 = self.project.get_value(key_s0, args=[phase])
                    val_usr = self.project.get_value(key_usr, args=[phase])
                    model = (CONSTANT if val_s0 is not None
                             else UDF if val_usr is not None
                             else VARIABLE)
                    if val_s0 is not None:
                        lineedit.setText(str(val_s0))

                setattr(self, name, model) # self.solids_<name>_model = model
                cb = getattr(ui, 'combobox_' + name)
                if model != cb.currentIndex():
                    cb.setCurrentIndex(model)
                # Make tooltip match item setting (for longer names which are truncated)
                cb.setToolTip(get_combobox_item(cb, model).toolTip())

                # Locatability
                cb.keys = [key_s0, key_usr]
                label = getattr(ui, 'label_' + name)
                label.keys = cb.keys

                # Enable lineedit for constant model
                enabled = (model==CONSTANT)
                for item in (lineedit, units_label):
                    item.setEnabled(enabled)

                if phase is None:
                    return

                energy_eq = self.project.get_value('energy_eq', default=True)

                if model == CONSTANT:
                    if self.project.get_value(key_s0, args=[phase]) is None:
                        val = self.get_retained_keyword(key_s0,
                                                        default=getattr(default_values, key_s0, None),
                                                        args=[phase])
                        if val == 'None':
                            val = ''
                        if val != '' and val is not None:
                            # issues/1269
                            if key_s0 == 'c_ps0'  and not energy_eq:
                                self.unset_keyword(key_s0, args=[phase])
                            else:
                                self.update_keyword(key_s0, val, args=[phase]) # Restore keyword value
                    self.unset_keyword(key_usr, args=phase) # Issues/435
                elif model == UDF:
                    self.retain_keyword(key_s0, args=phase)
                    self.unset_keyword(key_s0, args=phase)
                    lineedit.setText('')
                    self.update_keyword(key_usr, True, args=phase)
                else: # Continuum, mixture, etc
                    self.retain_keyword(key_s0, args=phase)
                    self.unset_keyword(key_s0, args=phase)
                    lineedit.setText('')
                    self.unset_keyword(key_usr, args=phase)

                if key_s0 == 'ro_s0': # Extra handling: Density column changes
                    nmax_s = self.project.get_value('nmax_s', default=0, args=phase)
                    if model == CONSTANT:
                        lineedit.required = True
                        lineedit.exclude_min = True
                        # Issues/645 When solids density model is
                        # toggled from "variable" to "constant" the
                        # keywords RO_Xs0, X_s0, and INERT_SPECIES
                        # must be unset to avoid a run time error
                        # check.
                        for i in range(1, 1+nmax_s):
                            args = [phase,i]
                            for k in 'ro_xs0', 'x_s0':
                                self.retain_keyword(k, args=args)
                                self.unset_keyword(k, args=args)
                        args=[phase]
                        self.retain_keyword('inert_species', args=args)
                        self.unset_keyword('inert_species', args=args)
                    elif model == VARIABLE:
                        lineedit.required = lineedit.exclude_min = False
                        for i in range(1, 1+nmax_s):
                            args = [phase,i]
                            for k in 'ro_xs0', 'x_s0':
                                val = self.get_retained_keyword(k, args)
                                if val is not None and val != '':
                                    self.update_keyword(k, val, args=args)
                            args = [phase]
                            k = 'inert_species'
                            val = self.get_retained_keyword(k, args)
                            if val is not None:
                                self.update_keyword(k, val, args=args)
                    else:
                        lineedit.required = lineedit.exclude_min = False
                    self.update_solids_species_table() # ro_xs0
                    self.update_solids_baseline_groupbox(model) # availability
                    self.update_solids_table() # 'density' column changes

            return setter


        for (name, key) in (
                ('density', 'ro'),
                ('viscosity', 'mu'),
                ('specific_heat', 'c_p')):

            model_name = 'solids_%s_model' % name
            setter = make_solids_model_setter(self, model_name, key)
            setattr(self, 'set_%s' % model_name, setter)
            cb = getattr(ui, 'combobox_'+model_name)
            cb.activated.connect(setter)
            cb.default_value = getattr(self, model_name)
            # Tooltips
            key_s0 = 'c_ps0' if key=='c_p' else key + '_s0'
            key_usr = 'usr_cps' if key=='c_p' else 'usr_' + key + 's'
            item =  get_combobox_item(cb, 0)
            self.add_tooltip(item, key=key_s0)
            item =  get_combobox_item(cb, 1)
            item.setToolTip('<b>%s</b>: Use MFiX default calculation.' % item.text()) #Full name of model
            if key_s0 == 'ro_s0':
                item.setToolTip(item.toolTip() + ' Requires densities for all species, and an inert species.')
            item =  get_combobox_item(cb, 2)
            if item:
                self.add_tooltip(item, key=key_usr)

        # Mol weight is special
        cb = ui.combobox_solids_mol_weight_model
        cb.setToolTip("<b>Mixture</b>: Use MFiX default calculation.")

        # Thermal conductivity
        def set_solids_tc_model(idx):
            ui = self.ui.solids
            phase = self.solids_current_phase
            if phase is None:
                return
            ks_model = KS_MODELS[idx]
            if self.project.get_value('ks_model', args=[phase]) == ks_model: # No change
                return
            dem_phases = [p for p in range(1,1+len(self.solids))
                          if self.project.get_value('solids_model', default='TFM', args=[p]) in ('DEM','CGP')
                          and p != phase]
            if dem_phases:
                resp = self.message(text="Setting ks_model applies to all DEM/CGP phases\nAre you sure?",
                                    buttons=['yes','no'],
                                    default=['no'])
                if resp != 'yes':
                    return
                for p in dem_phases:
                    self.update_keyword('ks_model', ks_model, args=[p])
                    k_s0 = self.project.get_value('k_s0', args=[p])
                    if ks_model == 'NONE':
                        if k_s0:
                            self.retain_keyword('k_s0', args=[p]) # Retain if nonzero
                        self.update_keyword('k_s0', 0, args=[p])
                    elif ks_model == 'MUSSER':
                        default = self.get_retained_keyword('k_s0', default=1.0, args=[p])
                        if k_s0==0.0 or k_s0 is None:
                            self.update_keyword('k_s0', default, args=[p])

            k_s0 = self.project.get_value('k_s0', default=0.0, args=[phase])
            lineedit = ui.lineedit_keyword_k_s0_args_P
            units_label = ui.label_k_s0_units
            cb = ui.combobox_solids_tc_model
            # Make tooltip match item setting (for longer names which are truncated)
            set_combobox_tooltip(cb)
            # Enable lineedit for constant model
            enabled = (idx > 0)
            for item in (lineedit, units_label):
                item.setEnabled(enabled)

            self.update_keyword('ks_model', KS_MODELS[idx], args=[phase])
            if ks_model == 'NONE':
                if k_s0: # Retain if non-zero
                    self.retain_keyword('k_s0', args=[phase])
                self.update_keyword('k_s0', 0, args=[phase])
                lineedit.setEnabled(False)
            elif ks_model in ('BAUER', 'CONST_EFF', 'MUSSER'):
                if k_s0 == 0.0:
                    default = self.get_retained_keyword('k_s0', default=1.0, args=[phase])
                    self.update_keyword('k_s0', default, args=[phase])

        cb = ui.combobox_solids_tc_model
        cb.activated.connect(set_solids_tc_model)

        # Emissivity
        cb = ui.combobox_solids_emissivity_model
        cb.activated.connect(self.handle_solids_emissivity_model)
        cb.key = 'des_em'
        self.add_tooltip(cb, cb.key)
        get_combobox_item(cb, 0).setToolTip("Disable emissivity.")
        get_combobox_item(cb, 1).setToolTip("Specify grey-body emissivity factor.")

        # Parcel weight is not a keyword
        le = ui.lineedit_pic_const_statwt
        le.dtype = float
        le.min = 0.0
        le.value_updated.connect(self.handle_pic_const_statwt)
        le.key = 'pic_const_statwt' # Pseudo-key
        tip = '<html>Defines a default statistical weight for parcels in IC regions and mass inflows, <br><b>IC_PIC_CONST_STATWT</b> and <br><b>BC_PIC_MI_CONST_STATWT</b></html>'
        le.setToolTip(tip)
        ui.label_pic_const_statwt.setToolTip(tip)

        ui.radiobutton_cgp_stat_wt.toggled.connect(self.solids_handle_cgp_radiobutton)
        ui.radiobutton_cgp_d_p0.toggled.connect(self.solids_handle_cgp_radiobutton)
        # Solids species
        tb = ui.toolbutton_solids_species_add
        tb.key = 'nmax_s' # Locatability
        tb.clicked.connect(self.solids_species_add)
        tb = ui.toolbutton_solids_species_edit
        tb.clicked.connect(self.solids_species_edit)
        tb.setEnabled(False)
        tb = ui.toolbutton_solids_species_delete
        tb.key = 'nmax_s' # Locatability
        tb.setEnabled(False)
        tb.clicked.connect(self.solids_species_delete)
        tw_solids_species.itemSelectionChanged.connect(self.handle_solids_species_selection)
        tw_solids_species.keys = ['species_alias_s', 'ro_xs0']

        ui.groupbox_baseline.keys = ['inert_species', 'x_s0']

        # Advanced
        cb = ui.checkbox_disable_close_pack
        cb.clicked.connect(self.disable_close_pack)
        self.add_tooltip(cb, 'close_packed')

        cb = ui.checkbox_enable_added_mass_force
        cb.keys = ['added_mass', 'm_am']
        cb.clicked.connect(self.enable_added_mass_force)
        self.add_tooltip(cb, 'added_mass')

        # connect solid tab buttons
        for i, btn in enumerate(self.solids_pushbuttons):
            btn.pressed.connect(lambda i=i: self.solids_change_tab(i))

        for tw in (ui.tablewidget_solids, ui.tablewidget_solids_species,
                   ui.tablewidget_solids_baseline):
            self.fixup_solids_table(tw)

        self.init_solids_tfm()
        self.init_solids_dem()
        self.init_solids_pic()

        # Add some tooltips
        cb = ui.combobox_solids_model
        key = 'solids_model'
        self.add_tooltip(cb, key=key)
        d = self.keyword_doc[key]
        get_combobox_item(cb, 0).setToolTip(d['valids']['TFM']['note'])
        get_combobox_item(cb, 1).setToolTip(d['valids']['DEM']['note'])
        get_combobox_item(cb, 2).setToolTip(d['valids']['CGP']['note'])
        get_combobox_item(cb, 3).setToolTip(d['valids']['PIC']['note'])

        # connect solids database popup
        self.particle_popup.add.connect(self.handle_solids_database_add)


    def solids_check_2d_DEM(self):
        if (self.project.solver in (DEM,CGP)
            and self.project.get_value('no_k')):
            self.set_z_max_from_d_p0()

    def solids_update_tabs(self):
        ui = self.ui.solids
        solver = self.project.solver
        if solver == SINGLE: # We shouldn't be here
            return
        enable_dict = {TFM: (True, True, False, False, False),
                       DEM: (True, False, True, False, False),
                       CGP: (True, False, False, True, False),
                       PIC: (True, False, False, False, True),
                       HYBRID: (True, True, True, False, False)}

        enabled = enable_dict[solver] if self.solids else (True, False, False, False, False)

        for (item, e) in zip(self.solids_pushbuttons, enabled):
            item.setDisabled(not e)

        # Don't stay on a disabled tab!
        # (Do we ever disable "Materials"? - don't think so)
        active_tab = ui.stackedwidget_solids.currentIndex()
        if active_tab == TAB_DEM and solver==CGP:
            active_tab = TAB_CGP
        if active_tab > 0 and not enabled[active_tab]:
            if solver==SINGLE:
                i = TAB_MATERIALS
            elif solver in (TFM, HYBRID):
                i = TAB_TFM
            elif solver==DEM:
                i = TAB_DEM
            elif solver==CGP:
                i = TAB_CGP
            elif solver==PIC:
                i = TAB_PIC
            self.solids_change_tab(i)

    # Solids sub-pane navigation
    def solids_change_tab(self, tabnum):
        ui = self.ui.solids
        to_btn = [ui.pushbutton_solids_materials, ui.pushbutton_solids_TFM,
                  ui.pushbutton_solids_DEM, ui.pushbutton_solids_CGP,
                  ui.pushbutton_solids_PIC][tabnum]
        self.solids_current_tab = tabnum
        target = tabnum
        if target >= TAB_CGP:
            target -= 1
        animate_stacked_widget(
            self,
            ui.stackedwidget_solids,
            (ui.stackedwidget_solids.currentIndex(), target),
            line=ui.line_solids,
            to_btn=to_btn,
            btn_layout=ui.gridlayout_tab_btns)
        self.setup_solids_tab(tabnum)
        for btn in self.solids_pushbuttons:
            btn.setChecked(btn == to_btn)
            font = btn.font()
            font.setBold(btn == to_btn)
            btn.setFont(font)


    def setup_solids(self, allow_disabled_tab=False):
        # Don't stay on disabled tab!
        ui = self.ui.solids

        tabnum = self.solids_current_tab
        solver = self.project.solver

        if tabnum == TAB_MATERIALS:
            ok = True
        elif tabnum == TAB_TFM:
            ok = solver in (TFM, HYBRID)
        elif tabnum == TAB_DEM:
            ok = solver in (DEM, HYBRID)
        elif tabnum == TAB_CGP:
            ok = (solver == CGP)
        elif tabnum == TAB_PIC:
            ok = (solver == PIC)
        if allow_disabled_tab:
            ok = True
        if not ok:
            if solver == TFM:
                tabnum = TAB_TFM
            elif solver in (DEM, HYBRID):
                tabnum = TAB_DEM
            elif solver == CGP:
                tabnum = TAB_CGP
            elif solver == PIC:
                tabnum = TAB_PIC
            else:
                tabnum = TAB_MATERIALS
            self.solids_change_tab(tabnum)
        else:
            self.setup_solids_tab(tabnum)


    def setup_solids_tab(self, tabnum):
        if tabnum == TAB_MATERIALS:
            self.update_solids_table()
            self.update_solids_detail_pane()
            self.update_solids_options()
        elif tabnum == TAB_TFM:
            self.setup_tfm_tab()
        elif tabnum in (TAB_DEM, TAB_CGP):
            self.setup_dem_tab()
        elif tabnum == TAB_PIC:
            self.setup_pic_tab()
        else:
            raise ValueError(tabnum)


    # Advanced
    def disable_close_pack(self, val):
        ui = self.ui.solids
        cb = ui.checkbox_disable_close_pack
        if val != cb.isChecked(): # not from a click action
            cb.setChecked(val)
            return
        phase = self.solids_current_phase
        if phase is None:
            return
        close_packed = self.project.get_value('close_packed', default=None, args=phase)
        if (close_packed is not False) and val: # Disabling - popup as per SRS p15
            resp=self.message(text="Disabling close-packing for %s\nAre you sure?" % self.solids_current_phase_name,
                              buttons=['yes','no'],
                              default = 'no')
            if resp != 'yes':
                cb.setChecked(False)
                return

        if close_packed is None and not val:
            return # We will leave the keyword at its default unset value

        self.update_keyword('close_packed', not val, args=phase)


    def enable_added_mass_force(self, val):
        ui = self.ui.solids
        cb = ui.checkbox_enable_added_mass_force
        phase = self.solids_current_phase
        if phase is None:
            return
        # Warn when unsetting added mass for other phases
        prev_phase = self.project.get_value('m_am')
        if prev_phase is not None and prev_phase != phase and val:
            prev_phase_name = list(self.solids.keys())[prev_phase-1]
            resp=self.message(text="This will disable added mass force for %s\nAre you sure?" % prev_phase_name,
                              buttons=['yes','no'],
                              default = 'no')
            if resp == 'no':
                cb.setChecked(False)
                return

        if val:
            self.update_keyword('m_am', phase)
            self.update_keyword('added_mass', True)

        else:
            self.unset_keyword('m_am')
            self.unset_keyword('added_mass')


    def fixup_solids_table(self, tw, stretch_column=0):
        # Should we just hide the entire table (including header) if no rows?
        ui = self.ui.solids
        hv = QtWidgets.QHeaderView
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
                       + nrows*row_height + 4) # extra to avoid unneeded scrollbar (?)
        if tw == ui.tablewidget_solids: # In a splitter
            # top_frame is the tab bar, not the top part of the splitter!!
            #icon_height = sub_icon_height() + 8
            #ui.top_frame.setMaximumHeight(height+icon_height)
            #ui.top_frame.setMinimumHeight(header_height+icon_height+row_height*min(nrows,5))
            #ui.top_frame.updateGeometry()
            tw.setMaximumHeight(height)
            tw.setMinimumHeight(header_height+row_height*min(nrows,5))
        else:
            tw.setMaximumHeight(height) # Works for tablewidget inside groupbox
            tw.setMinimumHeight(height) #? needed for tablewidget_des_en_input. should we allow scrollbar?
        tw.updateGeometry() #? needed?


    def handle_solids_momentum_eq(self, key, enabled):
        P = self.solids_current_phase
        if P is None:
            return
        self.update_keyword(key, enabled, args=[P])


    def handle_solids_species_eq(self, enabled):
        P = self.solids_current_phase
        if P is None:
            return
        ui = self.ui.solids
        self.update_keyword('species_eq', enabled, args=[P])
        usr_ros = self.project.get_value('usr_ros', args=[P])
        if not enabled:
            self.set_solids_density_model(UDF if usr_ros else CONSTANT)
        set_item_enabled(get_combobox_item(ui.combobox_solids_density_model,
                                                VARIABLE), enabled)
        self.update_solids_species_groupbox() # availability

        # issues/183
        self.bcs_check_wall_keys()


    def setup_combobox_solids_model(self):
        """solids model combobox is tied to solver setting"""
        solver = self.project.solver
        if solver == SINGLE:
            # Note, if Single-Phase solver is enabled, this pane is disabled
            return
        ui = self.ui.solids
        cb = ui.combobox_solids_model
        model = cb.model()
        #          TFM,  DEM, CGP, PIC
        enabled = [False, False, False, False]
        enabled[0] = (solver==TFM or solver==HYBRID)
        enabled[1] = (solver==DEM or solver==HYBRID)
        enabled[2] = (solver==CGP)
        enabled[3] = (solver==PIC)

        for (i, e) in enumerate(enabled):
            set_item_enabled(get_combobox_item(cb,i), e)

        # Set for current phase
        if self.solids_current_phase is not None:
            mod = self.project.get_value("solids_model", args=self.solids_current_phase)
            i = 0 if mod=='TFM' else 1 if mod=='DEM' else 2 if mod=='CGP' else 3 if mod=='PIC' else None
            if i is None:
                i = 0 if solver in (TFM, HYBRID) else 1 if solver == DEM else 3

            if not enabled[i]:
                # Current selection no longer valid, so pick first valid choice
                # Don't leave a non-enabled item selected!
                i = enabled.index(True)
            cb.setCurrentIndex(i)
            set_combobox_tooltip(cb)
        else: # Set based on overall solver
            i = 0 if solver in (TFM, HYBRID) else 1 if solver == DEM else 2 if solver == CGP else 3
            cb.setCurrentIndex(i)
            set_combobox_tooltip(cb)


    def handle_combobox_solids_model(self, index):
        ## NB:  Solids model is not the same as solver!
        # Solver values are enumerated in constants.py.  Solids models are strings, 'TFM', 'DEM', 'PIC'
        if self.solids_current_phase is None:
            return # shouldn't get here

        phase = self.solids_current_phase
        model = ('TFM', 'DEM', 'CGP', 'PIC')[index]
        self.set_solids_model(phase, model)
        self.update_solids_table()
        self.update_solids_detail_pane()
        self.update_nav_tree() # PSs depends on solids_model

    def set_solids_model(self, phase, model):
        # In addition to combobox_solids_model, this is called
        # when we toggle energy_eq or add a new solids phase
        self.update_keyword('solids_model', model, args=[phase])
        name, data = list(self.solids.items())[phase-1]
        data['model'] = model
        if model != 'TFM':
            self.delete_scalars_of_phase(phase)
        energy_eq =  self.project.get_value('energy_eq', default=True)

        # Issues/965
        if model in ('DEM','CGP') and energy_eq:
            self.set_keyword_default('des_em', 0.0, args=[phase])

        # Issues/1034 ks_model
        ks_model = self.project.get_value('ks_model', args=[phase])
        if model == 'PIC' or not energy_eq:
            self.unset_keyword('ks_model', args=[phase])
            if self.project.get_value('k_s0', args=[phase]):
                self.retain_keyword('k_s0', args=[phase])
            self.unset_keyword('k_s0', args=[phase])
        elif model == 'TFM':
            if ks_model in (None, 'MUSSER'): # Musser not allowed for TFM
                ks_model = 'BAUER'
                self.update_keyword('ks_model', 'BAUER', args=[phase])
            default = self.get_retained_keyword('k_s0', args=[phase], default=1.0)
            self.set_keyword_default('k_s0', default if ks_model=='BAUER' else 0.0, args=[phase])
        elif model in ('DEM','CGP'):
            dem_phases = [p for p in range(1,1+len(self.solids))
                          if self.project.get_value('solids_model', default='TFM', args=[p]) in ('DEM','CGP')
                          and p != phase]

            if ks_model is None or ks_model not in ('NONE', 'MUSSER'):
                ks_model = ('MUSSER' if not dem_phases
                            else self.project.get_value('ks_model', args=[dem_phases[0]]))
                if any(self.project.get_value('k_s0', args=[p]) for p in dem_phases):
                    ks_model = 'MUSSER'
                self.update_keyword('ks_model', ks_model, args=[phase])
                if ks_model == 'NONE':
                    if self.project.get_value('k_s0', args=[phase]):
                        self.retain_keyword('k_s0', args=[phase])
                    self.update_keyword('k_s0', 0, args=[phase])
                else:
                    default = self.get_retained_keyword('k_s0', args=[phase], default=1.0)
                    self.set_keyword_default('k_s0', default, args=[phase])
            if any (self.project.get_value('ks_model',args=[p])=='MUSSER'
                    for p in dem_phases+[phase]):
                for p in sorted(dem_phases+[phase]):
                    self.update_keyword('ks_model', 'MUSSER', args=[p])
        # RDF_TYPE
        if model == 'TFM':
            self.check_rdf_type()


    def check_rdf_type(self):
        if not any(self.project.get_value('solids_model', default='TFM', args=[i])=='TFM'
                   for i in range(1,1+len(self.solids))):
            return
        key = 'rdf_type'
        rdf_type = self.project.get_value(key)
        if len(self.solids) > 1 and rdf_type not in RDF_TYPES[2:]:
            default = RDF_TYPES[2]
            self.warn("rdf_type '%s' invalid for polydisperse cases, setting to %s" %
                      (rdf_type, default),
                      popup=True)
            self.update_keyword(key, default)
        elif len(self.solids) < 2 and rdf_type not in RDF_TYPES[:2]:
            default = RDF_TYPES[0]
            self.warn("rdf_type '%s' invalid for monodisperse cases, setting to %s" %
                      (rdf_type, default),
                      popup=True)
            self.update_keyword(key, default)


    def make_solids_name(self, base='Solid'):
        n = 1
        while True:
            name = base + ' %d' % n
            if name not in self.solids:
                break
            n += 1
        return name

    def handle_solids_emissivity_model(self, index):
        ui = self.ui.solids
        le = ui.lineedit_keyword_des_em_args_P
        le.setEnabled(index != 0)
        P = self.solids_current_phase
        if P is None:
            return
        if index == 0: # No radiative flux
            self.update_keyword('des_em', 0.0, args=[P])
        cb = ui.combobox_solids_emissivity_model
        cb.setToolTip(get_combobox_item(cb, index).toolTip())

    def solids_add_phase(self, name='Solid', diameter=default_values.d_p0,
                         density=default_values.ro_s0):
        """Define a new solids phase.  It is given a generic name which can be
        changed by the user"""

        if self.project.solver == SINGLE: # Should not get here! this pane is disabled.
            return
        else:
            model = [None, 'TFM', 'DEM', 'CGP', 'PIC', 'TFM'][self.project.solver]

        ui = self.ui.solids
        tw = ui.tablewidget_solids
        nrows = tw.rowCount()
        n = nrows + 1 # index of new solid
        tw.setRowCount(n)
        name = self.make_solids_name(name)

        self.update_keyword('d_p0', diameter, args=[n])
        self.update_keyword('ro_s0', density, args=[n])
        self.solids[name] = {'model': model,
                             'diameter': diameter,
                             'density': density}
        self.set_solids_model(n, model)
        if model == 'PIC':
            self.solids[name]['pic_const_statwt'] = 1.0
        self.solids_species[n] = OrderedDict()
        self.update_keyword('mmax', len(self.solids))
        #self.update_keyword('nmax_s', 0, args=[n]) ? needed
        self.update_keyword('species_eq', False, args=[n]) # Disable species eq by default, per SRS
        if model in ('DEM','CGP'):
            #Issues/965
            if self.project.get_value('energy_eq', default=True):
                self.set_keyword_default('des_em', 0.0, args=[n])

        self.update_solids_table()
        tw.setCurrentCell(nrows, 0) # Select new item
        #Since c_ps0 is not defined the spec. heat model will default to VARIABLE.  But SRS
        # says default should be CONSTANT, so force that.  However, if user leaves this pane
        # without setting a value for spec. heat, the setting will revert to VARIABLE
        self.set_solids_specific_heat_model(CONSTANT)

        # Reshape triangular matrices
        for key in ('des_et_input', 'des_en_input'):
            prev_size = (n*(n-1))//2 # Size before row added
            vals = [self.project.get_value(key, args=i)
                    for i in range(1, 1+prev_size)]
            if any(v is not None for v in vals):
                new_vals = append_row_column_triangular(vals, n-1)
                for (i, val) in enumerate(new_vals, 1):
                    if val is None:
                        self.unset_keyword(key, args=i)
                    else:
                        self.update_keyword(key, val, args=i)

        # Update RDF_TYPE if we go from mono- to poly-disperse or vv
        self.check_rdf_type()

        # Initialize input fields for new phase
        self.update_solids_detail_pane()
        # Tabs enable/disable depending on number of solids
        self.solids_update_tabs()
        # Set BC keys for solids species at any defined walls
        self.bcs_check_wall_keys()

        # Update IC keys for new solids
        for i in self.ics:
            self.ics_set_default_keys(i)

        # ICs enabled/disabled depends on number of solids
        self.update_nav_tree()

        return name


    def handle_solids_search(self):
        # Reset popup?
        self.particle_popup.popup()

    def handle_solids_database_add(self, name, diameter, density, umf):

        # add the solid
        name = self.solids_add_phase(name, diameter, density)
        # add umf to parameters
        for r, w in [(' ', '_'), ('(', ''), (')', ''), ('+', ''), ('-', ''), (',', '')]:
            name = name.replace(r, w)
        PARAMETER_DICT[name+'_umf'] = umf

    def handle_solids_table_selection(self):
        ui = self.ui.solids
        self.P = self.solids_current_phase # should already be set
        # Clear out lineedits
        for w in widget_iter(ui.groupbox_solids_parameters):
            if isinstance(w, LineEdit):
                w.setText('')

        tw = ui.tablewidget_solids
        row = get_selected_row(tw)
        enabled = (row is not None)
        ui.toolbutton_solids_delete_phase.setEnabled(enabled)
        self.solids_current_phase_name = None if row is None else tw.item(row,0).text()
        self.solids_current_phase = (row+1) if row is not None else None
        self.update_solids_detail_pane()

    def solids_handle_cgp_radiobutton(self, *args):
        ui = self.ui.solids
        stat_wt = ui.radiobutton_cgp_stat_wt.isChecked()
        d_p0 = ui.radiobutton_cgp_d_p0.isChecked()

        ui.lineedit_keyword_cgp_d_p0_args_P.setEnabled(d_p0)
        ui.label_cgp_d_p0_units.setEnabled(d_p0)
        ui.lineedit_keyword_cgp_stat_wt_args_P.setEnabled(stat_wt)

        P = self.solids_current_phase
        if not P:
            return
        if stat_wt:
            self.retain_keyword('cgp_d_p0', args=[P])
            self.unset_keyword('cgp_d_p0', args=[P])
            ui.lineedit_keyword_cgp_d_p0_args_P.setText('')
            val = self.get_retained_keyword('cgp_stat_wt', args=[P])
            if val:
                self.update_keyword('cgp_stat_wt', val, args=[P])
                ui.lineedit_keyword_cgp_stat_wt_args_P.updateValue('cgp_stat_wt', val)

        if d_p0:
            self.retain_keyword('cgp_stat_wt', args=[P])
            self.unset_keyword('cgp_stat_wt', args=[P])
            ui.lineedit_keyword_cgp_stat_wt_args_P.setText('')
            val = self.get_retained_keyword('cgp_d_p0', args=[P])
            if val:
                self.update_keyword('cgp_d_p0', val, args=[P])
                ui.lineedit_keyword_cgp_d_p0_args_P.updateValue('cgp_d_p0', val)

    def update_solids_detail_pane(self):
        """update the solids detail pane for currently selected solids phase.
        if no solid phase # selected, pane is cleared and disabled"""
        # Note - this function enforces restrictions and sets/clears
        # keywords - probably more than an 'update_pane' function should do

        ui = self.ui.solids
        P = self.P = self.solids_current_phase
        if P is None:
            # Disable all inputs
            self.update_solids_species_table()
            self.fixup_solids_table(ui.tablewidget_solids)
            ui.detail_pane.setEnabled(False)
            for item in widget_iter(ui.detail_pane):
                if isinstance(item, QtWidgets.QCheckBox):
                    item.setChecked(False)
                if isinstance(item, QtWidgets.QLineEdit):
                    item.setText('')
            show = (self.project and self.project.solver==CGP)
            for item in (ui.radiobutton_cgp_stat_wt,
                         ui.lineedit_keyword_cgp_stat_wt_args_P,
                         ui.radiobutton_cgp_d_p0,
                         ui.lineedit_keyword_cgp_d_p0_args_P,
                         ui.label_cgp_d_p0_units
                         ):
                item.setVisible(show)
            show = (self.project and self.project.solver==PIC)
            for item in (ui.label_pic_const_statwt,
                         ui.lineedit_pic_const_statwt):
                item.setVisible(show)
            return

        name = list(self.solids.keys())[P-1]
        solid = self.solids[name]
        model = solid['model']

        # Enable the input areas, initialize to values for current solid
        ui.detail_pane.setEnabled(ui.input_enabled)
        self.solids_current_phase_name = name
        ui.lineedit_solids_phase_name.setText(self.solids_current_phase_name)
        self.setup_combobox_solids_model()

        # Initialize all the line edit widgets
        def as_str(x):
            return '' if x is None else str(x)
        def get_widget(key):
            return getattr(ui, 'lineedit_keyword_%s_args_P' % key)
        for key in ('d_p0', 'ro_s0', 'des_em', 'cgp_stat_wt'):
            val = self.project.get_value(key, args=[P])
            w = get_widget(key)
            w.setText(as_str(val))
            w.saved_value = val # Reversion value for invalid input


        # Hybrid support
        show = (model == 'CGP')
        for w in (ui.radiobutton_cgp_stat_wt,
                  ui.lineedit_keyword_cgp_stat_wt_args_P,
                  ui.radiobutton_cgp_d_p0,
                  ui.label_cgp_d_p0_units,
                  ui.lineedit_keyword_cgp_d_p0_args_P,
                  ):
            w.setVisible(show)

        show = (model == 'PIC')
        for w in (ui.label_pic_const_statwt, ui.lineedit_pic_const_statwt):
            w.setVisible(show)

        # CGP radiobuttons
        if model == 'CGP':
            stat_wt = self.project.get_value('cgp_stat_wt', args=[P])
            d_p0 = self.project.get_value('cgp_d_p0', args=[P])
            if stat_wt is not None and d_p0 is not None:
                self.error("Statistical weight and coarse-grain size can not both be specified.  Disabling size.",
                           popup=True)
                self.retain_keyword('cgp_d_p0', args=[P])
                self.unset_keyword('cgp_d_p0', args=[P])
                d_p0 = None
            ui.radiobutton_cgp_d_p0.setChecked(d_p0 is not None)
            ui.radiobutton_cgp_stat_wt.setChecked(stat_wt is not None)
            ui.lineedit_keyword_cgp_d_p0_args_P.setText(as_str(d_p0))
            ui.lineedit_keyword_cgp_stat_wt_args_P.setText(as_str(stat_wt))


            self.solids_handle_cgp_radiobutton()

        # 'Parcel weight' is not a keyword
        le = ui.lineedit_pic_const_statwt
        if model == 'PIC':
            val = solid.get('pic_const_statwt')
            if val is None:
                val = solid['pic_const_statwt'] = 1.0
            le.setText(as_str(val))
            le.saved_value = val # Reversion value for invalid input
        else:
            le.setText('')

        # And the checkboxes
        for key in ('momentum_x_eq', 'momentum_y_eq', 'momentum_z_eq'):
            cb = getattr(ui, 'checkbox_%s'%key)
            if not hasattr(cb, 'tooltip0'):
                cb.tooltip0 = cb.toolTip()
            if model == 'TFM': # momentum eq only avail w/ TFM solid model
                val = self.project.get_value(key, default=True, args=[P])
                cb.setEnabled(True)
                cb.setChecked(val)
                cb.setToolTip(cb.tooltip0)
            else:
                cb.setEnabled(False)
                cb.setChecked(False)
                cb.setToolTip(cb.tooltip0 + '<br>Only available for TFM solids model.')

        # Set species eq checkbox to correct value
        species_eq = self.project.get_value('species_eq', default=True, args=[P])
        energy_eq = self.project.get_value('energy_eq', default=True)
        # issues/533  do not allow species eq when energy eq not enabled and solver == DEM
        cb = ui.checkbox_species_eq
        if not hasattr(cb, 'tooltip0'):
            cb.tooltip0 = cb.toolTip()
        cb.setEnabled(bool(energy_eq) if model in ('DEM','CGP') else True)
        if not cb.isEnabled():
            cb.setToolTip(cb.tooltip0 + '<br>Requires energy equations when using DEM model.')
        else:
            cb.setToolTip(cb.tooltip0)
        cb.setChecked(species_eq)

        ### Restrictions (see SRS p13)

        # Variable density model requires species equations
        cb_density =  ui.combobox_solids_density_model
        enabled = bool(species_eq)
        set_item_enabled(get_combobox_item(cb_density, VARIABLE), enabled)
        ro_s0 = self.project.get_value('ro_s0', args=[P], default=None)
        usr_ros = self.project.get_value('usr_ros', args=[P], default=None)
        self.set_solids_density_model(UDF if usr_ros
                                      else VARIABLE if (enabled and ro_s0 is None)
                                      else CONSTANT)

        # Viscosity only available for TFM solids
        self.set_solids_viscosity_model()
        enabled = (model=='TFM')
        for item in (ui.label_solids_viscosity_model,
                     ui.combobox_solids_viscosity_model):
            item.setEnabled(enabled)

        if not enabled:
            for item in (ui.label_solids_viscosity_model,
                         ui.combobox_solids_viscosity_model):
                item.setToolTip("Available for TFM solids only.")
            for item in (ui.lineedit_keyword_mu_s0_args_P,
                         ui.label_mu_s0_units):
                item.setEnabled(False)
        else:
            ui.label_solids_viscosity_model.setToolTip(None)
            cb = ui.combobox_solids_viscosity_model
            cb.setToolTip(get_combobox_item(cb, cb.currentIndex()).toolTip())

        # Mol. wt is locked to MIXTURE

        # Specific heat only available when solving energy eq
        self.set_solids_specific_heat_model()
        energy_eq = self.project.get_value('energy_eq', default=True)
        enabled = (energy_eq==True)
        for item in (ui.combobox_solids_specific_heat_model,
                     ui.label_solids_specific_heat_model):
            item.setEnabled(enabled)
            if enabled:
                if hasattr(item, 'tooltip_0'):
                    item.setToolTip(item.tooltip_0)
            else:
                if not hasattr(item, 'tooltip_0'):
                    item.tooltip_0 = item.toolTip()
                    item.setToolTip(item.tooltip_0 +
                                    '<br>Requires energy equations.')

        if not enabled:
            for item in (ui.lineedit_keyword_c_ps0_args_P,
                         ui.label_c_ps0_units):
                item.setEnabled(False)
                if not hasattr(item, 'tooltip_0'):
                    item.tooltip_0 = item.toolTip()
                    item.setToolTip(item.tooltip_0 +
                                    '<br>Requires energy equations.')
            self.retain_keyword('c_ps0', args=[P])
            self.unset_keyword('c_ps0', args=[P])
            ui.lineedit_keyword_c_ps0_args_P.setText('')

        # Thermal Conductivity Model (abbreviated to tc_model, but keyword is ks_model)
        cb = ui.combobox_solids_tc_model
        set_item_enabled(get_combobox_item(cb,0),  # None
                         model in ('TFM', 'DEM', 'CGP'))
        set_item_enabled(get_combobox_item(cb,1),  # Constant
                         model == 'TFM')
        set_item_enabled(get_combobox_item(cb,2),  # UDF
                         model == 'TFM')
        set_item_enabled(get_combobox_item(cb,3),  # Bauer
                         model == 'TFM')
        set_item_enabled(get_combobox_item(cb,4),  # Hi Jordan
                         model in ('DEM','CGP'))

        ks_model = self.project.get_value('ks_model', args=[P])
        k_s0 = self.project.get_value('k_s0', args=[P])
        le = ui.lineedit_keyword_k_s0_args_P

        enabled = bool(energy_eq) and model != 'PIC'
        if enabled:
            if ks_model is None:
                self.warning("ks_model is not set.  Setting to 'NONE'", popup=True)
                self.update_keyword('ks_model', 'NONE', args=[P])
                self.update_keyword('k_s0', 0, args=[P])
                ks_model = 'NONE'

            if ks_model not in KS_MODELS:
                idx = 0
                self.warning("Invalid ks_model '%s'.  Setting to 'NONE'" % ks_model, popup=True)
                self.update_keyword('ks_model', 'NONE', args=[P])
                self.update_keyword('k_s0', 0, args=[P])
                ks_model = 'NONE'

            idx = KS_MODELS.index(ks_model)
            cb.setCurrentIndex(idx)
            cb.setToolTip(get_combobox_item(cb, idx).toolTip())
            if idx==0:  # NONE
                self.update_keyword('k_s0', 0, args=[P])
                le.setEnabled(False)
            elif idx==1: # CONST_EFF
                le.setEnabled(True)
                le.required = True
                if k_s0 is None:
                    k_s0 = 1.0
                    self.update_keyword('k_s0', 1.0, args=[P])
                    le.saved_value = 1.0
                if le.saved_value is None or le.saved_value==0.0:
                    le.saved_value = 1.0
            elif idx==2: # USR
                le.setEnabled(True)
                le.required = False
            elif idx==3: # BAUER
                le.setEnabled(True)
                le.required = True
                if k_s0 is None or k_s0==0.0:
                    k_s0 = 1.0
                    self.update_keyword('k_s0', 1.0, args=[P])
                if le.saved_value is None or le.saved_value==0.0:
                    le.saved_value = 1.0
            elif idx==4: # MUSSER
                le.setEnabled(True)
                le.required = True
                if le.saved_value is None:
                    le.saved_value = 0.0
                if k_s0 is None:
                    k_s0 = 0.0
                    self.update_keyword('k_s0', 0, args=[P])

            for item in (ui.label_solids_tc_model,
                         ui.combobox_solids_tc_model):
                item.setEnabled(True)
                if hasattr(item, 'tooltip_0'):
                    item.setToolTip(item.tooltip_0)

            for item in (ui.lineedit_keyword_k_s0_args_P,
                         ui.label_k_s0_units):
                item.setEnabled(idx > 0)
                if hasattr(item, 'tooltip_0'):
                    item.setToolTip(item.tooltip_0)
            le.updateValue('k_s0', k_s0)
        else:  # Not enabled
            for item in (ui.label_solids_tc_model,
                         ui.combobox_solids_tc_model,
                         ui.lineedit_keyword_k_s0_args_P,
                         ui.label_k_s0_units):
                item.setEnabled(False)
                ui.combobox_solids_tc_model.setCurrentIndex(0)
                ui.lineedit_keyword_k_s0_args_P.setText('')
                if not hasattr(item, 'tooltip_0'):
                    item.tooltip_0 = item.toolTip()
                item.setToolTip(item.tooltip_0 +
                                '<br>Requires DEM/CGP or TFM solids and energy equations.')

        # Specify solids phase emissivity
        # Selection only available for MFIX-DEM solids model
        # Specification only available when solving energy equations
        enabled = (model in ('DEM','CGP') and energy_eq)
        for item in (ui.label_des_em,
                     ui.combobox_solids_emissivity_model,
                     ui.lineedit_keyword_des_em_args_P):
            item.setEnabled(enabled)
            if enabled:
                if hasattr(item, 'tooltip_0'):
                    item.setToolTip(item.tooltip_0)
            else:
                if not hasattr(item, 'tooltip_0'):
                    item.tooltip_0 = item.toolTip()
                item.setToolTip(item.tooltip_0 +
                                '<br>Requires DEM/CGP solids and energy equations.')

        if enabled:
            des_em = self.project.get_value('des_em', args=[P])
            cb = ui.combobox_solids_emissivity_model
            cb.setCurrentIndex(0 if des_em==0.0 else 1)
            set_combobox_tooltip(cb)
            if des_em == 0.0:
                ui.lineedit_keyword_des_em_args_P.setEnabled(False)

        # Parcel weight
        #   -  Specification only available for SOLIDS_MODEL(#)='PIC'
        # -  DEFAULT 1.0
        # -  This does not directly set any keywords, it is used to define
        # a default statistical weight for parcels in IC regions and
        # mass inflows, IC_PIC_CONST_STATWT and BC_PIC_MI_CONST_STATWT.
        enabled = (model=='PIC')
        for item in (ui.label_pic_const_statwt,
                     ui.lineedit_pic_const_statwt):
            item.setVisible(enabled)


        # Species input is in its own function
        self.update_solids_species_groupbox()
        # as is the baseline table
        self.update_solids_baseline_groupbox(self.solids_density_model)

        self.update_solids_species_table()
        self.fixup_solids_table(ui.tablewidget_solids)

        # Advanced
        enabled = (model=='TFM')
        ui.groupbox_advanced.setEnabled(enabled)
        if not enabled:
            ui.groupbox_advanced.setToolTip("Options for TFM model only")
        else:
            ui.groupbox_advanced.setToolTip("") #?
        kt_type = self.project.get_value('kt_type')
        if enabled:
            close_packed = self.project.get_value('close_packed', default=True, args=[P])
            self.disable_close_pack(not(close_packed))
        else:
            ui.checkbox_disable_close_pack.setChecked(False)
            ui.checkbox_enable_added_mass_force.setChecked(False)

        added_mass = self.project.get_value('added_mass', default=False)
        m_am = self.project.get_value('m_am', default=None)
        ui.checkbox_enable_added_mass_force.setChecked(added_mass and (m_am == P))
        # Added mass force not allowed with GHD model
        ui.checkbox_enable_added_mass_force.setEnabled(kt_type != 'GHD')

    def update_solids_table(self):
        ui = self.ui.solids
        table = ui.tablewidget_solids
        if self.solids is None:
            table.clearContents()
            self.unset_keyword('mmax')
            return
        phase = self.solids_current_phase
        nrows = len(self.solids)
        table.setRowCount(nrows)

        # helper fn
        def make_item(val):
            item = QtWidgets.QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item

        #Update the internal table from keywords
        for (i, (k,v)) in enumerate(self.solids.items(), 1):
            for (myname, realname) in (('model', 'solids_model'),
                                       ('diameter', 'd_p0'),
                                       ('density', 'ro_s0')):
                self.solids[k][myname] = self.project.get_value(realname, args=i)
                #if self.solids[k]['density'] is None and self.solids_density_model==VARIABLE:
                #    self.solids[k]['density'] = "Variable"

        for (row,(k,v)) in enumerate(self.solids.items()):
            item = make_item(k)
            table.setItem(row, 0, item)
            item.setToolTip("User-definable phase name")
            for (col, (mykey,key)) in enumerate((('model', 'solids_model'),
                                                 ('diameter', 'd_p0'),
                                                 ('density', 'ro_s0')), 1):
                item = make_item(v[mykey])
                item.args = [phase, row+1]
                self.add_tooltip(item, key)
                table.setItem(row, col, item)

        if nrows == 1: # If there's only 1 let's auto-select it for the user's convenience
            table.setCurrentCell(0,0)

        # trim excess horizontal space - can't figure out how to do this in designer
        header_height = table.horizontalHeader().height()
        if nrows==0:
            # 34 px is empirical, fixme, should calc. row height
            table.setMaximumHeight(header_height + 34)
        else:
            table.setMaximumHeight(header_height+nrows*table.rowHeight(0) + 4)
            # a little extra to avoid horiz scrollbar when not needed

        # Enable/disable the 'add' button
        # GHD is only valid for MMAX <= 2
        # NB: If we are going to enforce this, there are many other
        # MMAX-dependent settings ... search spec.txt for 'MMAX'
        kt_type  = self.project.get_value('kt_type')
        tb = ui.toolbutton_solids_add_phase
        disabled_msg = None
        if kt_type == 'GHD' and len(self.solids) > 1:
            tb.setEnabled(False)
            disabled_msg = '<b>kt_type=GHD</b>: only 2 solid phases permitted'
        elif kt_type in ('GD_99', 'GTSH') and self.solids:
            disabled_msg = '<b>kt_type=%s</b>: only 1 solid phase permitted' % kt_type
            ui.toolbutton_solids_add_phase.setEnabled(False)
        else:
            enabled = len(self.solids) < DIM_M
            ui.toolbutton_solids_add_phase.setEnabled(enabled)
            ui.toolbutton_solids_search.setEnabled(enabled)
            if not enabled:
                disabled_msg = 'Maximum number of solid phases reached.'
        for tb in (ui.toolbutton_solids_add_phase,
                   ui.toolbutton_solids_search):
            if not tb.isEnabled() and disabled_msg:
                tb.tooltip_0 = tb.toolTip()
                tb.setToolTip(disabled_msg)
            elif hasattr(tb, 'tooltip_0'):
                tb.setToolTip(tb.tooltip_0)

    def update_solids_options(self):
        ui = self.ui.solids
        items = (ui.checkbox_momentum_x_eq,
                 ui.checkbox_momentum_y_eq,
                 ui.checkbox_momentum_z_eq,
                 ui.checkbox_species_eq)
        P = self.P = self.solids_current_phase
        if P is None:
            for item in items:
                item.setEnabled(False)
                item.setChecked(False)
            return

        for item in items[:3]:
            enable = self.project.get_value('solids_model',
                                            args=[P],
                                            default='TFM') == 'TFM'
            item.setEnabled(enable)
            if not enable:
                if self.project.get_value(item.key, args=[P]) == False:
                    self.unset_keyword(item.key, args=[P]) # Key is True by default
                val = True
            else:
                val = self.project.get_value(item.key,
                                             args=[P],
                                             default=True)
            item.setChecked(val)

        ui.checkbox_species_eq.setEnabled(True)
        ui.checkbox_species_eq.setChecked(bool(self.project.get_value('species_eq',
                                                                      args=[P],
                                                                      default=True)))



    def handle_solids_phase_name(self, widget, value_dict, args):
        ui = self.ui.solids
        le = ui.lineedit_solids_phase_name
        new_name = le.text()
        phase = self.solids_current_phase
        if phase is None:
            return
        old_name = list(self.solids.keys())[phase-1]
        if new_name == old_name:
            return # Nothing to do
        if new_name in self.solids or new_name == self.fluid_phase_name: # Reject the input
            self.warning("%s: name is in use" % new_name, popup=True)
            le.setText(old_name)
            return
        self.solids_current_phase_name = new_name
        # rewriting dict to change key while preserving order - hack
        d = OrderedDict()
        for (k,v) in self.solids.items():
            if k==old_name:
                k = new_name
            d[k] = v
        self.solids = d
        self.update_solids_table()
        key = 'solids_phase_name(%s)'%phase
        if self.project.mfix_gui_comments.get(key) != new_name:
            self.project.mfix_gui_comments[key] = new_name
            self.set_unsaved_flag()
        return True


    def solids_delete_phase(self):
        ui = self.ui.solids
        tw = ui.tablewidget_solids
        row = get_selected_row(tw)

        if row is None: # No selection
            return

        phase = row+1
        phase_name = tw.item(row,0).text()

        # Warn if phase is in use
        refs = self.solids_get_phase_refs(phase)
        if refs:
            # Do we want to show the references?
            ret = self.message(text="%s has %d reference%s.\nAll references will be deleted\nContinue?" %
                               (phase_name,
                                len(refs),
                                's' if len(refs)!=1 else ''),
                               icon='question',
                              buttons=['ok', 'cancel'])
            if ret != 'ok':
                self.print_internal("Not deleting %s" % phase_name)
                return

        self.solids_current_phase = self.P = None
        self.solids_current_phase_name = None

        tw.itemSelectionChanged.disconnect() # Avoid selection callbacks during delete.  Re-enabled below
        try:
            name = tw.item(row, 0).text()
            tw.removeRow(row)
            del self.solids[name] # This is an ordered dict, keyed by name - no 'hole' problem

            if len(self.solids) < DIM_M:
                ui.toolbutton_solids_add_phase.setEnabled(True)
                ui.toolbutton_solids_search.setEnabled(bool(self.particle_popup.db))

            self.update_keyword('mmax', len(self.solids))

            # Clear out all keywords related to deleted phase
            # Note Must delete self.solids[name] before calling delete_phase_keys
            self.solids_delete_phase_keys(phase)

            # these panels all have a 'current solid' (index) which must
            # be adjusted
            self.bcs_delete_solids_phase(phase)
            self.ics_delete_solids_phase(phase)
            self.pss_delete_solids_phase(phase)
            self.iss_delete_solids_phase(phase)
            self.output_delete_solids_phase(phase)

            # Reallocate volume fraction to void phase
            for BC in self.bcs:
                if self.project.get_value('bc_ep_g', default=None, args=[BC]) is not None:
                    self.update_bc_ep_g(indices=[BC])

            for IC in self.ics:
                if self.project.get_value('ic_ep_g', default=None, args=[IC]) is not None:
                    self.update_ic_ep_g(indices=[IC])

            # Fixup phase names in mfix_gui_comments
            for pat in 'solids_phase_name', 'pic_const_statwt':
                for (k,v) in list(self.project.mfix_gui_comments.items()):
                    if k.startswith(pat):
                        del self.project.mfix_gui_comments[k]
            for (i, (name, solid)) in enumerate(self.solids.items(), 1):
                self.project.mfix_gui_comments['solids_phase_name(%s)'%i] = name
                val = solid.get('pic_const_statwt')
                if val is not None:
                    self.project.mfix_gui_comments['pic_const_statwt(%s)'%i] = val


            # Fix hole in restitution coeffs
            n = len(self.solids)
            for key in ('des_et_input', 'des_en_input'):
                prev_size = ((n+1)*(n+2))//2 # Size before row deleted
                vals = [self.project.get_value(key, args=i)
                        for i in range(1, 1+prev_size)]
                if any(v is not None for v in vals):
                    new_vals = drop_row_column_triangular(vals, n+1, phase)
                    for (i, val) in enumerate(new_vals, 1):
                        self.update_keyword(key, val, args=i)
                    for i in range(len(new_vals)+1,  len(vals)+1):
                        self.unset_keyword(key, args=i)

            # Delete all reactions involving deleted species
            for species in self.solids_species[phase].keys():
                self.chemistry_delete_reactions_of_species(species)
            # Fix species dictionary
            for n in range(phase, len(self.solids)+1):
                self.solids_species[n] = self.solids_species[n+1]
            del self.solids_species[len(self.solids_species)]

            # Fix PHASE4SCALAR
            self.delete_scalars_of_phase(phase)

            # Update RDF_TYPE if we go from mono- to poly-disperse or vv
            self.check_rdf_type()

            # Update GUI
            self.update_solids_table()

            # ICs enabled/disabled depends on nscalar & number of solids
            self.update_nav_tree()

            # Tabs enable/disable depending on number of solids
            self.solids_update_tabs()
        finally:
            # selection callbacks were disabled
            tw.itemSelectionChanged.connect(self.handle_solids_table_selection)
            self.handle_solids_table_selection()
            self.set_unsaved_flag()


    def update_solids_species_groupbox(self):
        """enable/disable species tables based on state"""
        # Species data required under any of the following conditions:
        #  Solving species equations
        #  Energy equations are solved with mixture specific heat model
        phase = self.solids_current_phase
        ui = self.ui.solids

        if phase is None:
            enabled = False
        else:
            species_eq = self.project.get_value('species_eq', args=phase, default=True)
            energy_eq = self.project.get_value('energy_eq', default=True)
            enabled = (species_eq) or (energy_eq and self.solids_specific_heat_model == MIXTURE)

        ui.groupbox_species.setEnabled(enabled)
        # Buttons seem to take up a lot of space when table is shrunk
        # OTOH we don't do this in the fluid pane, and hiding the buttons
        # is bad for locatability
        ui.frame_add_delete_copy_species.setVisible(enabled)


    def update_solids_baseline_groupbox(self, density_model):
        #Baseline (unreacted) composition selection:
        # Available only for variable solids density model
        ui = self.ui.solids
        tw = ui.tablewidget_solids_baseline
        enabled = density_model==VARIABLE
        ui.groupbox_baseline.setEnabled(enabled)
        if not enabled:
            ui.groupbox_baseline.setToolTip("Only available with variable solids density model")
            tw.hide()#clearContents()
            #tw.setRowCount(0)
            #self.fixup_solids_table(tw)
        else:
            tw.show()
            ui.groupbox_baseline.setToolTip(None)
            self.update_solids_baseline_table()

    def handle_solids_mass_fraction(self, widget, value_dict, args):
        phase = self.solids_current_phase
        if phase is None:
            return
        ui = self.ui.solids
        key = 'x_s0'
        val = value_dict[key]
        # TODO defaults for x_s0?
        table = ui.tablewidget_solids_baseline
        widget.updateValue(key, val)
        if val == '':
            self.unset_keyword(key, args=args)
        else:
            self.update_keyword(key, val, args=args)
        self.update_solids_mass_fraction_total()


    def handle_pic_const_statwt(self, widget, value_dict, args):
        P = self.solids_current_phase
        if P is None:
            return
        solid = list(self.solids.values())[P-1]
        key = 'pic_const_statwt' # Not really a keyword
        prev = solid.get(key, 1.0)
        val = value_dict[key]
        if prev == val:
            return
        self.set_unsaved_flag()
        solid[key] = val
        # Update all BCs and ICs which are at the default
        key = 'bc_pic_mi_const_statwt'
        for BC in self.bcs:
            if self.project.get_value('bc_type', args=[BC]).endswith('MI'):
                if self.project.get_value(key, args=[BC,P]) in (prev, None):
                    self.update_keyword(key, val, args=[BC,P])
        key = 'ic_pic_const_statwt'
        for IC in self.ics:
            if self.project.get_value(key, args=[IC,P]) in (prev, None):
                self.update_keyword(key, val, args=[IC,P])
        self.project.mfix_gui_comments['pic_const_statwt(%s)'%P] = val


    def update_solids_mass_fraction_total(self):
        ui = self.ui.solids
        key = 'x_s0'
        phase = self.solids_current_phase
        if phase is None:
            return
        table = ui.tablewidget_solids_baseline
        if table.rowCount() == 0:
            return
        total = sum(safe_float(self.project.get_value(key, default=0.0, args=[phase,i]))
                    for i in range(1,len(self.solids_species[phase])+1))
        total = round(total, 6)
        item = table.item(table.rowCount()-1, 1)
        font = item.font()
        font.setBold(True)
        item.setFont(font)
        item.setText(str(total))
        if total != 1.0:
            item.setForeground(Qt.red)
            #We should warn, but this creates too many popups while solid is being set up
            #self.warning("Mass fractions sum to %s, must be 1.0" % total, popup=True)
        elif ui.isEnabled():
            item.setForeground(Qt.black) # Fixme looks wrong when greyed-out


    def handle_solids_inert_species(self, species_index, val):
        phase = self.solids_current_phase
        if phase is None:
            return
        if val:
            self.update_keyword('inert_species', species_index, args=phase)
        else:
            self.unset_keyword('inert_species', args=phase)

        for (i, cb) in enumerate(self.solids_inert_species_checkboxes, 1):
            if i != species_index:
                cb.setChecked(False)


    def update_solids_baseline_table(self):
        ui = self.ui.solids
        table = ui.tablewidget_solids_baseline
        phase = self.solids_current_phase
        if phase is None:
            return

        if self.solids_species[phase]:
            nrows = len(self.solids_species[phase])+1 # "Total" row at end
        else:
            nrows = 0

        inert_species = self.project.get_value('inert_species', default=None, args=phase)

        if nrows != table.rowCount():
            table.clearContents() # Could save some of the rows, but this is easier
            table.setRowCount(nrows)
            self.solids_inert_species_checkboxes = []
            def make_item(val):
                item = QtWidgets.QTableWidgetItem('' if val is None else str(val))
                set_item_noedit(item)
                return item

            for (row, (alias,data)) in enumerate(self.solids_species[phase].items()):
                table.setItem(row, 0, make_item(alias))

                # mass fraction
                le = LineEdit()
                le.setdtype('dp')
                le.setValInfo(min=0.0, max=1.0)
                key = 'x_s0'
                le.key = key
                le.value_updated.connect(self.handle_solids_mass_fraction)
                table.setCellWidget(row, 1, le)

                # "Inert" checkbox
                cb = QtWidgets.QCheckBox()
                cb.key = 'inert_species'
                self.solids_inert_species_checkboxes.append(cb)
                cb.setChecked(row+1 == inert_species)
                cb.clicked.connect(lambda val, species_index=row+1:
                                   self.handle_solids_inert_species(species_index, val))
                # center checkbox in cell
                layout = QtWidgets.QHBoxLayout()
                layout.addWidget(cb)
                set_alignment(layout, Qt.AlignCenter)
                layout.setContentsMargins(0,0,0,0)
                widget = QtWidgets.QWidget()
                widget.setLayout(layout)
                widget.checkbox = cb
                table.setCellWidget(row, 2, widget)
            # Total
            if nrows > 0:
                table.setItem(nrows-1, 0, make_item("Total"))
                table.setItem(nrows-1, 1, make_item(''))
                item = table.item(nrows-1, 0)
                font = item.font()
                font.setBold(True)
                item.setFont(font)
                table.setItem(nrows-1, 2, make_item('')) # Avoid editable cell in corner

        # Set widgets to correct values and adjust dynamic tooltips
        for (row, (alias,data)) in enumerate(self.solids_species[phase].items()):
            table.item(row, 0).setText(alias)
            # mass fraction
            key = 'x_s0'
            le = table.cellWidget(row, 1)
            args = [phase, row+1]
            val = self.project.get_value(key, args=args, default=None)
            le.key = key
            le.args = args
            self.add_tooltip(le, key) # update args
            if val is not None:
                le.updateValue(key, val)
            # inert species
            cw = table.cellWidget(row, 2)
            cb = cw.checkbox # checkbox is centered in a layout
            cb.setChecked(row+1 == inert_species)
            cw.args = [phase]
            self.add_tooltip(cw, key='inert_species', value=row+1) # Set on whole cell
        if nrows > 0:
            self.update_solids_mass_fraction_total()
        self.fixup_solids_table(table)


    def solids_species_revert(self):
        # Nothing to do, popup was operating on a copy all along
        pass


    def solids_species_save(self):
        ui = self.ui.solids
        phase = self.solids_current_phase
        if phase is None:
            return
        old_aliases = dict(enumerate(self.solids_species[phase].keys(), 1))
        self.set_unsaved_flag() # TODO check if anything really changed
        self.solids_species[phase] = OrderedDict((alias, deepcopy(data))
            for (alias,data) in self.species_popup.defined_species.items())
        for (species, data) in enumerate(self.solids_species[phase].values(), 1):
            density = data.get('density')
            if density is not None:
                val = self.get_retained_keyword('ro_xs0', args=[phase, species])
                if val is not None:
                    self.retained_keys[('ro_xs0', (phase, species))] = density
        self.solids_normalize_keys()
        self.update_solids_species_table()
        self.update_solids_baseline_groupbox(self.solids_density_model)
        self.fixup_solids_table(ui.tablewidget_solids_species)
        self.fixup_solids_table(ui.tablewidget_solids_baseline)

        for (i,name) in enumerate(self.solids_species[phase].keys(),1):
            old_alias = old_aliases.get(i)
            new_alias = name
            if old_alias is None:
                continue
            if new_alias != old_alias:
                self.chemistry_rename_species(old_alias, new_alias)

        self.update_nav_tree() # Chemistry
        self.bcs_check_wall_keys() ## Set BC keys for solids species at any defined walls
        # Update IC keys for new solids
        for i in self.ics:
            self.ics_set_default_keys(i)


    def update_solids_species_table(self):
        """Update table in solids pane.  Also sets nmax_s, species_s and species_alias_s keywords,
        which are not tied to a single widget"""
        ui = self.ui.solids
        table = ui.tablewidget_solids_species
        table.clearContents()
        phase = self.solids_current_phase
        if phase is None or self.solids_species.get(phase) is None:
            self.update_solids_species_groupbox()
            return

        nrows = len(self.solids_species[phase])
        table.setRowCount(nrows)
        def make_item(val):
            item = QtWidgets.QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item
        old_nmax_s = self.project.get_value('nmax_s', default=0, args=phase)
        nmax_s = len(self.solids_species[phase])
        self.update_keyword('nmax_s', nmax_s, args=phase)

        for (row, (alias,data)) in enumerate(self.solids_species[phase].items()):
            args = [phase,row+1]
            for (col, key) in enumerate(('alias', 'phase', 'density', 'mol_weight', 'h_f')):
                data['alias'] = alias # for make_item
                val = data.get(key)
                if val is None and key == 'density':
                    val = self.get_retained_keyword('ro_xs0', args=args)
                item = make_item(val)
                if key=='alias':
                    self.add_tooltip(item, key='species_alias_s')
                elif key=='density':
                    self.add_tooltip(item, key='ro_xs0')
                table.setItem(row, col, item)
            # Fixme, we should not be setting keywords in an 'update_table' method
            density = data.get('density')
            if self.solids_density_model == CONSTANT: # Issues/646
                self.retain_keyword('ro_xs0', args=args)
                self.unset_keyword('ro_xs0', args=args)
            if density is not None and self.solids_density_model == VARIABLE:
                self.update_keyword('ro_xs0', density, args=args)

        # Clear any keywords with indices above nmax_s NEEDED?
        for i in range(nmax_s+1, old_nmax_s+1):
            for kw in ('species_s', 'species_alias_s', 'x_s0', 'ro_xs0'):
                self.unset_keyword(kw, args=[phase,i])

        self.fixup_solids_table(ui.tablewidget_solids_species)
        self.fixup_solids_table(ui.tablewidget_solids_baseline)

        # Autoselect if unique row
        for tw in (ui.tablewidget_solids_species, ui.tablewidget_solids_baseline):
            if tw.rowCount()==1 and get_selected_row(tw) is None:
                tw.setCurrentCell(0, 0)


    def handle_solids_species_selection(self):
        ui = self.ui.solids
        tw = ui.tablewidget_solids_species
        row = get_selected_row(tw)
        enabled = (row is not None)
        ui.toolbutton_solids_species_delete.setEnabled(enabled)
        ui.toolbutton_solids_species_edit.setEnabled(enabled)
        if enabled:
            # Double-click signal comes too soon, while mouse button is still down
            # and disturbs row selection.
            #tw.doubleClicked.connect(lambda: (time.sleep(0.25), self.solids_species_edit()))
            tw.doubleClicked.connect(lambda: QTimer.singleShot(250, self.solids_species_edit))
        else:
            try:
                tw.doubleClicked.disconnect() #self.solids_species_edit)
            except:
                pass

    def handle_solids_psd_selection(self):
        ui = self.ui.solids
        tw = ui.tablewidget_psd
        row = get_selected_row(tw)
        enabled = (row is not None)
        ui.toolbutton_delete_psd.setEnabled(enabled)
        ui.toolbutton_edit_psd.setEnabled(enabled)
        if enabled:
            # Double-click signal comes too soon, while mouse button is still down
            # and disturbs row selection.
            #tw.cellDoubleClicked.connect(lambda row,col: (time.sleep(0.25), self.solids_psd_edit(col=col)))
            tw.cellDoubleClicked.connect(lambda row,col: QTimer.singleShot(250, lambda col=col: self.solids_psd_edit(col=col)))

        else:
            try:
                tw.cellDoubleClicked.disconnect() #self.solids_psd_edit)
            except:
                pass


    def solids_normalize_keys(self):
        # species-alias unification,  issues/869
        for (phase, phase_data) in self.solids_species.items():
            for (sp, (alias,data)) in enumerate(phase_data.items(), 1):
                 args = [phase, sp]
                 species_s = self.species_burcat_name(alias, phase, sp)
                 self.update_keyword('species_s', species_s, args=args)
                 data['species'] = species_s # for thermo_data
                 self.update_keyword('species_alias_s', alias, args=args)
                 # We're avoiding mw_s in favor of the settings in THERMO DATA
                 #self.update_keyword('mw_s', data['mol_weight'], args=args)
            self.project.update_thermo_data(self.solids_species[phase])

        # migrate usr_ks to ks_model 'USR' issues/1034
        for P in range(1, 1+len(self.solids)):
            usr_ks = self.project.get_value('usr_ks', args=[P])
            ks_model = self.project.get_value('ks_model', args=[P])
            model = self.project.get_value('solids_model', args=[P])
            k_s0 = self.project.get_value('k_s0', args=[P])
            if model=='PIC':
                self.unset_keyword('usr_ks', args=[P])
                self.unset_keyword('ks_model', args=[P])
                self.unset_keyword('k_s0', args=[P])
                continue
            if usr_ks:
                self.unset_keyword('usr_ks', args=[P])
                self.update_keyword('ks_model', 'USR', args=[P])
            elif ks_model is None:
                if k_s0 is not None:
                    if k_s0 == 0:
                        self.update_keyword('ks_model', 'NONE', args=[P])
                    else:
                        if model == 'TFM':
                            self.update_keyword('ks_model', 'CONST_EFF', args=[P])
                        elif model in ('DEM','CGP'):
                            self.update_keyword('ks_model', 'MUSSER', args=[P])
                else: # K_S0 undefined, use defaults
                    if model == 'TFM':
                        self.update_keyword('ks_model', 'BAUER', args=[P])
                        self.update_keyword('k_s0', 1.0, args=[P])
                    elif model in ('DEM','CGP'):
                        self.update_keyword('ks_model', 'MUSSER', args=[P])
                        self.update_keyword('k_s0', 1.0, args=[P])


    def solids_species_add(self):
        phase = self.solids_current_phase
        if phase is None:
            return
        # Workaround for deleted phase
        if phase not in self.solids_species:
            self.solids_species[phase] = OrderedDict()
        sp = self.species_popup
        sp.set_phases('SC')
        sp.do_search('')
        # how to avoid this if dialog open already?
        sp.reset_signals()
        sp.cancel.connect(self.solids_species_revert)
        sp.save.connect(self.solids_species_save)
        sp.defined_species = deepcopy(self.solids_species[phase])
        sp.update_defined_species()
        sp.reserved_aliases = set(map(str.lower, self.species_all_aliases()))
        sp.setWindowTitle("Species for %s" %self.solids_current_phase_name)
        sp.enable_density(True)
        sp.popup()


    def solids_species_delete(self):
        phase = self.solids_current_phase
        if phase is None:
            return
        ui = self.ui.solids

        tw = ui.tablewidget_solids_species
        row = get_selected_row(tw)
        if row is None: # No selection
            return

        alias = tw.item(row,0).text()
        refs = self.solids_get_species_refs(phase, alias)

        if refs:
            # Do we want to show the references?
            ret = self.message(text="%s has %d reference%s.\nAll references will be deleted\nContinue?" %
                               (alias,
                                len(refs),
                                's' if len(refs)!=1 else ''),
                               icon='question',
                              buttons=['ok', 'cancel'])
            if ret != 'ok':
                self.print_internal("Not deleting %s" % alias)
                return


        tw.clearSelection() #?

        # NB Must remove from solids species before calling 'delete_species_keys'
        species_index = 1 + list(self.solids_species[phase].keys()).index(alias)
        self.solids_species[phase].pop(alias, None)
        self.solids_delete_species_keys(phase, species_index)
        self.update_solids_species_table()
        self.update_solids_baseline_groupbox(self.solids_density_model)
        self.fixup_solids_table(ui.tablewidget_solids_species)
        self.fixup_solids_table(ui.tablewidget_solids_baseline)

        # Sigh, we have to update the row in the popup too.
        # Should the popup just be modal, to avoid this?
        sp = self.species_popup
        sp.defined_species = deepcopy(self.solids_species[phase])
        sp.update_defined_species()
        self.chemistry_delete_reactions_of_species(alias)
        self.update_nav_tree() # Chemistry


    def solids_species_edit(self):
        phase = self.solids_current_phase
        if phase is None:
            return
        ui = self.ui.solids
        if not ui.input_enabled: # prevent double-click from popping us up during run
            return
        tw = ui.tablewidget_solids_species
        row = get_selected_row(tw)

        sp = self.species_popup
        sp.set_phases('SC')
        sp.reset_signals()
        sp.cancel.connect(self.solids_species_revert)
        sp.save.connect(self.solids_species_save)
        sp.defined_species = deepcopy(self.solids_species[phase])
        sp.update_defined_species()
        sp.reserved_aliases = set(map(str.lower, self.species_all_aliases()))

        if row is not None:
            sp.reserved_aliases.discard(tw.item(row,0).text().lower())
            sp.tablewidget_defined_species.setCurrentCell(row, 0)
            # Initialize search box to current species (?)
            sp.do_search(list(self.solids_species[phase].keys())[row])
        else:
            sp.do_search('')
            sp.tablewidget_defined_species.clearSelection()

        sp.setWindowTitle("Species for %s" % self.solids_current_phase_name)
        sp.enable_density(True)
        sp.popup()
        col = tw.currentColumn()
        if col==0:
            sp.ui.lineedit_alias.setFocus()
        elif col==1:
            # Phase is not editable
            pass
        elif col==2:
            sp.ui.lineedit_density.setFocus()
        elif col==3:
            sp.ui.lineedit_mol_weight.setFocus()
        elif col==4:
            sp.ui.lineedit_h_f.setFocus()



    def solids_get_species_refs(self, phase, species):
        ret = []

        msg = self.chemistry_get_species_refs(species)
        if msg:
            ret.append("reaction %s" % msg)

        species_num = 1 + list(self.solids_species[phase].keys()).index(species) # :(

        for key in keyword_args.keys_by_type['species']:
            indices = self.project.get_key_indices(key)
            if not indices: #Keys not set
                continue
            arg_types = keyword_args.keyword_args.get(key, ())
            if 'phase' not in arg_types: # It's a fluid species, not solid
                continue
            if arg_types == ('phase', 'species'): # This will be deleted
                continue
            phase_pos = arg_types.index('phase')
            species_pos = arg_types.index('species')
            for args in indices:
                if args[phase_pos] != phase or args[species_pos] != species_num:
                    continue
                if self.project.get_value(key, args=args): # Ignore settings of None, False, or 0
                    ret.append(format_key_with_args(key,args))

        return ret


    def solids_get_phase_refs(self, phase):
        """Return list of references to phase"""
        ret = []
        for i in range(1, 1+self.project.get_value('nscalar', default=0)):
            if self.project.get_value('phase4scalar', args=[i]) == phase:
                ret.append(self.scalar_names.get(i, 'Scalar %s' % i))

        for species in self.solids_species[phase]:
            refs = self.solids_get_species_refs(phase, species)
            for ref in refs:
                ret.append("%s (%s)" % (ref, species))

        for key in keyword_args.keys_by_type['phase']:
            indices = self.project.get_key_indices(key)
            if not indices:
                continue
            arg_types = keyword_args.keyword_args.get(key, ())
            if arg_types == ('phase',):
                # Single reference is OK.  We will
                # delete these keys along with the phase in solids_delete_phase_keys
                continue
            if arg_types == ('phase', 'species'):
                # These will all be deleted when we delete the phase
                continue
            phase_pos = arg_types.index('phase')

            for args in indices:
                if args[phase_pos] != phase:
                    continue
                val = self.project.get_value(key, args=args)
                if val is not None:
                # TODO check if value is default and if so, allow delete
                    if val != 0.0: # Assume 0 is default
                        ret.append(format_key_with_args(key,args))

        return ret


    def solids_delete_species_keys(self, phase_index, species_index):
        """Delete all keywords associated with specified species,
        fixing up the resulting gap in sequence"""
        # NB this assumes self.solids_species[phase][species] has already been deleted
        key = 'inert_species'
        val = self.project.get_value(key, args=phase_index)
        if val is not None:
            if val == species_index:
                self.unset_keyword(key, args=phase_index)
            elif val > species_index:
                self.update_keyword(key, val-1, args=phase_index)
        # Handle retained inert_species
        val = self.get_retained_keyword(key, args=phase_index)
        if val is not None:
            args = (phase_index,)
            if val == species_index:
                del self.retained_keys[(key, args)]
            elif val > species_index:
                self.retained_keys[(key, args)] = val-1

        for key in keyword_args.keys_by_type['species']:
            indices = self.project.get_key_indices(key)
            if not indices:
                continue
            arg_types = keyword_args.keyword_args.get(key, ())
            if 'phase' not in arg_types: # fluid species
                continue
            phase_pos = arg_types.index('phase')
            species_pos = arg_types.index('species')
            for args in sorted(indices):
                args_phase = args[phase_pos]
                args_species = args[species_pos]
                if args_phase != phase_index:
                    continue
                if args_species < species_index:
                    continue
                args1 = list(args)
                args1[species_pos] += 1
                val = self.project.get_value(key, args=args1)
                self.update_keyword(key, val, args=args)

        # Now handle retained keys
        for ((key,args), val) in sorted(list(self.retained_keys.items())):
            if not args:
                continue
            arg_types = keyword_args.keyword_args.get(key,())
            if 'phase' not in arg_types:
                continue
            if 'species' not in arg_types:
                continue
            phase_pos = arg_types.index('phase')
            species_pos = arg_types.index('species')
            args_phase = args[phase_pos]
            args_species = args[species_pos]
            if args_phase != phase_index:
                continue
            if args_species < species_index:
                continue
            args1 = list(args)
            args1[species_pos] += 1
            val = self.get_retained_keyword(key, args=args1)
            if val is not None:
                self.retained_keys[(key, args)] = val
            else:
                del self.retained_keys[(key, args)]


    def solids_delete_phase_keys(self, phase):
        """Delete all keywords associated with specified phase (1-based index),
        fixing up the resulting gap in sequence"""
        # NB this assumes that self.solids[phase] has already been deleted!
        prev_size = len(self.solids) + 1 # Size before row deleted
        for key in keyword_args.keys_by_type['phase']:
            arg_types = keyword_args.keyword_args.get(key, ())
            indices = self.project.get_key_indices(key)
            if not indices:
                continue
            if arg_types == ('phase',):
                # Copy/slide/trim
                vals = [self.project.get_value(key, args=i)
                        for i in range(1, 1+prev_size)]
                del vals[phase-1] # Slide (1-based)
                for (i, val) in enumerate(vals, 1):
                    self.update_keyword(key, val, args=i)
                self.unset_keyword(key, args=prev_size) #Trim off the end
            elif arg_types == ('phase', 'phase'):
                tmp_data = {}
                for args in indices:
                    p1, p2 = args
                    p1a = p1-1 if p1>=phase else p1
                    p2a = p2-1 if p2>=phase else p2
                    tmp_data[(p1a,p2a)] = self.project.get_value(key, args=args)
                    if (p1==prev_size or p2==prev_size):
                        self.unset_keyword(key, args=(p1, p2))
                for (args, val) in tmp_data.items():
                    self.update_keyword(key, val, args=args)
            else:
                phase_pos = arg_types.index('phase')
                new_vals = {}
                for args in sorted(indices):
                    args_phase = args[phase_pos]
                    if args_phase < phase: # Unaffected
                        continue
                    args1 = list(args)
                    args1[phase_pos] += 1
                    val = self.project.get_value(key, args=args1)
                    self.update_keyword(key, val, args=args)

        # Now handle retained keys
        for ((key,args), val) in sorted(list(self.retained_keys.items())):
            if not args:
                continue
            arg_types = keyword_args.keyword_args.get(key, ())
            if 'phase' not in arg_types:
                continue
            phase_pos = arg_types.index('phase')
            # We don't worry about ('phase', 'phase') yet because r_p is not getting retained
            args_phase = args[phase_pos]
            if args_phase < phase:
                continue
            args1 = list(args)
            args1[phase_pos] += 1
            val = self.get_retained_keyword(key, args=args1)
            if val is not None:
                self.retained_keys[(key,args)] = val
            else:
                del self.retained_keys[(key, args)]

    def solids_extract_psd(self):
        # TODO create entries in self.psd for any *PSD keys
        # this is just for compatibility w/ non-GUI projects
        pass

    def solids_psd_from_str(self, val):
        data = JSONDecoder().decode(val)
        for (name, info) in data.items():
            self.psd[name] = info


    def reset_solids(self):
        ui = self.ui.solids
        # Set all solid-related state back to default
        self.init_solids_default_models()
        self.solids_current_phase = self.P = None
        self.solids_current_phase_name = None
        self.solids.clear()
        self.solids_species.clear()
        self.psd.clear()
        self.update_solids_table()
        self.update_solids_detail_pane()
        self.solids_current_tab = 0
        ui.toolbutton_solids_add_phase.setEnabled(True)
        ui.toolbutton_solids_search.setEnabled(bool(self.particle_popup.db))
        self.solids_change_tab(0)

        ui.groupbox_filter_particles.setChecked(False)
        ui.groupbox_filter_particles_checked = None

        # Clean up dynamic widgets
        for i1 in ui.dynamic_widgets.values():
            for i2 in i1:
                if isinstance(i2, (list, tuple)):
                    for i3 in i2:
                        i3.deleteLater()
                else:
                    i2.deleteLater()
        ui.dynamic_widgets.clear()
        # TODO (?)  reset THERMO_DATA ?
