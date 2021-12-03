# methods to deal with fluid phase
from copy import deepcopy
from collections import OrderedDict

from qtpy import QtWidgets
from qtpy.QtCore import QTimer

from mfixgui import default_values
from mfixgui.constants import *
from mfixgui.species_handler import SpeciesHandler
from mfixgui.tools import format_key_with_args, keyword_args
from mfixgui.tools.qt import (get_selected_row, set_item_noedit,
                              get_combobox_item)

class FluidHandler(SpeciesHandler):
    # Defaults
    def init_fluid_default_models(self):
        ui = self.ui.fluid
        self.fluid_density_model = IDEAL
        self.fluid_viscosity_model = OTHER # Sutherland
        self.fluid_mol_weight_model = MIXTURE
        self.fluid_specific_heat_model = MIXTURE
        self.fluid_tc_model = AIR
        self.fluid_diffusion_model = AIR
        for s in ('density', 'viscosity', 'mol_weight',
                  'specific_heat', 'tc', 'diffusion'):
            name = 'fluid_%s_model' % s
            getattr(ui, 'combobox_'+name).default(getattr(self, name))

    ## Fluid phase methods
    def handle_fluid_species_eq(self, enabled):
        ui = self.ui.fluid
        self.update_keyword('species_eq', enabled, args=[0])
        for item in (ui.combobox_fluid_diffusion_model,
                     ui.label_fluid_diffusion_model,
                     # more ?
                     ):
            item.setEnabled(enabled)
        # dif_g0 == diffusion coeff model
        items = (ui.lineedit_keyword_dif_g0,
                 ui.label_dif_g0_units)
        for item in items:
            item.setEnabled(enabled and (self.fluid_diffusion_model == CONSTANT))

    def handle_fluid_momentum_eq(self, key, enabled):
        self.update_keyword(key, enabled, args=[0])


    def init_fluid_handler(self):
        self.fluid_species = OrderedDict() # keyed by ALIAS

        ui = self.ui.fluid

        ui.lineedit_fluid_phase_name.default_value = self.fluid_phase_name = "Fluid"

        self.init_fluid_default_models()
        # Handle a number of cases which are essentially the same
        # see 'set_fluid_mol_weight_model' below to help understand this
        def make_fluid_model_setter(self, name, key):
            def setter(model):
                ui = self.ui.fluid
                key_g0 = 'c_pg0' if key=='c_p' else key + '_g0'
                key_usr = 'usr_cpg' if key=='c_p' else 'usr_' + key + 'g'

                setattr(self, name, model) # self.fluid_<name>_model = model
                cb = getattr(ui, 'combobox_' + name)
                prev_model = cb.currentIndex()
                if model != prev_model:
                    cb.setCurrentIndex(model)
                # Make tooltip match item setting
                cb.setToolTip(get_combobox_item(cb, model).toolTip())
                #
                # Make combobox locatable
                cb.keys = [key_g0, key_usr]
                label = getattr(ui, 'label_' + name)
                label.keys = cb.keys
                # Enable lineedit for constant model
                lineedit = getattr(ui, 'lineedit_keyword_%s' % key_g0)
                label = getattr(ui, 'label_%s_units' % key_g0)

                for item in (lineedit, label):
                    item.setEnabled(model==CONSTANT)

                # Workaround for disabled fluid solver
                if self.fluid_solver_disabled and key_g0 == 'ro_g0':
                    lineedit.minimum = 0
                    lineedit.saved_value = 0.0
                    return

                if model == CONSTANT:
                    if self.project.get_value(key_g0) is None:
                        value = self.get_retained_keyword(key_g0, default=getattr(default_values, key_g0, None))
                        if value != '' and value is not None:
                            self.update_keyword(key_g0, value) # Restore keyword value
                    self.unset_keyword(key_usr) # Issues/435
                elif model == UDF:
                    self.retain_keyword(key_g0)
                    self.unset_keyword(key_g0)
                    self.update_keyword(key_usr, True)
                    lineedit.updateValue(key_g0, None)
                else: # Ideal gas law, Sutherland, etc
                    self.retain_keyword(key_g0)
                    self.unset_keyword(key_g0)
                    self.unset_keyword(key_usr)
                    lineedit.updateValue(key_g0, None)
                if key_g0 == 'ro_g0':
                    if model == CONSTANT:
                        lineedit.required = True
                        lineedit.exclude_min = True
                    else:
                        lineedit.required = False
                        lineedit.exclude_min = False

                # anything else to do in this case? validation?

            return setter

        # Create setters for the cases which are similar (mol. wt. handled separately)
        for (name, key) in (
                ('density', 'ro'),
                ('viscosity', 'mu'),
                ('specific_heat', 'c_p'),
                ('tc', 'k'),
                ('diffusion', 'dif')):
            model_name = 'fluid_%s_model' % name
            setattr(self, 'set_'+model_name, make_fluid_model_setter(self, model_name, key))

            # Set the combobox default value (?)
            cb = getattr(ui, 'combobox_'+model_name)
            cb.default_value = getattr(self, model_name)

            # Tooltips
            key_g0 = 'c_pg0' if key=='c_p' else key + '_g0'
            key_usr = 'usr_cpg' if key=='c_p' else 'usr_' + key + 'g'
            item =  get_combobox_item(cb, 0)
            self.add_tooltip(item, key=key_g0)
            item =  get_combobox_item(cb, 1)
            item.setToolTip('<b>%s</b>: Use MFiX default calculation.' % item.text()) #Full name of model
            item =  get_combobox_item(cb, 2)
            self.add_tooltip(item, key=key_usr)

        # Mol weight is special
        cb = ui.combobox_fluid_mol_weight_model
        cb.default_value = self.fluid_mol_weight_model
        item = get_combobox_item(cb, 0)
        self.add_tooltip(item, key='mw_avg')
        item = get_combobox_item(cb, 1)
        item.setToolTip("<b>Mixture</b>: Use MFiX default calculation.")
        cb.setToolTip(get_combobox_item(cb,
                                        cb.currentIndex()).toolTip())

        cb = ui.checkbox_species_eq
        self.add_tooltip(cb, key='species_eq')
        cb.clicked.connect(self.handle_fluid_species_eq)

        ui.lineedit_fluid_phase_name.value_updated.connect(
            self.handle_fluid_phase_name)

        for c in 'xyz':
            key = 'momentum_%s_eq' % c
            cb = getattr(ui, 'checkbox_'+key)
            self.add_tooltip(cb, key)
            cb.clicked.connect(lambda enabled, key=key: self.handle_fluid_momentum_eq(key, enabled))

        # Fluid phase models
        for name in ('density', 'viscosity', 'specific_heat', 'mol_weight',
                     'tc', 'diffusion'):
            model_name = 'fluid_%s_model' % name
            cb = getattr(ui, 'combobox_%s' % model_name)
            setter = getattr(self,'set_%s' % model_name)
            cb.currentIndexChanged.connect(setter)

        # Fluid species
        tb = ui.toolbutton_fluid_species_add
        #tb.key = 'nmax_g' # locatability
        tb.clicked.connect(self.fluid_species_add)
        tb = ui.toolbutton_fluid_species_edit
        tb.clicked.connect(self.fluid_species_edit)
        tb.setEnabled(False)
        tb = ui.toolbutton_fluid_species_delete
        #tb.key = 'nmax_g' # locatability
        tb.setEnabled(False)
        tb.clicked.connect(self.fluid_species_delete)
        tw = ui.tablewidget_fluid_species
        tw.itemSelectionChanged.connect(self.handle_fluid_species_selection)
        self.fixup_fluid_table()

        # Note, with find-by-args this will have to select a table row, not the
        # groupbox
        # Should nmax_g be on +/- buttons or on the groupbox? Both!
        ui.groupbox_species.keys = ['species_alias_g', 'species_g', 'nmax_g'] # Locatability
        #ui.lineedit_keyword_ro_g0.required = True # Only required when constant density
        for tb in (ui.toolbutton_fluid_species_add,
                   ui.toolbutton_fluid_species_delete,
                   ui.toolbutton_fluid_species_edit):
            tb.keys = ['species_alias_g', 'species_g', 'nmax_g'] # Locatability

    def fixup_fluid_table(self):
        hv = QtWidgets.QHeaderView
        ui = self.ui.fluid
        tw = ui.tablewidget_fluid_species
        resize = tw.horizontalHeader().setSectionResizeMode
        for n in range(tw.columnCount()):
            resize(n, hv.ResizeToContents if n>0
                   else hv.Stretch)


    # molecular wt model only has 2 choices, and the key names don't
    # follow the same pattern, so create its setter specially
    def set_fluid_mol_weight_model(self, model):
        ui = self.ui.fluid
        self.fluid_mol_weight_model = model
        cb = ui.combobox_fluid_mol_weight_model
        # Make tooltip match setting (for longer names which are truncated)
        cb.setToolTip(get_combobox_item(cb, cb.currentIndex()).toolTip())
        prev_model = cb.currentIndex()
        if model != prev_model:
            cb.setCurrentIndex(model)
        # Enable lineedit for constant mol_weight model
        lineedit = ui.lineedit_keyword_mw_avg
        label = ui.label_mw_avg_units
        for item in (lineedit, label):
            item.setEnabled(model==CONSTANT)
        if model == CONSTANT:
            value = lineedit.value # Possibly re-enabled gui item
            if value != '' and self.project.get_value("mw_avg") != value:
                self.update_keyword("mw_avg", value) # Restore keyword value
        else: # Mixture
            # TODO: validate, require mw for all component species
            self.unset_keyword("mw_avg")


    def handle_fluid_phase_name(self, widget, value_dict, args):
        ui = self.ui.fluid
        le = ui.lineedit_fluid_phase_name
        old_name = self.fluid_phase_name
        new_name = le.text()
        if new_name in self.solids: # Reject the input
            self.warning("%s: name is in use" % new_name, popup=True)
            le.setText(old_name)
        else:
            self.set_fluid_phase_name(new_name)


    def set_fluid_phase_name(self, value):
        if value != self.ui.fluid.lineedit_fluid_phase_name.text():
            self.ui.fluid.lineedit_fluid_phase_name.setText(value) # set GUI state
        if self.fluid_phase_name == value:
            return
        self.fluid_phase_name = value
        if self.project.mfix_gui_comments.get('fluid_phase_name') != value:
            self.project.mfix_gui_comments['fluid_phase_name'] = value
            self.set_unsaved_flag()

    def fluid_species_revert(self):
        pass

    def fluid_species_save(self):
        self.set_unsaved_flag()
        old_aliases = dict(enumerate(self.fluid_species.keys(), 1))
        self.set_unsaved_flag() # TODO check if anything really changed
        self.fluid_species = OrderedDict((alias, deepcopy(data))
            for (alias,data) in self.species_popup.defined_species.items())
        self.fluid_normalize_keys()
        self.update_fluid_species_table()

        for (i,name) in enumerate(self.fluid_species.keys(),1):
            old_alias = old_aliases.get(i)
            new_alias = name
            if old_alias is None:
                continue
            if new_alias != old_alias:
                self.chemistry_rename_species(old_alias, new_alias)

        # Update IC_X_G for new species
        for i in self.ics:
            self.ics_set_default_keys(i)

        self.update_nav_tree() # Chemistry


    def update_fluid_species_table(self):
        """Update table in fluid pane.  Also set nmax_g, species_g and species_alias_g keywords,
        which are not tied to a single widget"""
        tw = self.ui.fluid.tablewidget_fluid_species
        tw.clearContents()
        if self.fluid_species is None:
            self.fixup_fluid_table()
            return
        nrows = len(self.fluid_species)
        tw.setRowCount(nrows)
        def make_item(val):
            item = QtWidgets.QTableWidgetItem('' if val is None else str(val))
            set_item_noedit(item)
            return item
        old_nmax_g = self.project.get_value('nmax_g')
        nmax_g = len(self.fluid_species)
        if nmax_g > 0:
            self.update_keyword('nmax_g', nmax_g)
        else:
            self.unset_keyword('nmax_g')
        for (row, (alias,data)) in enumerate(self.fluid_species.items()):
            for (col, key) in enumerate(('alias', 'phase', 'mol_weight', 'h_f')):
                data['alias'] = alias # for make_item
                tw.setItem(row, col, make_item(data.get(key)))

        # Clear any keywords with indices above nmax_g NEEDED?
        if old_nmax_g is None:
            old_nmax_g = 0
        for i in range(nmax_g+1, old_nmax_g+1):
            self.unset_keyword('species_g', args=i)
            self.unset_keyword('species_alias_g', args=i)
        self.fixup_fluid_table()


    def fluid_normalize_keys(self):
        # issues/869
        for (sp, (alias,data)) in enumerate(self.fluid_species.items(), 1):
            species_g = self.species_burcat_name(alias, 0, sp)
            data['species'] = species_g # for thermo_data
            self.update_keyword('species_g',
                                species_g,
                                args=[sp])
            self.update_keyword('species_alias_g', alias, args=[sp])
            # We're avoiding mw_g in favor of the settings in THERMO DATA
            #self.update_keyword('mw_g', data['mol_weight'], args=row+1)#
        self.project.update_thermo_data(self.fluid_species)


    def handle_fluid_species_selection(self):
        ui = self.ui.fluid
        tw = ui.tablewidget_fluid_species
        row = get_selected_row(tw)
        enabled = (row is not None)
        ui.toolbutton_fluid_species_delete.setEnabled(enabled)
        ui.toolbutton_fluid_species_edit.setEnabled(enabled)
        if enabled:
            #tw.doubleClicked.connect(lambda: (time.sleep(0.25), self.fluid_species_edit()))
            tw.doubleClicked.connect(lambda: QTimer.singleShot(250, self.fluid_species_edit))
        else:
            try:
                tw.doubleClicked.disconnect() #self.fluid_species_edit)
            except:
                pass


    def fluid_species_add(self):
        sp = self.species_popup
        sp.set_phases('GL')
        sp.do_search('') # Init to full db
        # how to avoid this if dialog open already?
        sp.reset_signals()
        sp.cancel.connect(self.fluid_species_revert)
        sp.save.connect(self.fluid_species_save)
        sp.defined_species = deepcopy(self.fluid_species)
        sp.update_defined_species()
        sp.reserved_aliases = set(map(str.lower, self.species_all_aliases()))
        sp.setWindowTitle("Fluid species")
        sp.enable_density(False)
        sp.popup()


    def fluid_species_delete(self):
        ui = self.ui.fluid
        tw = ui.tablewidget_fluid_species
        row = get_selected_row(tw)
        if row is None: # No selection
            return

        alias = tw.item(row,0).text()

        # Warn if species in use
        refs = self.fluid_get_species_refs(alias)

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
        species_index = 1 + list(self.fluid_species.keys()).index(alias)
        self.bcs_delete_fluid_species(species_index) # special handling for memoized eq_type
        self.fluid_species.pop(alias, None)
        self.fluid_delete_species_keys(species_index) # Must remove fluid species first (why?)
        self.update_fluid_species_table()
        # Sigh, we have to update the row in the popup too.
        # Should the popup just be modal, to avoid this?
        sp = self.species_popup
        sp.defined_species = deepcopy(self.fluid_species)
        sp.update_defined_species()
        self.chemistry_delete_reactions_of_species(alias)
        self.update_nav_tree() # Chemistry

    def fluid_species_edit(self):
        ui = self.ui.fluid
        if not ui.input_enabled: #prevent double-click from popping us up during run
            return
        tw = ui.fluid.tablewidget_fluid_species
        row = get_selected_row(tw)
        sp = self.species_popup
        sp.set_phases('GL')
        sp.reset_signals()
        sp.cancel.connect(self.fluid_species_revert)
        sp.save.connect(self.fluid_species_save)
        sp.defined_species = deepcopy(self.fluid_species)
        sp.update_defined_species()
        sp.reserved_aliases = set(map(str.lower, self.species_all_aliases()))
        if row is not None:
            sp.reserved_aliases.discard(tw.item(row,0).text().lower())
            sp.tablewidget_defined_species.setCurrentCell(row, 0)
            # Initialize search box to current species (?)
            sp.do_search(list(self.fluid_species.keys())[row])
        else:
            sp.do_search('')
            sp.tablewidget_defined_species.clearSelection()

        sp.setWindowTitle("Fluid species")
        sp.enable_density(False)
        sp.popup()
        col = tw.currentColumn()
        if col==0:
            sp.ui.lineedit_alias.setFocus()
        elif col==1:
            # Phase is not editable
            pass
        elif col==2:
            sp.ui.lineedit_mol_weight.setFocus()
        elif col==3:
            sp.ui.lineedit_h_f.setFocus()



    def fluid_get_species_refs(self, species):
        ret = []
        msg = self.chemistry_get_species_refs(species)
        if msg:
            ret.append("reaction %s" % msg)

        species_num = 1 + list(self.fluid_species.keys()).index(species) # :(

        for key in keyword_args.keys_by_type['species']:
            indices = self.project.get_key_indices(key)
            if not indices: #Keys not set
                continue
            arg_types = keyword_args.keyword_args.get(key,())
            if 'phase' in arg_types: # It's a solid species, not fluid
                continue
            if arg_types == ('species',): # This will be deleted
                continue
            species_pos = arg_types.index('species')
            for args in indices:
                if args[species_pos] != species_num:
                    continue
                if self.project.get_value(key, args=args): # Ignore settings of None, False, or 0
                    ret.append(format_key_with_args(key,args))

        return ret


    def fluid_delete_species_keys(self, species):
        """Delete all keywords associated with specified species,
        fixing up the resulting gap in sequence"""
        prev_size = len(self.fluid_species) + 1 # Size before species deleted
        for key in keyword_args.keys_by_type['species']:
            indices = self.project.get_key_indices(key)
            if not indices:
                continue
            arg_types = keyword_args.keyword_args.get(key,())
            if 'phase' in arg_types: # solids species
                continue
            species_pos = arg_types.index('species')
            for args in sorted(indices):
                args_species = args[species_pos]
                if args_species < species:
                    continue
                args1 = list(args)
                args1[species_pos] += 1
                val = self.project.get_value(key, args=args1)
                self.update_keyword(key, val, args=args)


    def setup_fluid(self, allow_disabled_tab=False):
        # Called whenever we switch to fluid tab
        self.P = 0
        ui = self.ui.fluid
        energy_eq = self.project.get_value('energy_eq', default=True)
        species_eq = self.project.get_value('species_eq', default=True, args=[0])

        for item in (ui.label_fluid_specific_heat_model,
                     ui.combobox_fluid_specific_heat_model,
                     ui.label_fluid_tc_model,
                     ui.combobox_fluid_tc_model,
                     # more ?
                     ):
            item.setEnabled(bool(energy_eq))

        ui.checkbox_species_eq.setChecked(bool(species_eq))

        for c in 'xyz':
            key = 'momentum_%s_eq' % c
            cb = getattr(ui, 'checkbox_%s'%key)
            cb.setChecked(self.project.get_value(key, default=True, args=[0]))

        # c_pg0 == specific heat for fluid phase
        lineedit = ui.lineedit_keyword_c_pg0
        label = ui.label_c_pg0_units
        for item in (lineedit, label):
            item.setEnabled(bool(energy_eq)
                            and (self.fluid_specific_heat_model == CONSTANT))

        # k_g0 == thermal conductivity fluid phase
        lineedit = ui.lineedit_keyword_k_g0
        label = ui.label_k_g0_units
        for item in (lineedit, label):
            item.setEnabled(bool(energy_eq)
                            and (self.fluid_tc_model == CONSTANT))

        # issues/533  do not allow species eq when energy eq not enabled and solver == DEM/CGP
        cb = ui.checkbox_species_eq
        if not hasattr(cb, 'tooltip0'):
            cb.tooltip0 = cb.toolTip()

        if self.project.solver in (DEM,CGP) and not energy_eq:
            species_eq = False
            self.update_keyword('species_eq', False, args=[0])
            cb.setEnabled(False)
            cb.setToolTip(cb.tooltip0 + '\nRequires energy equations when using %s model.' %
                          ('DEM' if self.project.solver==DEM else 'CGP'))
        else:
            cb.setEnabled(True)
            cb.setToolTip(cb.tooltip0)
        cb.setChecked(species_eq)

        # Autoselect if unique row
        tw = ui.tablewidget_fluid_species
        if get_selected_row(tw) is None and tw.rowCount() == 1:
            tw.setCurrentCell(0,0)


    def reset_fluid(self):
        # Set all fluid-related state back to default
        ui = self.ui.fluid
        self.fluid_phase_name = 'Fluid'
        self.fluid_species.clear()
        self.init_fluid_default_models()
        le = ui.lineedit_keyword_ro_g0
        le.required = False
        le.minimum = 0.0
        le.saved_value = default_values.ro_g0 # fallback
        le.updateValue('ro_g0', le.saved_value)
        # TODO remove dynamically created input widgets, although this should
        #  get handled next time we call 'setup'

#Fluid phase Task Pane Window: (unavailable if fluid phase was disable)
#    Option to rename the phase (e.g, air, gas)

#    Option to disable Momentum Equations (enabled by default)
# Sets keyword: MOMENTUM_X/Y/Z_EQ(0)

#    Option to enable Species Equations
# Sets keyword: SPECIES_EQ(0)

#    Select Density Model:
# Selection always available
# Available selections:
#  Constant: [DEFAULT]
#    Selection always available
#    Specify constant gas density, RO_G0
#  Ideal gas law:
#    Selection always available
#    Keyword RO_G0 must be undefined

#    Requires a fluid phase molecular weight
#    Requires temperature field for full domain
#  UDF
#    Selection is always available
#    Sets keyword USR_ROg
#    MFIX runtime check verifies UDF was provided

#Select Viscosity Model:
# Selection always available
# Available selections:
#  Constant: [DEFAULT]
#    Selection always available
#    Specify constant gas viscosity, MU_G0
#  Sutherland's law
#    Selection always available
#    Keyword MU_G0 must be undefined
#    Requires temperature field for full domain
#  UDF
#    Selection always available
#    Sets keyword USR_MUg
#    MFIX runtime check verifies UDF was provided

#Select Molecular Weight Model:
# Selection always available
# Available selections:
#  Constant; [DEFAULT]
#    Specification always available
#    Specify constant molecular weight, MW_AVG
#  Mixture:
#    Selection always available
#    Requires molecular weights for all species components

#Select Specific Heat Model:
# Selection available only when solving thermal energy equations
# Available selections:
#  Constant; [DEFAULT]
#    Selection always available
#    Specify constant fluid phase specific heat, C_PG0
#  Mixture:
#    Selection always available
#    Keyword C_PG0 must be undefined
#    Requires specific heats for all species components
#  UDF
#    Selection always available
#    Sets keyword USR_CPg
#    MFIX runtime check verifies UDF was provided

#Select Thermal Conductivity Model:
# Selection only available when solving thermal energy equations
# Available selections:
#  Constant
#    Selection always available
#    Specify constant thermal conductivity, K_G0
#  Temperature dependent (air); [DEFAULT]
#    Selection always available
#    Keyword K_G0 must be undefined
#  UDF
#    Selection always available
#    Set keyword USR_KG
#    MFIX runtime check verifies UDF was provided

#Select Diffusion Coefficient Model:
# Selection only available when solving species equations
# Available selections:
#  Constant
#    Selection always available
#    Specify a constant diffusion coefficient, DIF_G0
#  Dilute Mixture Approximation (air); [DEFAULT]
#    Selection always available
#    Keyword DIF_G0 must be undefined
#    Requires temperature field for full domain
#  UDF
#    Selection always available
#    Sets keyword USR_DIFG
#    MFIX runtime check verifies UDF was provided

#Fluid phase species selection:
# Species data required under any of the following conditions:
#  Solving species equations
#  Density model is the ideal gas law with mixture molecular weight model
#  Energy equations are solved with mixture specific heat model
# Specification panel operates as a popup window triggered by an Add/Edit button
# Summary window provides a list of the species and an overview of some properties

#Fluid phase Material Database window (popup):
#    Select database (BURCAT); later could link in other databases.
#    Capability to search selected database for chemical name
#    Import from database copies the usable information from the database into a new entry in the 'run database'
#    New creates a new 'blank' species in the 'run database' where the user must supply all the thermochemical data.
#    Delete removes an entry from the 'run database'

#NOTE: The gas phase species molecular weights, MW_G(#) cannot be directly specified. This
#keyword is not needed because users can edit the molecular weight in the material database popup
#window.
