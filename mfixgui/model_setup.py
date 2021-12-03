# -*- coding: utf-8 -*-

from mfixgui.constants import *
from mfixgui import default_values
from mfixgui.tools.qt import get_combobox_item

class ModelSetup(object):
    #    Model Setup Task Pane Window: Select MFIX solver and other conservation equations

    def init_model_setup(self):
        ui = self.ui.model_setup
        ui.lineedit_keyword_description.setMaxLength(60)
        ui.combobox_solver.activated.connect(self.set_solver)
        ui.checkbox_disable_fluid_solver.clicked.connect(self.disable_fluid_solver)
        ui.checkbox_keyword_energy_eq.clicked.connect(self.enable_energy_eq)

        ui.lineedit_keyword_description.setMaxLength(60)
        key = 'turbulence_model'
        cb = ui.combobox_turbulence_model
        self.add_tooltip(cb, key)
        cb.activated.connect(self.set_turbulence_model)
        for (i, value) in enumerate(TURBULENCE_MODELS):
            item = get_combobox_item(cb, i)
            self.add_tooltip(item, key, value=value)

        key = 'gravity'
        ui.groupbox_gravity.key = key # Locatability

        key = 'drag_type'
        cb = ui.combobox_drag_type
        cb.activated.connect(self.set_drag_type)
        assert ui.combobox_drag_type.count() == len(DRAG_TYPES)
        for item in (ui.label_drag_type, cb):
            self.add_tooltip(item, key)
        for (i, val) in enumerate(DRAG_TYPES):
            item = get_combobox_item(cb, i)
            self.add_tooltip(item, key=key, description=self.keyword_doc[key]['valids'][val]['note'], value=val)

        ui.lineedit_keyword_ref_length_dg.exclude_min = True # Disallow 0

        cb = ui.combobox_momentum_formulation
        cb.activated.connect(self.set_momentum_formulation)
        self.add_tooltip(ui.label_momentum_formulation, key=None, description=self.keyword_doc['model_b']['description'])
        ui.label_momentum_formulation.keys = cb.keys = ['model_b', 'jackson', 'ishii']
        for i in range(4):
            item = get_combobox_item(cb, i)
            if i == 0:
                key = 'model_b'
                value = False
            elif i == 1:
                key = 'model_b'
                value = True
            elif i == 2:
                key = 'jackson'
                value = None
            else:
                key = 'ishii'
                value = None
            self.add_tooltip(item, key=key, value=value)

        key = 'subgrid_type'
        cb = ui.combobox_subgrid_type
        cb.key = key
        cb.activated.connect(self.set_subgrid_type)
        for (i, value) in enumerate(SUBGRID_TYPES):
            if value == 'NONE':
                self.add_tooltip(get_combobox_item(cb, i), key, value='None', description='No subgrid model')
            else:
                self.add_tooltip(get_combobox_item(cb, i), key, value=value.title())

        ui.checkbox_enable_des_usr_var_size.clicked.connect(self.enable_des_usr_var_size)
        self.add_tooltip(ui.checkbox_enable_des_usr_var_size, key='des_usr_var_size')
        self.fluid_solver_disabled = False # squelch pylint access-member-before-definition


    def set_solver(self, solver):
        #    Select MFIX Solver:
        # Available selections:
        #  Single phase
        #    Selection disables 'Solids' task pane menu
        #    Selection disables 'Continuum Solids Model' task pane menu
        #    Selection disables 'Discrete Element Model' task pane menu
        #    Selection disables 'Particle-in-Cell' task pane menu
        #  MFIX-TFM
        #    Selection enables 'Solids' task pane menu
        #    Selection enables 'Continuum Solids Model' task pane menu
        #  MFIX-DEM
        #    Selection enables 'Solids' task pane menu
        #    Selection enables 'Discrete Element Model' task pane menu
        #  MFIX-PIC
        #    Selection enables 'Solids' task pane menu
        #    Selection enables 'Particle-in-Cell' task pane menu
        #  MFIX-Hybrid
        #    Selection enables 'Solids' task pane menu
        #    Selection enables 'Continuum Solids Model' task pane menu
        #    Selection enables 'Discrete Element Model' task pane menu

        self.project.solver = solver
        if solver is None: #
            return

        # Issues/440 hide PIC and Hybrid options unless they were already present
        # Must do this first before setting up UI since some items may have been
        #  removed
        if self.project.solver == HYBRID:
            self.enable_hybrid()
        else:
            # Don't disable hybrid once it's been enabled in a project
            #self.disable_hybrid() # this is done in 'reset'
            pass

        ui = self.ui.model_setup
        cb = ui.combobox_solver
        if cb.currentIndex != solver:
            cb.setCurrentIndex(solver)

        solver_name = {SINGLE:"MFiX Single-Phase",
                       TFM:"MFiX-TFM",
                       DEM:"MFiX-DEM",
                       CGP:"MFiX-CGP",
                       PIC:"MFiX-PIC",
                       HYBRID:"MFiX-Hybrid"}.get(solver, "MFiX")
        mesher = self.sms_mode and self.project.get_value('ppo')
        if mesher:
            solver_name = 'SMS Mesher'
        if solver_name != self.solver_name: #sigh #pylint: disable=access-member-before-definition
            self.print_internal("Solver: %s" % solver_name, color='blue')
        self.solver_name = solver_name

        enabled = (solver != SINGLE)
        self.find_navigation_tree_item("Solids").setDisabled(not enabled)
        # issues/583 Prevent fluid solver disabling for pure fluid runs
        ui.checkbox_disable_fluid_solver.setEnabled(enabled)
        if solver == SINGLE:
            self.fluid_solver_disabled = False

        if enabled:
            self.solids_update_tabs()

        # Solids Model selection tied to Solver
        valid_models = (("DEM",) if solver==DEM
                        else ("CGP",) if solver==CGP
                        else ("TFM",) if solver==TFM
                        else ("PIC",) if solver==PIC
                        else ("TFM","DEM"))

        for (i,(k,v)) in enumerate(self.solids.items(), 1):
            model = v.get('model')
            if model not in valid_models:
                model = valid_models[0]
                self.set_solids_model(i, model)

        if solver == PIC:
            self.set_pic_defaults()
        else:
            self.clear_pic_defaults()

        # Issues/848
        resid_string = []
        for i in range(1,9):
            x = self.project.get_value('resid_string', args=[i])
            if x: # Filter "None" values
                resid_string.append(x)
        if solver == SINGLE:  # Remove references to phases above 0
            resid_string = [x for x in resid_string if x.endswith('0')]
        elif solver == TFM: # Add P1, U1 V1 for TFM cases.  Leave DEM and PIC alone.
            for x in 'P1', 'U1', 'V1': # W1?
                if x not in resid_string:
                    resid_string.append(x)
        resid_string = resid_string[:8]
        for (i, x) in enumerate(resid_string, 1):
            self.update_keyword('resid_string', x, args=[i])
        while i < 8:
            i += 1
            self.unset_keyword('resid_string', args=[i])

        if solver in (DEM,CGP):
            # Issues/533
            no_k = self.project.get_value('no_k')
            if no_k:
                self.set_z_max_from_d_p0()
            energy_eq = self.project.get_value('energy_eq', default=True)
            if not energy_eq:
                for i in range(0, 1+len(self.solids)):
                    self.update_keyword('species_eq', False, args=[i])
            # Issues/965
            if energy_eq:
                for i in range(1, 1+len(self.solids)):
                    self.set_keyword_default('des_em', 0.0, args=[i])
                    # Bauer/Schlünder not allowed for DEM/CGP
                    self.set_keyword_default('k_s0', 0.0, args=[i])

        # issues/1005, 1288
        enabled = (solver == TFM)
        cb = ui.combobox_drag_type
        for i,x in enumerate(DRAG_TYPES):
            if x.endswith('_PCF') or x=='HYS':
                get_combobox_item(cb, i).setEnabled(enabled)

        # issues/1119
        if solver not in (PIC, DEM, CGP):
            self.retain_keyword('des_usr_var_size')
            self.unset_keyword('des_usr_var_size')

        # TODO do we need to unset model-specific keys?
        # XXX this should probably be done when we set 'solids_model' keyword, not
        # when we set global solver.  Will matter more when 'Hybrid' is enabled
        if not mesher:
            for i in self.bcs.keys():
                self.bcs_set_default_keys(i)
            for i in self.ics.keys():
                self.ics_set_default_keys(i)
            for i in self.iss.keys():
                self.iss_set_default_keys(i)
            for i in self.pss.keys():
                self.pss_set_default_keys(i)

        #self.update_solids_table() # some of these settings are dependent on solver
        #  but we'll update this when we switch to the solids pane
        #self.setup_combobox_solids_model()

        self.setup_model_setup()

        #self.update_solids_detail_pane()
        self.update_window_title()
        self.update_nav_tree()


    def disable_hybrid(self):
        ui = self.ui.model_setup
        cb = ui.combobox_solver
        if len(cb) == len(SOLVERS):
            cb.removeItem(HYBRID)


    def enable_hybrid(self):
        ui = self.ui.model_setup
        cb = ui.combobox_solver
        # These strings match the .ui files
        if len(cb) != len(SOLVERS):
            cb.addItem("MFiX-Hybrid")


    def setup_model_setup(self, allow_disabled_tab=False):
        ui = self.ui.model_setup
        solver = self.project.solver
        ui.checkbox_disable_fluid_solver.setChecked(self.fluid_solver_disabled)
        self.add_tooltip(ui.checkbox_disable_fluid_solver, key=None, description="""Granular flow in vacuum:
Sets ro_g0 (fluid density) to 0 and disables fluid momentum equations for faster convergence.""")
        self.disable_fluid_solver(self.fluid_solver_disabled)

        # Don't allow setting single-phase once solids are defined
        cb = ui.combobox_solver
        get_combobox_item(cb, SINGLE).setEnabled(not self.solids)

        # Issues/871 don't allow 2D + PIC
        no_k = self.project.get_value('no_k', default=False)
        get_combobox_item(cb, PIC).setEnabled(not no_k)
        get_combobox_item(cb, PIC).setToolTip("PIC model does not support 2D simulation." if no_k
                                              else None)

        key = 'turbulence_model'
        turbulence_model = self.project.get_value(key, default=DEFAULT_TURBULENCE_MODEL)
        if turbulence_model not in TURBULENCE_MODELS:
            self.error("Invalid turbulence model %s" % turbulence_model)
            turbulence_model = DEFAULT_TURBULENCE_MODEL
            self.update_keyword(key, turbulence_model)
        else:
            self.set_turbulence_model(TURBULENCE_MODELS.index(turbulence_model))

        #Specify Gravitational acceleration
        #    Specification always available
        #    Sets keywords GRAVITY_X, GRAVITY_Y, GRAVITY_Z
        #    DEFAULT values
        # handled by keyword widgets

        #  Leave blank when converting dat files to mfx
        #  New cases take the values from the mfix.dat.template
        # Implementation Note:  Not doing this.  There's not a
        # strong reason for it, and we don't distinguish converting dat files from opening existing projects.


        #    If keyword GRAVITY is specified
        #  GRAVITY_Y = -GRAVITY
        #  GRAVITY_X = GRAVITY_Z = 0.0

        #Specify drag model
        # Selection requires TFM, DEM, CGP, or PIC solver
        enabled = self.project.solver in (TFM, DEM, CGP, PIC)

        for item in (ui.label_drag_type, ui.combobox_drag_type):
            item.setEnabled(enabled)

        val = self.project.get_value('drag_type')
        if val is None:
            idx = DRAG_TYPES.index(DEFAULT_DRAG_TYPE)
        else:
            if not val in DRAG_TYPES:
                self.error('Invalid drag_type %s' % val)
                idx = DRAG_TYPES.index(DEFAULT_DRAG_TYPE)
            else:
                idx = DRAG_TYPES.index(val)

        drag_type = DRAG_TYPES[idx]
        # Don't stay on disabled item
        if not get_combobox_item(ui.combobox_drag_type, idx).isEnabled():
            self.warning("Drag type %s not compatible with %s solver, setting to %s" %
                         (drag_type, self.solver_name, DEFAULT_DRAG_TYPE),
                         popup=True)
            drag_type = DEFAULT_DRAG_TYPE
            idx = DRAG_TYPES.index(drag_type)
            self.update_keyword("drag_type", drag_type)
        ui.combobox_drag_type.setCurrentIndex(idx)

        # SYAM_OBRIEN
        #    Specify model parameter: DRAG_C1
        #      DEFAULT 0.8
        #    Specify model parameter: DRAG_D1
        #      DEFAULT 2.65
        enabled = self.project.solver in (TFM, DEM, CGP, PIC) and drag_type == 'SYAM_OBRIEN'
        for item in (ui.label_syam_obrien,
                     ui.label_drag_c1, ui.lineedit_keyword_drag_c1,
                     ui.label_drag_d1, ui.lineedit_keyword_drag_d1):
            item.setEnabled(enabled)
        if enabled:
            for (key, default) in (('drag_c1', 0.8), ('drag_d1', 2.65)):
                val = self.project.get_value(key)
                if val is None:
                    val = default
                    self.update_keyword(key, val)

        # HYS
        #    Specify model parameter LAM_HYS
        #      DEFAULT 1.0e-6 (meters)
        key = 'lam_hys'
        default = 1.0e-6
        enabled =  (drag_type == 'HYS')
        for item in (ui.label_lam_hys, ui.lineedit_keyword_lam_hys,
                     ui.label_lam_hys_units):
            item.setEnabled(enabled)

        if enabled:
            val = self.project.get_value(key)
            if val is None:
                val = default
                self.update_keyword(key, val)

        enabled =  drag_type=='DIFELICE_GANSER'
        for item in (ui.label_difelice_ganser,
                     ui.label_sphericity_dg, ui.lineedit_keyword_sphericity_dg,
                     ui.label_ref_length_dg, ui.lineedit_keyword_ref_length_dg):
            item.setEnabled(enabled)


        # issues/994 J. Musser
        #The e_mrho_gg term is missing from the gas phase for dem model_b formulation.
        # Note:  if solver==hybrid should we allow it?
        item = get_combobox_item(ui.combobox_momentum_formulation, 1)
        if self.project.solver in (DEM,CGP):
            if not hasattr(item, 'tooltip_0'):
                item.tooltip_0 = item.toolTip()
            if 'Not available' not in item.toolTip():
                item.setToolTip(item.tooltip_0 + '<br>Not available for %s model.' %
                                ('DEM' if self.project.solver=='DEM' else 'CGP'))
            item.setEnabled(False)
            if self.project.get_value('model_b'):
                self.unset_keyword('model_b')
        else:
            item.setEnabled(True)
             # tooltip_0 attr does not persist on cb item!
            self.add_tooltip(item, key='model_b', value=True)

        #Specify momentum equation formulation; Select Model A, Model B; Jackson, Ishii
        #MODEL_B
        #[ .FALSE. ] -------------------------- Use Model A
        #.TRUE. ----------------------------- Use Model B.
        model_b = self.project.get_value('model_b', default=False)

        #ISHII
        #Flag to enable Ishii form of momentum equations.         two-phase flow.
        #.TRUE. ----------------------------- Solve Ishii form of momentum equations.
        # [ .FALSE. ]
        ishii = self.project.get_value('ishii', default=False)

        #JACKSON
        #LOGICAL
        #Flag to enable Jackson form of momentum equations.

        #.TRUE. ----------------------------- Solve Jackson form of momentum equations.
        #[ .FALSE. ] -------------------------- Default
        jackson = self.project.get_value('jackson', default=False)

        if model_b + ishii + jackson > 1:
            self.error("Invalid momentum equations: more than one of model_b, ishii and jackson specified")
        cb = ui.combobox_momentum_formulation
        idx = 0 # model A
        if model_b:
            idx = 1
        elif jackson:
            idx = 2
        elif ishii:
            idx = 3
        cb.setCurrentIndex(idx)
        # set combobox
        cb.setToolTip(get_combobox_item(cb, idx).toolTip())

        #Select sub-grid model:
        # Selection requirements:
        #  Only available with MFIX-TFM solver
        #  DRAG_TYPE="WEN_YU"
        #  KT_TYPE="ALGEBRAIC"
        #  TURBULENCE_MODEL /= K_EPSILON
        #  BLENDING_FUNCTION = NONE
        #  FRICTION_MODEL /= SRIVASTAVA
        #  (There are more restrictions…)
        key = 'subgrid_type'
        cb = ui.combobox_subgrid_type
        drag_type = self.project.get_value('drag_type', default=DEFAULT_DRAG_TYPE)
        kt_type = self.project.get_value('kt_type', default=DEFAULT_KT_TYPE)
        turbulence_model = self.project.get_value('turbulence_model', default=DEFAULT_TURBULENCE_MODEL)
        blending_function = self.project.get_value('blending_function', default='NONE')
        friction_model = self.project.get_value('friction_model', default=DEFAULT_FRICTION_MODEL)

        enabled = (solver == TFM
                   and drag_type == 'WEN_YU'
                   and kt_type == 'ALGEBRAIC'
                   and turbulence_model != 'K_EPSILON'
                   and blending_function == 'NONE'
                   and friction_model != 'SRIVASTAVA')
        cb = ui.combobox_subgrid_type
        for item in (ui.label_subgrid_type, cb,
                     ui.label_filter_size_ratio, ui.lineedit_keyword_filter_size_ratio,
                     ui.checkbox_keyword_subgrid_wall):
            item.setEnabled(enabled)
        if not enabled:
            self.unset_keyword(key)

        value = self.project.get_value(key, default=DEFAULT_SUBGRID_TYPE)
        if value not in SUBGRID_TYPES:
            self.error("Invalid subgrid_type %s" % value)
            value = DEFAULT_SUBGRID_TYPE
        idx = SUBGRID_TYPES.index(value)
        cb.setCurrentIndex(idx)
        cb.setToolTip(get_combobox_item(cb, idx).toolTip())

        #Specify sub-grid model filter size ratio:
        # Specification requires SUBGRID_TYPE =/ NONE
        # Sets keyword FILTER_SIZE_RATIO
        # DEFAULT 2.0 # handled by widget.default
        #Enable sub-grid wall correction model:
        # Specification requires SUBGRID_TYPE =/ NONE
        # Sets keyword SUBGRID_WALL
        # DEFAULT FALSE # handled by widget.default
        enabled = self.project.get_value('subgrid_type') not in (None, 'NONE')
        for item in (ui.label_filter_size_ratio, ui.lineedit_keyword_filter_size_ratio,
                     ui.checkbox_keyword_subgrid_wall):
            item.setEnabled(enabled)

        # Set des_usr_var widgets to correct state
        key = 'des_usr_var_size'
        des_usr_var_size = self.project.get_value(key, default=None)
        cb = ui.checkbox_enable_des_usr_var_size
        le = ui.lineedit_keyword_des_usr_var_size
        if self.project.solver in (DEM,CGP,PIC) or (des_usr_var_size is not None):
            self.add_tooltip(cb, key=key)
            self.add_tooltip(le, key=key)
            enabled = (des_usr_var_size is not None)
            cb.setEnabled(True)
            cb.setChecked(enabled)
            le.setEnabled(enabled)
            if not enabled:
                le.setText('')
        else:
            cb.setEnabled(False)
            cb.setChecked(False)
            le.setEnabled(False)
            le.setText('')
            reason = ' Available for DEM/CGP/PIC models only'
            for w in (cb, le):
                self.add_tooltip(w, key=key)
                w.setToolTip(w.toolTip() + reason)


    def set_subgrid_type(self, idx):
        # Sets keyword SUBGRID_TYPE
        # Available selections
        #  NONE (DEFAULT)
        #  IGCI
        #  MILIOLI
        # Force sets keyword PHIP = 0.0 when SUBGRID_TYPE /= NONE
        ui = self.ui.model_setup
        cb = ui.combobox_subgrid_type
        self.update_keyword('subgrid_type', SUBGRID_TYPES[idx])
        if idx > 0:
            self.update_keyword('phip', 0.0) # Issues/585
        cb.setToolTip(get_combobox_item(cb, idx).toolTip())
        self.setup_model_setup()


    def reset_model_setup(self):
        self.update_nav_tree()

    def enable_des_usr_var_size(self, val):
        #Enable user scalar tracking
        #Selection always available
        #Does not directly set any keywords
        #Enables specification of number of user scalars
        # Sets keyword DES_USR_VAR_SIZE
        ui = self.ui.model_setup
        le = ui.lineedit_keyword_des_usr_var_size
        key = 'des_usr_var_size'
        if not val:
            self.retain_keyword(key)
            self.unset_keyword(key)
            le.setText('')
        else:
            val = self.get_retained_keyword(key, default=0)
            self.update_keyword(key, val)
        self.setup_model_setup()


    def disable_fluid_solver(self, disabled):
        # Option to disable the fluid phase
        # Disables the Fluid task pane menu
        # Sets keyword RO_G0 to 0.0
        m = self.ui.model_setup
        enabled = not disabled
        item = self.find_navigation_tree_item("Fluid")
        item.setDisabled(disabled)
        m.combobox_turbulence_model.setEnabled(enabled)
        self.update_nav_tree()
        # TODO update nscalar (?)

        if self.fluid_solver_disabled:
            self.ui.fluid.lineedit_keyword_ro_g0.minimum = 0
            self.update_keyword('ro_g0', 0)
            self.ui.fluid.lineedit_keyword_ro_g0.required = (self.fluid_density_model == CONSTANT)

        # This hack avoids clobbering ro_g0 during project load (review this!)
        if self.fluid_solver_disabled == disabled:
            return

        self.fluid_solver_disabled = disabled

        if disabled:
            self.unset_keyword('turbulence_model')
            self.retain_keyword('ro_g0')
            self.update_keyword('ro_g0', 0) # issues/124
        else:
            val = self.retained_keys.get('ro_g0', default_values.ro_g0) if self.fluid_density_model == CONSTANT else None
            self.update_keyword('ro_g0', val)
            if val is None:
                self.ui.fluid.lineedit_keyword_ro_g0.setText('')

        # Disable fluid momentum equations if disabling fluid phase,
        #  else restore momentum equations to default (enabled) - issues/691
        for c in 'xyz':
            key = 'momentum_%s_eq' % c
            self.update_keyword(key, bool(enabled), args=[0])
        if disabled:
            self.delete_scalars_of_phase(0)

        # Other panes are enabled/disabled based on availability of fluid solver
        self.update_nav_tree()


    def enable_energy_eq(self, enabled):
        #    Option to enable thermal energy equations
        # This keyword should always be specified in the input deck
        # Sets keyword ENERGY_EQ
        # DEFAULT .FALSE.
        # Note, the mfix default is True.  We initialize to False in
        # the new project template (mfix.dat.template)

        # This is an additional callback on top of automatic keyword update,
        # since this has to change availability of several other GUI items
        ui = self.ui.model_setup
        ui.checkbox_keyword_energy_eq.setChecked(enabled)

        # No species eq without energy_eq for DEM (issues/533)
        if self.project.solver in (DEM,CGP) and not enabled:
            for i in range(0, 1+len(self.solids)):
                self.update_keyword('species_eq', False, args=[i])

        # Many per-model settings depend on ENERGY_EQ.  Resetting all
        #  the per-phase solids model is an easy way to make sure
        #  all dependent keys are handed
        for i in range(1, 1+len(self.solids)):
            self.set_solids_model(i,
                self.project.get_value('solids_model', default='TFM', args=[i]))


    def set_turbulence_model(self, val):
        # Available selections:
        #  None; [DEFAULT]
        #  Mixing Length:
        #    Selection always available
        #    Sets keyword TURBULENCE_MODEL to MIXING_LENGTH
        #    Requires IC_L_SCALE for all IC regions
        #  K-Epsilon
        #    Selection always available
        #    Sets keyword TURBULENCE_MODEL to K_EPSILON
        #    Requires IC_K_TURB_G for all IC regions TODO
        #    Requires IC_E_TURB_G for all IC regions TODO
        #    Requires BC_K_TURB_G for inflow (MI and PI) BC regions TODO
        #    Requires BC_E_TURB_G for inflow (MI and PI) BC regions TODO
        ui = self.ui.model_setup
        cb = ui.combobox_turbulence_model
        cb.key = 'turbulence_model'
        if cb.currentIndex() != val:
            cb.setCurrentIndex(val)
        turbulence_model = TURBULENCE_MODELS[val]
        # Avoid resetting turbulence_model from None to 'NONE' at startup
        if turbulence_model != self.project.get_value('turbulence_model',
                                                      default=DEFAULT_TURBULENCE_MODEL):
            self.update_keyword('turbulence_model', turbulence_model)
        # Set combobox tooltip to match item value
        cb.setToolTip(get_combobox_item(cb, val).toolTip())

        enabled = (turbulence_model != 'NONE')
        #Specify maximum fluid viscosity
        # Selection available if TURBULENCE_MODEL =/ 'NONE'
        # Sets keyword MU_GMAX
        # DEFAULT 1.0e3 (Pa.s)
        key = 'mu_gmax'
        for item in (ui.label_mu_gmax, ui.lineedit_keyword_mu_gmax, ui.label_mu_gmax_units):
            item.setEnabled(enabled)
        if enabled:
            if self.project.get_value(key):
                pass
            else:
                val = self.get_retained_keyword(key, default=default_values.mu_gmax)
                self.update_keyword(key, val)
        else:
            self.retain_keyword(key)
            self.unset_keyword(key)
            ui.lineedit_keyword_mu_gmax.clear()

    def set_drag_type(self, idx):
        #Specify drag model
        # Selection requires TFM, DEM, CGP, or PIC solver
        # Sets keyword DRAG_TYPE
        if self.project.solver not in (TFM, DEM, CGP, PIC):
            # Shouldn't be here
            return

        self.update_keyword('drag_type', DRAG_TYPES[idx])
        self.setup_model_setup()


    def set_momentum_formulation(self, idx):
        #Specify momentum equation formulation; Select Model A, Model B; Jackson, Ishii
        vals = [None, None, None]
        if idx > 0:
            vals[idx-1] = True
        for (key, val) in zip(('model_b', 'jackson', 'ishii'), vals):
            if val is None:
                self.unset_keyword(key)
            else:
                self.update_keyword(key, val)
        self.setup_model_setup()

    #Specify heat transfer correlation (requires TFM, DEM, CGP or PIC solver)
    #*This option may be premature as MFIX is limited in heat HTCs.
    # NOT IMPLEMENTED
