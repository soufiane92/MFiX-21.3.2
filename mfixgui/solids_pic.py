# Methods to deal with solids pic tab, split off from solids_handler.py

# Particle in Cell Model Task Pane Window

from .constants import *

PIC_defaults = ( # key, val, min, max, exclude_min, exclude_max
    ('ep_star', 0.42, 0.0, 0.1, True, True),                 # (0.0, 1.0)
    ('fric_exp_pic', 3.0, 1.0, 5.0, False, False),           # [1.0, 5.0]
    ('psfac_fric_pic', 100.0, 1.0, 10000.0, False, False),   # [1.0, 10000.0]
    ('mppic_coeff_en1', 0.85, 0.0, 1.0, True, False),        # (0.0, 1.0]
    ('fric_non_sing_fac', 1.0e-7, 0.0, 1.0e-4, True, False), # (0.0, 1.0e-4]
    ('mppic_coeff_en_wall', 0.85, 0.0, 1.0, True, False),    # (0.0, 1.0]
    ('mppic_coeff_et_wall', 1.0, 0.0, 1.0, True, False),     # (0.0, 1.0]
    ('mppic_velfac_coeff', 1.0, 0.0, 1.0, False, False))     # [0.0, 1.0]

class SolidsPIC(object):

    def init_solids_pic(self):
        ui = self.ui.solids

        # Set up ranges for PIC variables.
        for (key, default, min, max, exclude_min, exclude_max) in PIC_defaults:
            le = getattr(ui, 'lineedit_keyword_%s'%key)
            le.min = min
            le.max = max
            le.exclude_min = exclude_min
            le.exclude_max = exclude_max

        # EP_STAR is REQUIRED
        le = ui.lineedit_keyword_ep_star
        default = 0.42
        le.required = True
        le.saved_value = default
        le.initpython = default

        le = ui.lineedit_keyword_pic_cfl
        le.post_update = self.setup_pic_tab
        cb = ui.combobox_pic_cfl_control
        key = 'pic_cfl_control'
        cb.activated.connect(lambda idx:
                             self.update_keyword('pic_cfl_control',
                                                 CFL_CONTROL_TYPES[idx]))
        self.add_tooltip(cb, key=key)

    def setup_pic_tab(self):
        ui = self.ui.solids
        enable = ui.input_enabled and ui.pushbutton_solids_PIC.isEnabled()
        ui.PIC.setEnabled(enable)
        key = 'pic_cfl_control'
        val = self.project.get_value(key, default='MAX')
        if val not in CFL_CONTROL_TYPES:
            self.error("Invalid value for key %s:  '%s', setting to MAX" %
                       key, val, popup=True)
            val = 'MAX'
            self.update_keyword(key, val)
        cb = ui.combobox_pic_cfl_control
        cb.setCurrentIndex(CFL_CONTROL_TYPES.index(val))
        pic_cfl = self.project.get_value('pic_cfl')
        for w in (cb, ui.label_pic_cfl_parcel_fraction,
                  ui.label_pic_cfl_control,
                  ui.lineedit_keyword_pic_cfl_parcel_fraction):
            w.setEnabled(pic_cfl is not None and enable)



    def set_pic_defaults(self):
        for (key, default, *rest) in PIC_defaults:
            self.set_keyword_default(key, default)

        #-  Selection enables ‘Solids’ task pane menu
        #-  Selection enables ‘Particle-in-Cell’ task pane menu
        #-  Sets keyword DES_INTERP_ON=.TRUE.
        #-  Sets keyword DES_INTERP_MEAN_FIELDS=.TRUE.
        #-  Sets keyword DES_ONEWAY_COUPLED=.FALSE.
        #-  Sets keyword DES_INTERP_SCHEME=’LINEAR_HAT’
        #-  Sets keyword GENER_PART_CONFIG = .TRUE.

        self.update_keyword('des_interp_on', True)
        self.update_keyword('des_interp_mean_fields', True)
        #self.update_keyword('des_oneway_coupled', False)
        self.unset_keyword('des_oneway_coupled') # False is default
        self.update_keyword('des_interp_scheme', 'LINEAR_HAT')
        self.update_keyword('gener_part_config', True)
        self.unset_keyword('particles') # particles and gener_part_config cannot both be set

    def clear_pic_defaults(self):
        for (key, default, *rest) in PIC_defaults:
            if key != 'ep_star': # EP_STAR is not PIC_specific
                self.unset_keyword(key)

        #self.unset_keyword('gener_part_config') # will clobber keyword if model == DEM
        #self.unset_keyword('des_interp_on')
        #self.unset_keyword('des_interp_mean_fields')
        #self.unset_keyword('des_oneway_coupled')

        #self.unset_keyword('des_interp_scheme')
        key = 'des_interp_scheme'
        if self.project.get_value(key) == 'LINEAR_HAT':
            # This setting is required for PIC but not available for other solvers
            self.unset_keyword(key)
