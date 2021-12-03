# Methods for geometry task pane window

from mfixgui.constants import *
from mfixgui.tools.qt import widget_iter
from mfixgui.widgets.base import BaseWidget

class GeometryHandler(object):
    def init_geometry(self):
        ui = self.ui.geometry
        ui.checkbox_keyword_no_k.post_update = self.handle_no_k
        for w in widget_iter(ui):
            if isinstance(w, BaseWidget):
                w.post_update = self.clear_mesh_accepted

    def enable_z_input(self, enable):
        ui = self.ui.geometry
        for w in (ui.lineedit_keyword_z_min,
                  ui.lineedit_keyword_z_max,
                  ui.label_Z):
            w.setEnabled(enable)

    def reset_geometry(self):
        self.enable_z_input(True)

    def setup_geometry(self, allow_disabled_tab=False):
        ui = self.ui.geometry
        no_k = self.project.get_value('no_k', default=False)
        # Issues/871
        if self.project.solver == PIC:
            if no_k:
                self.error("PIC model does not support 2D simulation",
                           popup=True)
                self.unset_keyword('no_k')
            ui.checkbox_keyword_no_k.setEnabled(False)
            ui.checkbox_keyword_no_k.setChecked(False)
            ui.checkbox_keyword_no_k.setToolTip("PIC model does not support 2D simulation")
            self.enable_z_input(True)
        else:
            ui.checkbox_keyword_no_k.setEnabled(True)
            ui.checkbox_keyword_no_k.setChecked(no_k)
            self.add_tooltip(ui.checkbox_keyword_no_k, key='no_k')
            disable = no_k and self.project.solver in (DEM,CGP)
            self.enable_z_input(not disable)


    def set_z_max_from_d_p0(self):
        mmax = self.project.get_value('mmax', default=0)
        d =[self.project.get_value('d_p0', default=0, args=[i])
            for i in range(1, 1+mmax)]
        if d:
            self.update_keyword('z_min', 0)
            self.update_keyword('z_max', max(d))


    def handle_no_k(self):
        self.clear_mesh_accepted()
        no_k = self.project.get_value('no_k', default=False)
        if no_k and self.project.solver in (DEM,CGP):
            self.enable_z_input(False)
            self.set_z_max_from_d_p0()
        else:
            self.enable_z_input(True)
