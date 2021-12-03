import copy
import math

from qtpy import QtWidgets
from mfixgui.tools import get_unique_string
from mfixgui.tools.qt import get_pixmap, widget_iter, get_ui
from mfixgui.vtk_widgets.constants import *


class HopperWizard(QtWidgets.QDialog):
    def __init__(self, parent):
        QtWidgets.QDialog.__init__(self, parent)

        self.vtk_widget = parent

        ui = self.ui = get_ui('hopper.ui', self)

        self.setWindowTitle('Hopper Wizard')

        ui.pushbutton_close.clicked.connect(self.close)
        pixmap = get_pixmap('hopper_sketch.png', 118, 288)
        ui.label_image.setPixmap(pixmap)
        ui.lineedit_dh.setFocus()

        ui.pushbutton_apply.clicked.connect(self.apply_)

        for widget in widget_iter(self.ui):
            name = str(widget.objectName()).split('_')
            if 'lineedit' in name:
                widget.dtype = float
                #widget.allow_parameters = True

    def popup(self):

        self.show()
        self.raise_()
        self.activateWindow()

    def apply_(self):

        bool_list = []
        v = {}
        for widget in widget_iter(self.ui):
            name = str(widget.objectName()).split('_')
            if 'lineedit' in name:
                v[name[1]] = widget.value

        # create hopper
        h = copy.deepcopy(DEFAULT_IMPLICIT_PARAMS)
        h['radius'] = v['dh']/2.0
        h['height'] = v['hh']
        h['type'] = 'cylinder'
        h_name = self.vtk_widget.add_implicit(
            name=get_unique_string('hopper', self.vtk_widget.geometrydict.keys()),
            data=h)
        bool_list.append(h_name)

        # create cone
        c = copy.deepcopy(DEFAULT_IMPLICIT_PARAMS)
        c['radius'] = v['dh']/2.0
        tan = math.tan(math.radians(v['ah']))
        height = v['dh']/2.0 * tan
        add_h = v['do']/2.0 * tan
        c['height'] = height + add_h
        c['type'] = 'cone'
        c['rotationz'] = -90
        c['centery'] = -v['hh']/2.0 - height/2.0 - add_h/2.0
        c_name = self.vtk_widget.add_implicit(
            name=get_unique_string('cone', self.vtk_widget.geometrydict.keys()),
            data=c)
        bool_list.append(c_name)

        # outlet
        o = copy.deepcopy(DEFAULT_IMPLICIT_PARAMS)
        o['radius'] = v['do']/2.0
        o['height'] = v['ho']
        o['type'] = 'cylinder'
        o['centery'] = -v['hh']/2.0 - height - v['ho']/2.0 + add_h/2.0
        o_name = self.vtk_widget.add_implicit(
            name=get_unique_string('outlet', self.vtk_widget.geometrydict.keys()),
            data=o)
        bool_list.append(o_name)

        # outlet
        if self.ui.groupbox_collector.isChecked():
            c = copy.deepcopy(DEFAULT_IMPLICIT_PARAMS)
            c['radius'] = v['dc']/2.0
            c['height'] = v['hc']
            c['type'] = 'cylinder'
            c['centery'] = -v['hh']/2.0 - height - v['ho'] - v['hc']/2.0 + add_h/2.0 + v['hc']/100.0
            c_name = self.vtk_widget.add_implicit(
                name=get_unique_string('outlet', self.vtk_widget.geometrydict.keys()),
                data=c)
            bool_list.append(c_name)

        union = self.vtk_widget.boolean_operation(booltype='union', children=bool_list)

        # sample implicit
        union = self.vtk_widget.add_filter(
            'sample_implicit',
            name=get_unique_string('sample_implicit', self.vtk_widget.geometrydict.keys()),
            child=union)

        self.close()
