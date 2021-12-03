from PyQt5.QtWidgets import QSplitter, QStyleFactory, QWidget, QColorDialog
from PyQt5.QtGui import QColor

from mfixgui.animations import animation_speed
from mfixgui.tools.qt import get_ui, SETTINGS, widget_iter

class SettingsWidget(QWidget):
    def __init__(self, gui):
        """ Create widget for Settings filemenu pane """
        super(SettingsWidget, self).__init__()
        self._init_done = False
        get_ui("settings.ui", widget=self)
        self.gui = gui


        self.checkbox_developer_mode.clicked.connect(gui.enable_developer_mode)
        self.checkbox_collapse_splitters.clicked.connect(self.collapse_splitters)
        self.checkbox_show_resources.clicked.connect(gui.resource_monitor.show)
        self.groupbox_animation_speed.clicked.connect(lambda n: self.spinbox_animation_speed.setValue(200 if n else 0))
        self.spinbox_animation_speed.valueChanged.connect(lambda n: SETTINGS.setValue('animation_speed', n))
        self.checkbox_screenshot_res.clicked.connect(lambda state: SETTINGS.setValue('enable_screenshot_res', int(state)))
        self.combobox_output_template.currentIndexChanged.connect(self.set_template)
        self.combobox_sms.activated.connect(gui.set_sms_enabled)
        self.toolButton_vtk_color1.clicked.connect(lambda:self.change_color(self.toolButton_vtk_color1, 1))
        self.toolButton_vtk_color2.clicked.connect(lambda:self.change_color(self.toolButton_vtk_color2, 2))

        # GUI style
        current_style = gui.style
        avail_styles = [s.lower() for s in QStyleFactory.keys()]
        if current_style and current_style not in avail_styles:
            gui.error("Application style %s not available" % current_style, popup=True)
            avail_styles.append(current_style) # ?

        cb = self.combobox_appstyle
        cb.clear()
        cb.addItems(avail_styles)
        cb.currentIndexChanged.connect(lambda n, cb=cb: SETTINGS.setValue('app_style', cb.currentText()))

        self.pushbutton_reset.clicked.connect(self.reset_settings)
        self.update()
        self._init_done = True


    def reset_settings(self):
        ok = self.gui.message(text='This will reset all MFiX global settings\n'
                                   'to their original default values.\n'
                                   'Are you sure?',
                              buttons=['ok','cancel'],
                              default='cancel')
        if ok == 'ok':
            self.gui.print_internal("Settings cleared.", color='blue')
            SETTINGS.clear()
            SETTINGS.sync()
            self.update()
            self.gui.set_sms_enabled(False) # Default

    def set_template(self):
        template_name = self.combobox_output_template.currentText()
        SETTINGS.setValue('template', template_name)
        self.gui.set_unsaved_flag() # Allow re-save with new template
        if template_name == 'Custom' and self._init_done:
            self.gui.warning("Custom template must be supplied by user.  See MFiX documentation.",
                             popup=True)

    def update(self):
        self.checkbox_developer_mode.setChecked(
            int(SETTINGS.value('developer_mode', 0)))
        self.checkbox_collapse_splitters.setChecked(
            int(SETTINGS.value('collapse_qsplitter', 0)))
        self.checkbox_show_resources.setChecked(
            int(SETTINGS.value('show_resources', 0)))
        speed = animation_speed()
        self.groupbox_animation_speed.setChecked(speed > 0)
        self.spinbox_animation_speed.setValue(speed)
        self.checkbox_screenshot_res.setChecked(
            int(SETTINGS.value('enable_screenshot_res', 0)))

        # MFX file template
        current_template = SETTINGS.value('template', 'Standard')
        cb = self.combobox_output_template
        cb.setCurrentText(current_template)

        # SMS mode
        sms = SETTINGS.value('SMS', 0)
        if sms in ('False', 'false'):
            sms = 0
        elif sms in ('True', 'true'):
            sms = 1
        sms = int(sms)
        self.combobox_sms.setCurrentIndex(1 if sms else 0)
        self.combobox_appstyle.setCurrentText(self.gui.style)

        # VTK colors
        self.set_btn_color(QColor(SETTINGS.value('vtk_bck_color1', '#666666')),
                           self.toolButton_vtk_color1)
        self.set_btn_color(QColor(SETTINGS.value('vtk_bck_color2', '#ffffff')),
                           self.toolButton_vtk_color2)

    def collapse_splitters(self, enable):
        for widget in widget_iter(self.gui):
            if isinstance(widget, QSplitter):
                widget.setChildrenCollapsible(enable)

        SETTINGS.setValue("collapse_qsplitter", int(enable))

    def change_color(self, btn, ncolor=1):
        key = f"vtk_bck_color{ncolor}"
        original = QColor(SETTINGS.value(key))
        color = QColorDialog.getColor(
            original, parent=self, title='Select background color')

        if color.isValid():
            self.set_btn_color(color, btn)
            SETTINGS.setValue(key, color.name())

            c1 = QColor(SETTINGS.value('vtk_bck_color1', '#666666'))
            c2 = QColor(SETTINGS.value('vtk_bck_color2', '#ffffff'))
            #push update
            for tab in self.gui.graphics_tabs:
                if tab.vtk:
                    tab.vtk_widget.set_background_color(c1.getRgbF()[:3], c2.getRgbF()[:3])
            self.gui.vtkwidget.set_background_color(c1.getRgbF()[:3], c2.getRgbF()[:3])

    def set_btn_color(self, color, btn):
        rgb = [int(255 * c) for c in color.getRgbF()[:3]]
        btn.setStyleSheet(
            "QToolButton{{ background: rgb({},{},{});}}".format(
                *rgb))
