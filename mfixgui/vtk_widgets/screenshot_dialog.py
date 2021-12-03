"""
This is screenshot dialog.
"""
import os
from qtpy import QtCore, QtWidgets
from mfixgui.tools.qt import get_ui, SETTINGS, get_icon


class ScreenshotDialog(QtWidgets.QDialog):
    applyEvent = QtCore.Signal(object, object, object)

    def __init__(self, parent=None):
        QtWidgets.QDialog.__init__(self, parent)
        self.gui = parent.gui

        ui = self.ui = get_ui('screenshot_dialog.ui', self)

        # override sizeHint
        self.ui.sizeHint = self.size_hint

        self.setWindowTitle('Save Image')

        ui.lineedit_width.dtype = int
        ui.lineedit_height.dtype = int

        ui.toolbutton_browse.setIcon(get_icon('folder.svg'))
        ui.toolbutton_browse.clicked.connect(self.browse)
        ui.combobox_template.currentIndexChanged.connect(self.change_size)

        # hide resolution widgets
        if not int(SETTINGS.value('enable_screenshot_res', 0)):
            for wid in [ui.label_template, ui.combobox_template,
                        ui.label_width, ui.lineedit_width,
                        ui.label_height, ui.lineedit_height]:
                wid.setVisible(False)

        ui.adjustSize()

        ui.lineedit_filename.setFocus()

    def size_hint(self):
        size = QtCore.QSize(400, 100)
        return size

    def change_size(self, index=None):

        text = self.ui.combobox_template.currentText()

        w, h = None, None
        if '540p' in text:
            w, h = 960, 540
        elif '720p' in text:
            w, h = 1080, 720
        elif '1080p' in text:
            w, h = 1920, 1080
        elif '4K' in text:
            w, h = 3840, 2160

        if w is not None:
            self.ui.lineedit_width.updateValue('none', w)
            self.ui.lineedit_height.updateValue('none', h)

    def get(self):
        ui = self.ui

        if not ui.lineedit_path.text():
            ui.lineedit_path.setText(self.gui.get_project_dir())
            self.change_size()

        ret = self.exec_()

        fname = os.path.join(
            ui.lineedit_path.text(),
            ui.lineedit_filename.text() + ui.combobox_ext.currentText())

        size = (ui.lineedit_width.value, ui.lineedit_height.value)
        ok = (ret == QtWidgets.QDialog.Accepted)
        trans = ui.checkBox_trans.isChecked()
        return ok, fname, size, trans

    def browse(self):
        filename = QtWidgets.QFileDialog.getExistingDirectory(
            self,
            "Save screenshot",
            self.ui.lineedit_path.text())
        if isinstance(filename, (tuple, list)):
            filename = filename[0]
        if not filename:
            return
        self.ui.lineedit_path.setText(filename)
