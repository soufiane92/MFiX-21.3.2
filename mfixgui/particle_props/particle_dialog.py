import json
import os
import sys
import signal

from qtpy.QtWidgets import (QApplication, QDialog, QHeaderView,
                            QLineEdit, QTableWidgetItem)
from qtpy.QtCore import Signal
from mfixgui.tools.qt import (get_combobox_item, get_selected_row,
                              get_selected_rows, set_item_noedit, get_ui)
from mfixgui import default_values


SCRIPT_DIRECTORY = os.path.abspath(os.path.dirname(__file__))
PARTICLE_DATABASE = os.path.join(SCRIPT_DIRECTORY, 'particle_props.json')


class ParticlePopup(QDialog):

    add = Signal(str, float, float, float)
    cancel = Signal()

    def __init__(self, parent):
        QDialog.__init__(self, parent)
        ui = self.ui = get_ui('particle_dialog.ui', self)

        # read database
        self.db = {}
        if os.path.exists(PARTICLE_DATABASE):
            with open(PARTICLE_DATABASE, encoding="utf-8") as jsonfile:
                self.db = json.load(jsonfile)

        # setup table widget
        tw = self.tablewidget_particles
        tw.dtype = dict
        tw.setSortingEnabled(True)
        tw.set_selection_model()
        tw.set_columns(['id', 'name', 'smd (m)', 'density (kg/m3)', 'umf (m/s)', 'group'])
        tw.show_vertical_header(False)
        tw.auto_update_rows(True)
        tw.set_value(self.db)
        tw.fit_to_contents()
        tw.new_selection.connect(self.selected)

        # connect search
        ui.lineedit_search.textChanged.connect(self.search)

        self.pushbutton_import.setEnabled(False)

        buttons = (ui.pushbutton_import, ui.pushbutton_close)
        buttons[0].clicked.connect(self.emit_add)
        buttons[1].clicked.connect(lambda: (self.cancel.emit(), self.close()))

    def emit_add(self):
        tw = self.tablewidget_particles
        rows = tw.current_rows()
        if not rows:
            return

        p = list(self.db.values())[rows[0]]

        self.add.emit(p['name'],
                      p['smd (m)'] if p['smd (m)'] is not None else default_values.d_p0,
                      p['density (kg/m3)'] if p['density (kg/m3)'] is not None else default_values.ro_s0,
                      p['umf (m/s)'] if p['umf (m/s)'] is not None else 0.0)
        self.close()

    def popup(self):
        self.show()
        self.raise_()
        self.activateWindow()
        self.tablewidget_particles.set_value(self.db)

    def search(self, text):

        # look for number
        try:
            id = int(text)
        except ValueError:
            id = None

        vtext = len(text) > 0

        tw = self.tablewidget_particles
        tw.resetSort()
        tw.clear_selection()
        self.pushbutton_import.setEnabled(False)
        for i, p in enumerate(self.db.values()):
            hide = False
            if id is not None:
                hide = id != int(p['id'].split('-')[-1])
            elif vtext:
                hide = text.lower() not in p['name'].lower()
            tw.setRowHidden(i, hide)

    def selected(self):
        self.pushbutton_import.setEnabled(True)



def main():
    args = sys.argv
    qapp = QApplication(args)

    def print_add(name, smd, density, umf):
        print(name, smd, density, umf)

    species_popup = ParticlePopup(None)
    species_popup.add.connect(print_add)
    species_popup.popup()
    # exit with Ctrl-C at the terminal
    signal.signal(signal.SIGINT, signal.SIG_DFL)

    qapp.exec_()
    qapp.deleteLater()

    sys.exit()

if __name__ == '__main__':
    main()
