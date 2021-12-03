#!/usr/bin/env python
"""Species selector dialog for MFIX GUI, includes stand-alone test"""

# 2016-11-20  Species/alias unification
#  we will only expose 'alias' to the user.  'species' is only used
#  as a key into Burcat/THERMO_DATA, and we're going to inline all
#  of the thermodynamic data - cgw

# 2021-07-09 TODO use the Lineedit widget from mfix to do input
# validation

import os
import sys
import signal
from collections import OrderedDict
import pickle
from copy import deepcopy

from qtpy.QtWidgets import (QApplication, QDialog, QHeaderView,
                            QLineEdit, QTableWidgetItem)

from qtpy.QtGui import  QDoubleValidator, QValidator
from qtpy.QtCore import Signal

from mfixgui.tools.qt import (get_combobox_item, get_selected_row,
                              get_selected_rows, set_item_noedit, get_ui)
from mfixgui.tools.util import safe_float

try:
    from matplotlib import pyplot as plt
    import numpy as np
except ImportError:
    plt = None
    np = None

def resize_column(tw, col, flags):
    tw.horizontalHeader().setSectionResizeMode(col, flags)

# Make search case-, whitespace- and punctuation-insensitive
squash_table = [c.lower() if c.isalnum() else None for c in map(chr,range(256))]
def squash(string):
    r = string.translate(squash_table)
    return r

phase_names = 'Gas', 'Liquid', 'Solid', 'Composite'

class SpeciesPopup(QDialog):

    save = Signal()
    cancel = Signal()

    def load_burcat(self, path):
        if not os.path.exists(path):
            print("%s not found, create it by running read_burcat.py" % path)
            sys.exit(-1)
        with open(path, 'rb') as db_file:
            database = pickle.load(db_file)
        by_phase = {}

        for k, v in database.items():
            phase = k[1]
            if phase not in by_phase:
                by_phase[phase] = {}
            key = k[0], k[2], k[3]
            by_phase[phase][key] = v
        self.db = by_phase

        # build search list, lowercased
        self.haystack = []
        self.comments = {}
        for phase in 'GLSC':
            h_tmp = [((squash(k[0]), squash(v[2])), k, phase) for (k,v) in self.db[phase].items()]
            h_tmp.sort()
            self.haystack.extend(h_tmp)
            # comment fields
            self.comments[phase] = dict((k, v[2])
                                        for (k,v) in self.db[phase].items())


    def do_search(self, string):
        lineedit = self.ui.lineedit_search
        string = string.lstrip() # Don't allow leading spaces
        if string != lineedit.text():
            lineedit.setText(string)
            return

        self.ui.tablewidget_search.clearContents()
        results = []
        match_empty = True # Show all possibilities when no search string
        if match_empty or string:
            needle = squash(string)
            for (k, key, phase) in self.haystack:
                if (phase in self.phases and
                    (needle in k[0] or
                     (self.include_comments and needle in k[1]))):
                    results.append((key, phase))
        # Put exact matches & leading-substring matches first
        if string:
            results.sort(
                key=lambda x:
                (1 - x[0][0].lower().startswith(string.lower()), x))

        tw = self.ui.tablewidget_search
        nrows = len(results)
        self.ui.tablewidget_search.clearContents()
        self.ui.tablewidget_search.setRowCount(nrows)
        self.search_results = [None]*nrows

        # http://stackoverflow.com/questions/10192579/
        tw.model().blockSignals(True)
        for (i, r) in enumerate(results):
            key, phase = r
            comment = self.comments[phase][key]
            item = QTableWidgetItem(key[0])
            item.setToolTip(comment)
            set_item_noedit(item)
            tw.setItem(i, 0, item)
            item = QTableWidgetItem(phase)
            set_item_noedit(item)
            tw.setItem(i, 1, item)
            self.search_results[i] = (key, phase)
        tw.model().blockSignals(False)
        if nrows == 1: # Autoselect unique row
            tw.setCurrentCell(0,0)

    def get_species_data(self, key, phase):
        """exposes species database to external clients"""
        db = self.db.get(phase)
        if not db:
            return None
        # FIXME, this is inefficient.  remove tmin/tmax from key tuple.
        #  also, it's possible that there are multiple definitions for the
        #  same species, with different temp. ranges.  This just returns
        #  the first one
        for (keytuple, data) in db.items():
            (species, tmin, tmax) = keytuple
            if species == key:
                (coeffs, mol_weight, _) = data
                a_high = coeffs[:7]
                a_low = coeffs[7:14]
                h_f = coeffs[14]
                return {'phase': phase,
                        'mol_weight': mol_weight,
                        'h_f': h_f,
                        'tmin':  tmin,
                        'tmax': tmax,
                        'a_low': a_low,
                        'a_high': a_high}


    def handle_search_selection(self):
        row = get_selected_row(self.tablewidget_search)
        self.ui.pushbutton_import.setEnabled(row is not None)


    def handle_include_comments(self, val):
        self.include_comments = val
        self.do_search(self.ui.lineedit_search.text())


    def clear_species_panel(self):
        for item in self.species_panel_items:
            item.setEnabled(False)
            if hasattr(item, 'setText'):
                item.setText('')
        tw = self.ui.tablewidget_params
        for row in range(8):
            for col in range(2):
                w = tw.cellWidget(row, col)
                if w:
                    w.setText('')
                    #tw.cellWidget(i,j).setText('')


    def enable_species_panel(self):
        for item in self.species_panel_items:
            item.setEnabled(True)
        species = self.current_species
        data = self.defined_species.get(species)
        ui = self.ui
        self.ui.combobox_phase.setEnabled(False)

        def make_item(val, key=None):
            item = QLineEdit()
            item.setText(str(val))
            item.setValidator(QDoubleValidator(item))
            item.setFrame(False)
            if key:
                item.editingFinished.connect(make_handler(item=item, key=key))
                #inputRejected is only available in Qt 5.12 and above
                #item.inputRejected.connect(make_reject_handler(item=item, key=key))
                make_reject_handler(item=item, key=key)
                make_foe(item, key)
            return item

        def make_foe(item, key):
            item._focusOutEvent = item.focusOutEvent
            def foe(ev, item=item, key=key):
                txt = item.text()
                try:
                    val = float(txt)
                    item.setText(str(val))
                except ValueError:
                    val = 0.0
                    if txt == '':
                        if key == 'tmin':
                            val = 200
                        elif key == 'tmax':
                            val = 6000
                    item.setText(str(val))
                    item.reject_handler(item, key)
                    self.check_data()
                return item._focusOutEvent(ev)
            item.focusOutEvent = foe


        def make_handler(item, key):
            def handler(item=item, key=key):
                if not self.current_species:
                    print("Error, no current species")
                    return
                val = item.text()
                try:
                    data = self.defined_species[self.current_species]
                    val = float(val)
                    if isinstance(key, tuple):
                        data[key[0]][key[1]] = val
                    else:
                        data[key] = val
                except ValueError:
                    # should not get here, field has been validated
                    pass
                self.check_data()
            return handler

        def make_reject_handler(item, key):
            def handler(item=item, key=key):
                val = 0
                if not self.current_species:
                    print("Error, no current species")
                    return
                data = self.defined_species[self.current_species]
                if key == 'tmin':
                    val = 200
                if key == 'tmax':
                    val = 6000
                if isinstance(key, tuple):
                    data[key[0]][key[1]] = val
                else:
                    data[key] = val
                    # reset field to prev. value
                    pass
            item.reject_handler = handler
            return handler

        i = 'GLSC'.index(data['phase'])
        ui.combobox_phase.setCurrentIndex(i)
        ui.combobox_phase.setToolTip(phase_names[i])
        ui.lineedit_alias.setText(data['alias'])
        ui.lineedit_mol_weight.setText(str(data['mol_weight']))
        ui.lineedit_h_f.setText(str(data['h_f']))
        if self.density_enabled:
            density = data.get('density')
            ui.lineedit_density.setText('' if density is None else str(density))
            handler = make_handler(ui.lineedit_density, 'density')
            ui.lineedit_density.editingFinished.connect(handler)
        tw = ui.tablewidget_params
        tw.setCellWidget(0, 0, make_item(data['tmin'], key='tmin'))
        tw.setCellWidget(0, 1, make_item(data['tmax'], key='tmax'))
        for (i, x) in enumerate(data['a_low']):
            tw.setCellWidget(i+1, 0, make_item(x, key=('a_low', i)))
        for (i, x) in enumerate(data['a_high']):
            tw.setCellWidget(i+1, 1, make_item(x, key=('a_high', i)))


    def check_data(self):
        if not self.current_species:
            self.set_ok_button(False)
        species = self.defined_species[self.current_species]
        self.data_ok = True
        mw = species.get('mol_weight', 0)
        tmax = species.get('tmax')
        h_f = species.get('h_f')
        tmin = species.get('tmin')
        tmax = species.get('tmax')

        # Avoid zero-divide in plotting
        self.ui.pushbutton_plot.setEnabled(plt and mw >0)

        # Check user inputs
        if mw <= 0:
            self.data_ok = False
            self.set_ok_button(False, "Molecular weight must be > 0")
        elif tmin is None:
            self.data_ok = False
            self.set_ok_button(False, "Low-temp range limit must be defined")
        elif tmin < 0:
            self.data_ok = False
            self.set_ok_button(False, "Low-temp range limit must be > 0")
        elif tmin >= 1000:
            self.data_ok = False
            self.set_ok_button(False, "Low-temp range limit must be < 1000°K")
        elif tmax is None:
            self.data_ok = False
            self.set_ok_button(False, "High-temp range limit must be defined")
        elif tmax <= tmin:
            self.data_ok = False
            self.set_ok_button(False, "High-temp range limit must be > %s"%tmin)
        elif h_f is None:
            self.data_ok = False
            self.set_ok_button(False, "Heat of formation must be defined")
        elif all(x==0 for x in species['a_low']):
            self.data_ok = False
            self.set_ok_button(False, "Low-temp coefficients can not all be 0")
        elif (tmax > 1000 and all(x==0 for x in species['a_high'])):
            self.data_ok = False
            self.set_ok_button(False,
                "High-temp coefficients can not all be 0 when t_high > 1000°K")

        if self.data_ok and self.alias_ok:
            self.set_ok_button(True)


    def enable_density(self, enabled):
        self.density_enabled = enabled
        ui = self.ui
        if not enabled:
            if ui.lineedit_density in self.species_panel_items:
                self.species_panel_items.remove(ui.lineedit_density)
                for w in (ui.label_density, ui.label_density_units, ui.lineedit_density):
                    w.setEnabled(False)
            ui.lineedit_density.clear()
        else:
            for w in (ui.label_density, ui.label_density_units, ui.lineedit_density):
                w.setEnabled(True)
            self.species_panel_items.append(ui.lineedit_density)


    def handle_defined_species_selection(self):
        self.ui.tablewidget_search.clearSelection()
        tw = self.tablewidget_defined_species
        row = get_selected_row(tw)

        if row is None:
            self.current_species = None
            self.clear_species_panel()
            self.ui.pushbutton_copy.setEnabled(False)
            self.ui.combobox_phase.setEnabled(False)
        else:
            self.ui.pushbutton_copy.setEnabled(True)
            self.current_species = tw.item(row, 0).text()
            self.enable_species_panel()


    def make_alias(self, name):
        #Aliases must be unique.
        #Aliases are limited to 32 characters and must follow FORTRAN variable
        #naming conventions (i.e., alphanumeric combinations with a letter as the first
        #character).
        #Aliases are not case sensitive.
        #Aliases cannot conflict with existing MFiX variable names (e.g., a species
        # alias of MU_g will cause an error when compiling MFiX).

        for (pat, repl) in [(' ', '_'),
                            ('(', '_'),
                            (')', ''),
                            ('__', '_')]:
            name = name.replace(pat, repl)

        alias = ''.join([c for c in name if c.isalnum() or c=='_'])

        while alias and not alias[0].isalpha(): # strip leading _ and digits
            alias = alias[1:]

        if len(alias) > 32:
            alias = alias[:32]

        if alias.lower() not in self.reserved_aliases:
            return alias

        count = 1
        base = alias[:28] # leave room for _nn
        # Strip _nn suffix
        if '_' in alias:
            i = alias.rindex('_')
            if i > 0 and alias[i+1:].isdigit():
                count = 1 + int(alias[i+1:])
                base = alias[:i][:28]

        while alias.lower() in self.reserved_aliases:
            alias = '%s_%s' % (base, count)
            count += 1

        return alias


    def make_user_species_name(self):
        n = 1
        while "Species_%d" % n in self.user_species_names:
            n += 1
        name = "Species_%d" % n
        self.user_species_names.add(name)
        return name


    def do_import(self):
        rows = get_selected_rows(self.tablewidget_search)
        for row in rows:
            self.do_import_row(row)


    def do_import_row(self, row):
        self.ui.combobox_phase.setEnabled(False)
        rowdata = self.search_results[row]
        key, phase = rowdata
        data = self.db[phase][key]
        (species, tmin, tmax) = key
        (coeffs, mol_weight, _) = data

        alias = self.make_alias(species)

        a_high = coeffs[:7]
        a_low = coeffs[7:14]
        h_f = coeffs[14]

        species_data = {
            'phase': phase,
            'alias': alias,
            'species': species,
            'mol_weight': mol_weight,
            'h_f': h_f,
            'tmin':  tmin,
            'tmax': tmax,
            'a_low': a_low,
            'a_high': a_high,
            'burcat': key[0], # Save this as comment in THERMO DATA
        }

        if self.density_enabled:
            species_data['density'] = None # ? where do we get this?
        self.defined_species[alias] = species_data
        self.add_defined_species_row(alias, select=True)
        self.check_data()


    def update_defined_species(self):
        self.tablewidget_defined_species.clearSelection()
        self.tablewidget_defined_species.setRowCount(0)
        for species_key in self.defined_species.keys():
            self.add_defined_species_row(species_key, select=False)


    def add_defined_species_row(self, alias, select=False):
        species_data = self.defined_species[alias]
        ui = self.ui
        tw = ui.tablewidget_defined_species
        nrows = tw.rowCount()
        tw.setRowCount(nrows+1)
        phase = species_data['phase']
        item = QTableWidgetItem(alias)
        set_item_noedit(item)
        tw.setItem(nrows, 0, item)
        item = QTableWidgetItem(phase)
        set_item_noedit(item)
        tw.setItem(nrows, 1, item)

        if select:
            tw.setCurrentCell(nrows, 0) # select new row


    def handle_copy(self):
        tw = self.ui.tablewidget_defined_species
        row = get_selected_row(tw)
        if row is None:
            return
        species = tw.item(row, 0).text()
        alias = self.make_alias(species)
        if species not in self.defined_species:
            return
        species_data = deepcopy(self.defined_species[species])
        species_data['alias'] = alias
        species_data['species'] = species
        species = alias
        self.defined_species[species] = species_data
        self.current_species = species
        self.enable_species_panel()
        self.add_defined_species_row(alias, select=True)
        lineedit = self.ui.lineedit_alias
        lineedit.selectAll()
        lineedit.setFocus(0)
        self.ui.combobox_phase.setEnabled(True)
        self.check_data()

    def handle_new(self):
        phase = self.default_phase
        alias = species = self.make_user_species_name()
        mol_weight = 0
        density = None
        h_f = 0
        tmin = 200.0
        tmax = 6000.0
        a_low = [0.0]*7
        a_high = [0.0]*7

        species_data = {'phase': phase,
                        'alias': alias,
                        'species': species,
                        'mol_weight': mol_weight,
                        'density': density,
                        'h_f': h_f,
                        'tmin':  tmin,
                        'tmax': tmax,
                        'a_low': a_low,
                        'a_high': a_high}

        self.defined_species[species] = species_data
        self.current_species = species
        self.enable_species_panel()
        self.add_defined_species_row(alias, select=True)
        lineedit = self.ui.lineedit_alias
        lineedit.selectAll()
        lineedit.setFocus(0)
        self.ui.combobox_phase.setEnabled(True)
        self.check_data()

    def handle_alias(self):
        val = self.ui.lineedit_alias.text() # Already validated (?)
        tw = self.ui.tablewidget_defined_species
        row = get_selected_row(tw)
        if row is None: # No selection
            return
        #note, making a new item here, instead of changing item inplace
        item = QTableWidgetItem(val)
        set_item_noedit(item)
        tw.setItem(row, 0, item)
        defined_species = OrderedDict()
        for (key, data) in self.defined_species.items():
            if key == self.current_species:
                key = val
                data['alias'] = val
            defined_species[key] = data
        self.current_species = val
        self.defined_species = defined_species

    def set_ok_button(self, state, msg=''):
        self.ui.pushbutton_ok.setEnabled(state)
        self.ui.label_status.setText(msg)
        self.ui.label_status.setStyleSheet("color: red;" if state is False
                                           else "color: blue;")

    def handle_combobox_phase(self, index):
        phase = 'GLSC'[index]
        if not self.current_species:
            return
        species = self.defined_species[self.current_species]
        species['phase'] = phase
        self.ui.combobox_phase.setToolTip(phase_names[index])

    def reset_signals(self):
        for sig in (self.cancel, self.save):
            try:
                sig.disconnect()
            except:
                pass

    def handle_phase(self):
        phases = ''
        for phase in 'GLSC':
            button = getattr(self.ui, 'pushbutton_%s' % phase)
            if button.isChecked():
                phases += phase
        if phases == self.phases:
            return
        self.phases = phases
        self.default_phase = phases[0] if phases else ''
        self.do_search(self.ui.lineedit_search.text())

    def __init__(self, parent=None, phases='GLCS'):
        super(SpeciesPopup, self).__init__(parent)
        self.phases = phases
        self.include_comments = True
        self.default_phase = phases[0] if phases else ''
        self.density_enabled = True
        datadir = os.path.abspath(os.path.dirname(__file__))
        self.load_burcat(os.path.join(datadir, 'burcat.pickle'))
        ui = self.ui = get_ui('species_popup.ui', self)
        # key=alias, val=data tuple.  can add phase to key if needed
        self.defined_species = OrderedDict()
        self.reserved_aliases = set() # To support enforcing uniqueness
        if parent:
            self.mfix_keywords = set(k.lower() for k in self.parent().keyword_doc.keys())
        else:
            self.mfix_keywords = set()

        self.search_results = []
        self.user_species_names = set()

        # Set up UI
        ui.lineedit_search.textChanged.connect(self.do_search)
        ui.pushbutton_import.clicked.connect(self.do_import)
        ui.pushbutton_import.setEnabled(False)
        ui.tablewidget_search.itemSelectionChanged.connect(
            self.handle_search_selection)
        ui.tablewidget_defined_species.itemSelectionChanged.connect(
            self.handle_defined_species_selection)

        ui.pushbutton_new.clicked.connect(self.handle_new)
        ui.pushbutton_copy.clicked.connect(self.handle_copy)
        ui.checkbox_include_comments.setChecked(True)
        ui.checkbox_include_comments.clicked.connect(self.handle_include_comments)

        for phase in 'GLSC':
            button = getattr(self.ui, 'pushbutton_%s' % phase)
            button.clicked.connect(self.handle_phase)

        cb = ui.combobox_phase
        cb.currentIndexChanged.connect(self.handle_combobox_phase)
        for i,t in enumerate(phase_names):
            get_combobox_item(cb, i).setToolTip(t)

        #http://stackoverflow.com/questions/15845487/how-do-i-prevent-the-enter-key-from-closing-my-qdialog-qt-4-8-1
        # Do not use buttonbox.  https://mfix.netl.doe.gov/gitlab/develop/mfix/issues/101
        buttons = (ui.pushbutton_ok, ui.pushbutton_cancel)
        buttons[0].clicked.connect(lambda: (self.save.emit(), self.close()))
        buttons[1].clicked.connect(lambda: (self.cancel.emit(), self.close()))

        self.alias_ok = False
        self.data_ok = False
        class AliasValidator(QValidator):

            # Make sure aliases are unique
            def __init__(self, parent=None):
                super(AliasValidator, self).__init__()
                self.parent = parent

            def validate(self, text, pos):
                # 'parent' here is the popup, not the mfix gui
                self.parent.alias_ok = False
                if text == "":
                    self.parent.set_ok_button(False)
                    return (QValidator.Intermediate, text, pos)
                if (text[0].isdigit()
                    or text[0] == '_'
                    or not all (c=='_' or c.isalnum() for c in text)):
                    return (QValidator.Invalid, text, pos)
                tlower = text.lower()
                if tlower in self.parent.mfix_keywords:
                    self.parent.set_ok_button(False, '%s is an MFiX keyword'%text)
                    return (QValidator.Intermediate, text, pos)
                if tlower in self.parent.reserved_aliases:
                    self.parent.set_ok_button(False, 'Alias must be unique')
                    return (QValidator.Intermediate, text, pos)
                self.parent.alias_ok = True
                self.parent.check_data()
                return (QValidator.Acceptable, text, pos)

        lineedit = ui.lineedit_alias
        lineedit.setValidator(AliasValidator(parent=self))
        lineedit.editingFinished.connect(self.handle_alias)
        lineedit._focusOutEvent = lineedit.focusOutEvent
        def handle_alias_foe(ev):
            if lineedit.text() == '':
                lineedit.setText(self.current_species or '')
            return lineedit._focusOutEvent(ev)

        lineedit.focusOutEvent = handle_alias_foe
        lineedit.setMaxLength(32)

        for line_edit in (ui.lineedit_mol_weight,
                          ui.lineedit_h_f,
                          ui.lineedit_density):
            line_edit.setValidator(QDoubleValidator())

        self.species_panel_items = [ui.lineedit_alias,
                                    ui.lineedit_mol_weight,
                                    ui.lineedit_h_f,
                                    ui.lineedit_density,
                                    ui.tablewidget_params,
                                    ui.widget_plot_options]

        hv = QHeaderView
        for tw in (self.tablewidget_search, self.tablewidget_defined_species):
            resize_column(tw, 0, hv.Stretch)
            resize_column(tw, 1, hv.ResizeToContents)
        tw = self.tablewidget_params
        for i in (0, 1):
            resize_column(tw, i, hv.Stretch)

        # plot button
        b = ui.pushbutton_plot
        if plt is not None:
            b.clicked.connect(self.plot_cp)
        else:
            b.setEnabled(False)
            b.setToolTip("Matplotlib not available")

        def make_foe(item, key):
            item._focusOutEvent = item.focusOutEvent
            def foe(ev, item=item, key=key):
                txt = item.text()
                try:
                    val = float(txt)
                    item.setText(str(val))
                except ValueError:
                    item.setText('0.0')
                    item.reject_handler(item, key)
                    self.check_data()
                return item._focusOutEvent(ev)
            item.focusOutEvent = foe


        def make_handler(item, key):
            def handler(item=item, key=key):
                if not self.current_species:
                    print("Error, no current species")
                    return
                val = item.text()
                try:
                    data = self.defined_species[self.current_species]
                    val = float(val)
                    if isinstance(key, tuple):
                        data[key[0]][key[1]] = val
                    else:
                        data[key] = val
                except ValueError:
                    # should not get here, field has been validated
                    pass
                self.check_data()
            return handler

        def make_reject_handler(item, key):
            def handler(item=item, key=key):
                if not self.current_species:
                    print("Error, no current species")
                    return
                data = self.defined_species[self.current_species]
                if isinstance(key, tuple):
                    data[key[0]][key[1]] = 0
                else:
                    data[key] = 0
                    # reset field to prev. value
                    pass
            item.reject_handler = handler
            return handler

        handler = make_handler(ui.lineedit_mol_weight, 'mol_weight')
        ui.lineedit_mol_weight.editingFinished.connect(handler)
        make_reject_handler(ui.lineedit_mol_weight, 'mol_weight')
        make_foe(ui.lineedit_mol_weight, 'mol_weight')

        handler = make_handler(ui.lineedit_h_f, 'h_f')
        ui.lineedit_h_f.editingFinished.connect(handler)
        make_reject_handler(ui.lineedit_h_f, 'h_f')
        make_foe(ui.lineedit_h_f, 'h_f')

        self.set_ok_button(False) # nothing to accept
        self.clear_species_panel()


    def set_phases(self, phases):
        if phases == self.phases:
            return
        self.phases = phases
        for phase in 'GLSC':
            button = getattr(self.ui, 'pushbutton_%s' % phase)
            button.setChecked(phase in phases)
        self.default_phase = phases[0] if phases else ''
        self.do_search(self.ui.lineedit_search.text())


    def popup(self):
        self.show()
        self.raise_()
        self.activateWindow()

    def plot_cp(self):

        species = self.current_species
        data = self.defined_species.get(species)
        ui = self.ui
        t_range = [safe_float(ui.lineedit_plt_temp_from.text(), 250),
                   safe_float(ui.lineedit_plt_temp_to.text(), 4000)]
        from_ = max(min(t_range), data['tmin'])
        to_ = min(max(t_range), data['tmax'])
        t_break = 1000
        R = 8.3144598*1000  # J/(K.kmol)
        MW = data['mol_weight']  # kg/kmol

        r = R/MW  # J/(K.kg)

        plt.ion()
        plt.clf()
        #plt.grid(True) # Can we put this under user control?

        def plot(a, trange, lbl):
            temps = np.linspace(min(trange), max(trange), 500)
            cp = r * sum(a[i]*temps**i for i in range(5))
            plt.plot(temps, cp, label=lbl)
            return cp

        cp_low = cp_high = None
        if from_ < t_break:
            cp_low = plot(data['a_low'], [from_, min(t_break, to_)], "Low")
        if to_ > t_break:
            cp_high = plot(data['a_high'], [max(t_break, from_), to_], "High")

        if from_ > min(t_range):
            if cp_low is not None:
                plt.plot([from_, min(t_range)], [cp_low[0]]*2, 'k--', alpha=.5)
            elif cp_high is not None:
                plt.plot([from_, min(t_range)], [cp_high[0]]*2, 'k--', alpha=.5)
        if to_ < max(t_range):
            if cp_high is not None:
                plt.plot([to_, max(t_range)], [cp_high[-1]]*2, 'k--', alpha=.5)
            elif cp_low is not None:
                plt.plot([to_, max(t_range)], [cp_low[-1]]*2, 'k--', alpha=.5)

        plt.xlabel('Temperature [K]')
        plt.ylabel('Cp [J/kg.K]')
        plt.legend()
        plt.title(species)
        plt.tight_layout()
        gcf = plt.gcf()
        #set_window_title was moved to canvas.manager
        if hasattr(gcf.canvas, 'manager'):
            obj = gcf.canvas.manager
        else:
            obj = gcf.canvas
        obj.set_window_title('Specific heat for '+species)


        plt.show(block=False)


def main():
    args = sys.argv
    qapp = QApplication(args)
    dialog = QDialog()
    species_popup = SpeciesPopup(dialog, phases='GL')
    species_popup.show()
    # exit with Ctrl-C at the terminal
    signal.signal(signal.SIGINT, signal.SIG_DFL)

    qapp.exec_()
    qapp.deleteLater()

    sys.exit()

if __name__ == '__main__':
    main()
