# -*- coding: utf-8 -*-
"""Advanced pane"""

import re

from mfixgui.tools import keyword_args
from mfixgui.advanced_keys import advanced_keys

from qtpy import QtCore
from qtpy.QtCore import Signal, QRegExp, QTimer
from qtpy.QtWidgets import QDialog, QTableWidgetItem, QLabel
from qtpy.QtGui import QRegExpValidator
from mfixgui.widgets.base import LineEdit

from mfixgui.tools import format_key_with_args
from mfixgui.tools.qt import (set_item_noedit, get_selected_row,
                              get_ui, get_icon, get_combobox_item,
                              sub_icon_size)

from mfixgui.locate_keyword import locate_keyword
UserRole = QtCore.Qt.UserRole

class Advanced:
    def init_advanced(self):
        ui = self.ui.advanced
        ui.toolbutton_add.clicked.connect(self.add_advanced)
        ui.toolbutton_delete.clicked.connect(self.delete_advanced)
        ui.toolbutton_edit.clicked.connect(self.edit_advanced)
        for tb in (ui.toolbutton_delete, ui.toolbutton_edit):
            tb.setEnabled(False) # Need a selection
        tw = ui.tablewidget_advanced
        tw.itemSelectionChanged.connect(self.handle_advanced_selection)
        self.advanced_popup = AdvancedPopup(QDialog(), self)
        self.advanced_popup.setWindowTitle("MFiX keyword browser")
        # Double-click signal comes too soon, while mouse button is still down
        # and disturbs row selection.
        #tw.doubleClicked.connect(lambda: (time.sleep(0.25), self.edit_advanced()))
        tw.doubleClicked.connect(lambda: QTimer.singleShot(250, self.edit_advanced))
        self.unlocked_keys = []

    def reset_advanced(self):
        ui = self.ui.advanced
        tw = ui.tablewidget_advanced
        tw.clearContents()
        self.unlocked_keys.clear()
        for tb in (ui.toolbutton_delete, ui.toolbutton_edit):
            tb.setEnabled(False)


    def setup_advanced(self, allow_disabled_tab=False):
        ui = self.ui.advanced
        tw = ui.tablewidget_advanced
        row = get_selected_row(tw)
        # Autoselect if only 1 row
        if row is None and tw.rowCount() == 1:
            row = 0
            tw.setCurrentCell(row, 0)
        enabled = (row is not None)
        for item in (ui.toolbutton_delete, ui.toolbutton_edit):
            item.setEnabled(enabled)

        keys = []
        usr_keys = self.project.usr_keyword_doc or {}
        for k in self.project.keywordItems():
            if (k.key in advanced_keys
                or k.key in usr_keys
                or k.key in self.unlocked_keys):
                keys.append((k.key, k.args))
        keys.sort()
        tw.clearContents()
        tw.setRowCount(len(keys))

        def make_item(s):
            item = QTableWidgetItem(s)
            set_item_noedit(item)
            return item

        for i, (key, args) in enumerate(keys):
            val = self.project.get_value(key, args=args)
            item = make_item(format_key_with_args(key, args))
            item.args = args
            self.add_tooltip(item, key)
            item.setData(UserRole, (key, args))
            tw.setItem(i, 0, item)
            item = make_item(str(val))
            item.args = args
            tw.setItem(i, 1, item)
            self.add_tooltip(item, key, value=val)


        tw.resizeColumnToContents(0)

    def add_advanced(self):
        # Define a new advanced key
        # New row is inserted in alphanumeric order
        # This just shows the popup, row is added
        #  when pushbutton_ok is pressed
        ap = self.advanced_popup
        ap.reset()
        ap.set_advanced_mode()
        #ap.ui.lineedit_search.clear()
        ap.ui.info_area.clear()
        ap.ui.tablewidget_keys.clearSelection()
        ap.ui.lineedit_search.setFocus()
        ap.ui.pushbutton_ok.setEnabled(False) # No selection
        ap.show()
        ap.repaint()

    def add_advanced_1(self, *args):
        #self.advanced_popup.close() issues/916
        if self.advanced_popup.mode == 'basic':
            return # No modifications in basic mode
        key, args, val = self.advanced_popup.get_selection()
        if key is None: # User did not select anything
            return
        self.update_keyword(key, val, args=args)
        usr_keys = self.project.usr_keyword_doc or {}
        advanced = (key in advanced_keys or key in usr_keys)
        if not advanced and key not in self.unlocked_keys:
            self.unlocked_keys.append(key)

        self.setup_advanced()
        # Select newly added row
        ui = self.ui.advanced
        tw = ui.tablewidget_advanced
        for row in range(tw.rowCount()):
            if tw.item(row,0).data(UserRole) == (key, args):
                tw.setCurrentCell(row, 0)
                break


    def delete_advanced(self):
        ui = self.ui.advanced
        tw = ui.tablewidget_advanced
        row = get_selected_row(tw)
        if row is None:
            return
        key, args = tw.item(row, 0).data(UserRole)
        self.unset_keyword(key, args)
        tw.removeRow(row)
        if key in self.unlocked_keys and not self.project.get_key_indices(key):
            self.unlocked_keys.remove(key)


    def edit_advanced(self):
        ui = self.ui.advanced
        tw = ui.tablewidget_advanced
        row = get_selected_row(tw)
        if row is None:
            return
        row0 = row # Remember selection so we can restore it
        key, args = tw.item(row, 0).data(UserRole)
        ap = self.advanced_popup
        ap.set_advanced_mode()
        ap.reset()
        ap.ui.lineedit_search.clear()
        # Find matching key in popup table
        tw = ap.tablewidget_keys
        for row in range(tw.rowCount()):
            item = tw.item(row, 0)
            if item.text() == key:
                break
        else:
            self.error("key %s not found" % key)
            return
        if key in self.unlocked_keys:
            ap.ui.checkbox_include_gui_keys.setChecked(True)
        if args:
            for (le, arg) in zip(ap.ui.arg_inputs, args):
                le.updateValue(None, arg, None)
        ap.do_search()
        tw.scrollToItem(tw.item(row, 0), 1) # Put it at the top
        tw.setCurrentCell(row, 0) # triggers 'handle_selection'
        if key in self.unlocked_keys:
            ap.ui.checkbox_include_gui_keys.setChecked(True)
            ap.locked = False
            tb = ap.ui.toolbutton_lock
            tb.setIcon(ap.ui.unlock_icon)
            tb.setIconSize(sub_icon_size()/2)
            if not hasattr(tb, 'tooltip0'):
                tb.tooltip0 = tb.toolTip()
                tb.setToolTip("Unlocked")
        ap.do_search()
        if args:
            for (le, arg) in zip(ap.ui.arg_inputs, args):
                le.updateValue(None, arg, None)
        ap.show_keyword_value()
        ap.show()
        ap.repaint() #?
        # Restore selection
        ui.tablewidget_advanced.setCurrentCell(row0, 0)

    def handle_advanced_selection(self):
        ui = self.ui.advanced
        tw = ui.tablewidget_advanced
        row = get_selected_row(tw)
        enabled = (row is not None)
        for item in (ui.toolbutton_delete, ui.toolbutton_edit):
            item.setEnabled(enabled)


class AdvancedPopup(QDialog):
    # The Advanced Popup is a keyword browser, it also functions as the "help" dialog
    def __init__(self, app, parent=None):
        super(AdvancedPopup, self).__init__(parent)
        self.app = app
        self.parent = parent
        self.key = None
        ui = self.ui = get_ui('advanced_popup.ui', self)
        self.ui.arg_inputs = []
        self.ui.arg_labels = []
        self.mode = 'advanced'
        self.locked = True
        self.lock_icon = get_icon('lock.svg')
        self.unlock_icon = get_icon('unlock.svg')
        ui.pushbutton_ok.clicked.connect(self.parent.add_advanced_1)
        ui.pushbutton_ok2.clicked.connect(self.close)
        ui.pushbutton_cancel.clicked.connect(self.close)

        ui.checkbox_value.clicked.connect(lambda val, cb = ui.checkbox_value:
                        cb.setText('True' if val else 'False'))
        ui.lineedit_search.textChanged.connect(self.do_search)
        ui.lineedit_search.setValidator(QRegExpValidator(QRegExp('[a-zA-Z0-9_ ]*')))
        ui.lineedit_value.allow_parameters = True
        ui.checkbox_search_comments.clicked.connect(self.do_search)
        ui.checkbox_search_comments.setChecked(True) #Default
        ui.checkbox_include_gui_keys.clicked.connect(self.toggle_gui_keys)
        ui.checkbox_include_gui_keys.setChecked(False) #Default
        ui.pushbutton_ok.setEnabled(False)
        ui.pushbutton_locate.clicked.connect(self.locate_keyword)
        tw = ui.tablewidget_keys
        tw.itemSelectionChanged.connect(self.handle_selection)
        for w in (ui.label_value_combo, ui.combobox_value,
                  ui.label_value_bool, ui.checkbox_value,
                  ui.label_value, ui.lineedit_value):
            w.hide()
        ui.toolbutton_lock.clicked.connect(self.unlock)
        self.reset()

    def unlock(self):
        ui = self.ui
        if self.locked:
            confirm = self.parent.message(
                title='Unlock key?',
                text="""Warning: Unlocking this control allows direct editing of all MFiX keys, including keys normally controlled by the GUI.  <i>Adjusting values here may leave the GUI in an inconsistent state or produce simulations that MFiX will refuse to run.</i>  Please use caution.""",
                buttons=['ok', 'cancel'],
                default = 'cancel',
                print_console = False)
            if confirm != 'ok':
                return

        self.locked = False
        tb = ui.toolbutton_lock
        tb.setIcon(self.unlock_icon)
        tb.setIconSize(sub_icon_size()/2)
        if not hasattr(tb, 'tooltip0'):
            tb.tooltip0 = tb.toolTip()
        ui.toolbutton_lock.setToolTip("Unlocked")
        self.show_keyword_value()

    def reset(self):
        ui = self.ui
        self.locked = True
        tb = ui.toolbutton_lock
        tb.setIcon(self.lock_icon)
        tb.setIconSize(sub_icon_size()/2)
        if hasattr(tb, 'tooltip0'):
            tb.setToolTip(tb.tooltip0)
        tw = ui.tablewidget_keys
        all_keys = sorted(self.parent.project.keyword_doc.keys())
        tw.setRowCount(len(all_keys))
        def make_item(s):
            item = QTableWidgetItem(s)
            set_item_noedit(item)
            return item
        first_row = None
        usr_keys = self.parent.project.usr_keyword_doc or {}
        for (row, key) in enumerate(all_keys):
            tw.setItem(row, 0, make_item(key))
            advanced = (key in advanced_keys
                        or key in usr_keys or
                        key in self.parent.unlocked_keys)
            tw.item(row, 0).setData(UserRole, advanced)
            tw.setRowHidden(row, not advanced)
            if first_row is None and advanced:
                first_row = row
        # How to set table width?
        #tw.resizeColumnToContents(0)
        #tw.setCurrentCell(first_row, 0)


    def toggle_gui_keys(self, val):
        ui = self.ui
        #ui.pushbutton_locate.setEnabled(val)
        self.do_search()
        tw = ui.tablewidget_keys
        row = get_selected_row(tw)
        if row is not None:
            tw.scrollToItem(tw.item(row,0), 1)

    def do_search(self):
        ui = self.ui
        tw = ui.tablewidget_keys
        search_comments = ui.checkbox_search_comments.isChecked()
        needle = ui.lineedit_search.text().lower().strip()
        selected = get_selected_row(tw)
        first_row = None
        include_gui_keys = self.mode=='basic' or ui.checkbox_include_gui_keys.isChecked()
        n_match = 0
        first = None
        for row in range(0, tw.rowCount()):
            key = tw.item(row, 0).text()
            advanced = tw.item(row, 0).data(UserRole)
            if not (advanced or include_gui_keys):
                show = False
            #elif self.mode=='basic' and advanced:
            #    show = False # let help popup show everything
            elif not needle: #Empty string matches everything
                show = True
                if first is None:
                    first = row
            else:
                show = False
                if needle in key:
                    show = True
                    if first is None:
                        first = row
                elif search_comments:
                    doc = self.parent.project.keyword_doc.get(key, {})
                    if doc:
                        desc = doc.get('description', '')
                        # remove cylindrical refs
                        for x in ' (theta) direction', ' (r) direction':
                            desc = desc.replace(x, '-direction')
                        valids = doc.get('valids')
                        extra = []
                        if valids:
                            for (k,v) in valids.items():
                                extra.append(k)
                                alias = v.get('alias')
                                if alias and alias.lower() != k.lower():
                                    extra.append(alias)
                                note = v.get('note')
                                if note:
                                    extra.append(note)
                            desc += ' '.join(extra)

                        show = needle in desc.lower()
                        if show and first is None:
                            first = row
            tw.setRowHidden(row, not show)
            if show:
                n_match += 1

            if row==selected and not show:
                ui.info_area.clear()
                tw.clearSelection()
        if n_match == 1:
            tw.setCurrentCell(first, 0)
        # Force search term to highlight
        self.handle_selection()
        if n_match == 0:
            ui.info_area.setText("<b>No search results</b>")

    def get_selection(self):
        ui = self.ui
        tw = ui.tablewidget_keys
        row = get_selected_row(tw)
        if row is None:
            return (None, (), None)
        key = tw.item(row, 0).text()
        doc = self.parent.project.keyword_doc.get(key, {})
        dtype = doc.get('dtype')
        args = [w.value for w in ui.arg_inputs]
        val = (ui.checkbox_value.isChecked() if dtype=='L'
               else ui.lineedit_value.value if not (dtype=='C' and doc.get('valids'))
               else ui.combobox_value.currentData())
        return (key, args, val)

    def handle_selection(self):
        ui = self.ui
        tw = ui.tablewidget_keys
        row = get_selected_row(tw)
        layout = ui.gridlayout_bottom
        if row is None:
            ui.info_area.clear()
            for w in ui.arg_inputs + ui.arg_labels:
                layout.removeWidget(w)
                w.deleteLater()
            ui.arg_inputs.clear()
            ui.arg_labels.clear()
            for w in (ui.label_value_combo, ui.combobox_value,
                      ui.label_value_bool, ui.checkbox_value,
                      ui.label_value, ui.lineedit_value):
                w.hide()
            ui.pushbutton_locate.setEnabled(False)
            self.key = None
            return
        key = tw.item(row,0).text()
        self.key = key
        doc = self.parent.project.keyword_doc.get(key, {})
        descr = doc.get('description', 'Unavailable')
        # Highlight search term
        search_comments = ui.checkbox_search_comments.isChecked()
        needle = ui.lineedit_search.text().strip().lower()
        descr = format_desc(descr, needle)
        dtype = doc.get('dtype', 'Unknown')
        le = ui.lineedit_value
        if dtype not in ('Unknown', 'L'):
            le.setdtype(dtype)

        type_name = {'I':'Integer', 'DP':'Real', 'C':'String', 'L':'Boolean'}.get(dtype, dtype)
        args = doc.get('args', {})
        args = [v.get('id','?') for v in args.values()]
        if args:
            usage = '%s(%s) = value' % (key, ','.join(args))
        else:
            usage = '%s = value' % key

        text = ('<b>Description</b><br>%s<br><br>'
                '<b>Data type</b><br>%s<br><br>'
                '<b>Usage</b><br>%s' % (descr, type_name, usage))
        valids = doc.get('valids')
        if valids:
            lines = ['<br><br><b>Valid values</b>']
            for (k,v) in valids.items():
                alias = v.get('alias')
                val = alias or k
                if alias and k != alias:
                    val += '(%s)'% k
                note = format_desc(v.get('note', ''), needle)
                if needle: # Highlight in valid values, format_desc doesn't do this
                    pat = re.compile(f'({needle})', re.I)
                    val = pat.sub(r'<span style="background-color: #FFFF00">\1</span>', val)
                lines.append('<b>&bull; %s</b> %s'%(val, note))
            text += '<br>'.join(unicode_greek(l) for l in lines)

        validrange = doc.get('validrange', {})
        le.max = validrange.get('max')
        le.min = validrange.get('min')
        le.exclude_min = validrange.get('exclude_min', False)

        ui.info_area.setText(text)
        if needle:
            idx = text.lower().find(needle)
            if idx >= 0:
                c = ui.info_area.textCursor()
                c.movePosition(idx)
                ui.info_area.setTextCursor(c)
                ui.info_area.ensureCursorVisible() # Does not work

        layout = ui.gridlayout_bottom
        for w in (ui.label_value, ui.lineedit_value,
                  ui.label_value_bool, ui.checkbox_value,
                  ui.label_value_combo, ui.combobox_value):
            layout.removeWidget(w)
        for w in ui.arg_inputs + ui.arg_labels:
            layout.removeWidget(w)
            w.deleteLater()
        ui.arg_inputs.clear()
        ui.arg_labels.clear()

        if self.mode != 'basic':
            for i, arg in enumerate(args):
                label = QLabel(arg)
                layout.addWidget(label, i, 0)
                ui.arg_labels.append(label)
                le = LineEdit()
                le.dtype = int
                le.min = 1 #?
                ui.arg_inputs.append(le)
                le.value_updated.connect(self.handle_arg_input)
                layout.addWidget(le, i, 1)

        row = len(args) + 1
        layout.addWidget(ui.label_value, row, 0)
        layout.addWidget(ui.lineedit_value, row, 1)
        row += 1
        layout.addWidget(ui.label_value_bool, row, 0)
        layout.addWidget(ui.checkbox_value, row, 1)
        row += 1
        layout.addWidget(ui.label_value_combo, row, 0)
        layout.addWidget(ui.combobox_value, row, 1)

        for w in (ui.label_value, ui.lineedit_value):
            w.setHidden(bool(self.mode=='basic' or dtype=='L' or (dtype=='C' and valids)))
        for w in (ui.label_value_bool, ui.checkbox_value):
            w.setVisible(self.mode == 'advanced' and bool(dtype=='L'))
        for w in (ui.label_value_combo, ui.combobox_value):
            w.setVisible(self.mode == 'advanced' and bool(dtype == 'C' and valids))

        if (dtype == 'C' and valids): # Set up combobox
            cb = ui.combobox_value
            cb.clear()
            for (i, (name, data)) in enumerate(valids.items()):
                if name.lower() == 'undefined':
                    val = None
                else:
                    val = data.get('alias') or name

                if val is not None and val != name:
                    cb.addItem('%s(%s)'%(val,name), val)
                else:
                    cb.addItem(name, val)
                note = data.get('note')
                if note:
                    get_combobox_item(cb, i).setToolTip(note)

        row = get_selected_row(tw)
        if row is not None:
            advanced = tw.item(row, 0).data(UserRole)
            ui.pushbutton_locate.setEnabled(True)
        self.show_keyword_value()


    def show_keyword_value(self):
        ui = self.ui
        tw = ui.tablewidget_keys
        row = get_selected_row(tw)
        if row is None:
            # handle_selection already hid the value & arg widgets
            return
        if self.mode == 'basic':
            return
        key = tw.item(row,0).text()
        self.key = key
        doc = self.parent.project.keyword_doc.get(key, {})
        dtype = doc.get('dtype')
        valids = doc.get('valids', {})
        args = [le.value for le in self.arg_inputs]
        args = [None if a=='' else a for a in args]

        label, widget = ((ui.label_value_bool, ui.checkbox_value) if dtype=='L'
                         else (ui.label_value, ui.lineedit_value) if not (dtype=='C' and valids)
                         else (ui.label_value_combo, ui.combobox_value))

        if None in args:
            ui.pushbutton_ok.setEnabled(False)
            label.setEnabled(False)
            widget.setEnabled(False)
            if widget == ui.checkbox_value:
                widget.setChecked(False)
            elif widget == ui.lineedit_value:
                widget.setText('')
            else:
                widget.setCurrentIndex(0)
            return
        usr_keys = self.parent.project.usr_keyword_doc or {}
        advanced = (key in advanced_keys or key in usr_keys)
        for w in (ui.pushbutton_ok, label, widget):
            w.setEnabled(advanced or not self.locked)
        if not advanced:
            self.toolbutton_lock.show()
        default = doc.get('initpython', None)
        val = self.parent.project.get_value(key, default, args=args)
        if widget == ui.checkbox_value:
            widget.updateValue(key, bool(val), args=args)
            widget.setText('True' if bool(val) else 'False')
        elif widget == ui.combobox_value:
            for i in range(widget.count()):
                item = get_combobox_item(widget, i)
                txt = item.text()
                if (txt == val or
                    (txt == 'undefined' and val is None) or
                    ('(' in txt and val is not None and
                     (txt.startswith(val+'(') or txt.endswith('(%s)'%val)))):
                    widget.setCurrentIndex(i)
                    widget.setToolTip(item.toolTip())
                    break
            else:
                widget.setCurrentIndex(0)
                widget.setToolTip(get_combobox_item(widget, 0).toolTip())
                self.parent.error('%s: invalid value %s' % (key, val))
        elif widget == ui.lineedit_value:
            widget.updateValue(None, val, args=args)


    def set_advanced_mode(self):
        ## "Advanced" dialog
        ui = self.ui
        self.mode = 'advanced'
        for w in (#ui.label_value_combo, ui.combobox_value,
                  #ui.label_value_bool, ui.checkbox_value,
                  #ui.label_value, ui.lineedit_value,
                  ui.checkbox_include_gui_keys,
                  ui.pushbutton_cancel,
                  ui.pushbutton_ok,
                  ui.line):
            w.show()
        ui.pushbutton_ok2.hide()
        ui.toolbutton_lock.hide()
        self.do_search()

    def set_basic_mode(self):
        ## "Help" dialog
        ui = self.ui
        self.mode = 'basic'
        for w in (ui.label_value_combo, ui.combobox_value,
                  ui.label_value_bool, ui.checkbox_value,
                  ui.label_value, ui.lineedit_value,
                  ui.checkbox_include_gui_keys,
                  ui.pushbutton_cancel,
                  ui.pushbutton_ok,
                  ui.line):
            w.hide()
        ui.pushbutton_ok2.show()
        ui.toolbutton_lock.hide()
        self.do_search()

    def handle_arg_input(self, *args):
        self.show_keyword_value()


    def locate_keyword(self):
        ret, msg = locate_keyword(self.parent, self.key)
        if ret:
            self.close()
        else:
            text = "Cannot locate keyword <b>%s</b>"%self.key
            if msg:
                text += ':\n '+msg
            self.parent.error(text, popup=True)

def unicode_greek(text):
    ### epsilon
    text = text.replace('epsilon', 'ε')
    text = text.replace('Epsilon', 'ε')
    #text = text.replace('EPSILON', 'ε')

    ### theta
    for t in 'theta', 'THETA', 'Theta':
        text = text.replace(t, 'θ')

    ### lambda
    for t in 'lambda', 'LAMBDA', 'Lambda':
        text = text.replace(t, 'λ')
    return text

# TODO: this code is lifted from gui.py:add_tooltip
#  combine & move to tools
# note, this has highlighting which is not in gui.py
def format_desc(desc, highlight_term=None):
    desc = desc.strip()
    if desc.endswith(']') or desc.endswith('].'):
        dot = desc.endswith('.')
        desc = desc.rsplit('[', 1)[0]
        desc = desc.strip()
        if dot and not desc.endswith('.'): # Put it back
            desc += '.'
    # NB: Order here is different than order in gui.add_tooltip because we don't want
    #  highlighting to match HTML tags, and we don't want '<'/'>' replacement (le/ge)
    #  to remove the highlighting <span> tag

    # '>' and '<' will mess up HTML
    desc = desc.replace('<', '&lt;')
    desc = desc.replace('>', '&gt;')

    # Literature citation
    for x in ('\nSee ', ' See '):
        if x in desc:
            desc = desc.replace(x, '<br>See ')

    # Bullets
    desc = re.sub(r'^\s*-', '<br/>&bull;', desc, flags=re.MULTILINE)
    # non-breaking hyphen
    desc = desc.replace('-', '&#8209;')

    # Replace blank lines with break
    lines = [l if l.strip() else '<br>' for l in  desc.splitlines()]
    desc = '\n'.join(lines)

    if highlight_term:
        pat = re.compile(f'({highlight_term})', re.I)
        desc = pat.sub(r'<span style="background-color: #FFFF00">\1</span>', desc)

    # Don't split diff. eq's over multiple lines
    pat = re.compile(': *(\n.+[\n,])')
    match = pat.search(desc)
    if match or desc.startswith('Partial slip'):
        if match:
            text = match.group(1)
            repl = text[1:-1] #drop the newlines
        else:
            text = re.search('dv[^,]*,', desc).group()
            repl = text[:-1]

        repl = repl.replace('d(', 'd', 1)
        repl = repl.replace(')/', '/', 1)
        repl = repl.replace(' (', '(') # Squeeze out spaces before parens
        # Subscripts
        for sub in 'g', 's', 'M':
            repl = repl.replace('_%s'%sub, '<sub>%s</sub>'%sub) # Make _g into a subscript
        repl = repl.replace('ScalarW', 'Sw')
        repl = repl.replace('Scalar', 'S') # Scalar -> S in equation (?)
        #repl = repl.replace('>d', '></i>d<i>') # de-italicize differential op
        #repl = repl.replace('/d', '/</i>d<i>') # de-italicize differential op
        repl = repl.replace('d', '</i>d<i>')
        repl = '<br><nobr><i>'+repl+'</i></nobr><br>'   # put it on its own line
        #print("REPL=", repl)
        desc = desc.replace(text, repl)
        pat = re.compile(', ([A-Za-z_]+), in', re.MULTILINE)
        match = pat.search(desc)
        if match:
            var = match.group(1)
            text = ', %s, in' % var
            repl = var
            repl = repl.replace('ScalarW', 'Sw')
            repl = repl.replace('Scalar', 'S')

            # Subscripts
            for sub in 'g', 's', 'M':
                repl = repl.replace('_%s'%sub, '<sub>%s</sub>'%sub) # Make _g into a subscript
            repl = ' <i>%s</i> in' % repl
        desc = desc.replace(text, repl, 1)
        # Italicize normal vector to match equation display
        desc = desc.replace('where n is', 'where <i>n</i> is')

        if desc.startswith('Partial slip'):
            for s in 'Hw', 'vw':
                desc = desc.replace('%s'%s,
                                    '<i>%s</i>'%s)


    # remove cylindrical refs
    for x in ' (theta) direction', ' (r) direction':
        desc = desc.replace(x, '-direction')

    desc = unicode_greek(desc)
    ### Default value
    #pat = re.compile(r'\[[^]]+\]')
    #while True:
    #    match = pat.search(desc)
    #    if not match:
    #        break
    #    text = match.group(0)
    #    desc = desc.replace(text, '<i>Default: %s</i>'%text[1:-1])


    # Issues/590 (workaround)
    desc = re.sub(r'\.\s*-', '<br/>&bull;', desc, flags=re.MULTILINE)

    if 'list:' in desc.lower():
        for n in range(1, 20):
            s = ' %d: ' % n
            desc = desc.replace(s, '<br/>&bull;%d: '%n)
    return desc

# TODO should 'ppo' be listed with advanced keys?
