# -*- coding: utf-8 -*-
import copy
import os
from collections import OrderedDict

from qtpy import QtWidgets, QtCore, QtGui
from qtpy.QtCore import Qt, Signal
from qtpy.QtCore import QStringListModel, QItemSelectionModel

# optional imports
try:
    import numpy as np
except ImportError:
    np = None

from mfixgui.animations import make_widget_property_animation
from mfixgui.project import Equation, Keyword, make_FloatExp, pretty_eq_rep
from mfixgui.regexes import *
from mfixgui.constants import *

from mfixgui.tools.qt import get_icon, SETTINGS
from mfixgui.tools import get_unique_string

from mfixgui.tools.simpleeval_wrapper import VALID_EXPRESION_NAMES
VALID_EXP_NAMES = VALID_EXPRESION_NAMES + SPECIAL_PARAMETERS


def insert_append_action(menu, action, insert=None):
    if insert:
        menu.insertAction(insert, action)
    else:
        menu.addAction(action)


def insert_append_separator(menu, insert=None):
    if insert:
        menu.insertSeparator(insert)
    else:
        menu.addSeparator()


def rreplace(s, old, new, occurrence):
    li = s.rsplit(old, occurrence)
    return new.join(li)

class RangeError(ValueError):
    pass

class ElidedLabel(QtWidgets.QLabel):
    """
    A custom QLabel that can elide text.
    """
    def __init__(self, parent=None):
        QtWidgets.QLabel.__init__(self, parent)
        self.full_text = ''
        self.elide = QtCore.Qt.ElideLeft

    def setText(self, text):
        self.full_text = text
        QtWidgets.QLabel.setText(self, self.get_elided_text())

    def resizeEvent(self, event):
        QtWidgets.QLabel.setText(self, self.get_elided_text())

    def get_elided_text(self):
        font_metrics = QtGui.QFontMetrics(self.font())
        return font_metrics.elidedText(self.full_text, self.elide, self.width())

class BaseWidget(QtCore.QObject):
    value_updated = Signal(object, object, object)
    help_request = Signal()
    key = None
    default_value = None
    saved_value = None
    args = None
    min = None
    max = None
    exclude_min = False
    exclude_max = False
    required = None
    default_help_text = 'No help available.'
    help_text = None
    context_menu = None
    allow_parameters = False

    def extend_context_menu(self):
        if self.context_menu is None:
            return
        menu = self.context_menu() # pylint: disable=not-callable
        first_default_action = menu.actions()

        if first_default_action:
            first_default_action = first_default_action[0]
        # help
        help_action = QtWidgets.QAction(
            get_icon('question_mark.svg'), 'Help', menu)
        help_action.triggered.connect(self.show_help_message)
        insert_append_action(menu, help_action, first_default_action)

        # create parameter
        if self.allow_parameters:
            create_param_action = QtWidgets.QAction(
                get_icon('sliders.svg'), 'Create Parameter', menu)
            create_param_action.triggered.connect(self.create_parameter)
            insert_append_action(menu, create_param_action, first_default_action)

        insert_append_separator(menu, first_default_action)

        return menu

    def contextMenuEvent(self, event):
        if self.context_menu is not None:
            menu = self.extend_context_menu()
            menu.exec_(event.globalPos())

    def show_help_message(self):
        message_box = QtWidgets.QMessageBox(self)

        if self.key is None and hasattr(self, 'keys'):
            key = self.keys[0]
        elif self.key is not None:
            key = ': ' + self.key
        else:
            key = ''
        message_box.setWindowTitle('Help: ' + key)
        message_box.setIcon(QtWidgets.QMessageBox.Information)

        # Text
        msg = self.help_text or self.toolTip() or self.default_help_text
        message_box.setText(msg)
        message_box.addButton(QtWidgets.QMessageBox.Ok)
        message_box.exec_()

    def create_parameter(self):
        btn = QtWidgets.QMessageBox.Yes
        if isinstance(self.value, Equation):
            message_box = QtWidgets.QMessageBox(self)
            message_box.setWindowTitle('Warning')
            message_box.setIcon(QtWidgets.QMessageBox.Warning)

            # Text
            message_box.setText("Warning: Replace equation with parameter?")
            message_box.addButton(QtWidgets.QMessageBox.Yes)
            message_box.addButton(QtWidgets.QMessageBox.No)
            message_box.setDefaultButton(QtWidgets.QMessageBox.No)
            btn = message_box.exec_()

        if btn == QtWidgets.QMessageBox.Yes:

            name = get_unique_string('param', PARAMETER_DICT.keys())
            name, rtn = QtWidgets.QInputDialog.getText(
                self, 'Parameter name.', 'Please enter a name for the parameter.',
                text=name,
                )

            if rtn:
                name = name.replace(' ', '_')
                name = get_unique_string(name, PARAMETER_DICT.keys())

                v = self.dtype(0)
                if self.value:
                    v = self.dtype(self.value)
                PARAMETER_DICT[name] = v

                self.updateValue(None, name)
                self.emitUpdatedValue()

    def emitUpdatedValue(self):
        self.value_updated.emit(self, {self.key: self.value}, self.args)

    def setdtype(self, dtype=None):
        dtype = str(dtype).strip().lower()
        if dtype == 'i' or 'int' in dtype:
            self.dtype = int
        elif dtype in ('d', 'dp') or 'float' in dtype:
            self.dtype = float
        elif dtype == 'l' or  'bool' in dtype:
            self.dtype = bool
        elif dtype in ('c', 's') or 'str' in dtype:
            self.dtype = str
        else:
            raise TypeError(self.objectName(), dtype)

    def setValInfo(self, max=None, min=None):
        if max is not None:
            self.max = max
        if min is not None:
            self.min = min


    def default(self, val=None):
        if val is not None:
            self.default_value = val

        if self.default_value is not None:
            self.updateValue(self.key, self.default_value, args=self.args)

        return self.default_value

class LineEdit(QtWidgets.QLineEdit, BaseWidget):
    value_updated = Signal(object, object, object)

    def __init__(self, parent=None):
        QtWidgets.QLineEdit.__init__(self, parent)
        self.textChanged.connect(self.mark_changed)
        self.editingFinished.connect(self.emitUpdatedValue)
        self.dtype = str
        self.text_changed_flag = False
        self.context_menu = self.createStandardContextMenu
        self.error_check = True

        self._separators = ['*', '**', '/', '-', '+', ' ']
        self._completer_model = QStringListModel(sorted(list(PARAMETER_DICT.keys())))
        self._completer = QtWidgets.QCompleter()
        self._completer.setModel(self._completer_model)
        self._completer.setWidget(self)
        self._completer.setCaseSensitivity(Qt.CaseInsensitive)
        self._completer.activated.connect(self._insertCompletion)
        self._keysToIgnore = [Qt.Key_Enter,
                              Qt.Key_Return,
                              Qt.Key_Escape,
                              Qt.Key_Tab] # shouldn't TAB take us to next field?

    @classmethod
    def report_value_error(self, text): # pylint: disable=function-redefined
        print(text)

    @classmethod
    def report_value_required(self, text): # pylint: disable=function-redefined
        print(text)

    def mark_changed(self):
        self.text_changed_flag = True

    def emitUpdatedValue(self):
        need_to_signal = self.text_changed_flag
        self.text_changed_flag = False
        if need_to_signal:
            value = self.value
            if value is not None:
                self.value_updated.emit(self, {self.key: value}, self.args)

    def check_range(self, val):
        if self.error_check:
            if self.min is not None:
                pval = pretty_eq_rep(self.min, self.dtype)
                if self.exclude_min:
                    if val <= self.min:
                        raise RangeError("Value below allowed range:\n {:.3g} <= {}".format(val, pval))
                elif val < self.min:
                    raise RangeError("Value below allowed range:\n {:.3g} < {}".format(val, pval))
            if self.max is not None:
                pval = pretty_eq_rep(self.max, self.dtype)
                if self.exclude_max:
                    if val >= self.max:
                        raise RangeError("Value above allowed range:\n {:.3g} >= {}".format(val, pval))
                elif val > self.max:
                    raise RangeError("Value above allowed range:\n {:.3g} > {}".format(val, pval))

    @property
    def value(self):
        text = self.text().strip()
        parameters = VALID_EXP_NAMES + sorted(list(PARAMETER_DICT.keys()))
        if len(text) == 0:
            if self.required:
                self.report_value_required(self.key)
                return self.updateValue(None, self.saved_value)
            else:  # should we return None?
                return ''

        if self.dtype is str:
            return text
        elif self.dtype is float:
            if re_float.match(text) or re_int.match(text):
                try:
                    f = float(text)
                except ValueError as e: # Should not really happen, unless our regexes are bad
                    self.report_value_error(e)
                    return self.updateValue(None, self.saved_value)
                try:
                    self.check_range(f)
                    self.saved_value = f
                    return f
                except ValueError as e:
                    self.report_value_error(e)
                    return self.updateValue(None, self.saved_value)
            elif re_float_exp.match(text):
                try:
                    f = make_FloatExp(text)
                except ValueError as e:
                    self.report_value_error(e)
                    return self.updateValue(None, self.saved_value)
                try:
                    self.check_range(f)
                    self.saved_value = f
                    return f
                except ValueError as e:
                    self.report_value_error(e)
                    return self.updateValue(None, self.saved_value)
            elif re_math.search(text) or any(par in text for par in parameters):
                if text.startswith('@(') and text.endswith(')'):
                    text = text[2:-1]
                try:
                    eq = Equation(text)
                except ValueError as e:
                    self.report_value_error("Failed to create equation: %s" % e)
                    return self.updateValue(None, self.saved_value)
                try:
                    f = float(eq)
                except ValueError as e:
                    msg = str(e).replace('(<unknown>, line 1)', eq.eq)
                    self.report_value_error("Failed to evaluate equation: " + msg)
                    return self.updateValue(None, self.saved_value)
                try:
                    self.check_range(f)
                    self.saved_value = eq
                    return eq
                except ValueError as e:
                    self.report_value_error(e)
                    return self.updateValue(None, self.saved_value)
            else:
                return self.updateValue(None, self.saved_value)

        elif self.dtype is int:
            if re_math.search(text) or any(par in text for par in parameters):
                # integer equations
                try:
                    eq = Equation('%s' % text, dtype=int)
                except ValueError as e:
                    self.report_value_error("Failed to create equation: %s" % e)
                    return self.updateValue(None, self.saved_value)

                try:
                    i = int(eq)
                except ValueError as e:
                    msg = str(e).replace('(<unknown>, line 1)', eq.eq)
                    self.report_value_error("Failed to evaluate equation: " + msg)
                    return self.updateValue(None, self.saved_value)
                try:
                    self.check_range(i)
                    self.saved_value = eq
                    return eq
                except ValueError as e:
                    self.report_value_error(e)
                    return self.updateValue(None, self.saved_value)
            else:
                float_val = None
                int_val = None
                try:
                    float_val = float(text)
                    int_val = int(float_val)
                except ValueError as e:
                    self.report_value_error(e)
                    return self.updateValue(None, self.saved_value)
                try:
                    self.check_range(int_val)
                    self.saved_value = int_val
                    self.updateValue(None, int_val)
                    return int_val
                except ValueError as e:
                    self.report_value_error(e)
                    return self.updateValue(None, self.saved_value)


        else:
            raise TypeError(self.dtype)


    def updateValue(self, key, new_value, args=None):
        if new_value is None:
            self.setText('')
            self.saved_value = None
            return

        if new_value is not None:
            self.saved_value = new_value

        sval = str(new_value).strip()

        # Don't show @( ) for equations.  (Is this a good idea?)
        while sval.startswith("@(") and sval.endswith(")"):
            sval = sval[2:-1]

        self.setText(sval)
        self.home(False) # mark=False
        return new_value

    def default(self, val=None):
        if BaseWidget.default(self,val) is None:
            self.clear()
            self.saved_value = None
            self.text_changed_flag = False

    #completer functions
    def _insertCompletion(self, completion):
        """
        This is the event handler for the QCompleter.activated(QString) signal,
        it is called when the user selects an item in the completer popup.
        """
        # TODO FIXME this auto-completes an empty string
        text_under = self.textUnderCursor()
        cur_text = self.text()
        i = self.cursorPosition()

        beg = cur_text[:i]
        end = cur_text[i:]

        if text_under:
            beg = rreplace(beg, text_under, completion, 1)
        else:
            beg += completion
        self.setText(beg+end)
        self.setCursorPosition(len(beg))

    def textUnderCursor(self):
        text = self.text()
        textUnderCursor = ''
        i = self.cursorPosition() - 1
        while i >= 0 and text[i] not in self._separators:
            textUnderCursor = text[i] + textUnderCursor
            i -= 1
        return textUnderCursor

    def keyPressEvent(self, event):
        if not self.allow_parameters:
            QtWidgets.QLineEdit.keyPressEvent(self, event)
            return

        if self._completer.popup().isVisible():
            if event.key() in self._keysToIgnore:
                event.ignore()
                return
        else:
            self._update_completion_list()
        QtWidgets.QLineEdit.keyPressEvent(self, event)
        completionPrefix = self.textUnderCursor()
        if completionPrefix != self._completer.completionPrefix():
            self._updateCompleterPopupItems(completionPrefix)
        if event.text() and completionPrefix:
            self._completer.complete()
        if not completionPrefix:
            self._update_completion_list(allow_blank=True)
            self._updateCompleterPopupItems('')

    def _update_completion_list(self, allow_blank=False):
        comp_list = copy.deepcopy(VALID_EXP_NAMES)
        if allow_blank:
            comp_list.insert(0, '')
        for key, value in PARAMETER_DICT.items():
            if key in comp_list:
                continue
            elif self.dtype == str and isinstance(value, str):
                comp_list.append(key)
            elif self.dtype in [int, float]:
                comp_list.append(key)
        comp_list.sort()

        self._completer_model.setStringList(comp_list)
        self._completer.setModel(self._completer_model)

    def _updateCompleterPopupItems(self, completionPrefix):
        """
        Filters the completer's popup items to only show items
        with the given prefix.
        """
        self._completer.setCompletionPrefix(completionPrefix)
        self._completer.popup().setCurrentIndex(
                self._completer.completionModel().index(0, 0))

class PlainTextEdit(QtWidgets.QPlainTextEdit, BaseWidget):
    help_request = Signal() # Should we include the string?
    def __init__(self, parent=None):
        QtWidgets.QPlainTextEdit.__init__(self, parent)
        self.context_menu = self.createStandardContextMenu

    def extend_context_menu(self):
        menu = self.context_menu() # pylint: disable=not-callable
        first_default_action = menu.actions()

        if first_default_action:
            first_default_action = first_default_action[0]

        # help
        help_action = QtWidgets.QAction(
            get_icon('question_mark.svg'), 'Help', menu)
        help_action.triggered.connect(self.help_request.emit)
        insert_append_action(menu, help_action, first_default_action)

        # clear
        clear_action = QtWidgets.QAction(
            get_icon('close.svg'), 'Clear', menu)
        clear_action.triggered.connect(self.clear_text)
        insert_append_action(menu, clear_action, first_default_action)

        # save
        clear_action = QtWidgets.QAction(
            get_icon('save.svg'), 'Save', menu)
        clear_action.triggered.connect(self.save_to_file)
        insert_append_action(menu, clear_action, first_default_action)

        return menu

    def clear_text(self):
        self.clear()
        self.window().console_printer.clear_messages()

    def save_to_file(self):
        proj_dir = os.path.dirname(SETTINGS.value('project_file'))
        filename, ignore = QtWidgets.QFileDialog.getSaveFileName(self, "Save output to a file", proj_dir, "Text (*.txt)")

        if not filename:
            return

        text = self.toPlainText()
        with open(filename, 'w', encoding='utf-8', errors='replace') as txtfile:
            txtfile.write(text)

class CheckBox(QtWidgets.QCheckBox, BaseWidget):
    value_updated = Signal(object, object, object)

    def __init__(self, parent=None):
        QtWidgets.QCheckBox.__init__(self, parent)
        # stateChanged:  called on both user interaction and programmatic change
        # clicked:  user interaction only
        self.clicked.connect(self.emitUpdatedValue)
        self.context_menu = QtWidgets.QMenu
        self.dtype = bool
        self.true_value = None
        self.false_value = None

    @property
    def value(self):
        checked = self.isChecked()
        if self.true_value is not None and self.false_value is not None:
            return self.true_value if checked else self.false_value
        elif self.dtype == int:
            return int(checked)
        elif self.dtype == float:
            return float(checked)
        elif self.dtype == bool:
            return bool(checked)
        else:
            raise TypeError("Invalid dtype %s" % self.dtype)

    def updateValue(self, key, new_value, args=None):
        assert not isinstance(new_value, Keyword)  # value should not be keyword!
        if hasattr(new_value, 'lower'):
            new_value = new_value.lower()
            try:
                if 'true' in new_value:
                    v = True
                elif 'false' in new_value:
                    v = False
                else:
                    v = bool(new_value)
            except TypeError:
                v = bool(new_value)
        elif isinstance(new_value, (int, float)):
            v = True
            if new_value <= 0:
                v = False
        else:
            v = new_value
        self.setChecked(v)

    def default(self, val=None):
        if BaseWidget.default(self, val) is None:
            self.setChecked(False) #?

class GroupBox(QtWidgets.QGroupBox, CheckBox):
    value_updated = Signal(object, object, object)

    def __init__(self, parent=None):
        QtWidgets.QGroupBox.__init__(self, parent)
        self.clicked.connect(self.emitUpdatedValue)
        self.context_menu = QtWidgets.QMenu
        self.dtype = bool
        self.true_value = None
        self.false_value = None

class ComboBox(QtWidgets.QComboBox, BaseWidget):
    value_updated = Signal(object, object, object)

    def __init__(self, parent=None):
        QtWidgets.QComboBox.__init__(self, parent)
        # activated: only on user settings, not programmatic change
        self.activated.connect(self.emitUpdatedValue)
        self.dtype = str
        self.is_pop_up = False
        self.context_menu = QtWidgets.QMenu
        self.wheelEvent = lambda ev: None # Allow mouse wheel scrolling, don't disrupt ComboBox
    @property
    def value(self):
        sval = str(self.currentText())
        if self.dtype == int: # Labeled string entries, just return the number
            return int(sval.split('-')[0].strip())
        elif self.dtype == bool:
            return sval.lower() == 'true'
        return sval

    def updateValue(self, key, new_value, args=None):
        assert not isinstance(new_value, Keyword) # value should not be kw
        if isinstance(new_value, int):
            self.setCurrentIndex(new_value)
        else:
            self.setCurrentText(new_value)

    def setCurrentText(self, new_value):
        for itm in range(self.count()):
            if self.dtype == str and str(new_value).lower() == str(self.itemText(itm)).lower():
                self.setCurrentIndex(itm)
                break
            elif self.dtype == int and int(new_value) == int(str(self.itemText(itm)).split('-')[0].strip()):
                self.setCurrentIndex(itm)
                break
        else:
            raise ValueError(new_value)

    def default(self, val=None):
        if BaseWidget.default(self,val) is None:
            self.setCurrentIndex(0) # ?  Is this a good idea? see issues/860

class SpinBox(QtWidgets.QSpinBox, BaseWidget):
    value_updated = Signal(object, object, object)

    def __init__(self, parent=None):
        QtWidgets.QDoubleSpinBox.__init__(self, parent)
        # Would be nice to distinguish user input from programmatic setting
        self.valueChanged.connect(self.emitUpdatedValue)
        self.dtype = int
        self.context_menu = QtWidgets.QMenu

    def emitUpdatedValue(self): # calls self.value() instead of using self.value
        self.value_updated.emit(self, {self.key: self.value()}, self.args)

    def updateValue(self, key, new_value, args=None):
        assert not isinstance(new_value, Keyword)
        self.setValue(int(new_value))

    def setValInfo(self, max=None, min=None):
        BaseWidget.setValInfo(self, max=max, min=min)
        if max is not None:
            self.setMaximum(int(max))
        if min is not None:
            self.setMinimum(int(min))

    def default(self, val=None):
        if BaseWidget.default(self,val) is None:
            self.setValue(0) #?

class DoubleSpinBox(QtWidgets.QDoubleSpinBox, BaseWidget):
    value_updated = Signal(object, object, object)

    def __init__(self, parent=None):
        QtWidgets.QDoubleSpinBox.__init__(self, parent)
        # Distinguish user input from programmatic setting
        #self.valueChanged.connect(self.emitUpdatedValue)
        self.editingFinished.connect(self.emitUpdatedValue)
        self.dtype = float
        self.context_menu = QtWidgets.QMenu

    def textFromValue(self, value):
        ret = repr(value)
        return ret

    def emitUpdatedValue(self):
        self.value_updated.emit(self, {self.key: self.value()}, self.args)

    def updateValue(self, key, new_value, args=None):
        assert not isinstance(new_value, Keyword)
        self.setValue(float(new_value))

    def setValInfo(self, max=None, min=None):
        BaseWidget.setValInfo(self, max=max, min=min)
        if max:
            self.setMaximum(float(max))
        if min:
            self.setMinimum(float(min))

    def default(self, val=None):
        if BaseWidget.default(self,val) is None:
            self.setValue(0.0) #?

class ListWidget(QtWidgets.QListWidget, BaseWidget):
    value_updated = Signal(object, object, object)
    def __init__(self, parent=None):
        QtWidgets.QListWidget.__init__(self, parent)
        self.context_menu = QtWidgets.QMenu
        self.dtype = list
        self.itemClicked.connect(self.emitUpdatedValue)

    def emitUpdatedValue(self):
        self.value_updated.emit(self, {self.key: self.value}, self.args)

    @property
    def value(self):
        selected = []
        for i in range(self.count()):
            item = self.item(i)
            if item.checkState() == Qt.Checked:
                selected.append(item.text())
        return selected

    def updateValue(self, key, new_value, args=None):
        if new_value is None:
            return
        elif isinstance(new_value, (list, tuple)):
            for i in range(self.count()):
                item = self.item(i)
                if item.text() in new_value:
                    item.setCheckState(Qt.Checked)
                else:
                    item.setCheckState(Qt.Unchecked)

    def add_items(self, l):
        for i in l:
            item = QtWidgets.QListWidgetItem(i, self)
            item.setFlags(item.flags() | Qt.ItemIsUserCheckable)
            item.setCheckState(Qt.Unchecked)
            self.addItem(item)

# --- Table ---
class Table(QtWidgets.QTableView, BaseWidget):
    """A table view with built in dictionary and array table models. Custom
    delegates are also provided.

    Parameters
    ----------
    parent (QObject):
        parent of the widget (default None)
    dtype (type):
        the type of data, should be either `dict` for displaying
        `dict(dict())`, `list` for displaying `list(list())`, 2D
        `numpy.array`, or Pandas `DataFrame` (default None)
    columns (list):
        a list of column names, if `None`, hides column names (default [])
    rows (list):
        a list of row names, if `None`, hides row names  (default [])
    column_delegate (dict):
        a dictionary describing delegates for editing cells, column wise
        (default {})
    row_delegate (dict):
        a dictionary describing delegates for editing cells, row wise
        (default {})
    selection_behavior (str):
        a string describing the selection behavior. Either 'row', 'col', or
        'cell' for row selection, column selection, or single cell selection,
        respectively (default 'cell')
    multi_selection (bool):
        Either single selection (False), or multiple selections (True)
        (default False)

    Signals
    -------
    value_changed:
        emits the current value every time the value is changed
    lost_focus:
        emits when the widget has lost focus
    new_selection:
        emits the from and to indices of a selection change. """

    value_changed = Signal(object, object, object)
    lost_focus = Signal(object)
    new_selection = Signal(object, object)

    def __init__(self, parent=None, dtype=None, columns=[], rows=[],
                 column_delegate={}, row_delegate={}, selection_behavior='row',
                 multi_selection=False):

        QtWidgets.QTableView.__init__(self, parent)

        self.dtype = dtype
        self.columns = columns
        self.rows = rows
        self.block_selection_change_event = False
        self.selection = []
        self.mouse_pos = None
        self.proxy_model = None
        self._setModel()
        self.set_delegate(col=column_delegate, row=row_delegate)
        self.set_selection_model(selection_behavior, multi_selection)
        if columns is None:
            self.horizontalHeader().hide()
        if rows is None:
            self.verticalHeader().hide()

        # build context menu
        self.menu = QtWidgets.QMenu(self)
        # applyAction = QtWidgets.QAction('Apply to Column', self)
        # applyAction.triggered.connect(self.apply_val_to_column)
        # self.menu.addAction(applyAction)

    def _setModel(self):

        # remove old model
        oldModel = self.model()
        if oldModel:
            oldModel.deleteLater()

        # Setup model
        if self.dtype in (dict, OrderedDict):
            model = DictTableModel(columns=self.columns, rows=self.rows)
        elif self.dtype in (list, tuple,
                            np.ndarray if np else None):
            model = ArrayTableModel(columns=self.columns, rows=self.rows)
        else:
            model = None

        if model is not None:

            if self.isSortingEnabled():
                self.proxy_model = CustomSortFilterProxyModel()
                self.proxy_model.setSourceModel(model)
                QtWidgets.QTableView.setModel(self,  self.proxy_model)
                self.model().sourceModel().value_updated.connect(self.value_changed.emit)

            else:
                QtWidgets.QTableView.setModel(self, model)
                # self.model().modelReset.connect(self.hideRows)
                self.model().value_updated.connect(self.value_changed.emit)
                self.model().modelAboutToBeReset.connect(self.save_selection)

            # Need a reference or it segfaults with pyside
            # http://stackoverflow.com/questions/19211430/pyside-segfault-when-using-qitemselectionmodel-with-qlistview
            selectModel = self.selectionModel()
            selectModel.selectionChanged.connect(self.selection_changed_event)

    @property
    def get_model(self):
        if self.isSortingEnabled():
            m = self.model().sourceModel()
        else:
            m = self.model()
        return m

    def set_selection_model(self, behavior='row', multi=False):
        " set the selection model "

        if behavior == 'col' or behavior == 'column':
            self.setSelectionBehavior(
                QtWidgets.QAbstractItemView.SelectColumns)
        elif behavior == 'row':
            self.setSelectionBehavior(QtWidgets.QAbstractItemView.SelectRows)
        else:
            self.setSelectionBehavior(QtWidgets.QAbstractItemView.SelectItems)

        if multi:
            self.setSelectionMode(QtWidgets.QAbstractItemView.ExtendedSelection)
        elif behavior is None or behavior.lower() == 'none':
            self.setSelectionMode(QtWidgets.QAbstractItemView.NoSelection)
        else:
            self.setSelectionMode(QtWidgets.QAbstractItemView.SingleSelection)

        if self.selectionModel():
            selectionmodel = self.selectionModel()
            selectionmodel.selectionChanged.connect(
                self.selection_changed_event)

    def selection_changed_event(self):
        selection = self.selectionModel().selection().indexes()
        if not self.block_selection_change_event:
            if selection:
                # from
                from_ = selection[0]
                # to
                to = selection[-1]
                self.new_selection.emit([from_.row(), from_.column()],
                                        [to.row(), to.column()])
            else:
                self.new_selection.emit(None, None)

    def clear_selection(self):
        sel_model = self.selectionModel()
        sel_model.clearSelection()

    @property
    def value(self):
        if self.get_model:
            return self.get_model.datatable
        else:
            return None

    def set_value(self, value, block=None):
        if self.dtype != type(value):
            # FIXME, this is a problem if you try to mix dict and OrderedDict
            raise TypeError('Selected table model does not support type %s' % type(value))

        if self.get_model is not None:
            self.get_model.update(value, block)

        # reset the selection
        self.block_selection_change_event = True
        select_model = self.selectionModel()
        for selection in self.selection:
            select_model.setCurrentIndex(
                selection,
                QItemSelectionModel.Select)
        self.block_selection_change_event = False

    def set_columns(self, cols):
        self.columns = cols
        self.get_model._columns = cols

        if cols is None:
            self.horizontalHeader().hide()
        else:
            self.horizontalHeader().show()

    def set_rows(self, rows):
        self.rows = rows
        self.get_model._rows = rows

        if rows is None:
            self.verticalHeader().hide()
        else:
            self.verticalHeader().show()

    @property
    def column_colors(self):
        return self.delegate.column_color_dict

    @column_colors.setter
    def column_colors(self, color_dict):
        self.delegate.column_color_dict = color_dict

    @property
    def row_colors(self):
        return self.delegate.row_color_dict

    @row_colors.setter
    def row_colors(self, color_dict):
        self.delegate.row_color_dict = color_dict

    def auto_update_rows(self, b):
        self.get_model.update_rows = b

    def show_horizontal_header(self, b):
        if b:
            self.horizontalHeader().show()
        else:
            self.horizontalHeader().hide()

    def show_vertical_header(self, b):
        if b:
            self.verticalHeader().show()
        else:
            self.verticalHeader().hide()

    def set_delegate(self, col, row):
        self.delegate = CustomDelegate(column_dict=col,
                                       row_dict=row)
        self.setItemDelegate(self.delegate)

    def fit_to_contents(self):
        self.resizeColumnsToContents()
        self.horizontalHeader().setStretchLastSection(True)
        self.resizeRowsToContents()

    def save_selection(self):
        self.selection = self.selectionModel().selection().indexes()

    def contextMenuEvent(self, event):
        """Qt context menu over-ride"""
        self.mouse_pos = QtGui.QCursor.pos()
        # popup context menu
        self.menu.popup(self.mouse_pos)

    def get_clicked_cell(self):
        """get the cell of the contectMenuEvent, return row, col"""
        local_pos = self.viewport().mapFromGlobal(self.mouse_pos)
        return (self.rowAt(local_pos.y()), self.columnAt(local_pos.x()))

    def apply_val_to_column(self):
        row, column  = self.get_clicked_cell()
        value = self.get_model.data(col=column, row=row, role=Qt.EditRole)
        self.get_model.apply_to_column(column, value)

    def current_rows(self):
        i = self.selectionModel().selectedRows()

        if self.proxy_model is not None:
            i = [self.proxy_model.mapToSource(ind) for ind in i]
        if i:
            return [ind.row() for ind in i]
        else:
            return []

    def current_columns(self):
        i = self.selectionModel().selectedColumns()
        if i:
            return [ind.column() for ind in i]
        else:
            return []

    def clear(self):
        self.get_model.update({}) # TODO: change based on dtype?

    def default(self):
        '''if there is a default value, set it, else clear'''
        if self.default_value is not None:
            self.get_model.update(copy.deepcopy(self.default_value))
        else:
            self.clear()

    def setSortingEnabled(self, bool):
        QtWidgets.QTableView.setSortingEnabled(self, bool)
        self._setModel()
        self.horizontalHeader().sortIndicatorChanged.connect(self.sortChanged)
        # initialize the table to display in its unsorted form first
        self.horizontalHeader().setSortIndicator(-1, QtCore.Qt.AscendingOrder)

    def sortChanged(self):
        if self.get_model is not None and self.get_model.rowCount():
            self.columnSorted = self.horizontalHeader().sortIndicatorSection()
            self.sortOrder = self.horizontalHeader().sortIndicatorOrder()

    def resetSort(self):
        if self.proxy_model is not None:
            self.proxy_model.sort(-1, QtCore.Qt.AscendingOrder)
            self.horizontalHeader().setSortIndicator(-1, QtCore.Qt.AscendingOrder)


class CustomSortFilterProxyModel(QtCore.QSortFilterProxyModel):
    def __init__(self):
        QtCore.QSortFilterProxyModel.__init__(self)

    def lessThan(self, left, right):
        model = self.sourceModel()
        left = model.data(left, QtCore.Qt.EditRole)
        right = model.data(right, QtCore.Qt.EditRole)
        if left is None:
            if isinstance(right, (int, float)):
                left = float('inf')
            elif isinstance(right, (str)):
                left = ''
        if right is None:
            if isinstance(left, (int, float)):
                right = float('inf')
            elif isinstance(left, (str)):
                right = ''
        try:
            return left < right
        except:
            return False


class CustomDelegate(QtWidgets.QStyledItemDelegate):
    def __init__(self, column_dict={}, row_dict={}, column_color_dict={},
                 row_color_dict={}):
        QtWidgets.QStyledItemDelegate.__init__(self)

        self.column_dict = column_dict
        self.row_dict = row_dict
        self.row_color_dict = row_color_dict
        self.column_color_dict = column_color_dict

    def set_column_widgets(self, column_dict):
        self.column_dict.update(column_dict)

    def set_row_widgets(self, row_dict):
        self.row_dict.update(row_dict)

    def createEditor(self, parent, option, index):
        if self.column_dict and index.column() in self.column_dict:
            widgetData = self.column_dict[index.column()]
        elif self.row_dict and index.row() in self.row_dict:
            widgetData = self.row_dict[index.row()]
        else:
            widgetData = {'widget': 'lineedit'}

        editor = None

        if widgetData['widget'] == 'combobox':
            editor = ComboBox(parent)
            if 'items' in widgetData:
                editor.addItems(widgetData['items'])
        elif widgetData['widget'] == 'checkbox':
            editor = CheckBox(parent)
        elif widgetData['widget'] == 'lineedit':
            editor = LineEdit(parent)
        elif widgetData['widget'] == 'spinbox':
            editor = SpinBox(parent)
        elif widgetData['widget'] == 'doublespinbox':
            editor = DoubleSpinBox(parent)

        if editor:
            if 'dtype' in widgetData:
                editor.setdtype(widgetData['dtype'])

            max = widgetData.get('max')
            min = widgetData.get('min')

            if hasattr(editor, 'setRange'):
                editor.setRange(min, max)

        return editor

    def setEditorData(self, widget, index):
        index.model().blockUpdate = True
        value = index.model().data(index, Qt.EditRole)

        if widget.dtype is None:
            widget.dtype = type(value)
        widget.updateValue(None, value)

    def setModelData(self, widget, model, index):
        model.setData(index, widget.dtype(widget.value), Qt.EditRole)
        model.blockUpdate = False

    def updateEditorGeometry(self, editor, option, index):
        editor.setGeometry(option.rect)
        # What is this for?

    def eventFilter(self, widget, event):
        if isinstance(widget, ComboBox):
            # print(event, event.type())
            # TODO: there is a bug here that doesn't return focus to the
            # tableview
            if event.type() == QtCore.QEvent.FocusOut:
                pop = widget.is_pop_up
                widget.is_pop_up = True  # When does this get cleared?
                return pop

            else:
                return QtWidgets.QStyledItemDelegate.eventFilter(
                    self, widget, event)

        else: # ???
            return QtWidgets.QStyledItemDelegate.eventFilter(
                self, widget, event)

    def paint(self, painter, option, index):

        if index.column() in self.column_dict and self.column_dict[index.column()]['widget'] == 'progressbar':

            progress = int(index.model().data(index, Qt.EditRole))

            progressbar = QtWidgets.QStyleOptionProgressBar()
            progressbar.rect = option.rect
            progressbar.minimum = 0
            progressbar.maximum = 100
            progressbar.progress = progress
            progressbar.text = '{}%'.format(progress)
            progressbar.textVisible = True
            progressbar.textAlignment = Qt.AlignCenter

            QtWidgets.QApplication.style().drawControl(
                QtWidgets.QStyle.CE_ProgressBar, progressbar, painter)
        else:
            QtWidgets.QStyledItemDelegate.paint(self, painter, option, index)

class DictTableModel(QtCore.QAbstractTableModel):
    """Table model that handles dict(dict()).

    Parameters
    ----------
    columns (list):
        a list of column names (used as dict index), suggested but not
        required. if not provided, defaults to keys of the dictionary
        (default [])
    rows (list)
        a list of row names (used as dict index), suggested but not required.
        if not provided, defaults to keys of the dictionary (default [])
    parent (QObject):
        parent of the model (default None) """

    value_updated = Signal(object, object, object)

    def __init__(self, columns=[], rows=[], parent=None, ):
        QtCore.QAbstractTableModel.__init__(self, parent)
        self.datatable = {}
        self._columns = columns
        self._rows = rows
        self.blockUpdate = False
        self.update_rows = False

    def update(self, data, block=None):
        if block is not None:
            self.blockUpdate = block

        if not self.blockUpdate and data is not None:
            self.beginResetModel()
            self.datatable = data
            self.endResetModel()

            if not self._columns:
                self._columns = []
                for value in self.datatable.values():
                    if len(value) > len(self._columns):
                        self._columns = value.keys()

            if not self._rows or self.update_rows:
                self._rows = list(self.datatable.keys())

    def setData(self, index=None, value=None, role=Qt.EditRole,
                row=0, col=0):
        if role == Qt.EditRole:
            if index is not None:
                row = index.row()
                col = index.column()

            if self._columns:
                col = self._columns[col]
            if self._rows:
                row = self._rows[row]

            if row not in self.datatable:
                self.datatable[row] = {}

            self.datatable[row][col] = value
            self.value_updated.emit(row, col, value)

    def apply_to_column(self, col, val):
        for i in range(self.rowCount()):
            self.setData(col=col, row=i,
                         value=copy.deepcopy(val))

    def rowCount(self, parent=QtCore.QModelIndex()):
        'return the row count'
        if self.datatable.keys():
            return len(self.datatable.keys())
        else:
            return 0

    def columnCount(self, parent=QtCore.QModelIndex()):
        'return the column count'

        if self._columns:
            cols = len(self._columns)
        else:
            cols = 0
            for value in self.datatable.values():
                cols = max(cols, len(value))
        return cols

    def data(self, index=None, role=Qt.DisplayRole, row=0, col=0):
        if index is not None:
            row = index.row()
            col = index.column()

        if self._columns:
            col = self._columns[col]
        if self._rows:
            row = self._rows[row]

        if row in self.datatable and col in self.datatable[row]:
            value = self.datatable[row][col]
        else:
            value = None

        if role == Qt.DisplayRole:
            if value is None:
                value = None
            elif isinstance(value, QtGui.QPixmap):
                value = None
            else:
                value = str(value)
            return value
        elif role == Qt.EditRole:
            return value
        elif role == Qt.BackgroundRole and hasattr(value, 'qcolor'):
            return value.qcolor
        elif role == Qt.DecorationRole and isinstance(value, QtGui.QPixmap):
            return value
        else:
            return None

    def headerData(self, section, orientation, role=Qt.DisplayRole):
        if role == Qt.DisplayRole:
            if orientation == Qt.Horizontal:
                if len(self._columns) > section:
                    return self._columns[section].capitalize()
                else:
                    return str(section).capitalize()

            elif orientation == Qt.Vertical:
                if self._rows and len(self._rows) > section:
                    return self._rows[section]
                else:
                    return section
        else:
            return None

    def flags(self, index):
        return Qt.ItemIsEnabled | Qt.ItemIsEditable | Qt.ItemIsSelectable

class ArrayTableModel(QtCore.QAbstractTableModel):
    """Table model that handles the following data types:
        - list()
        - tuple()
        - list(list())
        - tuple(tuple())
        - list(tuple())
        - tuple(list())
        - 2D numpy.ndarray """

    value_updated = Signal(object)

    def __init__(self, columns=[], rows=[], parent=None, ):
        QtCore.QAbstractTableModel.__init__(self, parent)
        self.datatable = []
        self._columns = columns
        self._rows = rows
        self.blockUpdate = False

    def update(self, data, block=None):
        if block is not None:
            self.blockUpdate = block
        if not self.blockUpdate:
            self.beginResetModel()
            self.datatable = data
            self.endResetModel()

    def setData(self, index=None, value=None, role=Qt.EditRole, col=0, row=0):
        if role == Qt.EditRole:
            if index is not None:
                row = index.row()
                col = index.column()

            self.datatable[row][col] = value

            self.value_updated.emit(self.datatable)

    def apply_to_column(self, col, val):
        for i in range(self.rowCount()):
            self.setData(col=col, row=i,
                         value=copy.deepcopy(val))

    def rowCount(self, parent=QtCore.QModelIndex()):
        return len(self.datatable)

    def columnCount(self, parent=QtCore.QModelIndex()):
        if (isinstance(self.datatable, (list, tuple)) and
                len(self.datatable) > 0):
            if isinstance(self.datatable[0], (list, tuple)):
                return len(self.datatable[0])
            else:
                return 1

        elif np and isinstance(self.datatable, np.ndarray):
            return int(self.datatable.shape[1])

        else:
            return 0

    def data(self, index=None, role=Qt.DisplayRole, row=0, col=0):
        if index is not None:
            row = index.row()
            col = index.column()

        value = self.datatable[row][col]

        if role == Qt.DisplayRole:
            if value is None:
                value = None
            else:
                value = str(value)
            return value
        elif role == Qt.EditRole:
            return value
        elif role == Qt.BackgroundRole and hasattr(value, 'qcolor'):
            return value.qcolor
        elif role == Qt.DecorationRole and isinstance(value, QtGui.QPixmap):
            return value
        else:
            return None

    def headerData(self, section, orientation, role=Qt.DisplayRole):
        if role == Qt.DisplayRole:
            if orientation == Qt.Horizontal:
                if self._columns and len(self._columns) > section:
                    return self._columns[section].capitalize()
                else:
                    return section.capitalize()

            elif orientation == Qt.Vertical:
                if self._rows and len(self._rows) > section:
                    return self._rows[section]
                else:
                    return section
        else:
            return None

    def flags(self, index):
        return Qt.ItemIsEnabled | Qt.ItemIsEditable | Qt.ItemIsSelectable


BASE_WIDGETS = {
    'lineedit': LineEdit,
    'combobox': ComboBox,
    'checkbox': CheckBox,
    'groupbox': GroupBox,
    'spinbox': SpinBox,
    'doublespinbox': DoubleSpinBox,
    'plaintextedit': PlainTextEdit,
    'listwidget': ListWidget,
}
