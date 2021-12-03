#!/usr/bin/env python

import os
import sys
import signal

from math import pi as 𝜋

from qtpy.QtWidgets import QApplication, QDialog, QFileDialog

from qtpy.QtGui import  QDoubleValidator, QValidator
from qtpy.QtCore import Signal

from mfixgui.tools.qt import (get_combobox_item, get_selected_row,
                              get_selected_rows, set_item_noedit, get_ui)
from mfixgui.widgets.base import LineEdit
from mfixgui.tools.util import safe_float

try:
    from matplotlib import pyplot as plt
    from matplotlib.patches import Patch
    plt.rcParams['figure.constrained_layout.use'] = True
    #https://matplotlib.org/tutorials/intermediate/constrainedlayout_guide.html
    import numpy as np

except ImportError:
    plt = None
    np = None

def load_custom_psd(filename):
    # Returns (X, PDF, CDF, type 'PDF'/'CDF', mean, sigma)
    # or raises exception
    a = np.loadtxt(filename, skiprows=3)
    X = a.T[0]
    Y = a.T[1]
    npts = len(X)
    #x_range= [X[0], X[-1]]
    type_ = None
    with open(filename, 'r') as f:
        count = f.readline() # Count, ignored for now
        type_ = f.readline().strip() # Second line of file
        if type_ not in ('CDF', 'PDF'):
            raise ValueError("Invalid type '%s', must be CDF or PDF" % type_)

    dX = np.diff(np.hstack(([0],X)))

    if type_ == 'PDF':
        pdf = Y
        cdf = np.cumsum(Y * dX)  # ∫y dx
    else:
        cdf = Y
        pdf = np.hstack(([Y[0]/X[0]], np.diff(Y)/np.diff(X)))  # dy/dx
    µ = np.sum(pdf*X*dX) / np.sum(pdf*dX)       # ∫y dx
    𝜎 = np.sqrt(np.sum( (X-µ)**2 * pdf * dX))   # √∫ (X-µ)² Y dx

    return X, pdf, cdf, type_, µ, 𝜎


class PSDPopup(QDialog):

    save = Signal()
    cancel = Signal()

    def set_ok_button(self, state, msg=''):
        self.ui.pushbutton_ok.setEnabled(state)
        self.ui.label_status.setText(msg)
        self.ui.pushbutton_plot.setEnabled(state
                                           and self.ui.lineedit_plot_range_min.value is not None
                                           and self.ui.lineedit_plot_range_max.value is not None)

    def reset_signals(self):
        for sig in (self.cancel, self.save):
            try:
                sig.disconnect()
            except:
                pass



    def __init__(self, parent=None):
        super(PSDPopup, self).__init__(parent)
        self.parent = parent
        ui = self.ui = get_ui('psd_popup.ui', self)
        self.reserved_aliases = set()
        self.filename = None
        self.editing = False
        self.custom_data = None
        if parent:
            LineEdit.report_value_error = parent.popup_value_error
            LineEdit.report_value_required = parent.popup_value_required

        class AliasValidator(QValidator):

            def __init__(self, parent=None):
                super(AliasValidator, self).__init__()
                self.parent = parent

            def validate(self, text, pos):
                # 'parent' here is the popup, not the mfix gui
                self.parent.update_buttons()
                if text in self.parent.reserved_aliases:
                    self.parent.set_ok_button(False, "Names must be unique")
                    return (QValidator.Intermediate, text, pos)
                if text == "":
                    self.parent.set_ok_button(False, "Name is required")
                    return (QValidator.Intermediate, text, pos)
                if text.startswith(' '):
                    return (QValidator.Invalid, text, pos)
                return (QValidator.Acceptable, text, pos)

        lineedit = ui.lineedit_alias
        lineedit.setValidator(AliasValidator(parent=self))

        le = ui.lineedit_plot_range_min
        le.saved_value = le.default_value = 0.0
        le.required = True
        le.dtype = float
        le.key = 'plot range min'
        le.allow_parameters = True
        le.updateValue(le.key, le.default_value)
        le.editingFinished.connect(self.update_buttons)

        le = ui.lineedit_plot_range_max
        le.saved_value = le.default_value = 0.1
        le.required = True
        le.dtype = float
        le.key = 'plot range max'
        le.allow_parameters = True
        le.updateValue(le.key, le.default_value)
        le.editingFinished.connect(self.update_buttons)

        for le in (ui.lineedit_mean,
                   ui.lineedit_sigma,
                   ui.lineedit_min,
                   ui.lineedit_max):
            le.min = 0
            le.dtype = float
            le.exclude_min = True
            le.allow_parameters = True
            le.editingFinished.connect(self.update_buttons)

        ui.combobox_type.activated.connect(self.handle_type)

        # File selector for custom PSD
        ui.pushbutton_select_file.clicked.connect(self.select_file)
        ui.pushbutton_show_help.clicked.connect(self.show_help)

        # plot button
        b = ui.pushbutton_plot
        if plt is not None:
            b.clicked.connect(self.plot_psd)
        else:
            b.setEnabled(False)
            b.setToolTip("Matplotlib not available")

        self.set_ok_button(False) # nothing to accept
        ui.pushbutton_plot.setEnabled(False) # nothing to plot
        ui.pushbutton_ok.clicked.connect(lambda: (self.save.emit(), self.close()))
        ui.pushbutton_cancel.clicked.connect(lambda: (self.cancel.emit(), self.close()))

    def show_help(self):
        msg ='''\
<pre><i>Custom PSD file format example:</i>

51                     <i>Number of points in PSD</i>
CDF                    <i>Specify 'PDF' or 'CDF'</i>
!Diameter  CDF         <i>Header line</i>
2.9156e-04 0.0000e+00  <i>data</i>
3.5244e-04 3.0000e-04  <i>data</i>
...                    <i>additional data lines</i>
</pre>'''
        if self.parent:
            self.parent.message(title='File format info', icon='info', text=msg,
                                print_console=False)
        else:
            print(msg)


    def handle_type(self, idx):
        ui = self.ui
        custom = (idx == 2)
        ui.stacked_widget.setCurrentIndex(1 if custom else 0)
        if custom:
            ui.checkbox_copy_file.setHidden(True) # Will get shown if needed when file chosen
            self.label_filename.setText(self.filename or '')
        ui.label_plot_range.setHidden(custom)
        ui.label_plot_range_to.setHidden(custom)
        ui.lineedit_plot_range_min.setHidden(custom)
        ui.lineedit_plot_range_max.setHidden(custom)
        ui.label_plot_range_units.setHidden(custom)

        self.update_buttons()

    def select_file(self):
        ui = self.ui
        fname = QFileDialog.getOpenFileName(self)[0]
        if fname and os.path.isabs(fname) and os.path.dirname(fname)==os.getcwd():
            fname = os.path.basename(fname)
        self.filename = fname
        self.custom_data = None
        self.label_filename.setText(fname or '')
        if fname:
            try:
                self.custom_data = load_custom_psd(fname)
            except Exception as e:
                msg = str(e)
                if self.parent:
                    self.parent.error(msg, popup=True)
                else:
                    print(msg)
                self.set_ok_button(False)
                return

        if (fname
            and os.path.isabs(fname)
            and os.path.dirname(fname) != os.getcwd()): # Not in current directory
            self.checkbox_copy_file.setHidden(False)
            self.checkbox_copy_file.setChecked(True)
        else:
            self.checkbox_copy_file.setHidden(True)
        self.update_buttons()

    def clear_inputs(self):
        ui = self.ui
        ui.combobox_type.setCurrentIndex(0)
        self.filename = None
        self.handle_type(0)
        for w in (ui.lineedit_alias,
                  ui.lineedit_mean, ui.lineedit_sigma,
                  ui.lineedit_min, ui.lineedit_max,
                  ui.label_filename):
            w.setText('')
        self.update_buttons()




    def update_buttons(self):
        ui = self.ui
        name = ui.lineedit_alias.text().strip()
        if name in self.reserved_aliases:
            self.set_ok_button(False, "Names must be unique")
            return
        if ui.combobox_type.currentIndex() == 2: # Custom
            ok = (self.filename is not None
                  and ui.lineedit_alias.text())
        else:
            ok = all(le.text().strip() for le in(ui.lineedit_alias,
                                                 ui.lineedit_mean,
                                                 ui.lineedit_sigma))
            # optional ui.lineedit_min, ui.lineedit_max
        if ok:
               self.set_ok_button(True, "Definition valid")
        else:
               self.set_ok_button(False)


    def popup(self):
        self.show()
        self.raise_()
        self.activateWindow()
        #self.ui.combobox_type.setCurrentIndex(0)
        self.handle_type(0)
        self.ui.lineedit_alias.selectAll()
        self.ui.lineedit_alias.setFocus(0)


    def plot_psd(self):
        ui = self.ui
        npts = 1000
        custom = False
        type_ = None

        alias = ui.lineedit_alias.text().strip()
        𝜎 = safe_float(ui.lineedit_sigma.value)
        µ = µ0 = safe_float(ui.lineedit_mean.value, default=None)
        mn = safe_float(ui.lineedit_min.value, default=None)
        mx = safe_float(ui.lineedit_max.value, default=None)
        psd_type = ui.combobox_type.currentIndex()

        if psd_type in (0,1):
            x_range = (safe_float(ui.lineedit_plot_range_min.value),
                       safe_float(ui.lineedit_plot_range_max.value))
            x_range = min(x_range), max(x_range)

        if psd_type == 0: # Normal
            X = np.linspace(x_range[0], x_range[1], npts)
            pdf = 1 /(𝜎*np.sqrt(2*𝜋)) * np.exp(-1/2 * ((X-µ)/𝜎)**2)

        elif psd_type == 1: # Log-normal
            X = np.linspace(x_range[0], x_range[1], npts)
            X[0] = X[1] # Avoid log(0)
            µ, 𝜎  = (np.log(µ**2/np.sqrt(µ**2 +  𝜎**2)),
                     np.sqrt(np.log(1.0 + (𝜎/µ)**2)))
            pdf = 1/(X*𝜎*np.sqrt(2*𝜋)) * np.exp(-1/2 * ((np.log(X)-µ)/𝜎)**2)
            µ_star = np.exp(µ)
            𝜎_star = np.exp(𝜎)
            mode = np.exp(µ - 𝜎**2)
            # Variance is just square of std.dev, why bother?
            #var =  (np.exp(𝜎**2) - 1) * np.exp(2*µ + 𝜎**2)
            #var = np.exp(2*µ + 2 * 𝜎**2) - np.exp(2*µ + 𝜎**2)
            skew = (np.exp(𝜎**2) + 2) * np.sqrt(np.exp(𝜎**2) - 1)

        if psd_type in (0,1):
            # cdf = 0.5*(1+erf((x-µ)/(𝜎*sqrt(2))))
            # no erf in numpy and I don't want to pull in scipy
            cdf = np.cumsum(pdf) * (x_range[1]-x_range[0]) / npts

        else: # Custom
            custom = True
            fname = self.filename
            x_range = None
            if not fname:
                self.update_buttons()
                return
            if not self.custom_data:
                try:
                    self.custom_data = load_custom_psd(fname)
                    self.set_ok_button(True)
                except Exception as e:
                    msg = str(e)
                    if self.parent:
                        self.parent.error(msg, popup=True)
                    else:
                        print(msg)
                    self.set_ok_button(False)
                    return
            X, pdf, cdf, type_, µ, 𝜎 = self.custom_data
            µ0 = µ
        plt.ion()
        plt.clf()

        #plt.grid(True) # Can we put this under user control?
        fig = plt.figure(1, constrained_layout=True)
        grid = fig.add_gridspec(2,1)
        axs = fig.add_subplot(grid[0]), fig.add_subplot(grid[1])

        fig.suptitle(alias, fontsize=16)

        patches = None

        # Plot the PDF
        ax = axs[0]
        label = 'Probability density (PDF)'
        if custom and type_ == 'CDF':
            label += ' (computed)'
        ax.set_title(label)
        ax.plot(X, pdf)
        ax.set_xlabel('Size (m)')
        ax.set_ylabel('Probability/m')
        if x_range:
            ax.set_xlim(left=x_range[0], right=x_range[1])
        ax.set_ylim(bottom=0.0)
        ax.axvline(x=µ0, linestyle='-', label='mean', color='blue')
        if not custom:
            if psd_type == 1: # log-normal
                ax.axvline(x=µ_star, linestyle='-.', label='median', color='green')
                ax.axvline(x=mode, linestyle='-.', label='mode', color='orange')

            if mn is not None:
                ax.axvline(x=mn, linestyle='--', label='min', color='black')
            if mx is not None:
                ax.axvline(x=mx, linestyle='--', label='max', color='red')


            ## Confidence bands
            patches = []
            for (l, c, n) in (('99.7%', (0.9,0.9,0.9), 3),
                              ('95%',   (0.8,0.8,0.8), 2),
                              ('68%',   (0.7,0.7,0.7), 1)):
                patches.append(Patch(color=c, label=l))
                if psd_type == 0:
                    lo, hi = µ - n*𝜎,  µ + n*𝜎
                else: # log-normal
                    lo, hi = µ_star / (𝜎_star**n),  µ_star * (𝜎_star**n)

                # use data coords for x axis, axis coords for y, so '1' is ymax
                #https://matplotlib.org/gallery/lines_bars_and_markers/fill_between_demo.html
                ax.fill_between(X, 0, 1,
                                where = np.logical_and(X>=lo, X<=hi),
                                color=c,
                                transform=ax.get_xaxis_transform())
            patches.reverse()


        handles, labels = ax.get_legend_handles_labels()
        for h in handles:
            label = h.get_label()
            val = None
            if label == 'mean':
                val = µ0
            elif label == 'median':
                val = µ_star
            elif label == 'mode':
                val = mode
            elif label == 'min':
                val = mn
            elif label == 'max':
                val = mx
            if val is not None:
                val = round(val, 4)
                h.set_label('%s: %s' % (label, val))

        # Abuse some patches to get text into the legend. Is there a better way?
        if psd_type == 1: # Log-normal
            # Variance is just square of std. dev so why show it?
            #handles.append(Patch(color='white', label='variance: %s' % round(var, 4)))
            handles.insert(3,(Patch(color='white', label='skewness: %s' % round(skew, 4))))

        legend = ax.legend(handles=handles, loc='upper left')
        ax.add_artist(legend)

        if patches:
            ax.legend(handles=patches, loc='upper right')

        # Plot the CDF
        ax = axs[1]
        label = 'Cumulative distribution (CDF)'
        if custom and type_ == 'PDF':
            label += ' (computed)'
        ax.set_title(label)
        ax.plot(X, cdf)
        ax.set_xlabel('Size (m)')
        ax.set_ylabel('Probability')
        if x_range:
            ax.set_xlim(left=x_range[0], right=x_range[1])
        ax.set_ylim(bottom=0.0)
        ax.axvline(x=µ0, linestyle='-', label='mean', color='blue')
        if not custom:
            if mn is not None:
                ax.axvline(x=mn, linestyle='--', label='min', color='black')
            if mx is not None:
                ax.axvline(x=mx, linestyle='--', label='max', color='red')

        ##ax.legend(loc='upper left')   Does bottom plot need its own legend?

        fig.execute_constrained_layout()
        plt.show(block=False)
        fig.canvas.manager.set_window_title('Distribution ' + alias)



def main():
    args = sys.argv
    qapp = QApplication(args)
    dialog = QDialog()
    psd_popup = PSDPopup(dialog)
    psd_popup.show()
    # exit with Ctrl-C at the terminal
    signal.signal(signal.SIGINT, signal.SIG_DFL)

    qapp.exec_()
    qapp.deleteLater()

    sys.exit()

if __name__ == '__main__':
    main()
