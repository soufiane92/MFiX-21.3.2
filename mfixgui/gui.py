#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Qt-based user interface for MFiX fluid dynamics engine"""

import argparse
import copy
import datetime
import errno
import glob
import json
import logging
import os
import re
import requests
import shutil
import signal
import sys
import tempfile
import threading
import time
import traceback
import warnings
import fnmatch
import git

from collections import OrderedDict
from getpass import getuser

from qtpy import QtGui, QtCore, QtWidgets
from qtpy.QtCore import Qt, Signal

from .constants import *
from .bug_report import excepthook

from .tools import (convert_string_to_python,
                    plural, find_project_file,
                    parse_key_with_args,
                    format_key_with_args,
                    get_run_name_from_file,
                    case_insensitive)

from .tools.qt import (get_icon, get_image_path, get_pixmap, get_combobox_item,
                       main_icon_size, sub_icon_size, get_preferred_style,
                       set_main_icon_size, set_sub_icon_size, set_alignment,
                       widget_iter, get_ui, SETTINGS)

from .tools.keyword_args import (keyword_args, reset_keyword_args,
                                        add_keyword_args)
from .namelistparser import getKeywordDoc, parse
from .expand_template import chemistry_lines, thermo_data_lines
from .regexes import re_valid_run_name_qt
from .tools.thumbnail import create_project_thumbnail
from .tools.monitor_reader import MonitorReader
from . import default_values, stylesheets
from . import vc
from .bcs import BCS
from .ics import ICS
from .iss import ISS
from .pss import PSS
from .chemistry import Chemistry
from .dashboard import Dashboard
from .console.console_printer import ConsolePrinter
from .fluid_handler import FluidHandler
from .solids_handler import SolidsHandler
from .scalar_handler import ScalarHandler
from .geometry_handler import GeometryHandler
from .graphic_tabs import GraphicTabs
from .interpreter import Interpreter
from .job import JobManager
from .file_menu import FileMenu
from .logger import Logger
from .mesh import Mesh
from .model_setup import ModelSetup
from .monitors import Monitors
from .numerics import Numerics
from .output import Output
from .advanced import Advanced
from .project_manager import ProjectManager
from .project import ExtendedJSON
from .status_manager import StatusManager
from .solver.process import ProcessManager
from .editor.ide_widget import IDEWidget
from .mesh_stats_viewer import MeshStatsViewer

from .animations import animation_speed

from .widgets.base import (BaseWidget, CheckBox, ComboBox,
                                  DoubleSpinBox, LineEdit, SpinBox, Table)

from mfixgui.widgets.new_project_popup import NewProjectDialog
from mfixgui.widgets.output_selection_popup import OutputSelectionPopup
from mfixgui.widgets.parameter_dialog import ParameterDialog
from mfixgui.widgets.regions import RegionsWidget
from mfixgui.widgets.regions_popup import RegionsPopup
from mfixgui.widgets.run_popup import open_run_popup
from mfixgui.widgets.species_popup import SpeciesPopup
from mfixgui.widgets.psd_popup import PSDPopup
from mfixgui.particle_props.particle_dialog import ParticlePopup
from mfixgui.widgets.build_popup import BuildPopup
from mfixgui.widgets.resource_monitor import ResourceMonitor

from mfixgui import vc

from mfixgui.version import __version__, parse_version

# Initialize logger early
log = logging.getLogger('mfix-gui' if __name__ == '__main__' else __name__)

# Use US English locale (for decimal separator in STL files)
os.environ["LC_NUMERIC"] = "en_US.utf8"

# --- Main Gui ---
class MfixGui(QtWidgets.QMainWindow,
              ModelSetup,
              Mesh,
              GeometryHandler,
              FluidHandler,
              SolidsHandler,
              ScalarHandler,
              ICS, BCS, PSS, ISS,
              Chemistry,
              Numerics,
              Monitors,
              Output,
              Dashboard,
              GraphicTabs,
              FileMenu,
              Interpreter,
              Logger,
              Advanced,
              MeshStatsViewer):

    # Main window class for MFIX-GUI
    project_file = None
    stdout_signal = Signal(str)
    stderr_signal = Signal(str)
    signal_update_runbuttons = Signal(str)
    signal_new_version_available = Signal(str)

    # Allow LineEdit widgets to report out-of-bounds values.
    def popup_value_error(self, exc):
        text = str(exc)
        text = text.replace('>', '&gt;')
        text = text.replace('<', '&lt;')
        self.message(title='Error', text=text)

    # Allow LineEdit widgets to report REQUIRED values.
    def popup_value_required(self, key):
        text = "A value is required for %s\nRestoring previous/default value" % key
        self.message(title='Warning', text=text)

    def error(self, msg, popup=False, print_console=True):
        # Show the user a warning & log it - use this instead of log.error

        if self.message_box and self.message_box.isVisible():
            popup=False # #Avoid cascading error popups
            print_console=True # Don't drop message!
            # FIXME, when printing to console HTML tags show up

        if print_console and not popup:
            self.print_internal('Error: %s' % msg)
        else:
            self.message(title='Error',
                         icon='warning', # FIXME create error icon
                         text=msg,
                         buttons=['ok'],
                         default='ok',
                         print_console=print_console)

    def warn(self, msg, popup=False, print_console=True):
        # Show the user a warning & log it - use instead of log.warn
        if self.message_box and self.message_box.isVisible():
            popup=False # Avoid multiple popups
            print_console=True  # But print message, so it's not lost
        if print_console and not popup:
            #print_internal will call log.warn if message starts with "Warning"
            # avoid double-warning for "Project has been converted" message
            if not any(msg.startswith(x) for x in ("Warning:",
                                    "Project has been converted")):
                msg = "Warning: " + msg
            self.print_internal(msg)
        else:
            self.message(text=msg, icon='warning', print_console=print_console)
            # Will also print-internal and log
    warning = warn

    def __init__(self, app, parent=None, project_file=None, loadnodeworks=True,
                 loadvtk=True, set_splash_text=None, style=''):
        self.app = app
        self.style = style.lower()
        QtWidgets.QMainWindow.__init__(self, parent)
        self.setObjectName('mfixgui')
        if project_file is not None:
            self.set_project_file(project_file)

        LineEdit.report_value_error = self.popup_value_error
        LineEdit.report_value_required = self.popup_value_required

        self.setWindowIcon(get_icon('mfix.png'))

        s = stylesheets.DEFAULT_STYLESHEET
        if 'macintosh' in self.style:
                s += stylesheets.OSX_STYLESHEET
        app.setStyleSheet(s)

        self.message_box = None # for tests to access
        # Initialize data members - make sure these values match 'reset'!
        self.solver_name = None
        self.fluid_solver_disabled = False # infer at load time
        self.hybrid_enabled = False # Hybrid solver disabled by default
        self.mfix_exe = None
        self.mfix_exe_flags = {}
        self.commandline_option_exe = None
        self.run_cmd = None # job launched by run_popup
        self.mfix_available = False
        self.open_succeeded = False
        self.unsaved_flag = False
        self.mesh_accepted = False
        self.mtime = 0.0
        self.run_popup = None
        self.process_manager = ProcessManager(self)
        if set_splash_text is not None:
            self.set_splash_text = set_splash_text
        else:
            def noop(text): return
            self.set_splash_text = noop
        self.cpu_ram_timer = None

        self.retained_keys = {}
        self.unlocked_keys = []
        self.allow_disabled_tab = False
        self.curr_nav_label = None
        # load UI
        # High-DPI.  Do this early before any widgets are created
        # Use minimum, to account for multi-screen setups
        #  (Qt reports DPI based on combined area of all monitors)
        dpi = min(self.physicalDpiX(),
                  self.physicalDpiY())
        if dpi > 96:
            set_main_icon_size(24*dpi/96)
            set_sub_icon_size(16*dpi/96)

        self.customWidgets = {
            'LineEdit':      LineEdit,
            'CheckBox':      CheckBox,
            'ComboBox':      ComboBox,
            'DoubleSpinBox': DoubleSpinBox,
            'SpinBox':       SpinBox,
            'Table':         Table}

        self.ui = get_ui('gui.ui')
        self.ui.panes = []
        self.setCentralWidget(self.ui)

        names = ('model_setup',
                 'geometry',
                 'mesh',
                 'regions',
                 'fluid',
                 'solids',
                 'scalars',
                 'initial_conditions',
                 'boundary_conditions',
                 'point_sources',
                 'internal_surfaces',
                 'chemistry',
                 'numerics',
                 'output',
                 'monitors',
                 'run',
                 'dashboard',
                 'advanced')

        for name in names:
            if name == 'regions':  # Regions.__init__ loads its own .ui file
                widget = RegionsWidget(gui=self)
            else:
                widget = QtWidgets.QWidget()
                try:
                    get_ui(name+'.ui', widget)
                except Exception as e:
                    # report which ui file it was, otherwise stack trace
                    # is too generic to be helpful.
                    print("Error loading", name+'.ui')
                    # Exception is re-raised don't need to print it here
                    raise

            # assign 'self.ui.general', etc
            setattr(self.ui, name, widget)
            self.ui.task_pane.addWidget(widget)
            widget.input_enabled = True
            widget.partial_input = False
            self.ui.panes.append(widget)
            # set text on splash
            self.set_splash_text('Creating %s widgets'%name)
        # end of ui loading

        # add and hide a progress bar in the status bar
        # this is a hack to show text on top of a QtWidgets.QProgressBar
        # add bar first, then label
        self.progress_bar = QtWidgets.QProgressBar()
        self.progress_bar.setTextVisible(False)
        self.ui.horizontallayout_mode_bar.addWidget(self.progress_bar, 0, 6)
        self.progress_bar.hide()
        self.ui.label_status = QtWidgets.QLabel('')
        set_alignment(self.ui.label_status,(Qt.AlignRight|Qt.AlignVCenter))
        self.ui.horizontallayout_mode_bar.addWidget(self.ui.label_status, 0, 6)

        # create cpu and ram resource progress bars
        self.resource_monitor = ResourceMonitor()
        self.ui.horizontallayout_mode_bar.addWidget(self.resource_monitor, 0, 7)
        self.resource_monitor.show(int(SETTINGS.value('show_resources', 0)))

        # build keyword documentation from namelist docstrings
        self.keyword_doc = getKeywordDoc()

        # converted/banned keys
        self.orig_keyword_doc = self.keyword_doc.copy()
        for k in ('xlength', 'ylength', 'zlength'):
            self.keyword_doc.pop(k, None)

        self.add_extra_keyword_doc()
        self.saved_keyword_doc = self.keyword_doc.copy() # For reset

        # Setup the navigation tree widget
        tw = self.ui.treewidget_navigation
        self.max_label_len = tw.fontMetrics().width('Boundary Conditions') + 10

        self.nav_labels = [("Model setup", "Model"),
                           ("Post-processing", "Post"),
                           ("Boundary conditions", "BC"),
                           ("Initial conditions", "IC"),
                           ("Point sources", "PS"),
                           ("Internal surfaces", "IS"),
                           ("Dashboard", "Dash")]

        # Set tooltips for nav tree
        root = tw.invisibleRootItem()
        for i in range(root.childCount()):
            item = root.child(i)
            item.setToolTip(0, item.text(0))
            for j in range(item.childCount()):
                subitem = item.child(j)
                subitem.setToolTip(0, subitem.text(0))

        # Intercept the resize event
        tw.resizeEvent = (lambda old_method:
                          (lambda event:
                           (self._nav_tree_resized(event),
                            old_method(event))[-1]))(tw.resizeEvent) # pylint: disable=undefined-variable

        # Initialize popup dialogs
        self.set_splash_text('Initializing Dialogs')
        self.species_popup = SpeciesPopup(self)
        self.build_popup = BuildPopup(self)
        self.psd_popup = PSDPopup(self)
        self.regions_popup = RegionsPopup(self)
        self.particle_popup = ParticlePopup(self)

        # SMS mode
        self.mode = None
        self.sms_mode = False
        self.sms_enabled = False
        self.ui.pushbutton_mesher.hide()
        # Cannot init tooltips in widgets/regions.py because it loads before keyword_doc
        cb = self.ui.regions.combobox_bc_type
        self.add_tooltip(cb, key='bc_type')
        for i in range(1,len(cb)): # offset for "None"
            self.add_tooltip(get_combobox_item(cb, i), key='bc_type', value=BC_TYPES[i-1])
        get_combobox_item(cb,0).setToolTip("Region is not a boundary")

        # Create project manager
        # NOTE.  it's a ProjectManager, not a Project.  But
        # ProjectManager is a subclass of Project.  Please
        # do not "fix" the code by renaming self.project to
        # self.project_manager
        self.project = ProjectManager(self, self.orig_keyword_doc)

        # Extra setup for fluid & solids panes.  Needs to happen
        # after we create ProjectManager, because widgets get registered
        self.set_splash_text('Creating panes')
        self.init_model_setup()
        self.init_fluid_handler()
        self.init_solids_handler()
        self.init_scalar_handler()
        self.init_mesh()
        self.init_geometry()
        self.init_ics()
        self.init_bcs()
        self.init_pss()
        self.init_iss()
        self.init_chemistry()
        self.init_numerics()
        self.init_output()
        self.init_monitors()
        self.init_dashboard()
        self.init_logger()
        self.init_graphics_tabs(loadvtk)
        self.init_keyboard_shortcuts()
        self.init_advanced()
        self.init_mesh_stats_viewer()
        self.init_history()

        # In-process REPL (for development, should we enable this for users?)
        self.init_interpreter()

        # delete popup
        self.output_selection_popup = OutputSelectionPopup(self)

        # --- animation data
        self.modebuttons = {'mesher': self.ui.pushbutton_mesher,
                            'modeler':   self.ui.pushbutton_modeler,
                            'nodeworks':  self.ui.pushbutton_nodeworks,
                            'editor': self.ui.pushbutton_editor,
                            'history': self.ui.pushbutton_history,
                            'interpreter': self.ui.pushbutton_interpreter}
        self.animating = False
        self.stack_animation = None

        # --- icons ---
        # loop through all widgets & set icons for any ToolButton with
        # add/delete/copy/edit in the name
        # for mac, remove blue border from QtWidgets.QListWidget and QtWidgets.QTreeWidget
        # Have to do this after instantiating a QApp

        self.set_splash_text('Loading icons')
        for widget in widget_iter(self):
            if isinstance(widget, QtWidgets.QToolButton):
                name = str(widget.objectName())
                if 'add' in name:
                    widget.setIcon(get_icon('add.svg'))
                    widget.setIconSize(sub_icon_size())
                elif 'delete' in name or 'remove' in name:
                    widget.setIcon(get_icon('remove.svg'))
                    widget.setIconSize(sub_icon_size())
                elif 'copy' in name:
                    widget.setIcon(get_icon('copy.svg'))
                    widget.setIconSize(sub_icon_size())
                elif 'edit' in name:
                    widget.setIcon(get_icon('edit.svg'))
                    widget.setIconSize(sub_icon_size())
                elif 'up' in name:  # Might accidentally match 'update', etc
                    widget.setArrowType(Qt.UpArrow)
                    #widget.setIconSize(sub_icon_size()) Has no effect for built-in QArrow
                elif 'down' in name:
                    widget.setArrowType(Qt.DownArrow)
                    #widget.setIconSize(sub_icon_size())
                elif 'left' in name:
                    widget.setArrowType(Qt.LeftArrow)
                    #widget.setIconSize(sub_icon_size())
                elif 'right' in name:
                    widget.setArrowType(Qt.RightArrow)
                    #widget.setIconSize(sub_icon_size())
                elif 'search' in name:
                    widget.setIcon(get_icon('search.svg'))
                    widget.setIconSize(sub_icon_size())
            elif isinstance(widget, (QtWidgets.QListWidget, QtWidgets.QTreeWidget)):
                # remove blue outline focus on mac
                widget.setAttribute(Qt.WA_MacShowFocusRect, 0)

            if isinstance(widget, QtWidgets.QAbstractButton):
                # Make sure lineedits lose focus so updates stick!!
                widget.setFocusPolicy(Qt.ClickFocus)

            if isinstance(widget, QtWidgets.QComboBox):
                # Allow mousewheel scrolling, don't disrupt combobox settings
                widget.wheelEvent = lambda ev: None

        # Toolbuttons at top of frame
        ui = self.ui
        for (button, icon_name, function) in (
                (ui.toolbutton_menu, 'menu', self.open_file_menu),
                (ui.toolbutton_save, 'save', self.handle_save),
                (ui.toolbutton_run_mfix, 'play', self.handle_run),
                (ui.toolbutton_pause_mfix, 'pause', self.handle_pause),
                (ui.toolbutton_stop_mfix, 'stop', self.handle_stop),
                (ui.toolbutton_reset_mfix, 'restore_delete', self.remove_output_files),
                (ui.toolbutton_compile, 'build', self.handle_compile),
                (ui.toolbutton_parameters, 'sliders', self.handle_parameters),
                (ui.toolbutton_settings, 'settings', lambda: self.open_file_menu('settings')),
                (ui.toolbutton_help, 'help', lambda: self.open_file_menu('help')),
                (ui.toolbutton_search, 'search', self.handle_search),
                (ui.toolbutton_new_version, 'notification_important', self.handle_update)):
            button.setIcon(get_icon(icon_name+'.svg'))
            button.setIconSize(main_icon_size())
            button.clicked.connect(function)

        ui.toolbutton_new_version.setVisible(False)

        ui.toolbutton_stop_mfix.mouseDoubleClickEvent = self.handle_force_stop

        # disable compile until a project is opened
        ui.toolbutton_compile.setEnabled(False)
        # We want to know what window has focus when the user presses 'Help',
        #  so prevent help button from grabbing focus.  Doing this in .ui doesn't work
        ui.toolbutton_help.setFocusPolicy(Qt.NoFocus)

        # Make sure lineedits lose focus so keywords update before save/run !!
        for button in (ui.toolbutton_run_mfix, ui.toolbutton_save):
            button.setFocusPolicy(Qt.ClickFocus)

        # Geometry toolbuttons
        geo = self.ui.geometry
        for btn, icon in [('toolbutton_add_geometry', 'geometry.svg'),
                          ('toolbutton_add_filter', 'filter.svg'),
                          ('toolbutton_wizard', 'wand.svg'),
                          ('toolbutton_geometry_union', 'union.svg'),
                          ('toolbutton_geometry_intersect', 'intersect.svg'),
                          ('toolbutton_geometry_difference', 'difference.svg')]:
            b = getattr(geo, btn)
            b.setIcon(get_icon(icon))
            b.setIconSize(sub_icon_size())

        self.init_mode_buttons()

        self.init_navigation_tree()

        ui.treewidget_navigation.itemSelectionChanged.connect(
            self.navigation_changed)

        # Make tree fully open & non-closable
        # We expect "rootIsDecorated" has been set False in the .ui file
        tree = ui.treewidget_navigation
        tree.expandAll()
        tree.setExpandsOnDoubleClick(False)
        tree.setMaximumWidth(tree.fontMetrics().width('Boundary Conditions') + 10)
        tree.setMinimumWidth(tree.fontMetrics().width('Chemistry') + 10)

        # Make splitters non-collapsing
        collapse = int(SETTINGS.value('collapse_qsplitter', 0))
        for widget in widget_iter(self):
            if isinstance(widget, QtWidgets.QSplitter):
                widget.setChildrenCollapsible(collapse)

        # Job manager / monitor
        self.set_splash_text('Creating job manager')
        self.status_manager = StatusManager(self)
        self.job_manager = JobManager(self)
        self.job_manager.sig_change_run_state.connect(self.slot_update_runbuttons)

        self.job_manager.sig_update_job_status.connect(self.slot_update_status)
        self.watch_run_dir_timer = QtCore.QTimer()
        self.watch_run_dir_timer.timeout.connect(self.slot_rundir_timer)
        self.watch_run_dir_timer.start(100)

        self.signal_new_version_available.connect(self.show_new_version_popup)

        # monitor reader
        self.monitor_reader = MonitorReader('csv', self)
        self.project.register_widget(self.monitor_reader, keys=['monitor_name'], args=['*'], force=True)

        # Initialize console_printer
        self.console_printer = ConsolePrinter(self)
        self.ui.console.help_request.connect(self.handle_search)

        # Print welcome message.  Do this early so it appears before any
        # other messages that may occur during this __init__
        self.console_printer.print_welcome()

        ## Run signals
        self.stdout_signal.connect(self.console_printer.handle_stdout)
        self.stderr_signal.connect(self.console_printer.handle_stderr)
        self.signal_update_runbuttons.connect(self.slot_update_runbuttons)

        # --- Register widgets ---
        self.set_splash_text('Enabling controls')
        self.register_keyword_widgets()

        # --- Create file menu ---
        self.set_splash_text('Setting up menu')
        self.init_file_menu(self)

        # --- vtk setup ---
        self.set_splash_text('Loading vtk')
        self.init_vtk_widget(loadvtk)

        # --- nodeworks setup ---
        self.set_splash_text('Loading nodeworks')
        self.init_nodeworks_widget(loadnodeworks)

        # --- editor setup ---
        self.set_splash_text('Loading editor')
        self.init_editor_widget()

        # --  instantiate dialogs
        self.parameter_dialog = ParameterDialog(self)

        # --- default --- # do we need this?  note this gets reset in main anyhow
        #self.change_mode('modeler')
        #self.change_pane('model setup')

        # Update run options
        self.slot_update_runbuttons()

        # Hybrid solver not available yet
        self.disable_hybrid()

        # Set input validator for run_name
        le = self.ui.run.lineedit_keyword_run_name
        le.setValidator(QtGui.QRegExpValidator(
            QtCore.QRegExp(re_valid_run_name_qt)))
        le.required = True

        # Reset everything to default values
        # This is done in 'load_project'.  so why do it now?
        #self.reset() # Clear command_output too?

        #  end of __init__

    def check_project_version(self):
        """show a warning if the project file was created with a newer version of MFiX"""
        gui_version = parse_version(__version__)
        project_version_str =  self.project.mfix_gui_comments.get("gui_version", "")
        project_version = parse_version(project_version_str)

        if any(9999 in v for v in (gui_version, project_version)):
                return

        if gui_version < project_version:
            self.warning("""This project was created with a newer version of MFiX (%s).
This is not supported. Some settings may need to be manually adjusted."""
                         % project_version_str,
                         popup=True)

    def show_new_version_popup(self, version_str):
        self.ui.toolbutton_new_version.setVisible(True)
        self.show_update_available(version_str) # from FileMenu

    # Check website for updates
    def check_update_available(self, gui_version_str):
        json_url = "http://mfix.netl.doe.gov/wp-content/themes/mfix/mfix_versions.json"
        try:
            req = requests.get(json_url, timeout=1.0)
            versions = []
            for info in req.json().values():
                version = info.get("conda_version", None)
                if version and all(part.isdigit() for part in version.split('.')):
                    versions.append(version)
            if not versions:
                return
            versions.sort(key=parse_version)
            latest_version_str = versions[-1]
            latest_version = parse_version(latest_version_str)
            gui_version = parse_version(__version__)
            if 9999 in gui_version:
                return

            if gui_version < latest_version:
                self.signal_new_version_available.emit(latest_version_str)

        except Exception as e:
            print("Error '%s' checking for new version" % str(e))


    def init_mode_buttons(self):
        """ mode (modeler, nodeworks, editor) """
        for mode, btn in self.modebuttons.items():
            btn.pressed.connect(lambda mode=mode: self.change_mode(mode))
            # Force button to remain in 'checked' state
            btn.released.connect(lambda btn=btn: btn.setChecked(True))
            btn.setFocusPolicy(Qt.NoFocus)
            width = btn.fontMetrics().boundingRect(btn.text()).width() + 13
            btn.setMinimumWidth(width)
            btn.setMaximumWidth(width)

    def init_navigation_tree(self):
        icon = QtGui.QIcon()
        icon.addFile(get_image_path('right_light.svg'), QtCore.QSize(), icon.Normal, icon.Off)
        icon.addFile(get_image_path('right.svg'), QtCore.QSize(), icon.Active, icon.Off)
        self.icon_collapse_right = icon

        icon = QtGui.QIcon()
        icon.addFile(get_image_path('left_light.svg'), QtCore.QSize(), icon.Normal, icon.Off)
        icon.addFile(get_image_path('left.svg'), QtCore.QSize(), icon.Active, icon.Off)
        self.icon_collapse_left = icon

        self.ui.toolbutton_collapse_navigation.clicked.connect(self.toggle_nav_menu)
        self.ui.toolbutton_collapse_navigation.setIcon(self.icon_collapse_left)

    def init_keyboard_shortcuts(self):
        shortcuts = [('Ctrl+O', self.handle_open_shortcut),
                     ('Ctrl+N', self.handle_new_shortcut),
                     ('Ctrl+S', self.handle_save),
                     ('Ctrl+R', self.handle_run_shortcut),
                     ('Ctrl+B', self.handle_compile_shortcut),
                     ('Ctrl+Q', self.close),
                     ('Ctrl+H', self.handle_search),
                     ('F1', self.handle_search),
                     ('Esc', self.handle_esc)]

        for key, callback in shortcuts:
            q = QtWidgets.QShortcut(QtGui.QKeySequence(key), self)
            q.activated.connect(callback)

    def handle_compile_shortcut(self):
        if self.file_menu.isVisible():
            self.hide_file_menu()
        if self.ui.toolbutton_compile.isEnabled():
            self.handle_compile()

    def handle_run_shortcut(self):
        if self.file_menu.isVisible():
            self.hide_file_menu()
        if self.ui.toolbutton_run_mfix.isEnabled():
            self.handle_run()

    def handle_esc(self):
        if self.file_menu.isVisible():
            self.hide_file_menu()

    def add_extra_keyword_doc(self):
        # Add a little extra knowledge. # TODO , move all of this to *.f

        # These are all fractions, must be btwn 0 and 1, not documented as such
        for key in ('des_em',
                    'eps_f_min',
                    'bc_xw_g',
                    'phip0'):
            self.keyword_doc[key]['validrange'] = {'min':0.0, 'max':1.0}

        # Diameter must be > 0, setting 'min:0' doesn't enforce this, hence this hack
        self.keyword_doc['d_p0']['validrange'] = {'min': 0, 'exclude_min':True}
        # Density also?
        self.keyword_doc['ro_s0']['validrange'] = {'min':0, 'exclude_min':True}

        # ro_g0 must be >0 if set.  We set 'exclude_min' dynamically
        self.keyword_doc['ro_g0']['validrange'] = {'min':0}

        # MAX_INLET_VEL_FAC
        # DEFAULT 1.0
        # Error check: Value greater than or equal to 1.0
        self.keyword_doc['max_inlet_vel_fac']['validrange'] = {'min': 1.0}
        # Error check: value bounded between 0 and 1
        self.keyword_doc['ur_kth_sml']['validrange'] = {'min':0.0, 'max':1.0}

        # Number of particles must be non-negative
        self.keyword_doc['particles']['validrange'] = {'min':0.0}

        # Times must be nonnegative
        self.keyword_doc['res_dt']['validrange'] = {'min':0.0}
        self.keyword_doc['res_backups']['validrange'] = {'min': 0}
        self.keyword_doc['nrr']['validrange'] = {'min': 0}

        # Remove mention of 'cylindrical' since we don't support it
        self.keyword_doc['no_k']['description'] = 'Flag to disable the third dimension (i.e., 2D simulation).'

        ## Remove this docstring completely (if exists) - refers to cylindrical coordinates
        #  But is 'xmin' exposed anywhere?  NB xmin != x_min
        #self.keyword_doc['xmin'].pop('description', None)

        # Don't allow 0 for number of cells
        for key in 'imax', 'jmax', 'kmax':
            self.keyword_doc[key]['validrange']['min'] = 1

        # MW_AVG (issues/402)
        self.keyword_doc['mw_avg']['validrange']['min'] = 0.0

        # pressure must be >= 0
        self.keyword_doc['p_ref']['validrange']['min'] = 0.0

        self.keyword_doc['ro_g0']['description'] = ("""
Specified constant gas density [kg/m^3 in SI]. An equation of state
(the ideal gas law by default) is used to calculate the gas density
if this parameter is undefined. To set this value to zero and
simulate granular flow in a vacuum, select 'Disable fluid solver'
in the 'Model' pane""")

        valids = self.keyword_doc['is_type']['valids']
        # Don't expose aliases for is_type keys - we are preferring long form.
        # But we can't delete alias from init_namelist XML or some cases won't run
        for v in valids:
            valids[v].pop('alias', None)
        for axis in 'XYZ':
            for is_type in 'IMPERMEABLE', 'SEMIPERMEABLE':
                val = "%s_%s" % (axis, is_type)
                valids[val] = valids[is_type].copy()
                valids[val]['value'] = val
                valids[val]['note'] = valids[val]['note'].replace(
                    'through the surface', 'in the %s direction'%axis)
        valids['STL']={'value':"STL",
                       'note': 'Moveable internal surface'}

        # issues/1227
        self.keyword_doc['max_nit']['validrange']['exclude_min'] = True
        self.keyword_doc['ppg_den']['validrange']['exclude_min'] = True
        self.keyword_doc['epp_den']['validrange']['exclude_min'] = True
        self.keyword_doc['des_buff_resize_factor']['validrange']['exclude_min'] = True

        # issues/1239
        self.keyword_doc['bc_k_turb_g']['validrange']['exclude_min'] = False
        self.keyword_doc['bc_e_turb_g']['validrange']['exclude_min'] = True
        self.keyword_doc['ic_k_turb_g']['validrange']['exclude_min'] = False
        self.keyword_doc['ic_e_turb_g']['validrange']['exclude_min'] = True


        # should we enforce all temperatures > 0 ?


    def set_no_project(self):
        """setup mode when no project is open"""
        self.open_succeeded = False
        self.set_solver(None)
        self.set_project_file(None)
        self.clear_unsaved_flag() # sets save button
        self.update_window_title()
        self.enable_input(False)
        self.fluid_solver_disabled = False
        self.disable_fluid_solver(False)
        self.ui.toolbutton_compile.setEnabled(False) # disable compile
        # This gets set by guess_solver if we're loading a project, otherwise
        # we need to set the default.  (Do other defaults need to be set here?)

        self.status_manager.status_message("No project - open existing MFiX project or create a new one")
        self.change_pane("model setup")
        self.editor_widget.open_project('.')

    def reset(self):
        """Reset all widgets to default values and set GUI to blank-slate"""

        #self.change_pane("model setup") # Default pane
        # see issues/860 - this conflicts with restoring active pane from job file

        # clean out job
        self.job_manager.disconnect()
        self.process_manager.disconnect()

        # Defaults - see __init__
        self.solver_name = None
        self.retained_keys.clear()
        self.project.reset() # Clears all keywords & collections
        reset_keyword_args()
        self.slot_rundir_timer()

        # Keyword doc may have additional keys from usr_init_namelist.f.  Set it
        #  back to default.  Don't just assign to self.keyword_doc here b/c
        #  project_manager also holds a reference to it
        self.keyword_doc.clear()
        self.keyword_doc.update(self.saved_keyword_doc)

        self.reset_model_setup()
        self.reset_fluid()
        self.reset_solids()
        self.reset_scalars()
        self.reset_mesh()
        self.reset_geometry()
        self.ui.regions.reset_regions()
        self.reset_ics()
        self.reset_bcs()
        self.reset_iss()
        self.reset_pss()
        self.reset_chemistry()
        self.reset_dashboard()
        self.reset_output()
        self.reset_logger()
        self.reset_monitors()
        self.reset_advanced()
        self.reset_graphics_tabs()
        self.reset_mesh_stats_viewer()
        self.monitor_reader.reset()
        if self.ui.nodeworks_widget.NODEWORKS_AVAILABLE:
            self.ui.nodeworks_widget.reset()
        self.editor_widget.reset()

        self.retained_keys.clear()
        self.allow_disabled_tab = False
        self.fluid_solver_disabled = False
        self.disable_fluid_solver(False)

        self.mode = None

        # SMS mode will depend on next project loaded, so maybe
        # we should avoid extra GUI churn here
        self.sms_mode = False
        self.ui.pushbutton_mesher.hide()

        # Set all custom widgets to default
        for w in widget_iter(self):
            if isinstance(w, BaseWidget):
                w.default()
            elif hasattr(w, 'default'):
                w.default()
            else:
                pass # What to do for rest of widgets?

        # reset parameters
        base_parameters = OrderedDict([(key, 0.0) for key in PARAMETER_DICT])
        PARAMETER_DICT.clear()
        PARAMETER_DICT.update(base_parameters)
        PARAMETER_DICT.update(copy.deepcopy(CONVERSION_TO_METERS))

        #self.console_printer.clear_console()

        self.update_nav_tree()
        #self.clear_mesh_accepted() # Sets unsaved_flag, but we clear it next
        self.mesh_accepted = False # clear_mesh_accepted sets PPO and we don't want that
        self.clear_unsaved_flag()

        #Workaround for ro_g0 bug (still needed?)
        le = self.ui.fluid.lineedit_keyword_ro_g0
        le.required = False
        le.minimum = 0.0
        #self.set_project_file(None)  - do we want to do this?


    def confirm_close(self):
        """before closing, ask user whether to end job and save project"""

        if self.job_manager.job:
            confirm = self.message(text="Currently running job. Are you sure you want to quit?",
                                   buttons=['ok', 'cancel'],
                                   default='cancel')
            if confirm == 'cancel':
                return False

        if self.unsaved_flag:
            confirm = self.message(text="Save project before quitting?",
                                   title="Save?",
                                   icon='question',
                                   buttons=['yes', 'no', 'cancel'],
                                   default='cancel')
            if confirm == 'yes':
                self.save_project()
            return confirm != 'cancel'
        else:
            return True


    def update_keyword(self, key, value, args=None):
        """like set_keyword but no action if value already set"""
        if isinstance(args, int):
            args = [args]
        elif isinstance(args, tuple):
            args = list(args)
        elif args is None:
            args = []
        args = self.project.expand_args(args)
        expected_args = keyword_args.get(key)
        if expected_args is None:
            self.error("Unknown keyword %s" % key, popup=True)
            return
        if expected_args is not None:
            if len(args) != len(expected_args):
                self.error("keyword %s: argument mismatch, expected %s, got %s" %
                           (key, str(expected_args), str(args)))
                return

        # For MONITOR_ and VTK_ booleans, unset instead of saving many False values
        if key.startswith(('vtk_', 'monitor_', 'part_in_', 'part_out_')):
            if value is False is self.keyword_doc.get(key,{}).get('initpython'):
                value = None

        # Can't do this unless we expand args out first ('BC', etc)
        # if args:
        #     for arg in args:
        #         if not isinstance(arg, int):
        #             self.error("keyword %s: indices must be integer, not %s" %
        #                        (key, repr(arg)))
        #             return
        if value is None or value=='':
            return self.unset_keyword(key, args)

        #check whether it's actually changed
        v = self.project.get_value(key, args=args)
        if type(v) == type(value) and str(v) == str(value):
            return False
        if v==value==0.0: # treat 0 and 0.0 as equivalent
            return False
        self.set_unsaved_flag()

        # check if it is a bc or is key, tell vtkwidget to save
        if any(key.startswith(pre) for pre in ['bc', 'is']):
            self.vtkwidget.needs_saved = True

        self.project.submit_change(None, {key:value}, args)
        return True


    def set_keyword_default(self, key, value, args=None):
        """Set key to default value if not already set"""
        prev = self.project.get_value(key, args=args)
        if prev is None:
            return self.update_keyword(key, value, args=args)


    def unset_keyword(self, key, args=None):
        """Undefine keyword.  Report to user, also catch and report any errors"""
        #  Note - keyword is still registered!  This method does not deregister
        #   keywords with project manager
        if isinstance(args, int):
            args = [args]
        elif isinstance(args, tuple):
            args = list(args)
        elif args is None:
            args = []

        args = self.project.expand_args(args)

        # If any element of 'args' is itself a list, iterate over all values
        # This does not handle more than one list/tuple
        # See similar code in project_manager.change
        if any(isinstance(arg, (list,tuple)) for arg in args):
            r = None
            copy_args = list(args)
            for (i, arg) in enumerate(args):
                if isinstance(arg, (list,tuple)):
                    for a in arg:
                        copy_args[i] = a
                        r=self.unset_keyword(key, args=copy_args)
                    break
            return r

        try:
            success = self.project.removeKeyword(key, args, warn=False)
            if success:
                self.set_unsaved_flag()
                self.print_internal("%s" % format_key_with_args(key, args),
                                    font='strikeout')
                return True

        except Exception as e:
            msg = 'Warning: Failed to unset %s: %s: %s' % (format_key_with_args(key, args),
                                                           e.__class__.__name__, e)
            self.print_internal(msg, color='red')
            traceback.print_exception(*sys.exc_info())

    def retain_keyword(self, key, args=None):
        val = self.project.get_value(key, args=args)
        if val is None:
            return
        if args is None:
            args = []
        else:
            if isinstance(args, int):
                args = [args]
        self.retained_keys[(key,tuple(args))] = val


    def get_retained_keyword(self, key, args=None, default=None):
        if args is None:
            args = []
        else:
            if isinstance(args, int):
                args = [args]
        return self.retained_keys.get((key,tuple(args)), default)

    def clear_retained_keyword(self, key, args=None):
        if args is None:
            args = []
        else:
            if isinstance(args, int):
                args = [args]
        self.retained_keys.pop((key,tuple(args)), None)

    def _nav_tree_resized(self, ev):
        w = ev.size().width()
        if w < self.max_label_len:
            self.short_labels()
        else:
            self.long_labels()

    def short_labels(self):
        tree = self.ui.treewidget_navigation
        flags = Qt.MatchFixedString | Qt.MatchRecursive
        for (long, short) in self.nav_labels:
            items = tree.findItems(long, flags, 0)
            for item in items:
                item.setText(0, short)

    def long_labels(self):
        tree = self.ui.treewidget_navigation
        flags = Qt.MatchFixedString | Qt.MatchRecursive
        for (long, short) in self.nav_labels:
            items = tree.findItems(short, flags, 0)
            for item in items:
                item.setText(0, long)

    def unimplemented(self):
        self.message(title='Unimplemented',
                     text='Feature not implemented')

    def update_nav_tree(self):
        self.ics_update_enabled()
        self.bcs_update_enabled()
        self.pss_update_enabled()
        self.iss_update_enabled()
        self.chemistry_update_enabled()
        self.scalars_update_enabled()
        # Other panes?  does monitors, vtk, etc depend on having
        #  regions?

    def set_sms_enabled(self, on):
        ui = self.ui
        on = bool(on)
        SETTINGS.setValue('SMS', int(on))
        SETTINGS.sync()
        self.sms_enabled = on

        # File menu items are (somewhat bizarrely) in our UI namespace
        #  and they have setHidden but not setVisible
        ui.new_mesh.setHidden(not on)
        ui.open_mesh.setHidden(not on)
        if self.project_file is None:
            return
        if on:
            if self.project.get_value('ppo') is not None:
                self.set_sms_mode(on)
            else:
                self.print_internal("Converting project to SMS workflow", color='blue')
                self.update_keyword('ppo', True)
                self.clear_mesh_accepted()
                self.set_sms_mode(True)
                #self.warning("Current project does not support SMS.\nUsing standard workflow.",
                #             popup=True)
                #self.set_sms_mode(False)
        else:
            self.set_sms_mode(False)


    def set_sms_mode(self, on):
        # Segregated Mesher/Solver
        #if not self.sms_enabled:
        #    return
        self.sms_mode = on
        ui = self.ui

        # Show/hide UI items
        for item in (ui.mesh.pushbutton_generate_mesh,
                     ui.mesh.pushbutton_delete_mesh,
                     ui.mesh.pushbutton_accept_mesh):
                     #ui.mesh.label_mesh_accepted):
            item.setVisible(on)
        ui.mesh.label_mesh_accepted.setVisible(bool(self.mesh_accepted))
        ui.mesh.pushbutton_accept_mesh.setDown(bool(self.mesh_accepted))
        if on:
            ui.pushbutton_mesher.show()
            ui.pushbutton_modeler.show()
            ui.pushbutton_modeler.setEnabled(bool(self.mesh_accepted))
            if not self.mesh_accepted:
                ui.pushbutton_modeler.setToolTip("Mesh not accepted")
            else:
                ui.pushbutton_modeler.setToolTip(None)
            ui.new_mesh.setHidden(False)
            ui.open_mesh.setHidden(False)
            if self.project.get_value('ppo') is None:
                self.update_keyword('ppo', True)
            if self.mode == 'modeler' and not self.mesh_accepted:
                self.change_mode('mesher')
                if self.curr_nav_label != 'Regions': #This is the only tab that is in both modes
                    self.change_pane('geometry') #?
        else: # Non-SMS
            if self.project.get_value('ppo'):
                self.print_internal("Removing SMS settings from project", color='blue')
            self.unset_keyword('ppo')
            self.project.mfix_gui_comments.pop('mesh_accepted', None)
            ui.pushbutton_mesher.hide()
            ui.pushbutton_modeler.setEnabled(True)
            ui.pushbutton_modeler.setToolTip(None)
            tw = ui.treewidget_navigation
            # Put all nav items back in Modeler
            for i in range(tw.topLevelItemCount()):
                item = tw.topLevelItem(i)
                item.setHidden(False)
            if self.mode == 'mesher':
                self.change_mode('modeler')
                if self.curr_nav_label != 'Regions':
                    self.change_pane('model setup') # From the top


    def update_region(self, name, data):
        for update in (self.ics_update_region,
                       self.bcs_update_region,
                       self.iss_update_region,
                       self.pss_update_region,
                       self.monitors_update_region,
                       self.output_update_region):
            update(name, data)

    def get_region_users(self, name):
        """Return a list of object types referring to region.  Always return a list, even if empty"""
        return [t for (t, check) in (('IC', self.ics_check_region_in_use),
                                     ('BC', self.bcs_check_region_in_use),
                                     ('PS', self.pss_check_region_in_use),
                                     ('IS', self.iss_check_region_in_use),
                                     ('Output', self.output_check_region_in_use), #VTK/Usr
                                     ('Monitor', self.monitors_check_region_in_use))
                                     # any more places region can be used?
                if check(name)]

    def change_region_name(self, name, new_name):
        self.bcs_change_region_name(name, new_name)
        self.ics_change_region_name(name, new_name)
        self.iss_change_region_name(name, new_name)
        self.pss_change_region_name(name, new_name)
        self.output_change_region_name(name, new_name)
        self.monitors_change_region_name(name, new_name)
        # any more places region can be used?


    def toggle_nav_menu(self):
        nav_menu = self.ui.treewidget_navigation
        nav_menu.setVisible(not nav_menu.isVisible())

        if nav_menu.isVisible():
            self.ui.toolbutton_collapse_navigation.setIcon(self.icon_collapse_left)
        else:
            self.ui.toolbutton_collapse_navigation.setIcon(self.icon_collapse_right)

    def slot_rundir_timer(self):
        project_dir = self.get_project_dir()
        if project_dir and not self.job_manager.job:
            run_name = self.project.get_value('run_name')
            if run_name:
                pidfile = os.path.join(project_dir, run_name+'.pid')
                self.job_manager.try_to_connect(pidfile)

    def enable_input(self, enabled=True, partial=False):
        # Enable/disable all inputs (while job running, etc)
        # enable some inputs if partial==True (paused, resumable)
        # TODO clean this up, it knows too much about internals
        # of each pane. Move to bcs.enable, ics.enable, etc
        for pane in self.ui.panes:
            pane.input_enabled = enabled
            name = pane.objectName()
            if name in ['dashboard']:
                continue
            targets = [pane]
            if hasattr(pane, 'detail_pane'):
                targets = [pane.detail_pane]
            elif name == 'geometry':
                targets = [pane.groupbox_domain_extents,
                           pane.frame_geometry_toolbar,
                           pane.groupBoxGeometryParameters]
                self.vtkwidget.enable(enabled, partial)
            elif name == 'numerics':
                targets = [pane.stackedwidget_numerics]
            elif name == 'output':
                targets = [#pane.bottom_frame_vtk, # bottom of VTK pane, leave table enabled
                    pane.cell_common,
                    pane.scrollarea_cell_contents,
                    pane.scrollarea_particle_contents,
                    pane.bottom_frame_usr,
                    pane.page_basic,
                    pane.page_spx,
                    pane.page_residuals,
                    pane.page_log]
            elif name == 'monitors':
                pass
            elif name == 'fluid':
                targets = [pane.frame_name,
                           pane.groupbox_options,
                           pane.groupbox_properties,
                           pane.frame_add_delete_species]

            elif name == 'mesh':
                targets = [pane.stackedwidget_mesh] # leave top frame enabled
            for w in widget_iter(pane):
                if w.objectName().startswith('frame_add_'):
                    targets.append(w)
            if name == 'solids':
                targets = [pane.detail_pane,
                           pane.detail_pane_dem,
                           pane.frame_add_delete_copy,
                           pane.TFM,
                           pane.PIC]

            if name == 'regions':
                self.ui.regions.update_region_parameters()
                targets = []

            for target in targets:
                enable_target = False
                if enabled:
                    enable_target = True
                elif partial:
                    enable_target = (name in {'monitors', 'output'} or
                                     (name in {'boundary_conditions', 'point_sources',
                                               'internal_surfaces', 'numerics', 'run'}
                                      and not target.objectName().startswith('frame_add')))
                pane.partial_input = partial
                pane.input_enabled = enable_target

                target.setEnabled(enable_target)
        # Don't allow changing type of BC.  FIXME, there are a bunch of other
        # items that should be disabled on a per-item basis, should use the 'locked'
        # metadata in keyword_doc
        # TODO Move this to bcs.py
        bcs = self.ui.boundary_conditions
        if not enabled:
            for item in (bcs.combobox_bc_type, # disallow changing wall type type
                         bcs.label_bc_type,
                         bcs.combobox_solids_wall_type,
                         bcs.label_solids_wall_type,
                         bcs.combobox_fluid_wall_type,
                         bcs.label_fluid_wall_type,
                         bcs.toolbutton_add, # but these are managed by bcs_handle_selection!
                         bcs.toolbutton_delete, #see comment above
                         bcs.toolbutton_up,
                         bcs.toolbutton_down):
                item.setEnabled(False)
        else:
            for item in (bcs.combobox_bc_type, # Allow changing type
                         bcs.label_bc_type,
                         bcs.combobox_solids_wall_type,
                         bcs.label_solids_wall_type,
                         bcs.combobox_fluid_wall_type,
                         bcs.label_fluid_wall_type,
                         bcs.toolbutton_add):
                item.setEnabled(True)
            self.handle_bcs_region_selection() # Update add/delete/up/down buttons

        # Update VTK pane and subpanes (move to output.py:enable_input)
        self.handle_output_vtk_region_selection()
        self.handle_output_usr_region_selection()

        # Don't allow changing MONITOR_TYPE
        for item in (self.ui.monitors.combobox_monitor_type_cell,
                     self.ui.monitors.combobox_monitor_type_particle):
            item.setEnabled(enabled)

        # Don't allow changing RUN_NAME while paused.
        item = self.ui.run.lineedit_keyword_run_name
        item.setEnabled(enabled)

        # Don't allow enable/disable reactions
        tw = self.ui.chemistry.tablewidget_reactions
        for i in range(0, tw.rowCount()):
            tw.cellWidget(i,0).setEnabled(enabled) # checkbox

        self.setup_current_pane()

    def slot_update_status(self):
        self.status_manager.update_status_if_changed()

    # This is called by several different signals for different reasons
    # 1) executables changed
    # 2) project directory changed
    # 3) process started
    # 4) process stopped
    def slot_update_runbuttons(self, message=None):
        self.status_manager.update_runbuttons(message)

    def resizeEvent(self, event):
        '''over-ride of qt resize event'''
        if self.file_menu.isVisible():
            self.file_menu.setGeometry(0, 0, self.width(), self.height())

        QtWidgets.QMainWindow.resizeEvent(self, event)

    def closeEvent(self, event):
        '''override of qt close event'''
        if not self.confirm_close():
            event.ignore()
            return

        # check nodeworks local queue and stop
        if self.ui.nodeworks_widget.NODEWORKS_AVAILABLE:
            if not self.ui.nodeworks_widget.confirm_close():
                event.ignore()
                return

        # save global state
        SETTINGS.setValue('geometry', self.saveGeometry())
        SETTINGS.setValue('splitter_left_right', self.ui.splitter_left_right.sizes())
        SETTINGS.setValue('splitter_graphics_cmd_output', self.ui.splitter_graphics_cmd_output.sizes())

        # Goodbye world
        os._exit(0)


    def find_navigation_tree_item(self, item_name):
        item_name = item_name.lower()
        tree = self.ui.treewidget_navigation
        flags = Qt.MatchFixedString | Qt.MatchRecursive # Case-insensitive`
        items = tree.findItems(item_name, flags, 0)
        if len(items) == 1:
            return items[0]
        else:
            for (long, short) in self.nav_labels:
                if item_name == long.lower():
                    items = tree.findItems(short, flags, 0)
                    if len(items) == 1:
                        return items[0]


    def register_keyword_widgets(self):
        """Look for and connect keyword widgets to the project manager.
        Keyword information from the namelist doc strings is added to each
        keyword widget. The widget must be named: *_keyword_<keyword> where
        <keyword> is the actual keyword.
        Args are also supported via widgetname_keyword_KEY_args_ARGS"""

        def try_int(str):
            try:
                return int(str)
            except ValueError:
                return str

        # loop through all widgets looking for *_keyword_<keyword>
        for widget in widget_iter(self):
            name_list = str(widget.objectName()).split('_')
            if name_list[0] == 'label': # Note no 'keyword'
                if 'units' in name_list:
                    continue # Don't set key on unit labels
                if 'args' in name_list:
                    args_idx = name_list.index('args')
                    args = [try_int(name) for name in name_list[args_idx+1:]]
                    key = '_'.join(name_list[1:args_idx])
                else:
                    if name_list[-1].isdigit(): # strip suffix
                        name_list = name_list[:-1]

                    args = None
                    key = '_'.join(name_list[1:])
                if not widget.toolTip():
                    if args:
                        self.add_tooltip(widget, key, value=args[0])
                    else:
                        self.add_tooltip(widget, key)

            elif 'keyword' in name_list:
                key_idx = name_list.index('keyword')
                args = None
                # Look for _args_ following <keyword>
                if 'args' in name_list:
                    args_idx = name_list.index('args')
                    args = [try_int(name) for name in name_list[args_idx+1:]]
                    key = '_'.join(name_list[key_idx+1:args_idx])
                else:
                    key = '_'.join(name_list[key_idx+1:])

                # sometimes multiple widgets point to the same key ...
                # name them widget_keyword_KW_1, _2, etc
                if key not in self.keyword_doc:
                    name_list = key.split('_')
                    if name_list[-1].isdigit():
                        base_key = '_'.join(name_list[:-1])
                        if base_key not in self.keyword_doc:
                            log.error("UNKNOWN KEYWORD %s: not registering %s (also tried %s)" %
                                      (key, widget.objectName(), base_key))
                            continue
                        key = base_key

                if len(keyword_args.get(key, [])) != len(args or []):
                    self.error("keyword args mismatch: key=%s: expected %s, got %s" %
                               (key, keyword_args.get(key), args))

                # set the key attribute to the keyword
                widget.key = key
                widget.args = args

                # add info from keyword documentation
                if key in self.keyword_doc:
                    doc = self.keyword_doc[key]
                    try:
                        widget.setdtype(doc['dtype'])
                    except:
                        print(widget, widget.objectName())
                        raise
                    vr = doc.get('validrange', {})

                    # We're going to ignore the 'required' from keyword doc since
                    # it does not match the SRS.  We will set 'required' fields
                    # manually, in accordance with the SRS
                    #required=doc.get("required"))

                    # preserve any range info which is already setup
                    if vr:
                        if widget.min is None:
                            widget.min = vr.get('min')
                        if widget.max is None:
                            widget.max = vr.get('max')
                        if widget.exclude_min is None:
                            widget.exclude_min = vr.get('exclude_min', False)
                        if widget.exclude_max is None:
                            widget.exclude_max = vr.get('exclude_max', False)

                    # Set widget defaults from 'initpython' keys
                    default = doc.get('initpython')
                    if default is not None:
                        widget.default(default)
                    if not widget.toolTip():
                        self.add_tooltip(widget, key)

                    # NB not all widgets get set up this way
                    if isinstance(widget, QtWidgets.QLineEdit) and widget.dtype in (int, float):
                        widget.allow_parameters = True
                    elif isinstance(widget, QtWidgets.QComboBox) and widget.count() < 1:
                        widget.addItems(list(doc['valids'].keys()))

                    elif isinstance(widget, QtWidgets.QCheckBox):
                        # TODO: infer which value is true/false?
                        values = sorted([convert_string_to_python(v) for v in doc['valids'].keys()])
                        if len(values) == 2:
                            widget.false_value = values[0]
                            widget.true_value = values[1]

                else:
                    log.error("UNKNOWN KEYWORD %s: not registering %s" % (key, widget.objectName()))
                    continue

                # register the widget with the project manager
                self.project.register_widget(widget, keys=[key], args=args)


    def init_vtk_widget(self, load_vtk=True):
        #initialize the vtk widget
        disable_vtk = False
        if load_vtk and 'MFIX_NO_VTK' not in os.environ: # Avoid importing vtkwidget if MFIX_NO_VTK set
            from .vtk_widgets import VTK_AVAILABLE, VTK_IMPORT_INFO
            disable_vtk = not VTK_AVAILABLE
        else: # env var set
            disable_vtk = True
            VTK_IMPORT_INFO = None

        if disable_vtk:
            log.info("MFIX_NO_VTK set or vtk not importable, creating fake VTK")
            # Create a dummy object, so we don't have to test for 'if use_vtk' all over
            class FakeVtk:
                def noop(self, *args, **kwargs):
                    return None
                def __getattr__(self, key):
                    return self if key == 'vtkiren' else self.noop

            self.vtkwidget = FakeVtk()
            self.ui.regions.vtkwidget = self.vtkwidget

            # build widgets to display/copy message
            spacer = QtWidgets.QSpacerItem(10, 100,
                                           QtWidgets.QSizePolicy.Maximum,
                                           QtWidgets.QSizePolicy.Expanding)
            vtk_message = QtWidgets.QWidget()
            vtk_message_l = QtWidgets.QVBoxLayout(vtk_message)
            vtk_message_l.addItem(spacer)
            message = '\n'.join(VTK_IMPORT_INFO) if VTK_IMPORT_INFO is not None else 'VTK disabled.'
            label = QtWidgets.QLabel(message)
            set_alignment(label, Qt.AlignLeft)
            vtk_message_l.addWidget(label)

            if VTK_IMPORT_INFO is not None:
                def copy():
                    cp = self.app.clipboard()
                    cp.setText(message)
                cpy_btn = QtWidgets.QPushButton('Copy Error')
                cpy_btn.pressed.connect(copy)
                vtk_message_l.addWidget(cpy_btn)

            vtk_message_l.addItem(spacer)

            self.ui.horizontalLayoutModelGraphics.addWidget(vtk_message)
            return

        from .vtk_widgets.modeler import VtkWidget
        self.vtkwidget = VtkWidget(parent=self)
        self.ui.horizontalLayoutModelGraphics.addWidget(self.vtkwidget)

        # remove close btn
        tw = self.ui.tabWidgetGraphics
        rb = tw.tabBar().tabButton(0, QtWidgets.QTabBar.RightSide)
        if rb:
            rb.resize(0, 0)
        lb = tw.tabBar().tabButton(0, QtWidgets.QTabBar.LeftSide)
        if lb:
            lb.resize(0, 0)

        # register with project manager
        self.project.register_widget(
            self.vtkwidget, ['x_min', 'x_max', 'y_min', 'y_max', 'z_min',
                             'Z_max', 'imax', 'jmax', 'kmax', 'no_k'])

        # add reference to other widgets
        self.ui.regions.vtkwidget = self.vtkwidget

    def init_nodeworks_widget(self, load_nodeworks=True):
        # initialize the nodeworks widgets if nodeworks is available
        if load_nodeworks:
            from .widgets.nodeworks import NodeworksWidget, NODEWORKS_AVAILABLE
            if NODEWORKS_AVAILABLE:
                self.ui.nodeworks_widget = NodeworksWidget(self.project, self)
                self.ui.nodeworks_widget.NODEWORKS_AVAILABLE = True
                self.ui.verticallayout_nodeworks_mode.addWidget(
                    self.ui.nodeworks_widget)
            else:
                self.make_fake_nodeworks()
        else:
            self.make_fake_nodeworks()

    def make_fake_nodeworks(self):
        class FakeNodeworks:
            NODEWORKS_AVAILABLE = False
            def noop(self, *args, **kwargs):
                return None
            def __getattr__(self, key):
                return self if key == 'nodeChart' else self.noop

        self.ui.nodeworks_widget = FakeNodeworks()
        self.ui.pushbutton_nodeworks.setEnabled(False)
        self.ui.pushbutton_nodeworks.setToolTip(
            "Nodeworks disabled, can't import nodeworks")

    def init_editor_widget(self):
        w = self.editor_widget = IDEWidget(self.ui.editor)
        w.file_changed.connect(self.handle_file_changed)
        w.needs_saved_signal.connect(self.set_unsaved_flag)
        w.tree.open_project_button.hide()
        w.help_request.connect(self.handle_search)
        self.ui.gridLayout_editor.addWidget(w)

    def init_history(self):
        tb = self.ui.textbrowser_history
        tb.setOpenLinks(False)
        tb.anchorClicked.connect(vc.handle_click)


    @classmethod
    def get_project_file(cls):
        """get the project filename, including full path"""
        return cls.project_file if cls.project_file else None

    @classmethod
    def set_project_file(cls, value):
        cls.project_file = value = os.path.normpath(str(value)) if value else None
        SETTINGS.setValue('project_file', value)

    def get_project_dir(self):
        """get the current project directory"""
        project_file = self.get_project_file()
        return os.path.dirname(project_file) if project_file else None

    def handle_file_changed(self, fname):
        if fname == self.project.usr_init:
            self.keyword_doc.clear() # Remove usr keys, keep ref
            self.project.keyword_doc.clear()
            self.project.keyword_doc.update(self.orig_keyword_doc)
            self.keyword_doc.update(self.saved_keyword_doc)
            reset_keyword_args()

            # Lifted from project_manager.py  TODO reduce duplication
            with warnings.catch_warnings(record=True) as ws:
                with open(fname, encoding='utf-8', errors='replace') as f:
                    data = f.read()
                    self.project.usr_keyword_doc = parse(data)
                    self.project.keyword_doc.update(self.project.usr_keyword_doc)
                    self.keyword_doc.update(self.project.usr_keyword_doc)
                    for (k,v) in self.project.usr_keyword_doc.items():
                        args = v.get('args', {})
                        args = [a.get('id', '?').lower() for a in args.values()]
                        add_keyword_args(k, args)
            for w in ws:
                msg = '%s: %s' % (fname, str(w.message)
                                  .replace('&', '&amp;')
                                  .replace('<', '&lt;')
                                  .replace('>', '&gt;'))
                self.warning(msg, popup=True)
            self.setup_advanced()

    def change_mode(self, mode):
        """change the Modeler, Nodeworks, Editor tab"""
        ui = self.ui
        mode = mode.lower()

        if mode == self.mode:
            return

        if mode in self.modebuttons.keys():
            to_index = list(self.modebuttons.keys()).index(mode)
        else:
            self.error("Invalid mode %s" % mode)
            return

        self.mode = mode
        for key, btn in self.modebuttons.items():
            btn.setChecked(mode == key)
            font = btn.font()
            font.setBold(mode == key)
            btn.setFont(font)

        if mode == 'interpreter':
            self.capture_output(True)
            self.setup_interpreter()
        else:
            self.capture_output(False)

        nodeworks = (mode == 'nodeworks')
        if ui.nodeworks_widget is None:
            ui.nodeworks_widget.NODEWORKS_AVAILABLE = False

        if ui.nodeworks_widget.NODEWORKS_AVAILABLE:
            nc = ui.nodeworks_widget.nodeChart
            for btn in nc.enable_disable_btns:
                btn.setVisible(nodeworks)

        for btn in (ui.toolbutton_reset_mfix, ui.toolbutton_run_mfix,
                    ui.toolbutton_pause_mfix, ui.toolbutton_stop_mfix):
            btn.setVisible(not nodeworks)

        for item in (ui.regions.label_bc_type, ui.regions.combobox_bc_type):
            item.setVisible(mode == 'mesher')

        tw = ui.treewidget_navigation
        if mode == 'mesher':
            self.set_solver(self.project.solver) # Reset titlebar & status message
            l = ui.mesher.layout()
            if getattr(ui.mesher, 'dummy_label', None):
                # Remove dummy label
                l.removeWidget(ui.mesher.dummy_label)
                ui.mesher.dummy_label.deleteLater()
                ui.mesher.dummy_label = None
            if l.count() == 0:
                # Steal widgets from modeler
                l1 = ui.modeler.layout()
                p = QtGui.QPixmap(ui.modeler.size())
                p.fill(ui.modeler.palette().color(QtGui.QPalette.Window))
                ui.modeler.render(p, flags=QtWidgets.QWidget.DrawChildren) # avoid rendering bg
                ui.modeler.dummy_label = label = QtWidgets.QLabel()
                label.setPixmap(p)
                for w in (ui.treewidget_navigation,
                          ui.toolbutton_collapse_navigation,
                          ui.task_pane):
                    l1.removeWidget(w)
                    l.insertWidget(l.count(), w)
                l1.insertWidget(0, label)

            for i in range(tw.topLevelItemCount()):
                item = tw.topLevelItem(i)
                item.setHidden(item.text(0) not in ('Geometry', 'Mesh', 'Regions'))
            # Don't stay on disabled tab
            if self.curr_nav_label:
                item = self.find_navigation_tree_item(self.curr_nav_label)
            else:
                item = None
            if ((item and (item.isDisabled() or item.isHidden()))
                or not item):
                self.change_pane('geometry')


        elif mode == 'modeler': # and self.sms_mode:  User might have disabled SMS
            self.set_solver(self.project.solver) # Reset titlebar & status message
            l = ui.modeler.layout()
            if getattr(ui.modeler, 'dummy_label', None):
                # Remove dummy label
                l.removeWidget(ui.modeler.dummy_label)
                ui.modeler.dummy_label.deleteLater()
                ui.modeler.dummy_label = None
            if l.count() == 0:
                # Steal widgets from mesher
                l1 = ui.mesher.layout()
                p = QtGui.QPixmap(ui.mesher.size())
                p.fill(ui.modeler.palette().color(QtGui.QPalette.Window))
                ui.mesher.render(p, flags=QtWidgets.QWidget.DrawChildren) # avoid rendering bg
                ui.mesher.dummy_label = label = QtWidgets.QLabel()
                label.setPixmap(p)

                for w in (ui.treewidget_navigation,
                          ui.toolbutton_collapse_navigation,
                          ui.task_pane):
                    l1.removeWidget(w)
                    l.insertWidget(l.count(), w)
                l1.insertWidget(0, label)
            for i in range(tw.topLevelItemCount()): # Geometry and Mesh not shown in SMS/Modeler
                item = tw.topLevelItem(i)
                item.setHidden(self.sms_mode and item.text(0) in ('Geometry', 'Mesh'))
            if self.curr_nav_label:
                item = self.find_navigation_tree_item(self.curr_nav_label)
            else:
                item = None
            if (item and (item.isDisabled() or item.isHidden())
                or not item):
                self.change_pane('model setup')

        if mode == 'history':
            vc.update(self.ui.textbrowser_history)

        self.animate_stacked_widget(ui.stackedwidget_mode,
                                    ui.stackedwidget_mode.currentIndex(),
                                    to_index)

        if mode in ('modeler', 'mesher'): # open navigation menu whenever we go back to Modeler mode
            ui.treewidget_navigation.setVisible(True)
            self.ui.toolbutton_collapse_navigation.setIcon(
                self.icon_collapse_left)

    # --- modeler pane navigation ---
    def change_pane(self, name, allow_disabled_tab=False):
        self.allow_disabled_tab = allow_disabled_tab
        name = name.replace('_', ' ')
        """set current pane to the one matching 'name'.  Must be the long
        (non-abbreviated) navigation label.  Case-insensitive"""
        items = self.ui.treewidget_navigation.findItems(
            name,
            Qt.MatchFixedString | Qt.MatchRecursive, 0)
        if not items: # Nav menu may be in abbreviated mode.  Might be better
            # to identify navigation items by something other than text, since
            # that can change (long/short) and is possibly non-unique (eg "points")
            for (long, short) in self.nav_labels:
                if name.lower() == long.lower():
                    items = self.ui.treewidget_navigation.findItems(
                        short,
                        Qt.MatchFixedString | Qt.MatchRecursive, 0)
                    if items:
                        break
        if len(items) == 1:
            item = items[0]
            if item.isDisabled() or item.isHidden():
                self.error("Pane %s is disabled" % name)
                return False
            self.ui.treewidget_navigation.setCurrentItem(item)
        else:
            self.error("Cannot find pane %s" % name)
        #self.navigation_changed() # not needed, since setCurrentItem triggers callback
        self.allow_disabled_tab = False
        return True


    def navigation_changed(self):
        """an item in the tree was selected, change panes"""
        current_selection = self.ui.treewidget_navigation.selectedItems()
        if not current_selection:
            return None
        self.curr_nav_label = name = str(current_selection[0].text(0))
        # Translate from short to long name
        for (long, short) in self.nav_labels:
            if name == short:
                name = long
                break

        name = '_'.join(name.lower().split(' '))
        matches = [i for i in range(self.ui.task_pane.count())
                   if self.ui.task_pane.widget(i).objectName() == name]
        assert len(matches) == 1
        to_index = matches[0]

        self.animate_stacked_widget(
            self.ui.task_pane,
            self.ui.task_pane.currentIndex(),
            to_index,
            direction='vertical')

        self.setup_current_pane()

    def setup_current_pane(self):
        # Force any open popup to close
        # (if dialog is modal we don't need this)
        self.species_popup.done(0)
        self.regions_popup.done(0)
        self.advanced_popup.done(0)

        current_selection = self.ui.treewidget_navigation.selectedItems()
        if not current_selection:
            return
        text = str(current_selection[-1].text(0))

        # Translate from short to long name
        for (long, short) in self.nav_labels:
            if text == short:
                text = long
                break

        text = '_'.join(text.lower().split(' '))
        # Make sure panes are properly initialized as we change to them.  This way
        # we do not have to worry so much about inter-pane state dependencies

        setup_pane = { 'regions': self.ui.regions.setup_regions,
                       'model_setup': self.setup_model_setup,
                       'fluid': self.setup_fluid,
                       'solids': self.setup_solids,
                       'scalars': self.setup_scalars,
                       'initial_conditions': self.setup_ics,
                       'boundary_conditions': self.setup_bcs,
                       'point_sources': self.setup_pss,
                       'internal_surfaces': self.setup_iss,
                       'chemistry': self.setup_chemistry,
                       'numerics': self.setup_numerics,
                       'output': self.setup_output,
                       'monitors': self.setup_monitors,
                       'mesh': self.setup_mesh,
                       'geometry': self.setup_geometry,
                       'advanced': self.setup_advanced}.get(text)
        if setup_pane:
            setup_pane(allow_disabled_tab=self.allow_disabled_tab)
        self.allow_disabled_tab = False

    # --- animation methods ---
    def animate_stacked_widget(self, stackedwidget, from_, to,
                               direction='horizontal', line=None, to_btn=None,
                               btn_layout=None):
        """animate changing of qstackedwidget"""

        # check to see if already animating
        if self.animating and self.stack_animation is not None:
            self.stack_animation.stop()

        from_widget = stackedwidget.widget(from_)
        to_widget = stackedwidget.widget(to)

        # get from geometry
        width = from_widget.frameGeometry().width()
        height = from_widget.frameGeometry().height()

        # offset
        # bottom to top
        if direction == 'vertical' and from_ < to:
            offsetx = 0
            offsety = height

        # top to bottom
        elif direction == 'vertical' and from_ > to:
            offsetx = 0
            offsety = -height
        elif direction == 'horizontal' and from_ < to:
            offsetx = width
            offsety = 0
        elif direction == 'horizontal' and from_ > to:
            offsetx = -width
            offsety = 0
        else:
            return #?

        self.stack_animation = QtCore.QParallelAnimationGroup()

        if from_ != to:
            # move to widget and show
            # set the geometry of the next widget
            to_widget.setGeometry(0 + offsetx, 0 + offsety, width, height)
            to_widget.show()
            to_widget.raise_()
            #to_widget.activateWindow() ? needed?

            # animate
            # from widget
            self.animation_setup(from_widget, 0, 0, -offsetx, -offsety)
            # to widget
            self.animation_setup(to_widget, offsetx, offsety, 0, 0)

        # line
        line_to = None
        if line is not None and to_btn is not None:
            self.animation_setup(line, line.geometry().x(), line.geometry().y(),
                                 to_btn.geometry().x(), line.geometry().y(),
                                 to_btn.geometry().width())
            if btn_layout is not None:
                for i in range(0, btn_layout.columnCount()):
                    if btn_layout.itemAtPosition(0, i) == to_btn:
                        line_to = i
                        break

        # animation group
        # call back for when the animation is finished/canceled.
        self.stack_animation.stateChanged.connect(lambda: self.animate_stacked_widget_finished(
            stackedwidget, from_, to, btn_layout, to_btn, line, line_to))

        self.animating = True
        self.stack_animation.start()

    def animation_setup(self, target, x_start, y_start, x_end, y_end, width_end=None):
        """setup an animation widget"""
        animation = QtCore.QPropertyAnimation(target, b"pos")
        animation.setDuration(animation_speed())
        animation.setEasingCurve(QtCore.QEasingCurve.InOutQuint)
        animation.setStartValue(QtCore.QPoint(x_start, y_start))
        animation.setEndValue(QtCore.QPoint(x_end, y_end))
        self.stack_animation.addAnimation(animation)

        # resize line width
        if width_end is not None:
            animation = QtCore.QPropertyAnimation(target, b"size")
            animation.setDuration(animation_speed()),
            animation.setEasingCurve(QtCore.QEasingCurve.InOutQuint)
            size = target.size()
            animation.setStartValue(size)
            animation.setEndValue(QtCore.QSize(width_end, size.height()))
            self.stack_animation.addAnimation(animation)

    def animate_stacked_widget_finished(self, widget, from_, to,
                                        btn_layout=None, to_btn=None, line=None, line_to=None):
        """cleanup after animation"""
        try:
            if self.stack_animation.state() == QtCore.QAbstractAnimation.Stopped:
                widget.setCurrentIndex(to)
                if from_ != to:
                    from_widget = widget.widget(from_)
                    from_widget.hide()
                    from_widget.move(0, 0) #why?
                if btn_layout is not None and line is not None:
                    btn_layout.addItem(btn_layout.takeAt(
                        btn_layout.indexOf(line)), 1, line_to or to)
        except AttributeError: # Happens during shutdown. TODO: remove this hack
            pass
        finally:
            self.animating = False

    # --- helper methods ---
    def message(self,
                title='Warning',
                icon='warning',
                text='',
                buttons=['ok'],
                default='ok',
                info_text=None,
                detailed_text=None,
                traceback_text=None,
                post_traceback_text="",
                print_console=True):

        """Create and display a modal message box, also prints message in console
        title:  displayed in frame of message box
        text:  string, displayed in flowed format, variable width font, HTML
        icon:  'warning' or 'question', anything else is displayed as 'information'
        buttons: list of values, 'ok', 'yes', 'no', 'cancel','discard'
        default: 'ok' the default selected button
        info_text:  shown in message box as QtInformativeText (see QT docs)
        detailed_text:  shown in message box, requires "view details" button click
        traceback_text: string, displayed in monospace font, unflowed, no HTML

        Return the pressed button."""

        # Print messages in console
        if print_console:
            if traceback_text: # Avoid printing HTML tags in console window
                # TODO:  enable HTML for console?  Or format HTML to text?
                #self.print_internal(title, color='red')
                # This just prints 'Application error', so why bother?
                pass

            elif not any(title.lower().startswith(x)
                         for x in ('save', 'delete')):
                self.print_internal(title + ": " + text)

            for t in (info_text, detailed_text, traceback_text):
                if t:
                    self.print_internal(t)

        message_box = QtWidgets.QMessageBox(self)
        self.message_box = message_box # Make it accessible to tests
        message_box.setWindowTitle(title)
        message_box.setTextFormat(Qt.RichText) # support HTML

        # Icon
        if icon == 'warning':
            icon = QtWidgets.QMessageBox.Warning
        elif icon == 'question':
            icon = QtWidgets.QMessageBox.Question
        else:
            icon = QtWidgets.QMessageBox.Information

        message_box.setIcon(icon)

        # Text
        text = text.replace('\n', '<br>')# the joys of HTML
        if traceback_text:
            message_box.setText(text + "<pre>" + traceback_text + "</pre>" + post_traceback_text)
        else:
            message_box.setText(text)

        if info_text:
            message_box.setInformativeText(info_text)

        if detailed_text:
            message_box.setDetailedText(detailed_text)

        qbuttonDict = {'ok':      QtWidgets.QMessageBox.Ok,
                       'yes':     QtWidgets.QMessageBox.Yes,
                       'no':      QtWidgets.QMessageBox.No,
                       'cancel':  QtWidgets.QMessageBox.Cancel,
                       'discard': QtWidgets.QMessageBox.Discard}

        for b in buttons:
            button = qbuttonDict.get(b)
            #if not button:
            #    button = QtWidgets.QPushButton(b)
            #    role = QtWidgets.QMessageBox.AcceptRole # Seems to be OK to use for all buttonsrole)
            #message_box.addButton(button, role)
            #    qbuttonDict[b] = button
            #else:
            message_box.addButton(button)

            if b == default:
                message_box.setDefaultButton(button)

        ret = message_box.exec_()

        for (k, v) in qbuttonDict.items():
            if v == ret:
                return k

    def print_internal(self, line, color=None, font=None, advance=True):
        self.console_printer.print_internal(line, color, font, advance)

    def remove_output_files(self, output_files=None, message_text=None, force_remove=False):
        """ remove MFIX output files from current project directory

        output_files: List of patterns (?) to be matched for file removal
        message_text: Text to be displayed on the popup
        force_remove: Don't allow user to uncheck categories
        return: True for success, False for user cancel"""

        if output_files in (None, False): # "False" arg comes from button click signal
            output_files = self.get_output_files()

        if message_text is None:
            message_text = 'Reset the project by deleting the following files.'

        ok_to_delete = self.output_selection_popup.exec_(
            output_files, self.get_project_dir(), force_remove, message_text)

        if not ok_to_delete:
            return False

        # stop the monitor_reader threads
        self.monitor_reader.stop_threads()

        for path in self.output_selection_popup.get_output_files():
            try:
                os.remove(path)
            except OSError as err:
                if err.errno == errno.ENOENT:
                    pass # File already deleted
                else:
                    msg = 'Cannot delete %s: %s' % (path, err.strerror)
                    self.error(msg, popup=True)
                    break

        self.slot_update_runbuttons()

        # resume the monitor_reader threads
        self.monitor_reader.resume_threads()

        return True

    def get_output_files(self, patterns=None):
        """ get the output files of an MFIX solver job """
        project_dir = self.get_project_dir()
        if not project_dir:
            return
        if not patterns:
            patterns = RESTART_FILES + SPX_FILES + VTK_FILES + OTHER_FILES

            # collect monitor names
            prj = self.project
            margs = prj.get_key_indices('monitor_name')
            mnames = [prj.get_value('monitor_name', args=arg) + '.csv' for arg in margs]
            # collect log file names
            mnames.extend(v[1] + '.csv' for v in self.output_logs.values())
            mnames.extend(v[1] + '.txt' for v in self.output_logs.values())
            patterns.extend(mnames)

        vtu_dir = self.project.get_value('vtu_dir', default=None)

        outputs = []
        for p in patterns:
            pi = case_insensitive(p)
            outputs.extend(glob.glob(os.path.join(project_dir, pi)))
            if vtu_dir not in (None, '', '.'):
                outputs.extend(glob.glob(os.path.join(project_dir, case_insensitive(vtu_dir), pi)))

        # issue/1069: MESH.vtu, boundary.vtk, MESH_STATS.LOG, *.msh
        exclude_patterns = []
        if self.sms_mode and self.mesh_accepted:
            exclude_patterns.extend(['*MESH.vtu', '*_boundary.vtk', 'MESH_STATS.LOG', '*.msh'])
        # other states that have different requirements?
        exclude_files = [fnmatch.filter(outputs, ep) for ep in exclude_patterns]

        # filter out specific files
        for fnames in exclude_files:
            for fname in fnames:
                outputs.remove(fname)

        return outputs

    def get_res_files(self):
        return self.get_output_files(patterns=['*.res'])

    def handle_run(self):
        self.ui.responses.setText("")  # Clear old messages
        paused = self.job_manager.is_job_paused()
        word = 'resuming' if paused else 'starting'

        if paused:
            if self.unsaved_flag:
                self.confirm_save(title="Save?",
                                  icon='question',
                                  text="Must save project before %s run. Save?" % word,
                                  autostart=True)
                # Do not need to unpause with autostart.  Either user canceled save,
                # or job started ...
                return
            elif self.mtime > self.job_manager.job.mtime:
                self.handle_reinit(autostart=True)
                return
            else:
                self.print_internal("Unpausing...", color='blue')
                self.job_manager.job.unpause()
        else:
            if not self.confirm_save(title="Save?",
                                     icon='question',
                                     text="Must save project before %s run. Save?" % word):
                return
            # Job is started from run dialog
            open_run_popup(self)

        self.slot_update_runbuttons()


    def handle_pause(self):
        self.print_internal("Pausing MFiX...", color='blue')
        #self.set_stop_button(enabled=False) # Leave enabled so we can force-kill
        self.job_manager.pause_job()
        self.slot_update_status() # force status update
        self.update_window_title()


    def handle_reinit(self, autostart=False):
        # The "job is not None" checks should be delegated to the
        #  job_manager
        warning = None
        if not self.job_manager.job:
            warning = "No job"
        elif not self.job_manager.job.is_paused():
            warning = "Job is not paused"
        elif self.job_manager.is_job_pending():
            warning = "Job is pending"
        if warning:
            self.warning("reinit: " + warning)
            return

        # issues/414 filter out keyword DT
        #  FIXME, there are a bunch of "locked"/"unlocked" keys, we should
        #  treat them all the same, not just DT
        # TODO use 'locked/unlocked' attributes of keyword
        dt_pat = re.compile(r'\s*dt\s*=', re.IGNORECASE)
        def ok_for_reinit(line):  # No need to send comments, blank lines, etc
            line = line.strip()
            return not (
                line == '' or
                line.startswith('!') or
                line.startswith('#') or
                dt_pat.match(line))

        mfix_dat = "\n".join(
            sorted(
                filter(
                    ok_for_reinit, (key.line() for key in self.project.keywordItems())
                )
            )
            + list(chemistry_lines())
            + list(thermo_data_lines())
        )

        self.job_manager.job.reinit(mfix_dat, autostart=autostart)

        # TODO FIXME better success/failure reporting for reinit!
        # Since everything is async (both HTTP, and the reinit_flag handling in
        #  the solver), this requires some work
        # For now, assume that the reinit succeeded, and if we get a
        #  reinit failed message, we reset the job's mtime
        # Setting job.mtime=self.mtime on 'reinit succeeded' would not work b/c
        #  self.mtime might have advanced before that message came back
        # It would be good to include the mtime in the payload, that's the only
        #  way to track it reliably
        self.job_manager.job.mtime = self.mtime #XXX should only do this for successful reinit!

    def handle_stop(self):
        self.print_internal("STOP", color='red')
        self.status_manager.set_pause_button(enabled=False)
        self.job_manager.stop_mfix()
        QtCore.QTimer.singleShot(3000, self.job_manager.stop_mfix)

    def handle_force_stop(self, *args):
        self.print_internal("FORCE STOP", color='red')
        self.status_manager.set_pause_button(enabled=False)
        self.job_manager.force_stop_mfix()

    def confirm_save(self, title="Save?", text="Save project?",
                     icon='question', autostart=False):
        if self.unsaved_flag:
            response = self.message(title=title,
                                    icon=icon,
                                    text=text,
                                    buttons=['ok', 'cancel'])
            if response != 'ok':
                return False
            self.handle_save(autostart=autostart)
        return True

    def navigate_all(self):
        """iterate over all navigation panes, selecting each
        row of each table, to make sure associated keywords
        are all set"""
        tw = self.ui.treewidget_navigation
        r = tw.invisibleRootItem()
        for i in range(r.childCount()):
            c = r.child(i)
            gui.change_pane(c.text(0))
            # we should cycle through all the subtabs, too
            if gui.project.solver in ('DEM','CGP'):
                gui.setup_solids_dem_tab()
            elif gui.project.solver == 'TFM':
                gui.setup_solids_tfm_tab()
            elif gui.project.solver == 'PIC':
                gui.setup_solids_pic_tab()

        for t in (0,2,4,5):
            gui.setup_bcs_tab(t)

        for w in widget_iter(self):
            if isinstance(w, QtWidgets.QTableWidget):
                for r in range(0, w.rowCount()):
                    w.setCurrentCell(r, 0)


    def save_project(self, filename=None):
        """save project, optionally as a new project.

        project_file: string, full filename of project (including path)
        return: None"""

        if filename:
            project_dir = os.path.dirname(filename)
            project_file = filename
        else:
            project_dir = self.get_project_dir()
            project_file = self.get_project_file()

        if project_dir is None or project_file is None:
            return

        # save version
        v = self.project.mfix_gui_comments.get('project_version', 0)
        try:
            v = int(v)
        except Exception as e:
            self.error('Invalid project_version %s, resetting to 0'%v)
            v = 0
        version = str(1 + v)
        self.update_keyword('project_version', version)
        self.project.mfix_gui_comments['project_version'] = version
        self.project.mfix_gui_comments['gui_version'] = __version__
        self.project.mfix_gui_comments['project_notes'] = json.dumps(self.project_notes())

        # save users
        users = self.project.mfix_gui_comments.get('modified_by', [])
        if not isinstance(users, list):
            users = users.split('|')

        users.append(getuser())
        users = set(users)
        self.project.mfix_gui_comments['modified_by'] = '|'.join(users)

        # save geometry
        if self.vtkwidget.needs_saved:
            stl_path = os.path.join(project_dir, 'geometry.stl')
            self.print_internal("Info: Saving %s\n" % stl_path)
            self.vtkwidget.export_stl(stl_path)
        self.project.mfix_gui_comments['geometry'] = self.vtkwidget.geometry_to_str()
        self.project.mfix_gui_comments['visual_props'] = self.vtkwidget.visual_props_to_str()

        # save custom PSD files
        def copy_if_diff(src, dst):
            if not os.path.exists(dst):
                self.print_internal("Copy %s to %s" % (src, dst), color='blue')
                shutil.copyfile(src, dst)
            else:
                if open(src).read() != open(dst).read():
                    self.print_internal("Copy %s to %s" % (src, dst), color='blue')
                    shutil.copyfile(src, dst)

        for base in 'ic', 'bc':
            key = base + '_psd_type'
            data = self.ics if base == 'ic' else self.bcs
            for indices in self.project.get_key_indices(key):
                val = self.project.get_value(key, args=indices)
                I, P = indices
                if val == 'CUSTOM':
                    psd = data.get(I, {}).get('psd',{}).get(P)
                    if psd:
                        src = self.psd.get(psd,{}).get('filename')
                        dst =  '%s_PSD_%04d_%04d.txt' % (base.upper(), I, P)

                        if src:
                            if not os.path.exists(src):
                                self.error("File %s missing for PSD %s" % (src, psd),
                                           popup=True)
                            else:
                                try:
                                    if not os.path.exists(dst) or (open(src, 'rb').read() != open(dst, 'rb').read()):
                                        self.print_internal("Copy %s to %s" % (src, dst), color='blue')
                                        shutil.copyfile(src, dst)
                                except Exception as e:
                                    self.error(str(e), popup=True)

        # save regions
        self.project.mfix_gui_comments['regions_dict'] = self.ui.regions.regions_to_str()

        for (data, key) in ((self.bc_regions_to_str(), 'bc_regions'),
                            (self.bc_psd_to_str(), 'bc_distributions'),
                            (self.ic_regions_to_str(), 'ic_regions'),
                            (self.ic_psd_to_str(), 'ic_distributions'),
                            (self.is_regions_to_str(), 'is_regions'),
                            (self.ps_regions_to_str(), 'ps_regions'),
                            (self.vtk_regions_to_str(), 'vtk_regions'),
                            (self.usr_regions_to_str(), 'usr_regions'),
                            (self.monitor_regions_to_str(), 'monitor_regions'),
                            (self.output_logs_to_str(), 'log_files'),
                            (self.chemistry_to_str(), 'chemistry'),
                            (self.graphics_to_str(), 'graphics')):
            if data and data not in ('[]', '{}'):
                self.project.mfix_gui_comments[key] = data
            else:
                self.project.mfix_gui_comments.pop(key, None)

        # Save retained keys
        if self.retained_keys:
            self.project.mfix_gui_comments['retained_keys'] = ExtendedJSON.dumps(
                dict((format_key_with_args(*k), v)
                     for (k,v) in self.retained_keys.items()))
        else:
            self.project.mfix_gui_comments.pop('retained_keys', None)

        # Save unlocked advanced keys
        if self.unlocked_keys:
            self.project.mfix_gui_comments['unlocked_keys'] = ExtendedJSON.dumps(
                sorted(self.unlocked_keys))
        else:
            self.project.mfix_gui_comments.pop('unlocked_keys', None)

        # Save particle size distribution
        if self.psd:
            self.project.mfix_gui_comments['psd'] = ExtendedJSON.dumps(
                {k:self.psd[k] for k in sorted(self.psd)})
        else:
            self.project.mfix_gui_comments.pop('psd', None)

        # Trim unused species from THERMO_DATA
        species_keys = set()
        for p in range(1+len(self.solids)):
            key = 'species_g' if p==0 else 'species_s'
            nmax = len(self.fluid_species if p==0 else self.solids_species[p])
            for i in range(1, 1+nmax):
                species_keys.add(self.project.get_value(key, args=([p,i] if p else [i])).lower())

        for key in list(self.project.thermo_data.keys()):
            if key not in species_keys:
                # Do we need to report this to the user?
                entry = self.project.thermo_data[key][0][:18].strip()
                self.print_internal(entry, font='strikeout')
                del self.project.thermo_data[key]

        # collect project specific GUI state
        nav_name = self.curr_nav_label
        for (long, short) in self.nav_labels:
            if nav_name == short:
                nav_name = long
                break
        ui_state = {
            'mode': self.mode,
            'navigation': nav_name,
            'graphic_tab': self.ui.tabWidgetGraphics.currentIndex()
            }
        self.project.mfix_gui_comments['ui_state'] = json.dumps(ui_state)

        # Save .thumbnail file (preview)
        create_project_thumbnail(self)

        self.print_internal("Info: Saving %s\n" % project_file)
        template = SETTINGS.value('template', 'standard').lower()
        saved = False
        try:
            tmp_file = project_file + '.tmp'
            with warnings.catch_warnings(record=True) as ws:
                self.project.writeDatFile(tmp_file, template=template)
            for w in ws:
                self.warn(str(w.message), popup=True)
            if ws:
                self.print_internal("File saved with %s" % plural(len(ws), 'warning'), color='red')
            self.set_project_modified_time(project_file)
            os.replace(tmp_file, project_file)
            saved = True
        except Exception as e:
            self.error("Unexpected error saving project: '%s'.\nFile '%s' not modified "
                       % (e,project_file),
                       popup=True)

        # save nodeworks
        nww = self.ui.nodeworks_widget
        if nww.NODEWORKS_AVAILABLE and nww.needs_saved:
            workflow_file = os.path.abspath(os.path.join(project_dir, 'workflow.nc'))
            self.print_internal("Info: Saving %s\n" % workflow_file)
            nww.save(workflow_file)

        self.save_editor_files()
        self.save_recent_project_list()

        if saved:
            self.clear_unsaved_flag()
            try:
                r = git.Repo.init(path=project_dir)
                r.index.add(project_file)
                msg = 'version %s' % version
                r.index.commit(message=msg)
                r.create_tag(version)
            except Exception as e:
                self.error("Version control operation failed: %s" % e,
                           popup=True)

    def save_editor_files(self):
        # check editor files and save
        edit_tabs = self.editor_widget.tabs
        for i in range(edit_tabs.count()):
            tab = edit_tabs.widget(i)
            if hasattr(tab, 'editor') and tab.editor.unsaved_flag:
                self.print_internal("Info: Saving %s\n" % tab.editor.file_name)
                tab.save()

    def set_project_modified_time(self, project_file):
        mtime = os.path.getmtime(project_file)
        self.project.mfix_gui_comments['modified_time'] = datetime.datetime.strftime(
            datetime.datetime.fromtimestamp(mtime),
            '%Y-%m-%d %H:%M')

    def handle_save(self, autostart=False):
        # Save project, also update/reinit any paused job
        project_file = self.get_project_file()
        try:
            # TODO:  save backup copy, revert if reinit failed
            self.save_project()
            if self.job_manager.is_job_paused():
                if self.mtime > self.job_manager.job.mtime:
                    self.print_internal("Updating paused job", color='blue')
                    self.handle_reinit(autostart=autostart)

        except Exception as e:
            msg = 'Failed to save %s: %s: %s' % (project_file, e.__class__.__name__, e)
            traceback.print_exception(*sys.exc_info())
            self.message(title='Error',
                         icon='error',
                         text=msg,
                         buttons=['ok'],
                         default='ok')
            return
        self.hide_file_menu()

    def handle_parameters(self):
        """add/change parameters"""
        changed_params = self.parameter_dialog.get_parameters()
        if changed_params:
            self.set_unsaved_flag()
        self.update_parameters(changed_params)

    def update_parameters(self, changed_params):
        """update the changed parameters"""
        self.ui.regions.update_parameters(changed_params)
        self.vtkwidget.update_parameters(changed_params, render=self.open_succeeded)
        self.project.update_parameters(changed_params)

    def handle_compile(self):
        """compiling tool"""
        # make sure files in editor are saved, issue/973
        self.build_popup.popup()


    def handle_compile_finished(self):
        self.ui.toolbutton_compile.setEnabled(True)

    def update_window_title(self):
        title = self.solver_name or 'MFiX'
        project_file = self.get_project_file()
        if project_file:
            # add entire path to title, abbreviate user dir
            title += " - " + project_file.replace(os.path.expanduser('~'), '~')
            if self.unsaved_flag:
                title += '*'

            if self.job_manager.job:
                if not self.job_manager.job.is_paused():
                    if self.job_manager.stopping:
                        title += ', STOPPING'
                    elif self.job_manager.pausing:
                        title += ', PAUSING'
                    else:
                        title += ', RUNNING'
                    if self.job_manager.job.job_id is not None:
                        title += ', job %s'% self.job_manager.job.job_id
                    elif self.job_manager.job.mfix_pid is not None:
                        title += ', process %s'% self.job_manager.job.mfix_pid

                elif self.job_manager.job.is_paused():
                    title += ', PAUSED'
                elif self.get_res_files():
                    title += ', STOPPED, resumable' # Do we ever see this?

        self.setWindowTitle(title)

    def set_save_button(self, enabled):
        self.ui.toolbutton_save.setEnabled(enabled)

    def set_unsaved_flag(self):
        self.mtime = time.time()
        if not self.unsaved_flag:
            # For debugging problems where flag gets set during load
            #traceback.print_stack()
            pass

        self.unsaved_flag = True
        self.update_window_title()
        self.set_save_button(enabled=True)

    def clear_unsaved_flag(self):
        self.unsaved_flag = False
        self.set_save_button(enabled=False)
        # reinit support
        self.slot_update_runbuttons()

    def set_mesh_accepted(self):
        if self.project.mfix_gui_comments.get('mesh_accepted') != True:
            self.set_unsaved_flag()
            self.project.mfix_gui_comments['mesh_accepted'] = True
        if not self.mesh_accepted and self.sms_enabled:
            self.print_internal("Mesh accepted", color='blue')
        self.mesh_accepted = True
        self.update_keyword('ppo', False)
        self.ui.mesh.label_mesh_accepted.show()
        self.ui.mesh.pushbutton_accept_mesh.setDown(True)
        self.ui.pushbutton_modeler.setEnabled(True)
        self.ui.pushbutton_modeler.setToolTip(None)
        self.status_manager.set_run_button()

    def clear_mesh_accepted(self):
        if not self.sms_mode:
            self.unset_keyword('ppo')
            if self.project.mfix_gui_comments.pop('mesh_accepted', None) is not None:
                self.set_unsaved_flag()
            self.mesh_accepted = False
            self.status_manager.set_run_button()
            return
        if self.mesh_accepted:
            self.print_internal("Invalidating mesh", color='blue')
        self.mesh_accepted = False
        self.ui.mesh.label_mesh_accepted.hide()
        self.ui.mesh.pushbutton_accept_mesh.setDown(False)
        self.status_manager.set_run_button()
        if self.project.mfix_gui_comments.get('mesh_accepted') != False:
            self.set_unsaved_flag()
            self.project.mfix_gui_comments['mesh_accepted'] = False
        self.update_keyword('ppo', True)
        self.ui.pushbutton_modeler.setEnabled(False)
        self.ui.pushbutton_modeler.setToolTip("Mesh not accepted")
        if self.mode == 'modeler':
            self.change_mode('mesher')
            if self.curr_nav_label != 'Regions':
                self.change_pane('geometry') # ?
        self.set_solver(self.project.solver) #Update window title etc


    def check_writable(self, directory):
        """check whether directory is writable """
        try:
            testfile = tempfile.TemporaryFile(dir=directory)
            testfile.close()
            return True

        except Exception as e:
            self.message(
                title='Warning',
                icon='warning',
                text="The directory %s is not writable" % directory,
                buttons=['ok'],
                default='ok')

            return False

    def get_new_project_info(self, filename):
        """Queries user for with NewProjectDialog,
        returns project directory and run_name """

        run_name = get_run_name_from_file(filename)
        result = NewProjectDialog(self, run_name).get_name_and_location()
        if result is None:
            return None
        run_name, project_loc = result

        run_name = run_name.strip().replace(' ', '_')
        if not all([run_name, project_loc, self.check_writable(project_loc)]):
            return None

        project_dir = os.path.join(project_loc, run_name)
        if os.path.exists(project_dir):
            msg = 'Directory %s exists.  All files in this directory will be deleted.  Are you sure?' % project_dir
            response = self.message(title='Warning', icon='warning', text=msg, buttons=['yes','no'],
                                    default='no')
            if response != 'yes':
                self.print_internal("Not deleting %s" % project_dir, color='blue')
            else:
                self.print_internal("Deleting %s" % project_dir, color='blue')
                try:
                    shutil.rmtree(project_dir)
                except Exception as e:
                    self.error("Cannot delete directory: %s" % e, popup=True)
                    return None
        try:
            os.makedirs(project_dir)
        except Exception as e:
            self.error("Cannot create directory: %s" % e, popup=True)
            return None
        return project_dir, run_name


    def open_new_from_template(self, template):
        """Copy files from template directory to user-specified location,
        set run_name and modify comments,
        then open the new project"""

        info = self.get_new_project_info(template)
        if info is None:
            return
        project_dir, run_name = info

        template_dir = os.path.dirname(template)
        project_file = os.path.join(project_dir, run_name + ".mfx")

        # Start from template
        try:
            shutil.copyfile(template, project_file)
            for extra_file in os.listdir(template_dir):
                if extra_file != "mfix.dat" and not extra_file.endswith(".mfx"):
                    shutil.copy(os.path.join(template_dir, extra_file), project_dir)
            self.open_project(project_file, run_name)
            self.update_keyword("run_name", run_name)
            self.save_project()

        except Exception as e:
            self.message(text="Error creating new project: %s" % e,
                buttons=['ok'],
                default=['ok'])
            self.set_no_project()

    def initialize_create_comments(self):
        """ If author or created_date comments are missing, set reasonable defaults  """
        if not self.project.mfix_gui_comments.get("author"):
            self.project.mfix_gui_comments["author"] = getuser()
        if not self.project.mfix_gui_comments.get("created_date"):
            self.project.mfix_gui_comments["created_date"] = time.strftime("%Y-%m-%d %H:%M")

    def get_open_filename(self):
        """wrapper for call to getOpenFileName, override in for unit tests"""
        project_dir = self.get_project_dir()
        filename, ignore = QtWidgets.QFileDialog.getOpenFileName(
            self, 'Open Project Directory', project_dir,
            'MFiX Project (*.mfx *.dat);; All Files (*)')
        return filename

    def handle_open(self):
        """handler for toolbar Open button"""
        if self.unsaved_flag:
            confirm = self.message(text="Project not saved\nData may be lost!\nProceed?",
                                   buttons=['yes', 'no'],
                                   default='no')
            if confirm != 'yes':
                return
            self.clear_unsaved_flag()

        project_path = self.get_open_filename()
        if not project_path:
            return # user pressed Cancel
        self.open_project(project_path)

    def datfile(self):
        """ Return the lines of the datfile """
        if self.get_project_file():
            with open(self.get_project_file()) as datfile:
                datfile_lines = datfile.readlines()
            return datfile_lines
        return None

    def save_recent_project_list(self):
        rec_projects = SETTINGS.value('recent_projects')
        if rec_projects is None:
            rec_projects = []
        elif isinstance(rec_projects, str):
            rec_projects = rec_projects.split('|')

        # remove deleted projects
        clean_proj = []
        for proj in rec_projects:
            if os.path.exists(proj):
                clean_proj.append(proj)

        proj = self.get_project_file()
        if proj is not None:
            if proj in clean_proj:
                clean_proj.remove(proj)
            proj = [proj]
        else:
            proj = []

        new_rec_projects = (proj + clean_proj)[:MAX_RECENT_PROJECTS]
        SETTINGS.setValue('recent_projects', '|'.join(new_rec_projects))

    def open_project(self, project_path, run_name=None, interactive=True):
        """Open MFiX project. See also project_manager.load_project_file"""
        if self.file_menu.isVisible():
            self.hide_file_menu()

        # Make sure path is absolute
        if not os.path.isabs(project_path):
            project_path = os.path.abspath(project_path)

        # "path" may be a directory or a file
        if os.path.isdir(project_path):
            project_dir = project_path
            project_file = os.path.abspath(os.path.join(project_path, 'mfix.dat'))
        else:
            project_dir = os.path.dirname(project_path)
            project_file = project_path

        if not os.path.exists(project_file):
            self.message(title='Error',
                         icon='error',
                         text=('%s does not exist' % project_file),
                         buttons=['ok'],
                         default='ok')
            self.set_no_project()
            return


        if not (project_file.endswith('mfix.dat') or project_file.endswith('.mfx')):
            self.warning('%s does not end in *.mfx or mfix.dat. It may not be a valid MFiX file.'
                         % project_file)

        os.chdir(project_dir) # Make project dir CWD to match solver
        self.reset() # resets gui, keywords, file system watchers, etc

        dirname, basename = os.path.split(project_file)
        self.print_internal("Loading %s from %s" % (basename, dirname), color='blue')
        # --- read the mfix.dat or *.mfx file
        try:
            (units_converted, excs, ws) = self.project.load_project_file(project_file)
        except Exception as e:
            msg = 'Failed to load %s: %s: %s' % (os.path.basename(project_file),
                                                 e.__class__.__name__, e)
            self.error(msg, popup=interactive) # don't popup in -t mode
            traceback.print_exception(*sys.exc_info())
            self.set_no_project()
            return

        self.check_project_version()

        run_name = get_run_name_from_file(project_file)
        pidfile = os.path.join(project_dir, run_name +'.pid') if run_name else None
        self.do_open(project_file, pidfile)
        # report any errors
        for (prefix, errlist) in (('Error', excs), ('Warning', ws)):
            for err in errlist:
                fname = getattr(err, 'filename', None)
                msg = str(err.message if hasattr(err, 'message') else
                          err)
                if fname:
                    msg = fname + ': ' + msg
                msg_lines = msg.splitlines()
                if len(msg_lines) > 1:
                    msg = msg.replace('>', '&gt;').replace('<', '&lt;')
                    msg_lines = msg.splitlines()
                    if interactive:
                        self.message(prefix, text=msg_lines[0],
                                     traceback_text='\n'.join(msg_lines[1:]), # use monospace font
                                     print_console=False)
                    self.print_internal('%s: %s' % (prefix, msg_lines[0]), color='red')
                    for line in msg_lines[1:]:
                        self.print_internal(line, font='monospace')
                else:
                    self.print_internal("%s: %s" % (prefix, msg),
                                            color='red')


        if units_converted:
            msg = ["Project has been converted to SI.", "Please check result of conversion."]
            udfs = glob.glob(os.path.join(os.path.dirname(project_file), '*.f'))
            if udfs:
                msg += ["", "The following Fortran source files have not been converted:"]
                msg += ['  '+os.path.basename(x) for x in udfs]
            msg = '\n'.join(msg)
            self.warn(msg, popup=interactive)

        short_name = os.path.basename(project_file)
        if excs: # Errors occurred
            msg = plural(len(excs), 'error')
            self.print_internal("Warning: %s loading %s" %
                                    (msg , short_name),
                                    color='red')
        else:
            if ws: # No errors, but some warnings
                msg = plural(len(ws), 'warning')
                self.print_internal("Loaded %s with %s" %
                                        (short_name, msg),
                                        color='red')
            else: # Success
                self.print_internal("Loaded %s" % short_name,
                                        color='blue')

        # setup editor widget
        self.editor_widget.open_project(project_dir)
        tab = self.editor_widget.open(project_file, read_only=True, closeable=False, project_file=True)
        self.editor_widget.remove_untitled_tabs() # remove untitled tab

        ### PPO/SMS mode
        # 4 cases:
        # 1A:  SMS enabled, file contains PPO:  check for .msh file on load
        # 1B:  SMS enabled, file does not contain PPO key:  autoconvert to SMS (should we do .msh check here?)
        # 2A:  SMS disabled, file does not contain PPO: no action needed
        # 2B:  SMS disabled, file contains PPO:  prompt, either enable SMS or turn off PPO
        ppo = self.project.get_value('ppo')
        mesh_accepted = self.mesh_accepted = self.project.mfix_gui_comments.get('mesh_accepted')
        if self.sms_enabled:
            if ppo is not None: # 1A, SMS enabled, PPO set
                self.set_sms_mode(True)
                # Check for .msh file
                run_name = self.project.get_value('run_name', default=None)
                if run_name is None:
                    pass # Don't warn on creating project from tutorial
                    #self.warn("run_name is not set", popup=True)
                else:
                    mesh_file = run_name.upper() + '.msh'
                    # We are already in project dir, so relative path is fine
                    if not os.path.exists(mesh_file):
                        self.print_internal("Mesh file %s not found" % mesh_file, color='blue')
                        self.clear_mesh_accepted()
                        #  There is no .MSH file present: Go to "Mesher
                        #  mode", Mesh pane, Mesher tab so the user can
                        #  generate the mesh (it is assumed the .mfx file
                        #  is a complete file and ready to go. Otherwise
                        #  the user can complete the settings before
                        #  generating the mesh)
                        self.update_keyword('ppo', True)
                        self.change_mode('mesher')
                        self.change_pane('mesh')
                        self.change_mesh_tab(1) # Mesh
                    else:
                        # There is a .MSH file present: This creates
                        # difficulties since there is no absolute
                        # guarantee that the .MSH file is compatible
                        # with the .mfx file. A data check must be
                        # implemented to verify compatibility
                        # (TBD). If the data check passes, then go to
                        # "Mesher mode", Mesh pane, Mesher tab so the
                        # user can accept the mesh (always verify the
                        # mesh before moving to "Modeler mode").
                        self.print_internal("Found mesh file %s" % mesh_file, color='blue')
                        if mesh_accepted is False:
                            self.print_internal("Mesh file not accepted", color='orangered')
                            self.clear_mesh_accepted()
                        else:
                            self.print_internal('Mesh file accepted', color='blue')
                            self.set_mesh_accepted()
                            self.update_keyword('ppo', False)
                            #self.change_mode('mesher')
                            #self.change_pane('mesh')
                            #self.change_mesh_tab(1) # Mesh
                self.ui.pushbutton_mesher.setEnabled(True)

            else: # 1B SMS enabled, PPO=None
                # Automatically convert to SMS mode if preference enabled
                # Do we need to confirm this?  No, it's easy to undo.
                self.print_internal("Converting project to SMS workflow", color='blue')
                self.update_keyword('ppo', True)
                self.clear_mesh_accepted()
                self.set_sms_mode(True)
                # self.warning("Current project does not support SMS.\nUsing standard workflow.",
                #             popup=True)
                #self.set_sms_mode(False)

        else: # SMS not enabled in Settings
            if ppo is None:  #2A sms_enabled = False, PPO = None
                self.set_sms_mode(False)
                if self.mode == 'mesher': # Don't stay in Mesher
                    self.change_mode('modeler')
                    if self.curr_nav_label != 'Regions':
                        self.change_pane('model setup')
            else: #2B bug user until they enable SMS
                resp = self.message(title='Notice',
                                    icon='question',
                                    text='This project is set up for SMS (Segregated Mesher/Solver) workflow.\n'
                                    'This is a beta feature, which can be disabled in the Settings menu.\n'
                                    'Enable SMS workflow?',
                                    buttons=['yes','no'],
                                    default='yes')
                if resp == 'yes':
                    SETTINGS.setValue('SMS', 1)
                    SETTINGS.sync() # Needed?
                    self.print_internal("Enabling Segregated Solver/Mesher workflow", color='blue')
                    self.set_sms_enabled(True)
                    self.set_sms_mode(True)
                else:
                    self.print_internal('Not enabling SMS, removing SMS settings from project', color='blue')
                    self.unset_keyword('ppo')
                    self.project.mfix_gui_comments.pop('mesh_accepted', None)
                    self.set_sms_mode(False)
                    if self.mode == 'mesher':
                        self.change_mode('modeler')
                        if self.curr_nav_label != 'Regions':
                            self.change_pane('model setup')

        self.mesh_dir_watcher.addPath(project_dir)
        self.mesh_dir_changed(project_dir)
        self.slot_update_runbuttons()
        self.status_manager.set_run_button()
        self.update_nav_tree()
        self.ui.toolbutton_compile.setEnabled(True)

        #if self.unsaved_flag: #
        # Settings changed during loading
        #    self.save_project()- let the user do this

        self.initialize_create_comments()
        self.set_project_modified_time(project_file)

        self.save_recent_project_list()
        self.setup_current_pane()

        ### Project Info
        self.update_info()

    def do_open(self, project_file, pidfile):
        project_dir = os.path.dirname(project_file)
        self.set_project_file(project_file) # set project file early

        # make sure the file_menu is closed
        self.hide_file_menu()

        # previously started job may be running, try to reconnect
        self.job_manager.try_to_connect(pidfile)

        self.open_succeeded = False  # set to true on success
        self.vtkwidget.defer_render = True # defer rendering vtk until load finished

        ### Geometry
        # Look for geometry.stl and load automatically
        geometry_file = os.path.abspath(os.path.join(project_dir, 'geometry.stl'))
        if os.path.exists(geometry_file) and 'geometry' not in self.project.mfix_gui_comments:
            self.vtkwidget.add_stl(filename=geometry_file)
        else:
            # order needs to be visual_props -> geometry -> regions (loaded below)
            # load props first
            props = self.project.mfix_gui_comments.get('visual_props')
            if props:
                self.vtkwidget.visual_props_from_str(props)

            # Parse geometry info
            geo = self.project.mfix_gui_comments.get('geometry', '')
            self.vtkwidget.geometry_from_str(geo)
            if not geo:
                # extract quadrics
                self.vtkwidget.process_quadrics(self.project)
                # read other geometry?

        # Mesh Stats
        self.look_for_mesh_stats()

        #  Non-keyword params stored as !#MFIX-GUI comments
        solids_phase_names = {}
        pic_const_statwt = {}
        for (key, val) in self.project.mfix_gui_comments.items():
            try:
                if key == 'fluid_phase_name':
                    self.set_fluid_phase_name(val)
                elif key.startswith('solids_phase_name('):
                    n = int(key.split('(')[1][:-1])
                    solids_phase_names[n] = val
                elif key.startswith('scalar_name('):
                    n = int(key.split('(')[1][:-1])
                    self.scalar_names[n] = val
                elif key.startswith('pic_const_statwt('):
                    n = int(key.split('(')[1][:-1])
                    pic_const_statwt[n] = float(val)
                elif key == 'regions_dict':
                    self.ui.regions.regions_from_str(val)
                elif key == 'ic_regions':
                    self.ics_regions_from_str(val)
                elif key == 'ic_distributions':
                    self.ics_psd_from_str(val)
                elif key == 'bc_regions':
                    self.bcs_regions_from_str(val)
                elif key == 'bc_distributions':
                    self.bcs_psd_from_str(val)
                elif key == 'is_regions':
                    self.iss_regions_from_str(val)
                elif key == 'ps_regions':
                    self.pss_regions_from_str(val)
                elif key == 'vtk_regions':
                    self.vtk_regions_from_str(val)
                elif key == 'usr_regions':
                    self.usr_regions_from_str(val)
                elif key == 'log_files':
                    self.output_logs_from_str(val)
                elif key == 'monitor_regions':
                    self.monitors_regions_from_str(val)
                elif key == 'chemistry':
                    self.chemistry_from_str(val)
                elif key == 'graphics':
                    self.graphics_from_str(val)
                elif key == 'geometry':
                    pass # handled in 'geometry' section above
                elif key == 'visual_props':
                    pass # handled in 'geometry' section above
                elif key == 'parameters':
                    pass # handled in project
                elif key == 'ui_state':
                    pass # handled after everything is loaded
                elif key == 'retained_keys':
                    for (k,v) in ExtendedJSON.loads(val).items():
                        k,a = parse_key_with_args(k)
                        if isinstance(a, int):
                            a = [a]
                        if a:
                            self.retained_keys[(k, tuple(a))] = v
                        else:
                            self.retained_keys[(k, ())] = v # Convert to empty arglist internally
                elif key == 'unlocked_keys':
                    self.unlocked_keys = ExtendedJSON.loads(val)
                elif key == 'psd':
                    self.solids_psd_from_str(val)
                # Add more here
                #else:  # Too many warnings!
                #    self.warn("Unknown mfix-gui setting '%s'" % key)

            except Exception as e:
                self.error("%s: %s" % (key, e))

        # Copy ordered dict to modify keys w/o losing order
        if solids_phase_names:
            s = OrderedDict()
            for (i, (k, v)) in enumerate(self.solids.items(), 1):
                s[solids_phase_names.get(i, k)] = v
            self.solids = s

        # Restore non-keyword pic_const_statwt
        for (k,v) in pic_const_statwt.items():
            solid = list(self.solids.values())[k-1]
            solid['pic_const_statwt'] = v

        #### Fluid phase
        self.fluid_solver_disabled = (self.project.get_value('ro_g0') == 0.0)
        self.disable_fluid_solver(self.fluid_solver_disabled)
        self.update_fluid_species_table() # Necessary?  Should be done when we show fluid pane.

        # fluid momentum and species eq. handled by _keyword_ widget

        # Scalar equations
        pass

        # handle a bunch of items which are essentially the same
        for (setter, name) in ((self.set_fluid_density_model, 'ro'),
                               (self.set_fluid_viscosity_model, 'mu'),
                               (self.set_fluid_specific_heat_model, 'c_p'), # inconsistent
                               (self.set_fluid_tc_model, 'k'),
                               (self.set_fluid_diffusion_model, 'dif')):
            name_g0 = 'c_pg0' if name=='c_p' else name+'_g0'
            name_usr = 'usr_cpg' if name=='c_p' else 'usr_'+name+'g'
            val_g0 = self.project.get_value(name_g0)
            val_usr = self.project.get_value(name_usr)

            if val_usr is not None and val_g0 is not None:
                self.print_internal('Warning: %s and %s are both set' % (name_g0, name_usr))
                # this is getting printed after error count ... should be included in # of errs

            setter(CONSTANT if val_g0 is not None
                   else UDF if val_usr is not None
                   else 1)

        # molecular weight model is the odd one (only 2 settings)
        if self.project.get_value('mw_avg') is not None:
            self.set_fluid_mol_weight_model(CONSTANT)
        else:
            # requires molecular weights for all species components, when should we validate?
            self.set_fluid_mol_weight_model(1)

        ### Solids
        # Restore 'density' from retained_key if ro_xs0 is not set
        for (phase, pdata) in self.solids_species.items():
            for (species, (sname,sdata)) in enumerate(pdata.items(), 1):
                if sdata.get('density') is None:
                    val = self.get_retained_keyword('ro_xs0', args=[phase, species])
                    if val is not None:
                        sdata['density'] = val
        # Needed?  will this get done when we switch to solids tab?
        self.update_solids_table()
        self.solids_update_tabs()
        self.update_solids_detail_pane()

        ### Regions
        # Look for regions in IC, BC, PS, etc.
        self.ui.regions.extract_regions(self.project, str(project_dir))

        # background mesh
        self.init_background_mesh()

        # "Extract" pulls out info from non-GUI project files, which don't have MFIX_GUI section
        # Initial conditions
        self.ics_extract_regions()
        # Boundary conditions
        self.bcs_extract_regions()
        # Point sources
        self.pss_extract_regions()
        # Internal surfaces
        self.iss_extract_regions()
        # VTK output regions
        self.vtk_extract_regions()
        # USR output regions
        self.usr_extract_regions()
        # Monitors
        self.monitors_extract_regions()
        # Chemistry
        self.chemistry_extract_info()
        # Particle size distribution
        self.solids_extract_psd()

        # Scalars
        #self.scalar_extract_info() Nothing to do

        # monitor reader
        for mon in self.project.get_key_indices('monitor_name'):
            name = self.project.get_value('monitor_name', args=mon)
            if name is not None:
                self.monitor_reader.add_file(os.path.join(project_dir, name))

        ### Nodeworks
        if self.ui.nodeworks_widget.NODEWORKS_AVAILABLE:
            nodeworks_file = os.path.abspath(os.path.join(project_dir, 'workflow.nc'))
            if os.path.exists(nodeworks_file):
                self.ui.nodeworks_widget.clear()
                self.ui.nodeworks_widget.load(nodeworks_file)

            self.ui.nodeworks_widget.look_for_projects(project_dir)


        ### VTK modeler widget
        self.vtkwidget.reset_view()
        self.vtkwidget.update_region_color_map()
        self.vtkwidget.render(defer_render=False)

        ### GUI state (widget positions, etc.)
        # saved in project file
        ui_state = {}
        try:
            ui_state = json.loads(self.project.mfix_gui_comments.get('ui_state', '{}'))
        except json.decoder.JSONDecodeError:
            pass
        self.ui.tabWidgetGraphics.setCurrentIndex(ui_state.get('graphic_tab', 0))
        mode = ui_state.get('mode', 'modeler')
        pane = ui_state.get('navigation', 'model setup')
        if mode == 'modeler' and not(self.ui.pushbutton_modeler.isEnabled()):
            mode = 'mesher'
            pane = 'mesh' # ?

        self.change_mode(mode)
        self.change_pane(pane)
        if pane == 'mesh':
            self.change_mesh_tab(1) # ?

        # global
        self.open_succeeded = True

        #### ***** PUBLIC SERVICE ANNOUNCEMENT
        ##PLEASE do not call 'clear_unsaved_flag' here
        #  if it got set during file-load, either there's
        #  a bug (erroneous call to set_unsaved_flag)
        #  or a keyword got modified during loading (this
        #  happens!).  Clearing the flag is unsafe
        #  because there may be real changes that need
        #  to be saved


    def add_tooltip(self, widget, key, description=None, value=None):
        if not hasattr(widget, 'key') or widget.key is None:
            widget.key = key

        if description is None:
            doc = self.keyword_doc.get(key)
            if not doc:
                return
            description = doc.get('description')
            if description is None:
                return
            if value is not None and 'valids' in doc or key=='rdf_type':
                vkey = ('.FALSE.' if value is False
                        else '.TRUE.' if value is True
                        else str(value))
                for (k, v) in doc['valids'].items():
                    if (k.lower()==vkey.lower() or
                        #v.get('alias','') can return None
                        (vkey and (v.get('alias') or '').lower()==vkey.lower())):
                        description = v.get('note', str(value))
                        break
        # Clean it up a little
        description = description.strip()
        # Remove default values in brackets (these don't make sense
        #  once the value has been set)
        if description.endswith(']') or description.endswith('].'):
            dot = description.endswith('.')
            description = description.rsplit('[', 1)[0]
            description = description.strip()
            if dot and not description.endswith('.'): # Put it back
                description += '.'
        # '>' and '<' will mess up HTML
        description = description.replace('<', '&lt;')
        description = description.replace('>', '&gt;')

        # Literature citation
        for x in ('\nSee ', ' See '):
            if x in description:
                description = description.replace(x, '<br>See ')

        # Don't split diff. eq's over multiple lines
        pat = re.compile('condition: *(\n.+[\n,])')
        match = pat.search(description)
        if match or (value=='PSW' and key=='bc_type'):
            if match:
                text = match.group(1)
                repl = text[1:-1] #drop the newlines
            else:
                text = re.search('dv[^,]*,', description).group()
                repl = text[:-1]

            repl = repl.replace('d(', 'd', 1)
            repl = repl.replace(')/', '/', 1)
            repl = repl.replace(' (', '(') # Squeeze out spaces before parens
            # Subscripts
            for sub in 'g', 's', 'M':
                repl = repl.replace('_%s'%sub, '<sub>%s</sub>'%sub) # Make _g into a subscript
            repl = repl.replace('ScalarW', 'Sw')
            repl = repl.replace('Scalar', 'S') # Scalar -> S in equation (?)
            repl = repl.replace('d', '</i>d<i>') # de-italicize differential op
            repl = '<br><nobr><i>'+repl+'</i></nobr><br>'   # put it on its own line
            description = description.replace(text, repl)
            pat = re.compile(', ([A-Za-z_]+), in', re.MULTILINE)
            match = pat.search(description)
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

            description = description.replace(text, repl, 1)
            # Italicize normal vector to match equation display
            description = description.replace('where n is', 'where <i>n</i> is')
            if key == 'bc_type' and value == 'PSW':
                for s in 'Hw', 'vw':
                    description = description.replace('%s'%s,
                                                      '<i>%s</i>'%s)
        # remove cylindrical refs
        for x in ' (theta) direction', ' (r) direction':
            description = description.replace(x, '-direction')

        ### epsilon
        description = description.replace('epsilon', 'ε')
        description = description.replace('Epsilon', 'ε')
        #description = description.replace('EPSILON', 'ε')

        ### theta
        for t in 'theta', 'THETA', 'Theta':
            description = description.replace(t, 'θ')

        ### lambda
        for t in 'lambda', 'LAMBDA', 'Lambda':
            description = description.replace(t, 'λ')

        ### Default value
        #pat = re.compile(r'\[[^]]+\]')
        #while True:
        #    match = pat.search(description)
        #    if not match:
        #        break
        #    text = match.group(0)
        #    description = description.replace(text, '<i>Default: %s</i>'%text[1:-1])

        # Bullets
        description = re.sub(r'^\s*-', '<br/>&bull;', description, flags=re.MULTILINE)

        # Issues/590 (workaround)
        description = re.sub(r'\.\s*-', '<br/>&bull;', description, flags=re.MULTILINE)

        if 'list:' in description.lower():
            for n in range(1, 20):
                s = ' %d: ' % n
                description = description.replace(s, '<br/>&bull;%d: '%n)

        # non-breaking hyphen
        description = description.replace('-', '&#8209;')

        # Replace blank lines with break
        lines = [l if l.strip() else '<br>' for l in  description.splitlines()]
        description = '\n'.join(lines)

        args = getattr(widget, 'args', None)
        if args is None:
            # this really only applies to label widgets, which don't have '.args'
            args = keyword_args.get(key)
            if args: # translate to consistent terms
                replace = {'phase':'P',
                           'species': 'S',
                           'scalar': 'N'}
                args = [replace.get(a, a.upper()) for a in args]

        if isinstance(args, int):
            key = '%s(%s)' % (key, args)
        elif args:
            args = ['Phase' if arg=='P'
                    else 'Species' if arg=='S'
                    else 'Monitor' if arg=='MONITOR'
                    else arg for arg in args]
            key = '%s(%s)' % (key, ','.join(map(
                lambda x: str(x[0] if isinstance(x, (tuple, list)) else str(x)), args)))
        if value is not None:
            key = '%s=%s' % (key, value)
        if key is None:
            msg = '<b></b>%s</br>' % description
        else:
            msg = '<b>%s</b>: %s</br>' % (key, description)

        widget.setToolTip(msg)
        widget.help_text = msg # would be nice do something more useful with help_text

    # Following functions are overridable for test runner
    def confirm_clobber(self, renamed_project_file):
        clobber_msg = '%s exists, replace?' % renamed_project_file
        response = self.message(title='Warning',
                                icon='question',
                                text=clobber_msg,
                                buttons=['yes', 'no'],
                                default='no')
        return response == 'yes'

    def confirm_delete_files(self, message_text):
        response = self.message(title="Info",
                                icon="info",
                                text=message_text,
                                buttons=['ok', 'cancel'],
                                default='cancel')
        return response == 'ok'

    def handle_search(self):
        ap = self.advanced_popup
        ap.set_basic_mode()
        ap.ui.lineedit_search.clear()
        ap.ui.tablewidget_keys.clearSelection()
        ap.ui.info_area.clear()
        ap.reset()
        txt = ''
        fw = self.focusWidget()
        def is_ancestor(w1, w2):
            """Is widget w1 in the .parent() hierarchy of widget w2?"""
            if w1 == w2:
                return True
            while w2.parent():
                w2 = w2.parent()
                if w2 == w1:
                    return True
            return False

        if fw:
            win = None
            if is_ancestor(self.editor_widget, fw):
                idx = self.editor_widget.tabs.currentIndex()
                if idx is not None:
                    win = self.editor_widget.tabs.widget(idx).editor
            if is_ancestor(self.console_printer.console, fw):
                win = self.console_printer.console
            if win:
                tc = win.textCursor()
                if not tc.hasSelection():
                    tc.select(QtGui.QTextCursor.WordUnderCursor)
                txt = tc.selectedText()

        if txt:
            tw = ap.ui.tablewidget_keys
            ap.lineedit_search.setText(txt.strip())
            ap.do_search()
            txt = txt.strip().lower()
            for i in range(tw.rowCount()):
                t = tw.item(i,0).text()
                if t == txt:
                    tw.setCurrentCell(i,0)
                    break
        else:
            ap.do_search()
            ap.ui.lineedit_search.setFocus()
        ap.show()


    def get_developer_mode(self):
        return int(SETTINGS.value('developer_mode', 0))

    def enable_developer_mode(self, enable):
        ui = self.ui
        #self.change_mode('modeler') # Why?
        ui.pushbutton_interpreter.setVisible(enable)

        tw = ui.tabWidgetGraphics
        if enable:
            tw.insertTab(1, ui.mfix_response, 'MFiX status')
            for side in (QtWidgets.QTabBar.RightSide, QtWidgets.QTabBar.LeftSide):
                button = tw.tabBar().tabButton(1, side)
                if button:
                    button.resize(0, 0)
        else:
           tw.removeTab(tw.indexOf(ui.mfix_response))

        SETTINGS.setValue('developer_mode', int(enable))


def main():
    main_args(sys.argv[1:])

def main_args(sys_argv):
    global gui
    gui = None
    # 'gui' is initialized here instead of at module-level
    #  so that interpreter.py can 'import gui' without
    #  clobbering reference

    # handle command-line arguments
    styles_avail = [s.lower() for s in QtWidgets.QStyleFactory.keys()]
    parser = argparse.ArgumentParser(description='MFiX GUI')
    ARG = parser.add_argument
    ARG('project', action='store', nargs='?', default=None,
        help="open mfix.dat or <RUN_NAME>.mfx project file or search a specified directory for project files")
    ARG('-e', '--exe', metavar='EXE', action='store', default=None,
        help='specify MFiX executable (full path)')
    ARG('-l', '--log', metavar='LOG', action='store', default='WARN',
        choices=['error', 'warning', 'info', 'debug'],
        help='set logging level (error, warning, info, debug)')
    ARG('-s', '--style', metavar='STYLE', action='store', default=None,
        choices=styles_avail,
        help='specify app style %s'%styles_avail)
    ARG('-n', '--noload', action='store_true',
        help='do not autoload previous project')
    ARG('-w', '--nonodeworks', action='store_false',
        help='do not load the nodeworks environment')
    ARG('-k', '--novtk', action='store_false',
        help='do not load vtk')
    ARG('-g', '--default_geo', action='store_true',
        help="Use default geometry, don't restore previous state.")
    ARG('-d', '--developer', action='store_true',
        help="Enable developer mode.")
    ARG('-c', '--clear', action='store_true',
        help="Clear all saved settings.")
    ARG('-t', '--test', action='store_true',
        help="Enable test mode.")
    ARG('-ct', '--thumbnails', action='store_true',
        help="Create thumbnails in test mode.")
    ARG('--save', action='store_true',
        help="Save the project in test mode.")
    ARG('-v', '--version', action='version', version=__version__)

    args = parser.parse_args(sys_argv)

    if args.clear:
        print("Clearing all MFIX settings from ", SETTINGS.fileName())
        SETTINGS.clear()
        SETTINGS.sync()
        return

    # setup logging
    logging.basicConfig(stream=sys.stdout,
                        filemode='w', level=getattr(logging, args.log.upper()),
                        format='%(name)s - %(levelname)s - %(message)s')

    project_file = args.project
    if project_file and os.path.isdir(project_file):
        project_file = find_project_file(project_file)
        if project_file is None:
            print("Can't find *.mfx or mfix.dat in directory: %s" % project_file)
            parser.print_help()
            return

    elif project_file and not os.path.isfile(project_file):
        print("%s: is not a file " % project_file)
        parser.print_help()
        return

    # Set exception handler early, so we catch any errors in initialization
    if not args.test:
        def hook(etype, exc, tb):
            """ avoid exceptions when catching exceptions """
            try:
                excepthook(gui, args.developer, etype, exc, tb)
            except Exception:
                logging.getLogger(__name__).exception("Error displaying bug reporting dialog box")
        sys.excepthook = hook

    # Disable Qt scaling, it looks bad on most displays
    #QtCore.QCoreApplication.setAttribute(Qt.AA_Use96Dpi, False)
    #QtCore.QCoreApplication.setAttribute(Qt.AA_DisableHighDpiScaling, False)
    #QtCore.QCoreApplication.setAttribute(Qt.AA_EnableHighDpiScaling, True)

    # create the QApplication
    qapp = QtWidgets.QApplication([])

    # splash screen
    splash = None
    if not args.test:
        splash = QtWidgets.QSplashScreen(get_pixmap('splash.png', 600, 338))
        splash.setWindowFlags(Qt.SplashScreen |
                              Qt.BypassWindowManagerHint |
                              Qt.FramelessWindowHint |
                              Qt.WindowStaysOnTopHint)
        #splash.setWindowIcon(get_icon('mfix.png'))
        splash.show()

    def set_splash_text(text):
        if splash is None:
            return
        v = 'Version: ' + __version__
        splash.showMessage('\n'.join([v, text]),
                           int(Qt.AlignHCenter|Qt.AlignBottom),
                           #QtGui.QColor('orange')
                           #Qt.blue)
                           )
        #splash.repaint() #showMessage does this
        qapp.processEvents()

    set_splash_text('Starting...')

    # set style
    current_style = qapp.style().objectName().lower()
    use_style = get_preferred_style(args.style, current_style)
    if use_style:
        qapp.setStyle(use_style.lower())

    # create the gui
    set_splash_text('Creating application')
    gui = MfixGui(qapp, project_file=project_file, loadnodeworks=args.nonodeworks,
                  loadvtk=args.novtk, set_splash_text=set_splash_text, style=use_style)


    geo = SETTINGS.value('geometry')
    if geo is not None and not args.default_geo:
        # set previous geometry
        gui.restoreGeometry(geo)
        left_right = SETTINGS.value('splitter_left_right')
        if left_right is not None:
            gui.ui.splitter_left_right.setSizes([int(num) for num in left_right])

        cmd_output = SETTINGS.value('splitter_graphics_cmd_output')
        if cmd_output is not None:
            gui.ui.splitter_graphics_cmd_output.setSizes([int(num) for num in cmd_output])
    else:
        # default geometry
        geo = gui.frameGeometry()
        screen = qapp.desktop().screenNumber(qapp.desktop().cursor().pos())
        centerPoint = qapp.desktop().screenGeometry(screen).center()
        geo.moveCenter(centerPoint)
        gui.move(geo.topLeft())

    # set developer mode
    gui.enable_developer_mode(int(SETTINGS.value('developer_mode', 0)) or args.developer)

    # show the gui, unless disabled
    if not args.test:
        gui.show()

    # close the splash
    if splash is not None:
        splash.close()

    # Start update check
    threading.Thread(target=gui.check_update_available,
                     args=(__version__,)).start()

    if args.exe:
        #print('exe option passed: %s' % mfix_exe_option)
        gui.commandline_option_exe = args.exe

    # Handle SMS mode (issues/1032)
    sms_enabled = SETTINGS.value('SMS', 0)
    if sms_enabled in ('false', 'False'):
        sms_enabled = 0
    elif sms_enabled in ('true', 'True'):
        sms_enabled = 1
    gui.set_sms_enabled(int(sms_enabled))

    last_project = None
    if project_file is None and not args.noload:
        # autoload last project
        last = SETTINGS.value('project_file')
        last_project = project_file = os.path.normpath(last) if last else None

    if project_file and not args.noload and os.path.exists(project_file):
        set_splash_text('Loading project')
        gui.open_project(project_file, interactive=(not args.test))
    else:
        gui.set_no_project()
        gui.open_file_menu()

    # have to initialize vtk after the widget is visible!
    if not (args.novtk or args.test):
        gui.vtkwidget.vtkiren.Initialize()

    # exit with Ctrl-C at the terminal
    signal.signal(signal.SIGINT, signal.SIG_DFL)

    if not args.test:
        qapp.exec_()

    else:  # Run internal test suite
        gui.navigate_all()
        # create thumbnails
        if args.thumbnails:
            create_project_thumbnail(gui, save_gui_info=True, test=True)

        # save project
        if args.save:
            gui.handle_save()


if __name__ == '__main__':
    main()
