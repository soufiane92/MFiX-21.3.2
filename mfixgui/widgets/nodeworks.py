#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''
This module contains the work flow widget.
'''

import asyncio
import copy
import glob
import json
import os
import re
import shutil
import subprocess
import multiprocessing
import threading
import queue

from collections import OrderedDict
from qtpy import QtCore, QtGui, QtWidgets
from qtpy.QtCore import Signal

try:
    from nodeworks import NodeWidget, Node
    NODEWORKS_AVAILABLE = True
except ImportError:
    NodeWidget = None
    NODEWORKS_AVAILABLE = False
    Node = object

from mfixgui.constants import PARAMETER_DICT
from mfixgui.job import JobManager
from mfixgui.project import Project, ExtendedJSON
from mfixgui.solver.process import ProcessManager
from mfixgui.tools import is_vnc, SCRIPT_DIRECTORY, replace_with_dict, case_insensitive
from mfixgui.tools.qt import get_icon, deepcopy_dict, SETTINGS

from mfixgui.widgets.base import Table
from mfixgui.widgets.run_popup import RunPopup
from mfixgui.widgets.regions import DEFAULT_REGION_DATA
try:
    from mfixgui.vtk_widgets.geometry_engine import GeometryEngine
    VTK_AVAILABLE = True
except ImportError:
    VTK_AVAILABLE = False
try:
    from nodeworks.tools.wizard import Wizard
    WIZARD_AVAILABLE = True
except ImportError:
    WIZARD_AVAILABLE = False


JOB_QUEUE = queue.Queue()


def job_worker():
    while True:
        job = JOB_QUEUE.get()

        # check for poision pill
        if job is None:
            JOB_QUEUE.task_done()
            break

        # run the job, blocking
        rcmd, proj_dir, env = job
        with open(os.devnull, "w", encoding="utf-8") as devnull:
            p = subprocess.run(rcmd, cwd=proj_dir, env=env, stdout=devnull, stderr=devnull)
        # tell queue that we are done.
        JOB_QUEUE.task_done()


# --- Mock Parent for job submission ---
class Mock(object):
    def noop(self, *args, **kwargs):
        return False
    def __getattr__(self, name):
        return self.noop


class MockParent(QtWidgets.QWidget):
    stderr_signal = Signal(object)
    stdout_signal = Signal(object)
    signal_update_runbuttons = Signal(object)
    project = None
    project_dir = None
    project_file = None
    settings = None
    monitor = Mock()
    ui = Mock()
    ui.tabWidgetGraphics = Mock()
    job_manager = Mock()
    job_manager.job = None
    job_manager.pidfile = None
    status_manager = Mock()
    console_printer = Mock()

    def __init__(self, parent=None, mfix_gui = None):
        QtWidgets.QWidget.__init__(self, parent)
        self.mfix_gui = mfix_gui

    def noop(self, *args, **kwargs):
        return None

    def __getattr__(self, name):
        return self.noop

    def get_project_dir(self):
        return self.project_dir

    def get_project_file(self):
        return self.project_file

    def print_internal(self, *args, **kwargs):
        if self.mfix_gui is not None:
            self.mfix_gui.print_internal(*args, **kwargs)

    def message(self, *args, **kwargs):
        if self.mfix_gui is not None:
            self.mfix_gui.message(*args, **kwargs)


# --- custom run_pop ---
class NodeworksRunPopup(RunPopup):
    def __init__(self, mfix_exe, parent):
        RunPopup.__init__(self, mfix_exe, parent, None)
        self.solver_options = None

        # tweak the dialog
        self.ui.groupbox_restart.hide()
        self.ui.button_cancel.clicked.connect(self.handle_abort)

        # add local queue widgets
        gb = self.groupbox_localqueue = QtWidgets.QGroupBox()
        self.ui.layout.addWidget(gb, 3, 0)
        gb.setTitle('Local Queue')
        gb.setCheckable(True)
        local = int(self.gui_comments.get('local_queue', 1))
        queue = int(self.gui_comments.get('submit_to_queue', 0))
        gb.setChecked(local and not queue)
        def set_v(v):
            self.gui_comments['local_queue'] = int(v)
        gb.clicked.connect(set_v)
        self.queue_toggle()

        font = QtGui.QFont()
        font.setBold(True)
        gb.setFont(font)

        verticalLayout = QtWidgets.QVBoxLayout(gb)
        verticalLayout.setContentsMargins(0, 0, 0, 0)
        widget = QtWidgets.QWidget(gb)
        verticalLayout.addWidget(widget)
        oldfont = QtGui.QFont()
        oldfont.setBold(False)
        widget.setFont(oldfont)

        glayout = QtWidgets.QGridLayout(widget)
        glayout.setContentsMargins(5, 5, 0, 0)
        glayout.setSpacing(5)
        glayout.addWidget(QtWidgets.QLabel('Concurrent models'), 0, 0)
        sbj = self.spinbox_jobs = QtWidgets.QSpinBox()
        sbj.setRange(1, 9999)
        sbj.setValue(int(SETTINGS.value('local_job_cnt', multiprocessing.cpu_count())))
        sbj.valueChanged.connect(lambda v: SETTINGS.setValue('local_job_cnt', int(v)))
        glayout.addWidget(sbj, 0, 1)

        self.ui.groupbox_queue.toggled.connect(self.queue_toggle)

    def queue_toggle(self):

        enable = not self.ui.groupbox_queue.isChecked()
        self.groupbox_localqueue.setEnabled(enable)
        if not enable and self.groupbox_localqueue.isChecked():
            self.groupbox_localqueue.setChecked(False)

    @property
    def local_queue(self):
        return self.groupbox_localqueue.isChecked()

    # over-rides
    def handle_run(self):
        self.cancel = False

        self.template_manager.save_settings()
        self.save_dmp_keywords()

        use_queue = self.ui.groupbox_queue.isChecked()
        template = self.current_template() if use_queue else None
        omp_num_threads = self.ui.spinbox_threads.value()

        # collect nodes[ijk] from project to guarantee that mpirun matches
        nodesi = self.mfixgui.project.get_value("nodesi", 1)
        nodesj = self.mfixgui.project.get_value("nodesj", 1)
        nodesk = self.mfixgui.project.get_value("nodesk", 1)

        self.solver_options = (self.solver, template, omp_num_threads, (nodesi, nodesj, nodesk))
        self.ui.close()

    def handle_abort(self):
        self.cancel = True


class FakeJob(object):
    job=None


# helper functions
def replace_prj(cmd, prj):
    # find the entry after '-f' and replace
    idx = cmd.index('-f')
    new_cmd = copy.deepcopy(cmd)
    new_cmd[idx+1] = prj
    return new_cmd


# --- Nodeworks Widget ---
class NodeworksWidget(QtWidgets.QWidget):
    def __init__(self, project, parent=None):
        QtWidgets.QWidget.__init__(self, parent)

        self.project = project
        self.job_dict = {}
        self.update_timer = QtCore.QTimer()
        self.update_timer.timeout.connect(self.update_job_status)
        self.solver_options = None
        self.local_queue = False
        self.file_timer = QtCore.QTimer()
        self.file_timer.timeout.connect(self.look_for_pid)
        self.watch_dir_paths = []
        self.process_managers = []
        self.running_in_vnc = is_vnc()
        self.job_workers = []
        self.needs_saved = False
        self.process_manager = ProcessManager(parent)

        # --- initialize the node widget ---
        nc = self.nodeChart = NodeWidget(showtoolbar=False)
        if hasattr(self.nodeChart, 'needsSavedEvent'):
            self.nodeChart.needsSavedEvent.connect(self.set_save_btn)
        self.nodeChart.setSizePolicy(QtWidgets.QSizePolicy.Expanding,
                                     QtWidgets.QSizePolicy.Preferred)
        self.nodeChart.setGeometry(0, 0, 100, 1000)

        # add btns to gui toolbar
        ly = parent.ui.horizontallayout_menu_bar
        i = ly.count() - 10

        nc.enable_disable_btns = []
        size = parent.ui.toolbutton_run_mfix.iconSize()

        # build screenshot button
        sb = self.screenshotToolButton = QtWidgets.QToolButton()
        sb.setIcon(get_icon('camera'))
        sb.setToolTip('save as image.')
        sb.clicked.connect(lambda ignore: nc.screenShot())
        sb.setEnabled(True)

        btns = [nc.runToolButton, nc.stepToolButton, nc.autorunToolButton,
                nc.stopToolButton, sb]
        for btn in btns:
            btn.setIconSize(size)
            btn.setAutoRaise(True)
            ly.insertWidget(i, btn)
            nc.enable_disable_btns.append(btn)
            i += 1

        # add the wizard on the end
        if WIZARD_AVAILABLE:
            w = self.wizard = Wizard()

            def wizard_menu_callback():
                w.current_nc = nc  # provide handle to current nodechart
                w.exec_()

            btn = nc.wizardToolButton = QtWidgets.QToolButton()
            btn.setIcon(get_icon('wand.svg'))
            btn.setIconSize(size)
            btn.setAutoRaise(True)
            btn.setCheckable(False)
            btn.setToolTip('Open node wizard')
            btn.clicked.connect(wizard_menu_callback)
            ly.insertWidget(ly.count()-1, btn)
            nc.enable_disable_btns.append(btn)


        # add an attribute for the project manager
        self.nodeChart.project = project

        # add an attribute for the mfixgui
        self.mfixgui = parent
        self.nodeChart.nodeworks_widget = self
        self.nodeChart.mfixgui = parent

        # Build default node library
        self.nodeChart.nodeLibrary.buildDefaultLibrary()

        # Add custom Nodes
        # for node in []:
        #     self.nodeChart.nodeLibrary.addNode(node, ['MFIX', ])

        # --- initialize job status table ---
        self.job_frame = QtWidgets.QWidget()
        self.job_layout = QtWidgets.QVBoxLayout(self.job_frame)
        self.job_layout.setContentsMargins(0, 0, 0, 0)
        self.job_layout.setSpacing(0)
        self.job_frame.setSizePolicy(QtWidgets.QSizePolicy.Expanding,
                                     QtWidgets.QSizePolicy.Preferred)
        self.job_frame.setLayout(self.job_layout)

        self.job_toolbar = QtWidgets.QWidget()
        self.job_toolbar.setSizePolicy(QtWidgets.QSizePolicy.Expanding,
                                       QtWidgets.QSizePolicy.Fixed)
        self.job_toolbar_layout = QtWidgets.QHBoxLayout(self.job_toolbar)
        self.job_toolbar_layout.setContentsMargins(0, 0, 0, 0)
        self.job_toolbar_layout.setSpacing(0)
        self.job_toolbar.setLayout(self.job_toolbar_layout)
        self.job_layout.addWidget(self.job_toolbar)

        self.tool_btn_dict = OrderedDict()
        for tool, icon, callback in [
                ('play', 'play.svg', self.handle_play),
                ('stop', 'stop.svg', self.handle_stop),
                ('pause', 'pause.svg', self.handle_pause),
                ('delete', 'delete.svg', self.handle_delete),
                ('restart', 'restart.svg', self.handle_restart),
                ('auto restart', 'autorenew.svg', self.handle_renew),
                ('remove from queue', 'removefromqueue.svg', self.handle_remove_from_queue),
                ('submit to queue', 'addtoqueue.svg', self.handle_add_to_queue),
                ('open', 'folder.svg', self.handle_open),
                ('settings', 'settings.svg', self.handle_settings)]:
            btn = QtWidgets.QToolButton()
            btn.setIcon(get_icon(icon))
            btn.clicked.connect(callback)
            btn.setAutoRaise(True)
            btn.setEnabled(tool == 'settings')
            btn.setToolTip(tool)
            self.tool_btn_dict[tool] = btn
            self.job_toolbar_layout.addWidget(btn)
        self.job_toolbar_layout.addStretch()

        self.job_status_table = Table(
            dtype=OrderedDict,
            columns=['status', 'job id', 'progress', 'time', 'dt', 'time remaining', 'path'],
            column_delegate={2: {'widget': 'progressbar'}},
            multi_selection=True
            )
        self.job_status_table.set_value(OrderedDict())
        self.job_status_table.show_vertical_header(True)
        self.job_status_table.auto_update_rows(True)
        self.job_status_table.setAutoScroll(False)
        self.job_status_table.default_value = OrderedDict()
        self.job_status_table.setSizePolicy(QtWidgets.QSizePolicy.Expanding,
                                            QtWidgets.QSizePolicy.Preferred)
        self.job_status_table.new_selection.connect(self.update_btns)
        self.job_layout.addWidget(self.job_status_table)

        # splitter
        self.splitter = QtWidgets.QSplitter(QtCore.Qt.Vertical)
        self.splitter.addWidget(self.nodeChart)
        self.splitter.addWidget(self.job_frame)

        # main layout
        self.layout = QtWidgets.QVBoxLayout(self)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.setSpacing(0)
        self.setLayout(self.layout)
        self.layout.addWidget(self.splitter)

    def reset(self):
        self.clear()

    def set_save_btn(self):
        self.mfixgui.set_unsaved_flag()
        self.needs_saved = True

    @property
    def parameter_dict(self):
        return PARAMETER_DICT

    @property
    def used_parameters(self):
        return self.mfixgui.project.parameter_key_map.keys()

    def look_for_projects(self, path):
        data = self.job_status_table.value

        # read the previous status table
        job_status = os.path.join(path, '.nodeworks_run_status.json')
        job_data = {}
        if os.path.exists(job_status):
            with open(job_status, 'r', encoding='utf-8', errors='replace') as json_file:
                job_data = json.load(json_file)
        data.update(job_data)

        # clean table
        for name in list(data.keys()):
            if not os.path.exists(os.path.join(path, name)):
                data.pop(name)

        # look for other simulation directories
        for name in glob.glob(os.path.join(path, '*', '*.mfx')):
            d = os.path.dirname(name)
            dir_base = os.path.basename(d)
            if dir_base not in data:
                data[dir_base] = {'status':'waiting for pid', 'progress':0,
                                  'path':d, 'dt':'None', 'time remaining':'None',
                                  'job id': None}
                run_data_path = os.path.join(d, '.nodeworks_run_cmd.json')
                if os.path.exists(run_data_path):
                    with open(run_data_path, encoding='utf-8', errors='replace') as json_file:
                        json_obj = json.load(json_file)
                    data[dir_base].update(json_obj)
            self.create_job_manager(d)
            self.watch_dir_paths.append(d)

        self.job_status_table.set_value(data)
        if not self.update_timer.isActive():
            self.update_timer.start(1000)
        if not self.file_timer.isActive():
            self.file_timer.start(1000)

    def export_project(self, path=None, param_dict={}, keyword_dict={},
                       restart=None, copy_project=None):
        """
        export a mfix project

        :path: directory to export the project to
        :param_dict: dictionary of parameters and values to use {'x':1.3}
        :keyword_dict: dictionary of keywords and values {'BC_V_g,1': 5.0}
        :restart: must be one of None, 'restart_1', or 'restart_2'
        :copy_project: directory to copy *.RES, *.SP?, and *.pvd files from
        """
        # copy parameters
        param_copy = copy.deepcopy(PARAMETER_DICT)

        # copy project
        proj = copy.deepcopy(self.mfixgui.project)

        # copy files
        proj_dir = self.mfixgui.get_project_dir()
        f_patterns = ['particle_input.dat', 'poly.dat', 'gridmap.dat',
                      'species.inc', 'geometry.stl.original', '*.stl']
        if restart == 'restart_1':
            f_patterns += ['*.res', '*.sp?', '*.pvd']
        elif restart == 'restart_2':
            f_patterns += ['*.res']
        files_to_copy = []
        for f_pattern in f_patterns:
            src = proj_dir
            if copy_project is not None and f_pattern in ['*.res', '*.sp?', '*.pvd']:
                src = copy_project
            files_to_copy += glob.glob(os.path.join(src, case_insensitive(f_pattern)))
        for f in files_to_copy:
            shutil.copyfile(f, os.path.join(path, os.path.basename(f)))

        # change parameters
        PARAMETER_DICT.update(copy.deepcopy(param_dict))
        # change keywords
        for key_args, value in keyword_dict.items():
            key_args = key_args.split(',')
            key = key_args[0]
            if len(key_args) > 1:
                args = [int(arg) for arg in key_args[1:]]
            else:
                args = []
            proj.updateKeyword(key, value, args=args)
        # change run type (restart)
        if restart is not None:
            proj.updateKeyword('run_type', restart)
        else:
            proj.updateKeyword('run_type', 'new')

        run_name = proj.get_value('run_name')
        if run_name is None:
            self.mfixgui.error('The project does not have a run_name, is it a valid project?')
            return

        # copy and create geometry
        if VTK_AVAILABLE:
            geo_engine = GeometryEngine(gui=None, proj=proj)
            geo_engine.geometry_from_str(proj.mfix_gui_comments.get('geometry'),
                                         proj_dir=path)

            loaded_data = ExtendedJSON.loads(proj.mfix_gui_comments.get('regions_dict'))
            data = {}
            for region in loaded_data['order']:
                region_data = data[region] = deepcopy_dict(DEFAULT_REGION_DATA)
                region_data.update(loaded_data['regions'][region])
            geo_engine.region_dict = data

        else:
            self.mfixgui.print_internal(
                "Warning: Could not import VTK, can not create new geometry.",
                color='red')

        # write the project
        copied_proj = os.path.join(path, run_name+'.mfx')
        self.mfixgui.print_internal("Exporting to: %s" % copied_proj,
                                    color='green')
        proj.writeDatFile(copied_proj)

        if VTK_AVAILABLE:
            geo_file = os.path.join(path, 'geometry.stl')
            geo_engine.export_stl(geo_file, self.mfixgui.bcs)

        # reset parameters
        PARAMETER_DICT.update(param_copy)
        for key in set(PARAMETER_DICT.keys())-set(param_copy.keys()):
            PARAMETER_DICT.pop(key)

        return copied_proj

    def run_popup(self):

        parent = MockParent()
        parent.mfix_gui = self.mfixgui
        parent.project_dir = self.mfixgui.get_project_dir()
        parent.project_file = self.mfixgui.get_project_file()
        parent.project = self.project
        parent.settings = SETTINGS
        run_popup = NodeworksRunPopup(None, parent)
        run_popup.ui.setParent(self.mfixgui, QtCore.Qt.Dialog)
        run_popup.ui.exec_()

        # Check for cancel
        if run_popup.cancel:
            return False

        self.solver_options = run_popup.solver_options
        if self.solver_options is None:
            return False
        self.local_queue = run_popup.local_queue

        if self.solver_options[1] is None and self.local_queue:
            # create workers for local queue
            nw = run_popup.spinbox_jobs.value() - len(self.job_workers)
            # start new workers
            if nw > 0:
                for i in range(nw):
                    t = threading.Thread(target=job_worker)
                    t.start()
                    self.job_workers.append(t)
            # shutdown workers
            elif nw < 0:
                [JOB_QUEUE.put(None) for i in range(abs(nw))]
        return True

    def confirm_close(self):

        if self.local_queue and (not JOB_QUEUE.empty() or self.get_running_projects()):
            confirm = self.mfixgui.message(
                text="Currently running local queue. Quitting will shutdown these jobs.\nAre you sure you want to quit?",
                buttons=['ok', 'cancel'],
                default='cancel')
            if confirm == 'cancel':
                return False

        projs = list(self.job_status_table.value.keys())
        self.shutdown_local_queue()
        self.stop_jobs(projs, force=True)

        return True

    def shutdown_local_queue(self):

        # clear out queued jobs
        while not JOB_QUEUE.empty():
            try:
                j = JOB_QUEUE.get_nowait()
                JOB_QUEUE.task_done()
            except asyncio.QueueEmpty:
                continue

        # add poision pill to queue
        [JOB_QUEUE.put(None) for i in range(len(self.job_workers))]

    def run_project(self, mfx_file):
        """
        Run the mfix project

        :mfx_file: path to mfx project file
        """
        proj_dir = os.path.dirname(mfx_file)
        dir_base = os.path.basename(proj_dir)
        data = self.job_status_table.value
        if dir_base in data:
            self.mfixgui.print_internal("Error: Project already submitted: %s" % proj_dir)
            return

        if self.solver_options is None:
            if not self.run_popup():
                return
        queue = self.solver_options[1] is not None

        data[dir_base] = {'status':'waiting for pid', 'progress':0,
                          'path':proj_dir, 'dt':'None', 'time remaining':'None',
                          'job id': None if queue else 'local'}
        self.job_status_table.set_value(data)

        if not os.path.exists(mfx_file):
            self.mfixgui.print_internal("Error: No project file: %s" % proj_dir)
            return

        # run it
        self.mfixgui.print_internal("Running project: %s" % proj_dir)
        job_id = self._run(proj_dir, mfx_file)

        # save some info on the run
        data[dir_base]['queue'] = queue
        data[dir_base]['file'] = mfx_file
        self.job_status_table.set_value(data)

        save_data = {
            'queue': queue,
            'file': mfx_file,
            'job id': job_id if queue else 'local',
        }

        with open(os.path.join(proj_dir, '.nodeworks_run_cmd.json'), 'w', encoding='utf-8', errors='replace') as f:
            json.dump(save_data, f)

        if not self.update_timer.isActive():
            self.update_timer.start(1000)
        if not self.file_timer.isActive():
            self.file_timer.start(1000)

    def _run(self, proj_dir, mfx_file):

        if self.solver_options is None:
            if not self.run_popup():
                return

        # Queue
        job_id = None
        solver, template, omp, nodes = self.solver_options
        rcmd = self.process_manager.get_run_command(solver, omp, nodes, mfx_file)

        queue = template is not None
        if queue:
            msg = 'Submitting to Queue'
            job_id = self.submit_to_queue(proj_dir, rcmd)
        # local
        else:
            msg = 'Running: %s' % ' '.join(rcmd)
            if self.local_queue:
                JOB_QUEUE.put((rcmd, proj_dir, os.environ))
            else:
                parent = MockParent()
                parent.mfix_gui = self.mfixgui
                parent.project_dir = proj_dir
                parent.project_file = mfx_file
                parent.project = Project(mfx_file)
                parent.settings = SETTINGS
                parent.stderr_signal.connect(self.job_error)

                pm = ProcessManager(parent)
                pm.start_solver(*self.solver_options)
                self.process_managers.append(pm)


        self.watch_dir_paths.append(proj_dir)
        self.mfixgui.print_internal(msg, color='green')
        return job_id

    def submit_to_queue(self, project_dir, rcmd):
        script_name = '.qsubmit_script'

        solver, template, omp, nodes = self.solver_options
        submit_cmd = self.process_manager.get_submit_command(rcmd, template)

        # write the script
        with open(os.path.join(project_dir, script_name), 'w', encoding='utf-8', errors='replace') as f:
            f.write(submit_cmd.script)

        replace_dict = copy.deepcopy(submit_cmd.replace_dict)
        replace_dict['SCRIPT'] = script_name

        cmd = replace_with_dict(submit_cmd.sub_cmd, replace_dict)

        # submit the job
        self.mfixgui.print_internal("Job submit CMD: {}".format(cmd),
                                    color='blue')

        proc = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE,
                                cwd=project_dir,
                                env=dict(os.environ, LD_PRELOAD=""))
        out, err = proc.communicate()
        job_id_regex = submit_cmd.job_id_regex
        if job_id_regex is not None and out:
            job_id = re.findall(job_id_regex, str(out))
        else:
            job_id = []
        if job_id:
            job_id = job_id[0]
            self.mfixgui.print_internal("Job successfully submitted with job id: {}".format(job_id),
                                        color='blue')
        else:
            self.mfixgui.error('Could not determine job id')
            job_id = None
        if err:
            self.mfixgui.error('Error with submission:\n{}'.format(err))

        dir_base = os.path.basename(project_dir)
        data = self.job_status_table.value
        data[dir_base]['job id'] = job_id
        data[dir_base]['status'] = 'submitted to queue'

        return job_id

    def look_for_pid(self):

        for d in copy.deepcopy(self.watch_dir_paths):
            pid_files = glob.glob(os.path.join(d, '*.pid'))
            if pid_files:
                self.create_job_manager(d)
        if len(self.watch_dir_paths) == 0:
            self.file_timer.stop()

    def create_job_manager(self, proj_dir):
        pid_files = glob.glob(os.path.join(proj_dir, '*.pid'))
        dir_base = os.path.basename(proj_dir)
        if pid_files:
            if len(pid_files) > 1:
                self.mfixgui.print_internal('more than one pid file', color='red')

            mfx_files = glob.glob(os.path.join(proj_dir, '*.mfx'))

            parent = MockParent()
            parent.mfix_gui = self.mfixgui
            parent.project_dir = proj_dir
            parent.project_file = mfx_files[0]
            parent.project = Project(mfx_files[0])
            parent.settings = SETTINGS

            full_run_name_pid = os.path.join(proj_dir, pid_files[0])
            job = JobManager(parent)
            job.try_to_connect(full_run_name_pid)
            self.job_dict[dir_base] = job
            try:
                self.watch_dir_paths.remove(proj_dir)
            except ValueError:
                # already removed?
                pass
        else:
            self.job_dict[dir_base] = FakeJob()

    def job_error(self, error):
        self.mfixgui.print_internal(error, color='red') # use mfixgui.error function

    def save(self, fname):
        """save a node chart file at the given path"""
        self.nodeChart.save(path=fname)
        self.needs_saved = False

    def load(self, fname):
        """load node chart file"""
        self.nodeChart.open(path=fname)
        self.needs_saved = False

    def clear(self):
        """clear all nodes"""
        self.nodeChart.deleteAllNodes(confirm=False)

    # --- job update ---
    def update_job_status(self):
        """update the current job status"""

        data = self.job_status_table.value

        for job_name in data.keys():
            if job_name in self.job_dict:

                job = self.job_dict[job_name].job

                if job is None:
                    continue

                if job.status:
                    s = job.status
                    time = s.get('time', 0)
                    tstop = s.get('tstop', 100)
                    paused = s.get('paused')
                    running = s.get('running')
                    finished = s.get('finished')
                    dt = s.get('dt')
                    t_remain = s.get('walltime_remaining')

                    if paused:
                        status = 'paused'
                    elif running:
                        status = 'running'
                    else:
                        status = ('finished' if time >= tstop*.95 else
                                  'stopped')

                    if finished:
                        job.exit_mfix()

                    data[job_name]['progress'] = (100 if status == 'finished' else
                                                  time/tstop*100)

                    if dt is not None:
                        data[job_name]['dt'] = '{0:.2e}'.format(dt)
                    try:
                        data[job_name]['time remaining'] = '{0:.3g}'.format(float(t_remain))
                    except:
                        pass
                    try:
                         data[job_name]['time'] = '{0:.3g}'.format(float(time))
                    except:
                        pass
                    data[job_name]['status'] = status

        self.job_status_table.set_value(data)
        try:
            with open('.nodeworks_run_status.json', 'w', encoding='utf-8', errors='replace') as json_file:
                json.dump(data, json_file)
        except Exception as e:
            self.mfixgui.print_internal('Failed to save {}:\n{}'.format('.nodeworks_run_status.json', e), color='red') # use mfixgui.error function

    # --- job management ---
    def get_selected_jobs(self):
        """get the currently selected jobs"""
        projs = list(self.job_status_table.value.keys())
        return [self.job_dict[projs[i]] for i in self.job_status_table.current_rows()
                if self.job_dict[projs[i]].job is not None]

    def get_selected_projects(self):
        """get the currently selected project names"""
        projs = list(self.job_status_table.value.keys())
        return [projs[i] for i in self.job_status_table.current_rows()]

    def get_running_projects(self):
        running_jobs = []
        for proj, data in self.job_status_table.value.items():
            if  data['status'] in ['running', 'paused']:
                running_jobs.append(proj)
        return running_jobs

    def update_btns(self):
        """enable/disable btns"""

        n_btns = len(self.tool_btn_dict)
        enable_list = [False]*(n_btns-1)

        projs = self.get_selected_projects()

        if projs:
            enable_list[:4] = [True]*5
        if len(projs) == 1:
            enable_list[n_btns-2] = True

        for enable, btn in zip(enable_list, list(self.tool_btn_dict.values())[:-1]):
            btn.setEnabled(enable)

    def handle_play(self):
        """play the selected job"""
        jobs = self.get_selected_jobs()
        for job in jobs:
            job.job.unpause()

    def handle_stop(self):
        """stop the selected job"""
        projs = self.get_selected_projects()
        self.stop_jobs(projs)

    def stop_jobs(self, projs, force=False):
        """stop the given list of jobs"""
        data = self.job_status_table.value
        for proj in projs:
            job = self.job_dict.get(proj, None)
            if job is None:
                continue
            if not isinstance(job, FakeJob):
                if force:
                    job.force_stop_mfix()
                else:
                    job.stop_mfix()

            data[proj]['status'] = 'stopped'
        self.job_status_table.set_value(data)

    def handle_pause(self):
        """pause the selected job"""
        jobs = self.get_selected_jobs()
        for job in jobs:
            job.job.pause()

    def handle_delete(self):
        """delete the selected job"""
        projs = self.get_selected_projects()

        btn = self.mfixgui.message(
            text='The selected directories will be delete.\nContinue?',
            buttons=['yes', 'no'],
            default='no',)

        if btn != 'yes':
            return

        data = self.job_status_table.value
        self.job_status_table.clear_selection()
        for proj in projs:

            # make sure the job is stopped
            job = self.job_dict.get(proj, None)
            if job is not None and not isinstance(job, FakeJob):
                job.stop_mfix()
                data[proj]['status'] = 'stopped'

            # remove the dir
            path = data[proj]['path']
            try:
                shutil.rmtree(path)
            except Exception as e:
                msg = "Error deleting directory: {}\n{}".format(path, e)
                self.mfixgui.print_internal(msg, color='red')
                continue

            data.pop(proj)
            if proj in self.job_dict:
                self.job_dict.pop(proj)

        self.job_status_table.set_value(data)

    def handle_restart(self):
        """restart the selected job"""
        projs = self.get_selected_projects()
        data = self.job_status_table.value
        for proj in projs:
            p = data[proj]

            # make sure the job is stopped
            job = self.job_dict[proj]
            if not isinstance(job, FakeJob):
                job.stop_mfix()
                p['status'] = 'stopped'


            # read the project
            proj = Project(p['file'])

            # look for *.SP? files
            spx_files = glob.glob(os.path.join(p['path'], '*SP?'))

            restart = 'restart_2'
            if spx_files:
                restart = 'restart_1'

            proj.updateKeyword('run_type', restart)
            proj.writeDatFile(p['file'])

            self._run(p['path'], p['file'])
            p['status'] = 'waiting for pid'

        self.job_status_table.set_value(data)

    def handle_remove_from_queue(self):
        """remove job from queue"""
        # projs = self.get_selected_projects()
        print('remove')

    def handle_add_to_queue(self):
        """add job to queue"""
        # projs = self.get_selected_projects()
        print('add')

    def handle_renew(self):
        """auto restart the selected job"""
        # projs = self.get_selected_projects()
        print('auto restart')

    def handle_open(self):
        """open the selected job"""
        projs = self.get_selected_projects()
        if not projs:
            return

        data = self.job_status_table.value
        path = data[projs[0]]['path']

        #TODO: this is hard coded to > python gui.py project
        gui_path = os.path.join(SCRIPT_DIRECTORY, 'gui.py')
        cmd = []
        if self.running_in_vnc:
            cmd.append('vglrun')
        cmd += ['python', gui_path, path]
        subprocess.Popen(cmd)

    def handle_settings(self):
        """open the run settings dialog"""
        self.tool_btn_dict['settings'].setDown(False)
        self.run_popup()

    def handle_import(self):
        """immport a nc file"""
        new_nc, _ = QtWidgets.QFileDialog.getOpenFileName(
            self, "Open a node chart",
            self.mfixgui.get_project_dir(),
            'Node Chart (*.nc);; All Files (*)')
        if not new_nc:
            return

        self.nodeChart.open(path=new_nc)
        self.mfixgui.hide_file_menu()

    def handle_export(self):
        """export a nc file"""

        new_nc, _ = QtWidgets.QFileDialog.getSaveFileName(
            self, "Save the node chart",
            self.mfixgui.get_project_dir(),
            'Node Chart (*.nc)')
        if not new_nc:
            return

        self.nodeChart.save(path=new_nc)
        self.mfixgui.hide_file_menu()
