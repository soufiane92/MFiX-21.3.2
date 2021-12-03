import json
import multiprocessing
import os

from os.path import join
from distutils import spawn
from glob import glob

from collections import OrderedDict

from qtpy.QtCore import Qt, QObject, Signal

from qtpy.QtWidgets import (QComboBox, QFileDialog, QLabel,
                            QSpinBox, QListWidget, QLineEdit)

from mfixgui.tools import plural, safe_int
from mfixgui.tools.qt import (clear_layout, get_icon, sub_icon_size,
                              get_ui, widget_iter)

from mfixgui.template_manager import (QueueTemplateManager, QueueTemplate,
                                      init_template_manager)

from mfixgui.tools.qt import SETTINGS
from mfixgui.solver.manager import SolverManagerFactory, SolverStatus

RECENT_EXE_LIMIT = 5
SPX_GLOB = ["*.sp?"]
RESTART_2_REMOVE_GLOB = ["*.sp?", "*.pvd", "*.vtp", "*.vtu"]
RESTART_TYPES = OrderedDict(
    [("Resume", "restart_1"), ("Use as initial condition", "restart_2")]
)
RESTART_TYPES_INVS = {v: k for k, v in RESTART_TYPES.items()}


def open_run_popup(mfixgui):
    if not check_custom_solver_if_udfs(mfixgui):
        return
    # 1) Why are we creating a new RunPopup each time, and why are we sticking it in
    # the parent's namespace?
    # 2) We need better way of getting hold of global singleton mfixgui.
    # No need to be passing it as a parameter (twice here even)
    # 3) is it a Dialog or Popup?
    mfixgui.run_popup = RunPopup(mfixgui, mfixgui, mfixgui.commandline_option_exe)
    mfixgui.run_popup.popup()


def check_custom_solver_if_udfs(mfixgui):
    project_dir = mfixgui.get_project_dir()
    mfixsolver = glob(join(project_dir, "mfixsolver*"))
    if mfixsolver:
        return True

    project_dir = mfixgui.get_project_dir()
    if not list(glob(join(project_dir, "*.f"))):
        return True
    return confirm_ignore_udfs(
        mfixgui,
        (
            "Fortran source files exist for this project, but "
            "there is no custom solver in the project directory. "
            "This case probably won't run correctly unless this "
            "project's custom mfixsolver is selected. Proceed anyway?"
        ),
    )


def confirm_ignore_udfs(mfixgui, udf_msg):
    project_dir = mfixgui.get_project_dir()
    if not list(glob(join(project_dir, "*.f"))):
        return True

    response = mfixgui.message(
        title="Warning",
        icon="question",
        text=udf_msg,
        buttons=["yes", "no"],
        default="no",
    )
    return response == "yes"


n_cpus = multiprocessing.cpu_count()

class RunPopup(QObject):
    templates_updated = Signal()
    solvers_updated = Signal()

    def __init__(self, parent, mfixgui, cmdline_solver):
        super(RunPopup, self).__init__()
        self.mfixgui = mfixgui
        self.gui_comments = self.mfixgui.project.mfix_gui_comments
        self.solver_manager = SolverManagerFactory(
            self.mfixgui, self.solvers_updated, cmdline_solver
        ).solver_manager()

        # load ui
        ui = self.ui = get_ui("run_popup.ui")
        ui.setParent(parent, Qt.Dialog)
        ui.setModal(True)
        ui.closeEvent = self.closeEvent
        ui.layout.setSizeConstraint(ui.layout.SetFixedSize)

        ui.toolbutton_browse.clicked.connect(self.handle_browse_exe)
        ui.toolbutton_browse.setIcon(get_icon("add.svg"))
        ui.toolbutton_browse.setIconSize(sub_icon_size())

        ui.toolbutton_remove.clicked.connect(self.handle_remove_exe)
        ui.toolbutton_remove.setIcon(get_icon("remove.svg"))
        ui.toolbutton_remove.setIconSize(sub_icon_size())

        ui.toolbutton_view_error.clicked.connect(self.show_solver_error)

        ui.listwidget_solver_list.itemSelectionChanged.connect(self.slot_select_solver)
        ui.combobox_restart.addItems(RESTART_TYPES.keys())
        ui.combobox_restart.hide()

        ui.button_run.clicked.connect(self.handle_run)
        ui.button_cancel.clicked.connect(self.ui.close)

        txt = plural(n_cpus, "core")
        ui.groupbox_smp_options.setTitle("SMP options (%s available locally)" % txt)
        ui.groupbox_dmp_options.setTitle("DMP options (%s available locally)" % txt)
        ui.groupbox_queue.toggled.connect(self.toggle_run_btn_text)
        ui.widget_queue_options.hide()
        ui.combobox_template.setEnabled(True)

        ui.checkbox_keyword_bdist_io.clicked.connect(
            lambda x, mfixgui=mfixgui: mfixgui.update_keyword('bdist_io', x))


        self.init_ui()
        self.init_templates()
        self.solvers_updated.connect(self.update_solver_listbox)

        self.update_dialog()
        self.ui.listwidget_solver_list.setCurrentRow(0)

    def slot_select_solver(self):
        self.update_dialog()
        self.save_selected_exe()

    @property
    def solver(self):
        """The currently selected solver"""
        item = self.ui.listwidget_solver_list.currentItem()
        if item is None:
            return None

        return self.solver_manager.get_solver_from_text(item.text())

    def project_dir(self):
        projdir = self.mfixgui.get_project_dir()
        if projdir is None:
            return None
        return projdir

    def toggle_run_btn_text(self):
        txt = "Submit" if self.ui.groupbox_queue.isChecked() else "Run"
        self.ui.button_run.setText(txt)

    def update_gui_comment(self, key, val):
        if self.gui_comments.get(key) != val:
            self.gui_comments[key] = val
            self.mfixgui.set_unsaved_flag()

    # UI update functions
    def init_ui(self):
        self.ui.setWindowTitle("Run solver")
        self.init_restart()
        self.init_smp()
        self.init_dmp()
        self.init_solver_list()
        self.ui.groupbox_queue.setChecked(
            bool(int(self.gui_comments.get("submit_to_queue", False))))
        self.ui.checkbox_keyword_bdist_io.setChecked(
            bool(int(self.mfixgui.project.get_value('bdist_io', False))))

    def init_restart(self):
        spx_files = self.mfixgui.get_output_files(SPX_GLOB)
        res = self.mfixgui.get_res_files()
        enable = bool(spx_files) or bool(res)
        self.ui.groupbox_restart.setEnabled(enable)
        if not enable:
            self.ui.groupbox_restart.setChecked(False)
            self.ui.groupbox_restart.setTitle("Restart - no restart files found")

        restart_1 = bool(spx_files) and bool(res)
        if not restart_1:
            self.ui.combobox_restart.setCurrentIndex(1)

        self.enable_restart_item(RESTART_TYPES_INVS["restart_1"], restart_1)
        self.enable_restart_item(RESTART_TYPES_INVS["restart_2"], bool(res))

    def init_smp(self):
        project_threads = self.gui_comments.get("OMP_NUM_THREADS", "1")
        n_threads = os.environ.get("OMP_NUM_THREADS", project_threads)
        n_threads = safe_int(n_threads, default=1)
        sb = self.ui.spinbox_threads
        sb.setValue(n_threads)
        sb.valueChanged.connect(self.update_total_dmp)
        sb.valueChanged.connect(self.update_total_smp)
        self.update_total_smp(n_threads)

    def init_dmp(self):
        nodes = [(self.ui.spinbox_nodesi, "nodesi"),
            (self.ui.spinbox_nodesj, "nodesj"),
            (self.ui.spinbox_nodesk, "nodesk")]
        for (sb, kw) in nodes:
            val = self.mfixgui.project.get_value(kw, default=1)
            sb.setValue(val)
            sb.valueChanged.connect(self.update_total_dmp)
        self.update_total_dmp()

    def init_solver_list(self):
        self.update_solver_listbox()
        self.ui.listwidget_solver_list.setCurrentRow(0)

    def update_total_dmp(self):
        val =(self.ui.spinbox_nodesi.value()
            * self.ui.spinbox_nodesj.value()
            * self.ui.spinbox_nodesk.value())
        label = self.ui.label_total_dmp
        txt = str(val)
        if val > n_cpus:
            txt += ' ⚠'
        label.setText(txt)
        label.setStyleSheet("color: red;" if val > n_cpus else "")

    def update_total_smp(self, val):
        sb = self.ui.spinbox_threads
        for w in widget_iter(sb):
            if isinstance(w, QLineEdit):
                w.setStyleSheet("color: red;" if val > n_cpus else "")
        l = self.ui.label_smp_warning
        l.setText("⚠" if val > n_cpus else "")
        l.setStyleSheet("color: red;")

    def init_templates(self):
        self.template_manager = QueueTemplateManager(self.templates_updated)
        init_template_manager(self.template_manager)

        self.templates_updated.connect(self.update_template_combobox)
        self.ui.browse_template_toolButton.clicked.connect(self._handle_browse_template)
        self.ui.browse_template_toolButton.setIcon(get_icon("add.svg"))
        self.ui.browse_template_toolButton.setIconSize(sub_icon_size())

        self.ui.combobox_template.currentIndexChanged.connect(self.select_template)

        self.ui.delete_template_toolButton.clicked.connect(
            lambda: self.template_manager.remove(
                self.ui.combobox_template.currentText()
            )
        )
        self.ui.delete_template_toolButton.setIcon(get_icon("remove.svg"))
        self.ui.delete_template_toolButton.setIconSize(sub_icon_size())

        curr_template = "Joule"
        template_values = None
        temp = self.gui_comments.get("queue_template")
        if temp:
            template_values = json.loads(temp)
            t_name = template_values.get("template")
            if t_name:
                curr_template = t_name
        self.update_template_combobox(curr_template)
        if template_values is not None:
            self.current_template().init_values(template_values)

    def current_template(self):
        return self.get_template(self.ui.combobox_template.currentText())

    def get_template(self, template_name):
        if template_name in self.template_manager.template_keys():
            return self.template_manager[template_name]
        return None

    def update_template_combobox(self, name="Joule"):
        """" Update when template is added or removed """
        self.ui.combobox_template.clear()
        self.ui.combobox_template.addItems(self.template_manager.template_keys())
        cb = self.ui.combobox_template
        for itm in range(cb.count()):
            if str(name).lower() == str(cb.itemText(itm)).lower():
                cb.setCurrentIndex(itm)
                break
        self.update_queue_widgets()

    def select_template(self):
        self.update_queue_widgets()

    def update_queue_widgets(self):
        """" Update widgets when selected template changes """
        queue_template = self.current_template()
        if queue_template is None:
            return

        layout = self.ui.groupbox_queue_options_gridlayout
        if not self._check_submit_command_exists(queue_template):
            return

        self.ui.delete_template_toolButton.setEnabled(not queue_template.is_builtin)

        # remove previous queue widgets
        while layout.count():
            item = layout.takeAt(0)
            if item is None:
                break
            item.widget().setVisible(False)

        # add the selected queue widgets
        layout.addWidget(queue_template.widget, 0, 0)
        queue_template.widget.setVisible(True)

    def _check_submit_command_exists(self, queue_template):
        """ Display a warning and return False if the submit command doesn't exist, otherwise return True """

        cmd = queue_template.submit().split()[0]
        if spawn.find_executable(cmd):
            return True

        label = QLabel(
            'The submission command "{}" does not exist in '
            "the current environment. Please select another "
            "template, edit the template, and/or check your "
            "environment.".format(cmd)
        )
        label.setStyleSheet("color:red")
        label.setWordWrap(True)
        q_options_grid = self.ui.groupbox_queue_options_gridlayout
        q_options_grid.addWidget(label, 0, 0)
        return False

    def enable_restart_item(self, text, enable):
        cb = self.ui.combobox_restart
        model = cb.model()
        index = cb.findText(text)
        item = model.item(index)
        if not enable:
            flags = Qt.NoItemFlags
        else:
            flags = Qt.ItemIsSelectable | Qt.ItemIsEnabled

        item.setFlags(flags)

    def update_dialog(self):
        """ Enable or disable options based on self.solver features,
        local or remote settings """

        ui = self.ui

        if self.solver is None:
            ui.button_run.setEnabled(False)
            ui.groupbox_queue.setEnabled(False)
            ui.groupbox_smp_options.setEnabled(False)
            ui.groupbox_dmp_options.setEnabled(False)
            ui.label_solver_error.setText("")
            ui.toolbutton_view_error.setVisible(False)
            return

        error = self.solver.get_status() == SolverStatus.ERROR
        good = self.solver.get_status() == SolverStatus.GOOD
        smp = self.solver.smp_enabled()
        dmp = self.solver.dmp_enabled()
        no_k = self.mfixgui.project.get_value("no_k")

        ui.toolbutton_view_error.setVisible(error)
        ui.button_run.setEnabled(good)
        ui.groupbox_queue.setEnabled(good)

        ui.groupbox_smp_options.setEnabled(smp)

        ui.groupbox_dmp_options.setEnabled(dmp)
        ui.spinbox_nodesk.setEnabled(dmp and not no_k)

        status_text = self.solver.get_status_text()
        python = self.solver.python_enabled()
        if good and not python:
            status_text = "Cannot control selected solver (not built with support for interactivity)"
        ui.label_solver_error.setText(status_text)

    def show_solver_error(self):
        error_msg = self.solver.get_error()
        self.mfixgui.message(
            text="The solver test failed with the following error:", info_text=error_msg
        )

    def popup(self):
        self.ui.show()
        self.ui.raise_()
        self.ui.activateWindow()

    def closeEvent(self, _event):
        """save information on close"""
        # save solver list
        self.save_selected_exe()

        # queue
        self.save_template()
        self.update_gui_comment(
            "submit_to_queue", int(self.ui.groupbox_queue.isChecked())
        )
        self.update_gui_comment("OMP_NUM_THREADS", str(self.ui.spinbox_threads.value()))

    def save_template(self):
        """Save the current template data"""
        template_txt = self.ui.combobox_template.currentText()
        widget_values = self.current_template().widget_values()
        widget_values["template"] = template_txt
        self.update_gui_comment("queue_template", json.dumps(widget_values))

    def handle_run(self):
        if self.solver is None:
            return
        if self.finish_with_dialog():
            use_queue = self.ui.groupbox_queue.isChecked()
            template = self.current_template() if use_queue else None
            omp_num_threads = self.ui.spinbox_threads.value()

            # collect nodes[ijk] from project to guarantee that mpirun matches
            nodesi = self.mfixgui.project.get_value("nodesi", 1)
            nodesj = self.mfixgui.project.get_value("nodesj", 1)
            nodesk = self.mfixgui.project.get_value("nodesk", 1)

            self.mfixgui.process_manager.start_solver(
                self.solver, template, omp_num_threads, (nodesi, nodesj, nodesk)
            )
        self.mfixgui.slot_update_runbuttons()

    def finish_with_dialog(self):
        """ save run options in project file, then emit run signal """

        self.template_manager.save_settings()
        self.save_template()

        if not self.check_custom_solver_selected():
            return False

        if self.ui.groupbox_restart.isChecked():
            if not self.confirm_restart():
                return False
        else:
            if not self.confirm_new_run():
                return False

        thread_count = str(self.ui.spinbox_threads.value())

        self.update_gui_comment("OMP_NUM_THREADS", thread_count)

        self.save_dmp_keywords()

        if self.mfixgui.unsaved_flag:
            # run_type keyword updated and/or nodesi/nodesj/nodesk
            self.mfixgui.save_project()
        else:
            stl = join(self.project_dir(), "geometry.stl")  # is this needed?
            self.mfixgui.vtkwidget.export_stl(str(stl))

        self.ui.close()
        self.mfixgui.signal_update_runbuttons.emit("")
        return True

    def confirm_restart(self):
        restart_type = RESTART_TYPES.get(
            self.ui.combobox_restart.currentText(), "restart_1"
        )
        self.mfixgui.update_keyword("run_type", restart_type)
        if restart_type != "restart_2":
            return True

        spx_files = self.mfixgui.get_output_files(RESTART_2_REMOVE_GLOB)
        if self.mfixgui.remove_output_files(spx_files, force_remove=True):
            return True
        return False

    def confirm_new_run(self):
        self.mfixgui.update_keyword("run_type", "new")
        output_files = self.mfixgui.get_output_files()
        if not output_files:
            return True

        confirm_delete = self.mfixgui.remove_output_files(
            output_files,
            message_text="Starting a new run requires deleting the following files from the run directory:",
            force_remove=True,
        )
        if confirm_delete:
            return True

        return False

    def save_dmp_keywords(self):
        # collect nodes[ijk]
        dmp = self.solver is not None and self.solver.dmp_enabled()
        nodesi = self.ui.spinbox_nodesi.value() if dmp else 1
        nodesj = self.ui.spinbox_nodesj.value() if dmp else 1
        nodesk = self.ui.spinbox_nodesk.value() if dmp else 1

        no_k = self.mfixgui.project.get_value("no_k")

        # write the correct nodes[ijk] to project file
        self.mfixgui.update_keyword("nodesi", nodesi)
        self.mfixgui.update_keyword("nodesj", nodesj)
        self.mfixgui.update_keyword("nodesk", 1 if no_k else nodesk)

    def handle_remove_exe(self):
        item = self.ui.listwidget_solver_list.currentItem()
        if item is not None:
            path = item.text()
            if not path.startswith("[default]") and not path.startswith("[project"):
                self.solver_manager.remove(path)

    def handle_browse_exe(self):
        """ Handle file open dialog for user specified exe """
        new_exe, _ = QFileDialog.getOpenFileName(
            self.ui,
            "Select executable",
            directory=str(self.project_dir()),
            options=QFileDialog.DontResolveSymlinks,
        )

        if not new_exe:
            return

        self.add_solver(new_exe)

    def add_solver(self, new_exe):
        """ Add the string representing a new solver, to the SolverManager"""
        err_message = self.solver_manager.check_if_invalid_exe(new_exe)
        if err_message is not None:
            self.mfixgui.warn(err_message, popup=True)
            return

        display_name = self.solver_manager.display_name(new_exe)
        if display_name in self.solver_manager.solvers.keys():
            self.mfixgui.message(
                text="The selected solver is already in the list of "
                "available solvers."
            )
            return

        try:
            solver_path = new_exe
            self.solver_manager.add(solver_path)
            self.update_solver_listbox()
            self.ui.listwidget_solver_list.setCurrentRow(0)
        except ValueError as val_err:
            self.mfixgui.message(text=val_err)

    def update_solver_listbox(self):
        selected_item = self.ui.listwidget_solver_list.currentItem()
        selected_item_text = selected_item.text() if selected_item else ""
        lw = self.ui.listwidget_solver_list
        lw.clear()
        lw.addItems([str(path) for path in self.solver_manager.solver_keys()])
        for i in range(lw.count()):
            item = lw.item(i)
            solver = self.solver_manager.get_solver_from_text(item.text())
            item.setIcon(get_icon(solver.icon()))
            if item.text() == selected_item_text:
                self.ui.listwidget_solver_list.setCurrentItem(item)
        self.update_dialog()

    def _handle_browse_template(self):
        new_temp, _ = QFileDialog.getOpenFileName(self.ui,
                                                  "Select a queue template file",
                                                  directory=str(self.project_dir()))
        if new_temp:
            try:
                template = QueueTemplate(new_temp)
                self.template_manager.add(template)
                self.update_template_combobox(str(template.display_name()))
            except ValueError as err:
                self.mfixgui.message(text=f"Invalid template: {err}")

    def save_selected_exe(self):
        """ add new executable to recent list, save in project file and config,
        send signal(s) """
        if self.solver is None:
            self.mfixgui.warn("No solver selected")
            return

        solver_path = str(self.solver.path)
        SETTINGS.setValue("mfix_exe", solver_path)
        self.update_gui_comment("mfix_exe", solver_path)

        lw = self.ui.listwidget_solver_list
        recent_list = [
            lw.item(i).text() for i in range(0, min(RECENT_EXE_LIMIT, lw.count()))
        ]

        # insert new solver in the front
        new_solver_key = str(self.solver_manager.display_name(self.solver.path))
        if new_solver_key in recent_list:
            recent_list.remove(new_solver_key)
        recent_list.insert(0, new_solver_key)

        SETTINGS.setValue("recent_executables", str(os.pathsep).join(recent_list))


    def check_custom_solver_selected(self):
        project_dir = self.mfixgui.get_project_dir()
        if self.solver.path.startswith(project_dir):
            os.path.relpath(self.solver.path, project_dir)
            return True

        return confirm_ignore_udfs(
            self.mfixgui,
            (
                "Fortran source files exist for this project, but "
                "the selected mfixsolver is not in the project directory. "
                "This case probably won't run correctly unless this "
                "project's custom mfixsolver is selected. Proceed anyway?"
            ),
        )
