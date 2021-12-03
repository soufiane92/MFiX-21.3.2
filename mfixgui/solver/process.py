from collections import namedtuple
import errno
import logging
import os

from PyQt5.QtCore import QProcess, QProcessEnvironment, QObject

from mfixgui.tools import replace_with_dict, get_mfix_home


class ProcessManager(QObject):
    """ A command to start a local solver QProcess and add it to self.mfixgui.mfix_process """

    def __init__(self, mfixgui):
        super().__init__()
        self.mfixgui = mfixgui
        self.mfix_process = None
        self.cmd_str = None

    def start_solver(self, solver, template, omp_num_threads, nodes, project_file=None):
        """ Runs a local job if template is None, and submit to queue otherwise """

        self.get_mfixgui_ready_to_run_solver()
        self.remove_mfix_stop()
        run_command = self.get_run_command(solver, omp_num_threads, nodes, project_file=project_file)

        if template is None:
            msg = "Starting %s" % (" ".join(run_command))
            self.mfixgui.print_internal(msg, color="blue")
            self.start(cmd=run_command)

        else:
            submit_cmd = self.get_submit_command(run_command, template)

            msg = "Submitting to queue"
            self.mfixgui.print_internal(msg, color="blue")

            self.mfixgui.job_manager.submit_command(
                submit_cmd.script,
                submit_cmd.sub_cmd,
                submit_cmd.delete_cmd,
                submit_cmd.status_cmd,
                submit_cmd.job_id_regex,
                submit_cmd.replace_dict,
            )

    def get_mfixgui_ready_to_run_solver(self):
        """ Prepare MfixGui for a running solver """
        self.mfixgui.reset_plots()
        self.mfixgui.close_log_files()  # Close any logging from previous run
        self.mfixgui.open_log_files()  # Open new log files for this run
        self.mfixgui.job_manager.stopping = False
        self.mfixgui.job_manager.pausing = False
        self.mfixgui.status_manager.last_run_msg_time = 0.0
        self.mfixgui.console_printer.clear_messages()

    def remove_mfix_stop(self):
        """ Removes MFIX.STOP file to prepare for starting the solver """
        mfix_stop_file = os.path.join(self.mfixgui.get_project_dir(),
                                      "MFIX.STOP")
        if os.path.isfile(mfix_stop_file):
            try:
                os.unlink(mfix_stop_file)
            except OSError:
                self.mfixgui.warn("Cannot remove %s" % mfix_stop_file)
                return

    def get_run_command(self, solver, omp_num_threads, nodes, project_file=None):
        """returns argv list of strings for the run command, given a Solver, the
self.mfixgui install, and SMP/DMP info"""

        nodesi, nodesj, nodesk = nodes
        run_cmd = []

        if solver.smp_enabled():
            run_cmd += ["env", "OMP_NUM_THREADS=%s" % omp_num_threads]

        node_count = nodesi * nodesj * nodesk
        if solver.dmp_enabled() and node_count > 1:
            run_cmd += [
                "mpirun",
                "--use-hwthread-cpus",  # issues/1247
                # "-quiet",
                # "-mca", "orte_create_session_dirs", "true",
                "-mca",
                "mpi_warn_on_fork",
                "0",
                "-np",
                str(node_count),
            ]

        run_cmd += [str(solver.path)]

        # Add 'server' flag to start HTTP server
        if solver.python_enabled():
            run_cmd += ["-s"]

        if project_file is None:
            project_file = self.mfixgui.get_project_file()
        run_cmd += ["-f", project_file]

        return run_cmd

    def get_submit_command(self, cmd, template):
        """ Return a Named Tuple containing everything needed to start a queue job """
        mfix_run_cmd = " ".join(cmd)
        command = " ".join(["env", f'MFIX_RUN_CMD="{mfix_run_cmd}"'] + cmd)

        run_name = self.mfixgui.project.get_value("run_name", default="")
        replace_dict = template.template_values()
        replace_dict.update(
            {"PROJECT_NAME": run_name,
             "COMMAND": command,
             "MFIX_HOME": get_mfix_home()}
        )

        # replace twice to make sure that any references added the first time
        # get replaced
        script = replace_with_dict(template.get_script(), replace_dict)
        script = replace_with_dict(script, replace_dict)

        sub_cmd = template.submit()
        delete_cmd = template.delete()  # XXX
        status_cmd = template.status()
        job_id_regex = template.job_id_regex()

        SubmitCommand = namedtuple(
            "SubmitCommand",
            [
                "script",
                "sub_cmd",
                "delete_cmd",
                "status_cmd",
                "job_id_regex",
                "replace_dict",
            ],
        )
        return SubmitCommand(
            script, sub_cmd, delete_cmd, status_cmd, job_id_regex, replace_dict
        )

    def disconnect(self):
        """ disconnect stdout/stderror from mfix process """
        if self.mfix_process is None:
            return

        sigs = [
            self.mfix_process.error,
            self.mfix_process.readyReadStandardError,
            self.mfix_process.started,
            self.mfix_process.readyReadStandardOutput,
            self.mfix_process.finished,
        ]
        for sig in sigs:
            sig.disconnect()

        self.mfix_process = None
        self.cmd_str = None

    def start(self, cmd):
        """ Start the (local) solver QProcess """

        self.cmd_str = " ".join(cmd)

        self.mfix_process = QProcess()
        if not self.mfix_process:
            print("QProcess creation failed") # FIXME popup error
            return

        cwd = self.mfixgui.get_project_dir()
        self.mfix_process.setWorkingDirectory(cwd)

        self.mfix_process.started.connect(self.slot_start)
        self.mfix_process.readyReadStandardOutput.connect(self.slot_read_out)
        self.mfix_process.readyReadStandardError.connect(self.slot_read_err)
        self.mfix_process.finished.connect(self.slot_finish)
        self.mfix_process.error.connect(self.slot_error)

        start_detached = True

        # if sys.platform.startswith('win') or 'mpirun' not in cmd:
        #    start_detached = False
        # On Windows, start_detached gives a DOS box
        # What was the issue with mpirun?
        start_detached = False

        # https://bugreports.qt.io/browse/QTBUG-2284
        # QProcessEnvironment does not work with startDetached,
        #   fixed in Qt5.10 which we aren't using yet
        saved_env = None
        if not start_detached:
            process_env = QProcessEnvironment()
            add_env = process_env.insert
        else:
            add_env = os.environ.__setitem__
            saved_env = os.environ.copy()

        for key, val in os.environ.items():
            add_env(key, val)

        add_env("MFIX_RUN_CMD", " ".join(cmd))

        if not start_detached:
            self.mfix_process.setProcessEnvironment(process_env)
            self.mfix_process.start(cmd[0], cmd[1:])
        else:
            self.mfix_process.startDetached(cmd[0], cmd[1:])

        # restore environment
        if saved_env:
            for (key, value) in list(os.environ.items()):
                if key not in saved_env:
                    del os.environ[key]
                elif value != saved_env[key]:
                    os.environ[key] = saved_env[key]

        self.mfixgui.slot_rundir_timer()

    def slot_start(self):
        # processId was added in qt 5.3
        if hasattr(self.mfix_process, "processId"):
            pid = self.mfix_process.processId()
        else:
            # .pid() is obsolete.  Note that it returns a pointer on Windows,
            # not the real process ID
            pid = self.mfix_process.pid()

        msg = "MFiX process %d is running" % pid
        self.mfixgui.signal_update_runbuttons.emit(msg)

    def slot_read_out(self):
        out_str = bytes(self.mfix_process.readAllStandardOutput()).decode(
            "utf-8", errors="ignore"
        )
        self.mfixgui.stdout_signal.emit(out_str)

    def slot_read_err(self):
        err_str = bytes(self.mfix_process.readAllStandardError()).decode(
            "utf-8", errors="ignore"
        )
        self.mfixgui.stderr_signal.emit(err_str)

    def slot_finish(self, status):
        self.mfixgui.remove_mesh_tempfiles()
        self.mfixgui.close_log_files()  # Close any open files

        if self.mfixgui.job_manager.job:
            self.mfixgui.job_manager.job.cleanup_and_exit()
            self.mfixgui.job_manager.job = None
            msg = "MFiX process has stopped"
            self.mfixgui.signal_update_runbuttons.emit(msg)

        if self.mfixgui.job_manager.pidfile:
            try:
                os.unlink(self.mfixgui.job_manager.pidfile)
                self.mfixgui.job_manager.pidfile = None
            except OSError as err:
                if err.errno != errno.ENOENT:
                    raise

    def slot_error(self, error):
        if error == QProcess.FailedToStart:
            msg = "Process failed to start " + self.cmd_str
        elif error == QProcess.Crashed:
            msg = "Process exit " + self.cmd_str
        elif error == QProcess.Timedout:
            msg = "Process timeout " + self.cmd_str
        elif error in (QProcess.WriteError, QProcess.ReadError):
            msg = "Process communication error " + self.cmd_str
        else:
            msg = "Unknown error " + self.cmd_str

        # make the message print in red
        self.mfixgui.stderr_signal.emit(msg)
