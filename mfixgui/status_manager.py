import os
import pprint
import time

from mfixgui.tools import safe_float


class StatusManager:
    """ This class is responsible for updating the UI based on changes in job status """

    def __init__(self, mfixgui):
        self.job_state = None
        self.mfixgui = mfixgui
        self.last_run_msg_time = 0.0
        self.last_run_msg_prefix = ""

    def update_status_if_changed(self):
        new_job_state = "stopped"
        if self.mfixgui.job_manager and self.mfixgui.job_manager.job:
            new_job_state = self.update_status()
        if new_job_state != self.job_state:
            prev_state = self.job_state
            self.job_state = new_job_state
            self.update_runbuttons(print_in_console=(prev_state is not None))

    def update_status(self):
        status = self.mfixgui.job_manager.job.status

        self.update_residuals_pane()

        response_text = self.mfixgui.ui.responses.toPlainText().lower()
        if not response_text:
            self.mfixgui.ui.responses.setText("ok")

        if not self.mfixgui.job_manager.job.is_paused():
            self.mfixgui.update_plots(status)

        self.mfixgui.update_dashboard(status)

        # Handle logging of dt, nit, residuals
        self.mfixgui.update_logger_status(status)

        self.update_progress_bar(status)

        if self.mfixgui.job_manager.job.is_paused():
            self.mfixgui.job_manager.pausing = False
            self.mfixgui.job_manager.stopping = False

        new_job_state = ("stopped" if not self.mfixgui.job_manager.job
                         else "stopping" if self.mfixgui.job_manager.stopping
                         else "pausing" if self.mfixgui.job_manager.pausing
                         else "paused" if self.mfixgui.job_manager.job.is_paused()
                         else "running")

        self.update_status_message(status, new_job_state)

        # If the job has finished, collect final output from the webserver
        #  and then tell the server it can go away

        if status.get("finished") is True and status.get("running") is False:
            # Collect any stdout/stderr
            if not self.mfixgui.job_manager.is_output_pending():
                self.mfixgui.job_manager.exit_mfix()

        return new_job_state

    def update_residuals_pane(self):
        if not(self.mfixgui.get_developer_mode()):
            return
        status_text = pprint.pformat(self.mfixgui.job_manager.job.status)
        header = "<html><body><pre>"
        footer = "</pre></body></html>"
        status_text = "%s%s%s" % (header, status_text, footer)
        self.mfixgui.ui.residuals.setText(status_text)

    def update_progress_bar(self, status):
        if not status.get("running"):
            self.mfixgui.progress_bar.hide()
            return

        percent = None
        if status.get("finished"):
            percent = 100
        else:
            t = status.get("time", None)
            ts = status.get("tstop", None)
            if t is not None and ts is not None:
                percent = 100 * safe_float(t, 1.0) / safe_float(ts, 1.0)
        if percent is not None:
            self.mfixgui.progress_bar.setValue(int(percent))
        self.mfixgui.progress_bar.show()

    def update_status_message(self, status, new_job_state):
        w = status.get("walltime_elapsed", 0)
        w = safe_float(w, 0.0)
        m, s = divmod(w, 60)
        h, m = divmod(m, 60)

        if self.mfixgui.job_manager.job:
            self.status_message(
                "MFiX %s: elapsed time %d:%02d:%02d" % (new_job_state, h, m, s),
                print_in_console=True)

    def update_runbuttons(self, message=None, print_in_console=True):
        # This is the main state-transition handler
        if message is not None:
            if not "running" in message.lower():
                # highlight for visibility, this is an important state change
                self.mfixgui.print_internal(message, color="blue")

        # TODO: set this in __init__ or another early setup method
        # assemble list of available executables

        project_file = os.path.basename(self.mfixgui.get_project_file() or "")
        project_open = bool(project_file and self.mfixgui.open_succeeded)
        pending = self.mfixgui.job_manager.is_job_pending()
        # why both paused and unpaused states?
        paused = bool(self.mfixgui.job_manager.job and self.mfixgui.job_manager.job.is_paused())
        pausing = self.mfixgui.job_manager.pausing
        if paused:
            pausing = False

        unpaused = self.mfixgui.job_manager.job and not paused
        resumable = (bool(self.mfixgui.get_res_files())
                     and not self.mfixgui.job_manager.job)

        stopping = self.mfixgui.job_manager.stopping
        self.mfixgui.update_window_title()  # put run state in window titlebar

        self.mfixgui.enable_input(enabled=project_open and not (pending or unpaused or paused or resumable),
                                  partial=project_open and (paused or resumable))

        if pending:
            self.set_run_button(enabled=False)

        elif unpaused:
            self.set_run_button(enabled=False)

        elif paused:
            self.set_run_button(text="Unpause", enabled=True)
            if not "paused" in self.mfixgui.ui.label_status.text().lower():
                self.status_message("MFiX paused", print_in_console=False) # Want solver to print message

        elif resumable:
            self.set_run_button(text="Resume", enabled=True)
            self.status_message("Previous MFiX run is resumable.  Reset job to edit model")

        else:  # Not running
            self.set_run_button(text="Run", enabled=project_open)
            self.status_message("Ready", print_in_console=print_in_console)

        self.set_reset_button(enabled=resumable)
        self.set_pause_button(text="Pause",
                              enabled=unpaused and not (stopping or pausing))

        self.set_stop_button(enabled=pending or unpaused or paused)
        self.set_build_button(enabled=not (pending or unpaused or paused))

    def set_run_button(self, text=None, enabled=None):
        b = self.mfixgui.ui.toolbutton_run_mfix
        if self.mfixgui.sms_mode:
            if not self.mfixgui.mesh_accepted:
                b.setEnabled(False)
                b.setToolTip('Mesh must be accepted before starting simulation')
            else:
                if enabled is not None:
                    b.setEnabled(enabled)
                b.setToolTip('Run MFiX simulation')
        if text is not None:
            b.setToolTip("Resume previous MFiX run" if text == "Resume"
                         else text + " MFiX simulation")
        if enabled is not None:
            b.setEnabled(enabled)
            # disable reset (delete) button while a simulation is running (issues/403)
            self.set_reset_button(enabled)

    def set_pause_button(self, text=None, enabled=None):
        b = self.mfixgui.ui.toolbutton_pause_mfix
        if enabled is not None:
            b.setEnabled(enabled)
        if text is not None:
            b.setToolTip(text + " MFIX")

    def set_stop_button(self, enabled):
        b = self.mfixgui.ui.toolbutton_stop_mfix
        b.setEnabled(enabled)
        # tooltip?

    def set_reset_button(self, enabled):
        files = False
        if enabled:
            files = self.mfixgui.get_output_files()
        b = self.mfixgui.ui.toolbutton_reset_mfix
        b.setEnabled(enabled and bool(files))

    def set_build_button(self, enabled):
        b = self.mfixgui.ui.toolbutton_compile
        b.setEnabled(enabled)

    def status_message(self, message="", print_in_console=True):
        """set the status text"""
        if message.strip() == self.mfixgui.ui.label_status.text().strip():
            return
        # pad text (why?)
        message += "  "
        self.mfixgui.ui.label_status.setText(message)

        message_prefix = message.split(":", 1)[0]
        if print_in_console:
            now = time.time()
            if not self.last_run_msg_time:
                self.last_run_msg_time = 0.0

            if ((now - self.last_run_msg_time) >= 60
                or message_prefix != self.last_run_msg_prefix):
                self.last_run_msg_time = now
                self.last_run_msg_prefix = message_prefix
                self.mfixgui.print_internal(message, color="blue")
