"""" Module for showing dialog box for generating a bug report as a zipfile,
whenever an uncaught exception occurs in MFiX"""

import shutil
import subprocess
import traceback

import os
from datetime import datetime
from glob import glob
from zipfile import ZipFile

from mfixgui.tools import SCRIPT_DIRECTORY
from mfixgui.tools.qt import SETTINGS

from mfixgui.version_info import get_version_info

try:
    import git
    have_git = True
except:
    have_git = False

FORUM_URL = "https://mfix.netl.doe.gov/forum/"


def excepthook(gui, devmode, etype, exc, tback):
    """ set as sys.excepthook during MFiX startup """
    if devmode or gui is None:
        traceback.print_exception(etype, exc, tback)
        if gui is None:
            return

    if gui.message_box and gui.message_box.isVisible():
        # Avoid cascading dialog boxes
        return

    info = (
        f'An error occurred while running MFiX. Please report this error at\
        <a href="{FORUM_URL}">{FORUM_URL}</a>'
        "<p>If you\
    continue running, the application may become unstable. Consider\
    saving your work now.")

    traceback_text = cleanup_traceback(tback, "Error: %s\n" % exc)

    ret = gui.message(
        text=info,
        buttons=["ok", "cancel"],
        default="cancel" if int(SETTINGS.value("developer_mode", 0)) else "ok",
        traceback_text=traceback_text,
        post_traceback_text="Save diagnostic report?")

    if ret == "ok":
        save_bug_report(gui, traceback_text)


def save_bug_report(gui, traceback_text=None):
    """ Saves a zip file based on whether GUI has a file open """

    project_file = gui.get_project_file() if gui.get_project_file() else None
    if project_file is None:
        zdirname = os.getcwd()
        zfilename_stem = "mfix"
    else:
        zdirname = os.path.dirname(project_file)
        zfilename_stem = gui.project.get_value("run_name", default="mfix").lower()

    timestamp = datetime.now().isoformat().replace(":", "")
    ts_filename = os.path.join(zdirname, f"{zfilename_stem}_{timestamp}.zip")

    zfilename = BugReport(project_file, traceback_text, ts_filename).name()
    zipf = ZipFile(zfilename)
    filenames = "\n".join([str(f) for f in zipf.namelist()])

    gui.message(
        text=f'Bug report saved to:\n\
    <a href="file://{zfilename}">{zfilename}</a>\n\
    You can open the zip file and see what information is being disclosed. If you consent to sharing this data,\
    post the zipfile at <a href="{FORUM_URL}">{FORUM_URL}</a>.\n\
    This bug report contains the following files:',
        title="Bug report saved",
        icon="info",
        traceback_text=filenames,
        print_console=False,
    )


class BugReport:
    """ A zip file named zip_filename containing project_file"""

    def __init__(self, project_filename, traceback_text, zip_filename):
        self.project_filename = project_filename
        self.project_dir = os.path.dirname(self.project_filename)
        self.traceback_text = traceback_text
        self.zip_filename = zip_filename
        self.zipf = ZipFile(self.zip_filename, mode="w")
        self.write_version()
        self.write_traceback()
        self.write_conda_env()
        if have_git:
            self.write_git_history()

        if self.project_filename is not None:
            self.write_project_file()
            self.write_patterns()

        self.zipf.close()

    def name(self):
        """ Return filename of bug report zipfile """
        return self.zipf.filename

    def zipdir(self):
        """ directory name inside the zipfile"""
        stem, _ = os.path.splitext(os.path.basename(self.zip_filename))
        return stem

    def write_project_file(self):
        """ Include mfix.dat or <RUN_NAME>.mfx """
        path = self.zipdir() + "/" + os.path.basename(self.project_filename)
        self.zipf.write(self.project_filename, arcname=path)

    def write_git_history(self):
        subprocess.run(['git', 'bundle', 'create', 'git-history', '--all'],
                       stdout=subprocess.PIPE,
                       stderr=subprocess.STDOUT,
                       check=False)
        if os.path.exists('git-history'):
            self.zipf.write(self.project_dir + "/git-history", arcname=self.zipdir()+"/git-history")

    def write_patterns(self):
        """ Include UDFs, STLs, log files, etc. """
        patterns = ["*.LOG",
                    "*.dat",
                    "*.f",
                    "*.stl",
                    "*.stl.original",
                    "CMakeCache.txt"]
        for pattern in patterns:
            for filepath in glob(os.path.join(self.project_dir, pattern)):
                if os.path.abspath(self.project_filename) != os.path.abspath(filepath):
                    path = self.zipdir() + "/" + os.path.basename(filepath)
                    self.zipf.write(filepath, arcname=path)

    def write_version(self):
        """ Include output of mfixversioninfo command """
        path = self.zipdir() + "/mfixversioninfo.txt"
        self.zipf.writestr(path, "\n".join(get_version_info()))

    def write_traceback(self):
        """ Include current sys.excepthook exception (if any) """
        if self.traceback_text is not None:
            path = self.zipdir() + "/traceback.txt"
            self.zipf.writestr(path, self.traceback_text)

    def write_conda_env(self):
        """ Include output of command "conda env export" """
        conda = shutil.which("conda")
        if conda is None:
            return
        conda_env = subprocess.run(
            [conda, "env", "export"],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
        ).stdout
        path = self.zipdir() + "/conda_env.yml"
        self.zipf.writestr(path, conda_env)


def cleanup_traceback(tback, err_str):
    """ Clean up the traceback a little. """

    # Remove some leading whitespace
    tb_list = [
        line[2:] if line.startswith("  ") else line
        for line in traceback.format_tb(tback)
    ]

    # Don't let the traceback be too long (e.g. "recursion too deep")
    tb_list = tb_list[-20:]

    # Shorten long pathnames
    tb_list = [line.replace(SCRIPT_DIRECTORY, "...") for line in tb_list]

    def fix_html(txt):
        return txt.replace('<', '&lt;').replace('>','&gt;')
    tb_list = [fix_html(line) for line in tb_list]

    err_str = fix_html(err_str)
    details = [err_str] + tb_list

    return "".join(details)
