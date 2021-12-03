import os

from os.path import dirname, join, sep
from unittest.mock import patch
from zipfile import ZipFile

import pytest

from PyQt5.QtWidgets import QMainWindow

from mfixgui.bug_report import save_bug_report, BugReport, excepthook


@pytest.fixture
def conda_env():
    with patch("mfixgui.solver.manager.subprocess.run") as run:
        run.return_value.stdout = b"output_for_conda_env_export"
        yield


@pytest.fixture
def mfix_gui(conda_env, qtbot, mocker, tmpdir):
    """ mock MfixGui object """
    qmain = QMainWindow()
    qtbot.addWidget(qmain)
    gui = mocker.Mock()
    gui.message_box.isVisible.return_value = False
    gui.get_project_file.return_value = None
    with patch("mfixgui.bug_report.getcwd") as cwd:
        cwd.return_value = str(tmpdir)
        with patch("mfixgui.bug_report.get_version_info") as get_version_info:
            get_version_info.return_value = ["a", "b", "c"]
            yield gui


@pytest.fixture
def proj_mfx(tmpdir):
    """ Create currently open project file (for certain tests) """
    proj = join(tmpdir, "myproj.mfx")
    with open(proj, "w") as myproj:
        myproj.write('RUN_NAME = "MY NAME"')
    return proj


@pytest.fixture
def proj_dat(tmpdir):
    """ Create currently open project file (for certain tests) """
    proj = join(tmpdir, "mfix.dat")
    with open(proj, "w") as myproj:
        myproj.write('RUN_NAME = "MY NAME"')
    return proj


@pytest.fixture
def test_tb():
    """ Create a Traceback object for testing """
    try:
        raise RuntimeError()
    except RuntimeError as ex:
        return ex.__traceback__


@pytest.mark.parametrize("devmode", [True, False])
class TestExcepthook:
    """ test mfixgui.bug_report.excepthook """

    def test_no_gui(self, devmode, test_tb):
        """ should work before GUI is constructed """
        exc = Exception()

        excepthook(None, devmode, 123, exc, test_tb)

    def test_message_already_visible(self, conda_env, devmode, mfix_gui, test_tb):
        """ should avoid cascading error dialogs """
        mfix_gui.message_box.isVisible.return_value = True
        mfix_gui.message.return_value = None
        exc = Exception()

        excepthook(mfix_gui, devmode, 123, exc, test_tb)

    def test_cancel_message(self, devmode, mfix_gui, test_tb):
        """ should work when user cancels error dialog """
        mfix_gui.message.return_value = None
        exc = Exception()

        excepthook(mfix_gui, devmode, 123, exc, test_tb)

    @patch("mfixgui.bug_report.datetime")
    def test_no_project_save(self, datetime, devmode, mfix_gui, tmpdir, test_tb):
        """ should work when user saves bug report without a project open """
        mfix_gui.message.return_value = "ok"
        datetime.now.return_value.isoformat.return_value = "2000-11-22T12:34:56"

        exc = Exception()
        excepthook(mfix_gui, devmode, 123, exc, test_tb)

        zipfile = ZipFile(join(tmpdir, "mfix_2000-11-22T123456.zip"))
        tb_txt = zipfile.read("mfix_2000-11-22T123456/traceback.txt")
        assert tb_txt.startswith(
            winpath('Error: \nFile ".../tests/test_bug_report.py", line').encode(
                "utf-8"
            )
        )
        assert tb_txt.endswith(b"raise RuntimeError()\n")

    @patch("mfixgui.bug_report.datetime")
    def test_project_save(self, datetime, devmode, mfix_gui, proj_mfx, test_tb):
        """ should work when user save bugreport with project open """
        mfix_gui.message.return_value = "ok"
        mfix_gui.get_project_file.return_value = proj_mfx
        mfix_gui.project.get_value.return_value = "MYPROJ"
        datetime.now.return_value.isoformat.return_value = "2000-11-22T12:34:56"

        exc = Exception()
        excepthook(mfix_gui, devmode, 123, exc, test_tb)

        my_zip = join(dirname(proj_mfx), "myproj_2000-11-22T123456.zip")
        zipfile = ZipFile(my_zip)
        tb_txt = zipfile.read("myproj_2000-11-22T123456/traceback.txt")
        assert tb_txt.startswith(
            winpath('Error: \nFile ".../tests/test_bug_report.py", line').encode(
                "utf-8"
            )
        )
        assert tb_txt.endswith(b"raise RuntimeError()\n")


def winpath(path):
    return path.replace("/", sep)


class TestSaveBugReport:
    """ test mfixgui.bug_report.save_bug_report """

    @pytest.fixture
    def savebug_mfixgui(self):
        with patch("mfixgui.bug_report.datetime") as datetime:
            datetime.now.return_value.isoformat.return_value = "2000-11-22T12:34:56"
            yield

    def test_no_project(self, savebug_mfixgui, conda_env, mfix_gui, tmpdir):
        """ should work when no project is open """

        save_bug_report(mfix_gui)

        zipfile = ZipFile(join(tmpdir, "mfix_2000-11-22T123456.zip"))
        mvi_txt = zipfile.read("mfix_2000-11-22T123456/mfixversioninfo.txt")
        assert mvi_txt == b"a\nb\nc"

    def test_no_project_no_zip_ext(self, savebug_mfixgui, mfix_gui, tmpdir):
        """ should work when no project is open and user selects filename without zip extension """

        save_bug_report(mfix_gui)

        zipfile = ZipFile(join(tmpdir, "mfix_2000-11-22T123456.zip"))
        mvi_txt = zipfile.read("mfix_2000-11-22T123456/mfixversioninfo.txt")
        assert mvi_txt == b"a\nb\nc"

    def test_no_project_cancel(self, savebug_mfixgui, mfix_gui):
        """ should work when no project is open and user cancels """

        save_bug_report(mfix_gui)

    def test_project(self, savebug_mfixgui, mfix_gui, proj_mfx):
        """ should work when project is open """
        mfix_gui.get_project_file.return_value = str(proj_mfx)
        mfix_gui.project.get_value.return_value = "MY_PROJECT"

        save_bug_report(mfix_gui)

        my_zip = join(dirname(proj_mfx), "my_project_2000-11-22T123456.zip")
        zipfile = ZipFile(my_zip)
        mvi_txt = zipfile.read("my_project_2000-11-22T123456/mfixversioninfo.txt")
        assert mvi_txt == b"a\nb\nc"


class TestBugReport:
    """ test mfixgui.bug_report.bug_report """

    @patch("mfixgui.bug_report.get_version_info")
    def test_filename_timestamp(self, get_version_info, conda_env, mocker, tmpdir):
        """ test if no project is open """
        get_version_info.return_value = ["a", "b", "c"]

        filename = BugReport(None, None, join(tmpdir, "my.zip")).name()

        assert filename.endswith("my.zip")

    @patch("mfixgui.bug_report.get_version_info")
    def test_versioninfo_no_project(self, get_version_info, conda_env, mocker, tmpdir):
        """ test if no project is open """
        get_version_info.return_value = ["a", "b", "c"]

        zipfile = ZipFile(BugReport(None, None, join(tmpdir, "my.zip")).name())

        mvi_txt = zipfile.read("my/mfixversioninfo.txt")
        assert mvi_txt == b"a\nb\nc"

    @patch("mfixgui.bug_report.get_version_info")
    def test_versioninfo(self, get_version_info, conda_env, mocker, proj_mfx):
        """ should include output of mfixversioninfo """
        get_version_info.return_value = ["a", "b", "c"]

        my_zip = join(dirname(proj_mfx), "my.zip")
        zipfile = ZipFile(BugReport(proj_mfx, None, my_zip).name())

        mvi_txt = zipfile.read("my/mfixversioninfo.txt")
        assert mvi_txt == b"a\nb\nc"

    def test_mfx(self, conda_env, proj_mfx):
        """ should include *.mfx project file """

        my_zip = join(dirname(proj_mfx), "my.zip")
        zipfile = ZipFile(BugReport(proj_mfx, None, my_zip).name())

        proj_txt = zipfile.read("my/myproj.mfx")
        assert proj_txt == b'RUN_NAME = "MY NAME"'

    def test_udf(self, conda_env, proj_mfx):
        """ should include UDFs *.f in project dir """
        usr_2f = join(dirname(proj_mfx), "usr_2.f")
        with open(usr_2f, "w") as udf:
            udf.write("subroutine usr ... end subroutine")

        my_zip = join(dirname(proj_mfx), "my.zip")
        zipfile = ZipFile(BugReport(proj_mfx, None, my_zip).name())

        usr2 = zipfile.read("my/usr_2.f")
        assert usr2 == b"subroutine usr ... end subroutine"

    def test_stl(self, conda_env, proj_mfx):
        """ should include *.stl *.stl.original in project dir """
        geom_stl = join(dirname(proj_mfx), "geom.stl")

        with open(geom_stl + ".original", "w") as geom_org:
            geom_org.write("STL file created outside of the GUI")

        with open(geom_stl, "w") as geom:
            geom.write("STL file modified by the GUI")

        my_zip = join(dirname(proj_mfx), "my.zip")
        zipfile = ZipFile(BugReport(proj_mfx, None, my_zip).name())

        stl = zipfile.read("my/geom.stl.original")
        assert stl == b"STL file created outside of the GUI"

        stl = zipfile.read("my/geom.stl")
        assert stl == b"STL file modified by the GUI"

    def test_exception(self, conda_env, proj_mfx):
        """ should include traceback.txt in project dir """

        traceback_text = """\
    Traceback (most recent call last):
    File "<stdin>", line 1, in <module>
    TypeError: 'NoneType' object is not subscriptable"""

        my_zip = join(dirname(proj_mfx), "my.zip")
        zipfile = ZipFile(BugReport(proj_mfx, traceback_text, my_zip).name())

        tb_txt = zipfile.read("my/traceback.txt")
        assert (
            tb_txt
            == b"""\
    Traceback (most recent call last):
    File "<stdin>", line 1, in <module>
    TypeError: 'NoneType' object is not subscriptable"""
        )

    @pytest.mark.xfail(reason="FIXME")
    def test_mfix_dat(self, conda_env, proj_dat):
        """ should include *.mfx project file """

        my_zip = join(dirname(proj_dat), "my.zip")
        zipfile = ZipFile(BugReport(proj_dat, None, my_zip).name())

        assert sorted(
            ["my/conda_env.yml", "my/mfixversioninfo.txt", "my/mfix.dat"]
        ) == sorted(zipfile.namelist())
