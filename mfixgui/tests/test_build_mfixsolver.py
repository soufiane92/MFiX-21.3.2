# -*- coding: utf-8 -*-
"""
run with pytest
"""

import sys
import platform
import shutil

import pytest

from mfixgui.build_mfixsolver import generator, main_args
from mfixgui.version import __version__

GEN = ["-G", generator()]

PYMFIX = [
    "-DENABLE_PYMFIX=ON",
    f"-DPython3_EXECUTABLE={sys.executable}",
    f"-DPython3_ROOT_DIR={sys.prefix}",
]


def test_check_mfxs(mocker):
    glob = mocker.patch("mfixgui.build_mfixsolver.glob.glob")
    glob.return_value = ["one.mfx", "two.mfx"]
    which = mocker.patch("mfixgui.build_mfixsolver.shutil.which")
    which.return_value = True
    args = []
    with pytest.raises(SystemExit):
        main_args(args)


def test_clean():
    args = ["clean"]
    main_args(args)


def setup_test(mocker):
    mocker.patch("mfixgui.build_mfixsolver.chdir")
    mocker.patch("mfixgui.build_mfixsolver.symlink")
    mocker.patch("mfixgui.build_mfixsolver.makedirs")
    which = mocker.patch("mfixgui.build_mfixsolver.shutil.which")
    which.return_value = True
    check_call = mocker.patch("mfixgui.build_mfixsolver.subprocess.check_call")
    get_mfix_src = mocker.patch("mfixgui.build_mfixsolver.get_mfix_src")
    get_mfix_src.return_value = "MFIX_HOME"

    run_dir = "/RUNDIR"
    cwd = mocker.patch("mfixgui.build_mfixsolver.getcwd")
    cwd.return_value = run_dir

    extra_args = [
        f"-DCMAKE_INSTALL_PREFIX={run_dir}",
        f"-DUDF_DIR={run_dir}",
        f"-DVERSION={__version__}",
        "MFIX_HOME",
    ]

    return extra_args, check_call


def test_postmfix(mocker):
    extra_args, check_call = setup_test(mocker)

    args = ["postmfix"]
    main_args(args)

    check_call.assert_has_calls(
        [
            mocker.call(["cmake", "-DENABLE_POSTMFIX=ON"] + PYMFIX + GEN + extra_args),
            mocker.call(["cmake", "--build", ".", "--target", "install"]),
        ]
    )


def test_batch(mocker):
    extra_args, check_call = setup_test(mocker)

    args = ["--batch", '-DCMAKE_foo="bar baz"']
    main_args(args)

    check_call.assert_has_calls(
        [
            mocker.call(["cmake", '-DCMAKE_foo="bar baz"'] + GEN + extra_args),
            mocker.call(["cmake", "--build", ".", "--target", "install"]),
        ]
    )


def test_interactive(mocker):
    extra_args, check_call = setup_test(mocker)

    args = ['-DCMAKE_foo="bar baz"', "-j"]
    main_args(args)

    check_call.assert_has_calls(
        [
            mocker.call(["cmake", '-DCMAKE_foo="bar baz"'] + PYMFIX + GEN + extra_args),
            mocker.call(["cmake", "--build", ".", "--target", "install", "-j", "4"]),
        ]
    )


def test_interactive_smp(mocker):
    extra_args, check_call = setup_test(mocker)

    args = ["--smp"]
    main_args(args)

    check_call.assert_has_calls(
        [
            mocker.call(["cmake"] + PYMFIX + ["-DENABLE_OpenMP=1"] + GEN + extra_args),
            mocker.call(["cmake", "--build", ".", "--target", "install"]),
        ]
    )


def test_interactive_dmp(mocker):
    extra_args, check_call = setup_test(mocker)

    args = ["--dmp"]
    main_args(args)

    check_call.assert_has_calls(
        [
            mocker.call(["cmake"] + PYMFIX + ["-DENABLE_MPI=1"] + GEN + extra_args),
            mocker.call(["cmake", "--build", ".", "--target", "install"]),
        ]
    )
