#!/usr/bin/env python
""" Build mfixsolver as a Python extension library
"""

import argparse
import platform
import shutil
import subprocess
import sys
import os

from contextlib import contextmanager

from mfixgui.tools import get_mfix_src
from mfixgui.version import __version__


def main():
    """ build_mfixsolver command: build a custom mfixsolver from the command line """
    try:
        main_args(sys.argv[1:])
    except subprocess.CalledProcessError as err:
        print(
            74*"=",
            "                     BUILD FAILED",
            sep=os.linesep,
        )
        if err.stdout:
            print("Output:")
            print(err.stdout)
        if err.stderr:
            print("Error messages:")
            print(err.stderr)
        print(74*"=")
        sys.exit(err.returncode)


def main_args(args):
    """ Parse command line args, and build target """

    config, cmake_args = parse_args(args)
    rundir = os.getcwd()
    check_rundir(rundir)

    if config.clean:
        message = do_clean(rundir)
        print(message or "Nothing to clean.")
        return

    check_for_mfx_file()

    if not shutil.which("cmake"):
        print("\ncmake not in PATH. To build MFiX, install CMake 3.4 or later.\n")
        sys.exit(-1)

    with cd_builddir(rundir):
        build_target(config.jobs, config.verbose, rundir, cmake_args)
        if not (config.batch or config.server == "none"):
            make_symlink(rundir)
    sys.stdout.flush()


@contextmanager
def cd_builddir(rundir):
    """ Run CMake and make in build/ directory """
    builddir = os.path.join(rundir, "build")
    if not os.path.exists(builddir):
        os.makedirs(builddir, exist_ok=True)
    remove_cmakecache(builddir)
    os.chdir(builddir)
    yield
    os.chdir(rundir)


def remove_cmakecache(builddir):
    """remove CMakeCache.txt between build to not confuse the user with cached
    values for compiler, compiler flags, etc"""
    cmake_cache = os.path.join(builddir, "CMakeCache.txt")
    if os.path.isfile(cmake_cache):
        os.remove(cmake_cache)


def make_symlink(rundir):
    """ Make symlink on Unix"""
    if platform.system() != "Windows":
        solver = os.path.join(rundir, "mfixsolver")
        if os.path.exists(solver):
            os.remove(solver)
        os.symlink(os.path.join(rundir, "mfixsolver.sh"), solver)


def check_for_mfx_file():
    """ "Guarantee that there is one and only one project file in the run dir"""
    # See issues/1216

    mfx_files = [f for f in os.listdir('.')
                 if f.lower().endswith('.mfx')
                 or f.lower() == 'mfix.dat']
    if len(mfx_files) > 1:
        print("ERROR, multiple .mfx files found:")
        for mfx_file in mfx_files:
            print(mfx_file)
        sys.exit(-1)
    elif len(mfx_files) == 1:
        print("Building custom solver for %s" % (mfx_files[0]))
    else:
        print("Building generic solver")
    sys.stdout.flush()


def check_rundir(rundir):
    """Check for problem characters in the builddir path """
    ok = True
    for c in '&=':
        if c in rundir:
            print("Unable to build solver because the build path:")
            print(rundir)
            print("contains the character '%s'. Rename your working directory."%c)
            ok = False
    if not ok:
        sys.exit(-1)


def parse_args(cmdline_args):

    parser = argparse.ArgumentParser(
        description="Wrapper script to build MFiX Solver for running CMake and GNU Make"
    )
    parser.add_argument("-v", "--version", action="version", version=__version__)
    parser.add_argument(
        "--clean",
        action="store_true",
        help="Clean build artifacts (instead of building solver)",
    )
    parser.add_argument(
        "--postmfix", action="store_true", help="Build postmfix (instead of the solver)"
    )
    parser.add_argument(
        "--dmp",
        "--enable-dmp",
        action="store_true",
        help="Build with DMP (MPI) support",
    )
    parser.add_argument(
        "--smp",
        "--enable-smp",
        action="store_true",
        help="Build with SMP (OpenMP) support",
    )
    parser.add_argument(
        "--verbose", action="store_true", help='Build with "make VERBOSE=1"'
    )
    parser.add_argument(
        "-j",
        "--jobs",
        nargs="?",
        type=int,
        const=4,
        default=1,
        action="store",
        help="Run make with multiple parallel jobs",
    )
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "--batch",
        action="store_true",
        help="Build solver without support for GUI interaction (same as --server=none)",
    )
    group.add_argument(
        "-s",
        "--server",
        default="pymfix",
        choices=["none", "pymfix"],
        help="Support for MFiX GUI interactivity (default is --server=pymfix)",
    )

    args, cmake_args = parser.parse_known_args(cmdline_args)

    if "clean" in cmake_args:
        cmake_args.remove("clean")
        args.clean = True

    check_deprecated_args(cmake_args)

    fortran_compiler = ("-DMPI_Fortran_COMPILER=" if args.dmp
                        else "-DCMAKE_Fortran_COMPILER=")

    cmake_args = [arg.replace("FC=", fortran_compiler) if arg.startswith("FC=") else arg
        for arg in cmake_args]

    cmake_args = [arg.replace("postmfix", "-DENABLE_POSTMFIX=ON")
        if arg.startswith("postmfix")
        else arg
        for arg in cmake_args]

    cmake_args = [arg.replace("FCFLAGS=", "-DCMAKE_Fortran_FLAGS=")
        if arg.startswith("FCFLAGS=")
        else arg
        for arg in cmake_args]

    cmake_args = [arg.replace("CC=", "-DCMAKE_C_COMPILER=") if arg.startswith("CC=") else arg
        for arg in cmake_args]

    if args.batch:
        args.server = "batch"

    elif args.server == "pymfix":
        cmake_args.extend(["-DENABLE_PYMFIX=ON",
                           "-DPython3_EXECUTABLE=%s" % sys.executable,
                           "-DPython3_ROOT_DIR=%s" % sys.prefix])

    if args.dmp:
        cmake_args.append("-DENABLE_MPI=1")

    if args.smp:
        cmake_args.append("-DENABLE_OpenMP=1")

    return args, cmake_args


def generator():
    """ return -G argument for CMake """
    if platform.system() != "Windows":
        return "Unix Makefiles"

    return "MinGW Makefiles"


def generate_makefile(args, rundir):
    """ Run CMake to generate Makefile(s) """

    args.extend(
        [
            "-G",
            generator(),
            f"-DCMAKE_INSTALL_PREFIX={rundir}",
            f"-DUDF_DIR={rundir}",
            f"-DVERSION={__version__}",
            str(get_mfix_src()),
        ]
    )
    cmd = ["cmake"] + args
    print("Running cmake command:")
    print()
    print(*cmd)
    print()
    subprocess.check_call(cmd)


def do_clean(basedir):
    """ delete all build artifacts: build/ lib/ mfixsolver mfixsolver.so etc. """
    message = ""
    dirs_to_clean = [os.path.join(basedir, name) for name in ("build", ".build", "lib")
        if os.path.isdir(os.path.join(basedir, name))]

    for d in dirs_to_clean:
        message += ("Removing directory: %s\n" % d)
        shutil.rmtree(d, ignore_errors=True)

    files_to_clean = [os.path.join(basedir, name) for name in
             ("mfixsolver",
              "mfixsolver.bat",
              "mfixsolver.exe",
              "mfixsolver.sh",
              "postmfix",
              "postmfix.exe")
             if os.path.exists(os.path.join(basedir, name))]

    for f in files_to_clean:
        message += ("Removing: %s\n" % f)
        os.remove(f)

    return message


def build_target(jobs, verbose, rundir, cmake_args):
    """ Run Make """

    generate_makefile(cmake_args, rundir)

    cmd = ["cmake", "--build", ".", "--target", "install"]
    if jobs > 1:
        cmd.extend(["-j", str(jobs)])
    if verbose:
        cmd.append("--verbose")

    print("Build command:")
    print()
    print(*cmd)
    print()
    subprocess.check_call(cmd)
    print(
        74*"=",
        "                     BUILD SUCCESSFUL",
        "",
        "  To run solver from command line:  ./mfixsolver",
        "",
        "  To run solver from GUI:  Select [project]/mfixsolver in the Run dialog",
        74*"=",
        sep=os.linesep,
    )


def check_deprecated_args(cmake_args):
    """To avoid user confusion, stop if FC, CMAKE_Fortran_COMPILER, or
MPI_Fortran_COMPILER are specified on the command line."""

    fc_args = ["-DMPI_Fortran_COMPILER", "-DCMAKE_Fortran_COMPILER", "FC"]
    fc_args_used = [
        fc_arg
        for arg in cmake_args
        for fc_arg in fc_args
        if arg.startswith(f"{fc_arg}")
    ]
    if len(fc_args_used) > 1:
        print(f"Only one of {fc_args_used} can be used.")
        print()
        print()
        print("To specify a compiler and build with MPI support:")
        print()
        print("> build_mfixsolver FC=/path/to/fortran/compiler --dmp")
        print()
        print()
        print("To specify a compiler and build without MPI support:")
        print()
        print("> build_mfixsolver FC=/path/to/fortran/compiler")
        sys.exit(-1)


if __name__ == "__main__":
    main()
