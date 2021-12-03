"""Functions that return paths for MFiX project templates, documentation,
solver source code, etc."""

import platform
import sys

from os import environ
from os.path import dirname, realpath, join, isdir, isfile

# Base dir of Python 'mfixgui' package is parent directory of 'tools' where this file lives
SCRIPT_DIRECTORY = dirname(dirname(realpath(__file__)))

SOURCE_DIR = (
    dirname(SCRIPT_DIRECTORY)
    if isfile(join(dirname(SCRIPT_DIRECTORY), "pyproject.toml"))
    else None
)


def get_mfix_home():
    """ return top-level MFiX directory """
    for mfix_home in _mfix_homes():
        if isdir(mfix_home):
            return mfix_home
    raise Exception("Unable to find MFIX_HOME in %s" % list(_mfix_homes()))


def get_mfix_templates():
    """ return directory storing MFiX tutorials """
    for mfix_home in _mfix_homes():
        for templates_dir in (mfix_home, join(mfix_home, "templates")):
            tutorials_dir = join(templates_dir, "tutorials")
            if isdir(tutorials_dir):
                return templates_dir
    raise Exception("Unable to find MFiX templates in %s" % list(_mfix_homes()))


def get_mfix_doc_html():
    """ returns the directory containing html documentation """
    for mfix_home in _mfix_homes():
        html_dirs = (
            join(mfix_home, "doc", "_build", "html"),
            join(mfix_home, "doc", "html"),
        )
        for html_dir in html_dirs:
            if isdir(html_dir):
                return html_dir
    return None


def get_mfix_src():
    """ returns the directory containing solver source (model, post_mfix, build_aux) """
    for mfix_home in _mfix_homes():
        for src_dir in (join(mfix_home), join(mfix_home, "src")):
            if isdir(join(src_dir, "model")):
                return src_dir
    raise Exception("Unable to find MFiX source in %s" % list(_mfix_homes()))


def _mfix_homes():
    """find top level directory containing subdirs model, mfixgui, tutorials"""

    # check if running from source
    if SOURCE_DIR is not None:
        yield SOURCE_DIR

    # check current conda environment
    mfix_home = join(conda_prefix(), "share", "mfix")
    if isdir(mfix_home):
        yield mfix_home

    # check if installed with setuptools
    for pypath in sys.path:
        mfix_home = join(pypath, "mfix")
        if isdir(mfix_home):
            yield mfix_home

    # check if installed with pip
    for pypth in sys.path:
        names = ["model", "queue_templates", "tests", "tutorials"]
        if all(isdir(join(pypth, name)) for name in names):
            yield pypth


def conda_prefix():
    """ get current conda environment prefix directory """
    if "CONDA_PREFIX" in environ:
        return environ.get("CONDA_PREFIX")

    # check root conda environment
    python_dir = dirname(sys.executable)
    return python_dir if platform.system() == "Windows" else dirname(python_dir)
