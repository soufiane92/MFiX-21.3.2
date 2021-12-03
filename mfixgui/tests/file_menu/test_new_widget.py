from collections import OrderedDict
from os.path import join

import pytest

from PyQt5.QtWidgets import QSizePolicy

from mfixgui.file_menu.new_project_widget import (
    NewProjectWidget,
    get_thumbnail,
    collect_templates,
)

import mfixgui.file_menu.new_project_widget


@pytest.mark.parametrize("tile_mode", ["icon", "list", None])
def test_make_new_project_widget(tile_mode, mocker, qtbot):
    gui = mocker.Mock()
    NewProjectWidget(gui)


def test_get_icon(qtbot):
    get_thumbnail("foo")
    get_thumbnail("123")
