import os

from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import QListWidgetItem, QWidget

from mfixgui.tools.qt import get_ui, get_thumbnail, view_mode, SETTINGS


class OpenWidget(QWidget):
    def __init__(self, mfixgui):
        """ Create widget for Open filemenu pane """
        super(OpenWidget, self).__init__()
        get_ui("open.ui", widget=self)
        self.gui = mfixgui

        is_tile_mode = SETTINGS.value("open_list_mode", "tile") in (
            "icon",  # compat
            "tile",
        )

        self.browse_button.clicked.connect(mfixgui.handle_open)
        self.list_button.clicked.connect(self.switch_to_list_mode)
        self.tile_button.clicked.connect(self.switch_to_tile_mode)
        self.template_list.itemDoubleClicked.connect(self.open_project)
        self.template_list.setAttribute(Qt.WA_MacShowFocusRect, 0)
        self.template_list.setFrameStyle(self.template_list.NoFrame)
        self.template_list.setViewMode(view_mode(is_tile_mode))
        self.clear_recent_btn.pressed.connect(self.clear_recent)

        self.populate_recent_projects()
        self.switch_to_tile_mode()

    def switch_to_list_mode(self):
        SETTINGS.setValue("open_list_mode", "list")
        self.list_button.setChecked(True)
        self.template_list.setViewMode(view_mode(False))
        self.tile_button.setChecked(False)

    def switch_to_tile_mode(self):
        SETTINGS.setValue("open_list_mode", "tile")
        self.list_button.setChecked(False)
        self.template_list.setViewMode(view_mode(True))
        self.tile_button.setChecked(True)

    def clear_recent(self):
        SETTINGS.setValue("recent_projects", "|".join([]))
        self.template_list.clear()

    def open_project(self, item):
        """Open the project of the selected item"""
        if not item:
            return

        if self.gui.check_unsaved_abort():
            return

        project_path = item.full_path
        if os.path.exists(project_path):
            self.gui.open_project(project_path)
        else:
            self.gui.message(text="File does not exist: %s" % project_path)

    def populate_recent_projects(self):
        recent_projects = SETTINGS.value("recent_projects", "")
        if not recent_projects:
            return
        projects = recent_projects.split("|")

        self.template_list.clear()
        for project in projects:
            if not os.path.exists(project):
                continue
            item = make_project_list_item(project)
            self.template_list.addItem(item)

        self.template_list.verticalScrollBar().setValue(0)


def make_project_list_item(project):
    name = os.path.basename(project)
    dir_ = os.path.dirname(project)
    icon = get_thumbnail(dir_)

    (name, _) = os.path.splitext(name)
    description = read_project_description(project)
    text = "\n".join([name, description, project])

    item = QListWidgetItem(icon, text)
    item.full_path = project
    return item


def read_project_description(project):
    """ read description from project file """
    with open(project, encoding="utf-8", errors="replace") as project_file:
        for line in project_file:
            line = line.lstrip()
            if line.lower().startswith('description'):
                tok = line.split('=', 1)
                if len(tok) == 2:
                    desc = tok[1]
                    desc = desc.replace('"', '').replace("'", "")
                    desc = desc.strip()
                    return desc
    return ""
