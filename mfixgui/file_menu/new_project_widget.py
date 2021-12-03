from glob import glob

from collections import OrderedDict
from os.path import basename, dirname, join, split, relpath

from PyQt5.QtWidgets import QListWidgetItem, QWidget, QCheckBox
from PyQt5.QtCore import Qt

from mfixgui.tools import find_project_file, get_mfix_templates
from mfixgui.tools.collect_tutorial_info import get_template_info
from mfixgui.tools.qt import (get_ui, get_thumbnail, view_mode,
                              widget_iter, SETTINGS)


class NewProjectWidget(QWidget):
    def __init__(self, gui):
        super(NewProjectWidget, self).__init__()
        get_ui("new_project.ui", widget=self)

        self.gui = gui
        self.populate_templates()

        self.template_list.itemDoubleClicked.connect(self.create_new_project)
        self.template_list.setAttribute(Qt.WA_MacShowFocusRect, 0)
        self.template_list.setFrameStyle(self.template_list.NoFrame)
        self.search_bar.textChanged.connect(self.do_filter)
        self.list_button.clicked.connect(self.switch_to_list_mode)
        self.tile_button.clicked.connect(self.switch_to_tile_mode)

        self.solver_cbs = {
            "single": self.checkbox_single,
            "tfm": self.checkbox_tfm,
            "dem": self.checkbox_dem,
            "cgp": self.checkbox_cgp,
            "pic": self.checkbox_pic,
        }
        for item in widget_iter(self):
            if isinstance(item, QCheckBox):
                item.toggled.connect(self.do_filter)

        self.setObjectName("new")

        is_tile_mode = SETTINGS.value("open_list_mode", "tile") in ("icon", "tile")
        if is_tile_mode:
            self.switch_to_tile_mode()
        else:
            self.switch_to_list_mode()

    def create_new_project(self, item):
        if not self.gui.check_unsaved_abort():
            project_file = find_project_file(item.full_path)
            self.gui.open_new_from_template(project_file)

    def switch_to_tile_mode(self):
        SETTINGS.setValue("open_list_mode", "icon")
        self.list_button.setChecked(False)
        self.template_list.setViewMode(view_mode(True))
        self.tile_button.setChecked(True)

    def switch_to_list_mode(self):
        SETTINGS.setValue("open_list_mode", "")
        self.list_button.setChecked(True)
        self.template_list.setViewMode(view_mode(False))
        self.tile_button.setChecked(False)

    def matches_filter(self, item):
        if item.name == 'blank': # Always show blank template
            return True

        text = item.text().lower()

        cb = self.solver_cbs.get(item.solver)
        if cb is None:
            raise ValueError("No checkbox for solver %s" % item.solver)

        if not cb.isChecked():
            return False

        if not (self.checkbox_reactions.isChecked() and item.chem
                or self.checkbox_no_reactions.isChecked() and not item.chem):
            return False

        if not (self.checkbox_cut_cell.isChecked() and item.cut_cell
                or self.checkbox_no_cut_cell.isChecked() and not item.cut_cell):
            return False

        if (item.template_dir=='tests' and not self.checkbox_tests.isChecked()
            or item.template_dir=='tutorials' and not self.checkbox_tutorials.isChecked()):
            return False

        search_term = self.search_bar.text().lower().strip()
        if search_term and search_term not in text:
            return False

        return True



    def populate_templates(self):
        """ create templates on new tab """
        templates = collect_templates()
        temp_info = get_template_info()

        tuples = [
            (basename(path), path, template_dir)
            for template_dir, paths in templates.items()
            for path in paths
        ]
        tuples.sort(key = lambda tuple: (tuple[2].startswith('tests'),
                                         tuple[0]))
        blank_idx = None
        for i, (name, path, template_dir) in enumerate(tuples):
            item = make_listwidget_item(path, template_dir, temp_info)
            self.template_list.addItem(item)
            item.setHidden(template_dir in ["tests"])
            if name.strip() == 'blank':
                blank_idx = i
        if blank_idx is not None:
            item = self.template_list.takeItem(blank_idx)
            self.template_list.insertItem(0, item)

    def do_filter(self):
        for index in range(self.template_list.count()):
            item = self.template_list.item(index)
            item.setHidden(not self.matches_filter(item))


def collect_templates():
    """ look for template files """
    template_dict = OrderedDict()
    for template_category in ("tutorials", "tests"):
        template_dir = join(get_mfix_templates(), template_category)
        template_list = sorted(
            (
                relpath(dirname(mfx), template_dir)
                for mfxs in (
                    glob(join(template_dir, "**", "*.mfx"), recursive=True),
                    glob(join(template_dir, "**", "mfix.dat"), recursive=True),
                )
                for mfx in mfxs
            )
        )
        template_dict[template_category] = template_list
    return template_dict


def make_listwidget_item(path, template_dir, temp_info):
    name = basename(path)
    full_path = join(get_mfix_templates(), template_dir, path)
    # extract info from the template info file

    key = "/".join([template_dir] + list(split(path))).lower()
    info = temp_info.get(key, {})
    description = info.get("description", " " * 50)
    label = "\n".join([name, description])
    item = QListWidgetItem(get_thumbnail(full_path), label)
    item.full_path = full_path
    item.solver = info.get("solver", "single")
    item.cut_cell = info.get("cutcell", False)
    item.chem = info.get("chemistry", False)
    item.name = name
    item.template_dir = template_dir

    return item
