""" Module for parsing Queue Config Template files """

import configparser

from glob import glob
from io import StringIO
from os.path import exists, join

from qtpy.QtWidgets import (QCheckBox, QDoubleSpinBox, QSpinBox, QListWidget,
    QWidget, QGridLayout, QLabel, QComboBox)

from mfixgui.tools import extract_config, get_mfix_templates
from mfixgui.tools.qt import SETTINGS
from mfixgui.widgets.base import BASE_WIDGETS


def init_template_manager(template_manager):
    """ Initialize template manager with builtin templates and saved templates from settings """

    queue_template_dir = join(get_mfix_templates(), "queue_templates")
    for path in glob(join(queue_template_dir, "**/*"), recursive=True):
        builtin_template = QueueTemplate(path, True)
        template_manager.add(builtin_template)

    for settings_path in _paths_from_settings():
        template = QueueTemplate(settings_path)
        template_manager.add(template)


class QueueTemplateManager:
    """ Represent the set of templates available """

    def __init__(self, templates_updated):
        self.templates = {}
        self.templates_updated = templates_updated
        self.templates_updated.emit()

    def __getitem__(self, key):
        return self.templates[key]

    def template_keys(self):
        """ Return the combobox display names of all templates """
        return list(self.templates.keys())

    def remove(self, key):
        """ Remove template (if not builtin) """
        if not self.templates[key].is_builtin:
            del self.templates[key]
        self.templates_updated.emit()

    def add(self, template):
        """ Add a new template (if path is not already present)"""
        if template.path not in [t.path for t in self.templates.values()]:
            self.templates[template.display_name()] = template
        self.templates_updated.emit()

    def save_settings(self):
        SETTINGS.setValue(
            "queue_templates",
            "|".join(
                [str(template.path) for template in list(self.templates.values())]
            ),
        )


class QueueTemplate:
    """ Represents a builtin or custom template """

    def __init__(self, path, is_builtin=False):

        self.user_values = {}
        self.widget = QWidget()
        self.layout = QGridLayout(self.widget)

        config, script = extract_config(path)
        config_parser = configparser.ConfigParser()
        try:
            config_parser.read_file(StringIO(config))
        except configparser.MissingSectionHeaderError as err:
            raise ValueError(err)

        self._dict = dict(
            [(s, dict(config_parser.items(s))) for s in config_parser.sections()]
        )
        self._dict["path"] = str(path)
        self._dict["script"] = script

        self.name = str(path)
        self.path = path
        self.is_builtin = is_builtin
        if "options" not in self._dict:
            raise ValueError("Invalid template contents; missing [options] section")
        opt = self._dict["options"]
        name = opt.get("name", None)
        if name and name not in self:
            self.name = name
        if not self.submit():
            raise ValueError(
                "The template file at: {}\n"
                "does not have a submit_cmd defined".format(self.path)
            )

        self.make_widgets()

    def init_values(self, template_values):
        for key, value in template_values.items():
            widget = self._dict.get(key, {}).get("widget_obj")
            if widget is not None:
                widget.updateValue(None, value)

    def make_widgets(self):
        for i, (fieldname, template_field) in enumerate(self._dict.items()):
            if isinstance(template_field, dict) and fieldname != "options":
                label = QLabel(template_field.get("label", fieldname), self.widget)
                widget = _make_widget(template_field, self.widget)
                self.layout.addWidget(label, i, 0)
                self.layout.addWidget(widget, i, 1)
                template_field["widget_obj"] = widget

    def get_script(self):
        return self._dict["script"]

    def submit(self):
        return self._dict["options"].get("submit", False)

    def delete(self):
        return self._dict["options"].get("delete", False)

    def status(self):
        return self._dict["options"].get("status", False)

    def job_id_regex(self):
        return self._dict["options"].get("job_id_regex", None)

    def display_name(self):
        """ String displayed in Run dialog combobox """
        if self.is_builtin:
            return self.name
        return f"{self.name} - {self.path}"

    def template_values(self):
        """ dict used for substitution in the template file """
        return {
            key: " ".join(value) if isinstance(value, list) else value
            for key, value in self.widget_values().items()
        }

    def widget_values(self):
        """ dict storing widget values in JSON in settings """
        replace_dict = {}
        for name, widget_info in self._dict.items():
            if not isinstance(widget_info, dict):
                continue
            widget = widget_info.get("widget_obj")
            if widget is not None:
                replace_dict[name] = get_widget_value(widget_info, widget)

        return replace_dict

    def __getitem__(self, key):
        return self._dict[key]

    def __iter__(self):
        return iter(self._dict)


def _make_widget(template_field, parent):
    widget = BASE_WIDGETS.get(
        template_field.get("widget", "lineedit"), BASE_WIDGETS["lineedit"]
    )(parent)
    items = [it.strip() for it in template_field.get("items", "").split("|")]

    v = template_field.get("value")

    if isinstance(widget, QComboBox) and items:
        widget.addItems(items)
        if v not in items:
            v = items[0]
    elif isinstance(widget, QListWidget) and items:
        widget.add_items(items)
        widget.setMaximumHeight(100)
    elif isinstance(widget, QSpinBox) and items:
        if "min_value" in template_field and "max_value" in template_field:
            widget.setRange(
                int(template_field["min_value"]), int(template_field["max_value"])
            )

    widget.updateValue("", v)
    widget.help_text = template_field.get("help", "No help available.")
    return widget


def get_widget_value(widget_info, widget):
    """ Get the value for the widget based on the type of widget """
    if isinstance(widget, (QSpinBox, QDoubleSpinBox)):
        return widget.value()

    if isinstance(widget, QCheckBox):
        return widget_info.get("true" if widget.value else "false", "")

    if isinstance(widget, QListWidget):
        return widget.value

    return widget.value


def _paths_from_settings():
    queue_templates = SETTINGS.value("queue_templates")
    if queue_templates is None:
        queue_templates = ""
    return [
        saved_path
        for saved_path in queue_templates.split("|")
        if saved_path and exists(saved_path)
    ]
