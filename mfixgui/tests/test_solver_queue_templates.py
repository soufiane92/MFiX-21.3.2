# -*- coding: utf-8 -*-
"""
Test the Run dialog
"""

import os

from unittest.mock import patch

import pytest

import mfixgui.template_manager


@pytest.fixture
def template_manager(mocker, qtbot):
    temp_man = mfixgui.template_manager.QueueTemplateManager(mocker.Mock())
    return temp_man


@pytest.mark.parametrize("settings_queue_templates", ["", None])
def test_default_template(settings_queue_templates, template_manager):
    """ Check existence of default templates """
    with patch("mfixgui.template_manager.SETTINGS") as settings:
        settings.value.return_value = settings_queue_templates
        mfixgui.template_manager.init_template_manager(template_manager)

    assert sorted(template_manager.template_keys()) == [
        "Grid Engine (example)",
        "Joule",
    ]


TEST_TEMPLATE = """
#!/bin/bash -l
## CONFIG
# Special values
# SCRIPT - the path of this script, after replacement in the run directory
# PROJECT_NAME - name of the opened project
# JOB_ID - the job id extracted using job_id_regex
# COMMAND - the command to run mfix
# MFIX_HOME - the path to the mfix directory

[options]
name: My Wonderful Template
job_id_regex: (\d+)
status_regex: ([rqw])
submit: sbatch ${SCRIPT}
delete: scancel ${JOB_ID}
status: squeue -j ${JOB_ID}

[JOB_NAME]
widget: lineedit
label: Job Name
value: ${PROJECT_NAME}
help: The name of the job.

## END CONFIG
"""


@patch("mfixgui.template_manager.extract_config")
def test_add_template(extract_config, template_manager):
    """ Add a custom templates """
    extract_config.return_value = (TEST_TEMPLATE, "foobar")
    test_template = os.path.abspath("my/custom/template")
    template_manager.add(mfixgui.template_manager.QueueTemplate(test_template))

    assert sorted(template_manager.template_keys()) == [
        "My Wonderful Template - " + test_template
    ]


@patch("mfixgui.template_manager.extract_config")
def test_add_invalid_template(extract_config, template_manager):
    """ Check that adding a bad custom template raises exception """
    extract_config.return_value = ("not valid template text", "foobar")
    with pytest.raises(ValueError):
        template_manager.add(
            mfixgui.template_manager.QueueTemplate("my/custom/template")
        )

    assert sorted(template_manager.template_keys()) == []


@patch("mfixgui.template_manager.extract_config")
def test_add_valid_but_incomplete_template(extract_config, template_manager):
    """ Check that adding a bad custom template raises exception """

    almost_valid_template = "\n".join(
        ["[options]", "name: Template without a submit option"]
    )

    extract_config.return_value = (almost_valid_template, "foobar")
    with pytest.raises(ValueError):
        template_manager.add(
            mfixgui.template_manager.QueueTemplate("my/custom/template")
        )

    assert sorted(template_manager.template_keys()) == []


@patch("mfixgui.template_manager.extract_config")
def test_remove_template(extract_config, template_manager):
    """ Remove a custom template """
    extract_config.return_value = (TEST_TEMPLATE, "foobar")
    test_template = os.path.abspath("my/custom/template")
    template_manager.add(mfixgui.template_manager.QueueTemplate(test_template))

    template_manager.remove("My Wonderful Template - " + test_template)

    assert sorted(template_manager.template_keys()) == []
