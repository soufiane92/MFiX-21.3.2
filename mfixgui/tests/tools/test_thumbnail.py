# -*- coding: utf-8 -*-
"""
run with pytest
"""


from mfixgui.tools.thumbnail import create_thumbnail


def test_thumbnail():
    create_thumbnail(".thumbnail", "dem", True, True, "setup.png")
