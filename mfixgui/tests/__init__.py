# -*- coding: utf-8 -*-

import sys

from unittest.mock import Mock

import mfixgui.vtk_widgets.modeler

sys.modules['vtk'] = Mock()
mfixgui.vtk_widgets.VTK_AVAILABLE = False
mfixgui.vtk_widgets.VTK_IMPORT_INFO = []
