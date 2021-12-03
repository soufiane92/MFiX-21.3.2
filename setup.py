# -*- coding: utf-8 -*-
from setuptools import setup

packages = \
['mfixgui',
 'mfixgui.colormaps',
 'mfixgui.console',
 'mfixgui.editor',
 'mfixgui.file_menu',
 'mfixgui.images',
 'mfixgui.output_templates',
 'mfixgui.particle_props',
 'mfixgui.solver',
 'mfixgui.tests',
 'mfixgui.tests.colormaps',
 'mfixgui.tests.editor',
 'mfixgui.tests.file_menu',
 'mfixgui.tests.solver',
 'mfixgui.tests.tools',
 'mfixgui.tests.vtk_widgets',
 'mfixgui.tools',
 'mfixgui.uifiles',
 'mfixgui.vtk_widgets',
 'mfixgui.vtk_widgets.wizards',
 'mfixgui.widgets']

package_data = \
{'': ['*']}

install_requires = \
['Splipy>=1,<2',
 'ffmpeg-python',
 'flask>=1,<2',
 'gitpython>=3,<4',
 'matplotlib>=3,<4',
 'numpy==1.19.3',
 'packaging>=20.9,<21.0',
 'psutil>=5.8.0,<5.9.0',
 'pyqt5>=5.15.0,<5.16.0',
 'pyqtgraph>=0.11,<0.12',
 'pyyaml>=5,<6',
 'qtpy>=1,<2',
 'requests>=2,<3',
 'simplejson>=3,<4',
 'vtk>=8']

entry_points = \
{'console_scripts': ['build_mfixsolver = mfixgui.build_mfixsolver:main',
                     'create_geometry = '
                     'mfixgui.vtk_widgets.geometry_engine:main',
                     'mfix = mfixgui.gui:main',
                     'mfixversioninfo = mfixgui.version_info:main']}

setup_kwargs = {
    'name': 'mfix',
    'version': '21.3.2',
    'description': 'MFiX computational fluid dynamics software',
    'long_description': None,
    'author': 'Multiflow Science Group at NETL',
    'author_email': None,
    'maintainer': None,
    'maintainer_email': None,
    'url': None,
    'packages': packages,
    'package_data': package_data,
    'install_requires': install_requires,
    'entry_points': entry_points,
    'python_requires': '>=3.8,<4.0',
}


setup(**setup_kwargs)
