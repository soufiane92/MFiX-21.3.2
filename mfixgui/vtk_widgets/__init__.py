import logging
import traceback
LOG = logging.getLogger(__name__)

VTK_AVAILABLE = True
VTK_IMPORT_INFO = []
try:
    import vtk
    VTK_VERSION_STRING = vtk.vtkVersion.GetVTKVersion()
    LOG.info('VTK version: %s', VTK_VERSION_STRING)
    VTK_IMPORT_INFO.append('Successfuly imported vtk version: %s' % VTK_VERSION_STRING)
    if hasattr(vtk, 'VTK_MAJOR_VERSION'):
        VTK_MAJOR_VERSION = vtk.VTK_MAJOR_VERSION
    else: # vtk 9 and newer
        VTK_MAJOR_VERSION = vtk.vtkVersion.GetVTKMajorVersion()
except ImportError:
    VTK_AVAILABLE = False
    vtk = None
    VTK_MAJOR_VERSION = None
    ex = traceback.format_exc()
    LOG.info("can't import vtk:\n%s", ex)
    VTK_IMPORT_INFO.append('Could not import VTK, please check your installation.\n'
                           'traceback:\n{}'.format(ex))
