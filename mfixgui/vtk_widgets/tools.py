'''
Tools specific to VTK
'''

import logging
from xml.etree import ElementTree
import vtk
import os
import numpy as np
# import inspect

from mfixgui.project import Equation
from mfixgui.vtk_widgets.constants import DEFAULT_PARAMS

GUI = None
LOG = logging.getLogger(__name__)


def parse_pvd_file(fname):
    '''given a pvd file, return a dict of time (float) : file_name'''
    f_dict = {}
    if not os.path.exists(fname): return f_dict
    try:
        tree = ElementTree.parse(fname)
    except:
        return f_dict
    root = tree.getroot()
    for data_set in root.iter('DataSet'):
        f_dict[float(data_set.attrib['timestep'])] = data_set.attrib['file']

    return f_dict


def update_combo(cb, items):
    cur = cb.currentText()
    cb.clear()
    cb.addItems(items)
    return safe_combo(cb, cur)


def safe_combo(cb, val, d=None):
    index = cb.findText(val)
    if index < 0:
        if d is not None:
            cb.setCurrentText(d)
        else:
            cb.setCurrentIndex(0)
    else:
        cb.setCurrentText(val)
    return cb.currentText()


def safe_float(value, d=0.0):
    """try to convert the value to a float, if ValueError, send error to gui
    and return 0"""
    # Note this is slightly different from the function in tools, which does
    #  not have the reporting
    try:
        return float(value)
    except ValueError as error:
        # find out who is calling
        # curframe = inspect.currentframe()
        # calframe = inspect.getouterframes(curframe, 2)
        # print('safe_float', 'caller:', calframe[1][3])
        if GUI:
            GUI.error(str(error))
        else:
            LOG.error(str(error))
        return d


def safe_int(value, d=0):
    """try to convert the value to a int, if ValueError, send error to gui
    and return 0"""
    try:
        return int(value)
    except ValueError as error:
        if GUI:
            GUI.error(str(error))
        else:
            LOG.error(error)
        return 0


def clean_geo_dict(dirty_dict):
    """clean the geometry dictionary so it can be saved"""
    clean_dict = {}
    for geo, geo_dict in dirty_dict.items():
        new_dict = clean_dict[geo] = {}
        geo_type = None
        if 'geo_type' in geo_dict:
            geo_type = geo_dict['geo_type']
        new_dict['geo_type'] = geo_type
        for key, value in geo_dict.items():
            # filter out vtk objects/default values
            if not isinstance(value, vtk.vtkObject) and (isinstance(value, Equation) or value != DEFAULT_PARAMS[geo_type].get(key, None)):
                new_dict[key] = value

    return clean_dict


def remove_vtk_objects(dirty_dict):
    """given a dictionary, remove vtkObjects from it"""
    clean_dict = {}
    for key, value in dirty_dict.items():
        if not isinstance(value, vtk.vtkObject):
            clean_dict[key] = value
    return clean_dict


def clean_visual_dict(dirty_dict):
    """remove qcolor objects from visual dict and save hex values"""
    clean_dict = {}
    for geo, geo_dict in dirty_dict.items():
        clean_geo = clean_dict[geo] = {}
        for key, _ in geo_dict.items():
            if key in ['color', 'edge']:
                clean_geo[key] = geo_dict[key].name()
            else:
                clean_geo[key] = geo_dict[key]

    return clean_dict


def is_stl_ascii(fname):
    """see if the stl file is ASCII by checking the first line for solid"""
    with open(fname, 'rb') as stlFile:
        try:
            solid = stlFile.readline().strip().lower().startswith(b'solid')
            # verify by checking the second line, issues/500
            if solid:
                solid = stlFile.readline().strip().lower().startswith(b'facet normal')
        except:
            solid = False
    return solid


def purge_multi_solids(fname):
    """Remove multiple solids from an stl file, only works with ascii"""
    # if it already ends in .onesolid.stl, assume cleaned
    if fname.endswith('.onesolid.stl'):
        return fname
    name = os.path.splitext(os.path.basename(fname))[0]
    fname_dir = os.path.dirname(fname)
    newfile = os.path.join(fname_dir, name + '.onesolid.stl')
    multi_solid = 0
    with open(fname, 'r', encoding='utf-8', errors='replace') as input:
        with open(newfile, 'w', encoding='utf-8', errors='replace') as output:
            output.write('solid ascii\n')
            for line in input:
                if 'solid' not in line:
                    output.write(line)
                else:
                    multi_solid += 1
            output.write('endsolid\n')
    if multi_solid > 2:
        LOG.warn('the stl file: %s has multiple solids, removing', fname)
        return newfile
    else:
        os.remove(newfile)
        return fname


def unit_vector(vector):
    """Returns the unit vector of the vector."""
    return vector / np.linalg.norm(vector)


def angle_between(v1, v2):
    """Returns the angle in radians between vectors 'v1' and 'v2'"""
    v1_u = unit_vector(v1)
    v2_u = unit_vector(v2)
    return np.arccos(np.clip(np.dot(v1_u, v2_u), -1.0, 1.0))
