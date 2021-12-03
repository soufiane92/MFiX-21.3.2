import copy
import logging
import os
import shutil
from itertools import chain

from typing import Mapping

import numpy as np
import vtk

from mfixgui.project import Project, Equation, ExtendedJSON
from mfixgui.tools import topological_sort, get_unique_string
from mfixgui.vtk_widgets.tools import (
    safe_int,
    safe_float,
    clean_geo_dict,
    angle_between,
    is_stl_ascii,
    purge_multi_solids,
)
from mfixgui.vtk_widgets.constants import (
    DEFAULT_PARAMS,
    PARAMETRIC_DICT,
    DEFAULT_STL_PARAMS,
    PRIMITIVE_DICT,
    PROCEDURAL_DICT,
    DEFAULT_PROCEDURAL_PARAMS,
    IMPLICIT_DICT,
    IMPLICIT_DEFAULT_RES,
    DEFAULT_PRIMITIVE_PARAMS,
    DEFAULT_IMPLICIT_PARAMS,
    FILTER_DICT,
    DEFAULT_BOOLEAN_PARAMS,
    DEFAULT_PARAMETRIC_PARAMS,
    DEFAULT_FILTER_PARAMS,
)
from mfixgui.constants import PARAMETER_DICT
from mfixgui.tools import procedural_stl


def dict_iter(d):
    """iterator to recursively iterate over dict of dicts"""
    for key, val in d.items():
        if isinstance(val, dict):
            for (key2, dep) in dict_iter(val):
                yield key2, dep
        yield key, list(val.keys())


class GeometryEngine:
    """Geometry engine for creating and saving objects"""

    def __init__(self, gui=None, proj=None):

        self.gui = gui
        self.project = proj
        self.geometrydict = {}
        self.geometrytree = {}
        self.region_dict = {}
        self.parameter_key_map = {}

    def add_to_tree(self, name, icon=None, children=()):

        new = self.geometrytree.setdefault(name, {})

        for child in children:
            new[child] = self.geometrytree.pop(child)

    def remove_geometry(self, name):
        """remove geometry, put children back in top level"""
        children = self.geometrytree.pop(name)

        for child in children:
            self.geometrytree[child] = children[child]

    def geometry_to_str(self):
        """convert geometry to string"""

        # construct dependency list
        dep_tree = {}
        for key, dep in dict_iter(self.geometrytree):
            dep_tree[key] = dep

        # save tree
        data = {'geometry_dict': clean_geo_dict(self.geometrydict),
                'tree': dep_tree}
        return ExtendedJSON.dumps(data)

    def geometry_from_str(self, string, proj_dir=None):
        """convert string to geometry"""
        if len(string) < 2:
            # empty, need at least '{}'

            return
        try:
            data = ExtendedJSON.loads(string)
        except:
            # bad string, give up
            if self.gui:
                self.gui.message(text='Failed to decode geometry sting')
            return
        tree = data.get('tree')
        tree_copy = copy.deepcopy(tree)
        geo_dict = data.get('geometry_dict')
        if not tree or not data:
            return

        # convert lists to sets
        for key, value in tree.items():
            tree[key] = set(tree[key])

        # build geometry
        for nodes in topological_sort(tree):
            for node in nodes:
                if node in geo_dict and 'geo_type' in geo_dict[node]:
                    geo_data = copy.deepcopy(DEFAULT_PARAMS[geo_dict[node]['geo_type']])
                    geo_data.update(geo_dict[node])
                    geo_type = geo_dict[node]['geo_type']
                    if geo_type == 'primitive':
                        name = self.add_primitive(name=node, data=geo_data,
                                                  loading=True)
                    elif geo_type == 'procedural':
                        name = self.add_procedural(name=node, data=geo_data,
                                                   loading=True)
                    elif geo_type == 'implicit':
                        name = self.add_implicit(name=node, data=geo_data,
                                                 loading=True)
                    elif geo_type == 'parametric':
                        name = self.add_parametric(name=node, data=geo_data,
                                                   loading=True)
                    elif geo_type == 'filter':
                        name = self.add_filter(
                                name=node, data=geo_data,
                                child=tree[node].pop(), loading=True)
                    elif geo_type == 'boolean' or geo_type == 'boolean_implicit':
                        name = self.boolean_operation(
                                boolname=node, data=geo_data,
                                children=sorted(tree[node],
                                                key=tree_copy[node].index),
                                loading=True)
                    elif geo_type == 'stl':
                        name = self.add_stl(filename=geo_data['filename'],
                                            name=node, data=geo_data,
                                            loading=True, proj_dir=proj_dir)

                    if name is None and self.gui:
                        self.gui.message(text='Error loading geometry: {}'.format(node))
                        return

                    # update parameter mapping
                    for key, value in geo_data.items():
                        self.update_parameter_map(value, node, key,
                                                  check_old=False)

                    if hasattr(self, 'change_item_visibility') and not geo_data['visible']:
                        self.change_item_visibility(name, visible=False)
                else:
                    if self.gui:
                        self.gui.message(text='Error loading geometry: The geometry "{}" does not have parameters.'.format(node))
                    return

    # --- parameters ---
    def update_parameter_map(self, new_value, name, key, check_old=True):
        """update the mapping of parameters and keywords"""
        data = self.geometrydict
        name_key = ','.join([name, key])

        # new params
        new_params = []
        if isinstance(new_value, Equation):
            new_params = new_value.get_used_parameters()

        # old params
        old_params = []
        if check_old:
            old_value = data[name][key]
            if isinstance(old_value, Equation):
                old_params = old_value.get_used_parameters()

        add = set(new_params)-set(old_params)
        for param in add:
            if param not in self.parameter_key_map:
                self.parameter_key_map[param] = set()
            self.parameter_key_map[param].add(name_key)

        remove = set(old_params)-set(new_params)
        for param in remove:
            self.parameter_key_map[param].remove(name_key)
            if len(self.parameter_key_map[param]) == 0:
                self.parameter_key_map.pop(param)

    def update_parameters(self, params, render=True):
        """parameters have changed, update regions"""
        data = self.geometrydict
        self.defer_render = True
        for param in params:
            if param in self.parameter_key_map:
                for var in self.parameter_key_map[param]:
                    name, key = var.split(',')
                    value = data[name][key]
                    self.update_geometry_value(name, value, key)
        if hasattr(self, 'render') and render:
            self.render(defer_render=False)

    def remove_from_parameter_map(self, name, del_geo):
        for key, value in del_geo.items():
            name_key = ','.join([name, key])
            self._remove_key(name_key, value)

    def _remove_key(self, name_key, value):
        if not isinstance(value, Equation):
            return

        for param in value.get_used_parameters():
            self.parameter_key_map[param].remove(name_key)
            if len(self.parameter_key_map[param]) == 0:
                self.parameter_key_map.pop(param)

    def change_parameter_geo_name(self, new_name, old_name):
        """a referenced geometry object's name has change, update parameter
        map"""

        for param, name_keys in self.parameter_key_map.items():
            for name_key in name_keys:
                name, key = name_key.split(',')
                if name == old_name:
                    self.parameter_key_map[param].remove(name_key)
                    self.parameter_key_map[param].add(','.join([new_name, key]))

    # geometry methods
    def update_transform(self, name):
        """Update the specified object's transform filter."""
        geo = self.geometrydict.get(name)
        geo_type = geo['type']
        transform = geo['transform']
        transform_filter = geo['transformfilter']
        about_center_object = not geo.get('aboutorigin', False)

        # reset to Identity
        transform.Identity()
        transform.PostMultiply()

        # translate to center
        if about_center_object:
            transform.Translate(-safe_float(geo['centerx']),
                                -safe_float(geo['centery']),
                                -safe_float(geo['centerz']))

        # scale
        if 'scale' in geo:
            transform.Scale(safe_float(geo['scale']),
                            safe_float(geo['scale']),
                            safe_float(geo['scale']))

        # rotation
        transform.RotateWXYZ(safe_float(geo['rotationx']), 1, 0, 0)
        transform.RotateWXYZ(safe_float(geo['rotationy']), 0, 1, 0)
        transform.RotateWXYZ(safe_float(geo['rotationz']), 0, 0, 1)

        # back to position
        if about_center_object:
            transform.Translate(safe_float(geo['centerx']),
                                safe_float(geo['centery']),
                                safe_float(geo['centerz']))

        # translate stl files
        if geo_type == 'stl' or geo_type in PARAMETRIC_DICT:
            transform.Translate(
                safe_float(geo['translationx']),
                safe_float(geo['translationy']),
                safe_float(geo['translationz']),
                )

        # update
        transform_filter.Update()
        return transform_filter

    def add_stl(self, filename=None, name=None, data=None, loading=False,
                proj_dir=None):

        if filename is None:
            return None

        # check for ${proj_dir} in name
        if proj_dir:
            filename = os.path.realpath(filename.replace('${proj_dir}', proj_dir))

        # need to handle geometry.stl because the gui will over-write it
        if os.path.basename(filename) == 'geometry.stl':
            old_geometry = filename + '.original'
            # if geometry.stl.original doesn't exist, copy geometry.stl to preserve it.
            if not os.path.exists(old_geometry):
                shutil.copy(filename, filename + '.original')

                if data is not None:
                    # the original stl file could have been deleted
                    self.gui.warn('geometry.stl.original is missing, please make sure the stl file is transformed correctly')

            # use the original version
            filename = old_geometry

        # purge solids
        if is_stl_ascii(filename):
            filename = purge_multi_solids(filename)

        if name is None:
            name = os.path.basename(filename).lower()
            name = get_unique_string(name, list(self.geometrydict.keys()))

        if data is not None:
            self.geometrydict[name] = data
        else:
            self.geometrydict[name] = copy.deepcopy(DEFAULT_STL_PARAMS)

        geo_data = self.geometrydict.get(name)

        # save relative path if the file is in the project directory
        save_fname = filename
        if proj_dir is not None:
            save_fname = os.path.realpath(filename).replace(proj_dir, '${proj_dir}')
        geo_data['filename'] = save_fname

        # look for stl keywords, apply and remove from .mfx
        found = False
        if self.project is not None:
            for k, keyword in [('translationx', 'tx_stl'), ('translationy', 'ty_stl'), ('translationz', 'tz_stl'),
                               ('scale', 'scale_stl')]:
                if keyword in self.project.keyword_dict:
                    found = True
                    if keyword=='scale_stl':
                        geo_data['units'] = 'custom'
                    geo_data[k] = self.project.get_value(keyword, default= 1.0 if k=='scale' else 0.0)
                    self.project.removeKeyword(keyword)
        if found:
            geo_data['aboutorigin'] = True

        # reader
        reader = vtk.vtkSTLReader()
        reader.SetFileName(filename)
        reader.MergingOn()

        # flip normals
        reverse = vtk.vtkReverseSense()
        reverse.SetInputConnection(reader.GetOutputPort())

        # Create transformer
        transform = vtk.vtkTransform()
        transform_filter = vtk.vtkTransformPolyDataFilter()
        transform_filter.SetTransform(transform)
        transform_filter.SetInputConnection(reverse.GetOutputPort())
        transform_filter.Update()

        # find center of mass
        center_filter = vtk.vtkCenterOfMass()
        center_filter.SetInputConnection(transform_filter.GetOutputPort())
        center_filter.SetUseScalarsAsWeights(False)
        center_filter.Update()
        center = center_filter.GetCenter()

        bounds = transform_filter.GetOutput().GetBounds()
        for key, bound in zip(['extentxmin', 'extentxmax',
                               'extentymin', 'extentymax',
                               'extentzmin', 'extentzmax'],
                              bounds):
            geo_data[key] = bound

        # Add to dict
        geo_data['reader'] = reader
        geo_data['reverse'] = reverse
        geo_data['transform'] = transform
        geo_data['transformfilter'] = transform_filter
        geo_data['center_filter'] = center_filter

        geo_data['centerx'] = center[0]
        geo_data['centery'] = center[1]
        geo_data['centerz'] = center[2]

        self.update_stl(name)
        self.update_transform(name)

        # Add to tree
        self.add_to_tree(name, 'geometry.svg')

        return name

    def update_stl(self, name):
        geo = self.geometrydict.get(name)

        flip = safe_int(geo['flipnormals'])
        reverse_sense = geo['reverse']
        reverse_sense.SetReverseNormals(flip)
        reverse_sense.SetReverseCells(flip)
        reverse_sense.Update()

    def add_primitive(self, primtype='sphere', name=None, data=None, loading=False):
        """Add the specified primitive"""

        # create primitive
        if data is not None:
            primtype = data['type']
        if primtype in PRIMITIVE_DICT:
            source = PRIMITIVE_DICT[primtype]()
        else:
            return

        if name is None:
            name = get_unique_string(primtype, list(self.geometrydict.keys()))

        if data is None:
            self.geometrydict[name] = copy.deepcopy(DEFAULT_PRIMITIVE_PARAMS)
            self.geometrydict[name]['type'] = primtype
        else:
            self.geometrydict[name] = data
        geo = self.geometrydict.get(name)
        geo['source'] = source

        source = self.update_primitive(name)

        # convert to triangles
        trianglefilter = vtk.vtkTriangleFilter()
        trianglefilter.SetInputConnection(source.GetOutputPort())

        # Create transformer
        transform = vtk.vtkTransform()
        transform_filter = vtk.vtkTransformPolyDataFilter()
        transform_filter.SetTransform(transform)
        transform_filter.SetInputConnection(trianglefilter.GetOutputPort())

        geo['transform'] = transform
        geo['transformfilter'] = transform_filter
        geo['trianglefilter'] = trianglefilter
        geo['source'] = source

        # update transform
        self.update_transform(name)

        # Add to tree
        self.add_to_tree(name, 'geometry.svg')

        return name

    def update_primitive(self, name):
        """Update the specified primitive"""
        primtype = self.geometrydict[name]['type']
        geo = self.geometrydict.get(name)

        if 'source' in self.geometrydict[name]:
            source = geo['source']
        else:
            source = None

        # update source
        if primtype == 'sphere':
            source.SetRadius(safe_float(geo['radius']))
            source.SetThetaResolution(safe_int(geo['thetaresolution']))
            source.SetPhiResolution(safe_int(geo['phiresolution']))
        elif primtype == 'box':
            source.SetXLength(safe_float(geo['lengthx']))
            source.SetYLength(safe_float(geo['lengthy']))
            source.SetZLength(safe_float(geo['lengthz']))
        elif primtype == 'cone':
            source.SetRadius(safe_float(geo['radius']))
            source.SetHeight(safe_float(geo['height']))
            source.SetDirection(safe_float(geo['directionx']),
                                safe_float(geo['directiony']),
                                safe_float(geo['directionz']))
            source.SetResolution(safe_int(geo['resolution']))
            source.CappingOn()
        elif primtype == 'cylinder':
            source.SetRadius(safe_float(geo['radius']))
            source.SetHeight(safe_float(geo['height']))
            source.SetResolution(safe_int(geo['resolution']))
        elif primtype == 'stl':
            pass
        else:
            return

        # common props
        if source is not None:
            source.SetCenter(safe_float(geo['centerx']),
                             safe_float(geo['centery']),
                             safe_float(geo['centerz']))
        source.Update()

        return source

    def add_procedural(self, proceduraltype=None, name=None, data=None, loading=False):
        """Add an procedural object"""

        # create implicit
        if data is not None:
            proceduraltype = data['type']

        # Source
        source = vtk.vtkPolyData()

        # transform
        transform = vtk.vtkTransform()
        transform_filter = vtk.vtkTransformPolyDataFilter()
        transform_filter.SetTransform(transform)

        if name is None:
            name = get_unique_string(proceduraltype, list(self.geometrydict.keys()))

        if data is None:
            self.geometrydict[name] = copy.deepcopy(DEFAULT_PROCEDURAL_PARAMS)
            self.geometrydict[name]['type'] = proceduraltype
        else:
            self.geometrydict[name] = data
        geo = self.geometrydict.get(name)

        geo['source'] = source
        geo['transform'] = transform
        geo['transformfilter'] = transform_filter

        self.update_procedural(name)

        # Add to tree
        self.add_to_tree(name, 'geometry.svg')

        return name

    def update_procedural(self, name):
        """Update the procedural object"""

        geo = self.geometrydict.get(name)
        proceduraltype = geo['type']

        transform = geo.get('transform')
        transform_filter = geo.get('transformfilter')
        if transform is None or transform_filter is None:
            # Should not be able to get here.
            msg = f'Error: {name} has no transform.'
            logging.getLogger(__name__).warning(msg)
            return

        poly_data = None
        if proceduraltype == 'cylinder':
            poly_data = procedural_stl.cylinder(
                geo.get('radius', 0.5),
                geo.get('height', 1.0),
                theta_res=geo.get('circumferenceresolution', 32),
                n_height=int(geo.get('heightresolution', 20)),
                n_bottom_cap=int(0 if not geo.get('bottomcap', False) else geo.get('bottomresolution', 20)),
                n_top_cap=int(0 if not geo.get('topcap', False) else geo.get('topresolution', 20)),
                theta_start=geo.get('circumferencestartangle', 0),
                theta_end=geo.get('circumferenceendangle', 360),
            )
        elif proceduraltype == 'bend':
            poly_data = procedural_stl.bend(
                rb1=geo.get('bendmajorradius', 0.5),
                rb2=geo.get('bendminorradius', 0.5),
                theta1=geo.get('bendstartangle', 0.0),
                theta2=geo.get('bendendangle', 360.0),
                bend_res=geo.get('bendresolution', 32),
                r1=geo.get('frontradius', 0.5),
                l1=geo.get('frontlength', 1.0),
                n1=geo.get('frontresolution', 1.0),
                r2=geo.get('backradius', 0.5),
                l2=geo.get('backlength', 1.0),
                n2=geo.get('backresolution', 1.0),
                theta_start=geo.get('circumferencestartangle', 0),
                theta_end=geo.get('circumferenceendangle', 260),
                theta_res=geo.get('circumferenceresolution', 32),
                offset=geo.get('offset', 0.0),
                n_front_cap=int(0 if not geo.get('bottomcap', False) else geo.get('bottomresolution', 20)),
                n_back_cap=int(0 if not geo.get('topcap', False) else geo.get('topresolution', 20)),
                )

        if poly_data is None:
            return

        geo['source'] = poly_data

        transform_filter.SetInputData(poly_data)
        transform_filter.Update()

    def add_implicit(self, implicittype=None, name=None, data=None, loading=False):
        """Add an implicit function"""

        # create implicit
        if data is not None:
            implicittype = data['type']
        if implicittype in IMPLICIT_DICT:
            source = IMPLICIT_DICT[implicittype]()
        else:
            return

        if name is None:
            name = get_unique_string(implicittype, list(self.geometrydict.keys()))

        if data is None:
            self.geometrydict[name] = copy.deepcopy(DEFAULT_IMPLICIT_PARAMS)
            self.geometrydict[name]['type'] = implicittype
        else:
            self.geometrydict[name] = data
        geo = self.geometrydict.get(name)

        # transform
        transform = vtk.vtkTransform()
        source.SetTransform(transform)

        if implicittype == 'cylinder':
            boolean = vtk.vtkImplicitBoolean()
            boolean.SetOperationTypeToDifference()
            boolean.AddFunction(source)
            geo['cylinder_source'] = source
            p1 = geo['plane1'] = vtk.vtkPlane()
            p1.SetTransform(transform)
            boolean.AddFunction(p1)
            p2 = geo['plane2'] = vtk.vtkPlane()
            p2.SetTransform(transform)
            boolean.AddFunction(p2)
            source = boolean
        elif implicittype == 'cone':
            boolean = vtk.vtkImplicitBoolean()
            boolean.SetOperationTypeToDifference()
            boolean.AddFunction(source)
            geo['cone_source'] = source
            p1 = geo['plane1'] = vtk.vtkPlane()
            p1.SetTransform(transform)
            boolean.AddFunction(p1)
            p2 = geo['plane2'] = vtk.vtkPlane()
            p2.SetTransform(transform)
            boolean.AddFunction(p2)
            source = boolean

        sample = vtk.vtkSampleFunction()
        sample.SetSampleDimensions(IMPLICIT_DEFAULT_RES, IMPLICIT_DEFAULT_RES,
                                   IMPLICIT_DEFAULT_RES)
        sample.SetImplicitFunction(source)
        sample.ComputeNormalsOff()

        # contour
        surface = vtk.vtkContourFilter()
        surface.SetInputConnection(sample.GetOutputPort())
        surface.SetValue(0, 0.0)

        geo['source'] = source
        geo['sample'] = sample
        geo['surface'] = surface
        geo['transform'] = transform

        self.update_implicit(name)

        # Add to tree
        self.add_to_tree(name, 'function.svg')

        return name

    def update_implicit(self, name):
        """update the implicit function"""
        implicittype = self.geometrydict[name]['type']
        geo = self.geometrydict.get(name)

        source = geo.get('source', None)
        sample = geo.get('sample', None)
        surface = geo.get('surface', None)
        transform = geo.get('transform', None)

        x, y, z = safe_float(geo['centerx']), safe_float(geo['centery']), safe_float(geo['centerz'])
        rotx, roty, rotz = safe_float(geo['rotationx']), safe_float(geo['rotationy']), safe_float(geo['rotationz'])
        r = safe_float(geo['radius'])
        h = safe_float(geo['height'])

        # update transform
        if transform:
            # reset to Identity
            transform.Identity()
            transform.PostMultiply()

            # back to position
            transform.Translate(-x, -y, -z)

            # rotation
            transform.RotateWXYZ(rotx, 1, 0, 0)
            transform.RotateWXYZ(roty, 0, 1, 0)
            transform.RotateWXYZ(rotz, 0, 0, 1)

        # update source
        bounds = [safe_float(geo.get(k, d)) for (k, d) in
                  zip(['minx', 'maxx', 'miny', 'maxy', 'minz', 'maxz'],
                      [-1, 1, -1, 1, -1, 1])]
        if implicittype == 'sphere':
            source.SetRadius(r)
            bounds = [-r, r, -r, r, -r, r]
        elif implicittype == 'box':
            dx = safe_float(geo['lengthx'])/2.0
            dy = safe_float(geo['lengthy'])/2.0
            dz = safe_float(geo['lengthz'])/2.0
            bounds = [-dx, dx, -dy, dy, -dz, dz]
            source.SetBounds(bounds)
        elif implicittype == 'cone':
            angle = np.rad2deg(np.arctan(r/h))
            geo['cone_source'].SetAngle(angle)
            geo['plane1'].SetOrigin(h, 0, 0)
            geo['plane1'].SetNormal(-1, 0, 0)
            geo['plane2'].SetOrigin(0, 0, 0)
            geo['plane2'].SetNormal(1, 0, 0)
            transform.Translate(h/2.0, 0, 0)
            bounds = [0, h, -r, r, -r, r]
        elif implicittype == 'cylinder':
            geo['cylinder_source'].SetRadius(r)
            geo['plane1'].SetOrigin(x, h/2, z)
            geo['plane1'].SetNormal(0, -1, 0)
            geo['plane2'].SetOrigin(x, -h/2, z)
            geo['plane2'].SetNormal(0, 1, 0)
            bounds = [-r, r, -h/2, h/2, -r, r]
        elif implicittype == 'quadric':
            source.SetCoefficients(
                safe_float(geo['a0']),
                safe_float(geo['a1']),
                safe_float(geo['a2']),
                safe_float(geo['a3']),
                safe_float(geo['a4']),
                safe_float(geo['a5']),
                safe_float(geo['a6']),
                safe_float(geo['a7']),
                safe_float(geo['a8']),
                safe_float(geo['a9']),
                )
        elif implicittype == 'superquadric':
            source.SetPhiRoundness(safe_float(geo['phi']))
            source.SetThetaRoundness(safe_float(geo['theta']))
            source.SetThickness(safe_float(geo['thickness']))
            source.SetSize(r)
            source.SetToroidal(safe_int(geo['toroidal']))
            bounds = [-r, r, -r, r, -r, r]
        else:
            return

        # common props
        if source is not None and hasattr(source, 'Update'):
            source.Update()

        if sample:
            # transform bounds
            t_mat = transform.GetInverse().GetMatrix()
            bounds_list = []
            for dx in bounds[:2]:
                for dy in bounds[2:4]:
                    for dz in bounds[4:]:
                        bounds_list.append(t_mat.MultiplyFloatPoint([dx, dy, dz, 1]))

            xs = [i[0] for i in bounds_list]
            ys = [i[1] for i in bounds_list]
            zs = [i[2] for i in bounds_list]

            bounds = [min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)]
            geo['bounds'] = bounds
            sample.SetModelBounds(*bounds)
            sample.SetSampleDimensions(IMPLICIT_DEFAULT_RES, IMPLICIT_DEFAULT_RES, IMPLICIT_DEFAULT_RES)
            sample.Update()

        if surface:
            surface.Update()

    def add_parametric(self, paramtype=None, name=None, data=None, loading=False):
        """Add the specified parametric object"""

        if paramtype is None:
            paramtype = data['type']

        if name is None:
            name = get_unique_string(paramtype, list(self.geometrydict.keys()))

        parametric_object = PARAMETRIC_DICT[paramtype]()
        source = vtk.vtkParametricFunctionSource()
        source.SetParametricFunction(parametric_object)

        if data is None:
            self.geometrydict[name] = copy.deepcopy(DEFAULT_PARAMETRIC_PARAMS)
            self.geometrydict[name]['type'] = paramtype
        else:
            self.geometrydict[name] = data
        geo = self.geometrydict.get(name)

        geo['parametric_object'] = parametric_object
        geo['source'] = source

        source = self.update_parametric(name)

        # Make sure it is triangles
        trianglefilter = vtk.vtkTriangleFilter()
        trianglefilter.SetInputConnection(source.GetOutputPort())

        # Create transformer
        transform = vtk.vtkTransform()
        transform_filter = vtk.vtkTransformPolyDataFilter()
        transform_filter.SetTransform(transform)
        transform_filter.SetInputConnection(trianglefilter.GetOutputPort())

        geo['source'] = source
        geo['trianglefilter'] = trianglefilter
        geo['transform'] = transform
        geo['transformfilter'] = transform_filter

        # update transform
        self.update_transform(name)

        # Add to tree
        self.add_to_tree(name, 'geometry.svg')

        return name

    def update_parametric(self, name):
        """Update the specified parameteric object."""
        geo = self.geometrydict.get(name)
        paratype = geo['type']
        para_object = geo['parametric_object']
        source = geo['source']

        if paratype == 'torus':
            para_object.SetRingRadius(safe_float(geo['ringradius']))
            para_object.SetCrossSectionRadius(safe_float(geo['crosssectionradius']))
        elif paratype == 'boy':
            para_object.SetZScale(safe_float(geo['zscale']))
        elif paratype == 'conic_spiral':
            para_object.SetA(safe_float(geo['ascale']))
            para_object.SetB(safe_float(geo['bfunc']))
            para_object.SetC(safe_float(geo['cfunc']))
            para_object.SetN(safe_float(geo['nfunc']))
        elif paratype == 'dini':
            para_object.SetA(safe_float(geo['ascale']))
            para_object.SetB(safe_float(geo['bscale']))
        elif paratype == 'ellipsoid':
            para_object.SetXRadius(safe_float(geo['radiusx']))
            para_object.SetYRadius(safe_float(geo['radiusy']))
            para_object.SetZRadius(safe_float(geo['radiusz']))
        elif paratype == 'figure_8_klein':
            para_object.SetRadius(safe_float(geo['radius']))
        elif paratype == 'mobius':
            para_object.SetRadius(safe_float(geo['radius']))
        elif paratype == 'random_hills':
            para_object.SetHillXVariance(safe_float(geo['variancex']))
            para_object.SetXVarianceScaleFactor(safe_float(geo['scalex']))
            para_object.SetHillYVariance(safe_float(geo['variancey']))
            para_object.SetYVarianceScaleFactor(safe_float(geo['scaley']))
            para_object.SetHillAmplitude(safe_float(geo['amplitude']))
            para_object.SetAmplitudeScaleFactor(safe_float(geo['scaleamplitude']))
            para_object.SetNumberOfHills(safe_int(geo['nhills']))
            para_object.SetAllowRandomGeneration(safe_int(geo['allowrandom']))
        elif paratype == 'roman':
            para_object.SetRadius(safe_float(geo['radius']))
        elif paratype == 'super_ellipsoid':
            para_object.SetXRadius(safe_float(geo['radiusx']))
            para_object.SetYRadius(safe_float(geo['radiusy']))
            para_object.SetZRadius(safe_float(geo['radiusz']))
            para_object.SetN1(safe_float(geo['n1']))
            para_object.SetN2(safe_float(geo['n2']))
        elif paratype == 'super_toroid':
            para_object.SetXRadius(safe_float(geo['radiusx']))
            para_object.SetYRadius(safe_float(geo['radiusy']))
            para_object.SetZRadius(safe_float(geo['radiusz']))
            para_object.SetRingRadius(safe_float(geo['ringradius']))
            para_object.SetCrossSectionRadius(safe_float(geo['crosssectionradius']))
            para_object.SetN1(safe_float(geo['n1']))
            para_object.SetN2(safe_float(geo['n2']))

        source.Update()
        return source

    def add_filter(self, filtertype=None, name=None, data=None, child=None,
                   loading=False):
        """add the selected filter with the input being the currently selected
        toplevel item"""

        if name is None:
            name = get_unique_string(filtertype, list(self.geometrydict.keys()))

        if data is None:
            self.geometrydict[name] = copy.deepcopy(DEFAULT_FILTER_PARAMS)
            self.geometrydict[name]['type'] = filtertype
        else:
            if name is None:
                name = get_unique_string(data['type'], list(self.geometrydict.keys()))
            self.geometrydict[name] = data

        geo = self.geometrydict.get(name)

        # init filter
        if data is not None:
            filtertype = data['type']
        vtkfilter = geo['filter'] = FILTER_DICT[filtertype]()

        if filtertype == 'transform':
            t = geo['transform'] = vtk.vtkTransform()
            vtkfilter.SetTransform(t)

        # set input data
        if 'implicit' in filtertype:
            sample = vtk.vtkSampleFunction()
            source_data = self.geometrydict.get(child)
            sample.SetImplicitFunction(source_data.get('source'))
            sample.ComputeNormalsOff()
            geo['samplefunction'] = sample
            if 'minx' not in geo:
                extents = dict([(k, copy.deepcopy(v)) for k, v in zip(['minx', 'maxx', 'miny', 'maxy', 'minz', 'maxz'], source_data['bounds'])])
                geo.update(extents)
            vtkfilter.SetInputConnection(sample.GetOutputPort())
            vtkfilter.SetValue(0, 0.0)
        else:
            inputdata = self.get_input_data(child)
            vtkfilter.SetInputConnection(inputdata.GetOutputPort())

        # update filter
        self.update_filter(name)

        # Add to tree
        self.add_to_tree(name, 'filter.svg', [child])

        return name

    def update_filter(self, name):
        """Update the currently selected filter"""
        geo = self.geometrydict.get(name)
        filtertype = geo['type']
        vtkfilter = geo['filter']

        if filtertype == 'clean':
            vtkfilter.SetConvertLinesToPoints(safe_int(geo['linestopoints']))
            vtkfilter.SetConvertPolysToLines(safe_int(geo['polystolines']))
            vtkfilter.SetConvertStripsToPolys(safe_int(geo['stripstopolys']))
        elif filtertype == 'fill_holes':
            vtkfilter.SetHoleSize(safe_float(geo['maximumholesize']))
        elif filtertype == 'triangle':
            vtkfilter.SetPassVerts(safe_int(geo['processvertices']))
            vtkfilter.SetPassLines(safe_int(geo['processlines']))
        elif filtertype == 'decimate':
            vtkfilter.SetTargetReduction(safe_float(geo['targetreduction']))
        elif filtertype == 'quadric_decimation':
            vtkfilter.SetTargetReduction(safe_float(geo['targetreduction']))
        elif filtertype == 'quadric_clustering':
            vtkfilter.SetNumberOfXDivisions(safe_int(geo['divisionsx']))
            vtkfilter.SetNumberOfYDivisions(safe_int(geo['divisionsy']))
            vtkfilter.SetNumberOfZDivisions(safe_int(geo['divisionsz']))
            vtkfilter.SetAutoAdjustNumberOfDivisions(safe_int(geo['autoadjustdivisions']))
        elif filtertype == 'smooth':
            vtkfilter.SetEdgeAngle(safe_float(geo['edgeangle']))
            vtkfilter.SetFeatureAngle(safe_float(geo['featureangle']))
            vtkfilter.SetNumberOfIterations(safe_int(geo['iterations']))
            vtkfilter.SetRelaxationFactor(safe_float(geo['relaxation']))
            vtkfilter.SetFeatureEdgeSmoothing(safe_int(geo['featureedgesmoothing']))
            vtkfilter.SetBoundarySmoothing(safe_int(geo['boundarysmoothing']))
        elif filtertype == 'windowed_sinc':
            vtkfilter.SetEdgeAngle(safe_float(geo['edgeangle']))
            vtkfilter.SetFeatureAngle(safe_float(geo['featureangle']))
            vtkfilter.SetNumberOfIterations(safe_int(geo['iterations']))
            vtkfilter.SetPassBand(safe_float(geo['passband']))
            vtkfilter.SetFeatureEdgeSmoothing(safe_int(geo['featureedgesmoothing']))
            vtkfilter.SetBoundarySmoothing(safe_int(geo['boundarysmoothing']))
            vtkfilter.SetNonManifoldSmoothing(safe_int(geo['manifoldsmoothing']))
            vtkfilter.SetNormalizeCoordinates(safe_int(geo['normalize']))
        elif filtertype == 'reverse_sense':
            vtkfilter.SetReverseCells(safe_int(geo['reversecells']))
            vtkfilter.SetReverseNormals(safe_int(geo['reversenormals']))
        elif filtertype == 'flip_normals':
            flip = safe_int(geo['flipnormals'])
            vtkfilter.SetReverseCells(flip)
            vtkfilter.SetReverseNormals(flip)
        elif filtertype == 'transform':
            transform = geo['transform']
            # reset to Identity
            transform.Identity()
            transform.PostMultiply()

            # find the center
            vtkfilter.Update()
            polydata = vtkfilter.GetInput()
            com = vtk.vtkCenterOfMass()
            com.SetInputData(polydata)
            com.SetUseScalarsAsWeights(False)
            com.Update()
            x, y, z = com.GetCenter()

            # translate to center
            transform.Translate(-x, -y, -z)

            # scale
            transform.Scale(safe_float(geo['scalex']),
                            safe_float(geo['scaley']),
                            safe_float(geo['scalez']))

            # rotation
            transform.RotateWXYZ(safe_float(geo['rotationx']), 1, 0, 0)
            transform.RotateWXYZ(safe_float(geo['rotationy']), 0, 1, 0)
            transform.RotateWXYZ(safe_float(geo['rotationz']), 0, 0, 1)

            # translate
            transform.Translate(x + safe_float(geo['translatex']),
                                y + safe_float(geo['translatey']),
                                z + safe_float(geo['translatez']))
        elif filtertype == 'sample_implicit':
            sample = geo.get('samplefunction')
            sample.SetSampleDimensions(
                safe_int(geo['samplesx']), safe_int(geo['samplesy']), safe_int(geo['samplesz']))
            bounds = [safe_float(geo.get(k, d)) for (k, d) in
                      zip(['minx', 'maxx', 'miny', 'maxy', 'minz', 'maxz'],
                          [-1, 1, -1, 1, -1, 1])]
            sample.SetModelBounds(bounds)

            # look for the mapper, if it exists, replace the surface filter
            mapper = geo.get('mapper', None)
            if mapper is not None:
                surf = geo['filter'] = vtk.vtkContourFilter()
                surf.SetInputConnection(sample.GetOutputPort())
                surf.SetValue(0, 0.0)
                mapper.SetInputConnection(surf.GetOutputPort())

        vtkfilter.Update()

    def get_input_data(self, name):
        """based on the type of geometry, return the data"""
        geo = self.geometrydict.get(name)
        if geo is None:
            return None
        elif 'booleanoperation' in geo:
            inputdata = geo['booleanoperation']
        elif 'filter' in geo:
            inputdata = geo['filter']
        elif 'transformfilter' in geo:
            inputdata = geo['transformfilter']
        else:
            raise ValueError('Unknown Geometry: {}'.format(geo))
        inputdata.Update()
        return inputdata

    def boolean_operation(self, booltype=None, boolname=None, data=None,
                          children=None, loading=False):
        """Apply a boolean operation with the currently selected toplevel
        items."""

        if boolname is None:
            boolname = get_unique_string(booltype, list(self.geometrydict.keys()))

        # Save references
        if data is not None:
            bool_data = self.geometrydict[boolname] = data
            booltype = data['type']
        else:
            bool_data = self.geometrydict[boolname] = copy.deepcopy(DEFAULT_BOOLEAN_PARAMS)
            bool_data['type'] = booltype

        implicit = all(['implicit' in self.geometrydict.get(child).get('geo_type') for child in children])

        if implicit:
            boolean_operation = vtk.vtkImplicitBoolean()
        else:
            boolean_operation = vtk.vtkBooleanOperationPolyDataFilter()

        if booltype == 'union':
            icon = 'union'
            if implicit:
                boolean_operation.SetOperationTypeToUnion()
            else:
                boolean_operation.SetOperationToUnion()
        elif booltype == 'intersection':
            icon = 'intersect'
            if implicit:
                boolean_operation.SetOperationTypeToIntersection()
            else:
                boolean_operation.SetOperationToIntersection()
        else:
            icon = 'difference'
            if implicit:
                boolean_operation.SetOperationTypeToDifference()
            else:
                boolean_operation.SetOperationToDifference()

        union_list = []
        bool_data['children'] = children
        for i, child_name in enumerate(children):
            child = self.geometrydict[child_name]
            if implicit:
                boolean_operation.AddFunction(child['source'])
                union_list.append(child['bounds'])
            else:
                geometry = self.get_input_data(child_name)
                boolean_operation.SetInputConnection(
                    i, geometry.GetOutputPort())

            # hide the sources
            actor = child.get('actor', None)
            if actor is not None:
                actor.VisibilityOff()
                child['visible'] = False

        if implicit:
            sample = vtk.vtkSampleFunction()
            sample.SetSampleDimensions(*[IMPLICIT_DEFAULT_RES]*3)
            sample.SetImplicitFunction(boolean_operation)
            sample.ComputeNormalsOff()

            # union the bounds
            array = np.asarray(union_list)
            bounds = []
            for i in range(0, 6, 2):
                bounds += [min(array[:, i]), max(array[:, i+1])]
            sample.SetModelBounds(*bounds)

            # contour
            surface = vtk.vtkContourFilter()
            surface.SetInputConnection(sample.GetOutputPort())
            surface.SetValue(0, 0.0)

            bool_data['sample'] = sample
            bool_data['surface'] = surface
            bool_data['geo_type'] = 'boolean_implicit'
            bool_data['source'] = boolean_operation
            bool_data['bounds'] = bounds

        if hasattr(boolean_operation, 'Update'):
            boolean_operation.Update()

        # save references
        bool_data['booleanoperation'] = boolean_operation

        # Add to tree
        self.add_to_tree(boolname, icon, children)

        return boolname

    def get_toplevel_geomtry_names(self, visible_only=True):
        """collect toplevel geometry names"""
        names = []
        for top in self.geometrytree.keys():
            geo_data = self.geometrydict.get(top)
            if geo_data is None:
                continue
            geo_type = geo_data.get('geo_type', '')
            visible = geo_data.get('visible', False)
            if (visible or not visible_only) and 'implicit' not in geo_type:
                names.append(top)
        return names

    def collect_toplevel_geometry(self, visible_only=False):
        """collect and append visible toplevel polydata"""

        append_filter = vtk.vtkAppendPolyData()

        for top in self.get_toplevel_geomtry_names(visible_only=visible_only):
            append_filter.AddInputData(self.get_input_data(top).GetOutput())

        geo = None
        if append_filter.GetTotalNumberOfInputConnections() > 0:
            append_filter.Update()

            # clean
            clean_filter = vtk.vtkCleanPolyData()
            clean_filter.SetInputConnection(append_filter.GetOutputPort())
            clean_filter.Update()

            geo = clean_filter

        return geo

    def collect_all_geometry(self):
        """collect and append all visible polydata"""

        append_filter = vtk.vtkAppendPolyData()

        for top, geo_data in self.geometrydict.items():
            if geo_data is None:
                continue
            geo_type = geo_data.get('geo_type', '')
            visible = geo_data.get('visible', False)
            if visible and 'implicit' not in geo_type:
                append_filter.AddInputData(
                    self.get_input_data(top).GetOutput())

        geo = None
        if append_filter.GetTotalNumberOfInputConnections() > 0:
            append_filter.Update()

            # clean
            clean_filter = vtk.vtkCleanPolyData()
            clean_filter.SetInputConnection(append_filter.GetOutputPort())
            clean_filter.Update()

            geo = clean_filter

        return geo

    def get_geometry_extents(self):
        """determine the extents of the visible geometry"""

        geometry = self.collect_toplevel_geometry()

        bounds = None
        if geometry:
            bounds = geometry.GetOutput().GetBounds()

        return bounds

    def select_facets(self, region):

        output = None

        # collect the bounds
        bounds = [0.0]*6
        # make sure the from < to
        zipped = list(zip(region['from'], region['to']))
        bounds[::2] = [min(f, t) for f, t in zipped]
        bounds[1::2] = [max(f, t) for f, t in zipped]

        lengths = [abs(safe_float(to) - safe_float(f)) for
                   f, to in zip(region['from'], region['to'])]
        center = [min(f) + l / 2.0 for f, l in
                  zip(zip(region['from'], region['to']),
                      lengths)]

        # geometry pool
        geo_pool = region.get('geometry_pool', [])
        geo = None
        if geo_pool:
            append_filter = vtk.vtkAppendPolyData()
            for name in geo_pool:
                geo = self.get_input_data(name)
                if geo is not None:
                    append_filter.AddInputData(geo.GetOutput())

            geo = None
            if append_filter.GetTotalNumberOfInputConnections() > 0:
                append_filter.Update()
                # clean
                clean_filter = vtk.vtkCleanPolyData()
                clean_filter.SetInputConnection(append_filter.GetOutputPort())
                clean_filter.Update()
                geo = clean_filter
        # else:
        #     geo = self.collect_toplevel_geometry()

        # check for geometry, or not toplevel geometry was selected.
        if geo is None:
            # something failed
            return None, 0

        # create the implicit
        implicit = None
        stl_select_shape = region['stl_shape']
        if stl_select_shape.lower() == 'box':
            implicit = vtk.vtkBox()
            implicit.SetBounds(bounds)
        elif stl_select_shape.lower() == 'ellipsoid':
            trans = vtk.vtkTransform()
            # for some reason, everything is inverted?
            trans.Scale([1/l if l > 0 else 1 for l in lengths])
            trans.Translate([-c for c in center])
            implicit = vtk.vtkSphere()
            implicit.SetTransform(trans)
        else:
            implicit = vtk.vtkBox()
            extents = [e+o for e, o in zip(self.get_geometry_extents(), (-1, 1, -1, 1, -1, 1))]
            implicit.SetBounds(extents)

        method = region.get('method', 'full').lower()
        # Slice
        if method == 'slice':
            clipper = vtk.vtkClipPolyData()
            clipper.SetClipFunction(implicit)
            clipper.SetInputConnection(geo.GetOutputPort())
            clipper.GenerateClippedOutputOn()
            clipper.InsideOutOn()
            clipper.Update()
            clipper_output = clipper.GetClippedOutputPort()
        # select
        else:
            clipper = vtk.vtkExtractPolyDataGeometry()
            clipper.SetImplicitFunction(implicit)
            clipper.SetInputConnection(geo.GetOutputPort())
            clipper.ExtractInsideOn()
            if method == 'partial':
                clipper.ExtractBoundaryCellsOn()
            else:
                clipper.ExtractBoundaryCellsOff()
            clipper.Update()
            clipper_output = clipper.GetOutputPort()

        # -- filter normals --
        if region.get('filter_facets', False):
            # calculate normals
            normal_generator = vtk.vtkPolyDataNormals()
            normal_generator.SetInputConnection(clipper_output)
            normal_generator.ComputePointNormalsOff()
            normal_generator.ComputeCellNormalsOn()
            normal_generator.Update()
            normals = normal_generator.GetOutput().GetCellData().GetArray("Normals")
            if normals is not None:
                # build list of cells to keep
                ids = vtk.vtkIdTypeArray()
                ids.SetNumberOfComponents(1)
                filter_vet = np.asarray(region.get('filter', [0, 1, 0]))
                dev_angle = region.get('deviation_angle', 10)
                equil = region.get('equilibrant', False)
                invert = region.get('invert', False)
                for i in range(normals.GetNumberOfTuples()):
                    normal = normals.GetTuple3(i)
                    angle = np.degrees(angle_between(normal, filter_vet))

                    ind = None
                    if angle < dev_angle:
                        ind = i
                    elif equil:
                        angle = np.degrees(angle_between(normal, -filter_vet))
                        if angle < dev_angle:
                            ind = i

                    if ind is not None and not invert:
                        ids.InsertNextValue(ind)
                    elif ind is None and invert:
                        ids.InsertNextValue(i)

                # build selection
                selection_node = vtk.vtkSelectionNode()
                selection_node.SetFieldType(vtk.vtkSelectionNode.CELL)
                selection_node.SetContentType(vtk.vtkSelectionNode.INDICES)
                selection_node.SetSelectionList(ids)
                selection = vtk.vtkSelection()
                selection.AddNode(selection_node)

                # extract the selected cells
                extract_selection = vtk.vtkExtractSelectedIds()
                extract_selection.SetInputConnection(0, clipper_output)
                extract_selection.SetInputData(1, selection)
                extract_selection.Update()
                # convert from unstructed grid to polydata
                # This step seems strange
                surface = vtk.vtkDataSetSurfaceFilter()
                surface.SetInputConnection(extract_selection.GetOutputPort())
                surface.Update()
                output = surface.GetOutput()
        if output is None:
            output = clipper.GetOutput()

        return output, output.GetNumberOfCells()

    def export_stl(self, file_name, bcs={}, iss={}):
        """export visible toplevel geometry"""
        # Avoid problems with Unicode in user's home directory,
        # since VTK does not handle Unicode filenames
        file_name = os.path.relpath(file_name)
        path = os.path.dirname(file_name)

        # Export geometry
        geometry = self.collect_toplevel_geometry()
        use_stl = False
        if geometry:
            self.project.updateKeyword('cartesian_grid', True)
            self.project.updateKeyword('use_stl', True)
            use_stl = True
            # write file
            stl_writer = vtk.vtkSTLWriter()
            stl_writer.SetFileName(file_name)
            stl_writer.SetInputConnection(geometry.GetOutputPort())
            stl_writer.Write()
        elif self.project.get_value('n_quadric', None) is not None:
            self.project.updateKeyword('cartesian_grid', True)
        elif self.project.get_value('use_msh', False):
            self.project.updateKeyword('cartesian_grid', True)
        elif self.project.get_value('use_polygon', False):
            self.project.updateKeyword('cartesian_grid', True)
        else:
            self.project.updateKeyword('cartesian_grid', False)
            self.project.updateKeyword('use_stl', False)

        # break if not using STL as input
        if not use_stl:
            return

        # write name_000n.stl for stl regions
        for basename, items in [('geometry', bcs.items()), ('is', iss.items())]:

            for i, bc_data in items:
                region = bc_data['region']
                data = {}

                if region in self.region_dict:
                    data = self.region_dict[region]
                else:
                    continue
                if data['type'] == 'STL':
                    fname = os.path.join(path, basename + '_{0:04d}.stl'.format(i))
                    stl_writer = vtk.vtkSTLWriter()
                    stl_writer.SetFileName(fname)

                    if 'output' in data:
                        # use the clipper object in the gui
                        output = data['output']
                    else:
                        # we are copying geometry and saving it. need to slice
                        output, n_facets = GeometryEngine.select_facets(self, data)
                    if output is None:
                        msg = f"Error: No facets to write to {fname}"
                        logging.getLogger(__name__).warning(msg)
                        self.gui.message(text=msg)
                        continue
                    stl_writer.SetInputData(output)
                    if not stl_writer.Write():
                        msg = f"Error: VTK failed to write {fname}"
                        logging.getLogger(__name__).warning(msg)
                        self.gui.message(text=msg)


def test_load_spouted_bed():
    PARAMETER_DICT["height"] = 20.0
    PARAMETER_DICT["rad"] = 5.0
    PARAMETER_DICT["cone_height"] = 10.0
    PARAMETER_DICT["spout_rad"] = 1.0
    geo_spouted = """
        {"geometry_dict": {"cylinder1": {"geo_type": "implicit",
         "bounds": [-0.007499999832361937, 0.007499999832361937,
         -0.20000000298023224, 0.10000000894069672, -0.007499999832361937,
         0.007499999832361937], "centery": {"__type__": "equation", "eq":
         "-cone_height/2*cm", "type": "float"}, "radius": {"__type__":
         "equation", "eq": "spout_rad*cm", "type": "float"}, "height":
         {"__type__": "equation", "eq": "(height+cone_height)*cm", "type":
         "float"}, "type": "cylinder", "visible": false}, "cone":
         {"geo_type": "implicit", "bounds": [-0.05000000074505806,
         0.05000000074505806, -0.20000000298023224, -0.10000000149011612,
         -0.05000000074505806, 0.05000000074505806], "centery": {"__type__":
         "equation", "eq": "-(cone_height/2+height/2)*cm", "type": "float"},
         "rotationz": -90.0, "radius": {"__type__": "equation", "eq":
         "rad*cm", "type": "float"}, "height": {"__type__": "equation",
         "eq": "cone_height*cm", "type": "float"}, "type": "cone",
         "visible": false}, "cylinder": {"geo_type": "implicit", "bounds":
         [-0.05000000074505806, 0.05000000074505806, -0.10000000149011612,
         0.10000000149011612, -0.05000000074505806, 0.05000000074505806],
         "radius": {"__type__": "equation", "eq": "rad*cm", "type":
         "float"}, "height": {"__type__": "equation", "eq": "height*cm",
         "type": "float"}, "type": "cylinder", "visible": false},
         "cylinder2": {"geo_type": "implicit", "bounds":
         [-0.011250000447034836, 0.011250000447034836, -0.15000000596046448,
         -0.05000000074505806, -0.011250000447034836, 0.011250000447034836],
         "centery": {"__type__": "equation", "eq": "-height/2*cm", "type":
         "float"}, "radius": {"__type__": "equation", "eq":
         "spout_rad*1.5*cm", "type": "float"}, "height": {"__type__":
         "equation", "eq": "height/2*cm", "type": "float"}, "type":
         "cylinder", "visible": false}, "union": {"geo_type":
         "boolean_implicit", "children": ["cylinder", "cone"], "visible":
         false, "type": "union", "bounds": [-0.05000000074505806,
         0.05000000074505806, -0.20000000298023224, 0.10000000149011612,
         -0.05000000074505806, 0.05000000074505806]}, "difference":
         {"geo_type": "boolean_implicit", "children": ["union",
         "cylinder2"], "visible": false, "type": "difference", "bounds":
         [-0.05000000074505806, 0.05000000074505806, -0.20000000298023224,
         0.10000000149011612, -0.05000000074505806, 0.05000000074505806]},
         "union1": {"geo_type": "boolean_implicit", "children":
         ["cylinder1", "difference"], "visible": false, "type": "union",
         "bounds": [-0.05000000074505806, 0.05000000074505806,
         -0.20000000298023224, 0.10000000894069672, -0.05000000074505806,
         0.05000000074505806]}, "sample_implicit": {"geo_type": "filter",
         "minx": -0.05000000074505806, "maxx": 0.05000000074505806, "miny":
         -0.20000000298023224, "maxy": 0.10000000894069672, "minz":
         -0.05000000074505806, "maxz": 0.05000000074505806, "samplesy":
         60.0, "type": "sample_implicit"}}, "tree": {"sample_implicit":
         ["union1"], "union1": ["cylinder1", "difference"], "cylinder1": [],
         "difference": ["union", "cylinder2"], "union": ["cylinder",
         "cone"], "cylinder": [], "cone": [], "cylinder2": []}}
        """

    proj = NoopProject()
    ge = GeometryEngine(proj=proj)
    ge.geometry_from_str(geo_spouted)
    ge.export_stl('test.stl')
    print(ge.geometry_to_str())


def test_load():
    geo = """{"geometry_dict": {"cylinder1": {"geo_type": "primitive",
              "rotationz": 90.0, "radius": 0.4, "height": 3.0, "visible": false,
              "type": "cylinder"}, "sphere1": {"geo_type": "primitive",
              "visible": false, "type": "sphere"}, "torus": {"geo_type":
              "parametric", "type": "torus"}, "f1.stl": {"geo_type": "stl",
              "filename": "/nfs/home/jweber/mfix_projects/test/F1.STL",
              "centerx": 26.843507608475985,
              "centery": 55.90181490218072, "centerz": 36.23014965987697,
              "visible": false, "extentxmax": 72.1491928100586, "extentymin":
              -1.5070410197548512e-14, "extentymax": 101.61001586914062,
              "extentzmin": 0.03376975655555725, "extentzmax":
              69.00269317626953}, "sphere": {"geo_type": "implicit", "type":
              "sphere", "visible": false}, "cylinder": {"geo_type": "primitive",
              "radius": 0.4, "height": 3.0, "visible": false, "type":
              "cylinder"}, "union": {"geo_type": "boolean", "children":
              ["cylinder", "sphere1"], "visible": false, "type": "union"},
              "union1": {"geo_type": "boolean", "children": ["cylinder1",
              "union"], "type": "union"}, "sample_implicit": {"geo_type":
              "filter", "type": "sample_implicit"}}, "tree": {"torus": [],
              "f1.stl": [], "union1": ["cylinder1", "union"], "cylinder1": [],
              "union": ["cylinder", "sphere1"], "cylinder": [], "sphere1": [],
              "sample_implicit": ["sphere"], "sphere": []}}"""

    proj = NoopProject()
    ge = GeometryEngine(proj=proj)
    ge.geometry_from_str(geo)
    ge.export_stl('test.stl')
    print(ge.geometry_to_str())


class NoopProject:
    keyword_dict: Mapping[str, str] = {}

    def noop(self, *args, **kwargs):
        return False

    def __getattr__(self, name):
        return self.noop


def main():
    import argparse
    from mfixgui.tools.qt import deepcopy_dict
    from mfixgui.widgets.regions import DEFAULT_REGION_DATA

    parser = argparse.ArgumentParser(
        description="Given a *.mfx file, Write geometry.stl")
    ARG = parser.add_argument
    ARG(
        "project",
        action="store",
        default=None,
        help="mfx file to read",
    )
    args = parser.parse_args()

    proj_file = args.project
    proj_dir = os.path.dirname(proj_file)
    # check the project file
    if not os.path.exists(proj_file):
        print("Error: Project file does not exist: {}".format(proj_file))

    # load the project
    print('Loading project file: {}'.format(proj_file))
    proj = Project(proj_file)

    # load the geometry
    print('Creating geometry.')
    geo_engine = GeometryEngine(gui=None, proj=proj)
    geo_engine.geometry_from_str(proj.mfix_gui_comments.get('geometry'),
                                 proj_dir=proj_dir)

    loaded_data = ExtendedJSON.loads(proj.mfix_gui_comments.get('regions_dict'))
    data = {}
    for region in loaded_data['order']:
        region_data = data[region] = deepcopy_dict(DEFAULT_REGION_DATA)
        region_data.update(loaded_data['regions'][region])
    geo_engine.region_dict = data

    # collect bc info
    bcs = ExtendedJSON.loads(proj.mfix_gui_comments.get('bc_regions'))
    bcs_dict = {}
    for (indices, regions) in bcs:
        bcs_dict[indices[0]] = {'region': regions[0]}

    # collect is info
    iss = ExtendedJSON.loads(proj.mfix_gui_comments.get('is_regions'))
    iss_dict = {}
    for (indices, regions) in iss:
        iss_dict[indices[0]] = {'region': regions[0]}

    # save geometry.stl
    geo_file = os.path.join(proj_dir, 'geometry.stl')
    print('Saving {}'.format(geo_file))
    geo_engine.export_stl(geo_file, bcs_dict, iss_dict)


if __name__ == "__main__":
    main()
