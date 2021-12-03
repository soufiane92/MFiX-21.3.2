import vtk
import random
import math
import numpy as np
from vtk.util import numpy_support

import splipy.curve_factory   as cf
import splipy.surface_factory as sf
import splipy.volume_factory  as vf
from splipy.io import STL
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

def main():
    print('Procedural STL')
    print("Rendering Polydata, press 'q' while the render window is active to render the next polydata.")

# Below are a few examples of creating procedural geometry that can be saved as STL files or used as VTK PolyData.
# CAUTION: There are no data checks, so entering invalid input may result in a FPE.

# Example#1: Cylinder of radius 0.1 and height 0.5, with caps at both ends
    cyl = cylinder(0.1,0.5,theta_res=16, n_height=5 ,n_bottom_cap=3,n_top_cap=3)
    export_polydata_to_stl(cyl,'cylinder.stl')
    render(cyl)

# Example#2: Cyclone
# Use a list of segments and revolve them around the y-axis
# List of points (x,y) coordinates
    A = [0.0,  0.0,  0.0]
    B = [0.1,  0.0,  0.0]
    C = [0.1,  0.5,  0.0]
    D = [0.3,  1.0,  0.0]
    E = [0.3,  1.5,  0.0]
    F = [0.15, 1.5,  0.0]
    G = [0.15, 1.2,  0.0]
    H = [0.1,  1.2,  0.0]
    I = [0.1,  1.7,  0.0]
    J = [0.0,  1.7,  0.0]

# List of segments: a segment can be:
#   - a line: Two end points (x,y) and a resolution  (number of divisions)
#   - an arc: Center (x,y), radius, starting angle (deg), ending angle (deg), and resolution (number of divisions)
    cyclone_segments = [['line',A,B,3], ['line',B,C,10], ['line',C,D,10], ['line',D,E,10], ['line',E,F,3], ['line',F,G,3], ['line',G,H,2], ['line',H,I,5],['line',I,J,3]]

# Revolve the segments. The segments are divided inside Revolve_Segments
    cyclone = revolve_segments(cyclone_segments, theta_start=0.0, theta_end=360.0, theta_res=32)
    export_polydata_to_stl(cyclone,'cyclone.stl')
    render(cyclone)

# Example#3: Torus
# This is a special case of the 360 degree bend, with no front nor back sections
#                Torus                      Front cone      Back cone    Front cap        Back cap
    torus = bend(0.2,0.05,0.0,360.0,64,     0.0,0.0,0,      0.0,0.0,0 )
    export_polydata_to_stl(torus,'torus.stl')
    render(torus)

# Example#4: Coil
# Define points along a circle and revolve them.
# The offset translates the circle in the y-direction as it is rotated along y-axis
    # xc, yc = Circle_xy([0.2,0.0,0.0],0.05,Theta_start=0, Theta_end=360, Theta_res=32)
    # Coil = Revolve_points(xc, yc, Theta_start=0, Theta_end=1800, Theta_res=320, offset=0.2)
#                Torus                      Front cone      Back cone    Front cap        Back cap
    nrev = 5.0 # number of revolutions
    # coil2 = bend(0.2,0.05,0.0,nrev*360,int(nrev*32),     0.05,1.0,20,     0.05,1.0,20,  offset = 0.15, n_front_cap = 3, n_back_cap =3)
    coil = bend(0.2,0.05,0.0,nrev*360,int(nrev*32),     0.0,0.0,0,      0.0,0.0,0, offset = 0.2)
    export_polydata_to_stl(coil,'coil.stl')
    render(coil)

# Example#5: Bend
# A bend is made up of three sections:
#   - Torus section: Major radius, minor radius, starting angle (deg), ending angle (deg), number of divisions
#   - Front conical frustrum: front radius, length, number of divisions
#   - Back conical frustrum: back radius, length, number of divisions

# Note: to get front and back cylinders, use the same values for the front and back radii as the torus minor radius

#                     Torus                    Front cone      Back cone    Front cap        Back cap
    bend_90deg = bend(0.2,0.05,0.0,90.0,8,     0.1,0.5,10,     0.05,0.25,5, n_front_cap = 5, n_back_cap =5)
    export_polydata_to_stl(bend_90deg,'bend.stl')
    render(bend_90deg)


# Example#6: Beveled expansion
# Use a list of segments and revolve them around the y-axis
# The arc segments are creating the bevel between the straight edges

# List of points (x,y) coordinates
    A = [0.1, 0.0, 0.0]
    B = [0.1, 0.5, 0.0]
    C = [0.2, 0.6, 0.0]
    D = [0.3, 0.6, 0.0]
    E = [0.4, 0.7, 0.0]
    F = [0.4, 1.2, 0.0]
    G = [0.2, 0.5, 0.0]
    H = [0.3, 0.7, 0.0]

# List of segments: a segment can be:
#   - a line: Two end points (x,y) and a resolution  (number of divisions)
#   - an arc: Center (x,y), radius, starting angle (deg), ending angle (deg), and resolution (number of divisions)
    reactor_segments = [['line',A,B,15], ['arc',G,0.1,180,90,8], ['line',C,D,3], ['arc',H,0.1,270,360,8], ['line',E,F,10]]

# Revolve the segments. The segments are divided inside Revolve_Segments
    expansion = revolve_segments(reactor_segments, theta_start=0.0, theta_end=360.0, theta_res=32, offset=0.0)
    export_polydata_to_stl(expansion,'expansion.stl')
    render(expansion)

#Example 7: A Tube connecting a circle and a rectangle
    # Crossover = Circle_to_rectangle(Radius,W,nw,H,nh,L,n,y_offset=0, z_offset=0):
    crossover = circle_to_rectangle(0.1,0.2,8,0.1,4,1.0,20,x_offset=0.0, y_offset=0.0, n_front_cap=3, back_cap=True)
    export_polydata_to_stl(crossover,'crossover.stl')
    render(crossover)


# Example#8: Coil with extension
# This uses the Bend function with an offset
    nrev = 9.5 # number of revolutions
    coil2 = bend(0.2,0.05,0.0,nrev*360,int(nrev*32),     0.05,1.0,20,     0.05,1.0,20,  offset = 0.15, n_front_cap = 3, n_back_cap =3)
    export_polydata_to_stl(coil2,'coil2.stl')
    render(coil2)

# Example#9 sweep a curve along a spline
#   B-spline curve approximation of a helix (spiral/spring).
    t = np.linspace(0,3*np.pi, 25) # generate 50 points which we will interpolate
    x = np.array([np.cos(t), np.sin(t), t])

    curve = cf.cubic_curve(x.T) # transpose input so x[i,:] is one (x,y,z)-interpolation point

    points = np.array([ [ 0.0 ,  -0.3 ,  0.0  ],
                        [ 0.8 ,  -0.3 ,  0.0  ],
                        [ 0.8 ,   0.0 ,  0.0  ],
                        [ 0.8 ,   0.5 ,  0.0  ],
                        [ 1.0 ,   0.5 ,  0.0  ],
                        [ 1.0 ,   0.0 ,  0.0  ],
                        [ 1.0 ,  -0.5 ,  0.0  ],
                        [ 0.0 ,  -0.5 ,  0.0  ],
                        [-1.0 ,  -0.5 ,  0.0  ],
                        [-1.0 ,   0.0 ,  0.0  ],
                        [-1.0 ,   0.5 ,  0.0  ],
                        [-0.8 ,   0.5 ,  0.0  ],
                        [-0.8 ,   0.0 ,  0.0  ],
                        [-0.8 ,  -0.3 ,  0.0  ],
                        [ 0.0 ,  -0.3 ,  0.0  ] ])

    track = sweep(points,curve,5)
    export_polydata_to_stl(track,'track.stl')
    render(track)


def cylinder(radius,height,theta_res=32,n_height=20,n_bottom_cap=0,n_top_cap=0,theta_start=0.0,theta_end=360.0):
    """Creates a full or partial cylinder.
    The cylinder is oriented in the y-direction.
    The origin (bottom point) of the cylinder is located at x=0,y=0, z=0.
    The optional bottom and top caps are fans.
    The cylinder is build by defining line segments and revolving them around the y-axis
    Arguments:
        radius       : Cylinder radius
        height       : Cylinder height
        theta_res    : Resolution along the angular (theta) direction, default = 32
        n_height     : Resolution along the axial (height) direction,  default = 20
        n_bottom_cap : Resolution along the bottom cap, (set to 0 to have no bottom cap), default = 0
        n_top_cap    : Resolution along the top cap, (set to 0 to have no top cap), default = 0
        theta_start  : Starting angle (in degrees), default = 0.0
        theta_end    : Ending angle (in degrees), default = 360.0
    """

    segments = []
    # Bottom cap
    if n_bottom_cap>0:
        segments.append(['line',[0.0,0.0,0.0],[radius,0.0,0.0],n_bottom_cap])

    # Cylinder wall
    segments.append(['line',[radius,0.0,0.0],[radius,height,0.0],n_height])

    # Top cap
    if n_top_cap>0:
        segments.append(['line',[radius, height,0.0],[0.0, height,0.0],n_top_cap])


    return revolve_segments(segments, theta_start, theta_end, theta_res, offset=0.0)





def circle_xy(center,radius,theta_start, theta_end, theta_res):
    """  Returns a list of x and a list of y coordinates representing a full or partial circle.
    The cylinder is along the xy plane.
    Arguments:
        center       : Circle's center [x,y,z]
        radius       : Circle's radius
        theta_start  : Starting angle (in degrees), default = 0.0
        theta_end    : Ending angle (in degrees), default = 360.0
        theta_res    : Resolution along the angular (theta) direction, default = 32
    """
    theta = np.linspace(theta_start, theta_end, theta_res+1) * math.pi / 180.0

    x = center[0] + radius*np.cos(theta)
    y = center[1] + radius*np.sin(theta)
    z = center[2] * np.ones(theta_res+1)

    circle =  np.array([ x,y,z ]).T

    return circle




def build_points_from_segments(segments):
    """ Builds a list of points from a list of segments.
    A segment can be:
      - a line: Two end points [x,y,z] and a resolution  (number of divisions)
                Example: ['line',[1.0,0.0,0.0], [1.0,1.0,0.0], 10]
      - an arc: Center [x,y,z], radius, starting angle (deg), ending angle (deg), and resolution (number of divisions)
                Example: ['arc',[1.0,0.0,0.0],0.1,0.0,90.0,16]
    """

# I am still not sure how to do the vstack here without creating a row at the beginning and deleting it a t the end
    points = np.zeros(3)
    for segment in segments:
       # To avoid duplicate points, the last point of each segment, except the last segment is removed from the list
        if segment!=segments[-1]:
            remove_last_point = True
        else:
            remove_last_point = False
        if segment[0]=='line':
            p0, p1, n  = segment[1:]
            line = np.linspace(p0,p1,n+1)
            if remove_last_point:
                line=np.delete(line,-1,axis=0)
            points = np.vstack([points,line])
        elif segment[0]=='arc':
            center, radius, theta1, theta2, n = segment[1:]
            arc = circle_xy(center,radius,theta_start=theta1, theta_end=theta2, theta_res=n)
            if remove_last_point:
                arc=np.delete(arc,-1,axis=0)
            points = np.vstack([points,arc])


    points = np.delete(points,0,axis=0)

    return points

def revolve_segments(segments, theta_start=0.0, theta_end=360.0,theta_res=32,offset=0.0):
    """ Revolves a list of segments around the y-axis
    The offset value (per revolution) translates the geometry by "offset" in the y-direction every revolution.
    This can be used to generate a coil, when the base points represent a circle.
    Arguments:
        segments     : List of segments
        theta_start  : Starting angle (in degrees), default = 0.0
        theta_end    : Ending angle (in degrees), default = 360.0
        theta_res    : Resolution along the angular (theta) direction, default = 32
        offset       : Translation in the y-direction, per revolution, default= 0.0
    """

    points = build_points_from_segments(segments)

# This returns the VTK Polydat object
    return revolve_points(points, theta_start, theta_end, theta_res, offset=0.0)


def revolve_points_xyz(points, theta_start=0.0, theta_end=360.0,theta_res=32,offset=0.0):
    """ Revolves a list of points (x and y coordinates) around the y-axis
    The offset value (per revolution) translates the geometry by "offset" in the y-direction every revolution.
    This can be used to generate a coil, when the base points represent a circle.
    Arguments:
        points       : Array of points, each point is a [x,y,z] array
        theta_start  : Starting angle (in degrees), default = 0.0
        theta_end    : Ending angle (in degrees), default = 360.0
        theta_res    : Resolution along the angular (theta) direction, default = 32
        offset       : Translation in the y-direction, per revolution, default = 0.0
    """

    npts= len(points)

    rpoints_shape = (npts, theta_res)

    theta   = np.linspace(theta_start, theta_end, theta_res) * math.pi / 180.0
    delta_y = theta*offset/(2.0*math.pi)

# I am still not sure how to do the vstack here without creating a row at the beginning and deleting it a t the end
    rpoints = np.zeros(3)
    for point in points:
        r = np.vstack([point[0]*np.cos(theta),  point[1] + delta_y, point[0]*np.sin(theta)]).T
        rpoints = np.vstack([rpoints,r])

    rpoints = np.delete(rpoints,0,0)

    xyz = [rpoints_shape, rpoints]

# Returns a list: shape and array of points. The shape will be  used to generate a structured mesh.
    return xyz

def revolve_points(points, theta_start=0.0, theta_end=360.0,theta_res=32,offset=0.0):
    """ Revolves a list of points (x and y coordinates) around the y-axis
    The offset value (per revolution) translates the geometry by "offset" in the y-direction every revolution.
    This can be used to generate a coil, when the base points represent a circle.
    Arguments:
        points       : Array of points, each point is a [x,y,z] array
        Theta_start  : Starting angle (in degrees), default = 0.0
        Theta_end    : Ending angle (in degrees), default = 360.0
        Theta_res    : Resolution along the angular (theta) direction, default = 32
        offset       : Translation in the y-direction, per revolution, default = 0.0
    """

    xyz = revolve_points_xyz(points, theta_start, theta_end,theta_res,offset)

# This returns the VTK Polydat object
    return make_polydata(xyz)

#        Torus                               Front cone   Back cone    Circumference
def bend(rb1,rb2,theta1,theta2,bend_res,     r1,l1,n1,    r2,l2,n2,    theta_start=0, theta_end=360,theta_res=32, offset=0.0, n_front_cap=0, n_back_cap=0):
    """ Generates a bend, with optional front and back conical frustrums, and optional front and back caps.
    The bend is made up of three sections:
       - Torus section: Major radius, minor radius, starting angle (deg), ending angle (deg), number of divisions
       - Front conical frustrum: front radius, length, number of divisions
       - Back conical frustrum: back radius, length, number of divisions
    Note: to get front and back cylinders, use the same values for the front and back radii as the torus minor radius
          The offset value (per revolution) translates the geometry by "offset" in the y-direction every revolution.
          This can be used to generate a coil, when the base points represent a circle.
    Arguments:
        rB1          : Major radius
        rB2          : Minor radius
        theta1       : Bend starting angle
        theta2       : Bend ending angle
        bend_res     : Bend resolution (number of divisions
        r1           : Front radius
        l1           : Length of the front section
        n1           : Number of division along the front section
        r2           : Back radius
        l2           : Length of the back section
        n2           : Number of division along the back section
        The last optional argument control the cross section:
        theta_start  : Starting angle (in degrees), default = 0.0
        theta_end    : Ending angle (in degrees), default = 360.0
        theta_res    : Resolution along the angular (theta) direction, default = 32
    """


# Bend section (section of a torus)
    bend_circle  = circle_xy([rb1,0.0,0.0],rb2,theta_start = theta_start, theta_end = theta_end, theta_res = theta_res)
    xyz_bend = revolve_points_xyz(bend_circle , theta_start=theta1, theta_end=theta2 ,theta_res=bend_res,offset=offset)
    bend_section = [(theta_res + 1, bend_res), xyz_bend[1]]
    bend = [bend_section]


# Front section
    if n1>0:
# Create the front, and bend circles
        front_circle = circle_xy([rb1,0.0,0.0],r1,theta_start  = theta_start, theta_end = theta_end, theta_res = theta_res)
        bend_circle  = circle_xy([rb1,0.0,0.0],rb2,theta_start = theta_start, theta_end = theta_end, theta_res = theta_res)

# Rotate front and first bend circles to delimit the front section
        rfront_circle = rotate_and_translate(front_circle,theta1,-l1)
        rbend_circle  = rotate_and_translate(bend_circle,theta1,0.0)

# Connect them together to create the front section (conical frustrum)
        front_section = [(theta_res + 1, n1 + 1), connect(rfront_circle,rbend_circle,n1)]

        bend.append(front_section)


# Back section
    if n2>0:
# Create the bend and back circles
        y_offset = offset*(theta2-theta1)/360.0
        bend_circle  = circle_xy([rb1,y_offset,0.0],rb2,theta_start = theta_start, theta_end = theta_end, theta_res = theta_res)
        back_circle  = circle_xy([rb1,y_offset,0.0],r2,theta_start  = theta_start, theta_end = theta_end, theta_res = theta_res)

# Rotate back and last bend circles to delimit the back section
        rback_circle = rotate_and_translate(back_circle,theta2,l2)
        rbend_circle = rotate_and_translate(bend_circle,theta2,0.0)

# Connect them together to create the back section (conical frustrum)
        back_section = [(theta_res + 1, n2 + 1), connect(rbend_circle, rback_circle,n2)]

        bend.append(back_section)

# Front cap (fan)
    if n_front_cap>0:
# Create the front cap center circle (collapsed points) and front circle
        front_cap_center = circle_xy([rb1,0.0,0.0],0.0,theta_start=theta_start, theta_end=theta_end, theta_res=theta_res)
        front_circle = circle_xy([rb1,0.0,0.0],r1,theta_start  = theta_start, theta_end = theta_end, theta_res = theta_res)

# Rotate front cap center and front circle
        rfront_cap_center = rotate_and_translate(front_cap_center,theta1,-l1)
        rfront_circle = rotate_and_translate(front_circle,theta1,-l1)

# Connect them together to create the front cap fan
        front_cap = [(theta_res+1,n_front_cap+1),connect(rfront_cap_center,rfront_circle,n_front_cap)]
        bend.append(front_cap)

# Back cap (fan)
    if n_back_cap>0:
# Create the back cap center circle (collapsed points) and back circle
        y_offset = offset*(theta2-theta1)/360.0
        back_cap_center = circle_xy([rb1,y_offset,0.0],0.0,theta_start=theta_start, theta_end=theta_end, theta_res=theta_res)
        back_circle  = circle_xy([rb1,y_offset,0.0],r2,theta_start  = theta_start, theta_end = theta_end, theta_res = theta_res)

# Rotate back cap center and back circle
        rback_cap_center = rotate_and_translate(back_cap_center,theta2,l2)
        rback_circle = rotate_and_translate(back_circle,theta2,l2)

# Connect them together to create the back cap fan
        back_cap = [(theta_res+1,n_back_cap+1),connect(rback_circle,rback_cap_center,n_back_cap)]
        bend.append(back_cap)

    return make_polydata(*bend)


def connect(points_a, points_b,n):
    """ Connect two sets of points with n linear segments
        The two sets must have the same number of points
    Arguments:
        points_a     : First set of points
        points_b     : Second set of points
        n            : Number of divisions between the two sets of points
    """

    return np.vstack([np.linspace(a,b,n+1) for (a,b) in zip(points_a, points_b)])


def rotate_and_translate(points,theta,l):
    """ Rotates a 2D set of points (x,y) around the y-axis. After rotation, the set of points is
        translated in the Theta direction.
    Arguments:
        points       : Array of points, each point is a [x,y,z] array
        theta        : Angle of rotation (deg)
        l            : Translation distance
    """

    theta = theta /180.0*math.pi
    c, s = math.cos(theta), math.sin(theta)

# Rotation matrix
    M = np.array(((c, 0, 0),
                  (0, 1, 0),
                  (s, 0, 0)))


    rpoints = points @ M.T  + [-l*s, 0, l*c]

    return rpoints


def circle_to_rectangle(radius,w,nw,h,nh,l,n,x_offset=0, y_offset=0, n_front_cap=0, back_cap=False):
    """ Connects a circle to a rectangle to make a tube.
        The circle is centered at x=0, y=0, z=0.
        translated in the Theta direction.
    Arguments:
        radius       : Circles's radius
        w            : Rectangle's width (z-direction)
        nw           : number of divisions along the rectangle's width
        h            : Rectangle height (y-direction)
        nh           : number of divisions along the rectangle's height
        l            : Distance between the circle and rectangle in z-direction
        n            : number of divisions between the circle and rectangle in x-direction
        x_offset     : Distance between the circle and rectangle in x-direction, default = 0
        y_offset     : Distance between the circle and rectangle in y-direction, default = 0
        n_front_cap  : Number of divisions in the radial direction for the front cap (circle)
        back_cap     : Flag to add a back cap (rectangle). The number of disisions is automatically set by nw and nh
    """

# Define circle points
    front_circle = circle_xy([0.0,0.0,0.0],radius,theta_start=45, theta_end=360+45, theta_res=2*(nw+nh))

# Define rectangle points
    A = [ 0.5*w + x_offset, 0.5*h + y_offset, l]
    B = [-0.5*w + x_offset, 0.5*h + y_offset, l]
    C = [-0.5*w + x_offset,-0.5*h + y_offset, l]
    D = [ 0.5*w + x_offset,-0.5*h + y_offset, l]

    rect_segments = [['line',A,B,nw], ['line',B,C,nh], ['line',C,D,nw], ['line',D,A,nh]]
    back_rectangle = build_points_from_segments(rect_segments)

# Connect them together to create a conical frustrum
    c2r = [(2*(nw+nh)+1,n+1), connect(front_circle, back_rectangle,n)]

    circle_to_rectangle = [c2r]

# Front cap (fan)
    if n_front_cap>0:
# Create the front cap center circle (collapsed points) and front circle
        front_cap_center = circle_xy([0.0,0.0,0.0],0.0,theta_start=45, theta_end=360+45, theta_res=2*(nw+nh))
        front_circle = circle_xy([0.0,0.0,0.0],radius,theta_start=45, theta_end=360+45, theta_res=2*(nw+nh))

# Connect them together to create the front cap fan
        front_cap = [(2*(nw+nh)+1,n_front_cap+1),connect(front_cap_center,front_circle,n_front_cap)]
        circle_to_rectangle.append(front_cap)


# back cap (rectangular mesh)
    if back_cap:

        CD = build_points_from_segments([['line',C,D,nw]])
        BA = build_points_from_segments([['line',B,A,nw]])

        back_cap = [(nw+1,nh+1), connect(CD,BA,nh)]
        circle_to_rectangle.append(back_cap)


    return make_polydata(*circle_to_rectangle)


def sweep(points,curve,n):
    """ Sweeps a set of points along a curve.
    Arguments:
        points       : Array of points, each point is a [x,y,z] array
        curve        : Curve along which the points are swept
        n            : number of divisions between each curve's knots
    """

# It is assumed the input points lie in the xy plane
    base_vect = np.array([0.0, 0.0, 1.0])

    n_pts = len(points)


# Discretize curve between knots (n points between consecutive knots)
    k=curve.knots()[0]

    tp = np.array([])
    for s,e in zip(k, k[1:]):
        tp = np.append(tp,np.linspace(s,e,n))
    tp=np.unique(tp)
    n_sweep = len(tp)

    spoints = np.zeros(3)

    for point in points:

        for t in tp:
            # Normalized tangential vector
            tangent = curve.derivative(t, d=1)
            tnorm = np.linalg.norm(tangent)

            # Rotation angle between the base set of points and the tangent
            # Need to avoid singularity if tangent is parallel with base vector
            rot_vect = np.cross(base_vect,tangent)
            knorm = np.linalg.norm(rot_vect)
            sin_t = knorm
            cos_t = np.dot(base_vect, tangent) / tnorm if tnorm>0 else 1.0

            k = rot_vect / knorm if knorm > 0 else rot_vect

            #Theta = vg.angle(base_vect,tangent) /180.0*np.pi
            #cos_t = np.cos(Theta)
            #sin_t = np.sin(Theta)

# Rodrigues' rotation formula
            prot = point*cos_t + np.cross(k,point)*sin_t + k*np.dot(k,point)*(1.0-cos_t)

# Translate rotated points o they follow the curve
            c=curve.evaluate(t)
            spoints = np.vstack([spoints,c + prot])

    spoints = np.delete(spoints,0,0)

    xyz = [(n_pts,n_sweep), spoints]

    return make_polydata(xyz)



def make_polydata(*xyz_arg):
    """ Converts any number of xyz list into a VTK PolyData.
        An element of xyz_arg is a list of:
           - shape (imax,jmax) of the structured mesh organization of the points.
           - Array of points, each points is an array of coordinates [x , y, z]
    Arguments:
        xyz_arg       : One or several list of points (shape + array of coordinates)
    """

    Points    = vtk.vtkPoints()
    Triangles = vtk.vtkCellArray()
    PolyData  = vtk.vtkPolyData()
    ijk_start = 0

    for xyz in xyz_arg:

# Mesh size
        imax, jmax = xyz[0]

# Set points
        for point in xyz[1]:
            Points.InsertNextPoint(point)

# Set triangles
        for i in range(imax - 1):
            for j in range(jmax - 1):

# Each quad is split into two triangles
# Quad vertices q = [ijk , ijk + jmax, ijk + jmax + 1, ijk + 1], where ijk=jmax*i+j
# Triangles connectivity
                c = ijk_start + jmax*i + j + np.array([ np.array([0,    jmax,     1]),
                                                        np.array([jmax, jmax + 1, 1]) ])

                for t in range(0,2):
                    Triangle = vtk.vtkTriangle()

                    for n in range(0,3):
                        Triangle.GetPointIds().SetId(n, c[t,n])
# Add triangles
                    Triangles.InsertNextCell(Triangle)


# Connectivity starting point for next xyz
        ijk_start += imax*jmax

    PolyData.SetPoints(Points)
    PolyData.SetPolys(Triangles)

# Remove duplicate points.
    cleanFilter = vtk.vtkCleanPolyData()
    cleanFilter.SetInputData(PolyData)
    cleanFilter.Update()

# Apply triangle filter
    triangleFilter = vtk.vtkTriangleFilter()
    triangleFilter.SetInputData(cleanFilter.GetOutput())
    triangleFilter.PassLinesOff()
    triangleFilter.PassVertsOff()
    triangleFilter.Update()

    return triangleFilter.GetOutput()

def export_polydata_to_stl(PolyData, filename, format='binary'):
    """ Export PolyData into an STL file
        PolyData     : Polydata to be exported
        filename     : STL file name
        format       : STL file format (ascii or binary), default = 'binary'
    """
    Writer =  vtk.vtkSTLWriter();
    if format=='binary':
       Writer.SetFileTypeToBinary();
    else:
       Writer.SetFileTypeToASCII()
    Writer.SetInputData(PolyData);
    Writer.SetFileName(filename);
    Writer.Update();
    Writer.Write();


def render(PolyData):
    """ Render a PolyData into render window
        PolyData     : Polydata to be rendered
    """
    MeshMapper = vtk.vtkPolyDataMapper()
    MeshMapper.SetInputData(PolyData)
# Edges
    EdgeActor = vtk.vtkActor()
    EdgeActor.SetMapper(MeshMapper)
    EdgeActor.GetProperty().SetRepresentationToWireframe()
    EdgeActor.GetProperty().SetColor(0,0,0)
    EdgeActor.GetProperty().SetLineWidth(2.0)
# Surface
    SurfActor = vtk.vtkActor()
    SurfActor.SetMapper(MeshMapper)
    SurfActor.GetProperty().SetInterpolationToFlat()
    SurfActor.GetProperty().SetRepresentationToSurface()

    Renderer = vtk.vtkRenderer()
    RenderWindow = vtk.vtkRenderWindow()
    RenderWindow.SetSize(1200,800)
    RenderWindow.AddRenderer(Renderer)
    RenderWindowInteractor = vtk.vtkRenderWindowInteractor()
    RenderWindowInteractor.SetRenderWindow(RenderWindow)
    Renderer.AddActor(EdgeActor)
    Renderer.AddActor(SurfActor)
    Renderer.SetBackground(.3, .6, .3)

    RenderWindowInteractor.Initialize()
    RenderWindow.Render()
    RenderWindowInteractor.Start()   # Press 'q' to exit


if __name__ == '__main__':
    main()
