# Constants
from collections import OrderedDict

# Solver types
# must match combobox_solver in model_setup.ui
SINGLE, TFM, DEM, CGP, PIC, HYBRID = range(6)
SOLVERS = [SINGLE, TFM, DEM, CGP, PIC, HYBRID]

# model types
CONSTANT, AIR, UDF = 0, 1, 2
VARIABLE = MIXTURE = OTHER = IDEAL = AIR  # continuum, etc

DRAG_TYPES = ['SYAM_OBRIEN', # (DEFAULT)
              'BVK',
              'TPKKV',
              'GIDASPOW',
              'GIDASPOW_BLEND',
              'GIDASPOW_PCF',
              'GIDASPOW_BLEND_PCF',
              'HYS',
              'KOCH_HILL',
              'KOCH_HILL_PCF',
              'WEN_YU',
              'WEN_YU_PCF',
              'GAO',
              'SARKAR',
              'RADL',
              'TGS',
              'DIFELICE',
              'DIFELICE_GANSER',
              'USER_DRAG']

DEFAULT_DRAG_TYPE = 'SYAM_OBRIEN'

DES_CONV_CORR_VALUES = ['RANZ_1952',
                        'WAKAO',
                        'GUNN',
                        'TAVASSOLI']
DEFAULT_DES_CONV_CORR = 'RANZ_1952'

TURBULENCE_MODELS = ['NONE', 'MIXING_LENGTH', 'K_EPSILON']
DEFAULT_TURBULENCE_MODEL = 'NONE'

SUBGRID_TYPES = ['NONE', 'IGCI', 'MILIOLI']
DEFAULT_SUBGRID_TYPE = 'NONE'

DEFAULT_SOLIDS_MODEL='TFM'

#PIC time step control
CFL_CONTROL_TYPES = ['MAX', 'AVG']

# Viscous stress model
KT_TYPES = ['ALGEBRAIC', 'LUN_1984', 'IA_NONEP', 'SIMONIN',
            'AHMADI', 'GD_99', 'GTSH', 'GHD']

DEFAULT_KT_TYPE = 'ALGEBRAIC'

FRICTION_MODELS = ['SCHAEFFER', 'SRIVASTAVA', 'GUO_BOYCE', 'NONE']
DEFAULT_FRICTION_MODEL = 'SCHAEFFER'

RDF_TYPES = ['CARNAHAN_STARLING', 'MA_AHMADI', 'LEBOWITZ',
             'MODIFIED_LEBOWITZ', 'MANSOORI', 'MODIFIED_MANSOORI']


BLENDING_FUNCTIONS = ['NONE', 'TANH_BLEND', 'SIGM_BLEND']
DEFAULT_BLENDING_FUNCTION = 'NONE'

KS_MODELS = ['NONE', 'CONST_EFF', 'USR', 'BAUER', 'MUSSER']

BC_TYPES = ['MI', 'PO', 'NSW', 'FSW', 'PSW', 'PSW', #second PSW is "Mixed Wall"
            'PI', 'MO', 'CYCLIC'] # 'CYCLIC' is not really a bc_type

BC_NAMES = ['Mass inflow', 'Pressure outflow', 'No-slip wall',
            'Free-slip wall', 'Partial slip wall', 'Mixed wall',
            'Pressure inflow', 'Mass outflow',
            'Cyclic Boundary']

(MASS_INFLOW, PRESSURE_OUTFLOW,
 NO_SLIP_WALL, FREE_SLIP_WALL, PARTIAL_SLIP_WALL, MIXED_WALL,
 PRESSURE_INFLOW, MASS_OUTFLOW, CYCLIC_BOUNDARY) = range(9)

(NO_FLUX, SPECIFIED_TEMPERATURE, SPECIFIED_FLUX, CONVECTIVE_FLUX) = range(4)

DEFAULT_BC_TYPE = 'NSW'

WALL_TYPES = ["Partial Slip", "No-Slip", "Free-Slip"]
PARTIAL_SLIP, NO_SLIP, FREE_SLIP = range(3)
DEFAULT_WALL_TYPE = PARTIAL_SLIP

IS_NAMES = ['Impermeable', 'X-Axis Impermeable', 'Y-Axis Impermeable', 'Z-Axis Impermeable',
            'Semi-permeable', 'X-Axis semi-permeable','Y-Axis semi-permeable','Z-Axis semi-permeable',
            'Moveable (STL)']

IS_TYPES = ['IMPERMEABLE', 'X_IMPERMEABLE', 'Y_IMPERMEABLE', 'Z_IMPERMEABLE',
            'SEMIPERMEABLE', 'X_SEMIPERMEABLE', 'Y_SEMIPERMEABLE', 'Z_SEMIPERMEABLE', 'STL']

(IMPERMEABLE, X_IMPERMEABLE, Y_IMPERMEABLE, Z_IMPERMEABLE,
 SEMIPERMEABLE, X_SEMIPERMEABLE, Y_SEMIPERMEABLE, Z_SEMIPERMEABLE, MOVEABLE) = range(9)

DEFAULT_IS_TYPE = 'IMPERMEABLE'

MONITOR_TYPE_CELL_NAMES = ['Value',
                           'Sum',
                           'Min',
                           'Max',
                           'Average',
                           'Standard deviation',
                           'Area-weighted average',
                           'Flow rate',
                           'Mass flow rate',
                           'Mass-weighted average (2D)',
                           'Volume flow rate',
                           'Volume integral',
                           'Volume-weighted average',
                           'Mass-weighted integral',
                           'Mass-weighted average (3D)']

MONITOR_TYPES_CELL  = (MONITOR_CELL_VALUE,
                       MONITOR_CELL_SUM,
                       MONITOR_CELL_MIN,
                       MONITOR_CELL_MAX,
                       MONITOR_CELL_AVERAGE,
                       MONITOR_CELL_STANDARD_DEVIATION,
                       MONITOR_CELL_AREA_WEIGHTED_AVERAGE,
                       MONITOR_CELL_FLOW_RATE,
                       MONITOR_CELL_MASS_FLOW_RATE,
                       MONITOR_CELL_MASS_WEIGHTED_AVERAGE_SURFACE,
                       MONITOR_CELL_VOLUME_FLOW_RATE,
                       MONITOR_CELL_VOLUME_INTEGRAL,
                       MONITOR_CELL_VOLUME_WEIGHTED_AVERAGE,
                       MONITOR_CELL_MASS_WEIGHTED_INTEGRAL,
                       MONITOR_CELL_MASS_WEIGHTED_AVERAGE_VOLUME) = range(15)


MONITOR_TYPES_CELL_POINT = [MONITOR_CELL_VALUE]

MONITOR_TYPES_CELL_PLANE = [MONITOR_CELL_SUM,
                            MONITOR_CELL_MIN,
                            MONITOR_CELL_MAX,
                            MONITOR_CELL_AVERAGE,
                            MONITOR_CELL_STANDARD_DEVIATION,
                            MONITOR_CELL_AREA_WEIGHTED_AVERAGE,
                            MONITOR_CELL_FLOW_RATE,
                            MONITOR_CELL_MASS_FLOW_RATE,
                            MONITOR_CELL_MASS_WEIGHTED_AVERAGE_SURFACE,
                            MONITOR_CELL_VOLUME_FLOW_RATE]

MONITOR_TYPES_CELL_VOLUME = [MONITOR_CELL_SUM,
                             MONITOR_CELL_MIN,
                             MONITOR_CELL_MAX,
                             MONITOR_CELL_AVERAGE,
                             MONITOR_CELL_STANDARD_DEVIATION,
                             MONITOR_CELL_VOLUME_INTEGRAL,
                             MONITOR_CELL_VOLUME_WEIGHTED_AVERAGE,
                             MONITOR_CELL_MASS_WEIGHTED_INTEGRAL,
                             MONITOR_CELL_MASS_WEIGHTED_AVERAGE_VOLUME]

MONITOR_TYPE_PARTICLE_NAMES = ['Sum',
                               'Min',
                               'Max',
                               'Average',
                               'Standard deviation',
                               'Mass-weighted average',
                               'Volume-weighted average',
                               'Mass flow rate',
                               'Mass-weighted flow rate',
                               'Volume-weighted flow rate']


MONITOR_TYPES_PARTICLE = [MONITOR_PARTICLE_SUM,
                          MONITOR_PARTICLE_MIN,
                          MONITOR_PARTICLE_MAX,
                          MONITOR_PARTICLE_AVERAGE,
                          MONITOR_PARTICLE_STANDARD_DEVIATION,
                          MONITOR_PARTICLE_MASS_WEIGHTED_AVERAGE,
                          MONITOR_PARTICLE_VOLUME_WEIGHTED_AVERAGE,
                          MONITOR_PARTICLE_MASS_FLOW_RATE,
                          MONITOR_PARICLE_MASS_WEIGHTED_FLOW_RATE,
                          MONITOR_PARICLE_VOLUME_WEIGHTED_FLOW_RATE] = range(101, 111)


MONITOR_TYPES_PARTICLE_POINT = [] # None
MONITOR_TYPES_PARTICLE_PLANE = MONITOR_TYPES_PARTICLE # All types valid for planes
MONITOR_TYPES_PARTICLE_VOLUME = [MONITOR_PARTICLE_SUM,
                                 MONITOR_PARTICLE_MIN,
                                 MONITOR_PARTICLE_MAX,
                                 MONITOR_PARTICLE_AVERAGE,
                                 MONITOR_PARTICLE_STANDARD_DEVIATION,
                                 MONITOR_PARTICLE_MASS_WEIGHTED_AVERAGE,
                                 MONITOR_PARTICLE_VOLUME_WEIGHTED_AVERAGE]

# ./model/param_mod.f:67:      INTEGER, PARAMETER :: DIM_M = 10 # max # of solids phases
DIM_M = 10
#model/param_mod.f:      INTEGER, PARAMETER :: DIM_EQS = 10
DIM_EQS = 10

PRECON_TYPES = ['NONE', 'LINE', 'DIAG']
SWEEP_TYPES = ['RSRS', 'ASAS', 'ISIS', 'JSJS', 'KSKS']

DES_OUTPUT_TYPES = ['PARAVIEW', 'TECPLOT']

CONVERSION_TO_METERS = OrderedDict([
    ('km',     1000.0),
    ('m',      1.0),
    ('cm',     0.01),
    ('mm',     0.001),
    ('um',     1e-6),
    ('mile',   1609.34),
    ('yard',   0.9144),
    ('ft',     0.3048),
    ('ins',    0.0254),
])

SPECIAL_PARAMETERS = [a+b
                      for a in ('', 'x', 'y', 'z')
                      for b in ('min', 'max')]

PARAMETER_DICT = OrderedDict([(key, 0.0) for key in SPECIAL_PARAMETERS])

MAX_RECENT_PROJECTS = 20

# these are fnmatch and glob patterns to find output files
# Note, we apply them in case-insensitive manner
RESTART_FILES = ['*.res', '*.msh'] # Should .msh be here?
SPX_FILES = ['*.sp?']
VTK_FILES = ['*.vtp', '*.vtu', '*.pvtp', '*.pvtu', '*.pvd', '*_boundary.vtk',
             '*_frame_index.txt']
MONITOR_FILES = ['*.csv', '*.txt']
OTHER_FILES = ['*.out', '*.log', '*.pid', '*.env',
               '*.error', '*.e[0-9]*', '*.o[0-9]*',
               '*.pe[0-9]*', '*.po[0-9]*',
               'mfix.stop', '*_facets_read.stl']
