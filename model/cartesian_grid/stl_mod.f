      MODULE stl

      Use param1, only: zero
      Use param, only: dimension_bc

!     Maximum number of facets that can be read
      INTEGER, PARAMETER          :: DIM_STL = 10000000   !10 Million
!     Number of facets
      INTEGER                     :: N_FACETS
      INTEGER, PARAMETER :: WEST_FACEID = 9000000
      INTEGER, PARAMETER :: EAST_FACEID = 9000001
      INTEGER, PARAMETER :: SOUTH_FACEID = 9000002
      INTEGER, PARAMETER :: NORTH_FACEID = 9000003
      INTEGER, PARAMETER :: BOTTOM_FACEID = 9000004
      INTEGER, PARAMETER :: TOP_FACEID = 9000005

!     Number of facets for des. This could be a diiferent number from
! N_FACETS if the outer boundary is triangulated here
      INTEGER                     :: N_FACETS_DES
!     Vertex Coordinates X ,Y and Z
      DOUBLE PRECISION, DIMENSION(3,3,DIM_STL) :: VERTEX
!     Face normal vector (normalized)
      DOUBLE PRECISION, DIMENSION(3,DIM_STL) :: NORM_FACE
!     Flag to identify facets that are located along the MFiX domain box
      LOGICAL, DIMENSION(DIM_STL) :: STL_FACET_ALONG_BOX
!     TRANSLATION COMPONENTS
      DOUBLE PRECISION :: TX_STL,TY_STL,TZ_STL
      DOUBLE PRECISION :: TX_MSH,TY_MSH,TZ_MSH
!     SCALING FACTOR
      DOUBLE PRECISION :: SCALE_STL
      DOUBLE PRECISION :: SCALE_MSH
!     RANGE OF STL FILE:
      DOUBLE PRECISION :: XMIN_STL,XMAX_STL
      DOUBLE PRECISION :: YMIN_STL,YMAX_STL
      DOUBLE PRECISION :: ZMIN_STL,ZMAX_STL
      DOUBLE PRECISION :: XMIN_MSH,XMAX_MSH
      DOUBLE PRECISION :: YMIN_MSH,YMAX_MSH
      DOUBLE PRECISION :: ZMIN_MSH,ZMAX_MSH
!     VALUE OF F_STL OUTSIDE OF STL BOUNDING BOX
      DOUBLE PRECISION :: OUT_STL_VALUE
      DOUBLE PRECISION :: OUT_MSH_VALUE
      LOGICAL, DIMENSION(DIMENSION_BC) :: FLIP_STL_NORMALS
!     SMALLEST ANGLE FOR DETECTION OF SMALL TRIANGLES
      DOUBLE PRECISION :: STL_SMALL_ANGLE
      DOUBLE PRECISION :: MSH_SMALL_ANGLE
!     DIRECTION OF RAY TRACED TO DETERMINE WHETHER A POINT IS INSIDE/OUTSIDE
      CHARACTER(LEN=3) :: RAY_DIR
!     Tolerance for polygon edge detection
      DOUBLE PRECISION :: TOL_STL
      DOUBLE PRECISION :: TOL_MSH
      DOUBLE PRECISION :: TOL_STL_DP
!     Boundary condition ID
      INTEGER :: STL_BC_ID

      INTEGER, DIMENSION(DIM_STL) :: BC_ID_STL_FACE

! Flag to ignore edge (including point) intersection (needed when angle
! with neighbor facet is below a tolerance
      LOGICAL, DIMENSION(DIM_STL) :: IGNORE_EDGE_INTERSECTION

! Tolerance, expressed as an angle in degrees. When the angle between
! two adjacent STL facets is below this angle, the collision between
! particles and the facet angles will be ignored
      DOUBLE PRECISION, PARAMETER :: STL_NB_ANGLE = 5.0D0

! Cosine of the above angle
      DOUBLE PRECISION :: COS_STL_NB_ANGLE

!     Maximum number of facets per cell. The arrays below are used
! to define cut-cells under the CG modules
      INTEGER          :: DIM_FACETS_PER_CELL
      INTEGER, DIMENSION (:), ALLOCATABLE ::  N_FACET_AT
      INTEGER, DIMENSION (:,:), ALLOCATABLE ::  LIST_FACET_AT

! List of facets intersecting each DES grid cell
      TYPE FACETS_TO_DG
         INTEGER :: COUNT
         INTEGER, ALLOCATABLE :: ID(:)
         INTEGER, ALLOCATABLE :: DIR(:)
         DOUBLE PRECISION, ALLOCATABLE :: MIN(:)
         DOUBLE PRECISION, ALLOCATABLE :: MAX(:)
      END TYPE FACETS_TO_DG

! Start/End positions for the different kinds of STLs
      INTEGER, PARAMETER :: ALL_STL = 0
      INTEGER, PARAMETER :: BASE_STL = 1
      INTEGER, PARAMETER :: BCWALLS_STL = 2
      INTEGER, PARAMETER :: IMPRMBL_STL = 3
      INTEGER, PARAMETER :: DEFAULT_STL = 4
      INTEGER :: STL_START(4), STL_END(4)

      TYPE (FACETS_TO_DG), ALLOCATABLE ::  FACETS_AT_DG(:)
      CHARACTER(LEN=3) :: CAD_PROPAGATE_ORDER


! List of facets groups
      TYPE STL_GROUP_TYPE
! Group type: - BC_STL : stl file used as BC (geometry_####.stl)         
!             - IS_STL : stl file used as IS (is_####.stl)         
!             - BC_REG : regular bc (rectangular region)
!             - IS_REG : regular iinternal surface (rectangular region)
!             - BC_DEF : default bc (along MFiX box)
         CHARACTER(LEN=6) :: TYPE
! File name associated with BC_STL or IS_STL         
         CHARACTER(LEN=20) :: FILENAME
! Starting and endind indices of the STL facets         
         INTEGER :: START, END
! ID of the corresponding BC or IS
         INTEGER :: ID
! Convert STL file to VTU file         
         LOGICAL :: CONVERT_TO_VTU
      END TYPE STL_GROUP_TYPE

      INTEGER, PARAMETER          :: DIM_STL_GROUP = 100
      INTEGER :: N_STL_GROUP
      TYPE (STL_GROUP_TYPE) ::  STL_GROUP(DIM_STL_GROUP)

!     Initial Vertex Coordinates X ,Y and Z
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:) :: VERTEX_INIT
!     Initial Face normal vector (normalized)
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: NORM_FACE_INIT
      END MODULE stl
