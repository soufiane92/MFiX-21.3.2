MODULE cutcell

   USE param
   USE progress_bar
   USE stl

!     CUT_CELL.LOG unit number
   INTEGER  UNIT_CUT_CELL_LOG
   PARAMETER (UNIT_CUT_CELL_LOG = 111)

!     Flag to activate Cartesian grid

   LOGICAL :: CARTESIAN_GRID

!     Flag to activate cell re-indexing
   LOGICAL :: RE_INDEXING

!     Flag to activate adjustment of each processor domain size
   LOGICAL :: ADJUST_PROC_DOMAIN_SIZE

!     Flag to report best domain size on each processor and quit
   LOGICAL :: REPORT_BEST_DOMAIN_SIZE

!     maximum number of cut cells
   INTEGER :: DIMENSION_MAX_CUT_CELL

!     Factor used to allocate cut cells arrays
   DOUBLE PRECISION :: FAC_DIM_MAX_CUT_CELL

!     Flag to identify interior cells.
!     This flag is true for any cell within the range
!     istart1 <= i <= iend1 and so on for j and k.
!     Caution: it is local to each processor and this array
!     is not shared across processor boundaries.
!     For example, consider 1-D decomposition over two procs.
!     on each proc, INTERIOR_CELL_AT will be true for
!     istart1 <= i <= iend1. On processor 1, it will be false
!     at i=iend2 even though i=iend2 is an interior cell in the
!     global sense!
   LOGICAL, DIMENSION(:), ALLOCATABLE :: INTERIOR_CELL_AT

!     One-Dimensional Arrays for East, North, Top location of
!     original (uncut) scalar cells
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::XG_E
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::YG_N
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::ZG_T

!     location of U-momentum nodes
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_U
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_U
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_U

!     location of V-momentum nodes
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_V
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_V
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_V

!     location of W-momentum nodes
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_W
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_W
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_W

!     Intersection flags
   LOGICAL, DIMENSION(:), ALLOCATABLE :: INTERSECT_X
   LOGICAL, DIMENSION(:), ALLOCATABLE :: INTERSECT_Y
   LOGICAL, DIMENSION(:), ALLOCATABLE :: INTERSECT_Z

!     Location of intersections
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_int
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_int
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_int

!     Location of original (uncut) corner cell nodes
   DOUBLE PRECISION, DIMENSION(0:15):: X_NODE
   DOUBLE PRECISION, DIMENSION(0:15):: Y_NODE
   DOUBLE PRECISION, DIMENSION(0:15):: Z_NODE
   DOUBLE PRECISION, DIMENSION(0:15):: F_NODE
   INTEGER, DIMENSION(0:15) :: IJK_OF_NODE

!     Location of new (along intersecting edges) nodes
   INTEGER :: NUMBER_OF_NEW_POINTS
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_NEW_POINT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_NEW_POINT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_NEW_POINT

!     Location of new (along intersecting edges) nodes
   INTEGER :: NUMBER_OF_NEW_U_POINTS
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_NEW_U_POINT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_NEW_U_POINT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_NEW_U_POINT

!     Location of new (along intersecting edges) nodes
   INTEGER :: NUMBER_OF_NEW_V_POINTS
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_NEW_V_POINT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_NEW_V_POINT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_NEW_V_POINT

!     Location of new (along intersecting edges) nodes
   INTEGER :: NUMBER_OF_NEW_W_POINTS
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_NEW_W_POINT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_NEW_W_POINT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_NEW_W_POINT

!     Number of nodes
   INTEGER, DIMENSION(:), ALLOCATABLE ::  NUMBER_OF_NODES
   INTEGER, DIMENSION(:), ALLOCATABLE ::  NUMBER_OF_U_NODES
   INTEGER, DIMENSION(:), ALLOCATABLE ::  NUMBER_OF_V_NODES
   INTEGER, DIMENSION(:), ALLOCATABLE ::  NUMBER_OF_W_NODES

!     Connectivity
   INTEGER, DIMENSION(:,:), ALLOCATABLE :: CONNECTIVITY
   INTEGER, DIMENSION(:,:), ALLOCATABLE :: CONNECTIVITY_U
   INTEGER, DIMENSION(:,:), ALLOCATABLE :: CONNECTIVITY_V
   INTEGER, DIMENSION(:,:), ALLOCATABLE :: CONNECTIVITY_W

!     Processor assign to cell IJK
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: PARTITION

!     Normal Vector Defining cut face in Scalar Cell
   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::  NORMAL_S

!     Reference point Defining cut face in Scalar Cell
   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::  REFP_S

!     Flags for Wall momentum cells
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  WALL_U_AT
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  WALL_V_AT
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  WALL_W_AT

!     Areas of cut faces
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Area_CUT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Area_U_CUT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Area_V_CUT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Area_W_CUT

!     Distances from cell center to face center
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELX_Ue
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELX_Uw
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELY_Un
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELY_Us
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELZ_Ut
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELZ_Ub

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELX_Ve
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELX_Vw
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELY_Vn
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELY_Vs
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELZ_Vt
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELZ_Vb

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELX_We
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELX_Ww
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELY_Wn
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELY_Ws
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELZ_Wt
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELZ_Wb

!     Location of face centers
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_U_ec
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_U_ec
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_U_ec

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_U_nc
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_U_nc
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_U_nc

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_U_tc
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_U_tc
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_U_tc

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_V_ec
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_V_ec
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_V_ec

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_V_nc
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_V_nc
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_V_nc

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_V_tc
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_V_tc
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_V_tc

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_W_ec
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_W_ec
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_W_ec

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_W_nc
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_W_nc
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_W_nc

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_W_tc
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_W_tc
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_W_tc

!     Distance to cut face in Scalar Cell
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELH_Scalar


!     Distance to cut face in U-Momentum Cell
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELH_U

!     Normal Vector Defining cut face in U-Momentum Cell
   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::  NORMAL_U

!     Reference point Defining cut face in W-Momentum Cell
   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::  REFP_U


!     Correction factors for U-Momentum Cell
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Ue
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Ue_bar

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_U_ne
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_U_nw

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_U_te
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_U_tw

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ALPHA_Ue_c
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  NOC_U_E

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Un
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Un_bar

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ALPHA_Un_c
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  NOC_U_N

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Ut
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Ut_bar

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ALPHA_Ut_c
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  NOC_U_T

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  A_UPG_E
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  A_UPG_W

!     Distance to cut face in V-Momentum Cell
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELH_V

!     Normal Vector Defining cut face in V-Momentum Cell
   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::  NORMAL_V

!     Reference point Defining cut face in V-Momentum Cell
   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::  REFP_V


!     Correction factors for V-Momentum Cell
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_V_ne
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_V_se

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Vn
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Vn_bar

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_V_nt
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_V_st

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Ve
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Ve_bar

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ALPHA_Ve_c
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  NOC_V_E

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ALPHA_Vn_c
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  NOC_V_N

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Vt
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Vt_bar

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ALPHA_Vt_c
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  NOC_V_T

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  A_VPG_N
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  A_VPG_S


!     Distance to cut face in W-Momentum Cell
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DELH_W

!     Normal Vector Defining cut face in W-Momentum Cell
   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::  NORMAL_W

!     Reference point Defining cut face in W-Momentum Cell
   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::  REFP_W


!     Correction factors for W-Momentum Cell
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_W_te
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_W_be

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_W_tn
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_W_bn

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Wt
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Wt_bar

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_We
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_We_bar

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ALPHA_We_c
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  NOC_W_E

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Wn
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Theta_Wn_bar

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ALPHA_Wn_c
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  NOC_W_N

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ALPHA_Wt_c
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  NOC_W_T

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  A_WPG_T
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  A_WPG_B

!     1/dx, 1/dy, 1/dz
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ONEoDX_E_U
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ONEoDY_N_U
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ONEoDZ_T_U


   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ONEoDX_E_V
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ONEoDY_N_V
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ONEoDZ_T_V

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ONEoDX_E_W
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ONEoDY_N_W
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  ONEoDZ_T_W



   LOGICAL, DIMENSION(:), ALLOCATABLE :: ALONG_DOMAIN_BOUNDARY

!     Location of intersection points
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Xn_int
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Xn_U_int
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Xn_V_int
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Xn_W_int

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Ye_int
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Ye_U_int
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Ye_V_int
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Ye_W_int

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Zt_int
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Zt_U_int
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Zt_V_int
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Zt_W_int

!     Cut cell treatment flags
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  CUT_TREATMENT_AT
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  CUT_U_TREATMENT_AT
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  CUT_V_TREATMENT_AT
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  CUT_W_TREATMENT_AT

!     Various cell flags
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  CUT_CELL_AT
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  CUT_U_CELL_AT
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  CUT_V_CELL_AT
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  CUT_W_CELL_AT

   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  SMALL_CELL_AT
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  BLOCKED_CELL_AT
   INTEGER,  DIMENSION(:), ALLOCATABLE ::  SMALL_CELL_FLAG

   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  BLOCKED_U_CELL_AT
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  BLOCKED_V_CELL_AT
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  BLOCKED_W_CELL_AT

   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  STANDARD_CELL_AT
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  STANDARD_U_CELL_AT
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  STANDARD_V_CELL_AT
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  STANDARD_W_CELL_AT

!     Tolerance for snapping procedure
   DOUBLE PRECISION, DIMENSION(3) :: TOL_SNAP, TOL_SNAP_BCK

!     Tolerances for wall distance
   DOUBLE PRECISION :: TOL_DELH

!     Tolerance for merging nodes
   DOUBLE PRECISION :: TOL_MERGE

!     Tolerance for detecting small scalar cells
   DOUBLE PRECISION :: TOL_SMALL_CELL
   DOUBLE PRECISION :: TOL_SMALL_AREA
!     Maximum value of ALPHA correction factor
   DOUBLE PRECISION :: ALPHA_MAX

!     Flags to include effect of cut cells
   LOGICAL :: NOC,NOC_UG,NOC_VG,NOC_WG,NOC_US,NOC_VS,NOC_WS,NOC_TRDG,NOC_TRDS
   LOGICAL :: CUT_TAU_UG,CUT_TAU_VG,CUT_TAU_WG,CUT_TAU_US,CUT_TAU_VS,CUT_TAU_WS

!     pressure gradient option flag
   INTEGER :: PG_OPTION

!     Number of cells
   INTEGER NUMBER_OF_U_CUT_CELLS
   INTEGER NUMBER_OF_V_CUT_CELLS
   INTEGER NUMBER_OF_W_CUT_CELLS
   INTEGER NUMBER_OF_SMALL_CELLS

   INTEGER NUMBER_OF_U_WALL_CELLS
   INTEGER NUMBER_OF_V_WALL_CELLS
   INTEGER NUMBER_OF_W_WALL_CELLS

!     Vorticity and lambda2
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  VORTICITY
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  LAMBDA2

!     Re-ordering array
   INTEGER, DIMENSION(15) :: ORDER

!     Snapping flag
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  SNAP
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  SNAP_SCALAR

   INTEGER, DIMENSION(10) :: CG_SAFE_MODE
   LOGICAL :: PRINT_WARNINGS
   LOGICAL :: SET_CORNER_CELLS

!     Master cell of wall cell (FSW)
   INTEGER,  DIMENSION(:), ALLOCATABLE ::  U_MASTER_OF
   INTEGER,  DIMENSION(:), ALLOCATABLE ::  V_MASTER_OF
   INTEGER,  DIMENSION(:), ALLOCATABLE ::  W_MASTER_OF

   INTEGER :: N_USR_DEF

   LOGICAL :: USE_POLYGON

   LOGICAL :: USE_STL

   LOGICAL :: USE_MSH

!     Boundary condition flag
   INTEGER,  DIMENSION(:), ALLOCATABLE ::  BC_ID
   INTEGER,  DIMENSION(:), ALLOCATABLE ::  BC_U_ID
   INTEGER,  DIMENSION(:), ALLOCATABLE ::  BC_V_ID
   INTEGER,  DIMENSION(:), ALLOCATABLE ::  BC_W_ID

   INTEGER :: NSW_GHOST_BC_ID

!     Underrelaxation flag applied to cut cells
   DOUBLE PRECISION, DIMENSION(9):: CG_UR_FAC

!     Debugging_variables
   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::DEBUG_CG

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::U_g_CC
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::V_g_CC
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::W_g_CC

   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::U_s_CC
   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::V_s_CC
   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::W_s_CC

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  TRD_G_OUT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  PP_G_OUT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  EPP_OUT

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  dudx_OUT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  dvdy_OUT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  delv_OUT

   LOGICAL :: PARTIAL_CHECK_03

!     Keep track of whether the CG Hader was printed on screen
   LOGICAL :: CG_HEADER_WAS_PRINTED = .FALSE.

   LOGICAL, DIMENSION(:), ALLOCATABLE ::SCALAR_NODE_ATWALL

   DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::SCALAR_NODE_XYZ

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::Ovol_around_node

!     Snapping flag
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  POTENTIAL_CUT_CELL_AT

   INTEGER, PARAMETER :: MAX_CP = 100

   INTEGER                               ::  NCPX, NCPY, NCPZ ! Number of control points
   DOUBLE PRECISION, DIMENSION(0:MAX_CP) ::  CPX,CPY,CPZ  ! Control point location
   DOUBLE PRECISION, DIMENSION(MAX_CP)   ::  ERX,ERY,ERZ  ! Expansion Ratio
   INTEGER, DIMENSION(MAX_CP)            ::  NCX,NCY,NCZ  ! Number of cell in a segment
   DOUBLE PRECISION, DIMENSION(MAX_CP)   ::  FIRST_DX,LAST_DX! DX values at segment extremities
   DOUBLE PRECISION, DIMENSION(MAX_CP)   ::  FIRST_DY,LAST_DY! DY values at segment extremities
   DOUBLE PRECISION, DIMENSION(MAX_CP)   ::  FIRST_DZ,LAST_DZ! DZ values at segment extremities


   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  F_AT
   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  F_AT_SCALAR  ! Need a copy of scalar cell F_AT for the Mesh file

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DWALL
! Grid partition for reporting best size
   INTEGER :: NODESI_REPORT,NODESJ_REPORT,NODESK_REPORT

   LOGICAL :: MINIMIZE_SEND_RECV

   LOGICAL :: DWALL_BRUTE_FORCE

! Mesh mask
   LOGICAL,  DIMENSION(:), ALLOCATABLE ::  MESH_MASK

! Master flag Cell lists      
   INTEGER, DIMENSION(:), ALLOCATABLE ::  LIST_FL_CELLS, LIST_CT_CELLS, LIST_BL_CELLS
   INTEGER :: N_FL_CELLS,N_CT_CELLS,N_BL_CELLS
CONTAINS

   SUBROUTINE ALLOCATE_CUT_CELL_ARRAYS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!
!  Module name: ALLOCATE_ARRAYS
!  Purpose: allocate arrays
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:
!
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

      IMPLICIT NONE

      DIMENSION_MAX_CUT_CELL = INT(FAC_DIM_MAX_CUT_CELL*DIMENSION_3G)

      Allocate(  INTERIOR_CELL_AT  (DIMENSION_3) )

      Allocate( XG_E(0:DIMENSION_I) )
      Allocate( YG_N(0:DIMENSION_J) )
      Allocate( ZG_T(0:DIMENSION_K) )

      Allocate(  X_U (DIMENSION_3) )
      Allocate(  Y_U (DIMENSION_3) )
      Allocate(  Z_U (DIMENSION_3) )

      Allocate(  X_V (DIMENSION_3) )
      Allocate(  Y_V (DIMENSION_3) )
      Allocate(  Z_V (DIMENSION_3) )

      Allocate(  X_W (DIMENSION_3) )
      Allocate(  Y_W (DIMENSION_3) )
      Allocate(  Z_W (DIMENSION_3) )

      Allocate(  INTERSECT_X  (DIMENSION_3) )
      Allocate(  INTERSECT_Y  (DIMENSION_3) )
      Allocate(  INTERSECT_Z  (DIMENSION_3) )

      Allocate(  X_int (DIMENSION_3) )
      Allocate(  Y_int (DIMENSION_3) )
      Allocate(  Z_int (DIMENSION_3) )

      Allocate(  X_NEW_POINT  (DIMENSION_MAX_CUT_CELL) )
      Allocate(  Y_NEW_POINT  (DIMENSION_MAX_CUT_CELL) )
      Allocate(  Z_NEW_POINT  (DIMENSION_MAX_CUT_CELL) )

      Allocate(  X_NEW_U_POINT  (DIMENSION_MAX_CUT_CELL) )
      Allocate(  Y_NEW_U_POINT  (DIMENSION_MAX_CUT_CELL) )
      Allocate(  Z_NEW_U_POINT  (DIMENSION_MAX_CUT_CELL) )

      Allocate(  X_NEW_V_POINT  (DIMENSION_MAX_CUT_CELL) )
      Allocate(  Y_NEW_V_POINT  (DIMENSION_MAX_CUT_CELL) )
      Allocate(  Z_NEW_V_POINT  (DIMENSION_MAX_CUT_CELL) )

      Allocate(  X_NEW_W_POINT  (DIMENSION_MAX_CUT_CELL) )
      Allocate(  Y_NEW_W_POINT  (DIMENSION_MAX_CUT_CELL) )
      Allocate(  Z_NEW_W_POINT  (DIMENSION_MAX_CUT_CELL) )

      Allocate(  NUMBER_OF_NODES  (DIMENSION_3) )
      Allocate(  NUMBER_OF_U_NODES  (DIMENSION_3) )
      Allocate(  NUMBER_OF_V_NODES  (DIMENSION_3) )
      Allocate(  NUMBER_OF_W_NODES  (DIMENSION_3) )

      NUMBER_OF_NODES   = 0
      NUMBER_OF_U_NODES = 0
      NUMBER_OF_V_NODES = 0
      NUMBER_OF_W_NODES = 0

      Allocate(  CONNECTIVITY  (DIMENSION_3,15) )
      Allocate(  CONNECTIVITY_U  (DIMENSION_3,15) )
      Allocate(  CONNECTIVITY_V  (DIMENSION_3,15) )
      Allocate(  CONNECTIVITY_W  (DIMENSION_3,15) )

      Allocate(  PARTITION  (DIMENSION_3) )

      Allocate(  WALL_U_AT (DIMENSION_3) )
      Allocate(  WALL_V_AT (DIMENSION_3) )
      Allocate(  WALL_W_AT (DIMENSION_3) )

      WALL_U_AT = .FALSE.
      WALL_V_AT = .FALSE.
      WALL_W_AT = .FALSE.

      Allocate( Area_CUT  (DIMENSION_3) )
      Allocate( Area_U_CUT  (DIMENSION_3) )
      Allocate( Area_V_CUT  (DIMENSION_3) )
      Allocate( Area_W_CUT  (DIMENSION_3) )


      Allocate( DELX_Ue  (DIMENSION_3) )
      Allocate( DELX_Uw  (DIMENSION_3) )
      Allocate( DELY_Un  (DIMENSION_3) )
      Allocate( DELY_Us  (DIMENSION_3) )
      Allocate( DELZ_Ut  (DIMENSION_3) )
      Allocate( DELZ_Ub  (DIMENSION_3) )

      Allocate( DELX_Ve  (DIMENSION_3) )
      Allocate( DELX_Vw  (DIMENSION_3) )
      Allocate( DELY_Vn  (DIMENSION_3) )
      Allocate( DELY_Vs  (DIMENSION_3) )
      Allocate( DELZ_Vt  (DIMENSION_3) )
      Allocate( DELZ_Vb  (DIMENSION_3) )

      Allocate( DELX_We  (DIMENSION_3) )
      Allocate( DELX_Ww  (DIMENSION_3) )
      Allocate( DELY_Wn  (DIMENSION_3) )
      Allocate( DELY_Ws  (DIMENSION_3) )
      Allocate( DELZ_Wt  (DIMENSION_3) )
      Allocate( DELZ_Wb  (DIMENSION_3) )

      Allocate( X_U_ec  (DIMENSION_3) )
      Allocate( Y_U_ec  (DIMENSION_3) )
      Allocate( Z_U_ec  (DIMENSION_3) )
      Allocate( X_U_nc  (DIMENSION_3) )
      Allocate( Y_U_nc  (DIMENSION_3) )
      Allocate( Z_U_nc  (DIMENSION_3) )
      Allocate( X_U_tc  (DIMENSION_3) )
      Allocate( Y_U_tc  (DIMENSION_3) )
      Allocate( Z_U_tc  (DIMENSION_3) )

      Allocate( X_V_ec  (DIMENSION_3) )
      Allocate( Y_V_ec  (DIMENSION_3) )
      Allocate( Z_V_ec  (DIMENSION_3) )
      Allocate( X_V_nc  (DIMENSION_3) )
      Allocate( Y_V_nc  (DIMENSION_3) )
      Allocate( Z_V_nc  (DIMENSION_3) )
      Allocate( X_V_tc  (DIMENSION_3) )
      Allocate( Y_V_tc  (DIMENSION_3) )
      Allocate( Z_V_tc  (DIMENSION_3) )

      Allocate( X_W_ec  (DIMENSION_3) )
      Allocate( Y_W_ec  (DIMENSION_3) )
      Allocate( Z_W_ec  (DIMENSION_3) )
      Allocate( X_W_nc  (DIMENSION_3) )
      Allocate( Y_W_nc  (DIMENSION_3) )
      Allocate( Z_W_nc  (DIMENSION_3) )
      Allocate( X_W_tc  (DIMENSION_3) )
      Allocate( Y_W_tc  (DIMENSION_3) )
      Allocate( Z_W_tc  (DIMENSION_3) )

      Allocate( DELH_Scalar  (DIMENSION_3) )

      Allocate( DELH_U  (DIMENSION_3) )
      Allocate( Theta_Ue  (DIMENSION_3) )
      Allocate( Theta_Ue_bar (DIMENSION_3) )
      Allocate( Theta_U_ne  (DIMENSION_3) )
      Allocate( Theta_U_nw  (DIMENSION_3) )
      Allocate( Theta_U_te  (DIMENSION_3) )
      Allocate( Theta_U_tw  (DIMENSION_3) )
      Allocate( ALPHA_Ue_c  (DIMENSION_3) )
      Allocate( NOC_U_E  (DIMENSION_3) )
      Allocate( Theta_Un  (DIMENSION_3) )
      Allocate( Theta_Un_bar (DIMENSION_3) )
      Allocate( ALPHA_Un_c  (DIMENSION_3) )
      Allocate( NOC_U_N  (DIMENSION_3) )
      Allocate( Theta_Ut  (DIMENSION_3) )
      Allocate( Theta_Ut_bar (DIMENSION_3) )
      Allocate( ALPHA_Ut_c  (DIMENSION_3) )
      Allocate( NOC_U_T  (DIMENSION_3) )
      Allocate( A_UPG_E (DIMENSION_3) )
      Allocate( A_UPG_W (DIMENSION_3) )

      Allocate( DELH_V  (DIMENSION_3) )
      Allocate( Theta_V_ne  (DIMENSION_3) )
      Allocate( Theta_V_se  (DIMENSION_3) )
      Allocate( Theta_Vn  (DIMENSION_3) )
      Allocate( Theta_Vn_bar (DIMENSION_3) )
      Allocate( Theta_V_nt  (DIMENSION_3) )
      Allocate( Theta_V_st (DIMENSION_3) )
      Allocate( Theta_Ve  (DIMENSION_3) )
      Allocate( Theta_Ve_bar (DIMENSION_3) )
      Allocate( ALPHA_Ve_c  (DIMENSION_3) )
      Allocate( NOC_V_E  (DIMENSION_3) )
      Allocate( ALPHA_Vn_c  (DIMENSION_3) )
      Allocate( NOC_V_N  (DIMENSION_3) )
      Allocate( Theta_Vt  (DIMENSION_3) )
      Allocate( Theta_Vt_bar (DIMENSION_3) )
      Allocate( ALPHA_Vt_c  (DIMENSION_3) )
      Allocate( NOC_V_T  (DIMENSION_3) )
      Allocate( A_VPG_N (DIMENSION_3) )
      Allocate( A_VPG_S (DIMENSION_3) )

      Allocate( DELH_W (DIMENSION_3) )
      Allocate( Theta_W_te (DIMENSION_3) )
      Allocate( Theta_W_be (DIMENSION_3) )
      Allocate( Theta_W_tn (DIMENSION_3) )
      Allocate( Theta_W_bn (DIMENSION_3) )
      Allocate( Theta_Wt (DIMENSION_3) )
      Allocate( Theta_Wt_bar (DIMENSION_3) )
      Allocate( Theta_We (DIMENSION_3) )
      Allocate( Theta_We_bar (DIMENSION_3) )
      Allocate( ALPHA_We_c (DIMENSION_3) )
      Allocate( NOC_W_E (DIMENSION_3) )
      Allocate( Theta_Wn (DIMENSION_3) )
      Allocate( Theta_Wn_bar (DIMENSION_3) )
      Allocate( ALPHA_Wn_c (DIMENSION_3) )
      Allocate( NOC_W_N (DIMENSION_3) )
      Allocate( ALPHA_Wt_c (DIMENSION_3) )
      Allocate( NOC_W_T (DIMENSION_3) )
      Allocate( A_WPG_T (DIMENSION_3) )
      Allocate( A_WPG_B (DIMENSION_3) )


      Allocate( NORMAL_S (DIMENSION_3,3) )
      Allocate( NORMAL_U (DIMENSION_3,3) )
      Allocate( NORMAL_V (DIMENSION_3,3) )
      Allocate( NORMAL_W (DIMENSION_3,3) )

      Allocate( REFP_S (DIMENSION_3,3) )
      Allocate( REFP_U (DIMENSION_3,3) )
      Allocate( REFP_V (DIMENSION_3,3) )
      Allocate( REFP_W (DIMENSION_3,3) )

      Allocate(  ONEoDX_E_U (DIMENSION_3) )
      Allocate(  ONEoDY_N_U (DIMENSION_3) )
      Allocate(  ONEoDZ_T_U (DIMENSION_3) )

      Allocate(  ONEoDX_E_V (DIMENSION_3) )
      Allocate(  ONEoDY_N_V (DIMENSION_3) )
      Allocate(  ONEoDZ_T_V (DIMENSION_3) )

      Allocate(  ONEoDX_E_W (DIMENSION_3) )
      Allocate(  ONEoDY_N_W (DIMENSION_3) )
      Allocate(  ONEoDZ_T_W (DIMENSION_3) )

      Allocate(  Xn_int (DIMENSION_3) )
      Allocate(  Xn_U_int (DIMENSION_3) )
      Allocate(  Xn_V_int (DIMENSION_3) )
      Allocate(  Xn_W_int (DIMENSION_3) )

      Allocate(  Ye_int (DIMENSION_3) )
      Allocate(  Ye_U_int (DIMENSION_3) )
      Allocate(  Ye_V_int (DIMENSION_3) )
      Allocate(  Ye_W_int (DIMENSION_3) )

      Allocate(  Zt_int (DIMENSION_3) )
      Allocate(  Zt_U_int (DIMENSION_3) )
      Allocate(  Zt_V_int (DIMENSION_3) )
      Allocate(  Zt_W_int (DIMENSION_3) )

      Allocate(  SNAP (DIMENSION_3) )
      Allocate(  SNAP_SCALAR (DIMENSION_3) ) 

      SNAP        = .FALSE.
      SNAP_SCALAR = .FALSE.

      Allocate(  CUT_TREATMENT_AT (DIMENSION_3) )
      Allocate(  CUT_U_TREATMENT_AT (DIMENSION_3) )
      Allocate(  CUT_V_TREATMENT_AT (DIMENSION_3) )
      Allocate(  CUT_W_TREATMENT_AT (DIMENSION_3) )


      CUT_TREATMENT_AT = .FALSE.
      CUT_U_TREATMENT_AT = .FALSE.
      CUT_V_TREATMENT_AT = .FALSE.
      CUT_W_TREATMENT_AT = .FALSE.

      Allocate(  CUT_CELL_AT (DIMENSION_3) )
      Allocate(  CUT_U_CELL_AT (DIMENSION_3) )
      Allocate(  CUT_V_CELL_AT (DIMENSION_3) )
      Allocate(  CUT_W_CELL_AT (DIMENSION_3) )

      CUT_CELL_AT   = .FALSE.
      CUT_U_CELL_AT = .FALSE.
      CUT_V_CELL_AT = .FALSE.
      CUT_W_CELL_AT = .FALSE.

      Allocate( SMALL_CELL_AT  (DIMENSION_3) )
      SMALL_CELL_AT = .FALSE.

      Allocate( SMALL_CELL_FLAG  (DIMENSION_3) )
      SMALL_CELL_FLAG = 0

      Allocate(  BLOCKED_CELL_AT (DIMENSION_3) )
      Allocate(  BLOCKED_U_CELL_AT (DIMENSION_3) )
      Allocate(  BLOCKED_V_CELL_AT (DIMENSION_3) )
      Allocate(  BLOCKED_W_CELL_AT (DIMENSION_3) )

      BLOCKED_CELL_AT   = .FALSE.
      BLOCKED_U_CELL_AT = .FALSE.
      BLOCKED_V_CELL_AT = .FALSE.
      BLOCKED_W_CELL_AT = .FALSE.

      Allocate(  STANDARD_CELL_AT (DIMENSION_3) )
      Allocate(  STANDARD_U_CELL_AT (DIMENSION_3) )
      Allocate(  STANDARD_V_CELL_AT (DIMENSION_3) )
      Allocate(  STANDARD_W_CELL_AT (DIMENSION_3) )

      STANDARD_CELL_AT   = .TRUE.
      STANDARD_U_CELL_AT = .TRUE.
      STANDARD_V_CELL_AT = .TRUE.
      STANDARD_W_CELL_AT = .TRUE.

      Allocate(  VORTICITY (DIMENSION_3) )
      Allocate(  LAMBDA2 (DIMENSION_3) )

      Allocate(  TRD_G_OUT (DIMENSION_3) )
      Allocate(  PP_G_OUT (DIMENSION_3) )
      Allocate(  EPP_OUT (DIMENSION_3) )

      Allocate(  dudx_OUT (DIMENSION_3) )
      Allocate(  dvdy_OUT (DIMENSION_3) )
      Allocate(  delv_OUT (DIMENSION_3) )

      Allocate(  U_MASTER_OF (DIMENSION_3) )
      Allocate(  V_MASTER_OF (DIMENSION_3) )
      Allocate(  W_MASTER_OF (DIMENSION_3) )

      Allocate(  BC_ID (DIMENSION_3) )
      Allocate(  BC_U_ID (DIMENSION_3) )
      Allocate(  BC_V_ID (DIMENSION_3) )
      Allocate(  BC_W_ID (DIMENSION_3) )

      BC_ID   = 0
      BC_U_ID = 0
      BC_V_ID = 0
      BC_W_ID = 0

      Allocate(  DEBUG_CG (DIMENSION_3,15) )

      Allocate(  U_g_CC (DIMENSION_3) )
      Allocate(  V_g_CC (DIMENSION_3) )
      Allocate(  W_g_CC (DIMENSION_3) )

      Allocate(  U_s_CC (DIMENSION_3, DIMENSION_M) )
      Allocate(  V_s_CC (DIMENSION_3, DIMENSION_M) )
      Allocate(  W_s_CC (DIMENSION_3, DIMENSION_M) )

      ALLOCATE(N_FACET_AT(DIMENSION_3))
      N_FACET_AT = 0

      ALLOCATE(LIST_FACET_AT(DIMENSION_3,DIM_FACETS_PER_CELL))

      ALLOCATE(POTENTIAL_CUT_CELL_AT(DIMENSION_3))

      Allocate(  F_AT (DIMENSION_3) )
      Allocate(  F_AT_SCALAR (DIMENSION_3) ) 

      Allocate(  DWALL (DIMENSION_3) )

      Allocate(  MESH_MASK (DIMENSION_3) )  
      MESH_MASK = .FALSE.  

      RETURN

   END SUBROUTINE ALLOCATE_CUT_CELL_ARRAYS

END MODULE cutcell
