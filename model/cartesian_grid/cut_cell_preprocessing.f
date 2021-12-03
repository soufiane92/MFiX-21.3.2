#include "error.inc"

MODULE CUT_CELL_PREPROC

   USE bc
   USE cdist, only: bdist_io
   USE compar
   USE cutcell
   USE define_quadrics_mod, only: get_f_quadric, define_quadrics, reasssign_quadric
   USE des_bc, only: DEM_MIO
   USE discretelement, only: GENER_PART_CONFIG
   USE dmp_cartesian, only: send_receive_cut_cell_variables
   USE eos, only: EOSG
   USE error_manager
   USE geometry
   USE get_alpha_mod, only: get_3d_alpha_u_cut_cell, get_3d_alpha_v_cut_cell, get_3d_alpha_w_cut_cell
   USE get_bc_area_mod, only: cg_get_bc_area
   USE get_cut_cell_flags_mod, only: set_3d_cut_cell_flags, set_3d_cut_cell_treatment_flags, set_ghost_cell_flags
   USE get_cut_cell_flags_mod, only: set_3d_cut_u_cell_flags, set_3d_cut_v_cell_flags, set_3d_cut_w_cell_flags
   USE get_cut_cell_flags_mod, only: set_3d_cut_cell_flags_add
   USE get_delh_mod, only: get_distance_to_wall
   USE get_master_mod, only: get_u_master_cells, get_v_master_cells, get_w_master_cells
   USE indices
   USE mpi_utility
   USE param, only: dimension_3
   USE param1, only: undefined, one
   USE physprop
   USE ps
   USE run, only: run_type, subgrid_wall
   USE run, only:ppo, ENERGY_EQ
   USE run, only: GENERATE_MESH,MESH_FILE_PRESENT
   USE scales, only: p_ref
   USE set_odxyz_mod, only: set_odxyz_u_cut_cell, set_odxyz_v_cut_cell, set_odxyz_w_cut_cell
   USE toleranc, only: compare
   USE vtk
   USE vtk_out_mod
   use mfix_pic, only: mppic
   INTEGER :: REC_CP, REC_BC, REC_NP, REC_NC, REC_BULK
! Update mesh mask based on default value of an array
      INTERFACE UPDATE_MESH_MASK
         MODULE PROCEDURE UPDATE_MESH_MASK_I
         MODULE PROCEDURE UPDATE_MESH_MASK_L
         MODULE PROCEDURE UPDATE_MESH_MASK_D
      END INTERFACE


CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: CUT_CELL_PREPROCESSING                                 C
!  Purpose: Perform the cut-cell preprocessing stage:                  C
!           Identify cut cells, define face areas, and volumes         C
!           Set flags                                                  C
!           Compute Interpolations factors                             C
!           Compute Non-orthogonality Corrections terms                C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE CUT_CELL_PREPROCESSING

      IMPLICIT NONE

      INTEGER :: SAFE_MODE_COUNT
      DOUBLE PRECISION :: CPU_PP_START,CPU_PP_END


      IF(.NOT.CG_HEADER_WAS_PRINTED) CALL PRINT_CG_HEADER

      CALL CPU_TIME (CPU_PP_START)

!      CALL OPEN_CUT_CELL_FILES

      CALL ALLOCATE_CUT_CELL_ARRAYS

      CALL DEFINE_QUADRICS

      CALL SET_3D_CUT_CELL_FLAGS

      IF(GENERATE_MESH) CALL GATHER_DATA

!======================================================================
! Gather Data  and writes surface(s) defined by all cut cells
!======================================================================


!      CALL BUILD_MESH_MASK

      IF(.NOT.GENERATE_MESH) THEN
         CALL READ_MESH_DATA
         CALL GATHER_DATA
      ENDIF

      CALL SET_3D_CUT_CELL_FLAGS_ADD

      ! IF(WRITE_VTK_FILES.AND.(.NOT.BDIST_IO)) THEN
      !    IF(GENERATE_MESH) CALL WRITE_CUT_SURFACE_VTK
      ! ENDIF

      CALL SET_3D_CUT_U_CELL_FLAGS
      CALL SET_3D_CUT_V_CELL_FLAGS
      IF(DO_K) CALL SET_3D_CUT_W_CELL_FLAGS

      CALL SET_3D_CUT_CELL_TREATMENT_FLAGS

      CALL GET_3D_ALPHA_U_CUT_CELL
      CALL GET_3D_ALPHA_V_CUT_CELL
      IF(DO_K) CALL GET_3D_ALPHA_W_CUT_CELL

!      IF(GENERATE_MESH) CALL SET_GHOST_CELL_FLAGS
      CALL SET_GHOST_CELL_FLAGS
!      CALL SET_GHOST_CELL_FLAGS_OLD

      CALL SET_ODXYZ_U_CUT_CELL
      CALL SET_ODXYZ_V_CUT_CELL
      IF(DO_K) CALL SET_ODXYZ_W_CUT_CELL

      CALL GET_U_MASTER_CELLS
      CALL GET_V_MASTER_CELLS
      IF(DO_K) CALL GET_W_MASTER_CELLS

      CALL SEND_RECEIVE_CUT_CELL_VARIABLES


      IF(SUBGRID_WALL) CALL GET_DISTANCE_TO_WALL

      if(ADJUST_PARTITION) GENERATE_MESH=.FALSE.
      CALL SAVE_MESH_STATISTICS

      CALL CG_GET_BC_AREA

!JFD These subroutines do not exist anymore !!
!JFD      CALL SET_BC_FLOW

!JFD      CALL CG_FLOW_TO_VEL

!JFD      CALL CONVERT_CG_MI_TO_PS



      CALL CPU_TIME (CPU_PP_END)

      WRITE(ERR_MSG,20)'CARTESIAN GRID PRE-PROCESSING COMPLETED IN ',CPU_PP_END - CPU_PP_START, ' SECONDS.'
      CALL LOG_STATUS()

      WRITE(ERR_MSG,10)'============================================================================'
      CALL LOG_STATUS()

      IF(myPE == PE_IO) THEN

         SAFE_MODE_COUNT = SUM(CG_SAFE_MODE)

         IF(SAFE_MODE_COUNT>0) THEN


            WRITE(*,10)'######################################################################'
            WRITE(*,10)'######################################################################'
            WRITE(*,10)'##                                                                  ##'
            WRITE(*,10)'##                              ||                                  ##'
            WRITE(*,10)'##                              ||                                  ##'
            WRITE(*,10)'##                              \/                                  ##'
            WRITE(*,10)'##                                                                  ##'
            WRITE(*,10)'##  ===>   WARNING: RUNNING CARTESIAN GRID IN SAFE MODE !  <===     ##'
            WRITE(*,10)'##                                                                  ##'
            WRITE(*,10)'##  SAFE MODE ACTIVATED FOR :                                       ##'
            IF(CG_SAFE_MODE(1)==1) WRITE(*,10)'##                            - All scalar quantities               ##'
            IF(CG_SAFE_MODE(3)==1) WRITE(*,10)'##                            - X-Velocity (Gas and Solids)         ##'
            IF(CG_SAFE_MODE(4)==1) WRITE(*,10)'##                            - Y-Velocity (Gas and Solids)         ##'
            IF(CG_SAFE_MODE(5)==1) WRITE(*,10)'##                            - Z-Velocity (Gas and Solids)         ##'
            WRITE(*,10)'##                                                                  ##'
            WRITE(*,10)'##                              /\                                  ##'
            WRITE(*,10)'##                              ||                                  ##'
            WRITE(*,10)'##                              ||                                  ##'
            WRITE(*,10)'##                                                                  ##'
            WRITE(*,10)'######################################################################'
            WRITE(*,10)'######################################################################'

         ENDIF
      ENDIF

      RETURN

10    FORMAT(A)
20    FORMAT(1X,A,F8.2,A)
1000  FORMAT('Info: THIS IS A COLD DEM GRANULAR FLOW SIMULATION.'/   &
             '      SKIPPING CARTESIAN GRID PRE-PROCESSING.'/)

      END SUBROUTINE CUT_CELL_PREPROCESSING


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: PRINT_CG_HEADER                                        C
!  Purpose: Display Cartesian-Grid Header on screen                    C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE PRINT_CG_HEADER

      IMPLICIT NONE

#ifndef MFIX_INTERACTIVE
      WRITE(ERR_MSG,'(A)') '' // &
         '==============================================================================' // nl // &
         '  ____          ___________    _   ____      ____    __________   __________  ' // nl // &
         ' |    \        /           |  (_)  \   \    /   /   |          | |          | ' // nl // &
         ' |     \      /      ______|  ___   \   \  /   /    |    ______| |    ______| ' // nl // &
         ' |      \    /      |______  |   |   \   \/   /     |   |        |   |        ' // nl // &
         ' |       \  /              | |   |    \      /  === |   |        |   |  ____  ' // nl // &
         ' |   |\   \/   /|    ______| |   |    /      \      |   |        |   | |_   | ' // nl // &
         ' |   | \      / |   |        |   |   /   /\   \     |   |______  |   |___|  | ' // nl // &
         ' |   |  \    /  |   |        |   |  /   /  \   \    |          | |          | ' // nl // &
         ' |___|   \__/   |___|        |___| /___/    \___\   |__________| |__________| ' // nl // &
         '                                                                              ' // nl // &
         '============================================================================='
      CALL LOG_STATUS()
#endif
      WRITE(ERR_MSG,'(A)')' MFIX WITH CARTESIAN GRID IMPLEMENTATION.'
      CALL LOG_STATUS()

      IF(RE_INDEXING) THEN
         WRITE(ERR_MSG,'(A)')' RE-INDEXING IS TURNED ON.'
!            IF(ADJUST_PROC_DOMAIN_SIZE) THEN
!               WRITE(*,'(A)')'EACH PROCESSOR DOMAIN SIZE WILL BE ADJUSTED FOR BETTER LOAD BALANCING.'
!            ELSE
!               WRITE(*,'(A)')'WARNING: PROCESSOR DOMAIN SIZE WILL BE UNIFORMLY DISTRIBUTED.'
!               WRITE(*,'(A)')'THIS COULD RESULT IN VERY POOR LOAD BALANCING.'
!            ENDIF
      ELSE
         WRITE(ERR_MSG,'(A)')' RE-INDEXING IS TURNED OFF.'
      ENDIF
      CALL LOG_STATUS()

      CG_HEADER_WAS_PRINTED = .TRUE.

      RETURN

      END SUBROUTINE PRINT_CG_HEADER


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: OPEN_CUT_CELL_FILES                                    C
!  Purpose: Open CUT CELL related file                                 C
!           and writes headers                                         C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE OPEN_CUT_CELL_FILES

      IMPLICIT NONE

      IF(MyPE == PE_IO)  THEN
         OPEN(UNIT = UNIT_CUT_CELL_LOG, FILE = 'CUT_CELL.LOG')
      ENDIF

      RETURN

      END SUBROUTINE OPEN_CUT_CELL_FILES


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: CLOSE_CUT_CELL_FILES                                   C
!  Purpose: Close CUT CELL related file                                C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE CLOSE_CUT_CELL_FILES

      IMPLICIT NONE

      IF(MyPE == PE_IO) THEN
         CLOSE(UNIT_CUT_CELL_LOG)
      ENDIF

      RETURN


      END SUBROUTINE CLOSE_CUT_CELL_FILES

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_MESH_PPO                                         C
!  Purpose: Writes the mesh when PPO=.TRUE.                            C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 30-May-17  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE WRITE_MESH_PPO

      IMPLICIT NONE
      INTEGER :: MMAX_BCK, NMAX0_BCK,NSCALAR_BCK,nRR_BCK

! Setrup region 0 as the mesh region

      VTK_DEFINED(0)  = .TRUE.
      VTK_FILEBASE(0) = TRIM(RUN_NAME) // '_MESH'

      VTK_X_w(0) = X_MIN
      VTK_X_e(0) = X_MAX
      VTK_Y_s(0) = Y_MIN
      VTK_Y_n(0) = Y_MAX
      VTK_Z_b(0) = Z_MIN
      VTK_Z_t(0) = Z_MAX


! Back up and set the following to zero to avoid error in WRITE_VTU_FILE loops
      MMAX_BCK    = MMAX
      NMAX0_BCK   = NMAX(0)
      NSCALAR_BCK = NSCALAR
      nRR_BCK     = nRR

      MMAX        = 0
      NMAX(0)     = 0
      NSCALAR     = 0
      nRR         = 0

      IF(.NOT.CARTESIAN_GRID) THEN
         CALL SETUP_VTK_NO_CUTCELL
         CALL GATHER_DATA
         CALL SAVE_MESH_STATISTICS
      ENDIF

! Write mesh to vtu file
      CALL WRITE_VTU_FILE(0,2)

      CALL SAVE_MESH_DATA

!      IF(PPO) CALL WRITE_CUT_SURFACE_VTK
      IF(GENERATE_MESH) CALL WRITE_CUT_SURFACE_VTK

!      CALL WRITE_CUT_SURFACE_VTK

! Revert to backed-up data
      MMAX    = MMAX_BCK
      NMAX(0) = NMAX0_BCK
      NSCALAR = NSCALAR_BCK
      nRR     = nRR_BCK

      IF(MyPE==0) WRITE(*,100)' PRE_PROCESSING COMPLETE. '

100   FORMAT(76('='),/A/,76('='))


      END SUBROUTINE WRITE_MESH_PPO

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: SAVE_MESH_DATA                                         C
!  Purpose: Saves mesh data during PPO run, such that it can be used   C
!           later during a regular run.                                C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 28-Jun-17  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE SAVE_MESH_DATA

      USE param
      USE param1
      USE parallel
      USE constant
      USE run
      USE toleranc
      USE geometry
      USE indices
      USE compar
      USE sendrecv
      USE quadric
      USE cutcell
      USE polygon
      USE stl
      USE funits
      USE bc
      USE vtk

      USE mpi_utility
      USE functions
      use error_manager
      USE out_bin_512_mod, only: out_bin_512
      USE out_bin_512i_mod, only: out_bin_512i

      IMPLICIT NONE
      INTEGER :: BCV, NUMBER_OF_BC_PATCHES
      INTEGER :: IJK,I,J,K,GI,GJ,GK,GC
      INTEGER :: IM,IP,JM,JP,KM,KP,IMJK,IPJK,IJMK,IJPK,IJKM,IJKP
      INTEGER :: IJPKP,IPJKP,IPJPK

      DOUBLE PRECISION, DIMENSION(DIMENSION_3) :: Xint,Yint,Zint
      DOUBLE PRECISION, DIMENSION(DIMENSION_3) :: Xintflag,Yintflag,Zintflag
      DOUBLE PRECISION, DIMENSION(DIMENSION_3) :: Snapflag, bc_idflag


      INTEGER :: NN,I1,I2,J1,J2,K1,K2,L

      INTEGER :: PACK_SIZE_N_CELLS, PACK_SIZE_N_NODES, PACKED_CONNECTIVITY_SIZE

      INTEGER, DIMENSION(:), ALLOCATABLE :: PACKED_CONNECTIVITY
      INTEGER, DIMENSION(DIMENSION_3) :: MASTER_FLAG

      INTEGER :: MESH_FILE_UNIT
      CHARACTER(LEN=255) :: MESH_FILE_NAME

      INTEGER :: NXE,NYE,NZE,NFAT
! Next record number in direct access output file
      INTEGER          NEXT_REC


! Initialize the error flag array.
      IER = 0

      IF(.NOT.CARTESIAN_GRID.OR.bDist_IO.or.ADJUST_PARTITION) THEN
         RETURN
      ENDIF

      IF(GENERATE_MESH) THEN


         WRITE(ERR_MSG,100) ' Saving Mesh connectivity and flags...'
         CALL LOG_STATUS()

! The mesh points and connectivity is done on the head node
! Only info (point coordinates and connectivity) related to cut cells
! needs to be saved. Info related to regular (uncut cells) can be
! recalculated easily and is not saved (to save space).

         IF(MyPE == PE_IO) THEN



            NUMBER_OF_BC_PATCHES = 0
            DO BCV = 1, DIMENSION_BC
               IF(BC_DEFINED(BCV)) THEN
                  NUMBER_OF_BC_PATCHES = NUMBER_OF_BC_PATCHES + 1
               ENDIF
            ENDDO

            REC_NP   = 7   ! Record storing number of points
            REC_NC   = 8   ! Record storing number of cells
            REC_CP   = 10  ! Record storing control points
            REC_BULK = 100 ! Record storing the bulk of the mesh data. May need to make this dynamic.

! Mesh/Geometry info
            WRITE(UNIT_MSH, REC=3) IMAX, JMAX, KMAX, DO_K, X_MIN, X_MAX, Y_MIN, Y_MAX, Z_MIN, Z_MAX,NCPX,NCPY,NCPZ
            WRITE(UNIT_MSH, REC=4) CARTESIAN_GRID, RE_INDEXING, USE_STL, STL_BC_ID, OUT_STL_VALUE, TX_STL, TY_STL, TZ_STL, SCALE_STL
            WRITE(UNIT_MSH, REC=5) TOL_SMALL_CELL, TOL_SMALL_AREA, TOL_MERGE, TOL_SNAP_BCK(1:3), FAC_DIM_MAX_CUT_CELL, &
                                   ITERMAX_INT, TOL_STL, STL_SMALL_ANGLE, TOL_STL_DP, DIM_FACETS_PER_CELL, TOL_DELH
! Record 6 stores the number of BC patches and the starting record for the BC
! info (extent and BC type). This depends on the the number of control points.
! See 'WRITE(UNIT_MSH, REC=6)' below


! Control points
            NEXT_REC = REC_CP
            IF(NCPX>0) THEN
               CALL OUT_BIN_512  (UNIT_MSH, CPX(0:NCPX),NCPX+1, NEXT_REC)
               CALL OUT_BIN_512I (UNIT_MSH, NCX(:),NCPX, NEXT_REC)
               CALL OUT_BIN_512  (UNIT_MSH, ERX(:),NCPX, NEXT_REC)
               CALL OUT_BIN_512  (UNIT_MSH, FIRST_DX(:),NCPX, NEXT_REC)
               CALL OUT_BIN_512  (UNIT_MSH, LAST_DX(:),NCPX, NEXT_REC)
            ENDIF
            IF(NCPY>0) THEN
               CALL OUT_BIN_512  (UNIT_MSH, CPY(0:NCPY),NCPY+1, NEXT_REC)
               CALL OUT_BIN_512I (UNIT_MSH, NCY(:),NCPY, NEXT_REC)
               CALL OUT_BIN_512  (UNIT_MSH, ERY(:),NCPY, NEXT_REC)
               CALL OUT_BIN_512  (UNIT_MSH, FIRST_DY(:),NCPY, NEXT_REC)
               CALL OUT_BIN_512  (UNIT_MSH, LAST_DY(:),NCPY, NEXT_REC)
            ENDIF
            IF(NCPZ>0) THEN
               CALL OUT_BIN_512  (UNIT_MSH, CPZ(0:NCPZ),NCPZ+1, NEXT_REC)
               CALL OUT_BIN_512I (UNIT_MSH, NCZ(:),NCPZ, NEXT_REC)
               CALL OUT_BIN_512  (UNIT_MSH, ERZ(:),NCPZ, NEXT_REC)
               CALL OUT_BIN_512  (UNIT_MSH, FIRST_DZ(:),NCPZ, NEXT_REC)
               CALL OUT_BIN_512  (UNIT_MSH, LAST_DZ(:),NCPZ, NEXT_REC)
            ENDIF

! Boundary condition regions
            REC_BC = NEXT_REC
            WRITE(UNIT_MSH, REC=6) NUMBER_OF_BC_PATCHES, REC_BC
            DO BCV = 1, DIMENSION_BC
               IF(BC_DEFINED(BCV)) THEN
                  WRITE(UNIT_MSH, REC=NEXT_REC) BCV, BC_TYPE(BCV), BC_X_W(BCV), BC_X_E(BCV), BC_Y_S(BCV), BC_Y_N(BCV), BC_Z_B(BCV), BC_Z_T(BCV)
                  NEXT_REC = NEXT_REC + 1
               ENDIF
            ENDDO



! The bulk of the data is written starting at the record below
! (Initial value of NEXT_REC).
! A small amount of data (number of points, number of cells etc)
! is written before this record. The amount of data must be such that it
! does not exceed the initial value of NEXT_REC.

         NEXT_REC = REC_BULK

! The packed connectivity is a list of integers:
! IJK,GLOBAL_NUMBER_OF_NODES(IJK),GLOBAL_CLEANED_CONNECTIVITY(IJK,L)-1,L=1,GLOBAL_NUMBER_OF_NODES(IJK) for each fluid cell IJK

! First step: Determine the size of the packed connectivity array
            PACK_SIZE_N_CELLS = 0
            PACK_SIZE_N_NODES = 0
            DO IJK = 1,IJKMAX3
               IF (BELONGS_TO_VTK_SUBDOMAIN(IJK).AND.GLOBAL_CUT_CELL_AT(IJK)) THEN
                  PACK_SIZE_N_CELLS = PACK_SIZE_N_CELLS + 1
                  PACK_SIZE_N_NODES = PACK_SIZE_N_NODES + GLOBAL_NUMBER_OF_NODES(IJK)
               ENDIF
            END DO

            PACKED_CONNECTIVITY_SIZE = 2*PACK_SIZE_N_CELLS + PACK_SIZE_N_NODES

! Second step: Allocate and fill-up the  packed connectivity array
            IF(ALLOCATED(PACKED_CONNECTIVITY)) DEALLOCATE(PACKED_CONNECTIVITY)
            ALLOCATE(PACKED_CONNECTIVITY(PACKED_CONNECTIVITY_SIZE))

            I=0
            DO IJK = 1,IJKMAX3
               IF (BELONGS_TO_VTK_SUBDOMAIN(IJK).AND.GLOBAL_CUT_CELL_AT(IJK)) THEN
                  I = I + 1
                  GI = GLOBAL_I_OF(IJK)
                  GJ = GLOBAL_J_OF(IJK)
                  GK = GLOBAL_K_OF(IJK)
                  PACKED_CONNECTIVITY(I) = funijk_io(GI,GJ,GK)
                  I = I + 1
                  PACKED_CONNECTIVITY(I) = GLOBAL_NUMBER_OF_NODES(IJK)
                  DO L=1,GLOBAL_NUMBER_OF_NODES(IJK)
                     I = I + 1
                     GC = GLOBAL_CONNECTIVITY(IJK,L)
                     IF(GC>IJKMAX3) THEN  ! This is a new point (not a corner point)
                        PACKED_CONNECTIVITY(I) = GC
                     ELSE  ! This is a cell corner point, need to write IO index
                        GI = GLOBAL_I_OF(GLOBAL_CONNECTIVITY(IJK,L))
                        GJ = GLOBAL_J_OF(GLOBAL_CONNECTIVITY(IJK,L))
                        GK = GLOBAL_K_OF(GLOBAL_CONNECTIVITY(IJK,L))
                        PACKED_CONNECTIVITY(I) = funijk_io(GI,GJ,GK)
                     ENDIF
                  ENDDO
               ENDIF
            END DO

! Coordinates of the new point (defining cut cells)
            CALL OUT_BIN_512 (UNIT_MSH, GLOBAL_X_NEW_POINT(:),GLOBAL_NUMBER_OF_NEW_POINTS, NEXT_REC)
            CALL OUT_BIN_512 (UNIT_MSH, GLOBAL_Y_NEW_POINT(:),GLOBAL_NUMBER_OF_NEW_POINTS, NEXT_REC)
            CALL OUT_BIN_512 (UNIT_MSH, GLOBAL_Z_NEW_POINT(:),GLOBAL_NUMBER_OF_NEW_POINTS, NEXT_REC)

! Connectivity
            CALL OUT_BIN_512I (UNIT_MSH, PACKED_CONNECTIVITY,PACKED_CONNECTIVITY_SIZE, NEXT_REC)

! Number of points and cells
            WRITE(UNIT_MSH, REC=REC_NP) NUMBER_OF_POINTS,GLOBAL_NUMBER_OF_NEW_POINTS,NUMBER_OF_VTK_CELLS,PACKED_CONNECTIVITY_SIZE,IJKMAX3

         ENDIF ! MyPE == PE_IO


! Consolidate flags into one master flag
         CALL SET_MASTER_FROM_FLAGS(MASTER_FLAG)
         CALL gatherWriteMasterList(MASTER_FLAG, NEXT_REC)

! Flags
         CALL gatherWriteMesh_ic(FLAG, .FALSE., NEXT_REC)

         CALL gatherWriteMesh_ic(FLAG_E, .FALSE., NEXT_REC)
         CALL gatherWriteMesh_ic(FLAG_N, .FALSE., NEXT_REC)
         IF(DO_K) CALL gatherWriteMesh_ic(FLAG_T, .FALSE., NEXT_REC)

! BC ID
         CALL gatherWriteMesh_ic(BC_ID, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_ic(BC_U_ID, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_ic(BC_V_ID, .TRUE., NEXT_REC)
         IF(DO_K) CALL gatherWriteMesh_ic(BC_W_ID, .TRUE., NEXT_REC)

! problematic cells
         CALL gatherWriteMesh_lc(SMALL_CELL_AT, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_lc(WALL_U_AT, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_lc(WALL_V_AT, .TRUE., NEXT_REC)
         IF(DO_K) CALL gatherWriteMesh_lc(WALL_W_AT, .TRUE., NEXT_REC)

! F_AT,SNAP
         CALL gatherWriteMesh_dpc(F_AT_SCALAR, .FALSE., NEXT_REC)
         CALL gatherWriteMesh_lc(SNAP_SCALAR, .FALSE., NEXT_REC)

! Flag for Garg interpolation
         CALL gatherWriteMesh_lc(SCALAR_NODE_ATWALL(1:DIMENSION_3), .FALSE., NEXT_REC)

!======================================================================
!  Scalar cells
!======================================================================
         WRITE(ERR_MSG,100) ' Saving Mesh info for Scalar cells...'
         CALL LOG_STATUS()

! Cell volume, areas
         CALL gatherWriteMesh_dpc(VOL, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(AXY, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(AXZ, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(AYZ, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Area_cut, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Aspect_ratio, .TRUE., NEXT_REC)

! Cut face Normal and reference points, wall distance
         CALL gatherWriteMesh_dpc(Normal_S(:,1), .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Normal_S(:,2), .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Normal_S(:,3), .TRUE., NEXT_REC)

         CALL gatherWriteMesh_dpc(REFP_S(:,1), .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(REFP_S(:,2), .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(REFP_S(:,3), .TRUE., NEXT_REC)

         CALL gatherWriteMesh_dpc(DELH_SCALAR, .TRUE., NEXT_REC)

!======================================================================
!  U-Momentum cells
!======================================================================
         WRITE(ERR_MSG,100) ' Saving Mesh info for U-MOMENTUM cells...'
         CALL LOG_STATUS()

! Cell volume, areas
         CALL gatherWriteMesh_dpc(VOL_U, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(AXY_U, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(AXZ_U, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(AYZ_U, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Area_U_cut, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Aspect_ratio_U, .TRUE., NEXT_REC)

! Cut face Normal and reference points, wall distance
         CALL gatherWriteMesh_dpc(Normal_U(:,1), .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Normal_U(:,2), .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Normal_U(:,3), .TRUE., NEXT_REC)

         CALL gatherWriteMesh_dpc(REFP_U(:,1), .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(REFP_U(:,2), .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(REFP_U(:,3), .TRUE., NEXT_REC)

         CALL gatherWriteMesh_dpc(DELH_U, .TRUE., NEXT_REC)

! Velocity nodes
         CALL gatherWriteMesh_dpc(X_U, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Y_U, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Z_U, .TRUE., NEXT_REC)

         CALL gatherWriteMesh_dpc(X_U_ec, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Y_U_ec, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Z_U_ec, .TRUE., NEXT_REC)

         CALL gatherWriteMesh_dpc(X_U_nc, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Y_U_nc, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Z_U_nc, .TRUE., NEXT_REC)

         IF(DO_K) CALL gatherWriteMesh_dpc(X_U_tc, .TRUE., NEXT_REC)
         IF(DO_K) CALL gatherWriteMesh_dpc(Y_U_tc, .TRUE., NEXT_REC)
         IF(DO_K) CALL gatherWriteMesh_dpc(Z_U_tc, .TRUE., NEXT_REC)

         CALL gatherWriteMesh_dpc(DELX_Ue, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(DELX_Uw, .TRUE., NEXT_REC)

         CALL gatherWriteMesh_dpc(DELX_Ve, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(DELX_Vw, .TRUE., NEXT_REC)

         IF(DO_K) CALL gatherWriteMesh_dpc(DELX_We, .TRUE., NEXT_REC)
         IF(DO_K) CALL gatherWriteMesh_dpc(DELX_Ww, .TRUE., NEXT_REC)

!         Xn_U_int = ZERO
         CALL gatherWriteMesh_dpc(Xn_U_int, .TRUE., NEXT_REC)

!======================================================================
!  V-Momentum cells
!======================================================================
         WRITE(ERR_MSG,100) ' Saving Mesh info for V-MOMENTUM cells...'
         CALL LOG_STATUS()

! Cell volume, areas
         CALL gatherWriteMesh_dpc(VOL_V, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(AXY_V, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(AXZ_V, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(AYZ_V, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Area_V_cut, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Aspect_ratio_V, .TRUE., NEXT_REC)

! Cut face Normal and reference points, wall distance
         CALL gatherWriteMesh_dpc(Normal_V(:,1), .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Normal_V(:,2), .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Normal_V(:,3), .TRUE., NEXT_REC)

         CALL gatherWriteMesh_dpc(REFP_V(:,1), .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(REFP_V(:,2), .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(REFP_V(:,3), .TRUE., NEXT_REC)

         CALL gatherWriteMesh_dpc(DELH_V, .TRUE., NEXT_REC)

! Velocity nodes
         CALL gatherWriteMesh_dpc(X_V, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Y_V, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Z_V, .TRUE., NEXT_REC)

         CALL gatherWriteMesh_dpc(X_V_ec, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Y_V_ec, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Z_V_ec, .TRUE., NEXT_REC)

         CALL gatherWriteMesh_dpc(X_V_nc, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Y_V_nc, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(Z_V_nc, .TRUE., NEXT_REC)

         IF(DO_K) CALL gatherWriteMesh_dpc(X_V_tc, .TRUE., NEXT_REC)
         IF(DO_K) CALL gatherWriteMesh_dpc(Y_V_tc, .TRUE., NEXT_REC)
         IF(DO_K) CALL gatherWriteMesh_dpc(Z_V_tc, .TRUE., NEXT_REC)

         CALL gatherWriteMesh_dpc(DELY_Un, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(DELY_Us, .TRUE., NEXT_REC)

         CALL gatherWriteMesh_dpc(DELY_Vn, .TRUE., NEXT_REC)
         CALL gatherWriteMesh_dpc(DELY_Vs, .TRUE., NEXT_REC)

         IF(DO_K) CALL gatherWriteMesh_dpc(DELY_Wn, .TRUE., NEXT_REC)
         IF(DO_K) CALL gatherWriteMesh_dpc(DELY_Ws, .TRUE., NEXT_REC)

         CALL gatherWriteMesh_dpc(Ye_V_int, .TRUE., NEXT_REC)

         IF(DO_K) THEN
!======================================================================
!  W-Momentum cells
!======================================================================
            WRITE(ERR_MSG,100) ' Saving Mesh info for W-MOMENTUM cells...'
            CALL LOG_STATUS()

! Cell volume, areas
            CALL gatherWriteMesh_dpc(VOL_W, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(AXY_W, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(AXZ_W, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(AYZ_W, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(Area_W_cut, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(Aspect_ratio_W, .TRUE., NEXT_REC)

! Cut face Normal and reference points, wall distance
            CALL gatherWriteMesh_dpc(Normal_W(:,1), .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(Normal_W(:,2), .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(Normal_W(:,3), .TRUE., NEXT_REC)

            CALL gatherWriteMesh_dpc(REFP_W(:,1), .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(REFP_W(:,2), .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(REFP_W(:,3), .TRUE., NEXT_REC)

            CALL gatherWriteMesh_dpc(DELH_W, .TRUE., NEXT_REC)

! Velocity nodes
            CALL gatherWriteMesh_dpc(X_W, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(Y_W, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(Z_W, .TRUE., NEXT_REC)

            CALL gatherWriteMesh_dpc(X_W_ec, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(Y_W_ec, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(Z_W_ec, .TRUE., NEXT_REC)

            CALL gatherWriteMesh_dpc(X_W_nc, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(Y_W_nc, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(Z_W_nc, .TRUE., NEXT_REC)

            CALL gatherWriteMesh_dpc(X_W_tc, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(Y_W_tc, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(Z_W_tc, .TRUE., NEXT_REC)

            CALL gatherWriteMesh_dpc(DELZ_Ut, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(DELZ_Ub, .TRUE., NEXT_REC)

            CALL gatherWriteMesh_dpc(DELZ_Vt, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(DELZ_Vb, .TRUE., NEXT_REC)

            CALL gatherWriteMesh_dpc(DELZ_Wt, .TRUE., NEXT_REC)
            CALL gatherWriteMesh_dpc(DELZ_Wb, .TRUE., NEXT_REC)

!            Zt_W_int = ZERO
            CALL gatherWriteMesh_dpc(Zt_W_int, .TRUE., NEXT_REC)

         ENDIF  ! DO_K

      ENDIF

      IF(myPE==PE_IO) CLOSE(UNIT_MSH)

      RETURN

100   FORMAT(1X,A)
110   FORMAT(1X,A,I8,A,I8)
120   FORMAT(1X,A,I8)
200   FORMAT(1X,4(I8),1X,D16.8)

      END SUBROUTINE SAVE_MESH_DATA

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine:                                                         C
!  Purpose:                                                            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      subroutine gatherWriteMasterList(MASTER_FLAG, NEXT_REC)

! Modules
!---------------------------------------------------------------------//
      USE geometry
      USE funits
      USE cdist
      USE compar
      USE mpi_utility
      USE sendrecv
      USE cutcell
      USE in_binary_512i
      USE out_bin_512i_mod, only: out_bin_512i
      USE param, only: dimension_3
      USE set_increments_mod, only: unshift_dp_array
      USE error_manager
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      integer, dimension(ijkmax2) :: array1
      integer, dimension(ijkmax3) :: array2
      integer, dimension(DIMENSION_3) :: MASTER_FLAG
      INTEGER :: NEXT_REC
      INTEGER :: IJK

! Local variables
!---------------------------------------------------------------------//
      double precision, dimension(DIMENSION_3) :: TMP_VAR

!---------------------------------------------------------------------//
         WRITE(ERR_MSG,100) ' Gather and Write MasterList...'
         CALL LOG_STATUS()

!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      if (.not.bDist_IO) then

!         IF(RE_INDEXING) THEN
!           CALL UNSHIFT_DP_ARRAY(VAR,TMP_VAR)
!           CALL gather (TMP_VAR,array2,root)
!         ELSE
            CALL gather (MASTER_FLAG,array2,root)
!         ENDIF
!         call gather (VAR,array2,root)  !//d pnicol

!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         if (myPE.eq.PE_IO) then

            call convert_to_io_i(array2,array1,ijkmax2)

            IF(ALLOCATED(LIST_FL_CELLS)) DEALLOCATE(LIST_FL_CELLS)
            IF(ALLOCATED(LIST_CT_CELLS)) DEALLOCATE(LIST_CT_CELLS)
            IF(ALLOCATED(LIST_BL_CELLS)) DEALLOCATE(LIST_BL_CELLS)

            ALLOCATE(LIST_FL_CELLS(IJKMAX2))
            ALLOCATE(LIST_CT_CELLS(IJKMAX2))
            ALLOCATE(LIST_BL_CELLS(IJKMAX2))

            N_FL_CELLS = 0
            N_CT_CELLS = 0
            N_BL_CELLS = 0

            DO IJK=1,IJKMAX2
               IF(array1(IJK)==1111) THEN
                  N_FL_CELLS = N_FL_CELLS + 1
                  LIST_FL_CELLS(N_FL_CELLS) = IJK
               ELSEIF(NO_K.AND.array1(IJK)==3331) THEN
                  N_BL_CELLS = N_BL_CELLS + 1
                  LIST_BL_CELLS(N_BL_CELLS) = IJK
               ELSEIF(DO_K.AND.array1(IJK)==3333) THEN
                  N_BL_CELLS = N_BL_CELLS + 1
                  LIST_BL_CELLS(N_BL_CELLS) = IJK
               ELSE
                  N_CT_CELLS = N_CT_CELLS + 1
                  LIST_CT_CELLS(N_CT_CELLS) = IJK
               ENDIF
            ENDDO

            WRITE(UNIT_MSH, REC=REC_NC) N_FL_CELLS,N_CT_CELLS,N_BL_CELLS
            CALL OUT_BIN_512i (UNIT_MSH, array1, IJKMAX2, NEXT_REC)
            CALL OUT_BIN_512i (UNIT_MSH, LIST_FL_CELLS(1:N_FL_CELLS), N_FL_CELLS, NEXT_REC)
            CALL OUT_BIN_512i (UNIT_MSH, LIST_CT_CELLS(1:N_CT_CELLS), N_CT_CELLS, NEXT_REC)
            CALL OUT_BIN_512i (UNIT_MSH, LIST_BL_CELLS(1:N_BL_CELLS), N_BL_CELLS, NEXT_REC)
         end if

      else

         ! IF(RE_INDEXING) THEN
         !    CALL UNSHIFT_DP_ARRAY(VAR,TMP_VAR)
         !    CALL OUT_BIN_512 (UNIT_RES, TMP_VAR, size(TMP_VAR), NEXT_REC)
         ! ELSE
         !    CALL OUT_BIN_512 (UNIT_RES, var, size(var), NEXT_REC)
         ! ENDIF
!         CALL OUT_BIN_512 (UNIT_RES, var, size(var), NEXT_REC)

      end if


100   FORMAT(1X,A)
      RETURN

      End subroutine gatherWriteMasterList
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine:                                                         C
!  Purpose:                                                            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      subroutine gatherWriteMesh_dpc(VAR, COMPACT, NEXT_REC)

! Modules
!---------------------------------------------------------------------//
      USE geometry
      USE funits
      USE cdist
      USE compar
      USE mpi_utility
      USE sendrecv
      USE cutcell
      USE in_binary_512
      USE out_bin_512_mod, only: out_bin_512
      USE param, only: dimension_3
      USE set_increments_mod, only: unshift_dp_array
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      double precision, dimension(ijkmax2) :: array1
      double precision, dimension(ijkmax3) :: array2
      double precision, dimension(ijkmax2) :: array3
      double precision, dimension(DIMENSION_3) :: VAR
      LOGICAL :: COMPACT ! Flag to write compact form (shorter) of VAR
      INTEGER :: NEXT_REC
      INTEGER :: IJK

! Local variables
!---------------------------------------------------------------------//
      double precision, dimension(DIMENSION_3) :: TMP_VAR

!---------------------------------------------------------------------//

!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      if (.not.bDist_IO) then

!         IF(RE_INDEXING) THEN
!            CALL UNSHIFT_DP_ARRAY(VAR,TMP_VAR)
!            CALL gather (TMP_VAR,array2,root)
!         ELSE
            CALL gather (VAR,array2,root)
!         ENDIF
!         call gather (VAR,array2,root)  !//d pnicol

!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         if (myPE.eq.PE_IO) then
            call convert_to_io_dp(array2,array1,ijkmax2)

            IF(COMPACT) THEN
               DO IJK=1,N_CT_CELLS
                  array3(IJK) = array1(LIST_CT_CELLS(IJK))
               ENDDO

               CALL OUT_BIN_512 (UNIT_MSH, array3, N_CT_CELLS, NEXT_REC)
            ELSE
               CALL OUT_BIN_512 (UNIT_MSH, array1, IJKMAX2, NEXT_REC)
            ENDIF

         end if

      else

!         IF(RE_INDEXING) THEN
!            CALL UNSHIFT_DP_ARRAY(VAR,TMP_VAR)
!            CALL OUT_BIN_512 (UNIT_RES, TMP_VAR, size(TMP_VAR), NEXT_REC)
!         ELSE
            CALL OUT_BIN_512 (UNIT_RES, var, size(var), NEXT_REC)
!         ENDIF
!         CALL OUT_BIN_512 (UNIT_RES, var, size(var), NEXT_REC)

      end if

      End subroutine gatherWriteMesh_dpc
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine:                                                         C
!  Purpose:                                                            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      subroutine gatherWriteMesh_ic(VAR, COMPACT, NEXT_REC)

! Modules
!---------------------------------------------------------------------//
      USE geometry
      USE funits
      USE cdist
      USE compar
      USE mpi_utility
      USE sendrecv
      USE cutcell
      USE in_binary_512i
      USE param, only: dimension_3
      USE set_increments_mod, only: unshift_dp_array
      USE out_bin_512i_mod, only: out_bin_512i
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      integer, dimension(ijkmax2) :: array1
      integer, dimension(ijkmax3) :: array2
      integer, dimension(ijkmax2) :: array3
      integer, dimension(DIMENSION_3) :: VAR
      LOGICAL :: COMPACT ! Flag to write compact form (shorter) of VAR
      INTEGER :: NEXT_REC
      INTEGER :: IJK

! Local variables
!---------------------------------------------------------------------//
      integer, dimension(DIMENSION_3) :: TMP_VAR

!---------------------------------------------------------------------//

!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      if (.not.bDist_IO) then

!         IF(RE_INDEXING) THEN
            ! CALL UNSHIFT_DP_ARRAY(VAR,TMP_VAR)
            ! CALL gather (TMP_VAR,array2,root)
!         ELSE
            CALL gather (VAR,array2,root)
!         ENDIF
!         call gather (VAR,array2,root)  !//d pnicol

!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         if (myPE.eq.PE_IO) then
            call convert_to_io_i(array2,array1,ijkmax2)

            IF(COMPACT) THEN
               DO IJK=1,N_CT_CELLS
                  array3(IJK) = array1(LIST_CT_CELLS(IJK))
               ENDDO

               CALL OUT_BIN_512i (UNIT_MSH, array3, N_CT_CELLS, NEXT_REC)
            ELSE
               CALL OUT_BIN_512i (UNIT_MSH, array1, IJKMAX2, NEXT_REC)
            ENDIF

         end if

      else

!         IF(RE_INDEXING) THEN
            ! CALL UNSHIFT_DP_ARRAY(VAR,TMP_VAR)
            ! CALL OUT_BIN_512i (UNIT_RES, TMP_VAR, size(TMP_VAR), NEXT_REC)
!         ELSE
            CALL OUT_BIN_512i (UNIT_RES, var, size(var), NEXT_REC)
!         ENDIF
!         CALL OUT_BIN_512 (UNIT_RES, var, size(var), NEXT_REC)

      end if

      End subroutine gatherWriteMesh_ic
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine:                                                         C
!  Purpose:                                                            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      subroutine gatherWriteMesh_lc(VAR, COMPACT, NEXT_REC)

! Modules
!---------------------------------------------------------------------//
      USE geometry
      USE funits
      USE cdist
      USE compar
      USE mpi_utility
      USE sendrecv
      USE cutcell
      USE in_binary_512l
      USE param, only: dimension_3
      USE set_increments_mod, only: unshift_dp_array
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      logical, dimension(ijkmax2) :: array1
      logical, dimension(ijkmax3) :: array2
      logical, dimension(ijkmax2) :: array3
      logical, dimension(DIMENSION_3) :: VAR
      LOGICAL :: COMPACT ! Flag to write compact form (shorter) of VAR
      INTEGER :: NEXT_REC
      INTEGER :: IJK

! Local variables
!---------------------------------------------------------------------//
      logical, dimension(DIMENSION_3) :: TMP_VAR

!---------------------------------------------------------------------//

!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      if (.not.bDist_IO) then

!         IF(RE_INDEXING) THEN
            ! CALL UNSHIFT_DP_ARRAY(VAR,TMP_VAR)
            ! CALL gather (TMP_VAR,array2,root)
!         ELSE
            CALL gather (VAR,array2,root)
!         ENDIF
!         call gather (VAR,array2,root)  !//d pnicol

!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         if (myPE.eq.PE_IO) then
            call convert_to_io_l(array2,array1,ijkmax2)

            IF(COMPACT) THEN
               DO IJK=1,N_CT_CELLS
                  array3(IJK) = array1(LIST_CT_CELLS(IJK))
               ENDDO

               CALL OUT_BIN_512l (UNIT_MSH, array3, N_CT_CELLS, NEXT_REC)
            ELSE
               CALL OUT_BIN_512l (UNIT_MSH, array1, IJKMAX2, NEXT_REC)
            ENDIF

         end if

      else

!         IF(RE_INDEXING) THEN
            ! CALL UNSHIFT_DP_ARRAY(VAR,TMP_VAR)
            ! CALL OUT_BIN_512l (UNIT_RES, TMP_VAR, size(TMP_VAR), NEXT_REC)
!         ELSE
            CALL OUT_BIN_512l (UNIT_RES, var, size(var), NEXT_REC)
!         ENDIF
!         CALL OUT_BIN_512 (UNIT_RES, var, size(var), NEXT_REC)

      end if

      End subroutine gatherWriteMesh_lc


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: READ_MESH_DATA                                         C
!  Purpose: Reads mesh data at beginning of a run, if it was generated C
!           in a previous PPO run.                                     C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 28-Jun-17  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE READ_MESH_DATA

      USE param
      USE param1
      USE parallel
      USE constant
      USE run
      USE toleranc
      USE geometry
      USE indices
      USE compar
      USE sendrecv
      USE quadric
      USE cutcell
      USE polygon
      USE stl
      USE bc
      USE vtk

      USE mpi_utility
      USE functions
      USE funits
      USE in_binary_512
      USE in_binary_512i
      USE in_binary_512r
      USE in_binary_512l
      USE error_manager

      IMPLICIT NONE
!      CHARACTER (LEN=*) :: TYPE_OF_CELL
      INTEGER :: IJK,I,J,K,L,GC,IJK_IO
      INTEGER :: IM,IP,JM,JP,KM,KP,IMJK,IPJK,IJMK,IJPK,IJKM,IJKP
      INTEGER :: IJPKP,IPJKP,IPJPK

      DOUBLE PRECISION, DIMENSION(DIMENSION_3) :: Xint,Yint,Zint
      DOUBLE PRECISION, DIMENSION(DIMENSION_3) :: Xintflag,Yintflag,Zintflag
      DOUBLE PRECISION, DIMENSION(DIMENSION_3) :: Snapflag, bc_idflag

      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: DP_GLOBAL_COORDS_OF_POINTS

      INTEGER :: NN,I1,I2,J1,J2,K1,K2,NODE,MSH_IJKMAX3
      INTEGER :: PACKED_CONNECTIVITY_SIZE

      INTEGER, DIMENSION(:), ALLOCATABLE :: PACKED_CONNECTIVITY
      INTEGER, DIMENSION(IJKMAX3) :: MASTER_FLAG
      INTEGER, DIMENSION(IJKMAX3) :: GLOBAL_IJK_OF_IO_IJK ! Convert IO IJK into global IJK

      INTEGER :: MESH_FILE_UNIT
      CHARACTER(LEN=255) :: MESH_FILE_NAME
      CHARACTER(LEN=255) :: DUMMY_CHAR

      INTEGER :: NXE,NYE,NZE,NFAT
      DOUBLE PRECISION :: TMP_DP
      INTEGER :: NEXT_REC


      IF(.NOT.GENERATE_MESH) THEN

         REC_NP   = 7
         REC_NC   = 8
         REC_CP   = 10
         REC_BULK = 100

         CALL CHECK_CONSISTENCY_BETWEEN_MESH_AND_MFX

         WRITE(ERR_MSG,100) ' Reading Mesh connectivity and flags ...'
         CALL LOG_STATUS()

! Read data size
         IF(MyPE == PE_IO) THEN
            READ(UNIT_MSH, REC=REC_NP)  NUMBER_OF_POINTS,GLOBAL_NUMBER_OF_NEW_POINTS,NUMBER_OF_VTK_CELLS,PACKED_CONNECTIVITY_SIZE,MSH_IJKMAX3
         ENDIF

! Allocate arrays
         IF(ALLOCATED(GLOBAL_X_NEW_POINT)) DEALLOCATE(GLOBAL_X_NEW_POINT)
         IF(ALLOCATED(GLOBAL_Y_NEW_POINT)) DEALLOCATE(GLOBAL_Y_NEW_POINT)
         IF(ALLOCATED(GLOBAL_Z_NEW_POINT)) DEALLOCATE(GLOBAL_Z_NEW_POINT)
         IF(ALLOCATED(PACKED_CONNECTIVITY)) DEALLOCATE(PACKED_CONNECTIVITY)
         IF(ALLOCATED(BELONGS_TO_VTK_SUBDOMAIN)) DEALLOCATE(BELONGS_TO_VTK_SUBDOMAIN)
         IF(ALLOCATED(GLOBAL_NUMBER_OF_NODES)) DEALLOCATE(GLOBAL_NUMBER_OF_NODES)
         IF(ALLOCATED(GLOBAL_CONNECTIVITY)) DEALLOCATE(GLOBAL_CONNECTIVITY)

         IF(MyPE == PE_IO) THEN
            ALLOCATE(GLOBAL_X_NEW_POINT(GLOBAL_NUMBER_OF_NEW_POINTS))
            ALLOCATE(GLOBAL_Y_NEW_POINT(GLOBAL_NUMBER_OF_NEW_POINTS))
            ALLOCATE(GLOBAL_Z_NEW_POINT(GLOBAL_NUMBER_OF_NEW_POINTS))
            ALLOCATE(PACKED_CONNECTIVITY(PACKED_CONNECTIVITY_SIZE))
            ALLOCATE (BELONGS_TO_VTK_SUBDOMAIN(ijkmax3))
            ALLOCATE (GLOBAL_NUMBER_OF_NODES(ijkmax3))
            ALLOCATE (GLOBAL_CONNECTIVITY(ijkmax3,15))
         ELSE
            ALLOCATE(GLOBAL_X_NEW_POINT(1))
            ALLOCATE(GLOBAL_Y_NEW_POINT(1))
            ALLOCATE(GLOBAL_Z_NEW_POINT(1))
            ALLOCATE(PACKED_CONNECTIVITY(1))
            ALLOCATE (BELONGS_TO_VTK_SUBDOMAIN(1))
            ALLOCATE (GLOBAL_NUMBER_OF_NODES(1))
            ALLOCATE (GLOBAL_CONNECTIVITY(1,15))

         ENDIF


! Read data. The initial value of NEXT_REC must match the one define at the top
! of SAVE_MESH_DATA
         NEXT_REC = REC_BULK


         IF(MyPE == PE_IO) THEN
! Coordinates of the new point (definig cut cells)
            CALL IN_BIN_512 (UNIT_MSH, GLOBAL_X_NEW_POINT(:),GLOBAL_NUMBER_OF_NEW_POINTS, NEXT_REC)
            CALL IN_BIN_512 (UNIT_MSH, GLOBAL_Y_NEW_POINT(:),GLOBAL_NUMBER_OF_NEW_POINTS, NEXT_REC)
            CALL IN_BIN_512 (UNIT_MSH, GLOBAL_Z_NEW_POINT(:),GLOBAL_NUMBER_OF_NEW_POINTS, NEXT_REC)

! Connectivity
            CALL IN_BIN_512I (UNIT_MSH, PACKED_CONNECTIVITY,PACKED_CONNECTIVITY_SIZE, NEXT_REC)

! Unpack global connectivity
! The packed connectivity is a list of integers:
! IJK,GLOBAL_NUMBER_OF_NODES(IJK),GLOBAL_CLEANED_CONNECTIVITY(IJK,L)-1,L=1,GLOBAL_NUMBER_OF_NODES(IJK) for each fluid cell IJK

! prepare mapping to convert IO index (from the MSH file) to global index
            do k = 1,kmax2
            do j = 1,jmax2
            do i = 1,imax2
               ijk    = funijk_gl(i,j,k)
               ijk_io = funijk_io(i,j,k)
               GLOBAL_IJK_OF_IO_IJK(ijk_io) = ijk
            end do
            end do
            end do

            BELONGS_TO_VTK_SUBDOMAIN(:) = .FALSE.
            I = 1
            DO WHILE (I<=PACKED_CONNECTIVITY_SIZE)
               IJK_IO = PACKED_CONNECTIVITY(I)
               IJK = GLOBAL_IJK_OF_IO_IJK(IJK_IO)
               BELONGS_TO_VTK_SUBDOMAIN(IJK) = .TRUE.
               I = I + 1
               GLOBAL_NUMBER_OF_NODES(IJK) = PACKED_CONNECTIVITY(I)
               I = I +1
               DO L=1,GLOBAL_NUMBER_OF_NODES(IJK)
                  GC = PACKED_CONNECTIVITY(I) ! Expressed in terms of IO index
                  IF(GC>MSH_IJKMAX3) THEN ! This is a new point (not a cell corner)
                     ! Adjustment due to different IJKMAX3 in MSH file and current partition
                     GLOBAL_CONNECTIVITY(IJK,L) = GC + IJKMAX3 - MSH_IJKMAX3
                  ELSE
                     GLOBAL_CONNECTIVITY(IJK,L) = GLOBAL_IJK_OF_IO_IJK(GC)
                  ENDIF

                  I = I + 1
               ENDDO
            ENDDO

         ENDIF  ! PE_IO


! Consolidate flags into Tne master flag
         CALL readScatterMasterList(MASTER_FLAG, NEXT_REC)
         CALL SET_FLAGS_FROM_MASTER(MASTER_FLAG)
         CALL SET_DEFAULT_GLOBAL_CONNECTIVITY

! Flags
         CALL readScatterMesh_ic(FLAG, 100, .FALSE., NEXT_REC)

         CALL readScatterMesh_ic(FLAG_E, 0, .FALSE., NEXT_REC)
         CALL readScatterMesh_ic(FLAG_N, 0, .FALSE., NEXT_REC)
         IF(DO_K) CALL readScatterMesh_ic(FLAG_T, 0, .FALSE., NEXT_REC)

! BC ID
         CALL readScatterMesh_ic(BC_ID, 0, .TRUE., NEXT_REC)
         CALL readScatterMesh_ic(BC_U_ID, 0, .TRUE., NEXT_REC)
         CALL readScatterMesh_ic(BC_V_ID, 0, .TRUE., NEXT_REC)
         IF(DO_K) CALL readScatterMesh_ic(BC_W_ID, 0, .TRUE., NEXT_REC)

! problematic cells
          CALL readScatterMesh_lc(SMALL_CELL_AT, .FALSE., .TRUE., NEXT_REC)
          CALL readScatterMesh_lc(WALL_U_AT, .FALSE., .TRUE., NEXT_REC)
          CALL readScatterMesh_lc(WALL_V_AT, .FALSE., .TRUE., NEXT_REC)
          IF(DO_K) CALL readScatterMesh_lc(WALL_W_AT, .FALSE., .TRUE., NEXT_REC)

! F_AT,SNAP
         CALL readScatterMesh_dpc(F_AT, ZERO, .FALSE., NEXT_REC)
         CALL readScatterMesh_lc(SNAP, .FALSE., .FALSE., NEXT_REC)

! Flag for Garg interpolation
         if(.not.allocated(SCALAR_NODE_ATWALL)) allocate(SCALAR_NODE_ATWALL(DIMENSION_3))
         CALL readScatterMesh_lc(SCALAR_NODE_ATWALL, .FALSE., .FALSE., NEXT_REC)
!======================================================================
!  Scalar cells
!======================================================================
         WRITE(ERR_MSG,100) ' Reading Mesh info for Scalar cells...'
         CALL LOG_STATUS()

! Cell volume, areas
         CALL readScatterMesh_dpc(VOL, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(AXY, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(AXZ, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(AYZ, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Area_cut, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Aspect_ratio, ZERO, .TRUE., NEXT_REC)

! Cut face Normal and reference points, wall distance

         CALL readScatterMesh_dpc(Normal_S(:,1), ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Normal_S(:,2), ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Normal_S(:,3), ZERO, .TRUE., NEXT_REC)

         CALL readScatterMesh_dpc(REFP_S(:,1), ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(REFP_S(:,2), ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(REFP_S(:,3), ZERO, .TRUE., NEXT_REC)

         CALL readScatterMesh_dpc(DELH_Scalar, ZERO, .TRUE., NEXT_REC)

! Consolidate blocked cells
         DO IJK = IJKSTART3, IJKEND3,-1
            IF(BLOCKED_CELL_AT(IJK)) THEN
               STANDARD_CELL_AT(IJK) = .FALSE.
               CUT_CELL_AT(IJK)      = .FALSE.

               FLAG(IJK)             = 100

               VOL(IJK)              = ZERO
               AXY(IJK)              = ZERO
               AXZ(IJK)              = ZERO
               AYZ(IJK)              = ZERO

               AXY(BOTTOM_OF(IJK))   = ZERO
               AXZ(SOUTH_OF(IJK))    = ZERO
               AYZ(WEST_OF(IJK))     = ZERO
            ENDIF
         ENDDO

!======================================================================
!  U-Momentum cells
!======================================================================
         WRITE(ERR_MSG,100) ' Reading Mesh info for U-Momentum cells...'
         CALL LOG_STATUS()

! Cell volume, areas
         CALL readScatterMesh_dpc(VOL_U, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(AXY_U, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(AXZ_U, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(AYZ_U, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Area_U_cut, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Aspect_ratio_U, ZERO, .TRUE., NEXT_REC)

! Cut face Normal and reference points, wall distance
         CALL readScatterMesh_dpc(Normal_U(:,1), ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Normal_U(:,2), ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Normal_U(:,3), ZERO, .TRUE., NEXT_REC)

         CALL readScatterMesh_dpc(REFP_U(:,1), ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(REFP_U(:,2), ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(REFP_U(:,3), ZERO, .TRUE., NEXT_REC)

         CALL readScatterMesh_dpc(DELH_U, ZERO, .TRUE., NEXT_REC)

! Velocity nodes
         CALL readScatterMesh_dpc(X_U, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Y_U, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Z_U, ZERO, .TRUE., NEXT_REC)

         CALL readScatterMesh_dpc(X_U_ec, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Y_U_ec, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Z_U_ec, ZERO, .TRUE., NEXT_REC)

         CALL readScatterMesh_dpc(X_U_nc, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Y_U_nc, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Z_U_nc, ZERO, .TRUE., NEXT_REC)

         IF(DO_K) CALL readScatterMesh_dpc(X_U_tc, ZERO, .TRUE., NEXT_REC)
         IF(DO_K) CALL readScatterMesh_dpc(Y_U_tc, ZERO, .TRUE., NEXT_REC)
         IF(DO_K) CALL readScatterMesh_dpc(Z_U_tc, ZERO, .TRUE., NEXT_REC)

         CALL readScatterMesh_dpc(DELX_Ue, ONE , .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(DELX_Uw, ONE , .TRUE., NEXT_REC)

         CALL readScatterMesh_dpc(DELX_Ve, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(DELX_Vw, ZERO, .TRUE., NEXT_REC)

         IF(DO_K) CALL readScatterMesh_dpc(DELX_We, ZERO, .TRUE., NEXT_REC)
         IF(DO_K) CALL readScatterMesh_dpc(DELX_Ww, ZERO, .TRUE., NEXT_REC)

         Xn_U_int = UNDEFINED
         CALL readScatterMesh_dpc(Xn_U_int, ZERO, .TRUE., NEXT_REC)

!======================================================================
!  V-Momentum cells
!======================================================================
         WRITE(ERR_MSG,100) ' Reading Mesh info for V-Momentum cells...'
         CALL LOG_STATUS()

! Cell volume, areas
         CALL readScatterMesh_dpc(VOL_V, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(AXY_V, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(AXZ_V, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(AYZ_V, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Area_V_cut, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Aspect_ratio_V, ZERO, .TRUE., NEXT_REC)

! Cut face Normal and reference points, wall distance
         CALL readScatterMesh_dpc(Normal_V(:,1), ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Normal_V(:,2), ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Normal_V(:,3), ZERO, .TRUE., NEXT_REC)

         CALL readScatterMesh_dpc(REFP_V(:,1), ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(REFP_V(:,2), ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(REFP_V(:,3), ZERO, .TRUE., NEXT_REC)

         CALL readScatterMesh_dpc(DELH_V, ZERO, .TRUE., NEXT_REC)

! Velocity nodes
         CALL readScatterMesh_dpc(X_V, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Y_V, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Z_V, ZERO, .TRUE., NEXT_REC)

         CALL readScatterMesh_dpc(X_V_ec, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Y_V_ec, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Z_V_ec, ZERO, .TRUE., NEXT_REC)

         CALL readScatterMesh_dpc(X_V_nc, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Y_V_nc, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(Z_V_nc, ZERO, .TRUE., NEXT_REC)

         IF(DO_K) CALL readScatterMesh_dpc(X_V_tc, ZERO, .TRUE., NEXT_REC)
         IF(DO_K) CALL readScatterMesh_dpc(Y_V_tc, ZERO, .TRUE., NEXT_REC)
         IF(DO_K) CALL readScatterMesh_dpc(Z_V_tc, ZERO, .TRUE., NEXT_REC)

         CALL readScatterMesh_dpc(DELY_Un, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(DELY_Us, ZERO, .TRUE., NEXT_REC)

         CALL readScatterMesh_dpc(DELY_Vn, ZERO, .TRUE., NEXT_REC)
         CALL readScatterMesh_dpc(DELY_Vs, ZERO, .TRUE., NEXT_REC)

         IF(DO_K) CALL readScatterMesh_dpc(DELY_Wn, ZERO, .TRUE., NEXT_REC)
         IF(DO_K) CALL readScatterMesh_dpc(DELY_Ws, ZERO, .TRUE., NEXT_REC)

         Ye_V_int = UNDEFINED
         CALL readScatterMesh_dpc(Ye_V_int, ZERO, .TRUE., NEXT_REC)

         IF(DO_K) THEN
!======================================================================
!  W-Momentum cells
!======================================================================
         WRITE(ERR_MSG,100) ' Reading Mesh info for W-Momentum cells...'
         CALL LOG_STATUS()

! Cell volume, areas
            CALL readScatterMesh_dpc(VOL_W, ZERO,  .TRUE.,NEXT_REC)
            CALL readScatterMesh_dpc(AXY_W, ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(AXZ_W, ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(AYZ_W, ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(Area_W_cut, ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(Aspect_ratio_W, ZERO, .TRUE., NEXT_REC)

! Cut face Normal and reference points, wall distance
            CALL readScatterMesh_dpc(Normal_W(:,1), ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(Normal_W(:,2), ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(Normal_W(:,3), ZERO, .TRUE., NEXT_REC)

            CALL readScatterMesh_dpc(REFP_W(:,1), ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(REFP_W(:,2), ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(REFP_W(:,3), ZERO, .TRUE., NEXT_REC)

            CALL readScatterMesh_dpc(DELH_W, ZERO, .TRUE., NEXT_REC)

! Velocity nodes
            CALL readScatterMesh_dpc(X_W, ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(Y_W, ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(Z_W, ZERO, .TRUE., NEXT_REC)

            CALL readScatterMesh_dpc(X_W_ec, ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(Y_W_ec, ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(Z_W_ec, ZERO, .TRUE., NEXT_REC)

            CALL readScatterMesh_dpc(X_W_nc, ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(Y_W_nc, ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(Z_W_nc, ZERO, .TRUE., NEXT_REC)

            CALL readScatterMesh_dpc(X_W_tc, ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(Y_W_tc, ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(Z_W_tc, ZERO, .TRUE., NEXT_REC)

            CALL readScatterMesh_dpc(DELZ_Ut, ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(DELZ_Ub, ZERO, .TRUE., NEXT_REC)

            CALL readScatterMesh_dpc(DELZ_Vt, ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(DELZ_Vb, ZERO, .TRUE., NEXT_REC)

            CALL readScatterMesh_dpc(DELZ_Wt, ZERO, .TRUE., NEXT_REC)
            CALL readScatterMesh_dpc(DELZ_Wb, ZERO, .TRUE., NEXT_REC)

            Zt_W_int = UNDEFINED
            CALL readScatterMesh_dpc(Zt_W_int, ZERO, .TRUE., NEXT_REC)


         ENDIF   ! DO_K

      ENDIF

!      IF(myPE==PE_IO) CLOSE(UNIT_MSH)

      RETURN

100   FORMAT(1X,A)
110   FORMAT(1X,A,I8,A,I8)
120   FORMAT(1X,A,I8)
200   FORMAT(1X,4(I8),1X,D16.8)

      END SUBROUTINE READ_MESH_DATA

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: CHECK_CONSISTENCY_BETWEEN_MESH_AND_MFX                  C
!  Purpose: Check data is consistent between mesh file and .mfx file   C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 28-Jan-20  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE CHECK_CONSISTENCY_BETWEEN_MESH_AND_MFX

      use cutcell
      use quadric
      use error_manager
      USE funits
      USE in_binary_512
      USE in_binary_512i
      USE toleranc

      IMPLICIT NONE
! Data stored in MEsh file. This needs to match the corresponding data in the
! .mfx file
      INTEGER :: NN
      CHARACTER (LEN=32) :: SUBNN
      CHARACTER (LEN=284) :: KW_NAME
      INTEGER :: NEXT_REC
      INTEGER :: MSH_IMAX, MSH_JMAX, MSH_KMAX
      LOGICAL :: MSH_DO_K, MSH_CARTESIAN_GRID, MSH_RE_INDEXING, MSH_USE_STL
      DOUBLE PRECISION :: MSH_x_min, MSH_x_max, MSH_y_min, MSH_y_max, MSH_z_min, MSH_z_max
      DOUBLE PRECISION :: MSH_OUT_STL_VALUE, MSH_TX_STL, MSH_TY_STL, MSH_TZ_STL, MSH_SCALE_STL
      DOUBLE PRECISION :: MSH_TOL_SMALL_CELL, MSH_TOL_SMALL_AREA, MSH_TOL_MERGE, MSH_FAC_DIM_MAX_CUT_CELL
      DOUBLE PRECISION, DIMENSION(3) :: MSH_TOL_SNAP(1:3)
      DOUBLE PRECISION :: MSH_TOL_STL, MSH_STL_SMALL_ANGLE, MSH_TOL_STL_DP, MSH_TOL_DELH
      INTEGER :: MSH_NCPX, MSH_NCPY,MSH_NCPZ
      INTEGER :: MSH_STL_BC_ID, MSH_ITERMAX_INT, MSH_DIM_FACETS_PER_CELL
      DOUBLE PRECISION, DIMENSION(0:MAX_CP) ::  MSH_CPX,MSH_CPY,MSH_CPZ  ! Control point location
      DOUBLE PRECISION, DIMENSION(MAX_CP)   ::  MSH_ERX,MSH_ERY,MSH_ERZ  ! Expansion Ratio
      INTEGER, DIMENSION(MAX_CP)            ::  MSH_NCX,MSH_NCY,MSH_NCZ  ! Number of cell in a segment
      DOUBLE PRECISION, DIMENSION(MAX_CP)   ::  MSH_FIRST_DX,MSH_LAST_DX! DX values at segment extremities
      DOUBLE PRECISION, DIMENSION(MAX_CP)   ::  MSH_FIRST_DY,MSH_LAST_DY! DY values at segment extremities
      DOUBLE PRECISION, DIMENSION(MAX_CP)   ::  MSH_FIRST_DZ,MSH_LAST_DZ! DZ values at segment extremities
      INTEGER :: BCV,MSH_BCV, MSH_NUMBER_OF_BC_PATCHES, NUMBER_OF_BC_PATCHES
      DOUBLE PRECISION :: msh_bc_x_w,msh_bc_x_e,msh_bc_y_s,msh_bc_y_n,msh_bc_z_b,msh_bc_z_t
      CHARACTER(LEN=16) MSH_BC_TYPE
      INTEGER :: IOS

      IOS = 0



      WRITE(ERR_MSG,100) ' Checking consistency between mesh data and .mfx file...'
      CALL LOG_STATUS()

      IF (myPE == PE_IO) THEN
         READ(UNIT_MSH, REC=3) MSH_IMAX, MSH_JMAX, MSH_KMAX, MSH_DO_K, MSH_X_MIN, MSH_X_MAX, MSH_Y_MIN, MSH_Y_MAX, MSH_Z_MIN, MSH_Z_MAX, &
                               MSH_NCPX,MSH_NCPY,MSH_NCPZ
         READ(UNIT_MSH, REC=4) MSH_CARTESIAN_GRID, MSH_RE_INDEXING, MSH_USE_STL, MSH_STL_BC_ID, MSH_OUT_STL_VALUE, &
                               MSH_TX_STL, MSH_TY_STL, MSH_TZ_STL,MSH_SCALE_STL
         READ(UNIT_MSH, REC=5) MSH_TOL_SMALL_CELL, MSH_TOL_SMALL_AREA, MSH_TOL_MERGE, MSH_TOL_SNAP(1:3), MSH_FAC_DIM_MAX_CUT_CELL, &
                               MSH_ITERMAX_INT, MSH_TOL_STL, MSH_STL_SMALL_ANGLE, MSH_TOL_STL_DP, MSH_DIM_FACETS_PER_CELL, MSH_TOL_DELH
         READ(UNIT_MSH, REC=6) MSH_NUMBER_OF_BC_PATCHES, REC_BC

! Control points
         NEXT_REC = REC_CP
         IF(NCPX>0) THEN
            CALL IN_BIN_512  (UNIT_MSH, MSH_CPX(0:NCPX),MSH_NCPX+1, NEXT_REC)
            CALL IN_BIN_512I (UNIT_MSH, MSH_NCX(:),MSH_NCPX, NEXT_REC)
            CALL IN_BIN_512  (UNIT_MSH, MSH_ERX(:),MSH_NCPX, NEXT_REC)
            CALL IN_BIN_512  (UNIT_MSH, MSH_FIRST_DX(:),MSH_NCPX, NEXT_REC)
            CALL IN_BIN_512  (UNIT_MSH, MSH_LAST_DX(:),MSH_NCPX, NEXT_REC)
         ENDIF
         IF(NCPY>0) THEN
            CALL IN_BIN_512  (UNIT_MSH, MSH_CPY(0:NCPY),MSH_NCPY+1, NEXT_REC)
            CALL IN_BIN_512I (UNIT_MSH, MSH_NCY(:),MSH_NCPY, NEXT_REC)
            CALL IN_BIN_512  (UNIT_MSH, MSH_ERY(:),MSH_NCPY, NEXT_REC)
            CALL IN_BIN_512  (UNIT_MSH, MSH_FIRST_DY(:),MSH_NCPY, NEXT_REC)
            CALL IN_BIN_512  (UNIT_MSH, MSH_LAST_DY(:),MSH_NCPY, NEXT_REC)
         ENDIF
         IF(NCPZ>0) THEN
            CALL IN_BIN_512  (UNIT_MSH, MSH_CPZ(0:NCPZ),MSH_NCPZ+1, NEXT_REC)
            CALL IN_BIN_512I (UNIT_MSH, MSH_NCZ(:),MSH_NCPZ, NEXT_REC)
            CALL IN_BIN_512  (UNIT_MSH, MSH_ERZ(:),MSH_NCPZ, NEXT_REC)
            CALL IN_BIN_512  (UNIT_MSH, MSH_FIRST_DZ(:),MSH_NCPZ, NEXT_REC)
            CALL IN_BIN_512  (UNIT_MSH, MSH_LAST_DZ(:),MSH_NCPZ, NEXT_REC)
         ENDIF

         CALL CHECK_INT_KW_CONSISTENCY('IMAX',msh_imax,imax,IOS)
         CALL CHECK_INT_KW_CONSISTENCY('JMAX',msh_jmax,jmax,IOS)
         CALL CHECK_INT_KW_CONSISTENCY('KMAX',msh_kmax,kmax,IOS)

         CALL CHECK_BOOL_KW_CONSISTENCY('DO_K',msh_do_k,do_k,IOS)

         CALL CHECK_DP_KW_CONSISTENCY('X_MIN',msh_x_min,x_min,IOS)
         CALL CHECK_DP_KW_CONSISTENCY('X_MAX',msh_x_max,x_max,IOS)
         CALL CHECK_DP_KW_CONSISTENCY('Y_MIN',msh_y_min,y_min,IOS)
         CALL CHECK_DP_KW_CONSISTENCY('Y_MAX',msh_y_max,y_max,IOS)
         CALL CHECK_DP_KW_CONSISTENCY('Z_MIN',msh_z_min,z_min,IOS)
         CALL CHECK_DP_KW_CONSISTENCY('Z_MAX',msh_z_max,z_max,IOS)

! Number of control points
         CALL CHECK_INT_KW_CONSISTENCY('NCPX',msh_NCPX,NCPX,IOS)
         CALL CHECK_INT_KW_CONSISTENCY('NCPY',msh_NCPY,NCPY,IOS)
         CALL CHECK_INT_KW_CONSISTENCY('NCPZ',msh_NCPZ,NCPZ,IOS)

! Control point settings
         DO NN=1,NCPX
            WRITE(SUBNN,*)NN
            KW_NAME = 'CPX('//TRIM(ADJUSTL(SUBNN))//')'
            CALL CHECK_DP_KW_CONSISTENCY(KW_NAME,MSH_CPX(NN),CPX(NN),IOS)
            KW_NAME = 'NCX('//TRIM(ADJUSTL(SUBNN))//')'
            CALL CHECK_INT_KW_CONSISTENCY(KW_NAME,MSH_NCX(NN),NCX(NN),IOS)
            KW_NAME = 'ERX('//TRIM(ADJUSTL(SUBNN))//')'
            CALL CHECK_DP_KW_CONSISTENCY(KW_NAME,MSH_ERX(NN),ERX(NN),IOS)
            KW_NAME = 'FIRST_DX('//TRIM(ADJUSTL(SUBNN))//')'
            CALL CHECK_DP_KW_CONSISTENCY(KW_NAME,MSH_FIRST_DX(NN),FIRST_DX(NN),IOS)
            KW_NAME = 'LAST_DX('//TRIM(ADJUSTL(SUBNN))//')'
            CALL CHECK_DP_KW_CONSISTENCY(KW_NAME,MSH_LAST_DX(NN),LAST_DX(NN),IOS)
         ENDDO

         DO NN=1,NCPY
            WRITE(SUBNN,*)NN
            KW_NAME = 'CPY('//TRIM(ADJUSTL(SUBNN))//')'
            CALL CHECK_DP_KW_CONSISTENCY(KW_NAME,MSH_CPY(NN),CPY(NN),IOS)
            KW_NAME = 'NCY('//TRIM(ADJUSTL(SUBNN))//')'
            CALL CHECK_INT_KW_CONSISTENCY(KW_NAME,MSH_NCY(NN),NCY(NN),IOS)
            KW_NAME = 'ERY('//TRIM(ADJUSTL(SUBNN))//')'
            CALL CHECK_DP_KW_CONSISTENCY(KW_NAME,MSH_ERY(NN),ERY(NN),IOS)
            KW_NAME = 'FIRST_DY('//TRIM(ADJUSTL(SUBNN))//')'
            CALL CHECK_DP_KW_CONSISTENCY(KW_NAME,MSH_FIRST_DY(NN),FIRST_DY(NN),IOS)
            KW_NAME = 'LAST_DY('//TRIM(ADJUSTL(SUBNN))//')'
            CALL CHECK_DP_KW_CONSISTENCY(KW_NAME,MSH_LAST_DY(NN),LAST_DY(NN),IOS)
         ENDDO

         DO NN=1,NCPZ
            WRITE(SUBNN,*)NN
            KW_NAME = 'CPZ('//TRIM(ADJUSTL(SUBNN))//')'
            CALL CHECK_DP_KW_CONSISTENCY(KW_NAME,MSH_CPZ(NN),CPZ(NN),IOS)
            KW_NAME = 'NCZ('//TRIM(ADJUSTL(SUBNN))//')'
            CALL CHECK_INT_KW_CONSISTENCY(KW_NAME,MSH_NCZ(NN),NCZ(NN),IOS)
            KW_NAME = 'ERZ('//TRIM(ADJUSTL(SUBNN))//')'
            CALL CHECK_DP_KW_CONSISTENCY(KW_NAME,MSH_ERZ(NN),ERZ(NN),IOS)
            KW_NAME = 'FIRST_DZ('//TRIM(ADJUSTL(SUBNN))//')'
            CALL CHECK_DP_KW_CONSISTENCY(KW_NAME,MSH_FIRST_DZ(NN),FIRST_DZ(NN),IOS)
            KW_NAME = 'LAST_DZ('//TRIM(ADJUSTL(SUBNN))//')'
            CALL CHECK_DP_KW_CONSISTENCY(KW_NAME,MSH_LAST_DZ(NN),LAST_DZ(NN),IOS)
         ENDDO

! Cartesian grid settings
         CALL CHECK_BOOL_KW_CONSISTENCY('CARTESIAN_GRID',MSH_CARTESIAN_GRID,CARTESIAN_GRID,IOS)
         CALL CHECK_BOOL_KW_CONSISTENCY('RE_INDEXING',MSH_RE_INDEXING,RE_INDEXING,IOS)
         CALL CHECK_BOOL_KW_CONSISTENCY('MSH_USE_STL',MSH_USE_STL,USE_STL,IOS)

         CALL CHECK_INT_KW_CONSISTENCY('STL_BC_ID',MSH_STL_BC_ID,STL_BC_ID,IOS)

         CALL CHECK_DP_KW_CONSISTENCY('OUT_STL_VALUE',MSH_OUT_STL_VALUE,OUT_STL_VALUE,IOS)
         CALL CHECK_DP_KW_CONSISTENCY('TX_STL',MSH_TX_STL,TX_STL,IOS)
         CALL CHECK_DP_KW_CONSISTENCY('TY_STL',MSH_TY_STL,TY_STL,IOS)
         CALL CHECK_DP_KW_CONSISTENCY('TZ_STL',MSH_TZ_STL,TZ_STL,IOS)
         CALL CHECK_DP_KW_CONSISTENCY('SCALE_STL',MSH_SCALE_STL,SCALE_STL,IOS)

! Cut cell settings
         CALL CHECK_DP_KW_CONSISTENCY('TOL_SMALL_CELL',MSH_TOL_SMALL_CELL,TOL_SMALL_CELL,IOS)
         CALL CHECK_DP_KW_CONSISTENCY('TOL_SMALL_AREA',MSH_TOL_SMALL_AREA,TOL_SMALL_AREA,IOS)
         CALL CHECK_DP_KW_CONSISTENCY('TOL_MERGE',MSH_TOL_MERGE,TOL_MERGE,IOS)
         CALL CHECK_DP_KW_CONSISTENCY('TOL_SNAP(1)',MSH_TOL_SNAP(1),TOL_SNAP(1),IOS)
         CALL CHECK_DP_KW_CONSISTENCY('TOL_SNAP(2)',MSH_TOL_SNAP(2),TOL_SNAP(2),IOS)
         CALL CHECK_DP_KW_CONSISTENCY('TOL_SNAP(3)',MSH_TOL_SNAP(3),TOL_SNAP(3),IOS)
         ! CALL CHECK_DP_KW_CONSISTENCY('FAC_DIM_MAX_CUT_CELL',MSH_FAC_DIM_MAX_CUT_CELL,FAC_DIM_MAX_CUT_CELL,IOS)

         CALL CHECK_INT_KW_CONSISTENCY('ITERMAX_INT',MSH_ITERMAX_INT,ITERMAX_INT,IOS)

         CALL CHECK_DP_KW_CONSISTENCY('TOL_STL',MSH_TOL_STL,TOL_STL,IOS)
         CALL CHECK_DP_KW_CONSISTENCY('STL_SMALL_ANGLE',MSH_STL_SMALL_ANGLE,STL_SMALL_ANGLE,IOS)
         CALL CHECK_DP_KW_CONSISTENCY('TOL_STL_DP',MSH_TOL_STL_DP,TOL_STL_DP,IOS)

         ! CALL CHECK_INT_KW_CONSISTENCY('DIM_FACETS_PER_CELL',MSH_DIM_FACETS_PER_CELL,DIM_FACETS_PER_CELL,IOS)

         CALL CHECK_DP_KW_CONSISTENCY('TOL_DELH',MSH_TOL_DELH,TOL_DELH,IOS)

! Check Boundary conditions regions

         NUMBER_OF_BC_PATCHES = 0
         DO BCV = 1, DIMENSION_BC
            IF(BC_DEFINED(BCV)) THEN
               NUMBER_OF_BC_PATCHES = NUMBER_OF_BC_PATCHES + 1
            ENDIF
         ENDDO

!         CALL CHECK_INT_KW_CONSISTENCY('NUMBER OF BC PATCHES',MSH_NUMBER_OF_BC_PATCHES,NUMBER_OF_BC_PATCHES,IOS)

         NEXT_REC = REC_BC
         DO BCV = 1, MSH_NUMBER_OF_BC_PATCHES
            READ(UNIT_MSH, REC=NEXT_REC) MSH_BCV,MSH_BC_TYPE,MSH_BC_X_W,MSH_BC_X_E,MSH_BC_Y_S,MSH_BC_Y_N,MSH_BC_Z_B,MSH_BC_Z_T
            NEXT_REC = NEXT_REC + 1
! Inconsistent BC type is currently allowed (users could have set NSW in mesh file and use FSW in .mfx file)
            IF(MSH_BC_TYPE/=BC_TYPE(MSH_BCV)) THEN
               WRITE(ERR_MSG,220) MSH_BCV, MSH_BC_TYPE, BC_TYPE(MSH_BCV)
               CALL LOG_WARNING()
            ENDIF

            CALL CHECK_DP_KW_CONSISTENCY('BC_X_E',MSH_BC_X_E,BC_X_E(MSH_BCV),IOS)
            CALL CHECK_DP_KW_CONSISTENCY('BC_X_W',MSH_BC_X_W,BC_X_W(MSH_BCV),IOS)
            CALL CHECK_DP_KW_CONSISTENCY('BC_Y_S',MSH_BC_Y_S,BC_Y_S(MSH_BCV),IOS)
            CALL CHECK_DP_KW_CONSISTENCY('BC_Y_N',MSH_BC_Y_N,BC_Y_N(MSH_BCV),IOS)
            CALL CHECK_DP_KW_CONSISTENCY('BC_Z_B',MSH_BC_Z_B,BC_Z_B(MSH_BCV),IOS)
            CALL CHECK_DP_KW_CONSISTENCY('BC_Z_T',MSH_BC_Z_T,BC_Z_T(MSH_BCV),IOS)
         ENDDO

      ENDIF ! MyPE==0



      CALL GLOBAL_ALL_SUM(IOS)
      IF(IOS /= 0) CALL LOG_ERROR()

      RETURN

100   FORMAT(1X,A)
210   FORMAT(1X,' Fatal Error: Inconsistent data in mesh file and .mfx file.', /' Keyword: ',A, &
              / ' Value in mesh file = ',I8, &
              / ' Value in .mfx file = ',I8)
110   FORMAT(1X,A,I8,A,I8)
120   FORMAT(1X,A,I8)
200   FORMAT(1X,4(I8),1X,D16.8)
220   FORMAT(1X,' Warning: Inconsistent BC type between mesh file and .mfx file.', /' BC ID: ',I8, &
              / ' Value in mesh file = ',A, &
              / ' Value in .mfx file = ',A)

      END SUBROUTINE CHECK_CONSISTENCY_BETWEEN_MESH_AND_MFX


      SUBROUTINE CHECK_INT_KW_CONSISTENCY(kw,msh_value,mfx_value,IOS)
      USE toleranc
      use error_manager

      IMPLICIT NONE
      CHARACTER (LEN=*) , INTENT(IN) :: kw
      INTEGER, INTENT(IN) :: msh_value
      INTEGER, INTENT(IN) :: mfx_value
      INTEGER, INTENT(OUT) ::IOS

      IF(.not.compare_int(msh_value,mfx_value)) THEN
         WRITE(ERR_MSG,210) KW, msh_value, mfx_value
         IOS = 1
      ENDIF

      RETURN

210   FORMAT(1X,' Fatal Error: Inconsistent data in mesh file and .mfx file.', /' Keyword: ',A, &
              / ' Value in mesh file = ',I8, &
              / ' Value in .mfx file = ',I8)
      END SUBROUTINE CHECK_INT_KW_CONSISTENCY

      SUBROUTINE CHECK_BOOL_KW_CONSISTENCY(kw,msh_value,mfx_value,IOS)
      USE toleranc
      use error_manager

      IMPLICIT NONE
      CHARACTER (LEN=*) , INTENT(IN) :: kw
      LOGICAL, INTENT(IN) :: msh_value
      LOGICAL, INTENT(IN) :: mfx_value
      INTEGER, INTENT(OUT) ::IOS

      IF(.not.compare_bool(msh_value,mfx_value)) THEN
         WRITE(ERR_MSG,210) KW, msh_value, mfx_value
         IOS = 1
      ENDIF

      RETURN

210   FORMAT(1X,' Fatal Error: Inconsistent data in mesh file and .mfx file.', /' Keyword: ',A, &
              / ' Value in mesh file = ',L8, &
              / ' Value in .mfx file = ',L8)
      END SUBROUTINE CHECK_BOOL_KW_CONSISTENCY



      SUBROUTINE CHECK_DP_KW_CONSISTENCY(kw,msh_value,mfx_value,IOS)
      USE toleranc
      use error_manager

      IMPLICIT NONE
      CHARACTER (LEN=*) , INTENT(IN) :: kw
      DOUBLE PRECISION, INTENT(IN) :: msh_value
      DOUBLE PRECISION, INTENT(IN) :: mfx_value
      INTEGER, INTENT(OUT) ::IOS

      IF(.not.compare(msh_value,mfx_value)) THEN
         WRITE(ERR_MSG,210) KW, msh_value, mfx_value
         IOS=1
      ENDIF

      RETURN

210   FORMAT(1X,' Fatal Error: Inconsistent data in mesh file and .mfx file.', /' Keyword: ',A, &
              / ' Value in mesh file = ',E14.8, &
              / ' Value in .mfx file = ',E14.8)
      END SUBROUTINE CHECK_DP_KW_CONSISTENCY

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: SET_DEFAULT_GLOBAL_CONNECTIVITY                        C
!  Purpose: Sets global connectivity is standard (uncut cells)         C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 28-Jun-17  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE SET_DEFAULT_GLOBAL_CONNECTIVITY

      USE param
      USE param1
      USE parallel
      USE constant
      USE run
      USE toleranc
      USE geometry
      USE indices
      USE compar
      USE sendrecv
      USE quadric
      USE cutcell
      USE polygon
      USE stl
      USE bc
      USE vtk

      USE mpi_utility
      USE functions
      use error_manager

      IMPLICIT NONE
      INTEGER :: IJK,NODE


      IF(.NOT.GENERATE_MESH) THEN

         WRITE(ERR_MSG,100) ' Setting global connectivity in rectangular cells...'
         CALL LOG_STATUS()

! Allocate arrays - Need global 1D cell indices

         IF(ALLOCATED(GLOBAL_I_OF)) DEALLOCATE(GLOBAL_I_OF)
         IF(ALLOCATED(GLOBAL_J_OF)) DEALLOCATE(GLOBAL_J_OF)
         IF(ALLOCATED(GLOBAL_K_OF)) DEALLOCATE(GLOBAL_K_OF)

         IF (myPE == PE_IO) THEN
            allocate (GLOBAL_I_OF(ijkmax3))
            allocate (GLOBAL_J_OF(ijkmax3))
            allocate (GLOBAL_K_OF(ijkmax3))
         ELSE
            allocate (GLOBAL_I_OF(1))
            allocate (GLOBAL_J_OF(1))
            allocate (GLOBAL_K_OF(1))
         ENDIF

! Gather global indices
         call gather (I_OF,GLOBAL_I_OF,root)
         call gather (J_OF,GLOBAL_J_OF,root)
         call gather (K_OF,GLOBAL_K_OF,root)

! Set connectivity
         IF (myPE == PE_IO) THEN
            DO IJK=1,IJKMAX3
               IF(GLOBAL_STANDARD_CELL_AT(IJK)) THEN
                  CALL GET_GLOBAL_CELL_NODE_COORDINATES(IJK,'SCALAR')
                  IF(NO_K) THEN
                     GLOBAL_NUMBER_OF_NODES(IJK) = 4
                     GLOBAL_CONNECTIVITY(IJK,1) = IJK_OF_NODE(5)
                     GLOBAL_CONNECTIVITY(IJK,2) = IJK_OF_NODE(6)
                     GLOBAL_CONNECTIVITY(IJK,3) = IJK_OF_NODE(8)
                     GLOBAL_CONNECTIVITY(IJK,4) = IJK_OF_NODE(7)
                  ELSE
                     GLOBAL_NUMBER_OF_NODES(IJK) = 8
                     DO NODE = 1,8
                        GLOBAL_CONNECTIVITY(IJK,NODE) = IJK_OF_NODE(NODE)
                     END DO
                  ENDIF
               ENDIF
            ENDDO
         ENDIF

      ENDIF


      RETURN

100   FORMAT(1X,A)
110   FORMAT(1X,A,I8,A,I8)
120   FORMAT(1X,A,I8)
200   FORMAT(1X,4(I8),1X,D16.8)

      END SUBROUTINE SET_DEFAULT_GLOBAL_CONNECTIVITY

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: SET_MASTER_FROM_FLAGS                                  C
!  Purpose: Sets global master flags from the flag                     C
!           before saving mesh file (to save amount of data to write)  C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 28-Jun-17  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE SET_MASTER_FROM_FLAGS(MASTER_FLAG)

      USE param
      USE param1
      USE parallel
      USE constant
      USE run
      USE toleranc
      USE geometry
      USE indices
      USE compar
      USE sendrecv
      USE quadric
      USE cutcell
      USE polygon
      USE stl
      USE bc
      USE vtk

      USE mpi_utility
      USE functions
      USE error_manager

      IMPLICIT NONE
      INTEGER :: IJK,NODE
      INTEGER, DIMENSION(DIMENSION_3) :: MASTER_FLAG ! Local flag


! The master flag is a 4 digit integer that combines the cell types
! for scalar and all momentum cells.
! From left to right:
! 1st digit (thousands) : Scalar cell
! 2nd digit (hundreds)  : U-momentum cell
! 3rd digit (tens)      : V-momentum cell
! 4th digit (units)     : W-momentum cell
!
! Each digit takes one of the following values:
! 1: Standard (uncut) fuild cell
! 2: Cut cell
! 3: Blocked cell
!
! For example, 1111 means all cells at a given IJK are standard
!              2122 means the u-momentum cell is not cut, all others are cut
!              3333 means all cells are blocked (3D)
! In 2D the last digit is always 1
!              3331 means all cells are blocked (2D)



      IF(GENERATE_MESH) THEN
         WRITE(ERR_MSG,100) ' Setting master from flags...'
         CALL LOG_STATUS()

         MASTER_FLAG = 0

         WHERE(STANDARD_CELL_AT) MASTER_FLAG = 1000
         WHERE(CUT_CELL_AT) MASTER_FLAG      = 2000
         WHERE(BLOCKED_CELL_AT) MASTER_FLAG  = 3000

         WHERE(STANDARD_U_CELL_AT) MASTER_FLAG = MASTER_FLAG + 100
         WHERE(CUT_U_CELL_AT) MASTER_FLAG      = MASTER_FLAG + 200
         WHERE(BLOCKED_U_CELL_AT) MASTER_FLAG  = MASTER_FLAG + 300

         WHERE(STANDARD_V_CELL_AT) MASTER_FLAG = MASTER_FLAG + 10
         WHERE(CUT_V_CELL_AT) MASTER_FLAG      = MASTER_FLAG + 20
         WHERE(BLOCKED_V_CELL_AT) MASTER_FLAG  = MASTER_FLAG + 30

         WHERE(STANDARD_W_CELL_AT) MASTER_FLAG = MASTER_FLAG + 1
         WHERE(CUT_W_CELL_AT) MASTER_FLAG      = MASTER_FLAG + 2
         WHERE(BLOCKED_W_CELL_AT) MASTER_FLAG  = MASTER_FLAG + 3


      ENDIF


      RETURN

100   FORMAT(1X,A)
110   FORMAT(1X,A,I8,A,I8)
120   FORMAT(1X,A,I8)
200   FORMAT(1X,4(I8),1X,D16.8)

      END SUBROUTINE SET_MASTER_FROM_FLAGS
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: SET_FLAGS_FROM MASTER                                  C
!  Purpose: Sets global flags from the master flag saved in mesh file  C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 28-Jun-17  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE SET_FLAGS_FROM_MASTER(MASTER_FLAG)

      USE param
      USE param1
      USE parallel
      USE constant
      USE run
      USE toleranc
      USE geometry
      USE indices
      USE compar
      USE sendrecv
      USE quadric
      USE cutcell
      USE polygon
      USE stl
      USE bc
      USE vtk

      USE mpi_utility
      USE functions
      USE error_manager

      IMPLICIT NONE
      INTEGER :: IJK,NODE
      INTEGER, DIMENSION(IJKMAX3) :: MASTER_FLAG
      CHARACTER(LEN=4) :: CHAR_FLAG
      CHARACTER(LEN=1) :: TMP_CHAR


      IF(.NOT.GENERATE_MESH) THEN
         WRITE(ERR_MSG,100) ' Setting flags from master flag...'
         CALL LOG_STATUS()

         IF(ALLOCATED(GLOBAL_STANDARD_CELL_AT)) DEALLOCATE(GLOBAL_STANDARD_CELL_AT)
         IF(ALLOCATED(GLOBAL_CUT_CELL_AT)) DEALLOCATE(GLOBAL_CUT_CELL_AT)
         IF(ALLOCATED(GLOBAL_BLOCKED_CELL_AT)) DEALLOCATE(GLOBAL_BLOCKED_CELL_AT)

         IF(MyPE == PE_IO) THEN
            ALLOCATE(GLOBAL_STANDARD_CELL_AT(IJKMAX3))
            ALLOCATE(GLOBAL_CUT_CELL_AT(IJKMAX3))
            ALLOCATE(GLOBAL_BLOCKED_CELL_AT(IJKMAX3))
         ELSE
            ALLOCATE(GLOBAL_STANDARD_CELL_AT(1))
            ALLOCATE(GLOBAL_CUT_CELL_AT(1))
            ALLOCATE(GLOBAL_BLOCKED_CELL_AT(1))
         ENDIF

         GLOBAL_STANDARD_CELL_AT = .FALSE.
         GLOBAL_CUT_CELL_AT      = .FALSE.
         GLOBAL_BLOCKED_CELL_AT  = .FALSE.

         STANDARD_CELL_AT = .FALSE.
         CUT_CELL_AT      = .FALSE.
         BLOCKED_CELL_AT  = .FALSE.

         STANDARD_U_CELL_AT = .FALSE.
         CUT_U_CELL_AT      = .FALSE.
         BLOCKED_U_CELL_AT  = .FALSE.

         STANDARD_V_CELL_AT = .FALSE.
         CUT_V_CELL_AT      = .FALSE.
         BLOCKED_V_CELL_AT  = .FALSE.

         STANDARD_W_CELL_AT = .FALSE.
         CUT_W_CELL_AT      = .FALSE.
         BLOCKED_W_CELL_AT  = .FALSE.

         DO IJK=IJKSTART3,IJKEND3
            WRITE(CHAR_FLAG,'(I4)')MASTER_FLAG(IJK)
            TMP_CHAR = CHAR_FLAG(1:1)
            SELECT CASE(TMP_CHAR)
               CASE('1')
                  STANDARD_CELL_AT(IJK) = .TRUE.
               CASE('2')
                  CUT_CELL_AT(IJK) = .TRUE.
               CASE('3')
                  BLOCKED_CELL_AT(IJK) = .TRUE.
            END SELECT

            SELECT CASE(CHAR_FLAG(2:2))
               CASE('1')
                  STANDARD_U_CELL_AT(IJK) = .TRUE.
               CASE('2')
                  CUT_U_CELL_AT(IJK) = .TRUE.
               CASE('3')
                  BLOCKED_U_CELL_AT(IJK) = .TRUE.
            END SELECT

            SELECT CASE(CHAR_FLAG(3:3))
               CASE('1')
                  STANDARD_V_CELL_AT(IJK) = .TRUE.
               CASE('2')
                  CUT_V_CELL_AT(IJK) = .TRUE.
               CASE('3')
                  BLOCKED_V_CELL_AT(IJK) = .TRUE.
            END SELECT

            SELECT CASE(CHAR_FLAG(4:4))
               CASE('1')
                  STANDARD_W_CELL_AT(IJK) = .TRUE.
               CASE('2')
                  CUT_W_CELL_AT(IJK) = .TRUE.
               CASE('3')
                  BLOCKED_W_CELL_AT(IJK) = .TRUE.
            END SELECT

         ENDDO

      ENDIF

      call gather(STANDARD_CELL_AT, GLOBAL_STANDARD_CELL_AT, PE_IO)
      call gather(CUT_CELL_AT, GLOBAL_CUT_CELL_AT, PE_IO)
      call gather(BLOCKED_CELL_AT, GLOBAL_BLOCKED_CELL_AT, PE_IO)

      RETURN

100   FORMAT(1X,A)
110   FORMAT(1X,A,I8,A,I8)
120   FORMAT(1X,A,I8)
200   FORMAT(1X,4(I8),1X,D16.8)

      END SUBROUTINE SET_FLAGS_FROM_MASTER


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine:                                                         C
!  Purpose:                                                            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      subroutine readScatterMasterList(MASTER_FLAG, NEXT_REC)

! Modules
!---------------------------------------------------------------------//
      use param1, only: zero, undefined
      use param, only: dimension_3
      USE geometry
      USE funits
      USE compar
      USE cdist
      USE cutcell
      USE mpi_utility
      USE sendrecv
      USE in_binary_512i
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      integer, dimension(DIMENSION_3) :: MASTER_FLAG
      integer, dimension(ijkmax2) :: array1
      integer, dimension(ijkmax3) :: array2
      integer :: Init  ! Initial value of VAR
      INTEGER :: NEXT_REC
!---------------------------------------------------------------------//

!// Reset global scratch arrays
!       array1(:) = Init
!       array2(:) = Init
       array1(:) = -1
       array2(:) = -1

      if (.not.bDist_IO .or. bStart_with_one_RES) then
         if (myPE == PE_IO) then
            CALL IN_BIN_512i (UNIT_MSH, array1, IJKMAX2, NEXT_REC)
            CALL convert_from_io_i(array1, array2, IJKMAX2)

            READ(UNIT_MSH, REC=REC_NC) N_FL_CELLS,N_CT_CELLS,N_BL_CELLS


            IF(ALLOCATED(LIST_FL_CELLS)) DEALLOCATE(LIST_FL_CELLS)
            IF(ALLOCATED(LIST_CT_CELLS)) DEALLOCATE(LIST_CT_CELLS)
            IF(ALLOCATED(LIST_BL_CELLS)) DEALLOCATE(LIST_BL_CELLS)

            ALLOCATE(LIST_FL_CELLS(IJKMAX2))
            ALLOCATE(LIST_CT_CELLS(IJKMAX2))
            ALLOCATE(LIST_BL_CELLS(IJKMAX2))
            CALL IN_BIN_512i (UNIT_MSH, LIST_FL_CELLS(1:N_FL_CELLS), N_FL_CELLS , NEXT_REC)
            CALL IN_BIN_512i (UNIT_MSH, LIST_CT_CELLS, N_CT_CELLS , NEXT_REC)
            CALL IN_BIN_512i (UNIT_MSH, LIST_BL_CELLS, N_BL_CELLS , NEXT_REC)

         end if
!        call MPI_barrier(MPI_COMM_WORLD,mpierr)

         call scatter(MASTER_FLAG, array2, PE_IO)
!        call MPI_barrier(MPI_COMM_WORLD,mpierr)
      else
         ! CALL IN_BIN_512i (UNIT_RES, var, size(var) , NEXT_REC)
      end if

      End subroutine readScatterMasterList
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine:                                                         C
!  Purpose:                                                            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      subroutine readScatterMesh_dpc(VAR, init, COMPACT, NEXT_REC)

! Modules
!---------------------------------------------------------------------//
      use param1, only: zero, undefined
      use param, only: dimension_3
      USE geometry
      USE funits
      USE compar
      USE cdist
      USE cutcell
      USE mpi_utility
      USE sendrecv
      USE in_binary_512
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      double precision, dimension(DIMENSION_3) :: VAR
      double precision, dimension(ijkmax2) :: array1
      double precision, dimension(ijkmax3) :: array2
      double precision, dimension(ijkmax2) :: array3
      double precision :: Init  ! Initial value of VAR
      LOGICAL :: COMPACT ! Flag to read compact form (shorter) of VAR
      INTEGER :: NEXT_REC
      INTEGER :: IJK
!---------------------------------------------------------------------//

!// Reset global scratch arrays
!       array1(:) = Init
!       array2(:) = Init
!       array1(:) = 1.0D0
!       array2(:) = 1.0D0



      if (.not.bDist_IO .or. bStart_with_one_RES) then
! Local array VAR is already filled up with default values in standards cells
! It needs to be gathered into a global array array2
! then shifted to IO indices to form array1
! The values read in the MESH files are at IO indices (array3)
! New values (array3) overwrite default values in array1 (still at IO indices)
! Then array1 is shifted back to global indices (array2)
! and finally, global array2 is scattered to local VAR

         IF(COMPACT) CALL gather (VAR,array2,root)

         if (myPE == PE_IO) then
            IF(COMPACT) THEN
               call convert_to_io_dp(array2,array1,ijkmax2)
               CALL IN_BIN_512 (UNIT_MSH, array3, N_CT_CELLS, NEXT_REC)
               DO IJK=1,N_CT_CELLS
                  array1(LIST_CT_CELLS(IJK)) = array3(IJK)
               ENDDO
            ELSE
               CALL IN_BIN_512 (UNIT_MSH, array1, IJKMAX2, NEXT_REC)
            ENDIF
            CALL convert_from_io_dp(array1, array2, IJKMAX2)
         end if
!        call MPI_barrier(MPI_COMM_WORLD,mpierr)
         call scatter(VAR, array2, PE_IO)
!        call MPI_barrier(MPI_COMM_WORLD,mpierr)
      else
         CALL IN_BIN_512 (UNIT_RES, var, size(var) , NEXT_REC)
      end if

      End subroutine readScatterMesh_dpc
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine:                                                         C
!  Purpose:                                                            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      subroutine readScatterMesh_ic(VAR, init, COMPACT, NEXT_REC)

! Modules
!---------------------------------------------------------------------//
      use param1, only: zero, undefined
      use param, only: dimension_3
      USE geometry
      USE funits
      USE compar
      USE cdist
      USE cutcell
      USE mpi_utility
      USE sendrecv
      USE in_binary_512i
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      integer, dimension(DIMENSION_3) :: VAR
      integer, dimension(ijkmax2) :: array1
      integer, dimension(ijkmax3) :: array2
      integer, dimension(ijkmax2) :: array3
      integer :: Init  ! Initial value of VAR
      LOGICAL :: COMPACT ! Flag to read compact form (shorter) of VAR
      INTEGER :: NEXT_REC
      INTEGER :: IJK
!---------------------------------------------------------------------//

!// Reset global scratch arrays
      IF(.NOT.COMPACT) THEN
          array1(:) = Init
          array2(:) = Init
       ENDIF
!       array1(:) = 1.0D0
!       array2(:) = 1.0D0


      if (.not.bDist_IO .or. bStart_with_one_RES) then
! Local array VAR is already filled up with default values in standards cells
! It needs to be gathered into a global array array2
! then shifted to IO indices to form array1
! The values read in the MESH files are at IO indices (array3)
! New values (array3) overwrite default values in array1 (still at IO indices)
! Then array1 is shifted back to global indices (array2)
! and finally, global array2 is scattered to local VAR

         IF(COMPACT) CALL gather (VAR,array2,root)

         if (myPE == PE_IO) then
            IF(COMPACT) THEN
               call convert_to_io_i(array2,array1,ijkmax2)
               CALL IN_BIN_512i (UNIT_MSH, array3, N_CT_CELLS, NEXT_REC)
               DO IJK=1,N_CT_CELLS
                  array1(LIST_CT_CELLS(IJK)) = array3(IJK)
               ENDDO
            ELSE
               CALL IN_BIN_512i (UNIT_MSH, array1, IJKMAX2, NEXT_REC)
            ENDIF
            CALL convert_from_io_i(array1, array2, IJKMAX2)
         end if
!        call MPI_barrier(MPI_COMM_WORLD,mpierr)
         call scatter(VAR, array2, PE_IO)
!        call MPI_barrier(MPI_COMM_WORLD,mpierr)
      else
         CALL IN_BIN_512i (UNIT_RES, var, size(var) , NEXT_REC)
      end if

      End subroutine readScatterMesh_ic
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine:                                                         C
!  Purpose:                                                            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      subroutine readScatterMesh_lc(VAR, init, COMPACT, NEXT_REC)

! Modules
!---------------------------------------------------------------------//
      use param1, only: zero, undefined
      use param, only: dimension_3
      USE geometry
      USE funits
      USE compar
      USE cdist
      USE cutcell
      USE mpi_utility
      USE sendrecv
      USE in_binary_512l
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      logical, dimension(DIMENSION_3) :: VAR
      logical, dimension(ijkmax2) :: array1
      logical, dimension(ijkmax3) :: array2
      logical, dimension(ijkmax2) :: array3
      logical:: Init  ! Initial value of VAR
      LOGICAL :: COMPACT ! Flag to read compact form (shorter) of VAR
      INTEGER :: NEXT_REC
      INTEGER :: IJK
!---------------------------------------------------------------------//

!// Reset global scratch arrays
!       array1(:) = Init
!       array2(:) = Init
!       array1(:) = 1.0D0
!       array2(:) = 1.0D0



      if (.not.bDist_IO .or. bStart_with_one_RES) then
! Local array VAR is already filled up with default values in standards cells
! It needs to be gathered into a global array array2
! then shifted to IO indices to form array1
! The values read in the MESH files are at IO indices (array3)
! New values (array3) overwrite default values in array1 (still at IO indices)
! Then array1 is shifted back to global indices (array2)
! and finally, global array2 is scattered to local VAR

         IF(COMPACT) CALL gather (VAR,array2,root)

         if (myPE == PE_IO) then
            IF(COMPACT) THEN
               call convert_to_io_l(array2,array1,ijkmax2)
               CALL IN_BIN_512l (UNIT_MSH, array3, N_CT_CELLS, NEXT_REC)
               DO IJK=1,N_CT_CELLS
                  array1(LIST_CT_CELLS(IJK)) = array3(IJK)
               ENDDO
            ELSE
               CALL IN_BIN_512l (UNIT_MSH, array1, IJKMAX2, NEXT_REC)
            ENDIF
            CALL convert_from_io_l(array1, array2, IJKMAX2)
         end if
!        call MPI_barrier(MPI_COMM_WORLD,mpierr)
         call scatter(VAR, array2, PE_IO)
!        call MPI_barrier(MPI_COMM_WORLD,mpierr)
      else
         CALL IN_BIN_512l (UNIT_RES, var, size(var) , NEXT_REC)
      end if

      End subroutine readScatterMesh_lc


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: UPDATE_MESH_MASK_I                                     C
!  Purpose: Update mesh mask from an Integer array                     C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 28-Jun-17  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE UPDATE_MESH_MASK_I(VAR,DEFAULT_VALUE)

      USE param
!      USE param1
!      USE indices
!      USE compar
      USE cutcell,  only : MESH_MASK
      use error_manager

      IMPLICIT NONE

      INTEGER, DIMENSION(DIMENSION_3), INTENT(IN) :: VAR
      INTEGER, INTENT(IN) :: DEFAULT_VALUE


      WHERE (VAR(:)/=DEFAULT_VALUE) MESH_MASK(:) = .TRUE.

      RETURN

      END SUBROUTINE UPDATE_MESH_MASK_I

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: UPDATE_MESH_MASK_L                                     C
!  Purpose: Update mesh mask from a Logical array                      C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 28-Jun-17  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE UPDATE_MESH_MASK_L(VAR,DEFAULT_VALUE)

      USE param
!      USE param1
!      USE indices
!      USE compar
      USE cutcell,  only : MESH_MASK
      use error_manager

      IMPLICIT NONE

      LOGICAL, DIMENSION(DIMENSION_3), INTENT(IN) :: VAR
      LOGICAL, INTENT(IN) :: DEFAULT_VALUE


      WHERE (VAR(:).neqv.DEFAULT_VALUE) MESH_MASK(:) = .TRUE.

      RETURN

      END SUBROUTINE UPDATE_MESH_MASK_L

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: UPDATE_MESH_MASK_D                                     C
!  Purpose: Update mesh mask from a Double precision array             C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 28-Jun-17  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE UPDATE_MESH_MASK_D(VAR,DEFAULT_VALUE)

      USE param
!      USE param1
!      USE indices
!      USE compar
      USE cutcell,  only : MESH_MASK
      use error_manager

      IMPLICIT NONE

      DOUBLE PRECISION, DIMENSION(DIMENSION_3), INTENT(IN) :: VAR
      DOUBLE PRECISION, INTENT(IN) :: DEFAULT_VALUE


      WHERE (VAR(:)/=DEFAULT_VALUE) MESH_MASK(:) = .TRUE.

      RETURN

      END SUBROUTINE UPDATE_MESH_MASK_D

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: BUILD_MESH_MASK                                        C
!  Purpose: Build the meah mask                                        C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 28-Jun-17  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE BUILD_MESH_MASK

      USE param
      USE param1
      USE indices
      USE compar
      USE sendrecv
      USE cutcell !,  only : MESH_MASK
      use error_manager

      IMPLICIT NONE

      INTEGER :: IJK
      INTEGER :: NMM


      MESH_MASK = .FALSE.


      CALL UPDATE_MESH_MASK_D(Area_cut,ZERO)



      NMM = 0

      DO IJK=IJKSTART3,IJKEND3

!         IF(VOL(IJK)==ZERO) print*,'zero volume',IJK

         IF(MESH_MASK(IJK)) THEN

            NMM = NMM + 1

         ENDIF

      ENDDO


      call SEND_RECEIVE_1D_LOGICAL(MESH_MASK,2)




      RETURN

      END SUBROUTINE BUILD_MESH_MASK


END MODULE CUT_CELL_PREPROC
