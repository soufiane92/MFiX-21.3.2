#include "error.inc"

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: stl_functions_des                                      !
!  Author: Rahul Garg                                 Date: 24-Oct-13  !
!                                                                      !
!  Purpose: This module contains routines for geometric interaction    !
!  required for STL files.                                             !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
MODULE STL_PREPROC_DES

   use bc, only: BC_DEFINED, BC_TYPE_ENUM, FREE_SLIP_WALL, NO_SLIP_WALL, PAR_SLIP_WALL
   use bc, only: BC_X_w, BC_X_e, BC_I_w, BC_I_e
   use bc, only: BC_Y_s, BC_Y_n, BC_J_s, BC_J_n
   use bc, only: BC_Z_b, BC_Z_t, BC_K_b, BC_K_t
   use bc, only: is_cg
   use compar, only: mype
   use constant, only: PI
   use cutcell, only: CARTESIAN_GRID
   use cutcell, only: use_stl
   use desgrid, only: DG_FUNIJK
   use desgrid, only: DG_IEND2, DG_ISTART2
   use desgrid, only: DG_IJKSIZE2
   use desgrid, only: DG_JEND2, DG_JSTART2
   use desgrid, only: DG_KEND2, DG_KSTART2
   use desgrid, only: IofPOS, JofPOS, KofPOS
   use desgrid, only: dg_dxinv, dg_xstart, dg_istart1
   use desgrid, only: dg_dyinv, dg_ystart, dg_jstart1
   use desgrid, only: dg_dzinv, dg_zstart, dg_kstart1
   use desgrid, only: dg_is_ON_myPE_plus1layers
   use discretelement, only: DES_PERIODIC_WALLS_X
   use discretelement, only: DES_PERIODIC_WALLS_Y
   use discretelement, only: DES_PERIODIC_WALLS_Z
   use discretelement, only: MAX_RADIUS
   use discretelement, only: XE, YN, ZT
   use error_manager
   use functions, only: funijk, bound_funijk
   use functions, only: is_on_mype_owns
   use functions, only: wall_icbc_flag
   use geometry, only: DO_K
   use geometry, only: ICBC_FLAG
   use geometry, only: XLENGTH, YLENGTH, ZLENGTH
   use geometry, only: X_MIN, X_MAX, Y_MIN, Y_MAX, Z_MIN, Z_MAX
   use is, only: IS_DEFINED, IS_TYPE
   use is, only: IS_I_w, IS_I_e
   use is, only: IS_J_s, IS_J_n
   use is, only: IS_K_b, IS_K_t
   use mpi_utility, only: BCAST, GLOBAL_ALL_SUM
   use param, only: DIMENSION_BC, DIMENSION_IS
   use param1, only: HALF, ONE, ZERO
   use stl, only: BASE_STL
   use stl, only: DIM_STL
   use stl, only: FACETS_AT_DG
   use stl, only: IGNORE_EDGE_INTERSECTION
   use stl, only: N_FACETS, N_FACETS_DES
   use stl, only: STL_NB_ANGLE, COS_STL_NB_ANGLE
   use stl, only: STL_START, STL_END, BCWALLS_STL, DEFAULT_STL, IMPRMBL_STL
   use stl, only: TOL_STL
   use stl, only: VERTEX, NORM_FACE
   use stl, only: STL_GROUP,N_STL_GROUP
   use stl, only: BC_ID_STL_FACE
   use stl_dbg_des
   use stl_functions_des, only: TRI_BOX_OVERLAP

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: DES_STL_PREPROCESSING                                   !
!  Author: Rahul Garg                                 Date: 24-Oct-13  !
!                                                                      !
!  Purpose:                                                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE DES_STL_PREPROCESSING

      IMPLICIT NONE

! Pre-procssing for the des in order to assign facets to grid cells.
      WRITE(ERR_MSG,"('Pre-Processing geometry for DES.')")
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

! Process the STL files
      N_FACETS_DES = merge(N_FACETS, 0, USE_STL)
! Store the Start/End of the base STLs from geometry files
      STL_START(BASE_STL)=1;   STL_END(BASE_STL)=N_FACETS_DES

! Process stair-step geometries
      ! CALL CONVERT_BC_WALLS_TO_STL
      CALL CONVERT_BC_REGIONS_TO_STL
! Process stair-step geometries
      CALL CONVERT_IMPERMEABLE_IS_TO_STL
! Process default walls
! All BCs must be defined with cut-cells, no default walls
! JFD: This makes some of the tutorials fail, so I am leaving the 
! default walls for now. It will need to be fixed so we don't rely 
! on default walls.
!      IF(.NOT.CARTESIAN_GRID) CALL CONVERT_DEFAULT_WALLS_TO_STL
      CALL CONVERT_DEFAULT_WALLS_TO_STL

! Bin the STL to the DES grid.
      CALL BIN_FACETS_TO_DG

! Flag facets which edge collision will be ignored
      CALL FLAG_FACETS_WHERE_EDGE_COLLISION_IS_IGNORED
! Some functions for debugging.
!      CALL STL_DBG_WRITE_FACETS(BASE_STL)
!      CALL STL_DBG_WRITE_FACETS(BCWALLS_STL)
!      CALL STL_DBG_WRITE_FACETS(IMPRMBL_STL)
!      CALL STL_DBG_WRITE_FACETS(DEFAULT_STL)
!      CALL STL_DBG_WRITE_FACETS(ALL_STL)
!      CALL STL_DBG_WRITE_STL_FROM_DG(STL_TYPE=BASE_STL)

! Pre-procssing for the des in order to assign facets to grid cells.
      WRITE(ERR_MSG,"('DES geometry pre-processing complete.')")
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

      RETURN
      END SUBROUTINE DES_STL_PREPROCESSING


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: BIN_FACETS_TO_DG                                        !
!  Author: Rahul Garg                                 Date: 24-Oct-13  !
!                                                                      !
!  Purpose:                                                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      Subroutine BIN_FACETS_TO_DG

      IMPLICIT NONE

! DES Grid cell index.
      INTEGER :: IJK
! Loop counters:
      INTEGER :: I1, I2, II  ! X-axis
      INTEGER :: J1, J2, JJ  ! Y-axis
      INTEGER :: K1, K2, KK  ! Z-axis
      INTEGER :: NN          ! STLs

! Maximum and minimum extents of the indexed STL
      DOUBLE PRECISION:: X1,Y1,Z1
      DOUBLE PRECISION:: X2,Y2,Z2

! Allocate the data storage array.
      IF(.not.allocated(FACETS_AT_DG)) &
         allocate(FACETS_AT_DG(DG_IJKSIZE2))

      FACETS_AT_DG(:)%COUNT = 0

      DO NN = 1,N_FACETS_DES

         X1 = minval(VERTEX(1:3,1,NN))
         X2 = maxval(VERTEX(1:3,1,NN))
         Y1 = minval(VERTEX(1:3,2,NN))
         Y2 = maxval(VERTEX(1:3,2,NN))
         Z1 = minval(VERTEX(1:3,3,NN))
         Z2 = maxval(VERTEX(1:3,3,NN))

         I1 = DG_IEND2
         I2 = DG_ISTART2
         IF(X2>=X_MIN-TOL_STL .AND. X1<=X_MAX+TOL_STL) THEN
            I1 = max(iofpos(X1)-1, dg_istart2)
            I2 = min(iofpos(X2)+1, dg_iend2)
         ENDIF

         J1 = DG_JEND2
         J2 = DG_JSTART2
         IF(Y2>=Y_MIN-TOL_STL .AND. Y1<=Y_MAX+TOL_STL) THEN
            J1 = max(jofpos(Y1)-1, dg_jstart2)
            J2 = min(jofpos(Y2)+1, dg_jend2)
         ENDIF

         K1 = DG_KEND2
         K2 = DG_KSTART2
         IF(DO_K) THEN
            IF(Z2>=Z_MIN-TOL_STL .AND. Z1<=Z_MAX+TOL_STL) THEN
               K1 = max(kofpos(Z1)-1, dg_kstart2)
               K2 = min(kofpos(Z2)+1, dg_kend2)
            ENDIF
         ENDIF

         DO KK=K1,K2
         DO JJ=J1,J2
         DO II=I1,I2
            IF(dg_is_ON_myPE_plus1layers(II,JJ,KK)) THEN
               IJK = DG_FUNIJK(II,JJ,KK)
               CALL ADD_FACET_FOR_DES(II,JJ,KK,IJK,NN)
            ENDIF
         ENDDO
         ENDDO
         ENDDO

      ENDDO

      RETURN
      END SUBROUTINE BIN_FACETS_TO_DG




!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: ADD_FACET_FOR_DES                                       !
!  Author: Rahul Garg                                  Date: 24-Oct-13 !
!                                                                      !
!  Purpose: Add facets to DES grid cells.                              !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE ADD_FACET_FOR_DES(I,J,K,IJK,N)

      IMPLICIT NONE

! DES grid index and facet index
      INTEGER, INTENT(IN) :: I,J,K,IJK, N

! Center of DES grid cell and half size. Note that a buffer is added to
! the half size to make the cell appear a little larger. This ensures
! that paricles near the edge 'see' STLs that are nearby but do not
! directly intersect the DES grid cell contain the particle center.
      DOUBLE PRECISION :: CENTER(3), HALFSIZE(3)
! Flag: STL intersects the DES grid cell
      LOGICAL :: OVERLAP
! DES grid cell dimensions
      DOUBLE PRECISION :: lDX, lDY, lDZ
! Buffer to ensure all particle-STL collisions are captured.
      DOUBLE PRECISION :: BUFFER

      BUFFER = 1.1d0*MAX_RADIUS

      lDX = ONE/DG_DXINV
      lDY = ONE/DG_DYINV
      lDZ = ONE/DG_DZINV

      CENTER(1) = dg_xstart + (dble(I-dg_istart1)+HALF)*lDX
      HALFSIZE(1) = HALF*lDX + BUFFER

      CENTER(2) = dg_ystart + (dble(J-dg_jstart1)+HALF)*lDY
      HALFSIZE(2) = HALF*lDY + BUFFER

      IF(DO_K)THEN
         CENTER(3) = dg_zstart + (dble(K-dg_kstart1)+HALF)*lDZ
         HALFSIZE(3) = HALF*lDZ + BUFFER
      ELSE
         CENTER(3) = HALF*lDZ
         HALFSIZE(3) = HALF*lDZ
      ENDIF

      CALL TRI_BOX_OVERLAP(CENTER, HALFSIZE, VERTEX(:,:,N), OVERLAP)

      IF(OVERLAP) CALL ADD_FACET(IJK, N)

      RETURN
      END SUBROUTINE ADD_FACET_FOR_DES



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: ADD_FACET                                               !
!                                                                      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE ADD_FACET(IJK, FACET_ID)

      implicit none

      INTEGER, INTENT(IN) :: IJK, facet_id

      INTEGER, ALLOCATABLE :: int_tmp(:)
      DOUBLE PRECISION, ALLOCATABLE :: real_tmp(:)

      INTEGER :: lSIZE, II,FC
      DOUBLE PRECISION :: smallest_extent, min_temp, max_temp


      FC = FACETS_AT_DG(IJK)%COUNT
      IF(FC > 0) THEN
!      IF(FACETS_AT_DG(IJK)%COUNT > 0) THEN

         DO II=1, FACETS_AT_DG(IJK)%COUNT
            IF(FACET_ID == FACETS_AT_DG(IJK)%ID(II)) RETURN
         ENDDO

         FACETS_AT_DG(IJK)%COUNT = FACETS_AT_DG(IJK)%COUNT+1

         lSIZE = size(FACETS_AT_DG(IJK)%ID)
         IF(FACETS_AT_DG(IJK)%COUNT +1> lSIZE) THEN
            allocate(int_tmp(2*lSIZE)); int_tmp=0
            int_tmp(1:lSIZE) = FACETS_AT_DG(IJK)%ID(1:lSIZE)
            call move_alloc(int_tmp,FACETS_AT_DG(IJK)%ID)

            allocate(int_tmp(2*lSIZE)); int_tmp=0
            int_tmp(1:lSIZE) = FACETS_AT_DG(IJK)%DIR(1:lSIZE)
            call move_alloc(int_tmp, FACETS_AT_DG(IJK)%DIR)

            allocate(real_tmp(2*lSIZE)); real_tmp=ZERO
            real_tmp(1:lSIZE) = FACETS_AT_DG(IJK)%MIN(1:lSIZE)
            call move_alloc(real_tmp, FACETS_AT_DG(IJK)%MIN)

            allocate(real_tmp(2*lSIZE)); real_tmp=ZERO
            real_tmp(1:lSIZE) = FACETS_AT_DG(IJK)%MAX(1:lSIZE)
            call move_alloc(real_tmp, FACETS_AT_DG(IJK)%MAX)
         ENDIF

      ELSE
         FACETS_AT_DG(IJK)%COUNT = 1
         IF(allocated(FACETS_AT_DG(IJK)%ID)) deallocate(FACETS_AT_DG(IJK)%ID)
         allocate(FACETS_AT_DG(IJK)%ID(4))
         IF(allocated(FACETS_AT_DG(IJK)%DIR)) deallocate(FACETS_AT_DG(IJK)%DIR)
         allocate(FACETS_AT_DG(IJK)%DIR(4))
         IF(allocated(FACETS_AT_DG(IJK)%MIN)) deallocate(FACETS_AT_DG(IJK)%MIN)
         allocate(FACETS_AT_DG(IJK)%MIN(4))
         IF(allocated(FACETS_AT_DG(IJK)%MAX)) deallocate(FACETS_AT_DG(IJK)%MAX)
         allocate(FACETS_AT_DG(IJK)%MAX(4))

         ! IF(.not.allocated(FACETS_AT_DG(IJK)%ID)) &
         !    allocate(FACETS_AT_DG(IJK)%ID(4))
         ! IF(.not.allocated(FACETS_AT_DG(IJK)%DIR)) &
         !    allocate(FACETS_AT_DG(IJK)%DIR(4))
         ! IF(.not.allocated(FACETS_AT_DG(IJK)%MIN)) &
         !    allocate(FACETS_AT_DG(IJK)%MIN(4))
         ! IF(.not.allocated(FACETS_AT_DG(IJK)%MAX)) &
         !    allocate(FACETS_AT_DG(IJK)%MAX(4))
      ENDIF

      FACETS_AT_DG(IJK)%ID(FACETS_AT_DG(IJK)%COUNT) = FACET_ID

      SMALLEST_EXTENT = HUGE(0.0)

      DO II=1,3
         MIN_TEMP = MINVAL(VERTEX(:,II,FACET_ID))
         MAX_TEMP = MAXVAL(VERTEX(:,II,FACET_ID))
         IF(ABS(MAX_TEMP - MIN_TEMP) < SMALLEST_EXTENT ) THEN
            FACETS_AT_DG(IJK)%DIR(FACETS_AT_DG(IJK)%COUNT) = II
            FACETS_AT_DG(IJK)%MIN(FACETS_AT_DG(IJK)%COUNT) = MIN_TEMP
            FACETS_AT_DG(IJK)%MAX(FACETS_AT_DG(IJK)%COUNT) = MAX_TEMP
            SMALLEST_EXTENT = ABS(MAX_TEMP - MIN_TEMP)
         ENDIF
      ENDDO

      RETURN
      END SUBROUTINE ADD_FACET


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: FLAG_FACETS_WHERE_EDGE_COLLISION_IS_IGNORED             !
!  Author: Jeff Dietiker                             Date: 10-Sept-18  !
!                                                                      !
!  Purpose:                                                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      Subroutine FLAG_FACETS_WHERE_EDGE_COLLISION_IS_IGNORED

      IMPLICIT NONE

! DES Grid cell index.
      INTEGER :: IJK
! Loop counters:
      INTEGER :: II,JJ,KK
      INTEGER :: F1,F2, FACET_ID, NB_FACET_ID ! Facets
      INTEGER :: EDGE, NB_EDGE ! Edges
! Edge centers
      DOUBLE PRECISION, DIMENSION(3,3) :: EC, NB_EC
      DOUBLE PRECISION, DIMENSION(3) :: DIST
      DOUBLE PRECISION :: DISTSQ, FACET_DP


! Flag to keep track of which facets have been assigned the edge
! detection flag. Used to avoid testing the same facet twice
      LOGICAL, DIMENSION(:), ALLOCATABLE :: ASSIGNED_EDGE_INTERSECTION

! Tolerance to detect identical edges
      DOUBLE PRECISION, PARAMETER :: TOL_EDGE = 1.0D-9

      ALLOCATE(ASSIGNED_EDGE_INTERSECTION(DIM_STL))

! Convert angle to its cosine, used in dot product check
! Add small number as safety
      COS_STL_NB_ANGLE = DCOS(STL_NB_ANGLE/180.0D0*PI) + 1.0D-6


! Allocate the data storage array.
      IF(.not.allocated(FACETS_AT_DG)) &
         allocate(FACETS_AT_DG(DG_IJKSIZE2))

      ASSIGNED_EDGE_INTERSECTION(:) = .FALSE.
      IGNORE_EDGE_INTERSECTION(:)   = .FALSE.

      DO KK=DG_KSTART2,DG_KEND2
      DO JJ=DG_JSTART2,DG_JEND2
      DO II=DG_ISTART2,DG_IEND2
         IF(dg_is_ON_myPE_plus1layers(II,JJ,KK)) THEN
            IJK = DG_FUNIJK(II,JJ,KK)


! Within a cell, we compare the 3 edges centers of all facets with all
! edge centers of all other neighbor facets. If the centers match, this
! means the two facets share the same edge. In that case, we compare the
! angle between these two facets. If the angle is small (below
! STL_NB_ANGLE, defined in stl_mod.f), then the edge collision with be
! ignored. The idea is that this edge should not contribute tot he
! collision because the two triangles are representing the same surface
! (a plane or near planar surface).

! Loop over all facets in the DG cell IJK
            DO F1=1, FACETS_AT_DG(IJK)%COUNT
               FACET_ID = FACETS_AT_DG(IJK)%ID(F1)
               IF(.NOT.ASSIGNED_EDGE_INTERSECTION(FACET_ID)) THEN

! Edge 1: Vertex 1 to Vertex 2
                  EC(1,1) = HALF*(VERTEX(1,1,FACET_ID) + VERTEX(2,1,FACET_ID))
                  EC(1,2) = HALF*(VERTEX(1,2,FACET_ID) + VERTEX(2,2,FACET_ID))
                  EC(1,3) = HALF*(VERTEX(1,3,FACET_ID) + VERTEX(2,3,FACET_ID))

! Edge 2: Vertex 1 to Vertex 3
                  EC(2,1) = HALF*(VERTEX(1,1,FACET_ID) + VERTEX(3,1,FACET_ID))
                  EC(2,2) = HALF*(VERTEX(1,2,FACET_ID) + VERTEX(3,2,FACET_ID))
                  EC(2,3) = HALF*(VERTEX(1,3,FACET_ID) + VERTEX(3,3,FACET_ID))

! Edge 3: Vertex 2 to Vertex 3
                  EC(3,1) = HALF*(VERTEX(2,1,FACET_ID) + VERTEX(3,1,FACET_ID))
                  EC(3,2) = HALF*(VERTEX(2,2,FACET_ID) + VERTEX(3,2,FACET_ID))
                  EC(3,3) = HALF*(VERTEX(2,3,FACET_ID) + VERTEX(3,3,FACET_ID))

! Now loop through edges
                  DO EDGE = 1,3

! Loop through neighbor facets (current FACET_ID is excluded, i.e. we
! want F1 not equal to F2
                     DO F2=1, FACETS_AT_DG(IJK)%COUNT
                        IF(F1/=F2) THEN
                           NB_FACET_ID = FACETS_AT_DG(IJK)%ID(F2)

! Neighbor Edge 1: Vertex 1 to Vertex 2
                           NB_EC(1,1) = HALF*(VERTEX(1,1,NB_FACET_ID) + VERTEX(2,1,NB_FACET_ID))
                           NB_EC(1,2) = HALF*(VERTEX(1,2,NB_FACET_ID) + VERTEX(2,2,NB_FACET_ID))
                           NB_EC(1,3) = HALF*(VERTEX(1,3,NB_FACET_ID) + VERTEX(2,3,NB_FACET_ID))

! Neighbor Edge 2: Vertex 1 to Vertex 3
                           NB_EC(2,1) = HALF*(VERTEX(1,1,NB_FACET_ID) + VERTEX(3,1,NB_FACET_ID))
                           NB_EC(2,2) = HALF*(VERTEX(1,2,NB_FACET_ID) + VERTEX(3,2,NB_FACET_ID))
                           NB_EC(2,3) = HALF*(VERTEX(1,3,NB_FACET_ID) + VERTEX(3,3,NB_FACET_ID))

! Neighbor Edge 3: Vertex 2 to Vertex 3
                           NB_EC(3,1) = HALF*(VERTEX(2,1,NB_FACET_ID) + VERTEX(3,1,NB_FACET_ID))
                           NB_EC(3,2) = HALF*(VERTEX(2,2,NB_FACET_ID) + VERTEX(3,2,NB_FACET_ID))
                           NB_EC(3,3) = HALF*(VERTEX(2,3,NB_FACET_ID) + VERTEX(3,3,NB_FACET_ID))

! Loop through Neighbor edges
                           DO NB_EDGE = 1,3

                              DIST(:) = NB_EC(NB_EDGE,:) - EC(EDGE,:)
                              DISTSQ = DOT_PRODUCT(DIST, DIST)

                              IF(DISTSQ < TOL_EDGE) THEN
! The two facets share the same edge. Now test the angle between facets
                                  FACET_DP = DOT_PRODUCT(NORM_FACE(:,FACET_ID),NORM_FACE(:,NB_FACET_ID))
                                 IF(FACET_DP > COS_STL_NB_ANGLE) THEN
                                    IGNORE_EDGE_INTERSECTION(FACET_ID) = .TRUE.
                                    ASSIGNED_EDGE_INTERSECTION(FACET_ID) = .TRUE.
                                    IGNORE_EDGE_INTERSECTION(NB_FACET_ID) = .TRUE.
                                    ASSIGNED_EDGE_INTERSECTION(NB_FACET_ID) = .TRUE.

                                 ENDIF

                              ENDIF ! DISTSQ < TOL_EDGE


                           ENDDO ! Neighbor edges



                        ENDIF !(F1/=F2)
                     ENDDO ! F2 loop





                  ENDDO ! loop through edges of FACET_ID


               ENDIF ! .NOT.ASSIGNED_EDGE_INTERSECTION(FACET_ID)





            ENDDO ! F1 Loop through all facets in DG cell IJK




         ENDIF
      ENDDO
      ENDDO
      ENDDO

      DEALLOCATE(ASSIGNED_EDGE_INTERSECTION)

      RETURN
      END SUBROUTINE FLAG_FACETS_WHERE_EDGE_COLLISION_IS_IGNORED

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CONVERT_BC_WALLS_TO_STL                                 !
!  Author: J.Musser                                   Date: 03-Nov-15  !
!                                                                      !
!  Purpose: Convert user specified walls to STLs for particle-wall     !
!  collision detection.                                                !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      Subroutine CONVERT_BC_WALLS_TO_STL

      IMPLICIT NONE

! Loop counter.
      INTEGER :: BCV
! Extents of the BC region with respect to the fluid grid.
      DOUBLE PRECISION :: lXw, lXe, lYs, lYn, lZb, lZt

      CHARACTER(LEN=1) :: PLANE

      INTEGER :: OWNER
      INTEGER :: I_W, I_E, IPJK
      INTEGER :: J_S, J_N, IJPK
      INTEGER :: K_B, K_T, IJKP

      STL_START(BCWALLS_STL)=N_FACETS_DES+1

      DO BCV=1, DIMENSION_BC
         IF(.NOT.BC_DEFINED(BCV)) CYCLE

         IF(BC_TYPE_ENUM(BCV) == FREE_SLIP_WALL .OR.   &
            BC_TYPE_ENUM(BCV) == NO_SLIP_WALL   .OR.   &
            BC_TYPE_ENUM(BCV) == PAR_SLIP_WALL) THEN

            lXw = XE(BC_I_w(BCV)-1); lXe = XE(BC_I_e(BCV))
            lYs = YN(BC_J_s(BCV)-1); lYn = YN(BC_J_n(BCV))
            IF(DO_K) THEN
               lZb = ZT(BC_K_b(BCV)-1); lZt = ZT(BC_K_t(BCV))
            ELSE
               lZb = Z_MIN ;lZt = Z_MAX
            ENDIF

            K_B = BC_K_B(BCV)
            K_T = BC_K_T(BCV)

            J_S = BC_J_S(BCV)
            J_N = BC_J_N(BCV)

            I_W = BC_I_W(BCV)
            I_E = BC_I_E(BCV)

            PLANE = '.'

            IF(BC_X_w(BCV) == BC_X_e(BCV).or. &
               BC_Y_s(BCV) == BC_Y_n(BCV).or. &
               BC_Z_b(BCV) == BC_Z_t(BCV)) THEN

               OWNER = merge(myPE, 0, IS_ON_myPE_owns(I_W, J_S, K_B))
               CALL GLOBAL_ALL_SUM(OWNER)

               IF(myPE == OWNER) THEN

                  IF(BC_X_w(BCV) == BC_X_e(BCV)) THEN

                     IPJK = BOUND_FUNIJK(I_W+1, J_S, K_B)
                     PLANE = merge('E','W', ICBC_FLAG(IPJK)(1:1)=='.')

                  ELSEIF(BC_Y_s(BCV) == BC_Y_n(BCV)) THEN

                     IJPK = BOUND_FUNIJK(I_W, J_S+1, K_B)
                     PLANE = merge('N','S',ICBC_FLAG(IJPK)(1:1)=='.')

                  ELSEIF(DO_K .and. BC_Z_b(BCV) == BC_Z_t(BCV)) THEN

                     IJKP = BOUND_FUNIJK(I_W, J_S, K_B+1)
                     PLANE = merge('T','B',ICBC_FLAG(IJKP)(1:1)=='.')
                  ENDIF

               ENDIF
               CALL BCAST(PLANE, OWNER)
            ENDIF

            SELECT CASE(PLANE)
            CASE('E')

               N_FACETS_DES = N_FACETS_DES+1
               VERTEX(1,:,N_FACETS_DES) = (/lXe, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES) = (/lXe, lYn, lZb/)
               VERTEX(3,:,N_FACETS_DES) = (/lXe, lYn, lZt/)
               NORM_FACE(:,N_FACETS_DES) = (/ ONE, ZERO, ZERO/)

               N_FACETS_DES = N_FACETS_DES+1
               VERTEX(1,:,N_FACETS_DES) = (/lXe, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES) = (/lXe, lYn, lZt/)
               VERTEX(3,:,N_FACETS_DES) = (/lXe, lYs, lZt/)
               NORM_FACE(:,N_FACETS_DES) = (/ ONE, ZERO, ZERO/)

            CASE('W')

               N_FACETS_DES = N_FACETS_DES+1
               VERTEX(1,:,N_FACETS_DES) = (/lXw, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES) = (/lXw, lYn, lZb/)
               VERTEX(3,:,N_FACETS_DES) = (/lXw, lYn, lZt/)
               NORM_FACE(:,N_FACETS_DES) = (/-ONE, ZERO, ZERO/)

               N_FACETS_DES = N_FACETS_DES+1
               VERTEX(1,:,N_FACETS_DES) = (/lXw, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES) = (/lXw, lYn, lZt/)
               VERTEX(3,:,N_FACETS_DES) = (/lXw, lYs, lZt/)
               NORM_FACE(:,N_FACETS_DES) = (/-ONE, ZERO, ZERO/)

            CASE('N')

               N_FACETS_DES = N_FACETS_DES+1
               VERTEX(1,:,N_FACETS_DES) = (/lXw, lYn, lZb/)
               VERTEX(2,:,N_FACETS_DES) = (/lXe, lYn, lZb/)
               VERTEX(3,:,N_FACETS_DES) = (/lXe, lYn, lZt/)
               NORM_FACE(:,N_FACETS_DES) = (/ZERO,  ONE, ZERO/)

               N_FACETS_DES = N_FACETS_DES+1
               VERTEX(1,:,N_FACETS_DES) = (/lXw, lYn, lZb/)
               VERTEX(2,:,N_FACETS_DES) = (/lXe, lYn, lZt/)
               VERTEX(3,:,N_FACETS_DES) = (/lXw, lYn, lZt/)
               NORM_FACE(:,N_FACETS_DES) = (/ZERO,  ONE, ZERO/)

            CASE('S')

               N_FACETS_DES = N_FACETS_DES+1
               VERTEX(1,:,N_FACETS_DES) = (/lXw, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES) = (/lXe, lYs, lZb/)
               VERTEX(3,:,N_FACETS_DES) = (/lXe, lYs, lZt/)
               NORM_FACE(:,N_FACETS_DES) = (/ZERO, -ONE, ZERO/)

               N_FACETS_DES = N_FACETS_DES+1
               VERTEX(1,:,N_FACETS_DES) = (/lXw, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES) = (/lXe, lYs, lZt/)
               VERTEX(3,:,N_FACETS_DES) = (/lXw, lYs, lZt/)
               NORM_FACE(:,N_FACETS_DES) = (/ZERO, -ONE, ZERO/)


            CASE('T')

               N_FACETS_DES = N_FACETS_DES+1
               VERTEX(1,:,N_FACETS_DES) = (/lXw, lYs, lZt/)
               VERTEX(2,:,N_FACETS_DES) = (/lXe, lYs, lZt/)
               VERTEX(3,:,N_FACETS_DES) = (/lXe, lYn, lZt/)
               NORM_FACE(:,N_FACETS_DES) = (/ZERO, ZERO,  ONE/)

               N_FACETS_DES = N_FACETS_DES+1
               VERTEX(1,:,N_FACETS_DES) = (/lXw, lYs, lZt/)
               VERTEX(2,:,N_FACETS_DES) = (/lXe, lYn, lZt/)
               VERTEX(3,:,N_FACETS_DES) = (/lXw, lYn, lZt/)
               NORM_FACE(:,N_FACETS_DES) = (/ZERO, ZERO,  ONE/)

            CASE('B')

               N_FACETS_DES = N_FACETS_DES+1
               VERTEX(1,:,N_FACETS_DES) = (/lXw, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES) = (/lXe, lYs, lZb/)
               VERTEX(3,:,N_FACETS_DES) = (/lXe, lYn, lZb/)
               NORM_FACE(:,N_FACETS_DES) = (/ZERO, ZERO, -ONE/)

               N_FACETS_DES = N_FACETS_DES+1
               VERTEX(1,:,N_FACETS_DES) = (/lXw, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES) = (/lXe, lYn, lZb/)
               VERTEX(3,:,N_FACETS_DES) = (/lXw, lYn, lZb/)
               NORM_FACE(:,N_FACETS_DES) = (/ZERO, ZERO, -ONE/)

            CASE DEFAULT
               CALL GENERATE_STL_BOX(lXw, lXe, lYs, lYn, lZb, lZt)
            END SELECT


! Add to list of STL groups
         N_STL_GROUP                     = N_STL_GROUP + 1
         STL_GROUP(N_STL_GROUP)%TYPE     = 'BC_REG'
         STL_GROUP(N_STL_GROUP)%FILENAME = ''
         STL_GROUP(N_STL_GROUP)%ID       = BCV
         IF(N_STL_GROUP== 1) THEN
            STL_GROUP(N_STL_GROUP)%START = 1
         ELSE
            STL_GROUP(N_STL_GROUP)%START    = STL_GROUP(N_STL_GROUP - 1)%END + 1 
         ENDIF
         STL_GROUP(N_STL_GROUP)%END    = N_FACETS_DES 

         ENDIF
      ENDDO
      STL_END(BCWALLS_STL)=N_FACETS_DES

      RETURN
      END SUBROUTINE CONVERT_BC_WALLS_TO_STL

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CONVERT_BC_REGIONS_TO_STL                               !
!  Author: J.Musser                                   Date: 01-Jun-20  !
!                                                                      !
!  Purpose: Convert user specified BC regions to STLs                  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      Subroutine CONVERT_BC_REGIONS_TO_STL

      IMPLICIT NONE

! Loop counter.
      INTEGER :: BCV
! Extents of the BC region with respect to the fluid grid.
      DOUBLE PRECISION :: lXw, lXe, lYs, lYn, lZb, lZt

      CHARACTER(LEN=1) :: PLANE

      INTEGER :: OWNER
      INTEGER :: I_W, I_E, IPJK
      INTEGER :: J_S, J_N, IJPK
      INTEGER :: K_B, K_T, IJKP

      STL_START(BCWALLS_STL)=N_FACETS_DES+1

      DO BCV=1, DIMENSION_BC
         IF(.NOT.BC_DEFINED(BCV)) CYCLE

         ! IF(BC_TYPE_ENUM(BCV) == FREE_SLIP_WALL .OR.   &
         !    BC_TYPE_ENUM(BCV) == NO_SLIP_WALL   .OR.   &
         !    BC_TYPE_ENUM(BCV) == PAR_SLIP_WALL) THEN

           IF(.NOT.IS_CG(BC_TYPE_ENUM(BCV))) THEN


            lXw = XE(BC_I_w(BCV)-1); lXe = XE(BC_I_e(BCV))
            lYs = YN(BC_J_s(BCV)-1); lYn = YN(BC_J_n(BCV))
            IF(DO_K) THEN
               lZb = ZT(BC_K_b(BCV)-1); lZt = ZT(BC_K_t(BCV))
            ELSE
               lZb = Z_MIN ;lZt = Z_MAX
            ENDIF

            K_B = BC_K_B(BCV)
            K_T = BC_K_T(BCV)

            J_S = BC_J_S(BCV)
            J_N = BC_J_N(BCV)

            I_W = BC_I_W(BCV)
            I_E = BC_I_E(BCV)

            PLANE = '.'

            IF(BC_X_w(BCV) == BC_X_e(BCV).or. &
               BC_Y_s(BCV) == BC_Y_n(BCV).or. &
               BC_Z_b(BCV) == BC_Z_t(BCV)) THEN

               OWNER = merge(myPE, 0, IS_ON_myPE_owns(I_W, J_S, K_B))
               CALL GLOBAL_ALL_SUM(OWNER)

               IF(myPE == OWNER) THEN

                  IF(BC_X_w(BCV) == BC_X_e(BCV)) THEN

                     IPJK = BOUND_FUNIJK(I_W+1, J_S, K_B)
                     PLANE = merge('E','W', ICBC_FLAG(IPJK)(1:1)=='.')

                  ELSEIF(BC_Y_s(BCV) == BC_Y_n(BCV)) THEN

                     IJPK = BOUND_FUNIJK(I_W, J_S+1, K_B)
                     PLANE = merge('N','S',ICBC_FLAG(IJPK)(1:1)=='.')

                  ELSEIF(DO_K .and. BC_Z_b(BCV) == BC_Z_t(BCV)) THEN

                     IJKP = BOUND_FUNIJK(I_W, J_S, K_B+1)
                     PLANE = merge('T','B',ICBC_FLAG(IJKP)(1:1)=='.')
                  ENDIF

               ENDIF
               CALL BCAST(PLANE, OWNER)
            ENDIF

            SELECT CASE(PLANE)
            CASE('E')

               N_FACETS_DES                 = N_FACETS_DES + 1
               VERTEX(1,:,N_FACETS_DES)     = (/lXe, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES)     = (/lXe, lYn, lZb/)
               VERTEX(3,:,N_FACETS_DES)     = (/lXe, lYn, lZt/)
               NORM_FACE(:,N_FACETS_DES)    = (/ ONE, ZERO, ZERO/)
               BC_ID_STL_FACE(N_FACETS_DES) = BCV

               N_FACETS_DES                 = N_FACETS_DES + 1
               VERTEX(1,:,N_FACETS_DES)     = (/lXe, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES)     = (/lXe, lYn, lZt/)
               VERTEX(3,:,N_FACETS_DES)     = (/lXe, lYs, lZt/)
               NORM_FACE(:,N_FACETS_DES)    = (/ ONE, ZERO, ZERO/)
               BC_ID_STL_FACE(N_FACETS_DES) = BCV

            CASE('W')

               N_FACETS_DES                 = N_FACETS_DES + 1
               VERTEX(1,:,N_FACETS_DES)     = (/lXw, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES)     = (/lXw, lYn, lZb/)
               VERTEX(3,:,N_FACETS_DES)     = (/lXw, lYn, lZt/)
               NORM_FACE(:,N_FACETS_DES)    = (/-ONE, ZERO, ZERO/)
               BC_ID_STL_FACE(N_FACETS_DES) = BCV

               N_FACETS_DES                 = N_FACETS_DES + 1
               VERTEX(1,:,N_FACETS_DES)     = (/lXw, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES)     = (/lXw, lYn, lZt/)
               VERTEX(3,:,N_FACETS_DES)     = (/lXw, lYs, lZt/)
               NORM_FACE(:,N_FACETS_DES)    = (/-ONE, ZERO, ZERO/)
               BC_ID_STL_FACE(N_FACETS_DES) = BCV

            CASE('N')

               N_FACETS_DES                 = N_FACETS_DES + 1
               VERTEX(1,:,N_FACETS_DES)     = (/lXw, lYn, lZb/)
               VERTEX(2,:,N_FACETS_DES)     = (/lXe, lYn, lZb/)
               VERTEX(3,:,N_FACETS_DES)     = (/lXe, lYn, lZt/)
               NORM_FACE(:,N_FACETS_DES)    = (/ZERO,  ONE, ZERO/)
               BC_ID_STL_FACE(N_FACETS_DES) = BCV

               N_FACETS_DES                 = N_FACETS_DES + 1
               VERTEX(1,:,N_FACETS_DES)     = (/lXw, lYn, lZb/)
               VERTEX(2,:,N_FACETS_DES)     = (/lXe, lYn, lZt/)
               VERTEX(3,:,N_FACETS_DES)     = (/lXw, lYn, lZt/)
               NORM_FACE(:,N_FACETS_DES)    = (/ZERO,  ONE, ZERO/)
               BC_ID_STL_FACE(N_FACETS_DES) = BCV

            CASE('S')

               N_FACETS_DES                 = N_FACETS_DES + 1
               VERTEX(1,:,N_FACETS_DES)     = (/lXw, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES)     = (/lXe, lYs, lZb/)
               VERTEX(3,:,N_FACETS_DES)     = (/lXe, lYs, lZt/)
               NORM_FACE(:,N_FACETS_DES)    = (/ZERO, -ONE, ZERO/)
               BC_ID_STL_FACE(N_FACETS_DES) = BCV

               N_FACETS_DES                 = N_FACETS_DES + 1
               VERTEX(1,:,N_FACETS_DES)     = (/lXw, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES)     = (/lXe, lYs, lZt/)
               VERTEX(3,:,N_FACETS_DES)     = (/lXw, lYs, lZt/)
               NORM_FACE(:,N_FACETS_DES)    = (/ZERO, -ONE, ZERO/)
               BC_ID_STL_FACE(N_FACETS_DES) = BCV


            CASE('T')

               N_FACETS_DES                 = N_FACETS_DES + 1
               VERTEX(1,:,N_FACETS_DES)     = (/lXw, lYs, lZt/)
               VERTEX(2,:,N_FACETS_DES)     = (/lXe, lYs, lZt/)
               VERTEX(3,:,N_FACETS_DES)     = (/lXe, lYn, lZt/)
               NORM_FACE(:,N_FACETS_DES)    = (/ZERO, ZERO,  ONE/)
               BC_ID_STL_FACE(N_FACETS_DES) = BCV

               N_FACETS_DES                 = N_FACETS_DES + 1
               VERTEX(1,:,N_FACETS_DES)     = (/lXw, lYs, lZt/)
               VERTEX(2,:,N_FACETS_DES)     = (/lXe, lYn, lZt/)
               VERTEX(3,:,N_FACETS_DES)     = (/lXw, lYn, lZt/)
               NORM_FACE(:,N_FACETS_DES)    = (/ZERO, ZERO,  ONE/)
               BC_ID_STL_FACE(N_FACETS_DES) = BCV

            CASE('B')

               N_FACETS_DES                 = N_FACETS_DES + 1
               VERTEX(1,:,N_FACETS_DES)     = (/lXw, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES)     = (/lXe, lYs, lZb/)
               VERTEX(3,:,N_FACETS_DES)     = (/lXe, lYn, lZb/)
               NORM_FACE(:,N_FACETS_DES)    = (/ZERO, ZERO, -ONE/)
               BC_ID_STL_FACE(N_FACETS_DES) = BCV

               N_FACETS_DES                 = N_FACETS_DES + 1
               VERTEX(1,:,N_FACETS_DES)     = (/lXw, lYs, lZb/)
               VERTEX(2,:,N_FACETS_DES)     = (/lXe, lYn, lZb/)
               VERTEX(3,:,N_FACETS_DES)     = (/lXw, lYn, lZb/)
               NORM_FACE(:,N_FACETS_DES)    = (/ZERO, ZERO, -ONE/)
               BC_ID_STL_FACE(N_FACETS_DES) = BCV

            CASE DEFAULT
               CALL GENERATE_STL_BOX(lXw, lXe, lYs, lYn, lZb, lZt)
            END SELECT

! Add to list of STL groups
         N_STL_GROUP                     = N_STL_GROUP + 1
         STL_GROUP(N_STL_GROUP)%TYPE     = 'BC_REG'
         STL_GROUP(N_STL_GROUP)%FILENAME = ''
         STL_GROUP(N_STL_GROUP)%ID       = -1
         IF(N_STL_GROUP== 1) THEN
            STL_GROUP(N_STL_GROUP)%START = 1
         ELSE
            STL_GROUP(N_STL_GROUP)%START    = STL_GROUP(N_STL_GROUP - 1)%END + 1 
         ENDIF
         STL_GROUP(N_STL_GROUP)%END    = N_FACETS_DES 

          ENDIF
      ENDDO
      STL_END(BCWALLS_STL)=N_FACETS_DES

      RETURN
      END SUBROUTINE CONVERT_BC_REGIONS_TO_STL



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CONVERT_IMPERMEABLE_IS_TO_STL                           !
!  Author: J.Musser                                   Date: 03-Nov-15  !
!                                                                      !
!  Purpose: Convert user specified impermeable surfaces to STLs for    !
!  particle-wall collision detection.                                  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CONVERT_IMPERMEABLE_IS_TO_STL

      IMPLICIT NONE

! Loop counter.
      INTEGER :: ISV
! Extents of the BC region with respect to the fluid grid.
      DOUBLE PRECISION :: lXw, lXe, lYs, lYn, lZb, lZt

      STL_START(IMPRMBL_STL)=N_FACETS_DES+1

      DO ISV=1, DIMENSION_IS
         IF(.NOT.IS_DEFINED(ISV)) CYCLE

         IF(trim(IS_TYPE(ISV)) == 'IMPERMEABLE') THEN

            lXw = XE(IS_I_w(ISV)-1); lXe = XE(IS_I_e(ISV))
            lYs = YN(IS_J_s(ISV)-1); lYn = YN(IS_J_n(ISV))
            IF(DO_K) THEN
               lZb = ZT(IS_K_b(ISV)-1); lZt = ZT(IS_K_t(ISV))
            ELSE
               lZb = Z_MIN ;lZt = Z_MAX
            ENDIF

            CALL GENERATE_STL_BOX(lXw, lXe, lYs, lYn, lZb, lZt)


! Add to list of STL groups
            N_STL_GROUP                     = N_STL_GROUP + 1
            STL_GROUP(N_STL_GROUP)%TYPE     = 'IS_REG'
            STL_GROUP(N_STL_GROUP)%FILENAME = ''
            STL_GROUP(N_STL_GROUP)%ID       = ISV
            IF(N_STL_GROUP== 1) THEN
               STL_GROUP(N_STL_GROUP)%START = 1
            ELSE
               STL_GROUP(N_STL_GROUP)%START    = STL_GROUP(N_STL_GROUP - 1)%END + 1 
            ENDIF
            STL_GROUP(N_STL_GROUP)%END    = N_FACETS_DES 

         ELSE
            WRITE(ERR_MSG,1000) ISV, IS_TYPE(ISV)
            CALL LOG_ERROR()
         ENDIF
      ENDDO

      STL_END(IMPRMBL_STL)=N_FACETS_DES

 1000 FORMAT("Error 1000: DES simulations do not support the ",/       &
         'specified IS TYPE:',/3x,'IS: ',I3,/3x,'IS_TYPE=',A)

      RETURN
      END SUBROUTINE CONVERT_IMPERMEABLE_IS_TO_STL


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CONVERT_DEFAULT_WALLS_TO_STL                            !
!  Author: J.Musser                                   Date: 03-Nov-15  !
!                                                                      !
!  Purpose: Convert user specified walls to STLs for particle-wall     !
!  collision detection.                                                !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      Subroutine CONVERT_DEFAULT_WALLS_TO_STL

      IMPLICIT NONE

      STL_START(DEFAULT_STL)=N_FACETS_DES+1

! West Face
      IF(.NOT.DES_PERIODIC_WALLS_X)THEN
         N_FACETS_DES = N_FACETS_DES+1
         VERTEX(1,:,N_FACETS_DES) = (/X_MIN, Y_MIN, Z_MIN/)
         VERTEX(2,:,N_FACETS_DES) = (/X_MIN, Y_MAX+YLENGTH, Z_MIN/)
         VERTEX(3,:,N_FACETS_DES) = (/X_MIN, Y_MIN, Z_MAX+ZLENGTH/)
         NORM_FACE(:,N_FACETS_DES) = (/ONE, ZERO, ZERO/)

! East Face
         N_FACETS_DES = N_FACETS_DES+1
         VERTEX(1,:,N_FACETS_DES) = (/X_MAX, Y_MIN, Z_MIN/)
         VERTEX(2,:,N_FACETS_DES) = (/X_MAX, Y_MAX+YLENGTH, Z_MIN/)
         VERTEX(3,:,N_FACETS_DES) = (/X_MAX, Y_MIN, Z_MAX+ZLENGTH/)
         NORM_FACE(:,N_FACETS_DES) = (/-ONE, ZERO, ZERO/)
      ENDIF

! South Face
      IF(.NOT.DES_PERIODIC_WALLS_Y)THEN
         N_FACETS_DES = N_FACETS_DES+1
         VERTEX(1,:,N_FACETS_DES) = (/X_MIN, Y_MIN, Z_MIN/)
         VERTEX(2,:,N_FACETS_DES) = (/X_MAX+XLENGTH, Y_MIN, Z_MIN/)
         VERTEX(3,:,N_FACETS_DES) = (/X_MIN, Y_MIN, Z_MAX+ZLENGTH/)
         NORM_FACE(:,N_FACETS_DES) = (/ZERO, ONE, ZERO/)

! North Face
         N_FACETS_DES = N_FACETS_DES+1
         VERTEX(1,:,N_FACETS_DES) = (/X_MIN, Y_MAX, Z_MIN/)
         VERTEX(2,:,N_FACETS_DES) = (/X_MAX+XLENGTH, Y_MAX, Z_MIN/)
         VERTEX(3,:,N_FACETS_DES) = (/X_MIN, Y_MAX, Z_MAX+ZLENGTH/)
         NORM_FACE(:,N_FACETS_DES) = (/ZERO, -ONE, ZERO/)
      ENDIF

! Bottom Face
      IF(.NOT.DES_PERIODIC_WALLS_Z .AND. DO_K) THEN
         N_FACETS_DES = N_FACETS_DES+1
         VERTEX(1,:,N_FACETS_DES) = (/X_MIN, Y_MIN, Z_MIN/)
         VERTEX(2,:,N_FACETS_DES) = (/X_MAX+XLENGTH, Y_MIN, Z_MIN/)
         VERTEX(3,:,N_FACETS_DES) = (/X_MIN, Y_MAX+YLENGTH, Z_MIN/)
         NORM_FACE(:,N_FACETS_DES) = (/ZERO, ZERO, ONE/)

! Top Face
         N_FACETS_DES = N_FACETS_DES+1
         VERTEX(1,:,N_FACETS_DES) = (/X_MIN, Y_MIN, Z_MAX/)
         VERTEX(2,:,N_FACETS_DES) = (/X_MAX+XLENGTH, Y_MIN, Z_MAX/)
         VERTEX(3,:,N_FACETS_DES) = (/X_MIN, Y_MAX+YLENGTH, Z_MAX/)
         NORM_FACE(:,N_FACETS_DES) = (/ZERO, ZERO, -ONE/)
      ENDIF

      STL_END(DEFAULT_STL)=N_FACETS_DES

! Add to list of STL groups
         N_STL_GROUP                     = N_STL_GROUP + 1
         STL_GROUP(N_STL_GROUP)%TYPE     = 'BC_DEF'
         STL_GROUP(N_STL_GROUP)%FILENAME = ''
         STL_GROUP(N_STL_GROUP)%ID       = -1
         IF(N_STL_GROUP== 1) THEN
            STL_GROUP(N_STL_GROUP)%START = 1
         ELSE
            STL_GROUP(N_STL_GROUP)%START    = STL_GROUP(N_STL_GROUP - 1)%END + 1 
         ENDIF
         STL_GROUP(N_STL_GROUP)%END    = N_FACETS_DES 

      RETURN
      END SUBROUTINE CONVERT_DEFAULT_WALLS_TO_STL

      ! END MODULE STL_PREPROC_DES




!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: GENERATE_STL_BOX                                        !
!  Author: J.Musser                                   Date: 03-Nov-15  !
!                                                                      !
!  Purpose: Given the six corners of a box, create the 12 STLs needed  !
!  to define the geometry.                                             !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE GENERATE_STL_BOX(pXw, pXe, pYs, pYn, pZb, pZt)

      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(IN) :: pXw, pXe, pYs, pYn, pZb, pZt

! West Face
      N_FACETS_DES = N_FACETS_DES+1
      VERTEX(1,:,N_FACETS_DES) = (/pXw, pYs, pZb/)
      VERTEX(2,:,N_FACETS_DES) = (/pXw, pYn, pZb/)
      VERTEX(3,:,N_FACETS_DES) = (/pXw, pYn, pZt/)
      NORM_FACE(:,N_FACETS_DES) = (/-ONE, ZERO, ZERO/)

      IF(DO_K)THEN
         N_FACETS_DES = N_FACETS_DES+1
         VERTEX(1,:,N_FACETS_DES) = (/pXw, pYs, pZt/)
         VERTEX(2,:,N_FACETS_DES) = (/pXw, pYs, pZb/)
         VERTEX(3,:,N_FACETS_DES) = (/pXw, pYn, pZt/)
         NORM_FACE(:,N_FACETS_DES) = (/-ONE, ZERO, ZERO/)
      ENDIF

! East Face
      N_FACETS_DES = N_FACETS_DES+1
      VERTEX(3,:,N_FACETS_DES) = (/pXe, pYs, pZb/)
      VERTEX(2,:,N_FACETS_DES) = (/pXe, pYn, pZb/)
      VERTEX(1,:,N_FACETS_DES) = (/pXe, pYn, pZt/)
      NORM_FACE(:,N_FACETS_DES) = (/ONE, ZERO, ZERO/)

      IF(DO_K) THEN
         N_FACETS_DES = N_FACETS_DES+1
         VERTEX(3,:,N_FACETS_DES) = (/pXe, pYs, pZt/)
         VERTEX(2,:,N_FACETS_DES) = (/pXe, pYs, pZb/)
         VERTEX(1,:,N_FACETS_DES) = (/pXe, pYn, pZt/)
         NORM_FACE(:,N_FACETS_DES) = (/ONE, ZERO, ZERO/)
      ENDIF

! South Face
      N_FACETS_DES = N_FACETS_DES+1
      VERTEX(1,:,N_FACETS_DES) = (/pXw, pYs, pZb/)
      VERTEX(2,:,N_FACETS_DES) = (/pXe, pYs, pZb/)
      VERTEX(3,:,N_FACETS_DES) = (/pXw, pYs, pZt/)
      NORM_FACE(:,N_FACETS_DES) = (/ZERO, -ONE, ZERO/)

      IF(DO_K) THEN
         N_FACETS_DES = N_FACETS_DES+1
         VERTEX(1,:,N_FACETS_DES) = (/pXe, pYs, pZt/)
         VERTEX(2,:,N_FACETS_DES) = (/pXe, pYs, pZb/)
         VERTEX(3,:,N_FACETS_DES) = (/pXw, pYs, pZt/)
         NORM_FACE(:,N_FACETS_DES) = (/ZERO, -ONE, ZERO/)
      ENDIF

! North Face
      N_FACETS_DES = N_FACETS_DES+1
      VERTEX(3,:,N_FACETS_DES) = (/pXw, pYn, pZb/)
      VERTEX(2,:,N_FACETS_DES) = (/pXe, pYn, pZb/)
      VERTEX(1,:,N_FACETS_DES) = (/pXw, pYn, pZt/)
      NORM_FACE(:,N_FACETS_DES) = (/ZERO, ONE, ZERO/)

      IF(DO_K) THEN
         N_FACETS_DES = N_FACETS_DES+1
         VERTEX(3,:,N_FACETS_DES) = (/pXe, pYn, pZt/)
         VERTEX(2,:,N_FACETS_DES) = (/pXe, pYn, pZb/)
         VERTEX(1,:,N_FACETS_DES) = (/pXw, pYn, pZt/)
         NORM_FACE(:,N_FACETS_DES) = (/ZERO, ONE, ZERO/)
      ENDIF

! Bottom Face
      IF(DO_K)THEN
         N_FACETS_DES = N_FACETS_DES+1
         VERTEX(1,:,N_FACETS_DES) = (/pXw, pYs, pZb/)
         VERTEX(2,:,N_FACETS_DES) = (/pXe, pYs, pZb/)
         VERTEX(3,:,N_FACETS_DES) = (/pXe, pYn, pZb/)
         NORM_FACE(:,N_FACETS_DES) = (/ZERO, ZERO, -ONE/)

         N_FACETS_DES = N_FACETS_DES+1
         VERTEX(1,:,N_FACETS_DES) = (/pXe, pYn, pZb/)
         VERTEX(2,:,N_FACETS_DES) = (/pXw, pYn, pZb/)
         VERTEX(3,:,N_FACETS_DES) = (/pXw, pYs, pZb/)
         NORM_FACE(:,N_FACETS_DES) = (/ZERO, ZERO, -ONE/)

! Top Face
         N_FACETS_DES = N_FACETS_DES+1
         VERTEX(3,:,N_FACETS_DES) = (/pXw, pYs, pZt/)
         VERTEX(2,:,N_FACETS_DES) = (/pXe, pYs, pZt/)
         VERTEX(1,:,N_FACETS_DES) = (/pXe, pYn, pZt/)
         NORM_FACE(:,N_FACETS_DES) = (/ZERO, ZERO, ONE/)

         N_FACETS_DES = N_FACETS_DES+1
         VERTEX(3,:,N_FACETS_DES) = (/pXe, pYn, pZt/)
         VERTEX(2,:,N_FACETS_DES) = (/pXw, pYn, pZt/)
         VERTEX(1,:,N_FACETS_DES) = (/pXw, pYs, pZt/)
         NORM_FACE(:,N_FACETS_DES) = (/ZERO, ZERO, ONE/)
      ENDIF

      RETURN

   END SUBROUTINE GENERATE_STL_BOX

END MODULE STL_PREPROC_DES
