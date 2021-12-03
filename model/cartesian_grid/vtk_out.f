#include "error.inc"
#include "version.inc"

MODULE vtk_out_mod

   use bc
   USE calc_cell_mod, only: calc_cell
   USE calc_vorticity_mod, only: calc_vorticity
   USE cdist, only: bdist_io
   USE compar, only: iend3_all
   USE compar, only: jend3_all
   USE compar, only: kend3_all
   USE compar, only: myPE, PE_IO, NODESI,NODESJ,NODESK, ijkend3
   USE cutcell
   USE discretelement
   USE dmp_cartesian, only: send_receive_1d_logical
   USE error_manager
   USE eval_f_mod, only: eval_f
   USE fldvar
   USE fs_util, only: create_dir
   USE functions
   USE funits, only: UNIT_VTU, UNIT_PVTU, UNIT_VTR, UNIT_MSH_STATS
   USE geometry, only: XLENGTH,YLENGTH,ZLENGTH
   USE geometry, only: imin1, jmin1, kmin1
   USE geometry, only: x_min,x_max,y_min,y_max,z_min,z_max
   USE geometry, ONLY: Aspect_Ratio, Aspect_Ratio_U, Aspect_Ratio_V, Aspect_Ratio_W
   USE get_connectivity_mod, only: get_global_cell_node_coordinates, get_cell_node_coordinates
   USE get_cut_cell_volume_and_areas_mod, only: reorder_polygon
   USE indices, only: i_of, j_of, k_of
   USE make_upper_case_mod, only: make_upper_case
   USE mpi_utility
   USE parallel
   USE parallel_mpi
   USE param1, only: zero
   USE parse
   USE physprop, only: mmax, nmax, mw_g, MW_MIX_g
   USE polygon, only: n_polygon
   USE pvd_mod, only: read_pvd_frames, open_pvd_file, update_and_close_pvd_file, write_pvd_frames
   USE quadric, only: tol_f
   USE resize, only: real_grow, integer_grow, integer_grow2_reverse
   USE run, only: subgrid_wall, run_name, time, generate_mesh, ppo
   USE rxns
   USE des_rxns
   USE scalars, only: nscalar
   USE sendrecv
   USE set_geometry1_mod, only: set_geometry1
   USE set_increments_mod, only: unshift_dp_array
   USE stl
   USE toleranc, only: compare
   USE turb, only: k_epsilon
   USE vtk
   USE vtp, only: write_vtp_file, update_frames
   USE, intrinsic :: iso_c_binding
   USE WRITE_OUT0_MOD, only:location

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_DBG_VTU_AND_VTP_FILES                            C
!  Purpose: Writes the cell and particle data in VTK format            C
!           for debug regions only.                                    C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 22-Jul-15  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE WRITE_DBG_VTU_AND_VTP_FILES

      IMPLICIT NONE
      INTEGER :: LC

      DO LC = 1, DIMENSION_VTK
         CALL WRITE_VTU_FILE(LC,1)
         IF(DISCRETE_ELEMENT) CALL WRITE_VTP_FILE(LC,1)
      ENDDO

      END SUBROUTINE WRITE_DBG_VTU_AND_VTP_FILES

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_VTU_FILE                                         C
!  Purpose: Writes the cell data grid in VTK format (Unstructured VTU) C
!           Binary format                                              C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE WRITE_VTU_FILE(LCV,MODE)

      IMPLICIT NONE
      INTEGER :: I,J,K,M,NN,R,IJK,LCV

      INTEGER :: SPECIES_COUNTER,LT

      CHARACTER (LEN=32) :: SUBM,SUBN,SUBR
      CHARACTER (LEN=284) :: VAR_NAME

      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  DP_BC_ID, IJK_ARRAY,DP_PARTITION
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  EP_S_ARRAY
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  REL_VOL ! Relative volume (cut/uncut)

      INTEGER :: PASS
      INTEGER :: WRITE_HEADER = 1
      INTEGER :: WRITE_DATA   = 2
      INTEGER :: MODE   ! MODE = 0 : Write regular VTK region file
                        ! MODE = 1 : Write debug   VTK region file (VTK_DBG_FILE = .TRUE.)
                        ! MODE = 2 : Write mesh file

! Bypass re-indexng when writing the mesh file
      LOGICAL :: RE_INDEXING_BCK

      ! There is nothing to write if we are not in adefined vtk region
      VTK_REGION = LCV
      IF(.NOT.VTK_DEFINED(VTK_REGION)) RETURN
      IF(VTK_DATA(LCV)/='C'.AND.VTK_DATA(LCV)/='G') RETURN
      IF(MODE==0.AND.(VTK_DBG_FILE(LCV))) RETURN
      IF(MODE==1.AND.(.NOT.VTK_DBG_FILE(LCV))) RETURN

!     Location of U-momentum cells for original (uncut grid)
      IF (DO_I) THEN
        XG_E(0:1) = X_MIN
        DO I = IMIN1, IMAX2
           XG_E(I) = XG_E(I-1) + DX(I)
        END DO
      ENDIF

!     Location of V-momentum cells for original (uncut grid)
      IF (DO_J) THEN
        YG_N(0:1) = Y_MIN
        DO J = JMIN1, JMAX2
           YG_N(J) = YG_N(J-1) + DY(J)
        END DO
      ENDIF

!     Location of W-momentum cells for original (uncut grid)
      IF (DO_K) THEN
        ZG_T(0:1) = Z_MIN
        DO K = KMIN1, KMAX2
           ZG_T(K) = ZG_T(K-1) + DZ(K)
        END DO
      ELSE
         ZG_T = ZERO
      ENDIF


      CALL SETUP_VTK_REGION

      CALL WRITE_VTU_VTR_PVTU_FILES(MODE)

      IF(MODE==0) CALL OPEN_PVD_FILE

      ! IF(.NOT.VTK_DOMAIN_DECOMPOSITION(VTK_REGION)) THEN
      IF(VTK_DATA(VTK_REGION)=='C') THEN

         CALL CLEAN_GEOMETRY

         DO PASS=WRITE_HEADER,WRITE_DATA


            CALL WRITE_GEOMETRY_IN_VTU_BIN(PASS)

            IF(VTK_EP_g(VTK_REGION).AND.ALLOCATED(EP_G)) &
               CALL WRITE_SCALAR_IN_VTU_BIN('EP_G',EP_G,PASS)

            IF(VTK_P_g(VTK_REGION).AND.ALLOCATED(P_G)) &
               CALL WRITE_SCALAR_IN_VTU_BIN('P_G',P_G,PASS)

            IF(VTK_VEL_g(VTK_REGION).AND.ALLOCATED(U_G).AND.ALLOCATED(V_G).AND.ALLOCATED(W_G)) &
               CALL WRITE_VECTOR_IN_VTU_BIN('Gas_Velocity',U_G,V_G,W_G,PASS)

            IF(VTK_U_g(VTK_REGION).AND.ALLOCATED(U_G)) &
               CALL WRITE_SCALAR_IN_VTU_BIN('U_G',U_G,PASS)

            IF(VTK_V_g(VTK_REGION).AND.ALLOCATED(V_G)) &
               CALL WRITE_SCALAR_IN_VTU_BIN('V_G',V_G,PASS)

            IF(VTK_W_g(VTK_REGION).AND.ALLOCATED(W_G)) &
               CALL WRITE_SCALAR_IN_VTU_BIN('W_G',W_G,PASS)

            IF(VTK_P_star(VTK_REGION).AND.ALLOCATED(P_STAR)) &
               CALL WRITE_SCALAR_IN_VTU_BIN('P_STAR',P_STAR,PASS)

            DO M = 1,MMAX
               IF(VTK_P_s(VTK_REGION,M).AND.ALLOCATED(P_S)) THEN
                  WRITE(SUBM,*)M
                  CALL WRITE_SCALAR_IN_VTU_BIN('P_S_'//ADJUSTL(SUBM),P_S(:,M),PASS)
               ENDIF
               IF(VTK_VEL_s(VTK_REGION,M).AND.ALLOCATED(U_S).AND.ALLOCATED(V_S).AND.ALLOCATED(W_S)) THEN
                  WRITE(SUBM,*)M
                  CALL WRITE_VECTOR_IN_VTU_BIN('Solids_Velocity_'//ADJUSTL(SUBM),U_S(:,M),V_S(:,M),W_S(:,M),PASS)
               ENDIF
               IF(VTK_U_s(VTK_REGION,M).AND.ALLOCATED(U_S)) THEN
                  WRITE(SUBM,*)M
                  CALL WRITE_SCALAR_IN_VTU_BIN('U_s_'//ADJUSTL(SUBM),U_S(:,M),PASS)
               ENDIF
               IF(VTK_V_s(VTK_REGION,M).AND.ALLOCATED(V_S)) THEN
                  WRITE(SUBM,*)M
                  CALL WRITE_SCALAR_IN_VTU_BIN('V_s_'//ADJUSTL(SUBM),V_S(:,M),PASS)
               ENDIF
               IF(VTK_W_s(VTK_REGION,M).AND.ALLOCATED(W_S)) THEN
                  WRITE(SUBM,*)M
                  CALL WRITE_SCALAR_IN_VTU_BIN('W_s_'//ADJUSTL(SUBM),W_S(:,M),PASS)
               ENDIF
            END DO

            DO M = 1,MMAX
               IF(VTK_ROP_s(VTK_REGION,M).AND.ALLOCATED(ROP_S)) THEN
                  WRITE(SUBM,*)M
                  CALL WRITE_SCALAR_IN_VTU_BIN('Solids_bulk_density_'//ADJUSTL(SUBM),ROP_S(:,M),PASS)
               ENDIF
            END DO

            DO M = 1,MMAX
               IF(VTK_RO_s(VTK_REGION,M).AND.ALLOCATED(RO_S)) THEN
                  WRITE(SUBM,*)M
                  CALL WRITE_SCALAR_IN_VTU_BIN('Solids_density_'//ADJUSTL(SUBM),RO_S(:,M),PASS)
               ENDIF
            END DO

            DO M = 1,MMAX
               IF(VTK_EP_s(VTK_REGION,M).AND.ALLOCATED(ROP_S)) THEN
                  WRITE(SUBM,*)M
                  Allocate(EP_S_ARRAY(DIMENSION_3))
                  EP_S_ARRAY(:) = ZERO
                  DO IJK = IJKSTART3, IJKEND3
                     IF(RO_S(IJK,M)>ZERO) EP_S_ARRAY(IJK) = ROP_S(IJK,M)/RO_S(IJK,M)
                  ENDDO
                  CALL WRITE_SCALAR_IN_VTU_BIN('Solids_volume_fraction_'//ADJUSTL(SUBM),EP_S_ARRAY(:),PASS)
                  Deallocate(EP_S_ARRAY)
               ENDIF
            END DO

            IF(VTK_T_g(VTK_REGION).AND.ALLOCATED(T_G)) &
               CALL WRITE_SCALAR_IN_VTU_BIN('Gas_temperature',T_g,PASS)

            DO M = 1,MMAX
               IF(VTK_T_s(VTK_REGION,M).AND.ALLOCATED(T_S)) THEN
                  WRITE(SUBM,*)M
                  CALL WRITE_SCALAR_IN_VTU_BIN('Solids_temperature_'//ADJUSTL(SUBM),T_S(:,M),PASS)
               ENDIF
            END DO

! Gas mixture molecular weight            
            IF(VTK_MW_MIX_g(VTK_REGION).AND.ALLOCATED(MW_MIX_G)) &
               CALL WRITE_SCALAR_IN_VTU_BIN('Gas_Mixture_MW',MW_MIX_g,PASS)

            SPECIES_COUNTER = 0
            DO NN = 1,NMAX(0)
               IF(VTK_X_g(VTK_REGION,NN).AND.ALLOCATED(X_g)) THEN
                  WRITE(SUBN,*)NN
                  IF(USE_RRATES) THEN
                     SPECIES_COUNTER = SPECIES_COUNTER + 1
                     VAR_NAME = ADJUSTL(SPECIES_NAME(SPECIES_COUNTER))
                     LT = LEN_TRIM(ADJUSTL(SPECIES_NAME(SPECIES_COUNTER)))
                  ELSE
                     VAR_NAME = ADJUSTL(SPECIES_ALIAS_g(NN))
                     LT = LEN_TRIM(ADJUSTL(SPECIES_ALIAS_g(NN)))
                  ENDIF
                  VAR_NAME = VAR_NAME(1:LT)//'_Gas_mass_fractions_'//ADJUSTL(SUBN)
                  CALL WRITE_SCALAR_IN_VTU_BIN(VAR_NAME,X_g(:,NN),PASS)
               ENDIF
            END DO

! Gas molar fractions
            SPECIES_COUNTER = 0
            DO NN = 1,NMAX(0)
               IF(VTK_Y_g(VTK_REGION,NN).AND.ALLOCATED(X_g)) THEN
                  WRITE(SUBN,*)NN
                  IF(USE_RRATES) THEN
                     SPECIES_COUNTER = SPECIES_COUNTER + 1
                     VAR_NAME = ADJUSTL(SPECIES_NAME(SPECIES_COUNTER))
                     LT = LEN_TRIM(ADJUSTL(SPECIES_NAME(SPECIES_COUNTER)))
                  ELSE
                     VAR_NAME = ADJUSTL(SPECIES_ALIAS_g(NN))
                     LT = LEN_TRIM(ADJUSTL(SPECIES_ALIAS_g(NN)))
                  ENDIF
                  VAR_NAME = VAR_NAME(1:LT)//'_Gas_molar_fractions_'//ADJUSTL(SUBN)
                  CALL WRITE_SCALAR_IN_VTU_BIN(VAR_NAME,X_g(:,NN)/MW_G(NN)*MW_MIX_G(:),PASS)
               ENDIF
            END DO

           DO M = 1, MMAX
              WRITE(SUBM,*)M
              DO NN = 1,NMAX(M)
                 IF(VTK_X_s(VTK_REGION,M,NN).AND.ALLOCATED(X_s)) THEN
                    WRITE(SUBN,*)NN
                    IF(USE_RRATES) THEN
                       SPECIES_COUNTER = SPECIES_COUNTER + 1
                       VAR_NAME = ADJUSTL(SPECIES_NAME(SPECIES_COUNTER))
                       LT = LEN_TRIM(ADJUSTL(SPECIES_NAME(SPECIES_COUNTER)))
                    ELSE
                       VAR_NAME = ADJUSTL(SPECIES_ALIAS_s(M,NN))
                       LT = LEN_TRIM(ADJUSTL(SPECIES_ALIAS_s(M,NN)))
                    ENDIF
                    VAR_NAME = VAR_NAME(1:LT)//'_Solids_mass_fractions_'//TRIM(ADJUSTL(SUBM))//'_'//ADJUSTL(SUBN)
                    CALL WRITE_SCALAR_IN_VTU_BIN(VAR_NAME,X_s(:,M,NN),PASS)
                 ENDIF
              END DO
           END DO

           DO M = 1,MMAX
              IF(VTK_Theta_m(VTK_REGION,M).AND.ALLOCATED(Theta_m)) THEN
                 WRITE(SUBM,*)M
                 CALL WRITE_SCALAR_IN_VTU_BIN('Granular_temperature_'//ADJUSTL(SUBM),Theta_m(:,M),PASS)
              ENDIF
           END DO

           DO NN = 1,NSCALAR
              IF(VTK_Scalar(VTK_REGION,NN).AND.ALLOCATED(Scalar)) THEN
                 WRITE(SUBN,*)NN
                 VAR_NAME = 'Scalar_'//ADJUSTL(SUBN)
                 CALL WRITE_SCALAR_IN_VTU_BIN(VAR_NAME,Scalar(:,NN),PASS)
              ENDIF
           END DO

           DO R = 1,nRR
              IF(VTK_RRate(VTK_REGION,R).AND.ALLOCATED(ReactionRates)) THEN
                 WRITE(SUBR,*)R
                 VAR_NAME = 'RRates_'//ADJUSTL(SUBR)
                 IF(TRIM(VTK_RRate_label(VTK_REGION,R))/='') &
                    VAR_NAME = TRIM(VTK_RRate_label(VTK_REGION,R))//'_'//TRIM(VAR_NAME)
                 CALL WRITE_SCALAR_IN_VTU_BIN(VAR_NAME,ReactionRates(:, R),PASS)
              ENDIF
          END DO


          IF(SAVE_FLUID_RRATES.AND.ALLOCATED(fluid_rrates_out)) THEN
             DO R = 1,NO_OF_RXNS
                IF(VTK_FLUID_RRate(VTK_REGION,R)) THEN
                   VAR_NAME = 'RRates_'//TRIM(RXN_NAME(R))
                   CALL WRITE_SCALAR_IN_VTU_BIN(VAR_NAME,fluid_rrates_out(:, R),PASS)
                ENDIF
             END DO
          ENDIF

          IF(SAVE_DES_RRATES.AND.ALLOCATED(Des_rrates_out)) THEN
             DO R = 1,NO_OF_DES_RXNS
                IF(VTK_DES_RRate(VTK_REGION,R)) THEN
                   VAR_NAME = 'RRates_'//TRIM(DES_RXN_NAME(R))
                   CALL WRITE_SCALAR_IN_VTU_BIN(VAR_NAME,Des_rrates_out(:, R),PASS)
                ENDIF
             END DO
          ENDIF


          IF(K_EPSILON) THEN
             IF(VTK_K_Turb_G(VTK_REGION).AND.ALLOCATED(K_Turb_G)) &
                CALL WRITE_SCALAR_IN_VTU_BIN('K_Turb_G',K_Turb_G,PASS)
             IF(VTK_E_Turb_G(VTK_REGION).AND.ALLOCATED(E_Turb_G)) &
                CALL WRITE_SCALAR_IN_VTU_BIN('E_Turb_G',E_Turb_G,PASS)
          ENDIF


          IF(VTK_VORTICITY(VTK_REGION).OR.VTK_LAMBDA_2(VTK_REGION)) THEN
             CALL CALC_VORTICITY
          ENDIF

          IF(VTK_VORTICITY(VTK_REGION).AND.ALLOCATED(VORTICITY)) &
             CALL WRITE_SCALAR_IN_VTU_BIN('VORTICITY_MAG',VORTICITY,PASS)
          IF(VTK_LAMBDA_2(VTK_REGION).AND.ALLOCATED(LAMBDA2)) &
             CALL WRITE_SCALAR_IN_VTU_BIN('LAMBDA_2',LAMBDA2,PASS)


          IF(VTK_PARTITION(VTK_REGION)) THEN
             Allocate(DP_PARTITION(DIMENSION_3))
             DP_PARTITION = DBLE(MyPE)
             CALL WRITE_SCALAR_IN_VTU_BIN('PARTITION',DP_PARTITION,PASS)
             DeAllocate(DP_PARTITION)
          ENDIF


          IF(VTK_BC_ID(VTK_REGION).AND.ALLOCATED(BC_ID)) THEN
             Allocate(DP_BC_ID(DIMENSION_3))
             DP_BC_ID = DBLE(BC_ID)
             CALL WRITE_SCALAR_IN_VTU_BIN('BC_ID',DP_BC_ID,PASS)
             DeAllocate(DP_BC_ID)
          ENDIF


          IF(VTK_DWALL(VTK_REGION).AND.SUBGRID_WALL.AND.ALLOCATED(DWALL)) &
             CALL WRITE_SCALAR_IN_VTU_BIN('DISTANCE_TO_WALL',DWALL,PASS)

          IF(VTK_IJK(VTK_REGION)) THEN
            Allocate(IJK_ARRAY(DIMENSION_3))
            DO IJK = IJKSTART3, IJKEND3
               IJK_ARRAY(IJK) = DBLE(IJK)
            ENDDO
            CALL WRITE_SCALAR_IN_VTU_BIN('IJK',IJK_ARRAY,PASS)
            DeAllocate(IJK_ARRAY)
         ENDIF

          IF(VTK_NORMAL(VTK_REGION).AND.CARTESIAN_GRID.AND.ALLOCATED(NORMAL_S)) &
             CALL WRITE_VECTOR_IN_VTU_BIN('Scalar normal',NORMAL_S(:,1),NORMAL_S(:,2),NORMAL_S(:,3),PASS)

          DO NN=1,15
             IF(VTK_DEBUG(VTK_REGION,NN).AND.ALLOCATED(DEBUG_CG)) THEN
                WRITE(SUBN,*)NN
                VAR_NAME = 'DEBUG_'//ADJUSTL(SUBN)
                CALL WRITE_SCALAR_IN_VTU_BIN(VAR_NAME,DEBUG_CG(:,NN),PASS)
             ENDIF
          ENDDO

          IF(MODE==2) THEN! Mesh file
! Bypass re-indexng when writing the mesh file (temporary set re-indexing to false)
             RE_INDEXING_BCK = RE_INDEXING
             RE_INDEXING = .FALSE.
             CALL WRITE_SCALAR_IN_VTU_BIN('Volume',VOL,PASS)
             Allocate(REL_VOL(DIMENSION_3))
             DO IJK = IJKSTART3, IJKEND3
                REL_VOL(IJK) = VOL(IJK)/(DX(I_OF(IJK))*DY(J_OF(IJK))*DZ(K_OF(IJK)))
             ENDDO
             CALL WRITE_SCALAR_IN_VTU_BIN('Relative_Volume',REL_VOL,PASS)
             DeAllocate(REL_VOL)
             CALL WRITE_SCALAR_IN_VTU_BIN('Aspect_Ratio',Aspect_Ratio,PASS)
             IF(CARTESIAN_GRID) CALL WRITE_SCALAR_IN_VTU_BIN('Delta_h',DELH_Scalar,PASS)

! Bypass re-indexng when writing the mesh file (reset re-indexing value)
             RE_INDEXING = RE_INDEXING_BCK
          ENDIF

         ENDDO ! PASS LOOP, EITHER HEADER OR DATA

      ELSEIF(VTK_DATA(VTK_REGION)=='G') THEN
         CALL WRITE_STL_IN_VTU_BIN

      ENDIF


      CALL CLOSE_VTU_FILE_BIN(MODE)
      IF(MODE==0) CALL UPDATE_AND_CLOSE_PVD_FILE

#ifdef MPI
      call MPI_barrier(MPI_COMM_WORLD,mpierr)
#endif

! Update Frames
      IF (myPE == PE_IO.AND.TIME_DEPENDENT_FILENAME) THEN
         CALL WRITE_PVD_FRAMES
      ENDIF

      WRITE(ERR_MSG,20)' DONE.'
      CALL LOG_STATUS()

20    FORMAT(A,1X/)
      RETURN

      END SUBROUTINE WRITE_VTU_FILE

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: OPEN_VTU_FILE                                          C
!  Purpose: Open a vtu file and writes the header                      C
!           Binary format                                              C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE WRITE_VTU_VTR_PVTU_FILES(MODE)

      IMPLICIT NONE
      INTEGER :: MODE
      ! MODE = 0 : Write regular VTK region file
      ! MODE = 1 : Write debug   VTK region file (VTK_DBG_FILE = .TRUE.)

#ifdef MPI
      call MPI_barrier(MPI_COMM_WORLD, mpierr)
#endif

! Only open the file from head node when not using distributed I/O
      IF (myPE /= PE_IO .AND. (.NOT. BDIST_IO)) THEN
         RETURN
      ENDIF

      IF (TRIM(VTU_DIR) /= '.' .AND. TRIM(VTU_DIR) /= '' .AND. myPE == PE_IO) THEN
         CALL CREATE_DIR(trim(VTU_DIR))
      ENDIF

      CALL UPDATE_FRAMES

      CALL OPEN_VTU
      CALL WRITE_VTR_FILE
      CALL OPEN_PVTU

   CONTAINS

      SUBROUTINE OPEN_VTU
         INTEGER :: ISTAT

         IF(.NOT.BDIST_IO) THEN
            IF(NUMBER_OF_VTK_CELLS>0) VTU_FILENAME = trim(FIND_VTK_BASENAME(MODE)) // ".vtu"
         ELSE
            VTU_FILENAME = trim(FIND_VTK_BASENAME(MODE,MyPE)) // ".vtu"
         ENDIF


! Echo
         IF (FULL_LOG) THEN
            IF (.NOT. BDIST_IO) THEN
               WRITE (ERR_MSG, 10) ' WRITING VTU FILE : ', TRIM(VTU_FILENAME), ' .'
               CALL LOG_STATUS()
            ELSE
               WRITE (ERR_MSG, 20) ' WRITING PVTU FILE : ', trim(FIND_VTK_BASENAME(MODE)), '.pvtu (EACH PROCESSOR IS WRITING ITS OWN VTU FILE)'
               CALL LOG_STATUS()
            ENDIF
         ENDIF

         IF (NUMBER_OF_VTK_CELLS > 0) THEN

            OPEN (UNIT=UNIT_VTU, &
                  FILE=TRIM(VTU_FILENAME), &
                  FORM='UNFORMATTED', &
                  ACCESS='STREAM', &
                  ACTION='WRITE', CONVERT='BIG_ENDIAN', IOSTAT=ISTAT)

            IF (ISTAT /= 0) THEN
               IF (DMP_LOG) WRITE (UNIT_LOG, 1001) VTU_FILENAME, UNIT_VTU, VTU_DIR
               IF (FULL_LOG .AND. myPE == PE_IO) WRITE (*, 1001) VTU_FILENAME, UNIT_VTU, VTU_DIR
               call log_error()
            ENDIF

1001        FORMAT(/1X, 70('*')//, ' From: OPEN_VTU_FILE', /, ' Message: ', &
                    'Error opening vtu file. Terminating run.', /10X, &
                    'File name:  ', A, /10X, &
                    'DES_UNIT :  ', i4, /10X, &
                    'PLEASE VERIFY THAT VTU_DIR EXISTS: ', A, &
                    /1X, 70('*')/)

            ! Write file Header
            BUFFER = '<?xml version="1.0"?>'
            WRITE (UNIT_VTU) TRIM(BUFFER)//END_REC

            WRITE (BUFFER, 110) '<!-- Time =', TIME, ' sec. -->'
            WRITE (UNIT_VTU) TRIM(BUFFER)//END_REC

            BUFFER = '<VTKFile type="UnstructuredGrid" version="0.1" byte_order="BigEndian">'
            WRITE (UNIT_VTU) TRIM(BUFFER)//END_REC

            BUFFER = '  <UnstructuredGrid>'
            WRITE (UNIT_VTU) TRIM(BUFFER)//END_REC

         ENDIF

10       FORMAT(/1X, 3A)
15       FORMAT(/1X, A)
20       FORMAT(/1X, 3A)
110      FORMAT(A, E14.7, A)

      END SUBROUTINE OPEN_VTU


      SUBROUTINE WRITE_VTR_FILE
         DOUBLE PRECISION :: X1, X2, Y1, Y2, Z1, Z2
         INTEGER :: I, J, K, LAST_CHAR
         INTEGER, DIMENSION(0:NODESI - 1) :: iend_all_vtk
         INTEGER, DIMENSION(0:NODESJ - 1) :: jend_all_vtk
         INTEGER, DIMENSION(0:NODESK - 1) :: kend_all_vtk
         DOUBLE PRECISION, DIMENSION(0:NODESI) :: LIST_OF_X_LINES_DES
         DOUBLE PRECISION, DIMENSION(0:NODESJ) :: LIST_OF_Y_LINES_DES
         DOUBLE PRECISION, DIMENSION(0:NODESK) :: LIST_OF_Z_LINES_DES

         IF (VTK_DOMAIN_DECOMPOSITION(VTK_REGION) .AND. MyPE == PE_IO) THEN

            iend_all_vtk = iend3_all(0:NODESI - 1) - 2
            jend_all_vtk = jend3_all(0:NODESI*NODESJ - 1:NODESI) - 2
            kend_all_vtk = kend3_all(0:NODESI*NODESJ*NODESK - 1:NODESJ*NODESI) - 2

            LIST_OF_X_LINES_DES(0) = ZERO
            DO I = 1, NODESI
               LIST_OF_X_LINES_DES(I) = xe(iend_all_vtk(i - 1))
            ENDDO
            X1 = LIST_OF_X_LINES_DES(0)
            X2 = LIST_OF_X_LINES_DES(NODESI)

            LIST_OF_Y_LINES_DES(0) = ZERO
            DO J = 1, NODESJ
               LIST_OF_Y_LINES_DES(J) = yn(jend_all_vtk(j - 1))
            ENDDO
            Y1 = LIST_OF_Y_LINES_DES(0)
            Y2 = LIST_OF_Y_LINES_DES(NODESJ)

            LIST_OF_Z_LINES_DES(0) = ZERO
            DO K = 1, NODESK
               LIST_OF_Z_LINES_DES(K) = zt(kend_all_vtk(k - 1))
            ENDDO
            Z1 = LIST_OF_Z_LINES_DES(0)
            Z2 = LIST_OF_Z_LINES_DES(NODESK)

! Change extension to .vtr (rectilinear mesh)
            LAST_CHAR = LEN(TRIM(VTU_FILENAME))
            VTU_FILENAME(LAST_CHAR:LAST_CHAR) = 'r'

            OPEN (UNIT=UNIT_VTR, FILE=VTU_FILENAME, status='unknown')

            WRITE (UNIT_VTR, '(A)') '<?xml version="1.0"?>'
            WRITE (UNIT_VTR, '(A)') '<VTKFile type="RectilinearGrid" &
               &version="0.1" byte_order="LittleEndian">'

            WRITE (UNIT_VTR, '(A,6I6,A)') '<RectilinearGrid &
               &WholeExtent="', 0, NODESI, 0, NODESJ, 0, NODESK, '">'

            WRITE (UNIT_VTR, '(A,6I6,A)') '<Piece Extent="', 0, &
               NODESI, 0, NODESJ, 0, NODESK, '">'
            WRITE (UNIT_VTR, '(A)') '<Coordinates>'

            WRITE (UNIT_VTR, '(A,F14.8,A,F14.8,A)') '<DataArray &
               &type="Float32" format="ascii" &
               &RangeMin="', X1, '" RangeMax="', X2, '">'
            WRITE (UNIT_VTR, '(10F14.8)') LIST_OF_X_LINES_DES
            WRITE (UNIT_VTR, '(A)') '</DataArray>'

            WRITE (UNIT_VTR, '(A,F14.8,A,F14.8,A)') '<DataArray &
               &type="Float32" format="ascii" &
               &RangeMin="', Y1, '" RangeMax="', Y2, '">'
            WRITE (UNIT_VTR, '(10F14.8)') LIST_OF_Y_LINES_DES
            WRITE (UNIT_VTR, '(A)') '</DataArray>'

            WRITE (UNIT_VTR, '(A,F14.8,A,F14.8,A)') '<DataArray &
               &type="Float32" format="ascii" &
               &RangeMin="', Z1, '" RangeMax="', Z2, '">'
            WRITE (UNIT_VTR, '(10F14.8)') LIST_OF_Z_LINES_DES

            WRITE (UNIT_VTR, '(A)') '</DataArray>'
            WRITE (UNIT_VTR, '(A)') '</Coordinates>'
            WRITE (UNIT_VTR, '(A)') '</Piece>'
            WRITE (UNIT_VTR, '(A)') '</RectilinearGrid>'
            WRITE (UNIT_VTR, '(A)') '</VTKFile>'

            CLOSE (UNIT_VTR)

         ENDIF

      END SUBROUTINE WRITE_VTR_FILE


      SUBROUTINE OPEN_PVTU

! For distributed I/O, open .pvtu file that combines all *.vtu files for a given FRAME
! this is a simple ASCII file

         IF (myPE == PE_IO .AND. BDIST_IO) THEN

            PVTU_FILENAME = FIND_VTK_BASENAME(MODE) // ".pvtu"

            OPEN (UNIT=UNIT_PVTU, FILE=TRIM(PVTU_FILENAME), CONVERT='BIG_ENDIAN')

            WRITE (UNIT_PVTU, 105) '<?xml version="1.0"?>'
            WRITE (UNIT_PVTU, 110) '<!-- Time =', TIME, ' sec. -->'
            WRITE (UNIT_PVTU, 120) '<VTKFile type="PUnstructuredGrid"', &
               ' version="0.1" byte_order="BigEndian">'

            WRITE (UNIT_PVTU, *) '  <PUnstructuredGrid GhostLevel="0">'
            WRITE (UNIT_PVTU, *) '      <PPoints>'
            WRITE (UNIT_PVTU, *) '        <PDataArray type="Float32" Name="coordinates" NumberOfComponents="3" &
                 &format="appended" offset=" 0" />'
            WRITE (UNIT_PVTU, *) '      </PPoints>'
            WRITE (UNIT_PVTU, *) ''
            WRITE (UNIT_PVTU, *) '      <PCellData Scalars="scalars">'

         ENDIF

105      FORMAT(A)
110      FORMAT(A, E14.7, A)
120      FORMAT(A, A)

         RETURN
      END SUBROUTINE OPEN_PVTU

   END SUBROUTINE WRITE_VTU_VTR_PVTU_FILES

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_STL_IN_VTU_BIN                                   C
!  Purpose: Write Geometry and connectivity in a vtu file              C
!           Binary format                                              C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE WRITE_STL_IN_VTU_BIN

      IMPLICIT NONE

      INTEGER :: IJK,L, NN, VV
      INTEGER :: OFFSET

      INTEGER :: CELL_TYPE

      REAL(c_float) :: float
      INTEGER(c_int) :: int

      INTEGER ::     nbytes_xyz,nbytes_connectivity,nbytes_offset,nbytes_type
      INTEGER ::     offset_xyz,offset_connectivity,offset_offset,offset_type

      INTEGER :: GEO, NG


! First a series of tags is written for the geometry (PASS=WRITE_HEADER)
!  - Coordinates
!  - Connectivity
!  - Connectivity offset
!  - cell types
!

! Since the data is appended (i.e., written after all tags), the
! offset, in number of bytes must be specified.  The offset includes
! the size of the data for each field, plus the size of the integer
! that stores the number of bytes.  this is why the offset of a field
! equals the offset of the previous field plus c_sizeof(int) plus the
! number of bytes of the field.

! Next, the actual data is written for the geometry (PASS=WRITE_DATA)
! The DATA is converted to single precision to save memory.

      IF (myPE == PE_IO) THEN
! Get number of cells and points
            
         NUMBER_OF_VTK_CELLS = 0
         DO NG=1,N_STL_GROUP
            STL_GROUP(NG)%CONVERT_TO_VTU = .FALSE.
            CALL MAKE_UPPER_CASE (STL_GROUP(NG)%FILENAME,20)
            DO GEO = 1, VTK_GEO_MAX
               IF(TRIM(VTK_GEO(VTK_REGION,GEO))=='') CYCLE
               IF(TRIM(VTK_GEO(VTK_REGION,GEO))==TRIM(STL_GROUP(NG)%FILENAME)) THEN
                  VTK_GEO_GROUP = NG
                  NUMBER_OF_VTK_CELLS = NUMBER_OF_VTK_CELLS + STL_GROUP(NG)%END - STL_GROUP(NG)%START + 1
                  STL_GROUP(NG)%CONVERT_TO_VTU = .TRUE.
                  EXIT
               ENDIF
            ENDDO
         ENDDO

         NUMBER_OF_POINTS = NUMBER_OF_VTK_CELLS * 3

! Number of bytes of each field
         nbytes_xyz          = NUMBER_OF_POINTS * 3 * c_sizeof(float)
         nbytes_connectivity = NUMBER_OF_VTK_CELLS * 3 * c_sizeof(int)
         nbytes_offset       = NUMBER_OF_VTK_CELLS * c_sizeof(int)
         nbytes_type         = NUMBER_OF_VTK_CELLS * c_sizeof(int)


! Offset of each field
         offset_xyz = 0
         offset_connectivity = offset_xyz          + c_sizeof(int) + nbytes_xyz
         offset_offset       = offset_connectivity + c_sizeof(int) + nbytes_connectivity
         offset_type         = offset_offset       + c_sizeof(int) + nbytes_offset


! Header
            WRITE(BUFFER,100)'    <Piece NumberOfPoints="',NUMBER_OF_POINTS,'" NumberOfCells="',NUMBER_OF_VTK_CELLS,'" >'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,110)'      <Points>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,100)'        <DataArray type="Float32" Name="coordinates" NumberOfComponents="3" &
                 &format="appended" offset="',offset_xyz,'" />'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,110)'      </Points>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,110)'      <Cells>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,100)'        <DataArray type="Int32" Name="connectivity" format="appended" offset="', &
                 offset_connectivity,'" />'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,100)'        <DataArray type="Int32" Name="offsets" format="appended" offset="',offset_offset,'" />'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,100)'        <DataArray type="Int32" Name="types" format="appended" offset="',offset_type,'" />'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,110)'      </Cells>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            VTU_offset =  offset_type       + c_sizeof(int) + nbytes_type  ! Store offset for first variable to be written

            WRITE(BUFFER,110)'      <CellData>'                          ! Preparing CellData
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC



! Data

            WRITE(BUFFER,110)'      </CellData>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,110)'    </Piece>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC


            WRITE(BUFFER,110)'  </UnstructuredGrid>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC


            WRITE(BUFFER,110)'  <AppendedData encoding="raw">'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC


! Starting raw binary data with an underscore

            WRITE(BUFFER,110)'_'
            WRITE(UNIT_VTU)TRIM(BUFFER)

! X,Y,Z coordinates
            WRITE(UNIT_VTU) nbytes_xyz

            DO NG=1, N_STL_GROUP
               IF(STL_GROUP(NG)%CONVERT_TO_VTU) THEN
                  DO NN = STL_GROUP(NG)%START,STL_GROUP(NG)%END
                     WRITE(UNIT_VTU) (REAL(VERTEX(VV,1:3,NN)), VV = 1,3)
                  ENDDO
               ENDIF
            ENDDO


! Connectivity
            WRITE(UNIT_VTU) nbytes_connectivity

            DO NN = 1, NUMBER_OF_VTK_CELLS
               WRITE(UNIT_VTU) 3*(NN - 1), 3*(NN - 1) + 1, 3*(NN - 1) + 2 
            END DO

! Offsets
            WRITE(UNIT_VTU) nbytes_offset

            OFFSET = 0
            DO NN = 1, NUMBER_OF_VTK_CELLS
               OFFSET = OFFSET + 3
               WRITE(UNIT_VTU) OFFSET
            END DO

! Types
            WRITE(UNIT_VTU)nbytes_type

            IF(NO_K) THEN
               CELL_TYPE = 7
            ELSE
               CELL_TYPE = 41
            ENDIF

            DO NN = 1, NUMBER_OF_VTK_CELLS
               WRITE(UNIT_VTU) 7
            END DO



      ENDIF


100   FORMAT(A,I12,A,I12,A)
110   FORMAT(A)

      RETURN

      END SUBROUTINE WRITE_STL_IN_VTU_BIN
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_GEOMETRY_IN_VTU_BIN                              C
!  Purpose: Write Geometry and connectivity in a vtu file              C
!           Binary format                                              C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE WRITE_GEOMETRY_IN_VTU_BIN(PASS)

      IMPLICIT NONE

      INTEGER :: IJK,L
      INTEGER :: OFFSET

      INTEGER :: CELL_TYPE

      REAL(c_float) :: float
      INTEGER(c_int) :: int

      INTEGER ::     nbytes_xyz,nbytes_connectivity,nbytes_offset,nbytes_type
      INTEGER ::     offset_xyz,offset_connectivity,offset_offset,offset_type

      INTEGER :: PASS
      INTEGER :: WRITE_HEADER = 1
      INTEGER :: WRITE_DATA   = 2


! First a series of tags is written for the geometry (PASS=WRITE_HEADER)
!  - Coordinates
!  - Connectivity
!  - Connectivity offset
!  - cell types
!

! Since the data is appended (i.e., written after all tags), the
! offset, in number of bytes must be specified.  The offset includes
! the size of the data for each field, plus the size of the integer
! that stores the number of bytes.  this is why the offset of a field
! equals the offset of the previous field plus c_sizeof(int) plus the
! number of bytes of the field.

! Next, the actual data is written for the geometry (PASS=WRITE_DATA)
! The DATA is converted to single precision to save memory.

      IF (myPE == PE_IO.AND.(.NOT.BDIST_IO)) THEN
! The number of points and number of VTK cells is now computed in
! SETUP_VTK_REGION

! Number of bytes of each field
         nbytes_xyz          = NUMBER_OF_POINTS * 3 * c_sizeof(float)

         nbytes_connectivity = 0
         DO IJK = 1,IJKMAX3
            IF (BELONGS_TO_VTK_SUBDOMAIN(IJK)) THEN
                  nbytes_connectivity = nbytes_connectivity + GLOBAL_NUMBER_OF_NODES(IJK)
            ENDIF
         END DO
         nbytes_connectivity = nbytes_connectivity * c_sizeof(int)


         nbytes_offset       = NUMBER_OF_VTK_CELLS * c_sizeof(int)

         nbytes_type         = NUMBER_OF_VTK_CELLS * c_sizeof(int)


! Offset of each field
         offset_xyz = 0
         offset_connectivity = offset_xyz          + c_sizeof(int) + nbytes_xyz
         offset_offset       = offset_connectivity + c_sizeof(int) + nbytes_connectivity
         offset_type         = offset_offset       + c_sizeof(int) + nbytes_offset


         IF(PASS==WRITE_HEADER) THEN

            WRITE(BUFFER,100)'    <Piece NumberOfPoints="',NUMBER_OF_POINTS,'" NumberOfCells="',NUMBER_OF_VTK_CELLS,'" >'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,110)'      <Points>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,100)'        <DataArray type="Float32" Name="coordinates" NumberOfComponents="3" &
                 &format="appended" offset="',offset_xyz,'" />'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,110)'      </Points>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,110)'      <Cells>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,100)'        <DataArray type="Int32" Name="connectivity" format="appended" offset="', &
                 offset_connectivity,'" />'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,100)'        <DataArray type="Int32" Name="offsets" format="appended" offset="',offset_offset,'" />'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,100)'        <DataArray type="Int32" Name="types" format="appended" offset="',offset_type,'" />'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,110)'      </Cells>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            VTU_offset =  offset_type       + c_sizeof(int) + nbytes_type  ! Store offset for first variable to be written

            WRITE(BUFFER,110)'      <CellData>'                          ! Preparing CellData
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC




         ELSEIF(PASS==WRITE_DATA) THEN

            WRITE(BUFFER,110)'      </CellData>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,110)'    </Piece>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC


            WRITE(BUFFER,110)'  </UnstructuredGrid>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC


            WRITE(BUFFER,110)'  <AppendedData encoding="raw">'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC


! Starting raw binary data with an underscore

            WRITE(BUFFER,110)'_'
            WRITE(UNIT_VTU)TRIM(BUFFER)

! X,Y,Z coordinates
            WRITE(UNIT_VTU) nbytes_xyz, (GLOBAL_COORDS_OF_POINTS(1:3,L), L = 1,NUMBER_OF_POINTS)

! Connectivity
            WRITE(UNIT_VTU) nbytes_connectivity

            DO IJK = 1,IJKMAX3
               IF (BELONGS_TO_VTK_SUBDOMAIN(IJK)) THEN
                  WRITE(UNIT_VTU) (GLOBAL_CLEANED_CONNECTIVITY(IJK,L)-1,L=1,GLOBAL_NUMBER_OF_NODES(IJK))
               ENDIF
            END DO

! Offsets
            WRITE(UNIT_VTU) nbytes_offset

            OFFSET = 0
            DO IJK = 1,IJKMAX3
               IF (BELONGS_TO_VTK_SUBDOMAIN(IJK)) THEN
                  OFFSET = OFFSET + GLOBAL_NUMBER_OF_NODES(IJK)
                  WRITE(UNIT_VTU) OFFSET
               ENDIF
            END DO

! Types
            WRITE(UNIT_VTU)nbytes_type

            IF(NO_K) THEN
               CELL_TYPE = 7
            ELSE
               CELL_TYPE = 41
            ENDIF

            DO IJK = 1,IJKMAX3
               IF (BELONGS_TO_VTK_SUBDOMAIN(IJK))  WRITE(UNIT_VTU) CELL_TYPE
            END DO


         ENDIF


      ELSEIF(BDIST_IO) THEN

! For distributed I/O, it works the same as above, except, the data is local to each processor
! First compute local number of cells and points

! The number of points and number of VTK cells is now computed in
! SETUP_VTK_REGION

! Number of bytes of each field
         nbytes_xyz          = NUMBER_OF_POINTS * 3 * c_sizeof(float)

         nbytes_connectivity = 0
         DO IJK = 1,IJKEND3
            IF (BELONGS_TO_VTK_SUBDOMAIN(IJK)) THEN
                  nbytes_connectivity = nbytes_connectivity + NUMBER_OF_NODES(IJK)
            ENDIF
         END DO
         nbytes_connectivity = nbytes_connectivity * c_sizeof(int)


         nbytes_offset       = NUMBER_OF_VTK_CELLS * c_sizeof(int)

         nbytes_type         = NUMBER_OF_VTK_CELLS * c_sizeof(int)


! Offset of each field
         offset_xyz = 0
         offset_connectivity = offset_xyz          + c_sizeof(int) + nbytes_xyz
         offset_offset       = offset_connectivity + c_sizeof(int) + nbytes_connectivity
         offset_type         = offset_offset       + c_sizeof(int) + nbytes_offset


         IF(PASS==WRITE_HEADER) THEN

            WRITE(BUFFER,100)'    <Piece NumberOfPoints="',NUMBER_OF_POINTS,'" NumberOfCells="',NUMBER_OF_VTK_CELLS,'" >'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,110)'      <Points>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,100)'        <DataArray type="Float32" Name="coordinates" NumberOfComponents="3" &
                 &format="appended" offset="',offset_xyz,'" />'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,110)'      </Points>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,110)'      <Cells>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,100)'        <DataArray type="Int32" Name="connectivity" format="appended" offset="', &
                 offset_connectivity,'" />'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,100)'        <DataArray type="Int32" Name="offsets" format="appended" offset="',offset_offset,'" />'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,100)'        <DataArray type="Int32" Name="types" format="appended" offset="',offset_type,'" />'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,110)'      </Cells>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            VTU_offset =  offset_type       + c_sizeof(int) + nbytes_type  ! Store offset for first variable to be written

            WRITE(BUFFER,110)'      <CellData>'                          ! Preparing CellData
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC




         ELSEIF(PASS==WRITE_DATA) THEN

            WRITE(BUFFER,110)'      </CellData>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,110)'    </Piece>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC


            WRITE(BUFFER,110)'  </UnstructuredGrid>'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC


            WRITE(BUFFER,110)'  <AppendedData encoding="raw">'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC


! Starting raw binary data with an underscore

            WRITE(BUFFER,110)'_'
            WRITE(UNIT_VTU)TRIM(BUFFER)

! X,Y,Z coordinates
            WRITE(UNIT_VTU) nbytes_xyz, (COORDS_OF_POINTS(L,1:3), L = 1,NUMBER_OF_POINTS)

! Connectivity
            WRITE(UNIT_VTU) nbytes_connectivity

            DO IJK = 1,IJKEND3
               IF (BELONGS_TO_VTK_SUBDOMAIN(IJK)) THEN
                  WRITE(UNIT_VTU) (CLEANED_CONNECTIVITY(IJK,L)-1,L=1,NUMBER_OF_NODES(IJK))
               ENDIF
            END DO

! Offsets
            WRITE(UNIT_VTU) nbytes_offset

            OFFSET = 0
            DO IJK = 1,IJKEND3
               IF (BELONGS_TO_VTK_SUBDOMAIN(IJK)) THEN
                  OFFSET = OFFSET + NUMBER_OF_NODES(IJK)
                  WRITE(UNIT_VTU) OFFSET
               ENDIF
            END DO

! Types
            WRITE(UNIT_VTU)nbytes_type

            IF(NO_K) THEN
               CELL_TYPE = 7
            ELSE
               CELL_TYPE = 41
            ENDIF

            DO IJK = 1,IJKEND3
               IF (BELONGS_TO_VTK_SUBDOMAIN(IJK))  WRITE(UNIT_VTU) CELL_TYPE
            END DO


         ENDIF


      ENDIF


100   FORMAT(A,I12,A,I12,A)
110   FORMAT(A)

      RETURN

      END SUBROUTINE WRITE_GEOMETRY_IN_VTU_BIN


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_SCALAR_IN_VTU_BIN                                C
!  Purpose: Write Scalar variable in a vtu file                        C
!           Binary format                                              C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE WRITE_SCALAR_IN_VTU_BIN(VAR_NAME,VAR,PASS)

      IMPLICIT NONE
      INTEGER :: I,IJK

      CHARACTER (*) :: VAR_NAME
      DOUBLE PRECISION, DIMENSION(DIMENSION_3) ::  VAR
      DOUBLE PRECISION, ALLOCATABLE :: GLOBAL_VAR(:)
      DOUBLE PRECISION, DIMENSION(DIMENSION_3) ::  TMP_VAR

      REAL(c_float) :: float

      INTEGER :: nbytes_scalar

      INTEGER :: PASS
      INTEGER :: WRITE_HEADER = 1
      INTEGER :: WRITE_DATA   = 2

      IF (.NOT.BDIST_IO) THEN

! For each scalar, write a tag, with corresponding offset

         nbytes_scalar = NUMBER_OF_VTK_CELLS * c_sizeof(float)

         IF(PASS==WRITE_HEADER) THEN
!           For each scalar, write a tag, with corresponding offset

            DO I = 1,LEN_TRIM(VAR_NAME)
               IF(VAR_NAME(I:I) == ' ') VAR_NAME(I:I) = '_'
            ENDDO

            IF (myPE == PE_IO) THEN
               WRITE(BUFFER,90)'        <DataArray type="Float32" Name="', &
                    TRIM(VAR_NAME),'" format="appended" offset="',VTU_offset,'" />'
               WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC
            ENDIF

            VTU_offset = VTU_offset + c_sizeof(float) + nbytes_scalar


         ELSEIF(PASS==WRITE_DATA) THEN
!           and write the data, always preceded by its size in number of bytes

            IF (myPE == PE_IO) THEN
               allocate (GLOBAL_VAR(ijkmax3))
            ELSE
               allocate (GLOBAL_VAR(1))
            ENDIF

            IF(RE_INDEXING) THEN
               CALL UNSHIFT_DP_ARRAY(VAR,TMP_VAR)
               CALL gather (TMP_VAR,GLOBAL_VAR,root)
            ELSE
               CALL gather (VAR,GLOBAL_VAR,root)
            ENDIF

            IF (myPE /= PE_IO) RETURN


            WRITE(UNIT_VTU) nbytes_scalar

            DO IJK = 1,IJKMAX3
               IF (BELONGS_TO_VTK_SUBDOMAIN(IJK)) WRITE(UNIT_VTU) REAL(GLOBAL_VAR(IJK))
            ENDDO


            Deallocate (GLOBAL_VAR)

         ENDIF


      ELSE ! BDIST_IO=.TRUE.


         nbytes_scalar = NUMBER_OF_VTK_CELLS * c_sizeof(float)

         IF(PASS==WRITE_HEADER) THEN
!           For each scalar, write a tag, with corresponding offset

            DO I = 1,LEN_TRIM(VAR_NAME)
               IF(VAR_NAME(I:I) == ' ') VAR_NAME(I:I) = '_'
            ENDDO

            WRITE(BUFFER,90)'        <DataArray type="Float32" Name="', &
                 TRIM(VAR_NAME),'" format="appended" offset="',VTU_offset,'" />'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC


            VTU_offset = VTU_offset + c_sizeof(float) + nbytes_scalar

            IF (myPE == PE_IO) THEN       ! Update pvtu file with variable name
               WRITE(UNIT_PVTU,90) '        <DataArray type="Float32" Name="', &
                    TRIM(VAR_NAME),'" format="appended" offset="',VTU_offset,'" />'
            ENDIF

         ELSEIF(PASS==WRITE_DATA) THEN
!           and write the data, always preceded by its size in number of bytes

            WRITE(UNIT_VTU) nbytes_scalar

            DO IJK = 1,IJKEND3
               IF (BELONGS_TO_VTK_SUBDOMAIN(IJK)) WRITE(UNIT_VTU) REAL(VAR(IJK))
            ENDDO

         ENDIF




      ENDIF


      IF (PASS==WRITE_DATA.AND.FULL_LOG.AND.myPE == PE_IO) WRITE(*,10,ADVANCE='NO')'.'

10    FORMAT(A)
90    FORMAT(A,A,A,I12,A)

      RETURN

      END SUBROUTINE WRITE_SCALAR_IN_VTU_BIN

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_VECTOR_IN_VTU                                    C
!  Purpose: Write Vector variable in a vtu file                        C
!           Binary format                                              C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE WRITE_VECTOR_IN_VTU_BIN(VAR_NAME,VARX,VARY,VARZ,PASS)

      IMPLICIT NONE
      INTEGER :: IJK

      CHARACTER (*) :: VAR_NAME
      DOUBLE PRECISION, DIMENSION(DIMENSION_3) ::  VARX,VARY,VARZ
      DOUBLE PRECISION, ALLOCATABLE :: GLOBAL_VARX(:),GLOBAL_VARY(:),GLOBAL_VARZ(:)
      DOUBLE PRECISION, DIMENSION(DIMENSION_3) ::  TMP_VAR

      REAL(c_float) :: float

      INTEGER :: nbytes_vector

      INTEGER :: PASS
      INTEGER :: WRITE_HEADER = 1
      INTEGER :: WRITE_DATA   = 2

      IF (.NOT.BDIST_IO) THEN

         nbytes_vector = NUMBER_OF_VTK_CELLS * 3 * c_sizeof(float)

         IF(PASS==WRITE_HEADER) THEN
!           For each vector, write a tag, with corresponding offset

            IF (myPE == PE_IO) THEN
               WRITE(BUFFER,90)'        <DataArray type="Float32" Name="', &
                    TRIM(VAR_NAME),'"  NumberOfComponents="3" format="appended" offset="',VTU_offset,'" />'
               WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC
            ENDIF


            VTU_offset = VTU_offset + c_sizeof(float) + nbytes_vector


         ELSEIF(PASS==WRITE_DATA) THEN
!           and write the data, always preceded by its size in number of bytes

            IF (myPE == PE_IO) THEN
               allocate (GLOBAL_VARX(ijkmax3))
               allocate (GLOBAL_VARY(ijkmax3))
               allocate (GLOBAL_VARZ(ijkmax3))
            ELSE
               allocate (GLOBAL_VARX(1))
               allocate (GLOBAL_VARY(1))
               allocate (GLOBAL_VARZ(1))
            ENDIF

            IF(RE_INDEXING) THEN
               CALL UNSHIFT_DP_ARRAY(VARX,TMP_VAR)
               call gather (TMP_VAR,GLOBAL_VARX,root)

               CALL UNSHIFT_DP_ARRAY(VARY,TMP_VAR)
               call gather (TMP_VAR,GLOBAL_VARY,root)

               CALL UNSHIFT_DP_ARRAY(VARZ,TMP_VAR)
               call gather (TMP_VAR,GLOBAL_VARZ,root)

            ELSE
               call gather (VARX,GLOBAL_VARX,root)
               call gather (VARY,GLOBAL_VARY,root)
               call gather (VARZ,GLOBAL_VARZ,root)
            ENDIF


            IF (myPE /= PE_IO) RETURN


            WRITE(UNIT_VTU) nbytes_vector

            DO IJK = 1,IJKMAX3
               IF (BELONGS_TO_VTK_SUBDOMAIN(IJK)) THEN
                  WRITE(UNIT_VTU) REAL(GLOBAL_VARX(IJK)),REAL(GLOBAL_VARY(IJK)),REAL(GLOBAL_VARZ(IJK))
               ENDIF
            ENDDO


            Deallocate (GLOBAL_VARX)
            Deallocate (GLOBAL_VARY)
            Deallocate (GLOBAL_VARZ)

         ENDIF


      ELSE ! BDIST_IO=.TRUE.


         nbytes_vector = NUMBER_OF_VTK_CELLS * 3 * c_sizeof(float)

         IF(PASS==WRITE_HEADER) THEN
!           For each vector, write a tag, with corresponding offset


            WRITE(BUFFER,90)'        <DataArray type="Float32" Name="', &
                 TRIM(VAR_NAME),'"  NumberOfComponents="3" format="appended" offset="',VTU_offset,'" />'
            WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC


            VTU_offset = VTU_offset + c_sizeof(float) + nbytes_vector

            IF (myPE == PE_IO) THEN       ! Update pvtu file with variable name
               WRITE(UNIT_PVTU,90)'        <DataArray type="Float32" Name="', &
                    TRIM(VAR_NAME),'"  NumberOfComponents="3" format="appended" offset="',VTU_offset,'" />'
            ENDIF

         ELSEIF(PASS==WRITE_DATA) THEN
!           and write the data, always preceded by its size in number of bytes

            WRITE(UNIT_VTU) nbytes_vector

            DO IJK = 1,IJKEND3
               IF (BELONGS_TO_VTK_SUBDOMAIN(IJK)) THEN
                  WRITE(UNIT_VTU) REAL(VARX(IJK)),REAL(VARY(IJK)),REAL(VARZ(IJK))
               ENDIF
            ENDDO

         ENDIF



      ENDIF


      IF (PASS==WRITE_DATA.AND.FULL_LOG.AND.myPE == PE_IO) WRITE(*,10,ADVANCE='NO')'.'

10    FORMAT(A)
90    FORMAT(A,A,A,I12,A)

      RETURN

      END SUBROUTINE WRITE_VECTOR_IN_VTU_BIN

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: CLOSE_VTU_FILE_BIN                                     C
!  Purpose: Close a vtu file                                           C
!           Binary format                                              C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE CLOSE_VTU_FILE_BIN(MODE)

      IMPLICIT NONE

      INTEGER:: N
      CHARACTER (LEN=32)  :: VTU_NAME
      INTEGER, DIMENSION(0:numPEs-1) :: ALL_VTK_CELL_COUNT
      INTEGER :: IERR
      INTEGER :: MODE   ! MODE = 0 : Write regular VTK region file
                        ! MODE = 1 : Write debug   VTK region file (VTK_DBG_FILE = .TRUE.)

      IF (myPE /= PE_IO.AND.(.NOT.BDIST_IO)) RETURN

      IF(NUMBER_OF_VTK_CELLS>0) THEN

! Write last tags and close the vtu file
          WRITE(BUFFER,110)'  </AppendedData>'
          WRITE(UNIT_VTU)END_REC//TRIM(BUFFER)//END_REC

          WRITE(BUFFER,110)'</VTKFile>'
          WRITE(UNIT_VTU)TRIM(BUFFER)//END_REC

         CLOSE(UNIT_VTU)

      ENDIF

! Update pvtu file and close

      IF(BDIST_IO)  CALL allgather_1i (NUMBER_OF_VTK_CELLS,ALL_VTK_CELL_COUNT,IERR)

      IF (myPE == PE_IO.AND.BDIST_IO) THEN
         WRITE(UNIT_PVTU, *) '      </PCellData>'

         DO N = 0,NumPEs-1
            IF(ALL_VTK_CELL_COUNT(N)>0) THEN
! The pvtu and vtu files are in the same directory (either project dir or VTU_DIR)
! The VTU_DIR should not be prepended to the basename (see .FALSE. as last argument)
               VTU_NAME = FIND_VTK_BASENAME(MODE, N, .FALSE.) // ".vtu"
               WRITE(UNIT_PVTU,110) '      <Piece Source="',TRIM(VTU_NAME),'"/>'
            ENDIF
         ENDDO

         WRITE(UNIT_PVTU, *) '  </PUnstructuredGrid>'
         WRITE(UNIT_PVTU, *) '</VTKFile>'
         CLOSE(UNIT_PVTU)
      ENDIF

110   FORMAT(A,A,A)

      RETURN

      END SUBROUTINE CLOSE_VTU_FILE_BIN


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_CUT_SURFACE_VTK                                  C
!  Purpose: Writes the cut cell surface in VTK format                  C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE WRITE_CUT_SURFACE_VTK

      IMPLICIT NONE

      INTEGER :: I,J,K,L,IJK,IJK_GL,NODE, BCV
      INTEGER :: NB
      INTEGER, DIMENSION(6) :: IJK_NB
      INTEGER :: POINT_ID,POLY_COUNT,FACE_ID,Q_ID
      INTEGER :: N_CUT_FACE_NODES,N_FACE_NODES

      INTEGER NUMBER_OF_FACES
      INTEGER NUMBER_OF_SURFACE_POINTS

      DOUBLE PRECISION, DIMENSION(3,15) :: COORD_CUT_FACE_NODES
      DOUBLE PRECISION, DIMENSION(3)    :: NORMAL
      DOUBLE PRECISION, DIMENSION(6) :: LOC

      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  FACE_CONNECTIVITY
      INTEGER, DIMENSION(:), ALLOCATABLE   ::  NUMBER_OF_CUT_FACE_POINTS
      INTEGER, DIMENSION(:), ALLOCATABLE   ::  FACE_BC_ID
      INTEGER, DIMENSION(:), ALLOCATABLE   ::  SMALL_CUT_CELL

      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  X_FACE_POINT
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Y_FACE_POINT
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  Z_FACE_POINT

      DOUBLE PRECISION :: X_COPY,Y_COPY,Z_COPY,F_COPY

      LOGICAL :: CLIP_FLAG
      LOGICAL :: KEEP_POINT, KEEP_X, KEEP_Y, KEEP_Z

      CHARACTER (LEN=255) :: FILENAME

      LOGICAL :: CORNER_POINT
      INTEGER :: NODE_OF_CORNER, IERROR
      INTEGER :: OLD_SIZE, NEW_SIZE

      IF(myPE/=0) RETURN

      ALLOCATE(FACE_CONNECTIVITY(DIMENSION_MAX_CUT_CELL,6))
      ALLOCATE(NUMBER_OF_CUT_FACE_POINTS(DIMENSION_MAX_CUT_CELL))
      ALLOCATE(FACE_BC_ID(DIMENSION_MAX_CUT_CELL))
      ALLOCATE(SMALL_CUT_CELL(DIMENSION_MAX_CUT_CELL))

      ALLOCATE(X_FACE_POINT(DIMENSION_MAX_CUT_CELL))
      ALLOCATE(Y_FACE_POINT(DIMENSION_MAX_CUT_CELL))
      ALLOCATE(Z_FACE_POINT(DIMENSION_MAX_CUT_CELL))

!======================================================================
!  Set-up connectivity for each cell, i.e., regular cells and cut cells
!======================================================================

      POLY_COUNT = 0

      NUMBER_OF_SURFACE_POINTS = 0

      NUMBER_OF_FACES = 0

      DO IJK = 1,IJKMAX3

!         IF(GLOBAL_CUT_CELL_AT(IJK)) THEN
         IF(GLOBAL_CUT_CELL_AT(IJK).OR.GLOBAL_SMALL_CELL_AT(IJK)) THEN

!======================================================================
!  Filter the connectivity to identify nodes belonging to cut face
!======================================================================


            NUMBER_OF_FACES = NUMBER_OF_FACES + 1

            N_CUT_FACE_NODES = 0

            CALL GET_GLOBAL_CELL_NODE_COORDINATES(IJK,'SCALAR')

            DO L = 1, GLOBAL_NUMBER_OF_NODES(IJK)
               IF(GLOBAL_CONNECTIVITY(IJK,L)>IJKMAX3) THEN   ! One of the new point
                  X_COPY = GLOBAL_X_NEW_POINT(GLOBAL_CONNECTIVITY(IJK,L)-IJKMAX3)
                  Y_COPY = GLOBAL_Y_NEW_POINT(GLOBAL_CONNECTIVITY(IJK,L)-IJKMAX3)
                  Z_COPY = GLOBAL_Z_NEW_POINT(GLOBAL_CONNECTIVITY(IJK,L)-IJKMAX3)
                  CORNER_POINT = .FALSE.
               ELSE                                   ! An existing point
                  DO NODE = 1,8
                  CORNER_POINT = .TRUE.
                     IF(GLOBAL_CONNECTIVITY(IJK,L) == IJK_OF_NODE(NODE)) THEN
                        NODE_OF_CORNER = NODE
                        X_COPY = X_NODE(NODE)
                        Y_COPY = Y_NODE(NODE)
                        Z_COPY = Z_NODE(NODE)

                        IF (GLOBAL_SNAP(IJK_OF_NODE(NODE))) THEN ! One of the snapped corner point which now belongs to the cut face
                           N_CUT_FACE_NODES = N_CUT_FACE_NODES + 1
                           COORD_CUT_FACE_NODES(1,N_CUT_FACE_NODES) = X_COPY
                           COORD_CUT_FACE_NODES(2,N_CUT_FACE_NODES) = Y_COPY
                           COORD_CUT_FACE_NODES(3,N_CUT_FACE_NODES) = Z_COPY
                        ENDIF
                     ENDIF
                  END DO

               ENDIF

               IF(CORNER_POINT) THEN
                  Q_ID = 1

                  CALL EVAL_F('QUADRIC',X_COPY,Y_COPY,Z_COPY,Q_ID,F_COPY,CLIP_FLAG)

                  CALL EVAL_F('POLYGON',X_COPY,Y_COPY,Z_COPY,N_POLYGON,F_COPY,CLIP_FLAG)

                  CALL EVAL_F('USR_DEF',X_COPY,Y_COPY,Z_COPY,N_USR_DEF,F_COPY,CLIP_FLAG)

                  IF(USE_STL.OR.USE_MSH) F_COPY = GLOBAL_F_AT(IJK_OF_NODE(NODE_OF_CORNER))

!                  CALL EVAL_STL_FCT_AT('SCALAR',IJK,NODE_OF_CORNER,F_COPY,CLIP_FLAG,BCID2)
               ELSE
                  F_COPY = ZERO
               ENDIF

               IF (ABS(F_COPY) < TOL_F ) THEN ! belongs to cut face
                  N_CUT_FACE_NODES = N_CUT_FACE_NODES + 1
                  COORD_CUT_FACE_NODES(1,N_CUT_FACE_NODES) = X_COPY
                  COORD_CUT_FACE_NODES(2,N_CUT_FACE_NODES) = Y_COPY
                  COORD_CUT_FACE_NODES(3,N_CUT_FACE_NODES) = Z_COPY
               ENDIF

            END DO

            CALL REORDER_POLYGON(N_CUT_FACE_NODES,COORD_CUT_FACE_NODES,NORMAL,IERROR)

            OLD_SIZE = SIZE(NUMBER_OF_CUT_FACE_POINTS,1)
            IF(NUMBER_OF_FACES>OLD_SIZE) THEN
               NEW_SIZE = 2*OLD_SIZE
               CALL INTEGER_GROW(NUMBER_OF_CUT_FACE_POINTS,NEW_SIZE)
               CALL INTEGER_GROW(FACE_BC_ID,NEW_SIZE)
               CALL INTEGER_GROW(SMALL_CUT_CELL,NEW_SIZE)
               CALL INTEGER_GROW2_reverse(FACE_CONNECTIVITY,NEW_SIZE)
            ENDIF

            NUMBER_OF_CUT_FACE_POINTS(NUMBER_OF_FACES) = N_CUT_FACE_NODES
            FACE_BC_ID(NUMBER_OF_FACES) = GLOBAL_BC_ID(IJK)
            IF(GLOBAL_SMALL_CELL_AT(IJK)) THEN
               SMALL_CUT_CELL(NUMBER_OF_FACES) = 1
            ELSE
               SMALL_CUT_CELL(NUMBER_OF_FACES) = 0
            ENDIF
            POLY_COUNT = POLY_COUNT + N_CUT_FACE_NODES + 1

            DO NODE = 1,N_CUT_FACE_NODES
               NUMBER_OF_SURFACE_POINTS = NUMBER_OF_SURFACE_POINTS + 1

               OLD_SIZE = SIZE(X_FACE_POINT,1)
               IF(NUMBER_OF_SURFACE_POINTS>OLD_SIZE) THEN
                  NEW_SIZE = 2*OLD_SIZE
                  CALL REAL_GROW(X_FACE_POINT,NEW_SIZE)
                  CALL REAL_GROW(Y_FACE_POINT,NEW_SIZE)
                  CALL REAL_GROW(Z_FACE_POINT,NEW_SIZE)
               ENDIF

               X_FACE_POINT(NUMBER_OF_SURFACE_POINTS) = COORD_CUT_FACE_NODES(1,NODE)
               Y_FACE_POINT(NUMBER_OF_SURFACE_POINTS) = COORD_CUT_FACE_NODES(2,NODE)
               Z_FACE_POINT(NUMBER_OF_SURFACE_POINTS) = COORD_CUT_FACE_NODES(3,NODE)
               FACE_CONNECTIVITY(NUMBER_OF_FACES,NODE) = NUMBER_OF_SURFACE_POINTS
            ENDDO

         ENDIF

      END DO

!      print*,'After identifying cut faces, NUMBER_OF_FACES=',NUMBER_OF_FACES


      DO BCV = 1, DIMENSION_BC
         IF (BC_DEFINED(BCV)) THEN

            IF (.not.IS_CG(BC_TYPE_ENUM(BCV))) THEN
               LOC(1) = LOCATION(BC_I_W(BCV),X_MIN,DX) - HALF*DX(BC_I_W(BCV))
               LOC(2) = LOCATION(BC_I_E(BCV),X_MIN,DX) + HALF*DX(BC_I_E(BCV))
               LOC(3) = LOCATION(BC_J_S(BCV),Y_MIN,DY) - HALF*DY(BC_J_S(BCV))
               LOC(4) = LOCATION(BC_J_N(BCV),Y_MIN,DY) + HALF*DY(BC_J_N(BCV))
               LOC(5) = LOCATION(BC_K_B(BCV),Z_MIN,DZ) - HALF*DZ(BC_K_B(BCV))
               LOC(6) = LOCATION(BC_K_T(BCV),Z_MIN,DZ) + HALF*DZ(BC_K_T(BCV))

               DO K = BC_K_B(BCV), BC_K_T(BCV)
               DO J = BC_J_S(BCV), BC_J_N(BCV)
               DO I = BC_I_W(BCV), BC_I_E(BCV)

                  IJK_NB(1) = funijk_gl(I+1,J,K)
                  IJK_NB(2) = funijk_gl(I-1,J,K)
                  IJK_NB(3) = funijk_gl(I,J-1,K)
                  IJK_NB(4) = funijk_gl(I,J+1,K)
                  IJK_NB(5) = funijk_gl(I,J,K-1)
                  IJK_NB(6) = funijk_gl(I,J,K+1)

                  IJK_GL=-1

                  DO NB = 1,6

                  if(1<=IJK_NB(NB).AND.IJK_NB(NB)<=IJKMAX3) then


                     if(GLOBAL_FLAG(IJK_NB(NB))==1) then
                        IJK_GL = IJK_NB(NB)
                     else
                        cycle
                     endif

                  else
                     cycle
                  endif


!======================================================================
!  Filter the connectivity to identify nodes belonging to boundary face
!======================================================================

                  NUMBER_OF_FACES = NUMBER_OF_FACES + 1

                  N_FACE_NODES = 0

                  CALL GET_GLOBAL_CELL_NODE_COORDINATES(IJK_GL,'SCALAR')

                  DO L = 1, GLOBAL_NUMBER_OF_NODES(IJK_GL)
                     IF(GLOBAL_CONNECTIVITY(IJK_GL,L)>IJKMAX3) THEN   ! One of the new point
                        X_COPY = GLOBAL_X_NEW_POINT(GLOBAL_CONNECTIVITY(IJK_GL,L)-IJKMAX3)
                        Y_COPY = GLOBAL_Y_NEW_POINT(GLOBAL_CONNECTIVITY(IJK_GL,L)-IJKMAX3)
                        Z_COPY = GLOBAL_Z_NEW_POINT(GLOBAL_CONNECTIVITY(IJK_GL,L)-IJKMAX3)
                        CORNER_POINT = .FALSE.
                     ELSE                                   ! An existing point
                        DO NODE = 1,8
                           CORNER_POINT = .TRUE.
                           IF(GLOBAL_CONNECTIVITY(IJK_GL,L) == IJK_OF_NODE(NODE)) THEN
                              NODE_OF_CORNER = NODE
                              X_COPY = X_NODE(NODE)
                              Y_COPY = Y_NODE(NODE)
                              Z_COPY = Z_NODE(NODE)

      !                        IF (GLOBAL_SNAP(IJK_OF_NODE(NODE))) THEN ! One of the snapped corner point which now belongs to the cut face
      !                           N_CUT_FACE_NODES = N_CUT_FACE_NODES + 1
      !                           COORD_CUT_FACE_NODES(1,N_CUT_FACE_NODES) = X_COPY
      !                           COORD_CUT_FACE_NODES(2,N_CUT_FACE_NODES) = Y_COPY
      !                           COORD_CUT_FACE_NODES(3,N_CUT_FACE_NODES) = Z_COPY
      !                        ENDIF

                           ENDIF
                        END DO

                     ENDIF

                     KEEP_POINT = .FALSE.
                     KEEP_X = .FALSE.
                     KEEP_Y = .FALSE.
                     KEEP_Z = .FALSE.

                     IF(COMPARE(LOC(1),X_COPY).OR.COMPARE(LOC(2),X_COPY)) KEEP_X = .TRUE.
                     IF(LOC(1)<=X_COPY.AND.X_COPY<=LOC(2))                KEEP_X = .TRUE.

                     IF(COMPARE(LOC(3),Y_COPY).OR.COMPARE(LOC(4),Y_COPY)) KEEP_Y = .TRUE.
                     IF(LOC(3)<=Y_COPY.AND.Y_COPY<=LOC(4))                KEEP_Y = .TRUE.

                     IF(COMPARE(LOC(5),Z_COPY).OR.COMPARE(LOC(6),Z_COPY)) KEEP_Z = .TRUE.
                     IF(LOC(5)<=Z_COPY.AND.Z_COPY<=LOC(6))                KEEP_Z = .TRUE.

                     IF(KEEP_X.AND.KEEP_Y.AND.KEEP_Z) THEN

                        N_FACE_NODES = N_FACE_NODES + 1
                        COORD_CUT_FACE_NODES(1,N_FACE_NODES) = X_COPY
                        COORD_CUT_FACE_NODES(2,N_FACE_NODES) = Y_COPY
                        COORD_CUT_FACE_NODES(3,N_FACE_NODES) = Z_COPY

                     ENDIF


                  ENDDO


                  CALL REORDER_POLYGON(N_FACE_NODES,COORD_CUT_FACE_NODES,NORMAL,IERROR)

                  OLD_SIZE = SIZE(NUMBER_OF_CUT_FACE_POINTS,1)
                  IF(NUMBER_OF_FACES>OLD_SIZE) THEN
                     NEW_SIZE = 2*OLD_SIZE
                     CALL INTEGER_GROW(NUMBER_OF_CUT_FACE_POINTS,NEW_SIZE)
                     CALL INTEGER_GROW(FACE_BC_ID,NEW_SIZE)
                     CALL INTEGER_GROW(SMALL_CUT_CELL,NEW_SIZE)
                     CALL INTEGER_GROW2_reverse(FACE_CONNECTIVITY,NEW_SIZE)
                  ENDIF

                  NUMBER_OF_CUT_FACE_POINTS(NUMBER_OF_FACES) = N_FACE_NODES
                  FACE_BC_ID(NUMBER_OF_FACES) = BCV
                  SMALL_CUT_CELL(NUMBER_OF_FACES) = 0
                  POLY_COUNT = POLY_COUNT + N_FACE_NODES + 1

                  DO NODE = 1,N_FACE_NODES
                     NUMBER_OF_SURFACE_POINTS = NUMBER_OF_SURFACE_POINTS + 1

                     OLD_SIZE = SIZE(X_FACE_POINT,1)
                     IF(NUMBER_OF_SURFACE_POINTS>OLD_SIZE) THEN
                        NEW_SIZE = 2*OLD_SIZE
                        CALL REAL_GROW(X_FACE_POINT,NEW_SIZE)
                        CALL REAL_GROW(Y_FACE_POINT,NEW_SIZE)
                        CALL REAL_GROW(Z_FACE_POINT,NEW_SIZE)
                     ENDIF

                     X_FACE_POINT(NUMBER_OF_SURFACE_POINTS) = COORD_CUT_FACE_NODES(1,NODE)
                     Y_FACE_POINT(NUMBER_OF_SURFACE_POINTS) = COORD_CUT_FACE_NODES(2,NODE)
                     Z_FACE_POINT(NUMBER_OF_SURFACE_POINTS) = COORD_CUT_FACE_NODES(3,NODE)
                     FACE_CONNECTIVITY(NUMBER_OF_FACES,NODE) = NUMBER_OF_SURFACE_POINTS
                  ENDDO


                  ENDDO


               ENDDO
               ENDDO
               ENDDO

            ENDIF

         ENDIF
      ENDDO

!      print*,'After identifying regular boundary faces, NUMBER_OF_FACES=',NUMBER_OF_FACES,NUMBER_OF_SURFACE_POINTS



      FILENAME= TRIM(RUN_NAME) // '_boundary.vtk'
      FILENAME = TRIM(FILENAME)
      OPEN(UNIT = 123, FILE = FILENAME)
      WRITE(123,1001)'# vtk DataFile Version 2.0'
      WRITE(123,1001)'3D CUT-CELL SURFACE'
      WRITE(123,1001)'ASCII'

      IF(NO_K) THEN   ! 2D GEOMETRY
         WRITE(123,1001)'DATASET UNSTRUCTURED_GRID'
      ELSE            ! 3D GEOMETRY
         WRITE(123,1001)'DATASET POLYDATA'
      ENDIF

      WRITE(123,1010)'POINTS ',NUMBER_OF_SURFACE_POINTS,' float'

      DO POINT_ID = 1,NUMBER_OF_SURFACE_POINTS
         WRITE(123,1020) X_FACE_POINT(POINT_ID),Y_FACE_POINT(POINT_ID),Z_FACE_POINT(POINT_ID)
      ENDDO

      IF(NO_K) THEN   ! 2D GEOMETRY

         WRITE(123,1030)'CELLS ',NUMBER_OF_FACES,POLY_COUNT
         DO FACE_ID = 1 , NUMBER_OF_FACES
            WRITE(123,1040) NUMBER_OF_CUT_FACE_POINTS(FACE_ID),(FACE_CONNECTIVITY(FACE_ID,L)-1,&
            L=1,NUMBER_OF_CUT_FACE_POINTS(FACE_ID))
         ENDDO
         WRITE(123,1030)'CELL_TYPES ',NUMBER_OF_FACES
         DO FACE_ID = 1 , NUMBER_OF_FACES
            WRITE(123,1040) 3
         ENDDO

      ELSE            ! 3D GEOMETRY

         WRITE(123,1030)'POLYGONS ',NUMBER_OF_FACES,POLY_COUNT
         DO FACE_ID = 1 , NUMBER_OF_FACES
            WRITE(123,1040) NUMBER_OF_CUT_FACE_POINTS(FACE_ID),(FACE_CONNECTIVITY(FACE_ID,L)-1,&
            L=1,NUMBER_OF_CUT_FACE_POINTS(FACE_ID))
         ENDDO

      ENDIF

      WRITE(123,1050)'CELL_DATA ',NUMBER_OF_FACES
      WRITE(123,1060)'SCALARS bc_id int 1'
      WRITE(123,1060)'LOOKUP_TABLE default'
      DO FACE_ID = 1 , NUMBER_OF_FACES
         WRITE(123,1070) FACE_BC_ID(FACE_ID)
      ENDDO
      WRITE(123,1060)'SCALARS small_cell int 1'
      WRITE(123,1060)'LOOKUP_TABLE default'
      DO FACE_ID = 1 , NUMBER_OF_FACES
         WRITE(123,1070) SMALL_CUT_CELL(FACE_ID)
      ENDDO

1001  FORMAT(A)
1010  FORMAT(A,I8,A)
1020  FORMAT(3(E16.8,2X))
1030  FORMAT(A,2(I8,2X))
1040  FORMAT(20(I8,2X))
1050  FORMAT(A,I8)
1060  FORMAT(A)
1070  FORMAT(I8)
3030  FORMAT(1X,A,A)
      CLOSE (123)

      WRITE(ERR_MSG,3030)'WROTE BOUNDARY IN VTK FILE : ',FILENAME
      CALL LOG_STATUS()
      RETURN

      END SUBROUTINE WRITE_CUT_SURFACE_VTK


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: GATHER_DATA                                            C
!  Purpose: Gather data from all processes in preparation of           C
!           Writing VTK files                                          C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE GATHER_DATA

      IMPLICIT NONE

      INTEGER :: IJK,I,J,K,L
      INTEGER :: IJK_OFFSET

      INTEGER :: iproc,IERR
      INTEGER, DIMENSION(0:numPEs-1) :: disp,rcount
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: SHIFTED_CONNECTIVITY
      INTEGER :: ALLOC_SIZE

!======================================================================
!  parallel processing
!======================================================================

! Always needed (PPO or not)
      IF(ALLOCATED(GLOBAL_I_OF)) DEALLOCATE(GLOBAL_I_OF)
      IF(ALLOCATED(GLOBAL_J_OF)) DEALLOCATE(GLOBAL_J_OF)
      IF(ALLOCATED(GLOBAL_K_OF)) DEALLOCATE(GLOBAL_K_OF)

      IF(ALLOCATED(GLOBAL_INTERIOR_CELL_AT)) DEALLOCATE(GLOBAL_INTERIOR_CELL_AT)

      IF(ALLOCATED(GLOBAL_SNAP)) DEALLOCATE(GLOBAL_SNAP)
      IF(ALLOCATED(GLOBAL_F_AT)) DEALLOCATE(GLOBAL_F_AT)
      IF(ALLOCATED(GLOBAL_BC_ID)) DEALLOCATE(GLOBAL_BC_ID)

      IF (myPE == PE_IO) THEN
         allocate (GLOBAL_I_OF(ijkmax3))
         allocate (GLOBAL_J_OF(ijkmax3))
         allocate (GLOBAL_K_OF(ijkmax3))

         allocate (GLOBAL_INTERIOR_CELL_AT(ijkmax3))

         allocate (GLOBAL_SNAP(ijkmax3))
         allocate (GLOBAL_F_AT(ijkmax3))
         allocate (GLOBAL_BC_ID(ijkmax3))
         allocate (GLOBAL_FLAG(ijkmax3))

      ELSE
         allocate (GLOBAL_I_OF(1))
         allocate (GLOBAL_J_OF(1))
         allocate (GLOBAL_K_OF(1))

         allocate (GLOBAL_INTERIOR_CELL_AT(1))

         allocate (GLOBAL_SNAP(1))
         allocate (GLOBAL_F_AT(1))
         allocate (GLOBAL_BC_ID(1))
         allocate (GLOBAL_FLAG(1))
      ENDIF


      IF(GENERATE_MESH) THEN

         CALL allgather_1i (NUMBER_OF_NEW_POINTS,rcount,IERR)

         IF (myPE == 0) THEN
            IJK_OFFSET = 0
         ELSE
            IJK_OFFSET = 0
            DO iproc=0,myPE-1
               IJK_OFFSET = IJK_OFFSET + rcount(iproc)
            ENDDO
         ENDIF

         CALL allgather_1i (IJK_OFFSET,disp,IERR)

      
         IF(ALLOCATED(GLOBAL_CONNECTIVITY)) DEALLOCATE(GLOBAL_CONNECTIVITY)
         IF(ALLOCATED(GLOBAL_NUMBER_OF_NODES)) DEALLOCATE(GLOBAL_NUMBER_OF_NODES)

         IF(ALLOCATED(GLOBAL_BLOCKED_CELL_AT)) DEALLOCATE(GLOBAL_BLOCKED_CELL_AT)
         IF(ALLOCATED(GLOBAL_STANDARD_CELL_AT)) DEALLOCATE(GLOBAL_STANDARD_CELL_AT)
         IF(ALLOCATED(GLOBAL_CUT_CELL_AT)) DEALLOCATE(GLOBAL_CUT_CELL_AT)
         IF(ALLOCATED(GLOBAL_SMALL_CELL_AT)) DEALLOCATE(GLOBAL_SMALL_CELL_AT)

         IF(ALLOCATED(GLOBAL_X_NEW_POINT)) DEALLOCATE(GLOBAL_X_NEW_POINT)
         IF(ALLOCATED(GLOBAL_Y_NEW_POINT)) DEALLOCATE(GLOBAL_Y_NEW_POINT)
         IF(ALLOCATED(GLOBAL_Z_NEW_POINT)) DEALLOCATE(GLOBAL_Z_NEW_POINT)


         call global_sum(NUMBER_OF_NEW_POINTS, GLOBAL_NUMBER_OF_NEW_POINTS,  PE_IO, ierr )
         ALLOC_SIZE = GLOBAL_NUMBER_OF_NEW_POINTS
!      print*,'gather c', NUMBER_OF_POINTS,NUMBER_OF_NEW_POINTS,GLOBAL_NUMBER_OF_NEW_POINTS

         IF (myPE == PE_IO) THEN

            allocate (GLOBAL_CONNECTIVITY(ijkmax3,15))
            allocate (GLOBAL_NUMBER_OF_NODES(ijkmax3))

            allocate (GLOBAL_BLOCKED_CELL_AT(ijkmax3))
            allocate (GLOBAL_STANDARD_CELL_AT(ijkmax3))
            allocate (GLOBAL_CUT_CELL_AT(ijkmax3))
            allocate (GLOBAL_SMALL_CELL_AT(ijkmax3))

            allocate (GLOBAL_X_NEW_POINT(ALLOC_SIZE))
            allocate (GLOBAL_Y_NEW_POINT(ALLOC_SIZE))
            allocate (GLOBAL_Z_NEW_POINT(ALLOC_SIZE))

         ELSE

            allocate (GLOBAL_CONNECTIVITY(1,15))
            allocate (GLOBAL_NUMBER_OF_NODES(1))

            allocate (GLOBAL_BLOCKED_CELL_AT(1))
            allocate (GLOBAL_STANDARD_CELL_AT(1))
            allocate (GLOBAL_CUT_CELL_AT(1))
            allocate (GLOBAL_SMALL_CELL_AT(1))

            allocate (GLOBAL_X_NEW_POINT(1))
            allocate (GLOBAL_Y_NEW_POINT(1))
            allocate (GLOBAL_Z_NEW_POINT(1))

         ENDIF

         GLOBAL_VAR_ALLOCATED = .TRUE.


         IF(numPEs==1) THEN  ! Serial run
            GLOBAL_X_NEW_POINT(1:NUMBER_OF_NEW_POINTS) =  X_NEW_POINT(1:NUMBER_OF_NEW_POINTS)
            GLOBAL_Y_NEW_POINT(1:NUMBER_OF_NEW_POINTS) =  Y_NEW_POINT(1:NUMBER_OF_NEW_POINTS)
            IF(DO_K) THEN
               GLOBAL_Z_NEW_POINT(1:NUMBER_OF_NEW_POINTS) =  Z_NEW_POINT(1:NUMBER_OF_NEW_POINTS)
            ELSE
               GLOBAL_Z_NEW_POINT(1:NUMBER_OF_NEW_POINTS) = Z_MIN
            ENDIF
         ELSE !Parallel run
            call gatherv_1d( X_NEW_POINT, NUMBER_OF_NEW_POINTS, GLOBAL_X_NEW_POINT, rcount, disp, PE_IO, ierr )
            call gatherv_1d( Y_NEW_POINT, NUMBER_OF_NEW_POINTS, GLOBAL_Y_NEW_POINT, rcount, disp, PE_IO, ierr )
            call gatherv_1d( Z_NEW_POINT, NUMBER_OF_NEW_POINTS, GLOBAL_Z_NEW_POINT, rcount, disp, PE_IO, ierr )
         ENDIF

! Setup global connectivity
         Allocate(  SHIFTED_CONNECTIVITY  (DIMENSION_3,15) )

         SHIFTED_CONNECTIVITY = CONNECTIVITY

         WHERE (SHIFTED_CONNECTIVITY > IJKEND3)
            SHIFTED_CONNECTIVITY = SHIFTED_CONNECTIVITY - IJKEND3 + IJKMAX3 + disp(myPE)
         END WHERE

         DO IJK = IJKSTART3,IJKEND3
            DO L=1,NUMBER_OF_NODES(IJK)
               IF(CONNECTIVITY(IJK,L) <= IJKEND3) THEN
                  I = I_OF(CONNECTIVITY(IJK,L))
                  J = J_OF(CONNECTIVITY(IJK,L))
                  K = K_OF(CONNECTIVITY(IJK,L))
                  SHIFTED_CONNECTIVITY(IJK,L) = funijk_gl(I,J,K)
               ENDIF
            ENDDO
         ENDDO

         call gather (SHIFTED_CONNECTIVITY,GLOBAL_CONNECTIVITY,root)

         call gather (NUMBER_OF_NODES,GLOBAL_NUMBER_OF_NODES,root)

         Deallocate(  SHIFTED_CONNECTIVITY )

         GLOBAL_INTERIOR_CELL_AT = .FALSE.
         GLOBAL_BLOCKED_CELL_AT  = .FALSE.
         GLOBAL_CUT_CELL_AT      = .FALSE.
         GLOBAL_SMALL_CELL_AT    = .FALSE.

         call gather (BLOCKED_CELL_AT,GLOBAL_BLOCKED_CELL_AT,root)
         call gather (STANDARD_CELL_AT,GLOBAL_STANDARD_CELL_AT,root)
         call gather (CUT_CELL_AT,GLOBAL_CUT_CELL_AT,root)
         call gather (SMALL_CELL_AT,GLOBAL_SMALL_CELL_AT,root)

      ELSE


      ENDIF



      call gather (I_OF,GLOBAL_I_OF,root)
      call gather (J_OF,GLOBAL_J_OF,root)
      call gather (K_OF,GLOBAL_K_OF,root)

      call gather (INTERIOR_CELL_AT,GLOBAL_INTERIOR_CELL_AT,root)

      IF(CARTESIAN_GRID) THEN
         call gather (SNAP,GLOBAL_SNAP,root)
         call gather (F_AT,GLOBAL_F_AT,root)
         call gather (BC_ID,GLOBAL_BC_ID,root)
      ENDIF
      call gather (FLAG,GLOBAL_FLAG,root)


      IF (myPE == PE_IO) THEN

         POLY_COUNTER = 0

         NUMBER_OF_CELLS = 0

         NUMBER_OF_CUT_CELLS = 0

         NUMBER_OF_BLOCKED_CELLS = 0

         NUMBER_OF_STANDARD_CELLS = 0

         DO IJK = 1, IJKMAX3

            IF (GLOBAL_INTERIOR_CELL_AT(IJK)) THEN

               NUMBER_OF_CELLS = NUMBER_OF_CELLS + 1

               IF (GLOBAL_BLOCKED_CELL_AT(IJK))  NUMBER_OF_BLOCKED_CELLS  = NUMBER_OF_BLOCKED_CELLS + 1
               IF (GLOBAL_STANDARD_CELL_AT(IJK)) NUMBER_OF_STANDARD_CELLS = NUMBER_OF_STANDARD_CELLS + 1
               IF (GLOBAL_CUT_CELL_AT(IJK))      NUMBER_OF_CUT_CELLS = NUMBER_OF_CUT_CELLS + 1

               IF (.NOT.GLOBAL_BLOCKED_CELL_AT(IJK))   POLY_COUNTER = POLY_COUNTER + GLOBAL_NUMBER_OF_NODES(IJK) + 1

            ENDIF

         END DO


         NUMBER_OF_POINTS = IJKMAX3 + GLOBAL_NUMBER_OF_NEW_POINTS
!      print*,'gather d', NUMBER_OF_POINTS,NUMBER_OF_NEW_POINTS,GLOBAL_NUMBER_OF_NEW_POINTS

!         print*, 'after gather_data,NUMBER_OF_POINTS, IJKMAX3, GLOBAL_NUMBER_OF_NEW_POINTS=',NUMBER_OF_POINTS, IJKMAX3,  GLOBAL_NUMBER_OF_NEW_POINTS

      ENDIF

      RETURN


      END SUBROUTINE GATHER_DATA


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: SETUP_VTK_NO_CUTCELL                                   C
!  Purpose: Setup VTK data for the regular grid (no cut cells)         C
!           This is called when CARTESIAN_GRID is .FALSE.              C
!           and WRITE>VTK_FILES is .TRUE.              .               C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 14-Jan-15  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE SETUP_VTK_NO_CUTCELL

      IMPLICIT NONE

      INTEGER :: IJK,I,J,K,L,NODE

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: SHIFTED_CONNECTIVITY

! Calculate cell volumes and face areas
      IF(.NOT.CARTESIAN_GRID) CALL SET_GEOMETRY1

! Only a few arrays need to be allocated here simce we do not need
! all Cartesian-grid arrays
      IF(.NOT.ALLOCATED(XG_E)) Allocate( XG_E(0:DIMENSION_I) )
      IF(.NOT.ALLOCATED(YG_N)) Allocate( YG_N(0:DIMENSION_J) )
      IF(.NOT.ALLOCATED(ZG_T)) Allocate( ZG_T(0:DIMENSION_K) )

      IF(.NOT.ALLOCATED(INTERIOR_CELL_AT)) THEN
         Allocate(  INTERIOR_CELL_AT  (DIMENSION_3) )
         INTERIOR_CELL_AT = .FALSE.
      ENDIF

      IF(.NOT.ALLOCATED(BLOCKED_CELL_AT)) THEN
         Allocate(  BLOCKED_CELL_AT  (DIMENSION_3) )
         BLOCKED_CELL_AT = .FALSE.
      ENDIF

      IF(.NOT.ALLOCATED(STANDARD_CELL_AT)) THEN
         Allocate(  STANDARD_CELL_AT  (DIMENSION_3) )
         STANDARD_CELL_AT = .TRUE.
      ENDIF

      IF(.NOT.ALLOCATED(CUT_CELL_AT)) THEN
         Allocate(  CUT_CELL_AT  (DIMENSION_3) )
         CUT_CELL_AT = .FALSE.
      ENDIF

      IF(.NOT.ALLOCATED(NUMBER_OF_NODES))  Allocate(  NUMBER_OF_NODES  (DIMENSION_3) )
      NUMBER_OF_NODES= 0

      IF(.NOT.ALLOCATED(CONNECTIVITY))     Allocate(  CONNECTIVITY  (DIMENSION_3,15) )


! This is a shoter version of Get_cut_cell_Flags
      DO IJK = IJKSTART3, IJKEND3

!======================================================================
!  Get coordinates of eight nodes
!======================================================================

         CALL GET_CELL_NODE_COORDINATES(IJK,'SCALAR')

         I = I_OF(IJK)
         J = J_OF(IJK)
         K = K_OF(IJK)

         IF(NO_K) THEN   ! 2D case

            INTERIOR_CELL_AT(IJK) = (     (I >= ISTART1 ).AND.(I <= IEND1 )  &
                                     .AND.(J >= JSTART1 ).AND.(J <= JEND1 ) )

         ELSE            ! 3D case

            INTERIOR_CELL_AT(IJK) = (     (I >= ISTART1 ).AND.(I <= IEND1 )  &
                                     .AND.(J >= JSTART1 ).AND.(J <= JEND1 )  &
                                     .AND.(K >= KSTART1 ).AND.(K <= KEND1 ) )

         ENDIF


         IF(INTERIOR_CELL_AT(IJK)) THEN

! Set up the connectivity: 4 nodes in 2D, 8 nodes in 3D
            IF(NO_K) THEN
               NUMBER_OF_NODES(IJK) = 4
               CONNECTIVITY(IJK,1) = IJK_OF_NODE(5)
               CONNECTIVITY(IJK,2) = IJK_OF_NODE(6)
               CONNECTIVITY(IJK,3) = IJK_OF_NODE(8)
               CONNECTIVITY(IJK,4) = IJK_OF_NODE(7)
            ELSE
               NUMBER_OF_NODES(IJK) = 8
               DO NODE = 1,8
                  CONNECTIVITY(IJK,NODE) = IJK_OF_NODE(NODE)
               END DO
            ENDIF

! If obstacles are defined, they will be flagged as blocked cells
! and will not be visible in the VTK files
            IF(WALL_AT(IJK)) THEN
               BLOCKED_CELL_AT(IJK)  = .TRUE.
               CUT_CELL_AT(IJK)      = .FALSE.
               STANDARD_CELL_AT(IJK) = .FALSE.
            ENDIF

         ENDIF

      ENDDO


!======================================================================
!  parallel processing
!======================================================================
      call SEND_RECEIVE_1D_LOGICAL(STANDARD_CELL_AT,2)
      call SEND_RECEIVE_1D_LOGICAL(BLOCKED_CELL_AT,2)
      call SEND_RECEIVE_1D_LOGICAL(CUT_CELL_AT,2)

      Allocate(  SHIFTED_CONNECTIVITY  (DIMENSION_3,15) )

      SHIFTED_CONNECTIVITY = CONNECTIVITY

! Replace local node index by global node index before gathering the array
      DO IJK = IJKSTART3,IJKEND3
         DO L=1,NUMBER_OF_NODES(IJK)
            IF(CONNECTIVITY(IJK,L) <= IJKEND3) THEN
               I = I_OF(CONNECTIVITY(IJK,L))
               J = J_OF(CONNECTIVITY(IJK,L))
               K = K_OF(CONNECTIVITY(IJK,L))
               SHIFTED_CONNECTIVITY(IJK,L) = funijk_gl(I,J,K)
            ENDIF
         ENDDO
      ENDDO

! Allocate, initialize and gather arrays
      IF(.TRUE.) THEN
!      IF(.NOT.GLOBAL_VAR_ALLOCATED) THEN

         IF (myPE == PE_IO) THEN
            if(.not.allocated(GLOBAL_I_OF)) allocate (GLOBAL_I_OF(ijkmax3))
            if(.not.allocated(GLOBAL_J_OF)) allocate (GLOBAL_J_OF(ijkmax3))
            if(.not.allocated(GLOBAL_K_OF)) allocate (GLOBAL_K_OF(ijkmax3))
            if(.not.allocated(GLOBAL_CONNECTIVITY)) allocate (GLOBAL_CONNECTIVITY(ijkmax3,15))
            if(.not.allocated(GLOBAL_NUMBER_OF_NODES)) allocate (GLOBAL_NUMBER_OF_NODES(ijkmax3))
            if(.not.allocated(GLOBAL_INTERIOR_CELL_AT)) allocate (GLOBAL_INTERIOR_CELL_AT(ijkmax3))
            if(.not.allocated(GLOBAL_BLOCKED_CELL_AT)) allocate (GLOBAL_BLOCKED_CELL_AT(ijkmax3))
            if(.not.allocated(GLOBAL_STANDARD_CELL_AT)) allocate (GLOBAL_STANDARD_CELL_AT(ijkmax3))
            if(.not.allocated(GLOBAL_CUT_CELL_AT)) allocate (GLOBAL_CUT_CELL_AT(ijkmax3))
            if(.not.allocated(GLOBAL_SMALL_CELL_AT)) allocate (GLOBAL_SMALL_CELL_AT(ijkmax3))

         ELSE
            if(.not.allocated(GLOBAL_I_OF)) allocate (GLOBAL_I_OF(1))
            if(.not.allocated(GLOBAL_J_OF)) allocate (GLOBAL_J_OF(1))
            if(.not.allocated(GLOBAL_K_OF)) allocate (GLOBAL_K_OF(1))
            if(.not.allocated(GLOBAL_CONNECTIVITY)) allocate (GLOBAL_CONNECTIVITY(1,15))
            if(.not.allocated(GLOBAL_NUMBER_OF_NODES)) allocate (GLOBAL_NUMBER_OF_NODES(1))
            if(.not.allocated(GLOBAL_INTERIOR_CELL_AT)) allocate (GLOBAL_INTERIOR_CELL_AT(1))
            if(.not.allocated(GLOBAL_BLOCKED_CELL_AT)) allocate (GLOBAL_BLOCKED_CELL_AT(1))
            if(.not.allocated(GLOBAL_STANDARD_CELL_AT)) allocate (GLOBAL_STANDARD_CELL_AT(1))
            if(.not.allocated(GLOBAL_CUT_CELL_AT)) allocate (GLOBAL_CUT_CELL_AT(1))
            if(.not.allocated(GLOBAL_SMALL_CELL_AT)) allocate (GLOBAL_SMALL_CELL_AT(1))
         ENDIF

         GLOBAL_VAR_ALLOCATED = .TRUE.

      ENDIF


      GLOBAL_INTERIOR_CELL_AT = .FALSE.
      GLOBAL_BLOCKED_CELL_AT  = .FALSE.
      GLOBAL_CUT_CELL_AT      = .FALSE.
      GLOBAL_SMALL_CELL_AT    = .FALSE.
      GLOBAL_STANDARD_CELL_AT = .TRUE.

      call gather (I_OF,GLOBAL_I_OF,root)
      call gather (J_OF,GLOBAL_J_OF,root)
      call gather (K_OF,GLOBAL_K_OF,root)
      call gather (SHIFTED_CONNECTIVITY,GLOBAL_CONNECTIVITY,root)
      call gather (NUMBER_OF_NODES,GLOBAL_NUMBER_OF_NODES,root)
      call gather (INTERIOR_CELL_AT,GLOBAL_INTERIOR_CELL_AT,root)
      call gather (BLOCKED_CELL_AT,GLOBAL_BLOCKED_CELL_AT,root)
      call gather (STANDARD_CELL_AT,GLOBAL_STANDARD_CELL_AT,root)
      call gather (CUT_CELL_AT,GLOBAL_CUT_CELL_AT,root)
      call gather (SMALL_CELL_AT,GLOBAL_SMALL_CELL_AT,root)

      deAllocate(  SHIFTED_CONNECTIVITY   )



! Count the number of cells
      GLOBAL_NUMBER_OF_NEW_POINTS = 0

      IF (myPE == PE_IO) THEN

         POLY_COUNTER = 0

         NUMBER_OF_CELLS = 0

         NUMBER_OF_CUT_CELLS = 0

         NUMBER_OF_BLOCKED_CELLS = 0

         NUMBER_OF_STANDARD_CELLS = 0

         DO IJK = 1, IJKMAX3

            IF (GLOBAL_INTERIOR_CELL_AT(IJK)) THEN

               NUMBER_OF_CELLS = NUMBER_OF_CELLS + 1

               IF (GLOBAL_BLOCKED_CELL_AT(IJK))  NUMBER_OF_BLOCKED_CELLS  = NUMBER_OF_BLOCKED_CELLS + 1
               IF (GLOBAL_STANDARD_CELL_AT(IJK)) NUMBER_OF_STANDARD_CELLS = NUMBER_OF_STANDARD_CELLS + 1
               IF (GLOBAL_CUT_CELL_AT(IJK))      NUMBER_OF_CUT_CELLS = NUMBER_OF_CUT_CELLS + 1

               IF (.NOT.GLOBAL_BLOCKED_CELL_AT(IJK))   POLY_COUNTER = POLY_COUNTER + GLOBAL_NUMBER_OF_NODES(IJK) + 1

            ENDIF

         END DO

! There are no new points since there a no cut cells
         NUMBER_OF_POINTS = IJKMAX3

      ENDIF

      RETURN


      END SUBROUTINE SETUP_VTK_NO_CUTCELL



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: SAVE_MESH_STATISTICS                                   C
!  Purpose: SAVE_MESH STATISTICS in MESH_STATS.LOG                     C
!                                                                      C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 14-Jan-20  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE SAVE_MESH_STATISTICS

      IMPLICIT NONE

      INTEGER :: IJK

      INTEGER :: IERR

      DOUBLE PRECISION :: MIN_VOL, MAX_VOL, GLOBAL_MIN_VOL,GLOBAL_MAX_VOL
      DOUBLE PRECISION :: MIN_AYZ, MAX_AYZ, GLOBAL_MIN_AYZ,GLOBAL_MAX_AYZ
      DOUBLE PRECISION :: MIN_AXZ, MAX_AXZ, GLOBAL_MIN_AXZ,GLOBAL_MAX_AXZ
      DOUBLE PRECISION :: MIN_AXY, MAX_AXY, GLOBAL_MIN_AXY,GLOBAL_MAX_AXY
      DOUBLE PRECISION :: MIN_CUT, MAX_CUT, GLOBAL_MIN_CUT,GLOBAL_MAX_CUT
      DOUBLE PRECISION :: LOCAL_MIN_Q,LOCAL_MAX_Q, GLOBAL_MIN_Q,GLOBAL_MAX_Q

      DOUBLE PRECISION :: lDBL_NOC

      if(NUMBER_OF_CELLS == 0) then
         lDBL_NOC = 1.0d-15
      else
         lDBL_NOC = DBLE(NUMBER_OF_CELLS)
      endif



      IF(.NOT.GRID_INFO_PRINTED_ON_SCREEN) THEN
         WRITE(ERR_MSG,5) 'MESH STATISTICS:'
         CALL LOG_STATUS()
         WRITE(ERR_MSG,5) 'NUMBER OF CELLS          = ', NUMBER_OF_CELLS
         CALL LOG_STATUS()
         WRITE(ERR_MSG,10)'NUMBER OF STANDARD CELLS = ', &
              NUMBER_OF_STANDARD_CELLS,DBLE(NUMBER_OF_STANDARD_CELLS) / lDBL_NOC * 100.0D0
         CALL LOG_STATUS()
         WRITE(ERR_MSG,10)'NUMBER OF CUT CELLS      = ', &
            NUMBER_OF_CUT_CELLS,DBLE(NUMBER_OF_CUT_CELLS) / lDBL_NOC * 100.0D0
         CALL LOG_STATUS()
         WRITE(ERR_MSG,10)'NUMBER OF FLUID CELLS    = ', NUMBER_OF_STANDARD_CELLS + NUMBER_OF_CUT_CELLS,&
           DBLE(NUMBER_OF_STANDARD_CELLS + NUMBER_OF_CUT_CELLS) / lDBL_NOC * 100.0D0
         CALL LOG_STATUS()
         WRITE(ERR_MSG,10)'NUMBER OF BLOCKED CELLS  = ', &
            NUMBER_OF_BLOCKED_CELLS,DBLE(NUMBER_OF_BLOCKED_CELLS) / lDBL_NOC * 100.0D0
         CALL LOG_STATUS()
         WRITE(ERR_MSG,12)
         CALL LOG_STATUS()

5        FORMAT(76('='),/1X,A,I8)
10       FORMAT(1X,A,I8,' (',F6.2,' % of Total)')
12       FORMAT(76('='))

         GRID_INFO_PRINTED_ON_SCREEN = .TRUE.
      ENDIF

      IF(MyPE == PE_IO)  THEN
         OPEN(UNIT = UNIT_MSH_STATS, FILE = 'MESH_STATS.LOG')

! Repeat in MESH_STATS.LOG
         WRITE(UNIT_MSH_STATS,15)'MESH STATISTICS:'
         WRITE(UNIT_MSH_STATS,15)'NUMBER OF CELLS                = ', NUMBER_OF_CELLS
         WRITE(UNIT_MSH_STATS,20)'NUMBER OF STANDARD CELLS       = ', &
               NUMBER_OF_STANDARD_CELLS,DBLE(NUMBER_OF_STANDARD_CELLS) / lDBL_NOC * 100.0D0
         WRITE(UNIT_MSH_STATS,20)'NUMBER OF CUT CELLS            = ', &
               NUMBER_OF_CUT_CELLS,DBLE(NUMBER_OF_CUT_CELLS) / lDBL_NOC * 100.0D0
         WRITE(UNIT_MSH_STATS,20)'NUMBER OF FLUID CELLS          = ', NUMBER_OF_STANDARD_CELLS + NUMBER_OF_CUT_CELLS, &
               DBLE(NUMBER_OF_STANDARD_CELLS + NUMBER_OF_CUT_CELLS) / lDBL_NOC * 100.0D0
         WRITE(UNIT_MSH_STATS,20)'NUMBER OF BLOCKED CELLS        = ', &
               NUMBER_OF_BLOCKED_CELLS,DBLE(NUMBER_OF_BLOCKED_CELLS) / lDBL_NOC * 100.0D0
         WRITE(UNIT_MSH_STATS,22)

         IF(NUMBER_OF_STANDARD_CELLS==0) THEN
            WRITE(ERR_MSG,30)'FATAL ERROR: There are no standard fluid cells in the computation.', &
                             'Please verify geometry and mesh settings.', &
                             'If using an STL file to define the geometry, this may be due to', & 
                             'normal vectors pointing in the wrong direction.'
            CALL LOG_ERROR()
         ENDIF
      ENDIF

      CALL BCAST(NUMBER_OF_CUT_CELLS)

15    FORMAT(76('='),/A,I8)
20    FORMAT(A,I8,' (',F6.2,' % of Total)')
22    FORMAT(76('='))
30    FORMAT(A,/,A,/,A,/,A)         

!======================================================================
!  Scalar Cell volumes and areas
!======================================================================

      MIN_VOL =   LARGE_NUMBER
      MAX_VOL = - LARGE_NUMBER
      MIN_AYZ =   LARGE_NUMBER
      MAX_AYZ = - LARGE_NUMBER
      MIN_AXZ =   LARGE_NUMBER
      MAX_AXZ = - LARGE_NUMBER
      MIN_AXY =   LARGE_NUMBER
      MAX_AXY = - LARGE_NUMBER

      DO IJK = IJKSTART3, IJKEND3
         IF(STANDARD_CELL_AT(IJK)) THEN              ! STANDARD CELLS

            MIN_VOL =   DMIN1(MIN_VOL,VOL(IJK))
            MAX_VOL =   DMAX1(MAX_VOL,VOL(IJK))
            MIN_AYZ =   DMIN1(MIN_AYZ,AYZ(IJK))
            MAX_AYZ =   DMAX1(MAX_AYZ,AYZ(IJK))
            MIN_AXZ =   DMIN1(MIN_AXZ,AXZ(IJK))
            MAX_AXZ =   DMAX1(MAX_AXZ,AXZ(IJK))
            MIN_AXY =   DMIN1(MIN_AXY,AXY(IJK))
            MAX_AXY =   DMAX1(MAX_AXY,AXY(IJK))

         ENDIF
      END DO

      call global_min(MIN_VOL, GLOBAL_MIN_VOL,  PE_IO, ierr )
      call global_max(MAX_VOL, GLOBAL_MAX_VOL,  PE_IO, ierr )
      call global_min(MIN_AYZ, GLOBAL_MIN_AYZ,  PE_IO, ierr )
      call global_max(MAX_AYZ, GLOBAL_MAX_AYZ,  PE_IO, ierr )
      call global_min(MIN_AXZ, GLOBAL_MIN_AXZ,  PE_IO, ierr )
      call global_max(MAX_AXZ, GLOBAL_MAX_AXZ,  PE_IO, ierr )
      call global_min(MIN_AXY, GLOBAL_MIN_AXY,  PE_IO, ierr )
      call global_max(MAX_AXY, GLOBAL_MAX_AXY,  PE_IO, ierr )

      IF (myPE == PE_IO) THEN
!         WRITE(UNIT_MSH_STATS,1000)  '============================================================================'
!         WRITE(UNIT_MSH_STATS,1000)  ' MESH STATISTICS                                                            '
!         WRITE(UNIT_MSH_STATS,1000)  '============================================================================'
         WRITE(UNIT_MSH_STATS,1000)  'SCALAR STANDARD CELLS:'
         WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXY                   = ', GLOBAL_MIN_AXY,GLOBAL_MAX_AXY
         WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXZ                   = ', GLOBAL_MIN_AXZ,GLOBAL_MAX_AXZ
         WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AYZ                   = ', GLOBAL_MIN_AYZ,GLOBAL_MAX_AYZ
         WRITE(UNIT_MSH_STATS,1000)  'RANGE OF VOLUME                = ', GLOBAL_MIN_VOL,GLOBAL_MAX_VOL
      ENDIF

      IF(NUMBER_OF_CUT_CELLS>0) THEN

         MIN_VOL =   LARGE_NUMBER
         MAX_VOL = - LARGE_NUMBER
         MIN_AYZ =   LARGE_NUMBER
         MAX_AYZ = - LARGE_NUMBER
         MIN_AXZ =   LARGE_NUMBER
         MAX_AXZ = - LARGE_NUMBER
         MIN_AXY =   LARGE_NUMBER
         MAX_AXY = - LARGE_NUMBER

         DO IJK = IJKSTART3, IJKEND3
            IF(CUT_CELL_AT(IJK)) THEN                   ! CUT CELLS

               MIN_VOL =   DMIN1(MIN_VOL,VOL(IJK))
               MAX_VOL =   DMAX1(MAX_VOL,VOL(IJK))
               MIN_AYZ =   DMIN1(MIN_AYZ,AYZ(IJK))
               MAX_AYZ =   DMAX1(MAX_AYZ,AYZ(IJK))
               MIN_AXZ =   DMIN1(MIN_AXZ,AXZ(IJK))
               MAX_AXZ =   DMAX1(MAX_AXZ,AXZ(IJK))
               MIN_AXY =   DMIN1(MIN_AXY,AXY(IJK))
               MAX_AXY =   DMAX1(MAX_AXY,AXY(IJK))

            ENDIF
         END DO

! Need to count number of wall_{u,v,w} cells
         NUMBER_OF_U_WALL_CELLS = 0
         NUMBER_OF_V_WALL_CELLS = 0
         NUMBER_OF_W_WALL_CELLS = 0
         DO IJK = IJKSTART3, IJKEND3
            IF(INTERIOR_CELL_AT(IJK)) THEN
               IF(WALL_U_AT(IJK).AND..NOT.BLOCKED_CELL_AT(IJK)) NUMBER_OF_U_WALL_CELLS = NUMBER_OF_U_WALL_CELLS +1
               IF(WALL_V_AT(IJK).AND..NOT.BLOCKED_CELL_AT(IJK)) NUMBER_OF_V_WALL_CELLS = NUMBER_OF_V_WALL_CELLS +1
               IF(WALL_w_AT(IJK).AND..NOT.BLOCKED_CELL_AT(IJK)) NUMBER_OF_w_WALL_CELLS = NUMBER_OF_W_WALL_CELLS +1
            ENDIF
         END DO

         call global_all_sum(NUMBER_OF_U_WALL_CELLS)
         call global_all_sum(NUMBER_OF_V_WALL_CELLS)
         call global_all_sum(NUMBER_OF_W_WALL_CELLS)

         call global_min(MIN_VOL, GLOBAL_MIN_VOL,  PE_IO, ierr )
         call global_max(MAX_VOL, GLOBAL_MAX_VOL,  PE_IO, ierr )
         call global_min(MIN_AYZ, GLOBAL_MIN_AYZ,  PE_IO, ierr )
         call global_max(MAX_AYZ, GLOBAL_MAX_AYZ,  PE_IO, ierr )
         call global_min(MIN_AXZ, GLOBAL_MIN_AXZ,  PE_IO, ierr )
         call global_max(MAX_AXZ, GLOBAL_MAX_AXZ,  PE_IO, ierr )
         call global_min(MIN_AXY, GLOBAL_MIN_AXY,  PE_IO, ierr )
         call global_max(MAX_AXY, GLOBAL_MAX_AXY,  PE_IO, ierr )

         IF (myPE == PE_IO) THEN
            WRITE(UNIT_MSH_STATS,1000)  'SCALAR CUT CELLS:'
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXY                   = ', GLOBAL_MIN_AXY,GLOBAL_MAX_AXY
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXZ                   = ', GLOBAL_MIN_AXZ,GLOBAL_MAX_AXZ
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AYZ                   = ', GLOBAL_MIN_AYZ,GLOBAL_MAX_AYZ
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF VOLUME                = ', GLOBAL_MIN_VOL,GLOBAL_MAX_VOL
            WRITE(UNIT_MSH_STATS,1010)  'NUMBER OF SMALL SCALAR CELLS   = ', NUMBER_OF_SMALL_CELLS
         ENDIF

      ENDIF
      IF (myPE == PE_IO) THEN
         WRITE(UNIT_MSH_STATS,1000)  '============================================================================'
      ENDIF

1000 FORMAT(A,E14.7,2X,E14.7)
1010 FORMAT(A,I8)


      IF(CARTESIAN_GRID) THEN

!======================================================================
!  U-Momentum Cell volumes and areas
!======================================================================

      MIN_VOL =   LARGE_NUMBER
      MAX_VOL = - LARGE_NUMBER
      MIN_AYZ =   LARGE_NUMBER
      MAX_AYZ = - LARGE_NUMBER
      MIN_AXZ =   LARGE_NUMBER
      MAX_AXZ = - LARGE_NUMBER
      MIN_AXY =   LARGE_NUMBER
      MAX_AXY = - LARGE_NUMBER

      DO IJK = IJKSTART3, IJKEND3
         IF(STANDARD_U_CELL_AT(IJK)) THEN              ! STANDARD CELLS

            MIN_VOL =   DMIN1(MIN_VOL,VOL_U(IJK))
            MAX_VOL =   DMAX1(MAX_VOL,VOL_U(IJK))
            MIN_AYZ =   DMIN1(MIN_AYZ,AYZ_U(IJK))
            MAX_AYZ =   DMAX1(MAX_AYZ,AYZ_U(IJK))
            MIN_AXZ =   DMIN1(MIN_AXZ,AXZ_U(IJK))
            MAX_AXZ =   DMAX1(MAX_AXZ,AXZ_U(IJK))
            MIN_AXY =   DMIN1(MIN_AXY,AXY_U(IJK))
            MAX_AXY =   DMAX1(MAX_AXY,AXY_U(IJK))

         ENDIF
      END DO

      call global_min(MIN_VOL, GLOBAL_MIN_VOL,  PE_IO, ierr )
      call global_max(MAX_VOL, GLOBAL_MAX_VOL,  PE_IO, ierr )
      call global_min(MIN_AYZ, GLOBAL_MIN_AYZ,  PE_IO, ierr )
      call global_max(MAX_AYZ, GLOBAL_MAX_AYZ,  PE_IO, ierr )
      call global_min(MIN_AXZ, GLOBAL_MIN_AXZ,  PE_IO, ierr )
      call global_max(MAX_AXZ, GLOBAL_MAX_AXZ,  PE_IO, ierr )
      call global_min(MIN_AXY, GLOBAL_MIN_AXY,  PE_IO, ierr )
      call global_max(MAX_AXY, GLOBAL_MAX_AXY,  PE_IO, ierr )

      IF (myPE == PE_IO) THEN
         WRITE(UNIT_MSH_STATS,1000)  'U-MOMENTUM STANDARD CELLS:'
         WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXY                   = ', GLOBAL_MIN_AXY,GLOBAL_MAX_AXY
         WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXZ                   = ', GLOBAL_MIN_AXZ,GLOBAL_MAX_AXZ
         WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AYZ                   = ', GLOBAL_MIN_AYZ,GLOBAL_MAX_AYZ
         WRITE(UNIT_MSH_STATS,1000)  'RANGE OF VOLUME                = ', GLOBAL_MIN_VOL,GLOBAL_MAX_VOL
      ENDIF

      IF(NUMBER_OF_CUT_CELLS>0) THEN
         MIN_VOL =   LARGE_NUMBER
         MAX_VOL = - LARGE_NUMBER
         MIN_AYZ =   LARGE_NUMBER
         MAX_AYZ = - LARGE_NUMBER
         MIN_AXZ =   LARGE_NUMBER
         MAX_AXZ = - LARGE_NUMBER
         MIN_AXY =   LARGE_NUMBER
         MAX_AXY = - LARGE_NUMBER
         MIN_CUT =   LARGE_NUMBER
         MAX_CUT = - LARGE_NUMBER

         DO IJK = IJKSTART3, IJKEND3
            IF(CUT_U_CELL_AT(IJK).AND.(.NOT.WALL_U_AT(IJK))) THEN      ! CUT CELLS

               MIN_VOL =   DMIN1(MIN_VOL,VOL_U(IJK))
               MAX_VOL =   DMAX1(MAX_VOL,VOL_U(IJK))
               MIN_AYZ =   DMIN1(MIN_AYZ,AYZ_U(IJK))
               MAX_AYZ =   DMAX1(MAX_AYZ,AYZ_U(IJK))
               MIN_AXZ =   DMIN1(MIN_AXZ,AXZ_U(IJK))
               MAX_AXZ =   DMAX1(MAX_AXZ,AXZ_U(IJK))
               MIN_AXY =   DMIN1(MIN_AXY,AXY_U(IJK))
               MAX_AXY =   DMAX1(MAX_AXY,AXY_U(IJK))
               MIN_CUT =   DMIN1(MIN_CUT,AREA_U_CUT(IJK))
               MAX_CUT =   DMAX1(MAX_CUT,AREA_U_CUT(IJK))

            ENDIF
         END DO

         call global_min(MIN_VOL, GLOBAL_MIN_VOL,  PE_IO, ierr )
         call global_max(MAX_VOL, GLOBAL_MAX_VOL,  PE_IO, ierr )
         call global_min(MIN_AYZ, GLOBAL_MIN_AYZ,  PE_IO, ierr )
         call global_max(MAX_AYZ, GLOBAL_MAX_AYZ,  PE_IO, ierr )
         call global_min(MIN_AXZ, GLOBAL_MIN_AXZ,  PE_IO, ierr )
         call global_max(MAX_AXZ, GLOBAL_MAX_AXZ,  PE_IO, ierr )
         call global_min(MIN_AXY, GLOBAL_MIN_AXY,  PE_IO, ierr )
         call global_max(MAX_AXY, GLOBAL_MAX_AXY,  PE_IO, ierr )
         call global_min(MIN_CUT, GLOBAL_MIN_CUT,  PE_IO, ierr )
         call global_max(MAX_CUT, GLOBAL_MAX_CUT,  PE_IO, ierr )

         IF (myPE == PE_IO) THEN
            WRITE(UNIT_MSH_STATS,1000)  'U-MOMENTUM CUT CELLS:'
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXY                   = ', GLOBAL_MIN_AXY,GLOBAL_MAX_AXY
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXZ                   = ', GLOBAL_MIN_AXZ,GLOBAL_MAX_AXZ
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AYZ                   = ', GLOBAL_MIN_AYZ,GLOBAL_MAX_AYZ
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF CUT AREA              = ', GLOBAL_MIN_CUT,GLOBAL_MAX_CUT
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF VOLUME                = ', GLOBAL_MIN_VOL,GLOBAL_MAX_VOL
            WRITE(UNIT_MSH_STATS,1010)  'NUMBER OF U WALL CELLS         = ', NUMBER_OF_U_WALL_CELLS
         ENDIF

      ENDIF
      IF (myPE == PE_IO) THEN
         WRITE(UNIT_MSH_STATS,1000)  '============================================================================'
      ENDIF
!======================================================================
!  V-Momentum Cell volumes and areas
!======================================================================


      MIN_VOL =   LARGE_NUMBER
      MAX_VOL = - LARGE_NUMBER
      MIN_AYZ =   LARGE_NUMBER
      MAX_AYZ = - LARGE_NUMBER
      MIN_AXZ =   LARGE_NUMBER
      MAX_AXZ = - LARGE_NUMBER
      MIN_AXY =   LARGE_NUMBER
      MAX_AXY = - LARGE_NUMBER

      DO IJK = IJKSTART3, IJKEND3
         IF(STANDARD_V_CELL_AT(IJK)) THEN              ! STANDARD CELLS

            MIN_VOL =   DMIN1(MIN_VOL,VOL_V(IJK))
            MAX_VOL =   DMAX1(MAX_VOL,VOL_V(IJK))
            MIN_AYZ =   DMIN1(MIN_AYZ,AYZ_V(IJK))
            MAX_AYZ =   DMAX1(MAX_AYZ,AYZ_V(IJK))
            MIN_AXZ =   DMIN1(MIN_AXZ,AXZ_V(IJK))
            MAX_AXZ =   DMAX1(MAX_AXZ,AXZ_V(IJK))
            MIN_AXY =   DMIN1(MIN_AXY,AXY_V(IJK))
            MAX_AXY =   DMAX1(MAX_AXY,AXY_V(IJK))

         ENDIF
      END DO

      call global_min(MIN_VOL, GLOBAL_MIN_VOL,  PE_IO, ierr )
      call global_max(MAX_VOL, GLOBAL_MAX_VOL,  PE_IO, ierr )
      call global_min(MIN_AYZ, GLOBAL_MIN_AYZ,  PE_IO, ierr )
      call global_max(MAX_AYZ, GLOBAL_MAX_AYZ,  PE_IO, ierr )
      call global_min(MIN_AXZ, GLOBAL_MIN_AXZ,  PE_IO, ierr )
      call global_max(MAX_AXZ, GLOBAL_MAX_AXZ,  PE_IO, ierr )
      call global_min(MIN_AXY, GLOBAL_MIN_AXY,  PE_IO, ierr )
      call global_max(MAX_AXY, GLOBAL_MAX_AXY,  PE_IO, ierr )

      IF (myPE == PE_IO) THEN
         WRITE(UNIT_MSH_STATS,1000)  'V-MOMENTUM STANDARD CELLS:'
         WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXY                   = ', GLOBAL_MIN_AXY,GLOBAL_MAX_AXY
         WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXZ                   = ', GLOBAL_MIN_AXZ,GLOBAL_MAX_AXZ
         WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AYZ                   = ', GLOBAL_MIN_AYZ,GLOBAL_MAX_AYZ
         WRITE(UNIT_MSH_STATS,1000)  'RANGE OF VOLUME                = ', GLOBAL_MIN_VOL,GLOBAL_MAX_VOL
      ENDIF

      IF(NUMBER_OF_CUT_CELLS>0) THEN
         MIN_VOL =   LARGE_NUMBER
         MAX_VOL = - LARGE_NUMBER
         MIN_AYZ =   LARGE_NUMBER
         MAX_AYZ = - LARGE_NUMBER
         MIN_AXZ =   LARGE_NUMBER
         MAX_AXZ = - LARGE_NUMBER
         MIN_AXY =   LARGE_NUMBER
         MAX_AXY = - LARGE_NUMBER
         MIN_CUT =   LARGE_NUMBER
         MAX_CUT = - LARGE_NUMBER

         DO IJK = IJKSTART3, IJKEND3
            IF(CUT_V_CELL_AT(IJK).AND.(.NOT.WALL_V_AT(IJK))) THEN      ! CUT CELLS

               MIN_VOL =   DMIN1(MIN_VOL,VOL_V(IJK))
               MAX_VOL =   DMAX1(MAX_VOL,VOL_V(IJK))
               MIN_AYZ =   DMIN1(MIN_AYZ,AYZ_V(IJK))
               MAX_AYZ =   DMAX1(MAX_AYZ,AYZ_V(IJK))
               MIN_AXZ =   DMIN1(MIN_AXZ,AXZ_V(IJK))
               MAX_AXZ =   DMAX1(MAX_AXZ,AXZ_V(IJK))
               MIN_AXY =   DMIN1(MIN_AXY,AXY_V(IJK))
               MAX_AXY =   DMAX1(MAX_AXY,AXY_V(IJK))
               MIN_CUT =   DMIN1(MIN_CUT,AREA_V_CUT(IJK))
               MAX_CUT =   DMAX1(MAX_CUT,AREA_V_CUT(IJK))

            ENDIF
         END DO

         call global_min(MIN_VOL, GLOBAL_MIN_VOL,  PE_IO, ierr )
         call global_max(MAX_VOL, GLOBAL_MAX_VOL,  PE_IO, ierr )
         call global_min(MIN_AYZ, GLOBAL_MIN_AYZ,  PE_IO, ierr )
         call global_max(MAX_AYZ, GLOBAL_MAX_AYZ,  PE_IO, ierr )
         call global_min(MIN_AXZ, GLOBAL_MIN_AXZ,  PE_IO, ierr )
         call global_max(MAX_AXZ, GLOBAL_MAX_AXZ,  PE_IO, ierr )
         call global_min(MIN_AXY, GLOBAL_MIN_AXY,  PE_IO, ierr )
         call global_max(MAX_AXY, GLOBAL_MAX_AXY,  PE_IO, ierr )
         call global_min(MIN_CUT, GLOBAL_MIN_CUT,  PE_IO, ierr )
         call global_max(MAX_CUT, GLOBAL_MAX_CUT,  PE_IO, ierr )

         IF (myPE == PE_IO) THEN
            WRITE(UNIT_MSH_STATS,1000)  'V-MOMENTUM CUT CELLS:'
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXY                   = ', GLOBAL_MIN_AXY,GLOBAL_MAX_AXY
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXZ                   = ', GLOBAL_MIN_AXZ,GLOBAL_MAX_AXZ
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AYZ                   = ', GLOBAL_MIN_AYZ,GLOBAL_MAX_AYZ
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF CUT AREA              = ', GLOBAL_MIN_CUT,GLOBAL_MAX_CUT
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF VOLUME                = ', GLOBAL_MIN_VOL,GLOBAL_MAX_VOL
            WRITE(UNIT_MSH_STATS,1010)  'NUMBER OF V WALL CELLS         = ', NUMBER_OF_V_WALL_CELLS
         ENDIF

      ENDIF
      IF (myPE == PE_IO) THEN
         WRITE(UNIT_MSH_STATS,1000)  '============================================================================'
      ENDIF
!======================================================================
!  W-Momentum Cell volumes and areas
!======================================================================


      IF(DO_K) THEN

         MIN_VOL =   LARGE_NUMBER
         MAX_VOL = - LARGE_NUMBER
         MIN_AYZ =   LARGE_NUMBER
         MAX_AYZ = - LARGE_NUMBER
         MIN_AXZ =   LARGE_NUMBER
         MAX_AXZ = - LARGE_NUMBER
         MIN_AXY =   LARGE_NUMBER
         MAX_AXY = - LARGE_NUMBER

         DO IJK = IJKSTART3, IJKEND3
            IF(STANDARD_W_CELL_AT(IJK)) THEN              ! STANDARD CELLS

               MIN_VOL =   DMIN1(MIN_VOL,VOL_W(IJK))
               MAX_VOL =   DMAX1(MAX_VOL,VOL_W(IJK))
               MIN_AYZ =   DMIN1(MIN_AYZ,AYZ_W(IJK))
               MAX_AYZ =   DMAX1(MAX_AYZ,AYZ_W(IJK))
               MIN_AXZ =   DMIN1(MIN_AXZ,AXZ_W(IJK))
               MAX_AXZ =   DMAX1(MAX_AXZ,AXZ_W(IJK))
               MIN_AXY =   DMIN1(MIN_AXY,AXY_W(IJK))
               MAX_AXY =   DMAX1(MAX_AXY,AXY_W(IJK))

            ENDIF
         END DO

         call global_min(MIN_VOL, GLOBAL_MIN_VOL,  PE_IO, ierr )
         call global_max(MAX_VOL, GLOBAL_MAX_VOL,  PE_IO, ierr )
         call global_min(MIN_AYZ, GLOBAL_MIN_AYZ,  PE_IO, ierr )
         call global_max(MAX_AYZ, GLOBAL_MAX_AYZ,  PE_IO, ierr )
         call global_min(MIN_AXZ, GLOBAL_MIN_AXZ,  PE_IO, ierr )
         call global_max(MAX_AXZ, GLOBAL_MAX_AXZ,  PE_IO, ierr )
         call global_min(MIN_AXY, GLOBAL_MIN_AXY,  PE_IO, ierr )
         call global_max(MAX_AXY, GLOBAL_MAX_AXY,  PE_IO, ierr )

         IF (myPE == PE_IO) THEN
            WRITE(UNIT_MSH_STATS,1000)  'W-MOMENTUM STANDARD CELLS:'
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXY                   = ', GLOBAL_MIN_AXY,GLOBAL_MAX_AXY
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXZ                   = ', GLOBAL_MIN_AXZ,GLOBAL_MAX_AXZ
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AYZ                   = ', GLOBAL_MIN_AYZ,GLOBAL_MAX_AYZ
            WRITE(UNIT_MSH_STATS,1000)  'RANGE OF VOLUME                = ', GLOBAL_MIN_VOL,GLOBAL_MAX_VOL
         ENDIF

         IF(NUMBER_OF_CUT_CELLS>0) THEN
            MIN_VOL =   LARGE_NUMBER
            MAX_VOL = - LARGE_NUMBER
            MIN_AYZ =   LARGE_NUMBER
            MAX_AYZ = - LARGE_NUMBER
            MIN_AXZ =   LARGE_NUMBER
            MAX_AXZ = - LARGE_NUMBER
            MIN_AXY =   LARGE_NUMBER
            MAX_AXY = - LARGE_NUMBER
            MIN_CUT =   LARGE_NUMBER
            MAX_CUT = - LARGE_NUMBER

            DO IJK = IJKSTART3, IJKEND3
               IF(CUT_W_CELL_AT(IJK).AND.(.NOT.WALL_W_AT(IJK))) THEN      ! CUT CELLS

                  MIN_VOL =   DMIN1(MIN_VOL,VOL_W(IJK))
                  MAX_VOL =   DMAX1(MAX_VOL,VOL_W(IJK))
                  MIN_AYZ =   DMIN1(MIN_AYZ,AYZ_W(IJK))
                  MAX_AYZ =   DMAX1(MAX_AYZ,AYZ_W(IJK))
                  MIN_AXZ =   DMIN1(MIN_AXZ,AXZ_W(IJK))
                  MAX_AXZ =   DMAX1(MAX_AXZ,AXZ_W(IJK))
                  MIN_AXY =   DMIN1(MIN_AXY,AXY_W(IJK))
                  MAX_AXY =   DMAX1(MAX_AXY,AXY_W(IJK))
                  MIN_CUT =   DMIN1(MIN_CUT,AREA_W_CUT(IJK))
                  MAX_CUT =   DMAX1(MAX_CUT,AREA_W_CUT(IJK))

               ENDIF
            END DO

            call global_min(MIN_VOL, GLOBAL_MIN_VOL,  PE_IO, ierr )
            call global_max(MAX_VOL, GLOBAL_MAX_VOL,  PE_IO, ierr )
            call global_min(MIN_AYZ, GLOBAL_MIN_AYZ,  PE_IO, ierr )
            call global_max(MAX_AYZ, GLOBAL_MAX_AYZ,  PE_IO, ierr )
            call global_min(MIN_AXZ, GLOBAL_MIN_AXZ,  PE_IO, ierr )
            call global_max(MAX_AXZ, GLOBAL_MAX_AXZ,  PE_IO, ierr )
            call global_min(MIN_AXY, GLOBAL_MIN_AXY,  PE_IO, ierr )
            call global_max(MAX_AXY, GLOBAL_MAX_AXY,  PE_IO, ierr )
            call global_min(MIN_CUT, GLOBAL_MIN_CUT,  PE_IO, ierr )
            call global_max(MAX_CUT, GLOBAL_MAX_CUT,  PE_IO, ierr )

            IF (myPE == PE_IO) THEN
               WRITE(UNIT_MSH_STATS,1000)  'W-MOMENTUM CUT CELLS:'
               WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXY                   = ', GLOBAL_MIN_AXY,GLOBAL_MAX_AXY
               WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AXZ                   = ', GLOBAL_MIN_AXZ,GLOBAL_MAX_AXZ
               WRITE(UNIT_MSH_STATS,1000)  'RANGE OF AYZ                   = ', GLOBAL_MIN_AYZ,GLOBAL_MAX_AYZ
               WRITE(UNIT_MSH_STATS,1000)  'RANGE OF CUT AREA              = ', GLOBAL_MIN_CUT,GLOBAL_MAX_CUT
               WRITE(UNIT_MSH_STATS,1000)  'RANGE OF VOLUME                = ', GLOBAL_MIN_VOL,GLOBAL_MAX_VOL
               WRITE(UNIT_MSH_STATS,1010)  'NUMBER OF W WALL CELLS         = ', NUMBER_OF_W_WALL_CELLS
            ENDIF

         ENDIF
         IF (myPE == PE_IO) THEN
            WRITE(UNIT_MSH_STATS,1000)  '============================================================================'
         ENDIF

      ENDIF



      LOCAL_MIN_Q = MINVAL(Alpha_Ue_c)
      LOCAL_MAX_Q = MAXVAL(Alpha_Ue_c)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO)  WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF U-MOMENTUM Alpha_Ue_c = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q

      LOCAL_MIN_Q = MINVAL(Alpha_Un_c)
      LOCAL_MAX_Q = MAXVAL(Alpha_Un_c)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF U-MOMENTUM Alpha_Un_c = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q

      LOCAL_MIN_Q = MINVAL(Alpha_Ut_c)
      LOCAL_MAX_Q = MAXVAL(Alpha_Ut_c)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF U-MOMENTUM Alpha_Ut_c = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q

      LOCAL_MIN_Q = MINVAL(Theta_Ue)
      LOCAL_MAX_Q = MAXVAL(Theta_Ue)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF U-MOMENTUM Theta_Ue   = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q

      LOCAL_MIN_Q = MINVAL(Theta_Un)
      LOCAL_MAX_Q = MAXVAL(Theta_Un)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF U-MOMENTUM Theta_Un   = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)

      LOCAL_MIN_Q = MINVAL(Theta_Ut)
      LOCAL_MAX_Q = MAXVAL(Theta_Ut)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF U-MOMENTUM Theta_Ut   = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)

      LOCAL_MIN_Q = MINVAL(Theta_U_ne)
      LOCAL_MAX_Q = MAXVAL(Theta_U_ne)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF U-MOMENTUM Theta_U_ne = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q

      LOCAL_MIN_Q = MINVAL(Theta_U_te)
      LOCAL_MAX_Q = MAXVAL(Theta_U_te)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF U-MOMENTUM Theta_U_te = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)

      LOCAL_MIN_Q = MINVAL(NOC_U_E)
      LOCAL_MAX_Q = MAXVAL(NOC_U_E)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF U-MOMENTUM NOC_U_E    = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q

      LOCAL_MIN_Q = MINVAL(NOC_U_N)
      LOCAL_MAX_Q = MAXVAL(NOC_U_N)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF U-MOMENTUM NOC_U_N    = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q

      LOCAL_MIN_Q = MINVAL(NOC_U_T)
      LOCAL_MAX_Q = MAXVAL(NOC_U_T)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF U-MOMENTUM NOC_U_T    = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)

      LOCAL_MIN_Q = MINVAL(DELH_U)
      LOCAL_MAX_Q = MAXVAL(DELH_U)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF U-MOMENTUM DELH_U     = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  '============================================================================'



      LOCAL_MIN_Q = MINVAL(Alpha_Ve_c)
      LOCAL_MAX_Q = MAXVAL(Alpha_Ve_c)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF V-MOMENTUM Alpha_Ve_c = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      LOCAL_MIN_Q = MINVAL(Alpha_Vn_c)
      LOCAL_MAX_Q = MAXVAL(Alpha_Vn_c)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF V-MOMENTUM Alpha_Vn_c = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      LOCAL_MIN_Q = MINVAL(Alpha_Vt_c)
      LOCAL_MAX_Q = MAXVAL(Alpha_Vt_c)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF V-MOMENTUM Alpha_Vt_c = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)
      LOCAL_MIN_Q = MINVAL(Theta_Ve)
      LOCAL_MAX_Q = MAXVAL(Theta_Ve)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF V-MOMENTUM Theta_Ve   = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      LOCAL_MIN_Q = MINVAL(Theta_Vn)
      LOCAL_MAX_Q = MAXVAL(Theta_Vn)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF V-MOMENTUM Theta_Vn   = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      LOCAL_MIN_Q = MINVAL(Theta_Vt)
      LOCAL_MAX_Q = MAXVAL(Theta_Vt)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF V-MOMENTUM Theta_Vt   = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)
      LOCAL_MIN_Q = MINVAL(Theta_V_ne)
      LOCAL_MAX_Q = MAXVAL(Theta_V_ne)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF V-MOMENTUM Theta_V_ne = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      LOCAL_MIN_Q = MINVAL(Theta_V_nt)
      LOCAL_MAX_Q = MAXVAL(Theta_V_nt)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF V-MOMENTUM Theta_V_nt = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)
      LOCAL_MIN_Q = MINVAL(NOC_V_E)
      LOCAL_MAX_Q = MAXVAL(NOC_V_E)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF V-MOMENTUM NOC_V_E    = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      LOCAL_MIN_Q = MINVAL(NOC_V_N)
      LOCAL_MAX_Q = MAXVAL(NOC_V_N)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF V-MOMENTUM NOC_V_N    = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      LOCAL_MIN_Q = MINVAL(NOC_V_T)
      LOCAL_MAX_Q = MAXVAL(NOC_V_T)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF V-MOMENTUM NOC_V_T    = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)
      LOCAL_MIN_Q = MINVAL(DELH_V)
      LOCAL_MAX_Q = MAXVAL(DELH_V)
      call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF V-MOMENTUM DELH_V     = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
      IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  '============================================================================'


      IF(DO_K) THEN

         LOCAL_MIN_Q = MINVAL(Alpha_We_c)
         LOCAL_MAX_Q = MAXVAL(Alpha_We_c)
         call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
         call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF W-MOMENTUM Alpha_We_c = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
         LOCAL_MIN_Q = MINVAL(Alpha_Wn_c)
         LOCAL_MAX_Q = MAXVAL(Alpha_Wn_c)
         call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
         call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF W-MOMENTUM Alpha_Wn_c = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
         LOCAL_MIN_Q = MINVAL(Alpha_Wt_c)
         LOCAL_MAX_Q = MAXVAL(Alpha_Wt_c)
         call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
         call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF W-MOMENTUM Alpha_Wt_c = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)
         LOCAL_MIN_Q = MINVAL(Theta_We)
         LOCAL_MAX_Q = MAXVAL(Theta_We)
         call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
         call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF W-MOMENTUM Theta_We   = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
         LOCAL_MIN_Q = MINVAL(Theta_Wn)
         LOCAL_MAX_Q = MAXVAL(Theta_Wn)
         call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
         call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF W-MOMENTUM Theta_Wn   = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
         LOCAL_MIN_Q = MINVAL(Theta_Wt)
         LOCAL_MAX_Q = MAXVAL(Theta_Wt)
         call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
         call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF W-MOMENTUM Theta_Wt   = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)
         LOCAL_MIN_Q = MINVAL(Theta_W_te)
         LOCAL_MAX_Q = MAXVAL(Theta_W_te)
         call global_min(LOCAL_MAX_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
         call global_max(LOCAL_MIN_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF W-MOMENTUM Theta_W_te = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
         LOCAL_MIN_Q = MINVAL(Theta_W_tn)
         LOCAL_MAX_Q = MAXVAL(Theta_W_tn)
         call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
         call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF W-MOMENTUM Theta_W_tn = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)
         LOCAL_MIN_Q = MINVAL(NOC_W_E)
         LOCAL_MAX_Q = MAXVAL(NOC_W_E)
         call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
         call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF W-MOMENTUM NOC_W_E    = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
         LOCAL_MIN_Q = MINVAL(NOC_W_N)
         LOCAL_MAX_Q = MAXVAL(NOC_W_N)
         call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
         call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF W-MOMENTUM NOC_W_N    = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
         LOCAL_MIN_Q = MINVAL(NOC_W_T)
         LOCAL_MAX_Q = MAXVAL(NOC_W_T)
         call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
         call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF W-MOMENTUM NOC_W_T    = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)
         LOCAL_MIN_Q = MINVAL(DELH_W)
         LOCAL_MAX_Q = MAXVAL(DELH_W)
         call global_min(LOCAL_MIN_Q, GLOBAL_MIN_Q,  PE_IO, ierr )
         call global_max(LOCAL_MAX_Q, GLOBAL_MAX_Q,  PE_IO, ierr )
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  ' RANGE OF W-MOMENTUM DELH_W     = ', GLOBAL_MIN_Q, GLOBAL_MAX_Q
         IF (myPE == PE_IO) WRITE(UNIT_MSH_STATS,1000)  '============================================================================'

      ENDIF

   ENDIF !CARTESIAN_GRID

#ifdef MPI
      call MPI_barrier(MPI_COMM_WORLD,mpierr)
#endif

! Histograms
! Scalar mesh
      CALL PRINT_HISTOGRAM(AXY,'AXY (SCALAR STANDARD CELLS)',STANDARD_CELL_AT)
      CALL PRINT_HISTOGRAM(AXZ,'AXZ (SCALAR STANDARD CELLS)',STANDARD_CELL_AT)
      CALL PRINT_HISTOGRAM(AYZ,'AYZ (SCALAR STANDARD CELLS)',STANDARD_CELL_AT)
      CALL PRINT_HISTOGRAM(VOL,'VOL (SCALAR STANDARD CELLS)',STANDARD_CELL_AT)
      CALL PRINT_HISTOGRAM(Aspect_Ratio,'Aspect Ratio (SCALAR STANDARD CELLS)',STANDARD_CELL_AT)

      IF(CARTESIAN_GRID) THEN

      IF(NUMBER_OF_CUT_CELLS>0) THEN
         CALL PRINT_HISTOGRAM(AXY,'AXY (SCALAR CUT CELLS)',CUT_CELL_AT)
         CALL PRINT_HISTOGRAM(AXZ,'AXZ (SCALAR CUT CELLS)',CUT_CELL_AT)
         CALL PRINT_HISTOGRAM(AYZ,'AYZ (SCALAR CUT CELLS)',CUT_CELL_AT)
         CALL PRINT_HISTOGRAM(AYZ,'AYZ (SCALAR CUT CELLS)',CUT_CELL_AT)
         CALL PRINT_HISTOGRAM(VOL,'VOL (SCALAR CUT CELLS)',CUT_CELL_AT)
         CALL PRINT_HISTOGRAM(Aspect_Ratio,'Aspect Ratio (SCALAR CUT CELLS)',CUT_CELL_AT)
      ENDIF

! U-Momentum mesh
      CALL PRINT_HISTOGRAM(AXY_U,'AXY (U-MOMENTUM STANDARD CELLS)',STANDARD_U_CELL_AT)
      CALL PRINT_HISTOGRAM(AXZ_U,'AXZ (U-MOMENTUM STANDARD CELLS)',STANDARD_U_CELL_AT)
      CALL PRINT_HISTOGRAM(AYZ_U,'AYZ (U-MOMENTUM STANDARD CELLS)',STANDARD_U_CELL_AT)
      CALL PRINT_HISTOGRAM(VOL_U,'VOL (U-MOMENTUM STANDARD CELLS)',STANDARD_U_CELL_AT)
      CALL PRINT_HISTOGRAM(Aspect_Ratio_U,'Aspect Ratio (U-MOMENTUM STANDARD CELLS)',STANDARD_U_CELL_AT)

      IF(NUMBER_OF_CUT_CELLS>0) THEN
         CALL PRINT_HISTOGRAM(AXY_U,     'AXY (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))
         CALL PRINT_HISTOGRAM(AXZ_U,     'AXZ (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))
         CALL PRINT_HISTOGRAM(AYZ_U,     'AYZ (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))
         CALL PRINT_HISTOGRAM(AREA_U_CUT,'CUT FACE AREA (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))
         CALL PRINT_HISTOGRAM(VOL_U,     'VOL (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))
         CALL PRINT_HISTOGRAM(Aspect_Ratio_U,'Aspect Ratio (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))

         CALL PRINT_HISTOGRAM(Alpha_Ue_c,'Alpha_Ue_c (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))
         CALL PRINT_HISTOGRAM(Alpha_Un_c,'Alpha_Un_c (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))
         CALL PRINT_HISTOGRAM(Alpha_Ut_c,'Alpha_Ut_c (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))

         CALL PRINT_HISTOGRAM(Theta_Ue,  'Theta_Ue (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))
         CALL PRINT_HISTOGRAM(Theta_Un,  'Theta_Un (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))
         CALL PRINT_HISTOGRAM(Theta_Ut,  'Theta_Ut (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))

         CALL PRINT_HISTOGRAM(Theta_U_ne,'Theta_U_ne (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))
         CALL PRINT_HISTOGRAM(Theta_U_te,'Theta_U_te (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))

         CALL PRINT_HISTOGRAM(NOC_U_E,   'NOC_U_E (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))
         CALL PRINT_HISTOGRAM(NOC_U_N,   'NOC_U_N (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))
         CALL PRINT_HISTOGRAM(NOC_U_T,   'NOC_U_T (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))

         CALL PRINT_HISTOGRAM(DELH_U,    'DELH_U (U-MOMENTUM CUT CELLS)',CUT_U_CELL_AT.AND.(.NOT.WALL_U_AT))
      ENDIF

! V-Momentum mesh
      CALL PRINT_HISTOGRAM(AXY_V,'AXY (V-MOMENTUM STANDARD CELLS)',STANDARD_V_CELL_AT)
      CALL PRINT_HISTOGRAM(AXZ_V,'AXZ (V-MOMENTUM STANDARD CELLS)',STANDARD_V_CELL_AT)
      CALL PRINT_HISTOGRAM(AYZ_V,'AYZ (V-MOMENTUM STANDARD CELLS)',STANDARD_V_CELL_AT)
      CALL PRINT_HISTOGRAM(VOL_V,'VOL (V-MOMENTUM STANDARD CELLS)',STANDARD_V_CELL_AT)
      CALL PRINT_HISTOGRAM(Aspect_Ratio_V,'Aspect Ratio (V-MOMENTUM STANDARD CELLS)',STANDARD_V_CELL_AT)

      IF(NUMBER_OF_CUT_CELLS>0) THEN
         CALL PRINT_HISTOGRAM(AXY_V,     'AXY (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))
         CALL PRINT_HISTOGRAM(AXZ_V,     'AXZ (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))
         CALL PRINT_HISTOGRAM(AYZ_V,     'AYZ (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))
         CALL PRINT_HISTOGRAM(AREA_V_CUT,'CUT FACE AREA (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))
         CALL PRINT_HISTOGRAM(VOL_V,     'VOL (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))
         CALL PRINT_HISTOGRAM(Aspect_Ratio_V,'Aspect Ratio (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))

         CALL PRINT_HISTOGRAM(Alpha_Ve_c,'Alpha_Ve_c (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))
         CALL PRINT_HISTOGRAM(Alpha_Vn_c,'Alpha_Vn_c (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))
         CALL PRINT_HISTOGRAM(Alpha_Vt_c,'Alpha_Vt_c (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))

         CALL PRINT_HISTOGRAM(Theta_Ve,  'Theta_Ve (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))
         CALL PRINT_HISTOGRAM(Theta_Vn,  'Theta_Vn (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))
         CALL PRINT_HISTOGRAM(Theta_Vt,  'Theta_Vt (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))

         CALL PRINT_HISTOGRAM(Theta_V_ne,'Theta_V_ne (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))
         CALL PRINT_HISTOGRAM(Theta_V_nt,'Theta_V_nt (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))

         CALL PRINT_HISTOGRAM(NOC_V_E,   'NOC_V_E (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))
         CALL PRINT_HISTOGRAM(NOC_V_N,   'NOC_V_N (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))
         CALL PRINT_HISTOGRAM(NOC_V_T,   'NOC_V_T (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))

         CALL PRINT_HISTOGRAM(DELH_V,    'DELH_V (V-MOMENTUM CUT CELLS)',CUT_V_CELL_AT.AND.(.NOT.WALL_V_AT))
      ENDIF

! W-Momentum mesh
      IF(DO_K) THEN
         CALL PRINT_HISTOGRAM(AXY_W,'AXY (W-MOMENTUM STANDARD CELLS)',STANDARD_W_CELL_AT)
         CALL PRINT_HISTOGRAM(AXZ_W,'AXZ (W-MOMENTUM STANDARD CELLS)',STANDARD_W_CELL_AT)
         CALL PRINT_HISTOGRAM(AYZ_W,'AYZ (W-MOMENTUM STANDARD CELLS)',STANDARD_W_CELL_AT)
         CALL PRINT_HISTOGRAM(VOL_W,'VOL (W-MOMENTUM STANDARD CELLS)',STANDARD_W_CELL_AT)
         CALL PRINT_HISTOGRAM(Aspect_Ratio_W,'Aspect Ratio (W-MOMENTUM STANDARD CELLS)',STANDARD_W_CELL_AT)

         IF(NUMBER_OF_CUT_CELLS>0) THEN
            CALL PRINT_HISTOGRAM(AXY_W,     'AXY (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))
            CALL PRINT_HISTOGRAM(AXZ_W,     'AXZ (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))
            CALL PRINT_HISTOGRAM(AYZ_W,     'AYZ (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))
            CALL PRINT_HISTOGRAM(AREA_W_CUT,'CUT FACE AREA (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))
            CALL PRINT_HISTOGRAM(VOL_W,     'VOL (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))
            CALL PRINT_HISTOGRAM(Aspect_Ratio_W,'Aspect Ratio (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))

            CALL PRINT_HISTOGRAM(Alpha_We_c,'Alpha_We_c (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))
            CALL PRINT_HISTOGRAM(Alpha_Wn_c,'Alpha_Wn_c (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))
            CALL PRINT_HISTOGRAM(Alpha_Wt_c,'Alpha_Wt_c (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))

            CALL PRINT_HISTOGRAM(Theta_We,  'Theta_We (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))
            CALL PRINT_HISTOGRAM(Theta_Wn,  'Theta_Wn (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))
            CALL PRINT_HISTOGRAM(Theta_Wt,  'Theta_Wt (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))

            CALL PRINT_HISTOGRAM(Theta_W_te,'Theta_W_te (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))
            CALL PRINT_HISTOGRAM(Theta_W_tn,'Theta_W_tn (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))

            CALL PRINT_HISTOGRAM(NOC_W_E,   'NOC_W_E (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))
            CALL PRINT_HISTOGRAM(NOC_W_N,   'NOC_W_N (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))
            CALL PRINT_HISTOGRAM(NOC_W_T,   'NOC_W_T (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))

            CALL PRINT_HISTOGRAM(DELH_W,    'DELH_W (W-MOMENTUM CUT CELLS)',CUT_W_CELL_AT.AND.(.NOT.WALL_W_AT))
         ENDIF
      ENDIF

      ENDIF ! CARTESIAN_GRID

#ifdef MPI
      call MPI_barrier(MPI_COMM_WORLD,mpierr)
#endif

      IF(MyPE == PE_IO) THEN
         CLOSE(UNIT_MSH_STATS)
      ENDIF


      RETURN

      END SUBROUTINE SAVE_MESH_STATISTICS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: PRINT_HISTOGRAM                                        C
!  Purpose: PRINT HISTOGRAM                                            C
!                                                                      C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 13-Jan-20  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C 
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE PRINT_HISTOGRAM(VAR,VARNAME,CRITERION_MET_AT)
    
      USE param
      USE param1
      USE parallel
      USE constant
      USE run
      USE toleranc
      USE geometry
      USE indices  
      USE compar
      USE mpi_utility 
      USE sendrecv
      USE quadric
      USE cutcell
      USE fldvar
      USE vtk


     
      IMPLICIT NONE

      INTEGER :: IJK,I,J,K,L
      INTEGER :: IJK_OFFSET

      INTEGER :: iproc,IERR

      DOUBLE PRECISION :: GLOBAL_MIN_Q,GLOBAL_MAX_Q, GLOBAL_RANGE

      DOUBLE PRECISION, DIMENSION(DIMENSION_3) ::  VAR
      LOGICAL, DIMENSION(DIMENSION_3)           ::  CRITERION_MET_AT
      CHARACTER (LEN=*) :: VARNAME
      INTEGER:: NUMBER_OF_CELLS_WITH_THIS_CRITERION,CELLS_IN_BIN
      INTEGER:: GLOBAL_NUMBER_OF_CELLS_WITH_THIS_CRITERION
      INTEGER:: H

      INTEGER, PARAMETER:: HISTO_DIVS          = 10
      INTEGER, PARAMETER:: HISTO_MAXHEIGHT     = 25
      DOUBLE PRECISION, PARAMETER:: HISTO_PMAX = 100.0
      INTEGER:: HISTO_HEIGHT,HISTO_START

      DOUBLE PRECISION, DIMENSION(HISTO_DIVS) :: HISTO_VALUE,BIN_PMIN,BIN_PMAX,BIN_VMIN,BIN_VMAX
      DOUBLE PRECISION, DIMENSION(HISTO_DIVS) :: GLOBAL_HISTO_VALUE
      DOUBLE PRECISION                         :: HISTO_STEP_SIZE,LOCAL_MIN,LOCAL_MAX,LOCAL_RANGE

      CHARACTER (LEN=1), DIMENSION(HISTO_DIVS,HISTO_MAXHEIGHT+2) :: HISTOCHAR



      HISTO_STEP_SIZE = HISTO_PMAX / HISTO_DIVS
      HISTO_VALUE     = ZERO

      DO H = 1,HISTO_DIVS
         BIN_PMIN(H) = DFLOAT(H-1)*HISTO_STEP_SIZE
         BIN_PMAX(H) = DFLOAT(H)  *HISTO_STEP_SIZE
      ENDDO


! Find local min and max
      LOCAL_MIN = UNDEFINED
      LOCAL_MAX = -UNDEFINED
      NUMBER_OF_CELLS_WITH_THIS_CRITERION = 0

      DO IJK = IJKSTART3, IJKEND3
         IF(CRITERION_MET_AT(IJK).AND.INTERIOR_CELL_AT(IJK)) THEN      ! ONLY LOOK AT CELLS WITH THIS CRITERION

            NUMBER_OF_CELLS_WITH_THIS_CRITERION = NUMBER_OF_CELLS_WITH_THIS_CRITERION + 1

            LOCAL_MIN =   DMIN1(LOCAL_MIN,VAR(IJK))
            LOCAL_MAX =   DMAX1(LOCAL_MAX,VAR(IJK))

         ENDIF
      END DO


! Get global min and max      
      call global_all_sum(NUMBER_OF_CELLS_WITH_THIS_CRITERION,GLOBAL_NUMBER_OF_CELLS_WITH_THIS_CRITERION)
      call global_all_min(LOCAL_MIN, GLOBAL_MIN_Q,  PE_IO, ierr )
      call global_all_max(LOCAL_MAX, GLOBAL_MAX_Q,  PE_IO, ierr )

      GLOBAL_RANGE =  GLOBAL_MAX_Q - GLOBAL_MIN_Q

      HISTO_START = 1
      IF(GLOBAL_RANGE==ZERO) HISTO_START = HISTO_DIVS

! Set Histogram bins      
      DO H = HISTO_START,HISTO_DIVS
         BIN_VMIN(H) = GLOBAL_MIN_Q + BIN_PMIN(H)/HISTO_PMAX*GLOBAL_RANGE
         BIN_VMAX(H) = GLOBAL_MIN_Q + BIN_PMAX(H)/HISTO_PMAX*GLOBAL_RANGE
      ENDDO

! Fill histogram      
      DO IJK = IJKSTART3, IJKEND3
         IF(CRITERION_MET_AT(IJK).AND.INTERIOR_CELL_AT(IJK)) THEN      ! ONLY LOOK AT CELLS WITH THIS CRITERION

            DO H = HISTO_START,HISTO_DIVS

               IF(BIN_VMIN(H)<=VAR(IJK).AND.VAR(IJK)<BIN_VMAX(H)) THEN
                  HISTO_VALUE(H) = HISTO_VALUE(H) + ONE
               ENDIF

            ENDDO

            IF(VAR(IJK)>=GLOBAL_MAX_Q)   HISTO_VALUE(HISTO_DIVS) = HISTO_VALUE(HISTO_DIVS) + ONE

         ENDIF
      ENDDO


      call global_all_sum(HISTO_VALUE(:),GLOBAL_HISTO_VALUE(:))


! Write histogram      
      IF(MYPE==PE_IO) THEN

         WRITE(UNIT_MSH_STATS,110)
         WRITE(UNIT_MSH_STATS,100)'HISTOGRAM FOR: ', TRIM(VARNAME)
         WRITE(UNIT_MSH_STATS,120)'MIN = ', GLOBAL_MIN_Q
         WRITE(UNIT_MSH_STATS,120)'MAX = ', GLOBAL_MAX_Q
         WRITE(UNIT_MSH_STATS,110)


         DO H = HISTO_START,HISTO_DIVS

            CELLS_IN_BIN = GLOBAL_HISTO_VALUE(H)
            GLOBAL_HISTO_VALUE(H)=GLOBAL_HISTO_VALUE(H)/GLOBAL_NUMBER_OF_CELLS_WITH_THIS_CRITERION

            HISTO_HEIGHT = INT(GLOBAL_HISTO_VALUE(H)*HISTO_MAXHEIGHT)
            HISTOCHAR(H,1) = '|'         
            HISTOCHAR(H,2:HISTO_HEIGHT+1) = '#'
            HISTOCHAR(H,HISTO_HEIGHT+2:HISTO_MAXHEIGHT+1) = ''
            HISTOCHAR(H,HISTO_MAXHEIGHT+2) = '|'

             WRITE(UNIT_MSH_STATS,200) BIN_VMIN(H),' -->',BIN_VMAX(H),HISTOCHAR(H,:),CELLS_IN_BIN,GLOBAL_HISTO_VALUE(H)*100.0D0,'%'

         ENDDO
         WRITE(UNIT_MSH_STATS,110)

      ENDIF


      RETURN



100       FORMAT(A,A)
110       FORMAT(76('='))
120       FORMAT(A,E12.4)
200       FORMAT(E10.3,A,E10.3,1X,27(A),I8,' CELLS OR'F6.1,A)



      END SUBROUTINE PRINT_HISTOGRAM


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: CLEAN_GEOMETRY                                         C
!  Purpose: Clean-up the list of point and only keep points            C
!           that are used in the connectivity list.                    C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 19-Dec-14  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE CLEAN_GEOMETRY

      IMPLICIT NONE

      INTEGER :: IJK,L

      INTEGER ::POINT_ID,IJKC
      INTEGER , ALLOCATABLE        ::  POINT_NEW_ID(:)
      INTEGER , ALLOCATABLE        ::  NEW_POINT_NEW_ID(:)
      LOGICAL , ALLOCATABLE        ::  KEEP_POINT(:)
      LOGICAL , ALLOCATABLE        ::  KEEP_NEW_POINT(:)
      DOUBLE PRECISION             ::  XCYL,ZCYL

      INTEGER :: ALLOC_SIZE


!      print*,'top of clean geo, NUMBER_OF_POINTS=',NUMBER_OF_POINTS
!      print*,'top of clean geo, GLOBAL NUMBER_OF_NEW_POINTS=',GLOBAL_NUMBER_OF_NEW_POINTS
      IF (myPE == PE_IO.AND.(.NOT.BDIST_IO)) THEN

         IF(ALLOCATED(GLOBAL_CLEANED_CONNECTIVITY)) DEALLOCATE(GLOBAL_CLEANED_CONNECTIVITY)
         IF(ALLOCATED(KEEP_NEW_POINT))              DEALLOCATE (KEEP_NEW_POINT)
         IF(ALLOCATED(POINT_NEW_ID))                DEALLOCATE (POINT_NEW_ID)
         IF(ALLOCATED(NEW_POINT_NEW_ID))            DEALLOCATE (NEW_POINT_NEW_ID)
         IF(ALLOCATED(KEEP_POINT))                  DEALLOCATE (KEEP_POINT)

         ALLOCATE (GLOBAL_CLEANED_CONNECTIVITY(ijkmax3,15))
         ALLOCATE (KEEP_NEW_POINT(GLOBAL_NUMBER_OF_NEW_POINTS))

         ALLOC_SIZE = MAX(IJKMAX3,GLOBAL_NUMBER_OF_NEW_POINTS)
         ALLOCATE (POINT_NEW_ID(ALLOC_SIZE))
         ALLOCATE (NEW_POINT_NEW_ID(ALLOC_SIZE))
         ALLOCATE (KEEP_POINT(ALLOC_SIZE))

! Step 1: Go through connectivity list and only keep points that are used.
!         For background cell corners, assign KEEP_POINT = .TRUE.
!         For cut cells, the new intersection points were called NEW_POINTS,
!         so assign KEEP_NEW_POINT = .TRUE.
!         A NEW_POINT had an IJK index larger than IJKMAX3

         KEEP_POINT = .FALSE.
         KEEP_NEW_POINT = .FALSE.

         DO IJK = 1,IJKMAX3
            IF (BELONGS_TO_VTK_SUBDOMAIN(IJK)) THEN
!               print*,'belongs to vtk',IJK,GLOBAL_CONNECTIVITY(IJK,1:GLOBAL_NUMBER_OF_NODES(IJK))
               DO L=1,GLOBAL_NUMBER_OF_NODES(IJK)
                  IJKC = GLOBAL_CONNECTIVITY(IJK,L)
                  IF(IJKC<=IJKMAX3) KEEP_POINT(IJKC) = .TRUE.
                  IF(IJKC>IJKMAX3) KEEP_NEW_POINT(IJKC-IJKMAX3) = .TRUE.
               ENDDO
            ENDIF
         END DO


! Step 2: Clean-up list of used points and store cleaned connectivity
         POINT_NEW_ID = -1
         NEW_POINT_NEW_ID = -1
         POINT_ID = 1
! This is for the background grid cell corners
         DO IJK = 1,IJKMAX3
            IF(KEEP_POINT(IJK)) THEN
               POINT_NEW_ID(IJK) = POINT_ID
               POINT_ID = POINT_ID + 1
            ENDIF
         END DO
! This is for the cut cell new corners
         DO IJK = 1,GLOBAL_NUMBER_OF_NEW_POINTS
            IF(KEEP_NEW_POINT(IJK)) THEN
               NEW_POINT_NEW_ID(IJK) = POINT_ID
               POINT_ID = POINT_ID + 1
            ENDIF
         END DO

! Update the true (clean) number of points
         NUMBER_OF_POINTS = POINT_ID - 1

! Now, store a list of coordinates for all used points
         IF(ALLOCATED(GLOBAL_COORDS_OF_POINTS)) DEALLOCATE(GLOBAL_COORDS_OF_POINTS)

         ALLOCATE(GLOBAL_COORDS_OF_POINTS(3,NUMBER_OF_POINTS))

         POINT_ID = 1
! This is for the background grid cell corners
         IF(COORDINATES=='CYLINDRICAL') THEN
            DO IJK = 1,IJKMAX3
               IF(KEEP_POINT(IJK)) THEN
                  XCYL = XG_E(GLOBAL_I_OF(IJK)) * DCOS(ZG_T(GLOBAL_K_OF(IJK)))
                  ZCYL = XG_E(GLOBAL_I_OF(IJK)) * DSIN(ZG_T(GLOBAL_K_OF(IJK)))
                  GLOBAL_COORDS_OF_POINTS(1:3,POINT_ID) = &
                       (/REAL(XCYL),REAL(YG_N(GLOBAL_J_OF(IJK))),REAL(ZCYL)/)
                  POINT_ID = POINT_ID + 1
               ENDIF
            END DO
         ELSE ! CARTESIAN COORDINATE SYSTEM
            DO IJK = 1,IJKMAX3
               IF(KEEP_POINT(IJK)) THEN
                  GLOBAL_COORDS_OF_POINTS(1:3,POINT_ID) = &
                       (/REAL(XG_E(GLOBAL_I_OF(IJK))),REAL(YG_N(GLOBAL_J_OF(IJK))),REAL(ZG_T(GLOBAL_K_OF(IJK)))/)
                  POINT_ID = POINT_ID + 1
               ENDIF
            END DO
         ENDIF
! This is for the cut cell new corners
         DO IJK = 1,GLOBAL_NUMBER_OF_NEW_POINTS
            IF(KEEP_NEW_POINT(IJK)) THEN
               NEW_POINT_NEW_ID(IJK) = POINT_ID
               GLOBAL_COORDS_OF_POINTS(1:3,POINT_ID) = &
                    (/REAL(GLOBAL_X_NEW_POINT(IJK)),REAL(GLOBAL_Y_NEW_POINT(IJK)),REAL(GLOBAL_Z_NEW_POINT(IJK))/)
               POINT_ID = POINT_ID + 1
            ENDIF
         END DO


! Step 3: Shift connectivity with new point indices
         DO IJK = 1,IJKMAX3
            IF (BELONGS_TO_VTK_SUBDOMAIN(IJK)) THEN
               DO L=1,GLOBAL_NUMBER_OF_NODES(IJK)
                  IF(GLOBAL_CONNECTIVITY(IJK,L)<=IJKMAX3) THEN
                     GLOBAL_CLEANED_CONNECTIVITY(IJK,L) = POINT_NEW_ID(GLOBAL_CONNECTIVITY(IJK,L))
                  ELSE
                     GLOBAL_CLEANED_CONNECTIVITY(IJK,L) = NEW_POINT_NEW_ID(GLOBAL_CONNECTIVITY(IJK,L)-IJKMAX3)
                  ENDIF
               ENDDO
            ENDIF
         END DO



       ELSEIF(BDIST_IO) THEN


          IF(ALLOCATED(CLEANED_CONNECTIVITY))  DEALLOCATE (CLEANED_CONNECTIVITY)
          IF(ALLOCATED(KEEP_NEW_POINT))        DEALLOCATE (KEEP_NEW_POINT)
          IF(ALLOCATED(POINT_NEW_ID))          DEALLOCATE (POINT_NEW_ID)
          IF(ALLOCATED(NEW_POINT_NEW_ID))      DEALLOCATE (NEW_POINT_NEW_ID)
          IF(ALLOCATED(KEEP_POINT))            DEALLOCATE (KEEP_POINT)

          ALLOCATE (CLEANED_CONNECTIVITY(IJKEND3,15))
          ALLOCATE (KEEP_NEW_POINT(NUMBER_OF_NEW_POINTS))

          ALLOCATE (POINT_NEW_ID(IJKEND3))
          ALLOCATE (NEW_POINT_NEW_ID(IJKEND3))
          ALLOCATE (KEEP_POINT(IJKEND3))

! Step 1: Go through connectivity list and only keep points that are used.
!         For background cell corners, assign KEEP_POINT = .TRUE.
!         For cut cells, the new intersection points were called NEW_POINTS,
!         so assign KEEP_NEW_POINT = .TRUE.
!         A NEW_POINT had an IJK index larger than IJKMAX3

          KEEP_POINT = .FALSE.
          KEEP_NEW_POINT = .FALSE.

          DO IJK = 1,IJKEND3
             IF (BELONGS_TO_VTK_SUBDOMAIN(IJK)) THEN
                DO L=1,NUMBER_OF_NODES(IJK)
                   IJKC = CONNECTIVITY(IJK,L)
                   IF(IJKC<=IJKEND3) KEEP_POINT(IJKC) = .TRUE.
                   IF(IJKC>IJKEND3) KEEP_NEW_POINT(IJKC-IJKEND3) = .TRUE.
                ENDDO
             ENDIF
          END DO


! Step 2: Clean-up list of used points and store cleaned connectivity
          POINT_NEW_ID = -1
          NEW_POINT_NEW_ID = -1
          POINT_ID = 1
! This is for the background grid cell corners
          DO IJK = 1,IJKEND3
             IF(KEEP_POINT(IJK)) THEN
                POINT_NEW_ID(IJK) = POINT_ID
                POINT_ID = POINT_ID + 1
             ENDIF
          END DO
! This is for the cut cell new corners
          DO IJK = 1,NUMBER_OF_NEW_POINTS
             IF(KEEP_NEW_POINT(IJK)) THEN
                NEW_POINT_NEW_ID(IJK) = POINT_ID
                POINT_ID = POINT_ID + 1
             ENDIF
          END DO

! Update the true (clean) number of points
          NUMBER_OF_POINTS = POINT_ID - 1

! Now, store a list of coordinates for all used points
          IF(ALLOCATED(COORDS_OF_POINTS)) DEALLOCATE(COORDS_OF_POINTS)

          ALLOCATE(COORDS_OF_POINTS(NUMBER_OF_POINTS,3))

          POINT_ID = 1
! This is for the background grid cell corners
          DO IJK = 1,IJKEND3
             IF(KEEP_POINT(IJK)) THEN
                COORDS_OF_POINTS(POINT_ID,1:3) = &
                     (/REAL(XG_E(I_OF(IJK))),REAL(YG_N(J_OF(IJK))),REAL(ZG_T(K_OF(IJK)))/)
                POINT_ID = POINT_ID + 1
             ENDIF
          END DO
! This is for the cut cell new corners
          DO IJK = 1,NUMBER_OF_NEW_POINTS
             IF(KEEP_NEW_POINT(IJK)) THEN
                NEW_POINT_NEW_ID(IJK) = POINT_ID
                COORDS_OF_POINTS(POINT_ID,1:3) = &
                     (/REAL(X_NEW_POINT(IJK)),REAL(Y_NEW_POINT(IJK)),REAL(Z_NEW_POINT(IJK))/)
                POINT_ID = POINT_ID + 1
             ENDIF
          END DO


! Step 3: Shift connectivity with new point indices
          DO IJK = 1,IJKEND3
             IF (BELONGS_TO_VTK_SUBDOMAIN(IJK)) THEN
                DO L=1,NUMBER_OF_NODES(IJK)
                   IF(CONNECTIVITY(IJK,L)<=IJKEND3) THEN
                      CLEANED_CONNECTIVITY(IJK,L) = POINT_NEW_ID(CONNECTIVITY(IJK,L))
                   ELSE
                      CLEANED_CONNECTIVITY(IJK,L) = NEW_POINT_NEW_ID(CONNECTIVITY(IJK,L)-IJKEND3)
                   ENDIF
                ENDDO
             ENDIF
          END DO

       ENDIF
!      print*,'bottom of clean geo, NUMBER_OF_POINTS=',NUMBER_OF_POINTS

      RETURN

      END SUBROUTINE CLEAN_GEOMETRY

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: SETUP_VTK_REGION                                       C
!                                                                      C
!  Purpose: Filter the cells based on the VTK region bounds and        C
!           set the flag BELONGS_TO_VTK_SUBDOMAIN(IJK) to .TRUE.       C
!           to keep the cell.                                          C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 19-Dec-14  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SETUP_VTK_REGION

      IMPLICIT NONE

      INTEGER :: IJK,I,J,K,I_E,I_W,J_N,J_S,K_T,K_B
      INTEGER :: NXS,NYS,NZS,NS,I_TMP,J_TMP,K_TMP
      INTEGER :: I_SLICE(DIM_I),J_SLICE(DIM_J),K_SLICE(DIM_K)
      DOUBLE PRECISION :: XE,XW,YS,YN,ZB,ZT
      DOUBLE PRECISION :: XSLICE,YSLICE,ZSLICE
      LOGICAL :: KEEP_XDIR,KEEP_YDIR,KEEP_ZDIR
      INTEGER :: NG
      CHARACTER(LEN=20) :: UC_FILENAME ! Upper case filename


! Exit if saving geometry or domain decomposition
      IF(VTK_DATA(VTK_REGION)=='G') RETURN

      IF(VTK_DOMAIN_DECOMPOSITION(VTK_REGION)) THEN
         NUMBER_OF_VTK_CELLS = 0
         RETURN
      ENDIF

! Get VTK region bounds
      XE = VTK_X_E(VTK_REGION)
      XW = VTK_X_W(VTK_REGION)
      YS = VTK_Y_S(VTK_REGION)
      YN = VTK_Y_N(VTK_REGION)
      ZB = VTK_Z_B(VTK_REGION)
      ZT = VTK_Z_T(VTK_REGION)

      NXS = VTK_NXS(VTK_REGION)
      NYS = VTK_NYS(VTK_REGION)
      NZS = VTK_NZS(VTK_REGION)

      CALL CALC_CELL (X_MIN, VTK_X_W(VTK_REGION), DX, IMAX, I_W)
      I_W = I_W !+ 1
      CALL CALC_CELL (X_MIN, VTK_X_E(VTK_REGION), DX, IMAX, I_E)


      CALL CALC_CELL (Y_MIN, VTK_Y_S(VTK_REGION), DY, JMAX, J_S)
      J_S = J_S !+ 1
      CALL CALC_CELL (Y_MIN, VTK_Y_N(VTK_REGION), DY, JMAX, J_N)

      IF (NO_K) THEN
         K_B = 1
         K_T = 1
      ELSE
         CALL CALC_CELL (Z_MIN, VTK_Z_B(VTK_REGION), DZ, KMAX, K_B)
         K_B = K_B !+ 1
         CALL CALC_CELL (Z_MIN, VTK_Z_T(VTK_REGION), DZ, KMAX, K_T)
      ENDIF

! get slice(s) location
      DO NS = 1,NXS
         XSLICE = XW + (XE-XW)/(NXS-1)*(NS-1)
         CALL CALC_CELL (X_MIN, XSLICE, DX, IMAX, I_TMP)
         I_SLICE(NS) = MAX(MIN(I_TMP,IMAX1),IMIN1)
      ENDDO

      DO NS = 1,NYS
         YSLICE = YS + (YN-YS)/(NYS-1)*(NS-1)
         CALL CALC_CELL (Y_MIN, YSLICE, DY, JMAX, J_TMP)
         J_SLICE(NS) = MAX(MIN(J_TMP,JMAX1),JMIN1)
      ENDDO

      DO NS = 1,NZS
         ZSLICE = ZB + (ZT-ZB)/(NZS-1)*(NS-1)
         CALL CALC_CELL (Z_MIN, ZSLICE, DZ, KMAX, K_TMP)
         K_SLICE(NS) = MAX(MIN(K_TMP,KMAX1),KMIN1)
      ENDDO

      IF (myPE == PE_IO.AND.(.NOT.BDIST_IO)) THEN

         IF(ALLOCATED(BELONGS_TO_VTK_SUBDOMAIN)) DEALLOCATE(BELONGS_TO_VTK_SUBDOMAIN)

         ALLOCATE (BELONGS_TO_VTK_SUBDOMAIN(ijkmax3))

! Filter the cells based on the VTK region bounds and set the
! flag BELONGS_TO_VTK_SUBDOMAIN(IJK) to .TRUE. to keep the cell.

         BELONGS_TO_VTK_SUBDOMAIN = .FALSE.
         NUMBER_OF_VTK_CELLS      = 0

         DO IJK = 1,IJKMAX3
            IF (GLOBAL_INTERIOR_CELL_AT(IJK))      THEN
               IF (.NOT.GLOBAL_BLOCKED_CELL_AT(IJK)) THEN
                  I = GLOBAL_I_OF(IJK)
                  J = GLOBAL_J_OF(IJK)
                  K = GLOBAL_K_OF(IJK)

                  IF(VTK_CUTCELL_ONLY(VTK_REGION)) THEN
                     IF(I==IMIN1.OR.I==IMAX1.OR. &
                        J==JMIN1.OR.J==JMAX1.OR. &
                        K==KMIN1.OR.K==KMAX1.OR. &
                        GLOBAL_CUT_CELL_AT(IJK)) THEN

                        BELONGS_TO_VTK_SUBDOMAIN(IJK) = .TRUE.
                        NUMBER_OF_VTK_CELLS = NUMBER_OF_VTK_CELLS + 1
                     ENDIF
                     CYCLE
                  ENDIF


! X-direction
                  KEEP_XDIR=.FALSE.
                  IF(NXS==0) THEN
                     IF(I_W<=I.AND.I<=I_E) KEEP_XDIR=.TRUE.
                  ELSE
                     DO NS = 1,NXS
                        IF(I==I_SLICE(NS)) KEEP_XDIR=.TRUE.
                     ENDDO
                  ENDIF

! Y-direction
                  KEEP_YDIR=.FALSE.
                  IF(NYS==0) THEN
                     IF(J_S<=J.AND.J<=J_N) KEEP_YDIR=.TRUE.
                  ELSE
                     DO NS = 1,NYS
                        IF(J==J_SLICE(NS)) KEEP_YDIR=.TRUE.
                     ENDDO
                  ENDIF

! Z-direction
                  KEEP_ZDIR=.FALSE.
                  IF(NZS==0) THEN
                     IF(K_B<=K.AND.K<=K_T) KEEP_ZDIR=.TRUE.
                  ELSE
                     DO NS = 1,NZS
                        IF(K==K_SLICE(NS)) KEEP_ZDIR=.TRUE.
                     ENDDO
                  ENDIF

! Now combine
                  IF(KEEP_XDIR.AND.KEEP_YDIR.AND.KEEP_ZDIR) THEN
                     BELONGS_TO_VTK_SUBDOMAIN(IJK) = .TRUE.
                     NUMBER_OF_VTK_CELLS = NUMBER_OF_VTK_CELLS + 1
                  ENDIF
               ENDIF
            ENDIF
         END DO

      ELSE  ! BDIST_IO

         IF(ALLOCATED(BELONGS_TO_VTK_SUBDOMAIN)) DEALLOCATE(BELONGS_TO_VTK_SUBDOMAIN)

         ALLOCATE (BELONGS_TO_VTK_SUBDOMAIN(ijkend3))

! Filter the cells based on the VTK region bounds and set the
! flag BELONGS_TO_VTK_SUBDOMAIN(IJK) to .TRUE. to keep the cell.

         BELONGS_TO_VTK_SUBDOMAIN = .FALSE.
         NUMBER_OF_VTK_CELLS      = 0

         DO IJK = 1,IJKEND3
            IF (INTERIOR_CELL_AT(IJK))      THEN
               IF (.NOT.BLOCKED_CELL_AT(IJK)) THEN
                  I = I_OF(IJK)
                  J = J_OF(IJK)
                  K = K_OF(IJK)

! X-direction
                  KEEP_XDIR=.FALSE.
                  IF(NXS==0) THEN
                     IF(I_W<=I.AND.I<=I_E) KEEP_XDIR=.TRUE.
                  ELSE
                     DO NS = 1,NXS
                        IF(I==I_SLICE(NS)) KEEP_XDIR=.TRUE.
                     ENDDO
                  ENDIF

! Y-direction
                  KEEP_YDIR=.FALSE.
                  IF(NYS==0) THEN
                     IF(J_S<=J.AND.J<=J_N) KEEP_YDIR=.TRUE.
                  ELSE
                     DO NS = 1,NYS
                        IF(J==J_SLICE(NS)) KEEP_YDIR=.TRUE.
                     ENDDO
                  ENDIF

! Z-direction
                  KEEP_ZDIR=.FALSE.
                  IF(NZS==0) THEN
                     IF(K_B<=K.AND.K<=K_T) KEEP_ZDIR=.TRUE.
                  ELSE
                     DO NS = 1,NZS
                        IF(K==K_SLICE(NS)) KEEP_ZDIR=.TRUE.
                     ENDDO
                  ENDIF

! Now combine
                  IF(KEEP_XDIR.AND.KEEP_YDIR.AND.KEEP_ZDIR) THEN
                     BELONGS_TO_VTK_SUBDOMAIN(IJK) = .TRUE.
                     NUMBER_OF_VTK_CELLS = NUMBER_OF_VTK_CELLS + 1
                  ENDIF
               ENDIF
            ENDIF
         END DO

      ENDIF

      RETURN

   END SUBROUTINE SETUP_VTK_REGION

END MODULE vtk_out_mod
