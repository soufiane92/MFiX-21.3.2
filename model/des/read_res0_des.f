MODULE READ_RES0_DES_MOD

   use des_allocate, only: allocate_dem_mi
   use des_bc, only: dem_mi, dem_bcmi, dem_mi_time
   use des_rxns, only: des_x_s
   use des_thermo, only: des_t_s
   use discretelement
   use error_manager
   use geometry, only: no_k
   use mfix_pic, only: MPPIC, DES_STAT_WT
   use param, only: dimension_n_s
   use read_res1_des, only: init_read_res_des, finl_read_res_des, read_par_col, read_par_pos
   use read_res1_des, only: read_res_carray, read_res_des, read_res_parray
   use run, only: energy_eq, run_name, run_type, time, any_species_eq

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: DES_READ_RESTART                                        !
!  Purpose : Reads either single restart file or multiple restart      !
!  fles (based on bdist_io) flag.                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE READ_RES0_DES

      implicit none

      INTEGER :: LC1, LC2
      INTEGER :: lDIMN, lNEXT_REC

      INTEGER :: lVAR_SIZE
      DOUBLE PRECISION :: VERSION

      lDIMN = merge(2,3,NO_K)

      CALL INIT_READ_RES_DES(trim(RUN_NAME), VERSION, lNEXT_REC)

      CALL READ_RES_DES(lNEXT_REC, VTP_FINDEX)
      CALL READ_RES_DES(lNEXT_REC, TECPLOT_FINDEX)
      CALL READ_RES_DES(lNEXT_REC, DTSOLID)

! Position data is read and used to setup pARRAY reads.
      CALL READ_PAR_POS(lNEXT_REC)

      CALL READ_RES_pARRAY(lNEXT_REC, iGLOBAL_ID)

      CALL READ_RES_pARRAY(lNEXT_REC, particle_state)

      DO LC1 = 1, lDIMN
         CALL READ_RES_pARRAY(lNEXT_REC, DES_VEL_NEW(:,LC1))
      ENDDO

      DO LC1 = 1, merge(1,3,NO_K)
         CALL READ_RES_pARRAY(lNEXT_REC, OMEGA_NEW(:,LC1))
      ENDDO

      CALL READ_RES_pARRAY(lNEXT_REC, DES_RADIUS)
      CALL READ_RES_pARRAY(lNEXT_REC, RO_SOL)

      IF(MPPIC) CALL READ_RES_pARRAY(lNEXT_REC, DES_STAT_WT)
      IF(CGDEM) CALL READ_RES_pARRAY(lNEXT_REC, DES_CGP_STW)
      IF(CGDEM) CALL READ_RES_pARRAY(lNEXT_REC, DES_CGP_RPR)
      IF(ENERGY_EQ) CALL READ_RES_pARRAY(lNEXT_REC, DES_T_s)

      IF(VERSION >= 1.2) THEN
         CALL READ_RES_pARRAY(lNEXT_REC, PIJK(:,5))
         IF(allocated(DES_X_s)) THEN
            DO LC1=1, DIMENSION_N_S
               CALL READ_RES_pARRAY(lNEXT_REC, DES_X_s(:,LC1))
            ENDDO
         ENDIF
      ELSE
         IF(ANY_SPECIES_EQ) THEN
            CALL READ_RES_pARRAY(lNEXT_REC, PIJK(:,5))
            DO LC1=1, DIMENSION_N_S
               CALL READ_RES_pARRAY(lNEXT_REC, DES_X_s(:,LC1))
            ENDDO
         ENDIF
      ENDIF

      IF(VERSION >= 1.1) THEN
         CALL READ_RES_DES(lNEXT_REC, lVAR_SIZE)
         DO LC1=1, lVAR_SIZE
            if(lVAR_SIZE <= DES_USR_VAR_SIZE) &
            CALL READ_RES_pARRAY(lNEXT_REC, DES_USR_VAR(LC1,:))
         ENDDO
      ENDIF

! Residence time
      CALL READ_RES_pARRAY(lNEXT_REC, RESIDENCE_TIME)

! RES2 does not need the collision of BC information.
      IF(RUN_TYPE == 'RESTART_2') RETURN

! Collision/neighbor data is read and used to setup cARRAY reads.
      IF(.NOT.MPPIC) THEN
         CALL READ_PAR_COL(lNEXT_REC)
         DO LC1=1, lDIMN
            CALL READ_RES_cARRAY(lNEXT_REC, PFT_NEIGHBOR(LC1,:))
         ENDDO
      ENDIF

! Save the number of BCMI's read from input file, then read the
! value from the restart file.
      CALL READ_RES_DES(lNEXT_REC, DEM_BCMI)

! Allocation of MIs is done here to ignore keyword changes during RES1.
      IF(DEM_BCMI > 0) CALL ALLOCATE_DEM_MI

! Only save the number of mass inflows for RESTART_1. This allows
! for mass inflows to be added/removed with RESTART_2.
! Todo: Prune entering/exiting flagged particles for RESTART_2.
      DO LC1=1, DEM_BCMI
         CALL READ_RES_DES(lNEXT_REC, DEM_MI_TIME(LC1))
         CALL READ_RES_DES(lNEXT_REC, DEM_MI(LC1)%VACANCY)
         CALL READ_RES_DES(lNEXT_REC, DEM_MI(LC1)%OCCUPANTS)
         CALL READ_RES_DES(lNEXT_REC, DEM_MI(LC1)%WINDOW)
         CALL READ_RES_DES(lNEXT_REC, DEM_MI(LC1)%OFFSET)
         CALL READ_RES_DES(lNEXT_REC, DEM_MI(LC1)%L)

         LC2 = DEM_MI(LC1)%OCCUPANTS

         allocate(DEM_MI(LC1)%W(LC2))
         CALL READ_RES_DES(lNEXT_REC, DEM_MI(LC1)%W(:))
         allocate(DEM_MI(LC1)%H(LC2))
         CALL READ_RES_DES(lNEXT_REC, DEM_MI(LC1)%H(:))
         allocate(DEM_MI(LC1)%P(LC2))
         CALL READ_RES_DES(lNEXT_REC, DEM_MI(LC1)%P(:))
         allocate(DEM_MI(LC1)%Q(LC2))
         CALL READ_RES_DES(lNEXT_REC, DEM_MI(LC1)%Q(:))
      ENDDO

      CALL FINL_READ_RES_DES


      WRITE(ERR_MSG,"('DES restart file read at Time = ',g12.5)") TIME
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

      RETURN

   END SUBROUTINE READ_RES0_DES

END MODULE READ_RES0_DES_MOD
