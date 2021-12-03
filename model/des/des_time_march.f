#include "error.inc"

MODULE DES_TIME_MARCH

      use calc_collision_wall, only: calc_dem_thermo_with_wall_stl
      use calc_drag_des_mod, only: calc_drag_des
      use calc_force_dem_mod, only: calc_force_dem
      use calc_interp_weights_mod, only: calc_interp_weights
      use calc_pg_grad_mod, only: calc_pg_grad
      use calc_rrates_des_mod, only: zero_rrate_des
      use calc_thermo_des_mod, only: calc_thermo_des
      use cfnewvalues_mod, only: cfnewvalues
      use cfupdateold_mod, only: cfupdateold
      use comp_mean_fields_mod, only: comp_mean_fields
      use compar, only: ADJUST_PARTITION
      use conv_gs_des1_mod, only: zero_energy_source, conv_gs_des1
      use des_bc, only: DEM_BCMI, DEM_BCMO
      use des_functions_mod, only: des_sort_particles_spatially, des_sort_particle_arrays, des_getvalid_fluid_cells
      use des_radiation_mod, only: calc_avgts
      use des_reaction_model_mod, only: des_reaction_model
      use des_thermo, only: CALC_RADT_DES
      use des_thermo_newvalues_mod, only: des_thermo_newvalues
      use desgrid, only: desgrid_pic
      use CHECK_SOLIDS_DEM_MOD, only: update_dtsolid
      use discretelement
      use drag_gs_des1_mod, only: drag_gs_des1
      use error_manager
      use mass_inflow_dem_mod, only: mass_inflow_dem
      use mass_outflow_dem_mod, only: mass_outflow_dem
      use mpi_funs_des, only: DESMPI_SEND_RECV_FIELD_VARS
      use mpi_funs_des, only: DES_PAR_EXCHANGE
      use mpi_utility
      use neighbour_mod, only: neighbour
      use output, only: DLB,DLB_TIME
      use output_man, only: OUTPUT_MANAGER
      use particles_in_cell_mod, only: particles_in_cell
      use run , only: optflag1
      use run, only: ANY_SPECIES_EQ
      use run, only: CALL_USR
      use run, only: ENERGY_EQ
      use run, only: NSTEP
      use run, only: TIME, TSTOP, DT
      use sendrecv
      use time_cpu, only: WALL_TIME

!---------------------------------------------------------------------//
! Total number of particles
      INTEGER, SAVE :: NP=0

! loop counter index for any initial particle settling incoupled cases
      INTEGER :: FACTOR
! Temporary variables when des_continuum_coupled is T to track
! changes in solid time step
      DOUBLE PRECISION :: DTSOLID_TMP
! Numbers to calculate wall time spent in DEM calculations.
      DOUBLE PRECISION :: TMP_WALL

      LOGICAL :: EXIT_LOOP

!......................................................................!

   CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!     Subroutine: DES_TIME_INIT                                        !
!     Author: Jay Boyalakuntla                        Date: 21-Jun-04  !
!                                                                      !
!     Purpose: Main DEM driver routine                                 !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE DES_TIME_INIT

         IMPLICIT NONE

         EXIT_LOOP = .FALSE.

! In case of restarts assign S_TIME from MFIX TIME
      S_TIME = TIME

! JFD: Update DTSOLID
      CALL UPDATE_DTSOLID

      DTSOLID_TMP = DTSOLID
      TMP_WALL = WALL_TIME()

! Initialize time stepping variables for coupled gas/solids simulations.
      IF(DES_CONTINUUM_COUPLED) THEN
         IF(DT.GE.DTSOLID) THEN
            FACTOR = CEILING(real(DT/DTSOLID))
         ELSE
            FACTOR = 1
            DTSOLID = DT
         ENDIF

! Initialize time stepping variable for pure granular simulations.
      ELSE
         FACTOR = CEILING(real((TSTOP-TIME)/DTSOLID))
         DT = DTSOLID
         CALL OUTPUT_MANAGER(.FALSE., .FALSE.)
      ENDIF   ! end if/else (des_continuum_coupled)

      NP = PIP - IGHOST_CNT
      CALL GLOBAL_ALL_SUM(NP)

      IF(DES_CONTINUUM_COUPLED) THEN
         WRITE(ERR_MSG, 1000) trim(iVal(factor)), trim(iVAL(NP))
         CALL LOG_STATUS()
      ELSE
         WRITE(ERR_MSG, 1100) TIME, DTSOLID, trim(iVal(factor))
         CALL LOG_STATUS()
      ENDIF
 1000 FORMAT(/'DEM NITs: ',A,3x,'Total PIP: ', A)
 1100 FORMAT(/'Time: ',g12.5,3x,'DT: ',g12.5,3x,'DEM NITs: ',A)

      IF(CALL_USR) CALL USR0_DES

      IF(DES_CONTINUUM_COUPLED) THEN
         IF(DES_EXPLICITLY_COUPLED) THEN
            CALL DRAG_GS_DES1
            IF(ENERGY_EQ) CALL CONV_GS_DES1
            IF(ANY_SPECIES_EQ) CALL DES_REACTION_MODEL
         ELSE
            IF(ANY_SPECIES_EQ) CALL ZERO_RRATE_DES
            IF(ENERGY_EQ) CALL ZERO_ENERGY_SOURCE
         ENDIF
         CALL CALC_PG_GRAD
      ENDIF

      IF(any(CALC_RADT_DES)) CALL CALC_avgTs

      !Hari Sitaraman	(particle sorting)===================
      if (optflag1.eq.1) then
        print *,"call spatial sort"
        CALL DES_SORT_PARTICLES_SPATIALLY()
      endif
      !======================================================

   END SUBROUTINE DES_TIME_INIT

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!     Subroutine: DES_TIME_STEP                                        !
!     Author: Jay Boyalakuntla                        Date: 21-Jun-04  !
!                                                                      !
!     Purpose: Main DEM driver routine                                 !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE DES_TIME_STEP(NN)

! Modules
!---------------------------------------------------------------------//
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NN
      logical :: mod_assertion
      mod_assertion = (NN == 1 .OR. MOD(NN,NEIGHBOR_SEARCH_N) == 0)


      ! print*,'NN,DTSOLID=',NN,DTSOLID

         IF(DES_CONTINUUM_COUPLED) THEN
! If the current time in the discrete loop exceeds the current time in
! the continuum simulation, exit the discrete loop
            IF(S_TIME.GE.(TIME+DT)) THEN
               EXIT_LOOP = .TRUE.
               RETURN
            ENDIF
! If next time step in the discrete loop will exceed the current time
! in the continuum simulation, modify the discrete time step so final
! time will match
            IF((S_TIME+DTSOLID).GT.(TIME+DT)) &
               DTSOLID = TIME + DT - S_TIME
         ENDIF

 ! NREL_CU_OPT sorting,swappping routines
if (optflag1.eq.1) then
    CALL DES_SORT_PARTICLE_ARRAYS
    CALL DES_GETVALID_FLUID_CELLS
endif

! Calculate inter particle forces acting (collisional, cohesion)
         CALL CALC_FORCE_DEM
! Calculate or distribute fluid-particle drag force.
         CALL CALC_DRAG_DES
! Calculate heat conduction to/from wall
         IF(ENERGY_EQ)CALL CALC_DEM_THERMO_WITH_WALL_STL

! Update the old values of particle position and velocity with the new
! values computed
         IF (DO_OLD) CALL CFUPDATEOLD
! Calculate thermochemical sources (energy and  rates of formation).
         CALL CALC_THERMO_DES
! Call user functions.

         IF(CALL_USR) CALL USR1_DES

! Update position and velocities
         CALL CFNEWVALUES

! Update particle temperatures
         CALL DES_THERMO_NEWVALUES

         DO_NSEARCH = (NN == 1 .OR. MOD(NN,NEIGHBOR_SEARCH_N) == 0)
         if (DO_NSEARCH .neqv. mod_assertion) THEN
            WRITE(ERR_MSG, *) "Failed assertion; problem in neighbor search algorithm"
            CALL LOG_ERROR()
            ENDIF
! Add/Remove particles to the system via flow BCs.
         IF(DEM_BCMI > 0) CALL MASS_INFLOW_DEM(DTSOLID_TMP)
         IF(DEM_BCMO > 0) CALL MASS_OUTFLOW_DEM(DO_NSEARCH)

! Call exchange particles - this will exchange particle crossing
! boundaries as well as updates ghost particles information
         IF (DO_NSEARCH .OR. (numPEs>1) .OR. DES_PERIODIC_WALLS) THEN
            CALL DESGRID_PIC(.TRUE.)
            CALL DES_PAR_EXCHANGE
         ENDIF

         IF(DO_NSEARCH) CALL NEIGHBOUR

! Explicitly coupled simulations do not need to rebin particles to
! the fluid grid every time step. However, this implies that the
! fluid cell information and interpolation weights become stale.
         IF(DES_CONTINUUM_COUPLED .AND. &
            .NOT.DES_EXPLICITLY_COUPLED) THEN
! Bin particles to fluid grid.
            CALL PARTICLES_IN_CELL
! Calculate interpolation weights
            CALL CALC_INTERP_WEIGHTS
! Calculate mean fields (EPg).
            CALL COMP_MEAN_FIELDS
         ENDIF

! Update time to reflect changes
         S_TIME = S_TIME + DTSOLID

! The following section targets data writes for DEM only cases:
         IF(.NOT.DES_CONTINUUM_COUPLED) THEN
! Keep track of TIME and number of steps for DEM simulations
            TIME = S_TIME
            NSTEP = NSTEP + 1
! Call the output manager to write RES and SPx data.
            DLB = .TRUE.
            CALL OUTPUT_MANAGER(.FALSE., .FALSE.)
         ENDIF  ! end if (.not.des_continuum_coupled)

         IF(CALL_USR) CALL USR2_DES

         IF(ADJUST_PARTITION) THEN
            EXIT_LOOP = .TRUE.
            RETURN
         ENDIF

   END SUBROUTINE DES_TIME_STEP

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!     Subroutine: DES_TIME_END                                         !
!     Author: Jay Boyalakuntla                        Date: 21-Jun-04  !
!                                                                      !
!     Purpose: Main DEM driver routine                                 !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE DES_TIME_END

      IMPLICIT NONE

      IF(CALL_USR) CALL USR3_DES

! Reset the discrete time step to original value.
      DTSOLID = DTSOLID_TMP

      IF(DES_CONTINUUM_COUPLED) CALL DESMPI_SEND_RECV_FIELD_VARS

      TMP_WALL = WALL_TIME() - TMP_WALL
      IF(TMP_WALL > 1.0d-10) THEN
         WRITE(ERR_MSG, 9000) trim(iVal(dble(FACTOR)/TMP_WALL))
      ELSE
         WRITE(ERR_MSG, 9000) '+Inf'
      ENDIF
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

 9000 FORMAT('    NITs/SEC = ',A)

      RETURN

   END SUBROUTINE DES_TIME_END

END MODULE DES_TIME_MARCH
