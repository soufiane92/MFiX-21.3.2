#include "version.inc"

MODULE PIC_TIME_MARCH_MOD

! Subroutines
!---------------------------------------------------------------------//
   use calc_drag_des_mod, only: calc_drag_des
   use calc_interp_weights_mod, only: calc_interp_weights
   use calc_pg_grad_mod, only: calc_pg_grad
   use calc_ps_pic_mod,  only: calc_ps_pic
   use calc_rrates_des_mod, only: zero_rrate_des
   use calc_thermo_des_mod, only: calc_thermo_des
   use comp_mean_fields_mod, only: comp_mean_fields
   use conv_gs_des1_mod, only: zero_energy_source, conv_gs_des1
   use des_reaction_model_mod, only: des_reaction_model
   use des_thermo_newvalues_mod, only: des_thermo_newvalues
   use drag_gs_des1_mod, only: drag_gs_des1
   use error_manager
   use mpi_funs_pic, only: pic_par_exchange
   use mpi_utility, only: global_sum
   use output_man, only: output_manager
   use particles_in_cell_mod, only: particles_in_cell
   use pic_flow_bcs,   only: mass_inflow_pic
   use pic_wall_bcs,   only: apply_wall_bc_pic

! Global variables
!---------------------------------------------------------------------//
! Fluid time, simulation end time, time step size, number of time steps
   use run, only: TIME, TSTOP, DT, NSTEP
! Discrete particle time, time step size
   use discretelement, only: S_TIME, DTSOLID
! Local particle count
   use discretelement, only: PIP
! Flag: Coupled fluid-solids simulation
   use discretelement, only: DES_CONTINUUM_COUPLED
! Flag: Call user defined subroutines
   use run, only: CALL_USR
! Flag: Explicitly coupled gas-solids drag
   use discretelement, only: DES_EXPLICITLY_COUPLED
! Number of mass inflows
   use mfix_pic, only: PIC_BCMI
! Incorporate energy
   use run, only: ENERGY_EQ
! Incorporate species
   use run, only: ANY_SPECIES_EQ

! Global number of parcels.
   INTEGER :: gPIP

!---------------------------------------------------------------------//
! time till which the PIC loop will be run
   double precision :: TEND_PIC_LOOP
   double precision, save :: DTSOLID_CFL = 1.0d0

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!     Subroutine: PIC_TIME_INIT                                        !
!     Author: Mark Meredith                          Date: 2019-10-04  !
!                                                                      !
!     Purpose: Initialize PIC time march variables                     !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE PIC_TIME_INIT
      implicit none

! Set solids time to fluid time.
      S_TIME = TIME
      DTSOLID = DT

      IF(CALL_USR) CALL USR0_DES

! Compute the gas-phase pressure gradient
      IF(DES_CONTINUUM_COUPLED) THEN

         TEND_PIC_LOOP = TIME+DT

! Explicitly coupled evaluates sources and integrates *some* variables:
         IF(DES_EXPLICITLY_COUPLED) THEN
! Calculate drag source: [beta*(Ug-Up)]
            CALL DRAG_GS_DES1
! Calculate convection source [gamma*(Tg-Tp)]
            IF(ENERGY_EQ) CALL CONV_GS_DES1
! Integrate particle mass / species mass equations.
            IF(ANY_SPECIES_EQ) CALL DES_REACTION_MODEL
         ELSE
            IF(ANY_SPECIES_EQ) CALL ZERO_RRATE_DES
            IF(ENERGY_EQ) CALL ZERO_ENERGY_SOURCE
         ENDIF
         CALL CALC_PG_GRAD

      else
         TEND_PIC_LOOP = TSTOP
      ENDIF

   END SUBROUTINE PIC_TIME_INIT


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!     Subroutine: PIC_TIME_STEP                                        !
!     Author: Mark Meredith                          Date: 2019-10-04  !
!                                                                      !
!     Purpose: Do one PIC time step                                    !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE PIC_TIME_STEP(PIC_ITERS)
     implicit none

      INTEGER, intent(in)::PIC_ITERS

! If next time step in the discrete loop will exceed the current time
! in the continuum simulation, modify the discrete time step so final
! time will match
      IF(S_TIME + DTSOLID > TEND_PIC_LOOP) &
         DTSOLID = TEND_PIC_LOOP - S_TIME

! This is how you step through the PIC logic
! Calculate the solids pressure
      CALL CALC_PS_PIC
      CALL INTERPOLATE_PIC

      IF(DES_CONTINUUM_COUPLED) CALL CALC_DRAG_DES

! Calculate thermochemical sources (energy and rates of formation)
      CALL CALC_THERMO_DES

! Call user functions specific to parcel motion
      IF (CALL_USR) CALL USR1_DES

! Advance velocity and position.
      CALL INTEGRATE_TIME_PIC(PIC_ITERS)

! Advance energy and species equations
      CALL DES_THERMO_NEWVALUES

! Impose the wall-particle boundary condition
      CALL APPLY_WALL_BC_PIC

! Exchange particle crossing processor boundaries
      CALL PIC_PAR_EXCHANGE

! Bin particles to the fluid grid
      CALL PARTICLES_IN_CELL

! Apply mass outflow/inflow boundary conditions
      IF(PIC_BCMI > 0) CALL MASS_INFLOW_PIC

      IF(S_TIME + DTSOLID < TEND_PIC_LOOP .OR. &
         .NOT.DES_EXPLICITLY_COUPLED ) THEN
! Calculate interpolation weights
         CALL CALC_INTERP_WEIGHTS
! Calculate mean fields
         CALL COMP_MEAN_FIELDS
      ENDIF

! Update time to reflect changes
      S_TIME = S_TIME + DTSOLID

! When coupled, all write calls are made in time_march (the continuum
! portion) according to user settings for spx_time and res_time.
! The following section targets data writes for DEM only cases:
      IF(.NOT.DES_CONTINUUM_COUPLED) THEN
! Keep track of TIME for DEM simulations
         TIME = S_TIME
         NSTEP = NSTEP + 1
! Call the output manager to write RES and SPx data.
         CALL OUTPUT_MANAGER(.FALSE., .FALSE.)
      ENDIF  ! end if (.not.des_continuum_coupled)

      IF (CALL_USR)  CALL USR2_DES

   END SUBROUTINE PIC_TIME_STEP


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!     Subroutine: PIC_TIME_END                                         !
!     Author: Mark Meredith                          Date: 2019-10-04  !
!                                                                      !
!     Purpose: Finish PIC time march                                   !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE PIC_TIME_END(PIC_ITERS)
      implicit none
      INTEGER, INTENT(IN) :: PIC_ITERS

      IF(CALL_USR) CALL USR3_DES

      CALL GLOBAL_SUM(PIP, gPIP)
      WRITE(ERR_MSG, 3000) trim(iVal(PIC_ITERS)), trim(iVal(gPIP))
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

3000  FORMAT(/'PIC NITs: ',A,3x,'Total PIP: ', A)

   END SUBROUTINE PIC_TIME_END


END MODULE PIC_TIME_MARCH_MOD
