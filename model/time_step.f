#include "error.inc"

MODULE STEP

   USE ADJUST_EPS_GHD_MOD, ONLY: ADJUST_EPS_GHD
   USE ADJUST_EPS_MOD, ONLY: ADJUST_EPS
   USE CALC_COEFF_MOD, only: CALC_COEFF_ALL, CALC_RRATE, CALC_TRD_AND_TAU
   USE CALC_VOL_FR_MOD, ONLY: SET_EP_FACTORS
   USE CHECK, ONLY: CHECK_MASS_BALANCE
   USE CN_EXTRAPOL_MOD, ONLY: CN_EXTRAPOL
   USE DASHBOARD, ONLY: RUN_STATUS, WRITE_DASHBOARD
   USE DISCRETELEMENT, ONLY: DES_EXPLICITLY_COUPLED
   USE ERROR_MANAGER
   USE EXIT, ONLY: EXIT_FLAG
   USE ITERATE, ONLY: NIT
   USE KINTHEORY_U_S_MOD, ONLY: CALC_EXPLICIT_MOM_SOURCE_S, COLL_MOMENTUM_COEFF_IA
   USE MAIN, ONLY: NIT_TOTAL, DNCHECK, NCHECK
   USE QMOMK_TIME_MARCH_MOD, ONLY: QMOMK_TIME_MARCH
   USE RUN, ONLY: DT, DT_MIN, IER
   USE RXNS_GS_DES1_MOD, ONLY: RXNS_GS_GAS1
   USE STIFF_CHEM, ONLY: STIFF_CHEMISTRY, STIFF_CHEM_SOLVER
   USE UPDATE_DASHBOARD_MOD, ONLY: UPDATE_DASHBOARD
   USE UPDATE_OLD_MOD, ONLY: UPDATE_OLD
   USE check_data_30_mod, only: check_data_30
   USE discretelement, only: des_continuum_hybrid, discrete_element
   USE iterate, only: nit
   USE leqsol, only: solver_statistics, report_solver_stats
   USE output, only: DLB
   USE output_man, only: output_manager
   USE param1, only: small_number
   USE qmom_kinetic_equation, only: qmomk
   USE run, only: call_dqmom
   USE run, only: call_usr
   USE run, only: cn_on, dt, dt_prev
   USE run, only: ghd_2007, kt_type_enum
   USE run, only: nstep, nsteprst, odt, run_type, time, use_dt_prev, steady_state
   USE set_bc1_mod, only: set_bc1
   USE toleranc, only: max_inlet_vel
   USE utilities, only: max_vel_inlet

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: TIME_STEP_INIT                                          !
!  Author: M. Syamlal                                 Date: 12-APR-96  !
!                                                                      !
!  Purpose: This module controls the iterations for solving equations  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE TIME_STEP_INIT(MFIX_DAT)

      IMPLICIT NONE

! Path to input file
      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

      IF (CALL_USR) CALL USR1

! Remove solids from cells containing very small quantities of solids
      IF(.NOT.(DISCRETE_ELEMENT .OR. QMOMK) .OR. &
           DES_CONTINUUM_HYBRID) THEN
         IF(KT_TYPE_ENUM == GHD_2007) THEN
            CALL ADJUST_EPS_GHD
         ELSE
            CALL ADJUST_EPS
         ENDIF
      ENDIF

! sof modification: uncomment code below and modify MARK_PHASE_4_COR to
! use previous MFIX algorithm. Nov 22 2010.
! Mark the phase whose continuity will be solved and used to correct
! void/volume fraction in calc_vol_fr (see subroutine for details)
!      CALL MARK_PHASE_4_COR (PHASE_4_P_G, PHASE_4_P_S, DO_CONT, MCP,&
!           DO_P_S, SWITCH_4_P_G, SWITCH_4_P_S, IER)

! Set wall boundary conditions and transient flow b.c.'s
      CALL SET_BC1
! include here so they are set before calculations rely on value
! (e.g., calc_mu_g, calc_mu_s)
      CALL SET_EP_FACTORS


      CALL OUTPUT_MANAGER(EXIT_FLAG, .FALSE.)

! Update previous-time-step values of field variables
      CALL UPDATE_OLD

! Calculate coefficients
      CALL CALC_COEFF_ALL (MFIX_DAT, 0, IER)

! Calculate the stress tensor trace and cross terms for all phases.
      CALL CALC_TRD_AND_TAU()

! Calculate additional solids phase momentum source terms
      IF (.NOT.DISCRETE_ELEMENT .OR. DES_CONTINUUM_HYBRID) THEN
         CALL CALC_EXPLICIT_MOM_SOURCE_S
      ENDIF

! Check rates and sums of mass fractions every NLOG time steps
      IF (NSTEP == NCHECK) THEN
         IF (DNCHECK < 256) DNCHECK = DNCHECK*2
         NCHECK = NCHECK + DNCHECK
! Update the reaction rates for checking
         CALL CALC_RRATE(MFIX_DAT, IER)
         CALL CHECK_DATA_30
      ENDIF

! Double the timestep for 2nd order accurate time implementation
      IF ((CN_ON.AND.NSTEP>1.AND.RUN_TYPE == 'NEW') .OR. &
           (CN_ON.AND.RUN_TYPE /= 'NEW' .AND. NSTEP >= (NSTEPRST+1))) THEN
         DT = 0.5d0*DT
         ODT = ODT * 2.0d0
      ENDIF

! Check for maximum velocity at inlet to avoid convergence problems
      MAX_INLET_VEL = MAX_VEL_INLET()

      END SUBROUTINE TIME_STEP_INIT


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: TIME_STEP_END                                           !
!  Author: M. Syamlal                                 Date: 12-APR-96  !
!                                                                      !
!  Purpose: This module controls the iterations for solving equations  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE TIME_STEP_END

      IMPLICIT NONE

! Other solids model implementations
      IF (QMOMK) CALL QMOMK_TIME_MARCH
      IF (CALL_DQMOM) CALL USR_DQMOM

! Advance the time step and continue
      IF((CN_ON.AND.NSTEP>1.AND.RUN_TYPE == 'NEW') .OR. &
           (CN_ON.AND.RUN_TYPE /= 'NEW' .AND. NSTEP >= (NSTEPRST+1))) THEN
! Double the timestep for 2nd order accurate time implementation
         DT = 2.d0*DT
         ODT = ODT * 0.5d0
! Perform the explicit extrapolation for CN implementation
         CALL CN_EXTRAPOL
      ENDIF

      IF (.NOT. STEADY_STATE) THEN
         IF(USE_DT_PREV) THEN
            TIME = TIME + DT_PREV
         ELSE
            TIME = TIME + DT
         ENDIF
         USE_DT_PREV = .FALSE.
         NSTEP = NSTEP + 1
      ENDIF

      NIT_TOTAL = NIT_TOTAL+NIT

      IF(SOLVER_STATISTICS) CALL REPORT_SOLVER_STATS(NIT_TOTAL, NSTEP)

! write (*,"('Compute the Courant number')")
! call get_stats(IER)

      FLUSH (6)

      IF(DISCRETE_ELEMENT) DLB = .TRUE.
      CALL OUTPUT_MANAGER(EXIT_FLAG, .TRUE.)

      END SUBROUTINE TIME_STEP_END

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_LOW_DT                                            !
!  Author: Mark Meredith                              Date: 04-JUN-16  !
!                                                                      !
!  Purpose: Exit if DT < DT_MIN                                        !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

      SUBROUTINE CHECK_LOW_DT

         IMPLICIT NONE

         IF(DT < DT_MIN) THEN

            IF(WRITE_DASHBOARD) THEN
               RUN_STATUS = 'DT < DT_MIN.  Recovery not possible!'
               CALL UPDATE_DASHBOARD(NIT,0.0d0,'    ')
            ENDIF

            WRITE(ERR_MSG, '(A)') 'DT < DT_MIN.  Recovery not possible!'
            CALL LOG_ERROR()
         ENDIF
      END SUBROUTINE CHECK_LOW_DT

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHEM_MASS                                               !
!  Author: Mark Meredith                              Date: 04-JUN-16  !
!                                                                      !
!  Purpose: Stiff chemistry and check mass balance                     !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

      SUBROUTINE CHEM_MASS

         IMPLICIT NONE

! Stiff Chemistry Solver.
         IF(STIFF_CHEMISTRY) THEN
            IF(DES_EXPLICITLY_COUPLED) CALL RXNS_GS_GAS1
            CALL STIFF_CHEM_SOLVER(DT, IER)
         ENDIF

! Check over mass and elemental balances.  This routine is not active by default.
! Edit the routine and specify a reporting interval to activate it.
         CALL CHECK_MASS_BALANCE (1)
      END SUBROUTINE CHEM_MASS

   END MODULE STEP
