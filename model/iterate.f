#include "error.inc"

MODULE ITERATE

   USE ACCUM_RESID_MOD, ONLY: ACCUM_RESID
   USE ADJUST_EPS_GHD_MOD, ONLY: ADJUST_EPS_GHD
   USE BC, only: DELP_X, DELP_Y, DELP_Z, FLUX_G
   USE CALC_K_CP_MOD, ONLY: CALC_K_CP
   USE CALC_MFLUX_MOD, ONLY: CALC_MFLUX
   USE CALC_P_STAR_MOD, ONLY: CALC_P_STAR
   USE CALC_RESID_MOD, ONLY: CALC_RESID_MB
   USE CALC_VOL_FR_MOD, ONLY: CALC_VOL_FR, SET_EP_FACTORS
   USE CG_SET_OUTFLOW_MOD, ONLY: CG_SET_OUTFLOW
   USE CHECK_CONVERGENCE_MOD, ONLY: CHECK_CONVERGENCE
   USE CALC_COEFF_MOD, only: CALC_COEFF, CALC_RRATE, CALC_TRD_AND_TAU
   USE COMPAR, ONLY: MYPE, PE_IO
   USE CONT, ONLY: SOLVE_CONTINUITY
   USE CONV_ROP_MOD, ONLY: CONV_ROP
   USE CORRECT_0_MOD, ONLY: CORRECT_0
   USE CORRECT_1_MOD, ONLY: CORRECT_1
   USE CUTCELL, ONLY: CARTESIAN_GRID
   USE DASHBOARD, ONLY: F_DASHBOARD, N_DASHBOARD, RUN_STATUS, WRITE_DASHBOARD
   USE DISCRETELEMENT, ONLY: DISCRETE_ELEMENT, DES_CONTINUUM_HYBRID
   USE DISPLAY_RESID_MOD, ONLY: DISPLAY_RESID
   USE ERROR_MANAGER
   USE FLDVAR, ONLY: EP_G, RO_G, ROP_G, ROP_S, P_STAR
   USE FUNITS, ONLY: DMP_LOG, UNIT_LOG, UNIT_OUT
   USE GEOMETRY, ONLY: CYCLIC, CYLINDRICAL, CYCLIC_X_MF, CYCLIC_Y_MF, CYCLIC_Z_MF
   USE GEOMETRY, ONLY: CYCLIC_X, CYCLIC_Y, CYCLIC_Z
   USE GEOMETRY, ONLY: DO_I, DO_J, DO_K
   USE GET_HLOSS_MOD, ONLY: GET_HLOSS
   USE GET_SMASS_MOD, ONLY: GET_SMASS
   USE INIT_RESID_MOD, ONLY: INIT_RESID
   USE ISO_C_BINDING, ONLY: C_INT
   USE K_EPSILON_PROP_MOD, ONLY: K_EPSILON_PROP
   USE LEQSOL, ONLY: LEQ_ADJUST
   USE MACHINE, ONLY: START_LOG, END_LOG
   USE MMS, ONLY: USE_MMS
   USE OUTPUT, ONLY: FULL_LOG, NLOG
   USE PARAM1, ONLY: ONE, SMALL_NUMBER, UNDEFINED, UNDEFINED_I, ZERO
   USE PHYSPROP, ONLY: MMAX, RO_G0, SMAX
   USE PSCOR, ONLY: K_CP, MCP
   USE QMOM_KINETIC_EQUATION, ONLY: QMOMK
   USE RADIAL_VEL_CORRECTION_MOD, ONLY: RADIAL_VEL_CORRECTION
   USE RESET_NEW_MOD, ONLY: RESET_NEW
   USE RESIDUAL, ONLY: RESID_P, RESID
   USE RUN, ONLY: CALL_USR
   USE RUN, ONLY: DT, DT_PREV, RUN_TYPE, TIME, TSTOP, NSTEP
   USE RUN, ONLY: FRICTION, TUNIT
   USE RUN, ONLY: GHD_2007, GRANULAR_ENERGY, KT_TYPE_ENUM
   USE RUN, ONLY: NSTEPRST, CN_ON, GET_TUNIT, STEADY_STATE
   USE RUN, ONLY: PHIP_OUT_ITER, ENERGY_EQ, IER, STEADY_STATE
   USE SCALARS, ONLY: NSCALAR
   USE SCALAR_PROP_MOD, ONLY: SCALAR_PROP
   USE SET_BC1_MOD, ONLY: SET_BC1
   USE SET_WALL_BC_MOD, ONLY: SET_WALL_BC
   USE SOLVE_ENERGY_EQ_MOD, ONLY: SOLVE_ENERGY_EQ
   USE SOLVE_EPP_MOD, ONLY: SOLVE_EPP
   USE SOLVE_GRANULAR_ENERGY_MOD, ONLY: SOLVE_GRANULAR_ENERGY
   USE SOLVE_K_EPSILON_EQ_MOD, ONLY: SOLVE_K_EPSILON_EQ
   USE SOLVE_PP_G_MOD, ONLY: SOLVE_PP_G
   USE SOLVE_SCALAR_EQ_MOD, ONLY: SOLVE_SCALAR_EQ
   USE SOLVE_SPECIES_EQ_MOD, ONLY: SOLVE_SPECIES_EQ
   USE SOLVE_VEL_STAR_MOD, ONLY: SOLVE_VEL_STAR
   USE TIME_CPU, ONLY: CPU0, CPU_NLOG, CPUOS, TIME_NLOG
   USE TOLERANC, ONLY: NORM_G, NORM_S
   USE TURB, ONLY: K_EPSILON
   USE UPDATE_DASHBOARD_MOD, ONLY: UPDATE_DASHBOARD
   USE UTILITIES, ONLY: MFIX_ISNAN
   USE VAVG_U_G_MOD, ONLY: VAVG_U_G, VAVG_FLUX_U_G
   USE VAVG_U_S_MOD, ONLY: VAVG_U_S
   USE VAVG_V_G_MOD, ONLY: VAVG_V_G, VAVG_FLUX_V_G
   USE VAVG_V_S_MOD, ONLY: VAVG_V_S
   USE VAVG_W_G_MOD, ONLY: VAVG_W_G, VAVG_FLUX_W_G
   USE VAVG_W_S_MOD, ONLY: VAVG_W_S

   implicit none

! flag indicating convergence status with MUSTIT = 0,1,2 implying
! complete convergence, non-covergence and divergence respectively
      INTEGER :: MUSTIT

! Number of iterations completed for current timestep
      INTEGER :: NIT

! User defined maximum number of iterations
      INTEGER :: MAX_NIT

      LOGICAL :: CONVERGED, DIVERGED

! cpu time left
      DOUBLE PRECISION :: TLEFT
! Normalization factor for gas & solids pressure residual
      DOUBLE PRECISION :: NORMg, NORMs
! Set normalization factor for gas and solids pressure residual
      LOGICAL :: SETg, SETs
! gas & solids pressure residual
      DOUBLE PRECISION :: RESg, RESs
! Weight of solids in the reactor
      DOUBLE PRECISION :: SMASS
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: errorpercent
! Error Message
      CHARACTER(LEN=32) :: lMsg

! Flag for disabling pressure solver for MMS tests
      LOGICAL :: CALL_SOLVE_PP_G = .TRUE.

      CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: ITERATE_INIT                                            !
!  Author: M. Syamlal                                 Date: 12-APR-96  !
!                                                                      !
!  Purpose: This module controls the iterations for solving equations  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE ITERATE_INIT

      IMPLICIT NONE

      DOUBLE PRECISION :: DISPLAYED_DT

! initializations
      DT_prev = DT
      NIT = 0
      MUSTIT = 0
      CONVERGED = .FALSE.
      DIVERGED  = .FALSE.
      RESG = ZERO
      RESS = ZERO

      IF(NORM_G == UNDEFINED) THEN
         NORMG = ONE
         SETG = .FALSE.
      ELSE
         NORMG = NORM_G
         SETG = .TRUE.
      ENDIF

      IF(NORM_S == UNDEFINED) THEN
         NORMS = ONE
         SETS = .FALSE.
      ELSE
         NORMS = NORM_S
         SETS = .TRUE.
      ENDIF

      LEQ_ADJUST = .FALSE.

! Initialize residuals
      CALL INIT_RESID ()

! Initialize the routine for holding gas mass flux constant with cyclic bc
      IF(CYCLIC) CALL GoalSeekMassFlux(NIT, MUSTIT, .false.)

! CPU time left
      IF (FULL_LOG) THEN
         TLEFT = (TSTOP - TIME)*CPUOS
         CALL GET_TUNIT (TLEFT, TUNIT)

         IF (STEADY_STATE) THEN
            CALL GET_SMASS (SMASS)
            WRITE (ERR_MSG, '(/A,G12.5, A,F9.3,1X,A)') &
               ' Starting solids mass = ', SMASS
            CALL LOG_STATUS()
         ELSE
            IF(myPE.eq.PE_IO) THEN
               IF ((CN_ON.AND.NSTEP>1.AND.RUN_TYPE == 'NEW') .OR. &
                   (CN_ON.AND.RUN_TYPE /= 'NEW' .AND.&
                    NSTEP >= (NSTEPRST+1))) THEN
                  DISPLAYED_DT = 2.D0*DT
               ELSE
                  DISPLAYED_DT = DT
               ENDIF

               WRITE(ERR_MSG, '(/A,G12.5, A,G12.5, A,F9.3,1X,A)') &
                  ' Time = ', TIME, '  Dt = ', DISPLAYED_DT
               CALL LOG_STATUS()
            ENDIF
         ENDIF   ! if/else(steady_state)
      ENDIF   ! if(full_log)

      CALL CALC_RESID_MB(0, errorpercent)

! Calculate the face values of densities and mass fluxes for the first
! solve_vel_star call.
      CALL CONV_ROP()
      CALL CALC_MFLUX ()
      CALL SET_BC1
      CALL SET_EP_FACTORS

! JFD: modification for cartesian grid implementation
      IF(CARTESIAN_GRID) CALL CG_SET_OUTFLOW

! Default/Generic Error message
      lMsg = 'Run diverged/stalled'

      RETURN
      END SUBROUTINE ITERATE_INIT


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: DO_ITERATION                                            !
!  Author: M. Syamlal                                 Date: 12-APR-96  !
!                                                                      !
!  Purpose: This module controls the iterations for solving equations  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE DO_ITERATION(MFIX_DAT)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

      INTEGER :: M

      PHIP_OUT_ITER=NIT ! To record the output of phip
! mechanism to set the normalization factor for the correction
! after the first iteration to the corresponding residual found
! in the first iteration
      IF (.NOT.SETG) THEN
         IF (RESG > SMALL_NUMBER) THEN
            NORMG = RESG
            SETG = .TRUE.
         ENDIF
      ENDIF
      IF (.NOT.SETS) THEN
         IF (RESS > SMALL_NUMBER) THEN
            NORMS = RESS
            SETS = .TRUE.
         ENDIF
      ENDIF

! Call user-defined subroutine to set quantities that need to be updated
! every iteration
      IF (CALL_USR) CALL USR2

! Calculate coefficients, excluding density and reactions.
      CALL CALC_COEFF(MFIX_DAT, IER, 1)
      IF (IER_MANAGER()) return

! Diffusion coefficient and source terms for user-defined scalars
      IF(NScalar /= 0) CALL SCALAR_PROP()

! Diffusion coefficient and source terms for K & Epsilon Eq.
      IF(K_Epsilon) CALL K_Epsilon_PROP()

! Update the stress tensor trace and cross terms each subiteration
! for MMS cases.
      IF(USE_MMS) CALL CALC_TRD_AND_TAU()

! Solve starred velocity components
      CALL SOLVE_VEL_STAR(IER)

! Correct the centerline velocity for cylindrical simulations.
      IF(CYLINDRICAL) CALL RADIAL_VEL_CORRECTION

! Calculate densities.
      CALL PHYSICAL_PROP(MFIX_DAT, IER, 0)
      IF (IER_MANAGER()) return

! Calculate chemical reactions.
      CALL CALC_RRATE(MFIX_DAT, IER)

! Solve solids volume fraction correction equation for close-packed
! solids phases
      IF(.NOT.(DISCRETE_ELEMENT .OR. QMOMK) .OR. &
         DES_CONTINUUM_HYBRID) THEN
         IF (MMAX > 0) THEN
! MMS:  Solve gas continuity only.
            IF(USE_MMS) THEN
               CALL SOLVE_CONTINUITY(0,IER)
! Regular, non-MMS cases.
            ELSE
               IF(MMAX == 1 .AND. MCP /= UNDEFINED_I)THEN
! if second phase (m=1) can overpack (e.g., bubbles) then solve its
! continuity equation
                  CALL CALC_K_CP (K_CP)
                  CALL SOLVE_EPP (NORMS, RESS, IER)
                  CALL CORRECT_1 ()
               ELSE

! If one chooses to revert back to old mark_phase_4_cor wherein the
! continuity of the gas phase can get marked to be solved then this
! loop should start at 0.
                  DO M=1,SMAX ! mmax -> smax for GHD theory
! Volume fraction correction technique for one of the solids phase
! is not implemented.  This will only slow down convergence.
!                      IF (M .EQ. MCP) THEN
!                       CALL CALC_K_CP (K_CP, IER)
!                       CALL SOLVE_EPP (NORMS, RESS, IER)
!                       CALL CORRECT_1 (IER)
!                    ELSE
                        CALL SOLVE_CONTINUITY(M,IER)
!                    ENDIF
                  ENDDO
               ENDIF   ! end if/else (mmax==1 .and. mcp /= undefined)
            ENDIF ! end if/else (MMS)

            IF(KT_TYPE_ENUM == GHD_2007) CALL ADJUST_EPS_GHD

            CALL CALC_VOL_FR (P_STAR, RO_G, ROP_G, EP_G, ROP_S, IER)
            IF (IER_MANAGER()) return

         ENDIF  ! endif (mmax >0)

      ENDIF  ! end if (.not.discrete_element)


! Calculate P_star in cells where solids continuity equation is
! solved
      IF(.NOT.(DISCRETE_ELEMENT .OR. QMOMK) .OR. &
         DES_CONTINUUM_HYBRID) THEN
         IF (MMAX > 0 .AND. .NOT.FRICTION) &
            CALL CALC_P_STAR (EP_G, P_STAR)
      ENDIF

! Calculate the face values of densities.
      CALL CONV_ROP()

      IF (RO_G0 /= ZERO) THEN
! Solve fluid pressure correction equation
         IF (CALL_SOLVE_PP_G) THEN
            CALL SOLVE_PP_G (NORMG, RESG, IER)
         ENDIF
! Correct pressure, velocities, and density
         CALL CORRECT_0 ()
      ENDIF

! Recalculate densities.
      CALL PHYSICAL_PROP(MFIX_DAT, IER, 0)
      IF (IER_MANAGER()) return

! Update wall velocities:
! modified by sof to force wall functions so even when NSW or FSW are
! declared, default wall BC will still be treated as NSW and no wall
! functions will be used
      IF(.NOT. K_EPSILON) CALL SET_WALL_BC ()

! Calculate the face values of mass fluxes
      CALL CALC_MFLUX ()
      CALL SET_BC1
      CALL SET_EP_FACTORS

! JFD: modification for cartesian grid implementation
      IF(CARTESIAN_GRID) CALL CG_SET_OUTFLOW

! Solve energy equations
      IF (ENERGY_EQ) THEN
         CALL SOLVE_ENERGY_EQ (IER)
         IF (IER_MANAGER()) return
      ENDIF

! Solve granular energy equation
      IF (GRANULAR_ENERGY) THEN
         IF(.NOT.DISCRETE_ELEMENT .OR. DES_CONTINUUM_HYBRID) THEN
            CALL SOLVE_GRANULAR_ENERGY (IER)
            IF (IER_MANAGER()) return
         ENDIF
      ENDIF

! Solve species mass balance equations.
      CALL SOLVE_SPECIES_EQ (IER)
      IF (IER_MANAGER()) return

! Solve other scalar transport equations
      IF(NScalar /= 0) CALL SOLVE_Scalar_EQ (IER)

! Solve K & Epsilon transport equations
      IF(K_Epsilon) CALL SOLVE_K_Epsilon_EQ (IER)

! User-defined linear equation solver parameters may be adjusted after
! the first iteration
      IF (.NOT.CYCLIC) LEQ_ADJUST = .TRUE.

! Check for convergence
      CALL ACCUM_RESID ! Accumulating residuals from all the processors
      RESG = RESID(RESID_P,0)
      RESS = RESID(RESID_P,1)
      CALL CALC_RESID_MB(1, errorpercent)
      MUSTIT = 0
      CALL CHECK_CONVERGENCE (NIT, errorpercent(0), MUSTIT)

      IF(CYCLIC .AND. (MUSTIT==0 .OR. NIT >= MAX_NIT)) &
         CALL GoalSeekMassFlux(NIT, MUSTIT, .true.)

! Display residuals
      CALL DISPLAY_RESID (NIT)

      CALL END_ITERATION

      contains

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: END_ITERATION                                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

      SUBROUTINE END_ITERATION
      IMPLICIT NONE

         ! Determine course of simulation: converge, non-converge, diverge?
         IF (MUSTIT == 0) THEN
            IF (STEADY_STATE .AND. NIT==1) RETURN   !Iterations converged
            CONVERGED = .TRUE.
            IER = 0
         ELSEIF (MUSTIT==2 .AND. .NOT.STEADY_STATE) THEN
            DIVERGED = .TRUE.
            IER = 1
         ENDIF

      END SUBROUTINE END_ITERATION


!----------------------------------------------------------------------!
! Function: IER_Manager                                                !
!                                                                      !
! Purpose: Identify and account for errors from called subroutines.    !
!          Returns .TRUE. for lErr >= 100, otherwise .FALSE.           !
!                                                                      !
! Reserved Error Blocks:                                               !
!                                                                      !
! [ 100,  109]: PHYSICAL_PROP                                          !
! [ 110,  119]: CALC_VOL_FR                                            !
! [ 120,  129]: SOLVE_ENERGY_EQ                                        !
! [ 130,  139]: SOLVE_SPECIES_EQ                                       !
! [ 140,  149]: SOLVE_GRANULAR_ENERGY                                  !
!                                                                      !
!----------------------------------------------------------------------!
      LOGICAL FUNCTION IER_MANAGER()

! Default case: do nothing.
      IF(IER < 100) THEN
         IER_MANAGER = .FALSE.
         return
      ENDIF

! Errors with an index greater than 100 will force an exit from iterate
! and in turn, reduce the step-size, and restart the time-step.
      IER_MANAGER = .TRUE.
      MUSTIT = 2

! Errors reported from PHYSICAL_PROP
!```````````````````````````````````````````````````````````````````````
      IF(IER <  110) THEN
         IF(IER ==  100) THEN
            lMsg = 'Negative gas density detected'
         ELSEIF(IER ==  101) THEN
            lMsg = 'Negative solids density detected'
         ELSE
            lMsg = 'UCE in PHYSICAL_PROP'
         ENDIF


! Errors reported from CALC_VOL_FR
!```````````````````````````````````````````````````````````````````````
      ELSEIF(IER <  120) THEN
         IF(IER ==  110) THEN
            lMsg = 'Negative void fraction detected'
         ELSE
            lMsg = 'UCE in CALC_VOL_FR'
         ENDIF


! Errors reported from SOLVE_ENERGY_EQ
!```````````````````````````````````````````````````````````````````````
      ELSEIF(IER <  130) THEN
         IF(IER ==  120) THEN
            lMsg = 'Energy Equation diverged'
         ELSE
            lMsg = 'UCE in SOLVE_ENERGY_EQ'
         ENDIF


! Errors reported from SOLVE_SPECIES_EQ
!```````````````````````````````````````````````````````````````````````
      ELSEIF(IER <  140) THEN
         IF(IER ==  130) THEN
            lMsg = 'Species Equation diverged'
         ELSE
            lMsg = 'UCE in SOLVE_SPECIES_EQ'
         ENDIF


! Errors reported from SOLVE_GRANULAR_ENERGY
!```````````````````````````````````````````````````````````````````````
      ELSEIF(IER <  150) THEN
         IF(IER ==  140) THEN
            lMsg = 'Granular Energy Eq diverged'
         ELSE
            lMsg = 'UCE in SOLVE_GRANULAR_ENERGY'
         ENDIF

! Unclassified Errors
!```````````````````````````````````````````````````````````````````````
      ELSE
         lMsg = 'Run diverged/stalled with UCE'
      ENDIF


      IF(STEADY_STATE) IER_MANAGER = .FALSE.

      CALL END_ITERATION

      return
      END FUNCTION IER_MANAGER

      END SUBROUTINE DO_ITERATION


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: ITERATE                                                 !
!  Author: M. Syamlal                                 Date: 12-APR-96  !
!                                                                      !
!  Purpose: This module controls the iterations for solving equations  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE POST_ITERATE

      IMPLICIT NONE

      IF (CONVERGED) THEN
         CALL LOG_CONVERGED
      ELSEIF (DIVERGED) THEN
         CALL LOG_DIVERGED
      ELSE
         CALL GET_SMASS (SMASS)
         WRITE(ERR_MSG, 5100) TIME, DT, NIT, SMASS
         CALL LOG_STATUS()
      ENDIF

      IF(.NOT.(CONVERGED .OR. DIVERGED)) THEN
         IER = 1
      ENDIF

5100  FORMAT(1X,'t=',F11.4,' Dt=',G11.4,' NIT>',I3,' Sm= ',G12.5, &
           'MbErr%=', G11.4)

      END SUBROUTINE POST_ITERATE

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: ITERATE                                                 !
!  Author: M. Syamlal                                 Date: 12-APR-96  !
!                                                                      !
!  Purpose: This module controls the iterations for solving equations  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE LOG_DIVERGED

      IMPLICIT NONE

      CHARACTER(LEN=4) :: TUNIT

      IF (FULL_LOG) THEN
         CALL START_LOG
         CALL CALC_RESID_MB(1, errorpercent)

         WRITE(ERR_MSG,5200) TIME, DT, NIT, errorpercent(0), trim(adjustl(lMsg))
         CALL LOG_STATUS()
      ENDIF

      ! JFD: modification for cartesian grid implementation
      IF(WRITE_DASHBOARD) THEN
         RUN_STATUS = 'Diverged/stalled...'
         N_DASHBOARD = N_DASHBOARD + 1
         IF(MOD(N_DASHBOARD,F_DASHBOARD)==0) THEN
            TLEFT = (TSTOP - TIME)*CPUOS
            CALL GET_TUNIT (TLEFT, TUNIT)
            CALL UPDATE_DASHBOARD(NIT,TLEFT,TUNIT)
         ENDIF
      ENDIF
5200  FORMAT(1X,'t=',F11.4,' Dt=',G11.4,' NIT=',&
                 I3,'MbErr%=', G11.4, ': ',A,' :-(')
      END SUBROUTINE LOG_DIVERGED

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: ITERATE                                                 !
!  Author: M. Syamlal                                 Date: 12-APR-96  !
!                                                                      !
!  Purpose: This module controls the iterations for solving equations  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE LOG_CONVERGED

      IMPLICIT NONE

      CHARACTER(LEN=4) :: TUNIT
! Perform checks and dump to screen every NLOG time steps
      IF (MOD(NSTEP,NLOG) == 0) CALL DUMP_TO_SCREEN

! JFD: modification for cartesian grid implementation
      IF(WRITE_DASHBOARD) THEN
         RUN_STATUS = 'In Progress...'
         N_DASHBOARD = N_DASHBOARD + 1
         IF(MOD(N_DASHBOARD,F_DASHBOARD)==0) THEN
            TLEFT = (TSTOP - TIME)*CPUOS
            CALL GET_TUNIT (TLEFT, TUNIT)
            CALL UPDATE_DASHBOARD(NIT,TLEFT,TUNIT)
         ENDIF
      ENDIF

      CONTAINS

      SUBROUTINE DUMP_TO_SCREEN
      IMPLICIT NONE

      ! phase index
      INTEGER :: M
! current cpu time used
      DOUBLE PRECISION :: CPU_NOW
! Heat loss from the reactor
      DOUBLE PRECISION :: HLOSS
! average velocity
      DOUBLE PRECISION :: Vavg

      CALL CPU_TIME (CPU_NOW)
      CPUOS = (CPU_NOW - CPU_NLOG)/(TIME - TIME_NLOG)
      CPU_NLOG = CPU_NOW
      TIME_NLOG = TIME
      CPU_NOW = CPU_NOW - CPU0

      CALL CALC_RESID_MB(1, errorpercent)
      CALL GET_SMASS (SMASS)
      IF (ENERGY_EQ) CALL GET_HLOSS (HLOSS)

      CALL START_LOG
      IF (ENERGY_EQ) THEN
         WRITE(ERR_MSG,5000)TIME, DT, NIT, SMASS, HLOSS, CPU_NOW
         CALL LOG_STATUS()
      ELSE
         WRITE(ERR_MSG,5001) TIME, DT, NIT, SMASS, CPU_NOW
         CALL LOG_STATUS()
      ENDIF

 5000 FORMAT(1X,'t=',F11.4,' Dt=',G11.4,' NIT=',I3,' Sm=',G12.5, &
         ' Hl=',G12.5,T84,'CPU=',F8.0,' s')

 5001 FORMAT(1X,'t=',F11.4,' Dt=',G11.4,' NIT=',I3,' Sm=',G12.5, &
         T84,'CPU=',F8.0,' s')

      WRITE (ERR_MSG, 5002) (errorpercent(M), M=0,MMAX)
      CALL LOG_STATUS()

 5002 FORMAT(3X,'MbError%(0,MMAX):', 5(1X,G11.4))

      IF (.NOT.FULL_LOG) THEN
         TLEFT = (TSTOP - TIME)*CPUOS
         CALL GET_TUNIT (TLEFT, TUNIT)
         IF(DMP_LOG)WRITE (UNIT_LOG, '(46X,A,F9.3,1X,A)')
      ENDIF

      IF (CYCLIC_X .OR. CYCLIC_Y .OR. CYCLIC_Z) THEN
         IF (DO_I) THEN
           Vavg = VAVG_U_G()
           IF(DMP_LOG)WRITE (UNIT_LOG, 5050) 'U_g = ', Vavg
         ENDIF
         IF (DO_J) THEN
           Vavg = VAVG_V_G()
           IF(DMP_LOG)WRITE (UNIT_LOG, 5050) 'V_g = ',  Vavg
         ENDIF
         IF (DO_K) THEN
           Vavg = VAVG_W_G()
           IF(DMP_LOG)WRITE (UNIT_LOG, 5050) 'W_g = ', Vavg
         ENDIF
         DO M = 1, SMAX
            IF (DO_I) Then
              Vavg = VAVG_U_S(M)
              IF(DMP_LOG)WRITE (UNIT_LOG, 5060) 'U_s(', M, ') = ', Vavg
            ENDIF
            IF (DO_J) Then
              Vavg = VAVG_V_S(M)
              IF(DMP_LOG)WRITE (UNIT_LOG, 5060) 'V_s(', M, ') = ', Vavg
            ENDIF
            IF (DO_K) Then
              Vavg = VAVG_W_S(M)
              IF(DMP_LOG)WRITE (UNIT_LOG, 5060) 'W_s(', M, ') = ', Vavg
            ENDIF
         ENDDO
      ENDIF   ! end if cyclic_x, cyclic_y or cyclic_z

      CALL END_LOG

5050  FORMAT(5X,'Average ',A,G12.5)
5060  FORMAT(5X,'Average ',A,I2,A,G12.5)

      END SUBROUTINE DUMP_TO_SCREEN

      END SUBROUTINE LOG_CONVERGED

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: GoalSeekMassFlux                                        !
!  Author: M. Syamlal                                 Date: 12-APR-96  !
!                                                                      !
! Purpose:  In the following subroutine the mass flux across a periodic!
! domain with pressure drop is held constant at a user-specified value.!
! This module is activated only if the user specifies a value for      !
! keyword FLUX_G                                                       !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE GoalSeekMassFlux(NIT, MUSTIT, doit)

      IMPLICIT NONE
!-----------------------------------------------
! Dummy arguments
!-----------------------------------------------
      INTEGER, INTENT(INOUT) :: NIT, MUSTIT
      LOGICAL, INTENT(IN) :: doit
!-----------------------------------------------
! Local Variables
!-----------------------------------------------
      INTEGER, PARAMETER :: MAXOUTIT = 500
      DOUBLE PRECISION, PARAMETER          :: omega = 0.9
      DOUBLE PRECISION, PARAMETER          :: TOL = 1E-03
      INTEGER, SAVE :: OUTIT
      LOGICAL, SAVE :: firstPass = .true.

      DOUBLE PRECISION, SAVE  :: mdot_n, mdot_nm1, delp_n, delp_nm1, err
      DOUBLE PRECISION        :: mdot_0, delp_xyz

      IF(CYCLIC_X_MF)THEN
         delp_n = delp_x
      ELSEIF(CYCLIC_Y_MF)THEN
         delp_n = delp_y
      ELSEIF(CYCLIC_Z_MF)THEN
         delp_n = delp_z
      ELSE
         RETURN
      ENDIF

      IF(.NOT.doit) THEN
         OUTIT = 0
         RETURN
      ENDIF

      OUTIT = OUTIT + 1
      IF(OUTIT > MAXOUTIT) THEN
         write(ERR_MSG,*) MAXOUTIT
         CALL LOG_ERROR()
      ENDIF

      mdot_0 = Flux_g


      ! calculate the average gas mass flux and error
      IF(CYCLIC_X_MF)THEN
        mdot_n = VAVG_Flux_U_G()
      ELSEIF(CYCLIC_Y_MF)THEN
        mdot_n = VAVG_Flux_V_G()
      ELSEIF(CYCLIC_Z_MF)THEN
        mdot_n = VAVG_Flux_W_G()
      ENDIF

      IF (mfix_isnan(mdot_n) .OR. mfix_isnan(delp_n)) THEN
         write(ERR_MSG,*) mdot_n, delp_n, ' NaN being caught in GoalSeekMassFlux'
         CALL LOG_WARNING()
         RETURN
      ENDIF

      err = abs((mdot_n - mdot_0)/mdot_0)
      IF( err < TOL) THEN
         MUSTIT = 0
      ELSE
        MUSTIT = 1
        NIT = 1
      ENDIF

! correct delp
      if(.not.firstPass)then
!        delp_xyz = delp_n - omega * (delp_n - delp_nm1) * (mdot_n - mdot_0) &
!                          / (mdot_n - mdot_nm1)
! Fail-Safe Newton's method (below) works better than the regular
! Newton method (above)

         delp_xyz = delp_n - omega * (delp_n - delp_nm1) * &
                     ((mdot_n - mdot_0)/(mdot_nm1 - mdot_0)) / &
                     ((mdot_n - mdot_0)/(mdot_nm1 - mdot_0) - ONE)
      else
         firstPass=.false.
         delp_xyz = delp_n*0.99
      endif

      IF(MUSTIT == 0) then
        IF(myPE.eq.PE_IO) Write(*,5500) TIME, OUTIT, delp_xyz, mdot_n
      ENDIF

      mdot_nm1 = mdot_n
      delp_nm1 = delp_n

      IF(CYCLIC_X_MF)THEN
        delp_x = delp_xyz
      ELSEIF(CYCLIC_Y_MF)THEN
        delp_y = delp_xyz
      ELSEIF(CYCLIC_Z_MF)THEN
        delp_z = delp_xyz
      ENDIF

      RETURN

5500  Format('  Time=', G12.5, ' MassFluxIterations=', I4, ' DelP=', &
      G12.5, ' Gas Flux=', G12.5)

      END SUBROUTINE GoalSeekMassFlux

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: ADJUST_DT()                                            !
!  Author: M. Syamlal                                 Date: FEB-10-97  !
!                                                                      !
!  Purpose: Automatically adjust time step.                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      LOGICAL FUNCTION ADJUSTDT(MFIX_DAT)

! Global Variables:
!---------------------------------------------------------------------//
! User defined type of run: new or restart
      use run, only: RUN_TYPE
! Integer flag: 0=Good, 100=initialize, otherwise bad.
      use run, only: IER
! User defined: min, max DT and adjustment factor
      use run, only: DT_MIN, DT_MAX, DT_FAC
! Flag: Use stored DT value for advancing TIME
      use run, only: USE_DT_PREV
! Flag: 2nd order time implementation
      use run, only: CN_ON
! Flag: Continue to run at DT_MIN
      use run, only: PERSISTENT_MODE
! The current number of time steps (value at restart).
      use run, only: NSTEP, NSTEPRST
! Current DT (1/DT) and direction of last change (+/-)
      use run, only: DT, oDT, DT_DIR, STEADY_STATE

! Global Parameters:
!---------------------------------------------------------------------//
      use param1, only: ZERO, ONE, UNDEFINED

! Module procedures:
!---------------------------------------------------------------------//
      use error_manager

      IMPLICIT NONE

! Dummy Arguments:

      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

! Local Variables:
!---------------------------------------------------------------------//
! Number of steps in between DT adjustments.
      INTEGER, PARAMETER :: STEPS_MIN = 5
! Number of time steps since last DT adjustment
      INTEGER, SAVE :: STEPS_TOT=0
! number of iterations since last DT adjustment
      INTEGER, SAVE :: NIT_TOT=0
! Iterations per second for last dt
      DOUBLE PRECISION, SAVE :: NIToS=0.0
! Current number of iterations per second
      DOUBLE PRECISION :: NITOS_NEW
! Flag to half/double the current time step
      LOGICAL :: CN_ADJUST_DT
!......................................................................!

! Initialize the function result.
      ADJUSTDT = .FALSE.
      USE_DT_PREV = .FALSE.

! Steady-state simulation.
      IF (STEADY_STATE .OR. DT<ZERO) RETURN

! Local flag for adjusting the time step for CN implementation.
      CN_ADJUST_DT = CN_ON .AND. ((RUN_TYPE=='NEW' .AND. NSTEP>1) .OR. &
         (RUN_TYPE/='NEW' .AND. NSTEP >= (NSTEPRST+1)))

! Iterate successfully converged.
!---------------------------------------------------------------------//
      IF(IER == 0) THEN

! Set back the timestep to original size which was halved previously for
! 2nd order accurate time implementation.
         IF(CN_ADJUST_DT) DT = 2.0D0*DT

! Calculate a new DT every STEPS_MIN time steps.
         IF(STEPS_TOT >= STEPS_MIN) THEN
            NITOS_NEW = DBLE(NIT_TOT)/(STEPS_TOT*DT)
            IF (NITOS_NEW > NITOS) DT_DIR = DT_DIR*(-1)
            STEPS_TOT = 0
            NITOS = NITOS_NEW
            NIT_TOT = 0
            IF (DT_DIR > 0) THEN
               IF(NIT < MAX_NIT) DT = MIN(DT_MAX,DT/DT_FAC)
            ELSE
               DT = DT*DT_FAC
               IF(PERSISTENT_MODE) DT = max(DT, DT_MIN)
            ENDIF

! DT was modified. Use the stored DT should be used to update TIME.
            USE_DT_PREV = .TRUE.

! Write the convergence stats to the screen/log file.
            WRITE(ERR_MSG,"('DT=',g11.4,3x,'NIT/s=',A)")  &
               DT, trim(iVal(nint(NITOS)))
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

         ELSE
            STEPS_TOT = STEPS_TOT + 1
            NIT_TOT = NIT_TOT + NIT
         ENDIF
! No need to iterate again
         ADJUSTDT = .FALSE.
! Cut the timestep into half for 2nd order accurate time implementation.
         IF(CN_ADJUST_DT) DT = 0.5d0*DT

! Iterate failed to converge.
!---------------------------------------------------------------------//
      ELSE

! Clear the error flag.
         IER = 0

! Reset the timestep to original size which was halved previously for
! 2nd order accurate time implementation.
         IF(CN_ADJUST_DT) DT = 2.0d0*DT

! Reset counters.
         DT_DIR = -1
         STEPS_TOT = 0
         NITOS = 0.
         NIT_TOT = 0

! Reduce the step size.
         DT = DT*DT_FAC

! The step size has decreased to the minimum.
         IF (DT_FAC >= ONE) THEN

            IF(PERSISTENT_MODE) THEN
               ADJUSTDT = .FALSE.
            ELSE
               WRITE(ERR_MSG,"(3X,A)") &
                  'DT_FAC >= 1. Recovery not possible!'
               CALL LOG_ERROR()
            ENDIF

         ELSEIF (DT > DT_MIN) THEN

            WRITE(ERR_MSG,"(3X,'Recovered: Dt=',G12.5,' :-)')") DT
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

            CALL RESET_NEW(MFIX_DAT)

! Iterate again with new dt
            ADJUSTDT = .TRUE.

! Cut the timestep for 2nd order accurate time implementation.
            IF(CN_ADJUST_DT) DT = 0.5d0*DT

! Set the return flag stop iterating.
         ELSE

! Prevent DT from dropping below DT_MIN.
            IF(PERSISTENT_MODE) DT = max(DT, DT_MIN)
            ADJUSTDT = .FALSE.
         ENDIF

      ENDIF

! Update ONE/DT variable.
      ODT = ONE/DT

      RETURN
      END FUNCTION ADJUSTDT

END MODULE ITERATE
