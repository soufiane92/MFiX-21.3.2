#include "error.inc"

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR1                                                   C
!  Purpose: This routine is called from the time loop and is           C
!           user-definable.  The user may insert code in this routine  C
!           or call appropriate user defined subroutines.  This        C
!           can be used for setting or checking errors in quantities   C
!           that vary with time.  This routine is not called from an   C
!           IJK loop, hence all indices are undefined.                 C
!                                                                      C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced:                                               C
!  Variables modified:                                                 C
!                                                                      C
!  Local variables:                                                    C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE USR1

! Modules
!--------------------------------------------------------------------//
      use usr, only: ABSORPTION_CHEM_TYPE_ENUM, EQUILIBRIUM_COUPLED
      use usr, only: usr_eqtime
      use output, only: usr_dt
      use run, only: dt, time, tstop, any_species_eq
      use param1, only: undefined

! Local variables
!--------------------------------------------------------------------//
! Calculation for timing
      DOUBLE PRECISION :: OVERSHOOT
!--------------------------------------------------------------------//

! don't call equilibrium solver if not solving species equations..
      IF (.NOT.ANY_SPECIES_EQ .OR. &
         ABSORPTION_CHEM_TYPE_ENUM /= EQUILIBRIUM_COUPLED) RETURN

! check frequency for call to the equilibrium solver
       IF (USR_DT(1) /= UNDEFINED) THEN
! Calculate if there is overshoot.
           OVERSHOOT = TIME + 0.1d0*USR_DT(1)

! Call equilibrium solver when past usr_eqtime
!           IF(OVERSHOOT < USR_EQTIME .AND. OVERSHOOT<TSTOP) RETURN
           IF (OVERSHOOT >= USR_EQTIME .OR. OVERSHOOT >=TSTOP) THEN
! Update the time to call equilibrium solver
              USR_EQTIME = (INT((OVERSHOOT)/USR_DT(1))+1)*USR_DT(1)
              CALL CHEM_SOLVE_MAIN
           ENDIF
      ELSE
         CALL CHEM_SOLVE_MAIN
      ENDIF

      RETURN
      END SUBROUTINE USR1


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: This is the main routine that, from an ijk loop, invokes   C
!  the call to solve the set of equilibrium equations specified in     C
!  reactions_m.                                                        C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE CHEM_SOLVE_MAIN

      use CHEMICAL_EQUILIBRIUM_M
      use NEWTON_RAPHSON_M, ONLY: NEWTON_RAPHSON_ERRORS
      use REACTIONS_M, only: NS
      use compar, only: MYPE,PE_IO
      use compar, only: ijkstart3, ijkend3
      use constant, only: gas_const, gas_const_cal
      use error_manager
      use fldvar, only: t_s, x_s, ro_s, ep_s
      use functions, only: fluid_at
      use geometry, only: imax,jmax,kmax
      use mpi_utility, only: global_max_0d
      use physprop, only: c_ps
      use run, only: units
      use time_cpu, only: WALL_START
      use time_cpu, only: WALL_TIME
      use usr, only: index_liq, isothermal_eq

      IMPLICIT NONE

! Local variables
!--------------------------------------------------------------------//
! Used for cpu timing
      DOUBLE PRECISION:: TICK, TOCK
! Cumulative cpu spent on chem solver
      DOUBLE PRECISION, SAVE:: CPU_CHEM=0.0
! Max cpu spent on chem solver (across all procs)
      DOUBLE PRECISION:: MAX_CPU_CHEM
! Mass fractions/Temp passed to/from solver
      DOUBLE PRECISION:: X_EQ(NS), T_EQ
! Error indicator
      INTEGER:: SUCCESS
! Cell index
      INTEGER:: IJK
! Specific heat divided by gas constant [mol/kg]
      DOUBLE PRECISION:: CP_hat

! SOLVER PARAMETERS:
! Minimum phase volume fraction required to enforce equilibrium
      DOUBLE PRECISION, PARAMETER :: V_LIMITER = 1.0d-6
! Chemistry solver tolerance
      DOUBLE PRECISION, PARAMETER:: TOL=1E-5
! Chemistry solver Max iterations
      INTEGER, PARAMETER:: ITER_MAX = 500
! Verbosity of solver
      LOGICAL:: VERBOSE = .FALSE.

!--------------------------------------------------------------------//

      TICK = WALL_TIME()

! Loop through fluid cells, and solve for equilibrium mass fractions of
! the 9 species involved in the aqueous CO2-MEA solution.
      DO IJK=IJKSTART3,IJKEND3
         IF(FLUID_AT(IJK)) THEN

! If liquid volume fraction is very small (or zero) ignore this cell
            IF(ep_s(ijk,index_liq) < V_LIMITER) CYCLE

! For 1 cell, let the chem solver write some extra debug info
            IF (IMAX==1 .AND. JMAX==1 .AND. KMAX==1) THEN
               VERBOSE = .TRUE.
            ENDIF

! Liquid mass Fractions
            X_EQ = X_S(IJK,index_liq,:)

! Liquid temperature
            T_EQ = T_s(IJK,index_liq)

! Liquid specific heat, normalized by Gas Constant
! Calculate Cp/R in units of [mol/kg] needed by solver
            if(UNITS == 'SI') then
               CP_hat = C_PS(IJK,index_liq) / GAS_CONST*1000.0
            else
               CP_hat = C_PS(IJK,index_liq) / GAS_CONST_cal*1000.0
            endif

! Solve for the equilibrium speciation and temperature.
            call SOLVE_CHEM_EQ(X_EQ,T_EQ,CP_hat,ISOTHERMAL_EQ &
                  ,TOL,ITER_MAX,VERBOSE,SUCCESS)

! Check for errors before updating mass fractions:
! Setting ABORT =.FALSE. below, will allow the solver to bypass errors,
! without updating mass fractions/temperature.
            if (SUCCESS==0)  then
               X_S(IJK,index_liq,1:NS) = X_EQ(1:NS)
               T_s(IJK,index_liq) = T_EQ
            else
               WRITE(ERR_MSG, 6001) IJK &
                  ,trim(NEWTON_RAPHSON_ERRORS(SUCCESS))
               IF(SUCCESS==3) then
                  CALL LOG_ERROR()
               ELSE
                  call log_info()
               ENDIF
6001 FORMAT('6001: FATAL CHEM SOLVER SOLVER ERROR: IJK=',i6,' ',a)
            endif

         ENDIF
      ENDDO

! Update the cpu consumed by chem solver:
      TOCK = WALL_TIME()
      CPU_CHEM = CPU_CHEM + ( TOCK - TICK)
      call global_max_0d(CPU_CHEM,MAX_CPU_CHEM,PE_IO)
      IF(MYPE==PE_IO .AND. TOCK /= WALL_START) THEN
         write(*,1101)'Equilibrium Chem. CPU [Sec, % of MFIX] = ['&
            ,MAX_CPU_CHEM,'s, '&
            ,MAX_CPU_CHEM/(TOCK-WALL_START)*100.0,'%]'
1101 FORMAT(a,es12.4,a,f6.2,a)
      ENDIF

      RETURN
      END SUBROUTINE CHEM_SOLVE_MAIN
