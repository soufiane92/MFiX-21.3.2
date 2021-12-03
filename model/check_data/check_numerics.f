#include "error.inc"

MODULE CHECK_NUMERICS_MOD

   use error_manager

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_NUMERICS                                          !
!  Purpose: Check the numerics control namelist section                !
!                                                                      !
!  Author: P. Nicoletti                               Date: 27-NOV-91  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE CHECK_NUMERICS


! Global Variables:
!---------------------------------------------------------------------//
! Flag: Use the Chi-scheme
      use run, only: CHI_SCHEME
! Flag: Set optimal LEQ solver parameters for parallel runs.
      use leqsol, only: OPT_PARALLEL
! Discretization scheme for various equations
      USE run, only: DISCRETIZE, SHEAR
! Solve system transpose
      use leqsol, only: DO_TRANSPOSE
! Minimize dot products in BiCGSTAB
      use leqsol, only: MINIMIZE_DOTPRODUCTS
! Report solver stats.
      use leqsol, only: SOLVER_STATISTICS
! Controls reduction of global sums for residuals.
      use run, only: DEBUG_RESID
! Linear equation, preconditioner sweep method.
      use leqsol, only: LEQ_SWEEP
! Linear equation solution method.
      use leqsol, only: LEQ_METHOD
! Calculate dot-products more efficiently for serial runs.
      use parallel, only: IS_SERIAL

      use param, only: dim_eqs

      use param1, only: zero

! tmin and tmax
      use toleranc, only: tmin, tmax

! Global Parameters:
!---------------------------------------------------------------------//
! NONE

! Global Module procedures:
!---------------------------------------------------------------------//
      use error_manager

! Skip data check when doing pre-processing only
      USE run, only:ppo


      IMPLICIT NONE


! Local Variables:
!---------------------------------------------------------------------//
! Loop counter
      INTEGER :: L

!......................................................................!

      IF(PPO) RETURN

      DO L = 1,DIM_EQS
         IF(DISCRETIZE(L) > 9 .OR. DISCRETIZE(L) < 0) THEN
            WRITE(ERR_MSG,2002) trim(ivar('DISCRETIZE',L)),&
               trim(ival(DISCRETIZE(L)))
            CALL LOG_ERROR()
         ENDIF
      ENDDO
 2002 FORMAT('Error 2002: Invalid option ', A,' = ', A, '.',/  &
         'Please correct the project settings.')

! Check chi scheme requirements.
      IF(CHI_SCHEME)THEN
         IF(DISCRETIZE(7) .NE. 3 .AND. DISCRETIZE(7).NE.6) THEN
            WRITE(ERR_MSG,2001)
            CALL LOG_ERROR()
 2001 FORMAT('Error 2001: CHI_SCHEME for species equations is only ',  &
         'implemented',/'for SMART and MUSCL discretization schemes ', &
         '[DISCRTIZE(7)].',/'Please correct the project settings.')
         ENDIF
         IF (SHEAR) THEN
            WRITE(ERR_MSG,2003)
            CALL LOG_ERROR()
 2003 FORMAT('Error 2003: CHI_SCHEME is currently not implemented ',   &
         'with SHEAR ',/'option. See calc_chi in module chischeme for '&
         'details.',/'Please correct the project settings.')
         ENDIF
      ENDIF


! Set the optimizations for DMP runs.
      IF (OPT_PARALLEL) THEN
         IS_SERIAL = .FALSE.
         DO_TRANSPOSE = .FALSE.
         MINIMIZE_DOTPRODUCTS = .TRUE.
         SOLVER_STATISTICS = .TRUE.
         DEBUG_RESID = .FALSE.
         LEQ_SWEEP(1:2) = 'ASAS'
         LEQ_METHOD(1:2) = 2
         LEQ_METHOD(3:9) = 1
      ENDIF

      IF(TMIN <= ZERO)THEN
         WRITE(ERR_MSG, 1001) 'TMIN', iVal_dbl(TMIN)
         CALL LOG_ERROR()
      ENDIF

      IF(TMAX <= ZERO)THEN
         WRITE(ERR_MSG, 1001) 'TMAX', iVal_dbl(TMAX)
         CALL LOG_ERROR()
      ENDIF

      IF(TMAX <= TMIN)THEN
         WRITE(ERR_MSG, 1002) ival_dbl(TMIN),  iVal_dbl(TMAX)
         CALL LOG_ERROR()
      ENDIF

 1001 FORMAT('Error 1001: Illegal or unphysical input: ',A,' = ',A,/   &
             'Please correct the project settings.')
 1002 FORMAT('Error 1002: Illegal input: TMIN must be less than TMAX.',/ &
             'Currently, TMIN = ',A,/'           TMAX = ',A,/ &
             'Please correct the project settings.')
      RETURN

   END SUBROUTINE CHECK_NUMERICS

END MODULE CHECK_NUMERICS_MOD
