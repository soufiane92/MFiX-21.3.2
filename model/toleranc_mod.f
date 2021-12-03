!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: tolerance                                              C
!  Purpose: Specify all tolerance parameters                           C
!                                                                      C
!  Author: M. Syamlal                                 Date: 24-JUL-92  C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      MODULE toleranc

      use param1, only: one

! Minimum value of solids volume fraction tracked
! As of 20.4 release, ZERO_EP_s is now a keyword with default value of 1.0D-8
      DOUBLE PRECISION :: ZERO_EP_s

! Small value for species mass fraction for disregarding residual
! calculation
! As of 20.4 release, ZERO_X_gs is now a keyword with default value of 1.0D-7
      DOUBLE PRECISION :: ZERO_X_gs

! Dilute flow threshold. When the volume fraction of a certain phase
! in a cell is smaller than this value the momentum equation for that
! phase is not solved in the cell.
! As of 20.4 release, DIL_EP_S is now a keyword with default value of 1.0D-4
      DOUBLE PRECISION :: DIL_EP_s

! Tolerance used for comparing two numbers for equality in function
! compare(a, b)
      DOUBLE PRECISION, PARAMETER :: TOL_COM = 1.0D-4

! Upper bound for temperatures
! As of 21.3 release, TMAX is now a keyword with default value of 4000.0D0
      DOUBLE PRECISION :: TMAX

! Lower bound for temperatures
! As of 21.3 release, TMIN is now a keyword with default value of 250.0D0
      DOUBLE PRECISION :: TMIN

! Reciprocal of a maximum molecular weight
      DOUBLE PRECISION, PARAMETER :: oMW_MAX = (ONE/500.D0)

! Maximum value of velocities set to avoid divergence problems.
      DOUBLE PRECISION :: MAX_INLET_VEL

! User definable factor used to scale MAX_INLET_VEL. Default value is 1.
      DOUBLE PRECISION :: MAX_INLET_VEL_FAC

! Maximum allowed velocity of gas or solids in case no inlet velocities
! (or zero velocities) are defined at inlet (see function
! check_vel_bound)
      DOUBLE PRECISION, PARAMETER :: MAX_ALLOWED_VEL = 500.0D+2

! The following quantities can be specified through the input data
! file, with namelist inputs of the same name.
! ------------------------------------------------------------------->>
! Tolerance in residuals allowed for convergence
      DOUBLE PRECISION :: TOL_RESID

! Tolerance in energy eq residuals allowed for convergence
      DOUBLE PRECISION :: TOL_RESID_T

! Tolerance in species eq residuals allowed for convergence
      DOUBLE PRECISION :: TOL_RESID_X

! Tolerance in scalr eq residuals allowed for convergence
      DOUBLE PRECISION :: TOL_RESID_Scalar

! Tolerance in K & Epsilon eq residuals allowed for convergence
      DOUBLE PRECISION :: TOL_RESID_K_Epsilon

! Tolerance in Granular Temperature eq residuals allowed for convergence
      DOUBLE PRECISION :: TOL_RESID_Th

! Minimum residual for declaring divergence
      DOUBLE PRECISION :: TOL_DIVERGE

! Absolute tolerance in sum of residuals, used to accelerate convergence at steady state
      DOUBLE PRECISION :: TOL_SUM_RESID_ABS

! Factor for normalizing the residual of gas cont. eq.
      DOUBLE PRECISION :: NORM_g

! Factor for normalizing the residual of solids cont. eq.
      DOUBLE PRECISION :: NORM_s

! Denominator used in normalization of epp residual
      DOUBLE PRECISION:: epp_den

! Denominator used in normalization of ppg residual
      DOUBLE PRECISION:: ppg_den
! -------------------------------------------------------------------<<

! Detect negative Rho_g in physical_prop to reduce DT in iterate
      LOGICAL :: Neg_RHO_G = .FALSE.

      CONTAINS


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Purpose: Test if two small values are nearly equal                  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

      LOGICAL FUNCTION COMPARE (V1, V2)

      use param1, only: small_number, one
      IMPLICIT NONE

! Dummy arguments
! -------------------------------------------------------------------//
! Values to be compared
      DOUBLE PRECISION, INTENT(IN) :: V1, V2

      IF (ABS(V1) <= SMALL_NUMBER) THEN
         IF (ABS(V2) <= SMALL_NUMBER) THEN
            COMPARE = .TRUE.
         ELSE
            COMPARE = .FALSE.
         ENDIF
      ELSE
         IF (ABS(V2/V1 - ONE) <= TOL_COM) THEN
            COMPARE = .TRUE.
         ELSE
            COMPARE = .FALSE.
         ENDIF
      ENDIF
      RETURN
      END FUNCTION COMPARE

      LOGICAL FUNCTION COMPARE_INT (V1, V2)

      IMPLICIT NONE

! Dummy arguments
! -------------------------------------------------------------------//
! Values to be compared
      INTEGER, INTENT(IN) :: V1, V2

      IF (V1 == V2 ) THEN
         COMPARE_INT = .TRUE.
      ELSE
         COMPARE_INT = .FALSE.
      ENDIF
      RETURN
      END FUNCTION COMPARE_INT

      LOGICAL FUNCTION COMPARE_BOOL (V1, V2)

      IMPLICIT NONE

! Dummy arguments
! -------------------------------------------------------------------//
! Values to be compared
      LOGICAL, INTENT(IN) :: V1, V2

      IF (V1.eqv.V2 ) THEN
         COMPARE_BOOL = .TRUE.
      ELSE
         COMPARE_BOOL = .FALSE.
      ENDIF
      RETURN
      END FUNCTION COMPARE_BOOL

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Purpose: Test if given variable is smaller than specified tolerance !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

      LOGICAL FUNCTION IS_SMALL (V, TOL)

      use compar, only: ijkstart3, ijkend3
      use functions, only: fluid_at
      use param, only: dimension_3
      IMPLICIT NONE

! Dummy arguments
!--------------------------------------------------------------------//
! Tolerance value for small
      DOUBLE PRECISION, INTENT(IN) :: TOL
! Field variable array
      DOUBLE PRECISION, INTENT(IN), DIMENSION(DIMENSION_3) :: V

! Local variables
!--------------------------------------------------------------------//
      INTEGER :: IJK

      IS_SMALL = .FALSE.
      DO IJK = ijkstart3, ijkend3
         IF (FLUID_AT(IJK)) THEN
            IF (ABS(V(IJK)) > TOL) RETURN
         ENDIF
      END DO
      IS_SMALL = .TRUE.

      RETURN
      END FUNCTION IS_SMALL

      END MODULE toleranc
