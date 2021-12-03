#include "error.inc"

MODULE CHECK_INTERNAL_SURFACES_MOD

   use error_manager
   use get_is_mod, only: get_is

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_INTERNAL_SURFACES                                 !
!  Author: M. Syamlal                                 Date: 21-OCT-92  !
!                                                                      !
!  Purpose: Check internal surface specifications, and convert         !
!           physical locations to i, j, k's.                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE CHECK_INTERNAL_SURFACES


! Global Variables:
!---------------------------------------------------------------------//
! Type of internal surface.
      use is, only: IS_TYPE
! Flag: IS is specified.
      use is, only: IS_DEFINED

! Global Parameters:
!---------------------------------------------------------------------//
! Maximum number of ISs that can be specified.
      USE param, only: DIMENSION_IS

! Skip data check when doing pre-processing only
      USE run, only:ppo

      IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//
! loop/variable indices
      INTEGER :: ISV
!......................................................................!

      IF(PPO) RETURN

      CALL CHECK_IS_GEOMETRY

      DO ISV=1, DIMENSION_IS

! Check that the input is valid.
         IF(IS_DEFINED(ISV)) THEN
! Convert spatial coordinates into I/J/K values.
            CALL GET_IS(ISV)
! Check that the required input is specified for all IS types.
            SELECT CASE(IS_TYPE(ISV))
            CASE('SEMIPERMEABLE')
               CALL CHECK_IS_SEMIPERMEABLE(ISV)
            CASE('X_SEMIPERMEABLE', &
                 'Y_SEMIPERMEABLE', &
                 'Z_SEMIPERMEABLE')
               CALL CHECK_IS_SEMIPERMEABLE(ISV)
            END SELECT
! Check that nothing is specified for undefined IS regions.
         ELSE
            CALL CHECK_IS_OVERFLOW(ISV)
         ENDIF
      ENDDO

      RETURN
      END SUBROUTINE CHECK_INTERNAL_SURFACES


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_IS_GEOMETRY                                       !
!  Author: M. Syamlal                                 Date: 21-OCT-92  !
!                                                                      !
!  Purpose: Verify that IS geometry is specified and that the IS_TYPE   !
!  is valid.                                                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_IS_GEOMETRY

      USE compar
      USE fldvar
      USE funits
      USE geometry
      USE indices
      USE is
      USE param
      USE param1
      USE physprop
      USE run

      IMPLICIT NONE

! Global Parameters:
!---------------------------------------------------------------------//

! Local Variables:
!---------------------------------------------------------------------//
! Loop/Variable indices
      INTEGER :: ISV, I
! Total number of valid IS types
      INTEGER, PARAMETER :: DIM_ISTYPE = 5
! Valid internal surface types
      CHARACTER(LEN=16), DIMENSION(1:DIM_ISTYPE) :: VALID_IS_TYPE = (/&
         'IMPERMEABLE     ', 'IP              ',&
         'SEMIPERMEABLE   ', 'SP              ',&
         'STL             '/)
!......................................................................!

! Initialize the ALL_IS flag.
      ANY_IS_DEFINED = .FALSE.

! DETERMINE WHICH INTERNAL SURFACE INDICES HAVE VALUES
      L50: DO ISV = 1, DIMENSION_IS

         IS_DEFINED(ISV) = .FALSE.
         IF (IS_X_W(ISV) /= UNDEFINED)   IS_DEFINED(ISV) = .TRUE.
         IF (IS_X_E(ISV) /= UNDEFINED)   IS_DEFINED(ISV) = .TRUE.
         IF (IS_Y_S(ISV) /= UNDEFINED)   IS_DEFINED(ISV) = .TRUE.
         IF (IS_Y_N(ISV) /= UNDEFINED)   IS_DEFINED(ISV) = .TRUE.
         IF (IS_Z_B(ISV) /= UNDEFINED)   IS_DEFINED(ISV) = .TRUE.
         IF (IS_Z_T(ISV) /= UNDEFINED)   IS_DEFINED(ISV) = .TRUE.
         IF (IS_I_W(ISV) /= UNDEFINED_I) IS_DEFINED(ISV) = .TRUE.
         IF (IS_I_E(ISV) /= UNDEFINED_I) IS_DEFINED(ISV) = .TRUE.
         IF (IS_J_S(ISV) /= UNDEFINED_I) IS_DEFINED(ISV) = .TRUE.
         IF (IS_J_N(ISV) /= UNDEFINED_I) IS_DEFINED(ISV) = .TRUE.
         IF (IS_K_B(ISV) /= UNDEFINED_I) IS_DEFINED(ISV) = .TRUE.
         IF (IS_K_T(ISV) /= UNDEFINED_I) IS_DEFINED(ISV) = .TRUE.

! Force 'STL" IS to appear as undefined because these are not
! regular internal surfaces and are not aligned with the mesh.
         IF(IS_TYPE(ISV)(1:3)=='STL') IS_DEFINED(ISV) = .FALSE.


         IF(.NOT.IS_DEFINED(ISV)) CYCLE L50

         ANY_IS_DEFINED = .TRUE.

         IF (IS_X_W(ISV)==UNDEFINED .AND. IS_I_W(ISV)==UNDEFINED_I) THEN
            IF (NO_I) THEN
               IS_X_W(ISV) = X_MIN
            ELSE
               WRITE(ERR_MSG,1101) ISV, 'IS_X_w and IS_I_w '
               CALL LOG_ERROR()
            ENDIF
         ENDIF

         IF(IS_X_E(ISV)==UNDEFINED .AND. IS_I_E(ISV)==UNDEFINED_I) THEN
            IF(NO_I) THEN
               IS_X_E(ISV) = X_MAX
            ELSE
               WRITE(ERR_MSG,1101) ISV, 'IS_X_e and IS_I_e '
               CALL LOG_ERROR()
            ENDIF
         ENDIF
         IF(IS_Y_S(ISV)==UNDEFINED .AND. IS_J_S(ISV)==UNDEFINED_I) THEN
            IF(NO_J) THEN
               IS_Y_S(ISV) = Y_MIN
            ELSE
               WRITE(ERR_MSG,1101) ISV, 'IS_Y_s and IS_J_s '
               CALL LOG_ERROR()
            ENDIF
         ENDIF
         IF(IS_Y_N(ISV)==UNDEFINED .AND. IS_J_N(ISV)==UNDEFINED_I) THEN
            IF(NO_J) THEN
               IS_Y_N(ISV) = Y_MAX
            ELSE
               WRITE(ERR_MSG,1101) ISV, 'IS_Y_n and IS_J_n '
               CALL LOG_ERROR()
            ENDIF
         ENDIF
         IF(IS_Z_B(ISV)==UNDEFINED .AND. IS_K_B(ISV)==UNDEFINED_I) THEN
            IF(NO_K) THEN
               IS_Z_B(ISV) = Z_MIN
            ELSE
               WRITE(ERR_MSG,1101) ISV, 'IS_Z_b and IS_K_b '
               CALL LOG_ERROR()
            ENDIF
         ENDIF
         IF(IS_Z_T(ISV)==UNDEFINED .AND. IS_K_T(ISV)==UNDEFINED_I) THEN
            IF(NO_K) THEN
               IS_Z_T(ISV) = Z_MAX
            ELSE
               WRITE(ERR_MSG,1101) ISV, 'IS_Z_t and IS_K_t '
               CALL LOG_ERROR()
            ENDIF
         ENDIF

 1101 FORMAT('Error 1101: Internal surface ',I3,' is ill-defined.',/   &
         A,' are not specified.',/'Please correct the project settings.')


         DO I = 1, DIM_ISTYPE
            IF(VALID_IS_TYPE(I) == IS_TYPE(ISV)) THEN
               IF(MOD(I,2) == 0) IS_TYPE(ISV) = VALID_IS_TYPE(I-1)
               CYCLE L50
            ENDIF
            IF(VALID_IS_TYPE(I) == IS_TYPE(ISV)(3:16)) THEN
               IF(MOD(I,2) == 0) IS_TYPE(ISV)(3:16) = VALID_IS_TYPE(I-1)

               SELECT CASE(IS_TYPE(ISV)(1:1))
               CASE('X', 'Y', 'Z'); CYCLE L50
               CASE DEFAULT
                  WRITE(ERR_MSG, 1102) ISV, IS_TYPE(ISV)(1:1)
                  CALL LOG_ERROR()
               END SELECT

 1102 FORMAT('Error 1102: Internal surface ',I3,' has an invalid ',&
         'prefix: ',A,/'Please correct the project settings.')

            ENDIF
         END DO
         WRITE(ERR_MSG, 1103) trim(iVar('IS_TYPE',ISV)),               &
            trim(IS_TYPE(ISV)), VALID_IS_TYPE
         CALL LOG_ERROR()

 1103 FORMAT('Error 1103: Illegal entry: ',A,' = ',A,/'Valid entries:',&
         ' ',4(/5X,A,2x,A))

      ENDDO L50

      RETURN
      END SUBROUTINE CHECK_IS_GEOMETRY



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_IS_SEMIPERMEABLE                                  !
!  Author: J.Musser                                   Date: 19-MAR-14  !
!                                                                      !
!  Purpose: Check that required input for semipermeable internal       !
!  surfaces is specified.                                              !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_IS_SEMIPERMEABLE(ISV)


! Global Variables:
!---------------------------------------------------------------------//
! Permeability coefficients for semipermeable internal surfaces.
      USE is, only: IS_PC

! Global Parameters:
!---------------------------------------------------------------------//
      USE param1, only: ZERO, UNDEFINED

      IMPLICIT NONE


! Dummy Arguments:
!---------------------------------------------------------------------//
! Internal surface index
      INTEGER, INTENT(in) :: ISV
!......................................................................!

! Check that the Darcy coefficient is specified and valid.
      IF(IS_PC(ISV,1) == UNDEFINED) THEN
         WRITE(ERR_MSG, 1000) trim(iVar('IS_PC',ISV,1))
         CALL LOG_ERROR()
      ENDIF

      IF(IS_PC(ISV,1) == ZERO) THEN
         WRITE(ERR_MSG, 1001) trim(iVar('IS_PC',ISV,1)), '0.0'
         CALL LOG_ERROR()
      ENDIF

! Check that the inertial resistance factor is specified.
      IF(IS_PC(ISV,2) == UNDEFINED) THEN
         WRITE(ERR_MSG, 1000) trim(iVar('IS_PC',ISV,2))
         CALL LOG_ERROR()
      ENDIF

      RETURN

 1000 FORMAT('Error 1000: Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

 1001 FORMAT('Error 1001: Illegal or unknown input: ',A,' = ',A,/   &
         'Please correct the project settings.')

      END SUBROUTINE CHECK_IS_SEMIPERMEABLE




!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_IS_OVERFLOW                                       !
!  Author: J.Musser                                   Date: 19-MAR-14  !
!                                                                      !
!  Purpose: Check internal surface specifications are not specified for !
!  ISs that are not defined.                                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_IS_OVERFLOW(ISV)


! Global Variables:
!---------------------------------------------------------------------//
! Permeability coefficients for semipermeable internal surfaces.
      USE is, only: IS_PC

! Global Parameters:
!---------------------------------------------------------------------//
      USE param1, only: ZERO, UNDEFINED

      IMPLICIT NONE

! Dummy Arguments:
!---------------------------------------------------------------------//
! Internal surface index.
      INTEGER, INTENT(in) :: ISV
!......................................................................!

! Check that the Darcy coefficient is undefined.
      IF(IS_PC(ISV,1) /= UNDEFINED) THEN
         WRITE(ERR_MSG, 1100) trim(iVar('IS_PC',ISV,1))
         CALL LOG_ERROR()
      ENDIF

! Check that the inertial resistance factor is undefined.
      IF(IS_PC(ISV,2) /= ZERO) THEN
         WRITE(ERR_MSG, 1100) trim(iVar('IS_PC',ISV,2))
         CALL LOG_ERROR()
      ENDIF

      RETURN

 1100 FORMAT('Error 1100: ',A,' specified in an undefined IS region')

   END SUBROUTINE CHECK_IS_OVERFLOW

END MODULE CHECK_INTERNAL_SURFACES_MOD
