#include "error.inc"

MODULE CHECK_BC0_FLOW_MOD

   use error_manager

CONTAINS


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
! Subroutine: CHECK_BC_VEL_INFLOW                                      !
! Author: J.Musser                                    Date: 01-Mar-14  !
!                                                                      !
! Purpose: Provided a check on velocity specification for MI/MO        !
! boundaries                                                           !
!                                                                      !
! Comments:                                                            !
!     The velocities at the inflow face are fixed and the momentum     !
!     equations are not solved in the inflow cells.                    !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_BC_VEL_INFLOW(M_TOT, SKIP, BCV)

      USE param, only: DIM_M
      USE param1, only: ZERO
      USE param1, only: UNDEFINED

      use physprop, only: ro_g0

      use geometry, only: NO_I
      use geometry, only: NO_J
      use geometry, only: NO_K

      use bc

      IMPLICIT NONE

      INTEGER, INTENT(in) :: BCV
      INTEGER, INTENT(in) :: M_TOT

      LOGICAL, INTENT(in) :: SKIP(DIM_M)

! loop/variable indices
      INTEGER :: M

      IF(RO_G0==ZERO) THEN
         BC_U_G(BCV) = ZERO
         BC_V_G(BCV) = ZERO
         BC_W_G(BCV) = ZERO
      ENDIF
! Check that gas phase velocities are defined.
      IF(BC_U_G(BCV) == UNDEFINED) THEN
         IF(NO_I) THEN
            BC_U_G(BCV) = ZERO
         ELSE
            WRITE(ERR_MSG,1000) trim(iVar('BC_U_g',BCV))
            CALL LOG_ERROR()
         ENDIF
      ENDIF

      IF (BC_V_G(BCV) == UNDEFINED) THEN
         IF (NO_J) THEN
            BC_V_G(BCV) = ZERO
         ELSE
            WRITE(ERR_MSG,1000) trim(iVar('BC_V_g',BCV))
            CALL LOG_ERROR()
         ENDIF
      ENDIF

      IF(BC_W_G(BCV) == UNDEFINED) THEN
         IF (NO_K) THEN
            BC_W_G(BCV) = ZERO
         ELSE
            WRITE(ERR_MSG,1000) trim(iVar('BC_W_g',BCV))
            CALL LOG_ERROR()
         ENDIF
      ENDIF

! Check that solids phase velocities are defined.
      DO M = 1, M_TOT
         IF(BC_U_S(BCV,M) == UNDEFINED) THEN
            IF(SKIP(M) .OR. NO_I) THEN
               BC_U_S(BCV,M) = ZERO
            ELSE
               WRITE(ERR_MSG,1000) trim(iVar('BC_U_s',BCV,M))
               CALL LOG_ERROR()
            ENDIF
         ENDIF

         IF(BC_V_S(BCV,M) == UNDEFINED) THEN
            IF(SKIP(M) .OR. NO_J) THEN
               BC_V_S(BCV,M) = ZERO
            ELSE
               WRITE(ERR_MSG,1000) trim(iVar('BC_V_s',BCV,M))
               CALL LOG_ERROR()
            ENDIF
         ENDIF

         IF(BC_W_S(BCV,M) == UNDEFINED) THEN
            IF(SKIP(M) .OR. NO_K) THEN
               BC_W_S(BCV,M) = ZERO
            ELSE
               WRITE(ERR_MSG,1000) trim(iVar('BC_W_s',BCV,M))
               CALL LOG_ERROR()
            ENDIF
         ENDIF
      ENDDO

! Check that gas phase velocities are consistent.
      SELECT CASE (BC_PLANE(BCV))

      CASE ('W')
         IF(BC_U_G(BCV) > ZERO) THEN
            WRITE(ERR_MSG,1300) trim(iVar('BC_U_g',BCV)), '<'
            CALL LOG_ERROR()
         ENDIF
         DO M = 1, M_TOT
            IF(BC_U_S(BCV,M) > ZERO) THEN
               WRITE(ERR_MSG, 1300) trim(iVar('BC_U_s',BCV,M)), '<'
               CALL LOG_ERROR()
            ENDIF
         ENDDO

      CASE('E')
         IF(BC_U_G(BCV) < ZERO) THEN
            WRITE(ERR_MSG,1300) trim(iVar('BC_U_g',BCV)), '>'
            CALL LOG_ERROR()
         ENDIF
         DO M = 1, M_TOT
            IF(BC_U_S(BCV,M) < ZERO) THEN
               WRITE(ERR_MSG, 1300) trim(iVar('BC_U_s',BCV,M)), '>'
              CALL LOG_ERROR()
            ENDIF
         ENDDO

      CASE('S')
         IF(BC_V_G(BCV) > ZERO) THEN
            WRITE(ERR_MSG,1300) trim(iVar('BC_V_g',BCV)), '<'
            CALL LOG_ERROR()
         ENDIF
         DO M = 1, M_TOT
            IF(BC_V_S(BCV,M) > ZERO) THEN
               WRITE(ERR_MSG, 1300) trim(iVar('BC_V_s',BCV,M)), '<'
               CALL LOG_ERROR()
            ENDIF
         ENDDO

      CASE('N')
         IF(BC_V_G(BCV) < ZERO) THEN
            WRITE(ERR_MSG,1300) trim(iVar('BC_V_g',BCV)), '>'
            CALL LOG_ERROR()
         ENDIF
         DO M = 1, M_TOT
            IF(BC_V_S(BCV,M) < ZERO) THEN
               WRITE(ERR_MSG, 1300) trim(iVar('BC_V_s',BCV,M)), '>'
               CALL LOG_ERROR()
            ENDIF
         ENDDO

      CASE('B')
         IF(BC_W_G(BCV) > ZERO) THEN
            WRITE(ERR_MSG,1300) trim(iVar('BC_W_g',BCV)), '<'
            CALL LOG_ERROR()
         ENDIF
         DO M = 1, M_TOT
            IF(BC_W_S(BCV,M) > ZERO) THEN
               WRITE(ERR_MSG, 1300) trim(iVar('BC_W_s',BCV,M)), '<'
               CALL LOG_ERROR()
            ENDIF
         ENDDO

      CASE('T')
         IF(BC_W_G(BCV) < ZERO) THEN
            WRITE(ERR_MSG,1300) trim(iVar('BC_W_g',BCV)), '>'
            CALL LOG_ERROR()
         ENDIF
         DO M = 1, M_TOT
            IF(BC_W_S(BCV,M) < ZERO) THEN
               WRITE(ERR_MSG, 1300) trim(iVar('BC_W_s',BCV,M)), '>'
               CALL LOG_ERROR()
            ENDIF
         ENDDO

      END SELECT

 1300 FORMAT('Error 1300: Invalid flow direction. ',A,' should be ',   &
         A,' zero.')

      RETURN

 1000 FORMAT('Error 1000: Required input not specified: ',A)

      END SUBROUTINE CHECK_BC_VEL_INFLOW


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
! Subroutine: CHECK_BC_VEL_OUTFLOW                                     !
! Author: J.Musser                                    Date: 01-Mar-14  !
!                                                                      !
! Purpose: Provided a detailed error message when the sum of volume    !
!                                                                      !
! Comments:                                                            !
!     The velocities at the outflow face are fixed and the momentum    !
!     equations are not solved in the outflow cells. Since the flow    !
!     is out of the domain none of the other scalars should need to    !
!     be specified (e.g., mass fractions, void fraction, etc.,).       !
!     Such values will become defined according to their adjacent      !
!     fluid cell                                                       !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_BC_VEL_OUTFLOW(M_TOT, SKIP, BCV)

      USE param
      USE param1
      USE geometry
      USE fldvar
      USE physprop
      USE run
      USE bc
      USE indices
      USE funits
      USE scalars
      USE compar
      USE sendrecv
      USE discretelement
      USE mfix_pic
      USE cutcell

      IMPLICIT NONE

! loop/variable indices
      INTEGER, intent(in) :: BCV
      INTEGER, intent(in) :: M_TOT
      LOGICAL, intent(in) :: SKIP(DIM_M)

! Loop variable
      INTEGER :: M

! Check that gas phase velocities are defined.
      IF(BC_U_G(BCV) == UNDEFINED) THEN
         IF(NO_I) THEN
            BC_U_G(BCV) = ZERO
         ELSE
            WRITE(ERR_MSG,1000) trim(iVar('BC_U_g',BCV))
            CALL LOG_ERROR()
         ENDIF
      ENDIF

      IF (BC_V_G(BCV) == UNDEFINED) THEN
         IF (NO_J) THEN
            BC_V_G(BCV) = ZERO
         ELSE
            WRITE(ERR_MSG,1000) trim(iVar('BC_V_g',BCV))
            CALL LOG_ERROR()
         ENDIF
      ENDIF

      IF(BC_W_G(BCV) == UNDEFINED) THEN
         IF (NO_K) THEN
            BC_W_G(BCV) = ZERO
         ELSE
            WRITE(ERR_MSG,1000) trim(iVar('BC_W_g',BCV))
            CALL LOG_ERROR()
         ENDIF
      ENDIF

! Check that solids phase velocities are defined.
      DO M = 1, M_TOT
         IF(BC_U_S(BCV,M) == UNDEFINED) THEN
            IF(SKIP(M) .OR. NO_I) THEN
               BC_U_S(BCV,M) = ZERO
            ELSE
               WRITE(ERR_MSG,1000) trim(iVar('BC_U_s',BCV,M))
               CALL LOG_ERROR()
            ENDIF
         ENDIF

         IF(BC_V_S(BCV,M) == UNDEFINED) THEN
            IF(SKIP(M) .OR. NO_J) THEN
               BC_V_S(BCV,M) = ZERO
            ELSE
               WRITE(ERR_MSG,1000) trim(iVar('BC_V_s',BCV,M))
               CALL LOG_ERROR()
            ENDIF
         ENDIF

         IF(BC_W_S(BCV,M) == UNDEFINED) THEN
            IF(SKIP(M) .OR. NO_K) THEN
               BC_W_S(BCV,M) = ZERO
            ELSE
               WRITE(ERR_MSG,1000) trim(iVar('BC_W_s',BCV,M))
               CALL LOG_ERROR()
            ENDIF
         ENDIF
      ENDDO


! Check that gas phase velocities are consistent.
      SELECT CASE (BC_PLANE(BCV))

      CASE ('W')
         IF(BC_U_G(BCV) < ZERO) THEN
            WRITE(ERR_MSG,1300) trim(iVar('BC_U_g',BCV)), '>'
            CALL LOG_ERROR()
         ENDIF
         DO M = 1, M_TOT
            IF(BC_U_S(BCV,M) < ZERO) THEN
               WRITE(ERR_MSG, 1300) trim(iVar('BC_U_s',BCV,M)), '>'
               CALL LOG_ERROR()
            ENDIF
         ENDDO

      CASE('E')
         IF(BC_U_G(BCV) > ZERO) THEN
            WRITE(ERR_MSG,1300) trim(iVar('BC_U_g',BCV)), '<'
            CALL LOG_ERROR()
         ENDIF
         DO M = 1, M_TOT
            IF(BC_U_S(BCV,M) > ZERO) THEN
               WRITE(ERR_MSG, 1300) trim(iVar('BC_U_s',BCV,M)), '<'
              CALL LOG_ERROR()
            ENDIF
         ENDDO

      CASE('S')
         IF(BC_V_G(BCV) < ZERO) THEN
            WRITE(ERR_MSG,1300) trim(iVar('BC_V_g',BCV)), '>'
            CALL LOG_ERROR()
         ENDIF
         DO M = 1, M_TOT
            IF(BC_V_S(BCV,M) < ZERO) THEN
               WRITE(ERR_MSG, 1300) trim(iVar('BC_V_s',BCV,M)), '>'
               CALL LOG_ERROR()
            ENDIF
         ENDDO

      CASE('N')
         IF(BC_V_G(BCV) > ZERO) THEN
            WRITE(ERR_MSG,1300) trim(iVar('BC_V_g',BCV)), '<'
            CALL LOG_ERROR()
         ENDIF
         DO M = 1, M_TOT
            IF(BC_V_S(BCV,M) > ZERO) THEN
               WRITE(ERR_MSG, 1300) trim(iVar('BC_V_s',BCV,M)), '<'
               CALL LOG_ERROR()
            ENDIF
         ENDDO

      CASE('B')
         IF(BC_W_G(BCV) < ZERO) THEN
            WRITE(ERR_MSG,1300) trim(iVar('BC_W_g',BCV)), '>'
            CALL LOG_ERROR()
         ENDIF
         DO M = 1, M_TOT
            IF(BC_W_S(BCV,M) < ZERO) THEN
               WRITE(ERR_MSG, 1300) trim(iVar('BC_W_s',BCV,M)), '>'
               CALL LOG_ERROR()
            ENDIF
         ENDDO

      CASE('T')
         IF(BC_W_G(BCV) > ZERO) THEN
            WRITE(ERR_MSG,1300) trim(iVar('BC_W_g',BCV)), '<'
            CALL LOG_ERROR()
         ENDIF
         DO M = 1, M_TOT
            IF(BC_W_S(BCV,M) > ZERO) THEN
               WRITE(ERR_MSG, 1300) trim(iVar('BC_W_s',BCV,M)), '<'
               CALL LOG_ERROR()
            ENDIF
         ENDDO

      END SELECT

 1300 FORMAT('Error 1300: Invalid flow direction. ',A,' should be ',   &
         A,' zero.')

      RETURN

 1000 FORMAT('Error 1000: Required input not specified: ',A)

   END SUBROUTINE CHECK_BC_VEL_OUTFLOW

END MODULE CHECK_BC0_FLOW_MOD
