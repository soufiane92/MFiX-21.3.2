#include "error.inc"

MODULE DISPLAY_RESID_MOD

   use error_manager
   use param1, only: UNDEFINED_I
   use residual, only: HYDRO_GRP, THETA_GRP, ENERGY_GRP
   use residual, only: RESID_GRP, RESID_GRP_STRING
   use residual, only: RESID_STRING, RESID_INDEX, RESID
   use residual, only: SPECIES_GRP, SCALAR_GRP, KE_GRP
   use residual_pub, only: GROUP_RESID
   use run, only: ANY_SPECIES_EQ, GRANULAR_ENERGY, ENERGY_EQ
   use scalars, only: NScalar
   use turb, only: K_EPSILON

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: DISPLAY_RESID(NIT, IER)                                !
!  Author: M. Syamlal                                 Date: 8-JUL-96   !
!                                                                      !
!  Purpose: Display residuals                                          !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE DISPLAY_RESID(NIT)

      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------//
! iteration number
      INTEGER, INTENT(IN) :: NIT
!---------------------------------------------------------------------//

! Print Location of Max_Resid
!      LOGICAL,PARAMETER:: Print_ijk=.FALSE.

      IF (GROUP_RESID) THEN
         CALL DISPLAY_GROUP_RESID(NIT)
      ELSE
         CALL DISPLAY_FIELD_RESID(NIT)
      ENDIF
!     IF(PRINT_IJK)

! Display maximum values of residuals
!     IF(PRINT_IJK) WRITE(*,'(A, G12.3, 3I6, A, G12.3, 3I6, A, G12.3)') &
!     & " Max Res/IJK: P_g: ", MAX_RESID(RESID_P, 0), &
!     & I_OF_G(IJK_RESID(RESID_P, 0)), &
!     & J_OF_G(IJK_RESID(RESID_P, 0)), &
!     & K_OF_G(IJK_RESID(RESID_P, 0)), &
!     & " P_s: ", MAX_RESID(RESID_p, 1), &
!     & I_OF_G(IJK_RESID(RESID_p, 1)), &
!     & J_OF_G(IJK_RESID(RESID_p, 1)), &
!     & K_OF_G(IJK_RESID(RESID_p, 1)), &
!     & " P_star=",  P_star(IJK_RESID(RESID_p, 1))
!
      RETURN

   END SUBROUTINE DISPLAY_RESID

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: DISPLAY_FIELD_RESID(NIT, IER)                          !
!  Author: M. Syamlal                                 Date: 8-JUL-96   !
!                                                                      !
!  Purpose: Display residuals for each field variable.                 !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE DISPLAY_FIELD_RESID(NIT)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NIT

! Local variables
!---------------------------------------------------------------------//
      INTEGER :: LL, LC, LS, LE
!---------------------------------------------------------------------//

      IF (NIT == 1) THEN
         WRITE (ERR_MSG(1) (1:5), '("  Nit")')
         LC = 1
         DO LL = 1, 8
            IF (RESID_INDEX(LL, 1) /= UNDEFINED_I) THEN
               LS = 6 + 10*(LC - 1)
               LE = 5 + 10*(LC)
               WRITE (ERR_MSG(1) (LS:LE), '(5X,A4)') RESID_STRING(LL)
               LC = LC + 1
            ENDIF
         END DO
         IF (RESID_INDEX(8, 1) == UNDEFINED_I) THEN
            LS = 6 + 10*(LC - 1)
            LE = 5 + 10*(LC)
            WRITE (ERR_MSG(1) (LS:LE), '(2X,A7)') 'Max res'
         ENDIF
         CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_STATUS, header=.false., footer=.false., log=.false.)
      ENDIF

      WRITE (ERR_MSG(1) (1:5), '(I5)') NIT
      LC = 1
      DO LL = 1, 8
         IF (RESID_INDEX(LL, 1) /= UNDEFINED_I) THEN
            LS = 6 + 10*(LC - 1)
            LE = 5 + 10*(LC)
            WRITE (ERR_MSG(1) (LS:LE), '(2X,1PG8.1)') &
               RESID(RESID_INDEX(LL, 1), RESID_INDEX(LL, 2))
            LC = LC + 1
         ENDIF
      ENDDO
      IF (RESID_INDEX(8, 1) == UNDEFINED_I) THEN
         LS = 6 + 10*(LC - 1)
         LE = 3 + 10*(LC)
         WRITE (ERR_MSG(1) (LS:LE), '(4X,A4)') RESID_STRING(8)
      ENDIF
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_STATUS, header=.false., footer=.false., log=.false.)

      RETURN

   END SUBROUTINE DISPLAY_FIELD_RESID

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: DISPLAY_GROUP_RESID(NIT, IER)                          !
!  Author: M. Syamlal                                 Date: 8-JUL-96   !
!                                                                      !
!  Purpose: Display residuals grouped by equation type.                !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE DISPLAY_GROUP_RESID(nit)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NIT

! Local variables
!---------------------------------------------------------------------//
      INTEGER :: LC, LS, LE
!---------------------------------------------------------------------//

      IF (NIT == 1) THEN
         WRITE (ERR_MSG(1) (1:5), '("  Nit")')
         LC = 1

         LS = 6 + 10*(LC - 1)
         LE = 5 + 10*(LC)
         WRITE (ERR_MSG(1) (LS:LE), 1000) RESID_GRP_STRING(HYDRO_GRP)
         LC = LC + 1

         IF (GRANULAR_ENERGY) THEN
            LS = 6 + 10*(LC - 1)
            LE = 5 + 10*(LC)
            WRITE (ERR_MSG(1) (LS:LE), 1000) RESID_GRP_STRING(THETA_GRP)
            LC = LC + 1
         ENDIF

         IF (ENERGY_EQ) THEN
            LS = 6 + 10*(LC - 1)
            LE = 5 + 10*(LC)
            WRITE (ERR_MSG(1) (LS:LE), 1000) RESID_GRP_STRING(ENERGY_GRP)
            LC = LC + 1
         ENDIF

         IF (ANY_SPECIES_EQ) THEN
            LS = 6 + 10*(LC - 1)
            LE = 5 + 10*(LC)
            WRITE (ERR_MSG(1) (LS:LE), 1000) RESID_GRP_STRING(SPECIES_GRP)
            LC = LC + 1
         ENDIF

         IF (NScalar > 0) THEN
            LS = 6 + 10*(LC - 1)
            LE = 5 + 10*(LC)
            WRITE (ERR_MSG(1) (LS:LE), 1000) RESID_GRP_STRING(SCALAR_GRP)
            LC = LC + 1
         ENDIF

         IF (K_EPSILON) THEN
            LS = 6 + 10*(LC - 1)
            LE = 5 + 10*(LC)
            WRITE (ERR_MSG(1) (LS:LE), 1000) RESID_GRP_STRING(KE_GRP)
            LC = LC + 1
         ENDIF

         LS = 6 + 10*(LC - 1)
         LE = 5 + 10*(LC)
         WRITE (ERR_MSG(1) (LS:LE), 1000) 'Max res '

         CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_STATUS, header=.false., footer=.false., log=.false.)
      ENDIF

1000  FORMAT(3X, A7)

      WRITE (ERR_MSG(1) (1:5), '(I5)') NIT
      LC = 1

      LS = 6 + 10*(LC - 1)
      LE = 5 + 10*(LC)
      WRITE (ERR_MSG(1) (LS:LE), 1100) RESID_GRP(HYDRO_GRP)
      LC = LC + 1

      IF (GRANULAR_ENERGY) THEN
         LS = 6 + 10*(LC - 1)
         LE = 5 + 10*(LC)
         WRITE (ERR_MSG(1) (LS:LE), 1100) RESID_GRP(THETA_GRP)
         LC = LC + 1
      ENDIF

      IF (ENERGY_EQ) THEN
         LS = 6 + 10*(LC - 1)
         LE = 5 + 10*(LC)
         WRITE (ERR_MSG(1) (LS:LE), 1100) RESID_GRP(ENERGY_GRP)
         LC = LC + 1
      ENDIF

      IF (ANY_SPECIES_EQ) THEN
         LS = 6 + 10*(LC - 1)
         LE = 5 + 10*(LC)
         WRITE (ERR_MSG(1) (LS:LE), 1100) RESID_GRP(SPECIES_GRP)
         LC = LC + 1
      ENDIF

      IF (NScalar > 0) THEN
         LS = 6 + 10*(LC - 1)
         LE = 5 + 10*(LC)
         WRITE (ERR_MSG(1) (LS:LE), 1100) RESID_GRP(SCALAR_GRP)
         LC = LC + 1
      ENDIF

      IF (K_EPSILON) THEN
         LS = 6 + 10*(LC - 1)
         LE = 5 + 10*(LC)
         WRITE (ERR_MSG(1) (LS:LE), 1100) RESID_GRP(KE_GRP)
         LC = LC + 1
      ENDIF

      LS = 6 + 10*(LC - 1)
      LE = 3 + 10*(LC)
      WRITE (ERR_MSG(1) (LS:LE), '(4X,A4)') RESID_STRING(8)

1100  FORMAT(2x, 1PG8.1)

      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_STATUS, header=.false., footer=.false., log=.false.)

      RETURN
   END SUBROUTINE DISPLAY_GROUP_RESID

END MODULE DISPLAY_RESID_MOD
