MODULE INIT_RESID_MOD
CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: INIT_RESID(IER)                                        C
!  Purpose: Initialize residuals                                       C
!                                                                      C
!                                                                      C
!  Author: M. Syamlal                                 Date: 8-JUL-96   C
!  Reviewer:                                          Date:            C
!                                                                      C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced:                                               C
!  Variables modified:                                                 C
!                                                                      C
!  Local variables:                                                    C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
   SUBROUTINE INIT_RESID()

      USE param
      USE param1
      USE physprop
      USE residual

      IMPLICIT NONE
!-----------------------------------------------
!
!                      residual number
      INTEGER          L
!
!                      Phase index
      INTEGER          M
!
!-----------------------------------------------
!
! JFD : Why not set the entire RESID ARRAY to ZERO?
!       The initialization below doesn't work anymore
!       for DEM (need to use DES_MMAX)

      RESID(:,:) = ZERO
      RETURN

      L = 1
      IF (NRESID > 0) THEN
         M = 0
         IF (MMAX + 1 > 0) THEN
            RESID(:NRESID,:MMAX) = ZERO
            M = MMAX + 1
         ENDIF
         L = NRESID + 1
      ENDIF
      RETURN
   END SUBROUTINE INIT_RESID

END MODULE INIT_RESID_MOD
