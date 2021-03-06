MODULE GET_HLOSS_MOD

   use bc
   use fldvar, only: t_g, t_s
   use get_philoss_mod, only: get_philoss
   use physprop, only: k_g, k_s, mmax

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: GET_HLOSS(HLOSS)                                       C
!  Purpose: Determine the total heat loss from the reactor             C
!                                                                      C
!  Author: M. Syamlal                                 Date: 31-MAR-95  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced: IMAX2, JMAX2, KMAX2, MMAX, ROP_s, DX, DY, DZ, C
!                        X, IMIN1, JMIN1. KMIN1                        C
!  Variables modified: I, J, K, M, IJK                                 C
!                                                                      C
!  Local variables:  None                                              C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
   SUBROUTINE GET_HLOSS(HLOSS)

      IMPLICIT NONE

!-----------------------------------------------
!
!                      Total heat loss from the reactor
      DOUBLE PRECISION HLOSS, HLOSSm
!
!                      Indices
      INTEGER          M
!
!-----------------------------------------------
!
      CALL GET_PHILOSS (T_G, K_G, BC_TW_G, BC_HW_T_G, BC_C_T_G, HLOSSM)
      HLOSS = HLOSSM
      DO M = 1, MMAX
         CALL GET_PHILOSS (T_S(1,M), K_S(1,M), BC_TW_S(1,M), BC_HW_T_S(1,M), &
            BC_C_T_S(1,M), HLOSSM)
         HLOSS = HLOSS + HLOSSM
      END DO
      RETURN

   END SUBROUTINE GET_HLOSS

END MODULE GET_HLOSS_MOD
