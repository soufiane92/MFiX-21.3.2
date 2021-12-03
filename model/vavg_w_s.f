MODULE VAVG_W_S_MOD

   USE compar, only: ijkstart3, ijkend3
   USE fldvar, only: ep_s, w_s
   USE functions, only: is_on_mype_wobnd, fluid_at
   USE geometry, only: vol_w
   USE indices, only: i_of, j_of, k_of
   USE mpi_utility, only: global_all_sum
   USE param1, only: zero

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: VAVG_W_s(M)                                            C
!  Purpose: Calculate volume averaged W_s                              C
!                                                                      C
!  Author: M. Syamlal                                 Date: 28-APR-94  C
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

   DOUBLE PRECISION FUNCTION VAVG_W_S (M)

      IMPLICIT NONE

!                      Indices
      INTEGER          IJK , M
!
!                      Integral of W_s*EP_s for entire volume
      DOUBLE PRECISION SUM_W_s
!
!                      Total volume of computational cells
      DOUBLE PRECISION SUM_VOL

!  Integrate the velocity values for the whole domain,
!
      SUM_W_S = ZERO
      SUM_VOL = ZERO

!!!!$omp   parallel do private(IJK) reduction(+:SUM_VOL,SUM_W_S)
      DO IJK = IJKSTART3, IJKEND3
      IF(.NOT.IS_ON_myPE_wobnd(I_OF(IJK), J_OF(IJK), K_OF(IJK))) CYCLE
         IF (FLUID_AT(IJK)) THEN
            SUM_VOL = SUM_VOL + VOL_W(IJK)
            SUM_W_S = SUM_W_S + W_S(IJK,M)*EP_S(IJK,M)*VOL_W(IJK)
         ENDIF
      END DO

      CALL GLOBAL_ALL_SUM(SUM_VOL)
      CALL GLOBAL_ALL_SUM(SUM_W_S)
      VAVG_W_S = SUM_W_S/SUM_VOL
!
      RETURN
   END FUNCTION VAVG_W_S

END MODULE VAVG_W_S_MOD
