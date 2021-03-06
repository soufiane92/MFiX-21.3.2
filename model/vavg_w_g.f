MODULE VAVG_W_G_MOD

   USE fldvar, only: ep_g, w_g
   USE functions, only: is_on_mype_wobnd, fluid_at
   USE mflux, only: flux_gt, flux_gst
   USE mpi_utility
   USE param1, only: zero
   USE run, only: added_mass
   USE utilities, ONLY: mfix_isnan

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: VAVG_W_g                                               C
!  Purpose: Calculate volume averaged W_g                              C
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

   DOUBLE PRECISION FUNCTION VAVG_W_G ()

      IMPLICIT NONE

!                      Indices
      INTEGER          IJK
!
!                      Integral of W_g*EP_g for entire volume
      DOUBLE PRECISION SUM_W_g
!
!                      Total volume of computational cells
      DOUBLE PRECISION SUM_VOL

!  Integrate the velocity values for the whole domain,
!
      SUM_W_G = ZERO
      SUM_VOL = ZERO

!!!!$omp   parallel do private(IJK) reduction(+:SUM_VOL,SUM_W_G)
      DO IJK = IJKSTART3, IJKEND3
      IF(.NOT.IS_ON_myPE_wobnd(I_OF(IJK), J_OF(IJK), K_OF(IJK))) CYCLE
         IF (FLUID_AT(IJK)) THEN
            SUM_VOL = SUM_VOL + VOL_W(IJK)
            SUM_W_G = SUM_W_G + W_G(IJK)*EP_G(IJK)*VOL_W(IJK)
         ENDIF
      END DO

      CALL GLOBAL_ALL_SUM(SUM_VOL)
      CALL GLOBAL_ALL_SUM(SUM_W_G)
      VAVG_W_G = SUM_W_G/SUM_VOL
!
! uncomment the following lines to enable trapping NaN's.
!      IF( mfix_isnan(VAVG_W_G) ) THEN
!        write(*,*) VAVG_W_G,  ' NaN being caught in VAVG_W_G.f '
!        AUTOMATIC_RESTART = .TRUE.
!      ENDIF
!
      RETURN
   END FUNCTION VAVG_W_G

   DOUBLE PRECISION FUNCTION VAVG_Flux_W_G ()

      IMPLICIT NONE

!                      Indices
      INTEGER          IJK
!
!                      Integral of W_g*EP_g for entire volume
      DOUBLE PRECISION SUM_W_g
!
!                      Total volume of computational cells
      DOUBLE PRECISION SUM_AREA
!
!-----------------------------------------------
!
!  Integrate the velocity values for the whole domain,
!
      SUM_W_G = ZERO
      SUM_AREA = ZERO

!!!!$omp   parallel do private(IJK) reduction(+:SUM_AREA,SUM_W_G)
      DO IJK = IJKSTART3, IJKEND3
      IF(.NOT.IS_ON_myPE_wobnd(I_OF(IJK), J_OF(IJK), K_OF(IJK))) CYCLE
         IF (FLUID_AT(IJK)) THEN
           IF(.NOT.ADDED_MASS) THEN
              SUM_W_G = SUM_W_G + Flux_gT(IJK)
           ELSE
              SUM_W_G = SUM_W_G + Flux_gST(IJK)
           ENDIF
           SUM_AREA = SUM_AREA + AXY(IJK)
         ENDIF
      END DO

      CALL GLOBAL_ALL_SUM(SUM_AREA)
      CALL GLOBAL_ALL_SUM(SUM_W_G)
      VAVG_Flux_W_G = SUM_W_G/SUM_AREA

      RETURN

   END FUNCTION VAVG_Flux_W_G

END MODULE VAVG_W_G_MOD
