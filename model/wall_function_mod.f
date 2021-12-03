MODULE WALL_FUNCTION_MOD

   USE fldvar, only: k_turb_g, ro_g
   USE param1, only: zero, one
   USE physprop, only: mu_g
   USE turb, only: turb_c_mu, turb_kappa
   USE visc_g, only: mu_gt

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: Wall_Function                                           C
!  Purpose: Calculate Slip velocity using wall functions               C
!                                                                      C
!  Author: S. Benyahia                                Date: MAY-13-04  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE Wall_Function(IJK1, IJK2, ODX_WF, W_F_Slip)

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! IJK indices for wall cell and fluid cell
      INTEGER :: IJK1, IJK2
! ODX_WF: 1/dx, and W_F_Slip: value of turb. shear stress at walls
      DOUBLE PRECISION ODX_WF, W_F_Slip

! Local variables
!---------------------------------------------------------------------//

!---------------------------------------------------------------------//

      IF(DABS(ODX_WF)>1.0D-5.AND.MU_gT(IJK2)>ZERO.AND.MU_g(IJK2)>ZERO) THEN
! Avoid division by near-zero. This can occur when del_h is undefined
! for some bad cut-cells. Will probably need user-defined tolerance.
! JFD 1/15/2019: Avoid cells where MU_gt or Mu_g are zero too.

         W_F_Slip = (ONE - ONE/ODX_WF * RO_g(IJK2)*turb_C_mu**0.25* &
            SQRT(K_Turb_G(IJK2))/MU_gT(IJK2) * turb_Kappa/&
            LOG(9.81D+0/(ODX_WF*2.D+0)*RO_g(IJK2)*turb_C_mu**0.25*&
            SQRT(K_Turb_G(IJK2))/MU_g(IJK2)))
      ELSE
! Should it be set to another value in this case?
         W_F_Slip = ONE
      ENDIF


      RETURN

   END SUBROUTINE Wall_Function

END MODULE WALL_FUNCTION_MOD
