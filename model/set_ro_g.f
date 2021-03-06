MODULE SET_RO_g_MOD
   CONTAINS
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: SET_RO_g                                                C
!  Purpose: Initialize the gas densities                               C
!                                                                      C
!  Author: M. Syamlal                                 Date: 21-JAN-92  C
!  Reviewer:M. Syamlal, S. Venkatesan, P. Nicoletti,  Date: 29-JAN-92  C
!           W. Rogers                                                  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced: MW_MIX_g, P_g, T_g, EP_g, RO_g0               C
!  Variables modified: RO_g, ROP_g                                     C
!  Local variables: IJK                                                C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE SET_RO_G

!-----------------------------------------------
! Modules
!-----------------------------------------------
      USE compar
      USE constant
      USE eos, only: EOSG
      USE fldvar
      USE functions
      USE geometry
      USE indices
      USE parallel
      USE param
      USE param1
      USE physprop
      use usr_prop, only: usr_rog, calc_usr_prop
      IMPLICIT NONE
!-----------------------------------------------
! Local variables
!-----------------------------------------------
      INTEGER :: IJK
!-----------------------------------------------
      IF (USR_ROG) THEN   ! user defined ro_g
         DO IJK=IJKSTART3,IJKEND3
            IF (WALL_AT(IJK)) CYCLE
            CALL USR_PROP_ROG(IJK)
            ROP_G(IJK) = EP_G(IJK)*RO_G(IJK)
         ENDDO

      ELSEIF (RO_G0 == UNDEFINED) THEN   ! compressible case

!!$omp parallel do private(IJK)
!         print*,'set_ro_g, ijkstart3, ijkend3=',ijkstart3, ijkend3,flag(86320)
         DO IJK = ijkstart3, ijkend3
! calculate ro_g and rop_g in all fluid and flow boundary cells
            IF (.NOT.WALL_AT(IJK)) THEN
!               if(ijk==86320)print*,'set_ro_g',IJK,I_OF(IJK),J_OF(IJK),K_OF(IJK),T_G(IJK),FLAG(IJK)
               RO_G(IJK) = EOSG(MW_MIX_G(IJK),P_G(IJK),T_G(IJK))
               ROP_G(IJK) = EP_G(IJK)*RO_G(IJK)
            ENDIF
         ENDDO

      ELSE   ! incompressible case

!!$omp   parallel do private(ijk)
         DO IJK = ijkstart3, ijkend3
            IF (.NOT.WALL_AT(IJK)) THEN
! assign ro_g and calculate rop_g in all fluid and flow boundary cells
! set_constprop will have already defined ro_g in fluid and flow
! boundary cells (redundant- remove here?)
               RO_G(IJK) = RO_G0
               ROP_G(IJK) = EP_G(IJK)*RO_G(IJK)
            ENDIF
         ENDDO
      ENDIF

      RETURN
      END SUBROUTINE SET_RO_G

END MODULE SET_RO_g_MOD
