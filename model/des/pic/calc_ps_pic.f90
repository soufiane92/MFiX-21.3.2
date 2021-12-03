module calc_ps_pic_mod

  private
  public :: calc_ps_pic

contains

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Purpose: Evaluate the particle stress model of Snider.              !
!                                                                      !
!  REF: D.M. Snider, "Three-Dimensional Multiphase Particle-in-Cell    !
!     Model for Dense Particle Flows," Journal of Computational        !
!     Physics, Vol. 170, No. 2, pp. 523-549, 2001.                     !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
SUBROUTINE CALC_PS_PIC

! Global Variables:
!---------------------------------------------------------------------//
  ! Model parameters for Snider particle stress model
  use mfix_pic, only: FRIC_EXP_PIC
  use mfix_pic, only: PSFAC_FRIC_PIC
  use mfix_pic, only: FRIC_NON_SING_FAC
  ! Fluid phase volume fraction
  use fldvar, only: EP_G
  ! Fluid volume fraction at close-pack
  use constant, only: EP_STAR
  ! Domain bounds
  use compar, only: IJKSTART3, IJKEND3
  ! Double precision parameters
  use param1, only: ONE, ZERO

  ! Gradient of solids pressure at particle position.
  use mfix_pic, only: PS_GRAD
  ! Max number of parcels on process
  use discretelement, only: MAX_PIP

  ! Function to determine parcels tate
  use functions, only: IS_NORMAL
  ! Function to determine if I/J/K are on this process
  use functions, only: IS_ON_MYPE_PLUS2LAYERS
  ! Function to convert I/J/K to IJK with cyclic wrap
  use compar, only: FUNIJK_MAP_C

  use functions, only: IM_OF, IP_OF
  use functions, only: JM_OF, JP_OF
  use functions, only: KM_OF, KP_OF

  use discretelement, only: iglobal_id

  USE indices, only: j_of

  use geometry, only: do_k

  use bilinear

! Module procedures:
!---------------------------------------------------------------------//
  use functions, only: FLUID_AT

  IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//
  ! Loop counter
  INTEGER :: IJK, NP

  INTEGER :: I, J, K, lI, lJ, lK, IC, JC, KC, Kub

  DOUBLE PRECISION :: Sx(0:1),  Sy(0:1),  Sz(0:1)
  DOUBLE PRECISION :: SxU(0:1), SyV(0:1), SzW(0:1)
  DOUBLE PRECISION :: lOoDX,    lOoDY,    lOoDZ

  DOUBLE PRECISION, PARAMETER :: SGN(0:1) = (/-1.0d0, 1.0d0/)

  DOUBLE PRECISION :: Ps(IJKSTART3:IJKEND3)
!......................................................................!


  ! Particle stress :: Snider (Eq 33)
  ! tau = (PsubS*ThetasubP^Beta)/max(ThetasubCP-ThetasubP,small#*Theatsubf)
  DO IJK = IJKSTART3, IJKEND3
     IF(VALID_PS_AT(IJK)) THEN
        Ps(IJK) = PSFAC_FRIC_PIC*((ONE - EP_G(IJK))**FRIC_EXP_PIC)/ &
             MAX((ONE-EP_STAR) - (ONE -EP_G(IJK)), FRIC_NON_SING_FAC*EP_G(IJK))
     ELSE
        Ps(IJK) = 0.0d0
     ENDIF
  ENDDO

  CALL EXTRAP_Ps(Ps)

  Kub = merge(1,0,DO_K)


  DO NP=1,MAX_PIP

     PS_GRAD(:,NP) = ZERO

     ! Skip parcels that don't exist.
     IF(IS_NORMAL(NP)) then

        call bilinear_x(NP, lI, Sx, SxU, lOoDX)
        call bilinear_y(NP, lJ, Sy, SyV, lOoDY)
        call bilinear_z(NP, lK, Sz, SzW, lOoDZ)

        DO KC=0,Kub
           DO JC=0,1
              DO IC=0,1
                 I = lI+IC
                 J = lJ+JC
                 K = lK+KC
                 IF(IS_ON_MYPE_PLUS2LAYERS(I,J,K)) THEN
                    IJK = FUNIJK_MAP_C(I,J,K)

                    PS_GRAD(1,NP) = PS_GRAD(1,NP)+Ps(IJK)*&
                         SGN(IC)*Sy(JC)*Sz(KC)*lOoDX

                    PS_GRAD(2,NP) = PS_GRAD(2,NP)+Ps(IJK)*&
                         SGN(JC)*Sx(IC)*Sz(KC)*lOoDY

                    PS_GRAD(3,NP) = PS_GRAD(3,NP)+Ps(IJK)*&
                         SGN(KC)*Sx(IC)*Sy(JC)*lOoDZ
                 ENDIF
              ENDDO
           ENDDO
        ENDDO

     ENDIF
  ENDDO

  RETURN

END SUBROUTINE CALC_PS_PIC

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Purpose: Evaluate the particle stress model of Snider.              !
!                                                                      !
!  REF: D.M. Snider, "Three-Dimensional Multiphase Particle-in-Cell    !
!     Model for Dense Particle Flows," Journal of Computational        !
!     Physics, Vol. 170, No. 2, pp. 523-549, 2001.                     !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
SUBROUTINE EXTRAP_Ps(Ps)

! Global Variables:
!---------------------------------------------------------------------//
  ! Domain bounds
  use compar, only: IJKSTART3, IJKEND3

  ! Flag for 3D (vs 2D) geometry
  USE geometry, only: DO_K

  use functions, only: IM_OF, IP_OF
  use functions, only: JM_OF, JP_OF
  use functions, only: KM_OF, KP_OF

! Module procedures:
!---------------------------------------------------------------------//
  use functions, only: FLUID_AT

  IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//
  ! Loop counter
  INTEGER :: IJK

  DOUBLE PRECISION, INTENT(INOUT) :: Ps(IJKSTART3:IJKEND3)
!......................................................................!

  ! Faces
  DO IJK = IJKSTART3, IJKEND3
     IF(.NOT.VALID_PS_AT(IJK) .and. Ps(IJK) == 0.0d0) THEN
        IF(VALID_PS_AT(IM_OF(IJK))) THEN
           Ps(IJK) = 2.0d0*Ps(IM_OF(IJK)) - Ps(IM_OF(IM_OF(IJK)))
        ELSEIF(VALID_PS_AT(IP_OF(IJK))) THEN
           Ps(IJK) = 2.0d0*Ps(IP_OF(IJK)) - Ps(IP_OF(IP_OF(IJK)))
        ELSEIF(VALID_PS_AT(JM_OF(IJK))) THEN
           Ps(IJK) = 2.0d0*Ps(JM_OF(IJK)) - Ps(JM_OF(JM_OF(IJK)))
        ELSEIF(VALID_PS_AT(JP_OF(IJK))) THEN
           Ps(IJK) = 2.0d0*Ps(JP_OF(IJK)) - Ps(JP_OF(JP_OF(IJK)))
        ELSEIF(DO_K .and. VALID_PS_AT(KM_OF(IJK))) THEN
           Ps(IJK) = 2.0d0*Ps(KM_OF(IJK)) - Ps(KM_OF(KM_OF(IJK)))
        ELSEIF(DO_K .and. VALID_PS_AT(KP_OF(IJK))) THEN
           Ps(IJK) = 2.0d0*Ps(KP_OF(IJK)) - Ps(KP_OF(KP_OF(IJK)))
        ENDIF
     ENDIF
  ENDDO

  ! Edges
  DO IJK = IJKSTART3, IJKEND3
     IF(.NOT.VALID_PS_AT(IJK).AND.PS(IJK)==0.0d0) THEN
        IF(VALID_PS_AT(IM_OF(JM_OF(IJK)))) THEN
           Ps(IJK) = (Ps(IM_OF(IJK)) + Ps(JM_OF(IJK)))/2.0d0
        ELSEIF(VALID_PS_AT(IM_OF(JP_OF(IJK)))) THEN
           Ps(IJK) = (Ps(IM_OF(IJK)) + Ps(JP_OF(IJK)))/2.0d0
        ELSEIF(VALID_PS_AT(IP_OF(JM_OF(IJK)))) THEN
           Ps(IJK) = (Ps(IP_OF(IJK)) + Ps(JM_OF(IJK)))/2.0d0
        ELSEIF(VALID_PS_AT(IP_OF(JP_OF(IJK)))) THEN
           Ps(IJK) = (Ps(IP_OF(IJK)) + Ps(JP_OF(IJK)))/2.0d0
        ELSEIF(DO_K .and. VALID_PS_AT(KM_OF(IM_OF(IJK)))) THEN
           Ps(IJK) = (Ps(KM_OF(IJK)) + Ps(IM_OF(IJK)))/2.0d0
        ELSEIF(DO_K .and. VALID_PS_AT(KM_OF(IP_OF(IJK)))) THEN
           Ps(IJK) = (Ps(KM_OF(IJK)) + Ps(IP_OF(IJK)))/2.0d0
        ELSEIF(DO_K .and. VALID_PS_AT(KM_OF(JM_OF(IJK)))) THEN
           Ps(IJK) = (Ps(KM_OF(IJK)) + Ps(JM_OF(IJK)))/2.0d0
        ELSEIF(DO_K .and. VALID_PS_AT(KM_OF(JP_OF(IJK)))) THEN
           Ps(IJK) = (Ps(KM_OF(IJK)) + Ps(JP_OF(IJK)))/2.0d0
        ELSEIF(DO_K .and. VALID_PS_AT(KP_OF(IM_OF(IJK)))) THEN
           Ps(IJK) = (Ps(KP_OF(IJK)) + Ps(IM_OF(IJK)))/2.0d0
        ELSEIF(DO_K .and. VALID_PS_AT(KP_OF(IP_OF(IJK)))) THEN
           Ps(IJK) = (Ps(KP_OF(IJK)) + Ps(IP_OF(IJK)))/2.0d0
        ELSEIF(DO_K .and. VALID_PS_AT(KP_OF(JM_OF(IJK)))) THEN
           Ps(IJK) = (Ps(KP_OF(IJK)) + Ps(JM_OF(IJK)))/2.0d0
        ELSEIF(DO_K .and. VALID_PS_AT(KP_OF(JP_OF(IJK)))) THEN
           Ps(IJK) = (Ps(KP_OF(IJK)) + Ps(JP_OF(IJK)))/2.0d0
        ENDIF
     ENDIF
  ENDDO


  ! Corners and other
  IF(DO_K) THEN
     DO IJK = IJKSTART3, IJKEND3
        IF(.NOT.VALID_PS_AT(IJK) .and. Ps(IJK) == 0.0d0) THEN
           IF(VALID_PS_AT(KM_OF(IM_OF(JM_OF(IJK))))) THEN
              Ps(IJK) = (Ps(KM_OF(IJK)) + Ps(IM_OF(IJK)) + Ps(JM_OF(IJK)))/3.0d0
           ELSEIF(VALID_PS_AT(KM_OF(IM_OF(JP_OF(IJK))))) THEN
              Ps(IJK) = (Ps(KM_OF(IJK)) + Ps(IM_OF(IJK)) + Ps(JP_OF(IJK)))/3.0d0
           ELSEIF(VALID_PS_AT(KM_OF(IP_OF(JM_OF(IJK))))) THEN
              Ps(IJK) = (Ps(KM_OF(IJK)) + Ps(IP_OF(IJK)) + Ps(JM_OF(IJK)))/3.0d0
           ELSEIF(VALID_PS_AT(KM_OF(IP_OF(JP_OF(IJK))))) THEN
              Ps(IJK) = (Ps(KM_OF(IJK)) + Ps(IP_OF(IJK)) + Ps(JP_OF(IJK)))/3.0d0
           ELSEIF(VALID_PS_AT(KP_OF(IM_OF(JM_OF(IJK))))) THEN
              Ps(IJK) = (Ps(KP_OF(IJK)) + Ps(IM_OF(IJK)) + Ps(JM_OF(IJK)))/3.0d0
           ELSEIF(VALID_PS_AT(KP_OF(IM_OF(JP_OF(IJK))))) THEN
              Ps(IJK) = (Ps(KP_OF(IJK)) + Ps(IM_OF(IJK)) + Ps(JP_OF(IJK)))/3.0d0
           ELSEIF(VALID_PS_AT(KP_OF(IP_OF(JM_OF(IJK))))) THEN
              Ps(IJK) = (Ps(KP_OF(IJK)) + Ps(IP_OF(IJK)) + Ps(JM_OF(IJK)))/3.0d0
           ELSEIF(VALID_PS_AT(KP_OF(IP_OF(JP_OF(IJK))))) THEN
              Ps(IJK) = (Ps(KP_OF(IJK)) + Ps(IP_OF(IJK)) + Ps(JP_OF(IJK)))/3.0d0
           ENDIF
        ENDIF
     ENDDO
  ENDIF


END SUBROUTINE EXTRAP_PS


logical function valid_ps_AT(ijk)

  use geometry, only: flag

  implicit none

  integer, intent(in) :: ijk

  integer :: lflag
  lflag = flag(ijk)

  valid_ps_at = (lflag == 1)                     .or. & ! fluid_at
       &        (lflag >= 10 .and. lflag <= 31)  .or. & ! flow_at
       &        (lflag == 106 .or. lflag == 107)        ! cyclic_at

end function valid_ps_at


end module calc_ps_pic_mod
