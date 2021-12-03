#include "error.inc"
#include "version.inc"

MODULE CALC_GAMMA_DES_MOD

   USE error_manager

CONTAINS
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Subroutine: DES_CALC_GAMMA                                          !
!                                                                      !
!  Purpose: Calculate the heat transfer coefficient (GAMMAxSA) for     !
!  particle-fluid heat transfer.                                       !
!                                                                      !
!  Author: J.Musser                                   Date: 16-Jun-10  !
!                                                                      !
!  Comments:                                                           !
!                                                                      !
!  REF: Ranz, W.E. and Marshall, W.R., "Friction and transfer          !
!       coefficients for single particles and packed beds," Chemical   !
!       Engineering Science, Vol. 48, No. 5, pp 247-253, 1925.         !
!                                                                      !
!                                                                      !
!  Author: X.Gao,L.Lu                               Date: 25-Feb-2021  !
!                                                                      !
!  Comments: Add three models for dense particulate flows              !                                                          !
!                                                                      !
!  REF: Gunn D. Transfer of heat or mass to particles in fixed and     !
!      fluidised beds. International Journal of Heat and Mass Transfer.!
!      1978;21(4):467-476.                                             !
!                                                                      !
!      Wakao N, Kaguei S, Funazkri T. Effect of fluid dispersion       !
!      coefficients on particle-to-fluid heat transfer coefficients    !
!      in packed beds: correlation of Nusselt numbers.                 !
!      Chemical engineering science. 1979;34(3):325-336.               !
!                                                                      !
!     Tavassoli H, Peters E, Kuipers J. Direct numerical simulation    !
!     of fluid-particle heat transferin fixed random arrays of         !
!     non-spherical particles. Chemical Engineering Science.           !
!     2015;129:42-48.                                                  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CALC_GAMMA_DES(NP, pGAMMA)

      USE compar
      USE constant
      USE des_thermo
      USE discretelement
      USE fldvar
      USE geometry
      USE indices
      USE param1
      USE physprop
      USE fun_avg
      USE functions

      IMPLICIT NONE

! Passed variables
!---------------------------------------------------------------------//
! Index value of particle
      INTEGER, INTENT(IN) :: NP
! Convective heat transfer coefficient
      DOUBLE PRECISION, INTENT(OUT) :: pGAMMA
! Local variables
!---------------------------------------------------------------------//
! Fluid cell indices
      INTEGER IMJK, IJMK, IJKM
! Double precision value for 1/3
      DOUBLE PRECISION, PARAMETER  :: THIRD = (1.0d0/3.0d0)
      DOUBLE PRECISION N_Pr  ! Prandtl Number
      DOUBLE PRECISION N_Re  ! Reynolds Number
      DOUBLE PRECISION N_Nu  ! Nusselt Number
! Magnitude of slip velocity
      DOUBLE PRECISION SLIP
! Particle Diameter
      DOUBLE PRECISION DP
! Fluid velocity
      DOUBLE PRECISION cUg, cVg, cWg
      DOUBLE PRECISION Us, Vs, Ws,epg
! Index value of fluid cell
      INTEGER :: IJK
!---------------------------------------------------------------------//
      IJK = PIJK(NP,4)
! Initialization
      IMJK = IM_OF(IJK)
      IJMK = JM_OF(IJK)
      IJKM = KM_OF(IJK)
! Initialize variables
      SLIP = ZERO
      N_Re = ZERO
      N_Nu = ZERO
! Gas velocity in fluid cell IJK
      cUg = AVG_X_E(U_g(IMJK), U_g(IJK), 1)
      cVg = AVG_Y_N(V_g(IJMK), V_g(IJK))
! Particle Velocity
      Us = DES_VEL_NEW(NP,1)
      Vs = DES_VEL_NEW(NP,2)
! Calculate the magnitude of the slip velocity
      IF(NO_K) THEN
          SLIP = SQRT((cUg-Us)**2 + (cVg-Vs)**2)
      ELSE
          cWg = AVG_Z_T(W_g(IJKM), W_g(IJK))
          Ws = DES_VEL_NEW(NP,3)
          SLIP = SQRT((cUg-Us)**2 + (cVg-Vs)**2 + (cWg-Ws)**2)
      ENDIF
! Calculate the Prandtl Number
      IF(K_G(IJK) > ZERO) THEN
           N_Pr = (C_PG(IJK)*MU_G(IJK))/K_G(IJK)
      ELSE
           N_Pr = LARGE_NUMBER
      ENDIF
      DP = 2.0d0*DES_RADIUS(NP)
      if(CGDEM) DP = 2.0d0*DES_CGP_RPR(NP)
! Calculate the particle Reynolds Number
      IF(MU_G(IJK) > ZERO) THEN
           N_Re = (DP*SLIP*RO_g(IJK)) / MU_g(IJK)
      ELSE
           N_Re = LARGE_NUMBER
      ENDIF
! Calculate the Nusselt Number
      SELECT CASE(DES_CONV_CORR_ENUM)
! Calculate the Nusselt Number, ranzi mashall
         CASE (RANZ_1952) ! (Ranz and Mrshall, 1952)
            N_Nu = 2.0d0 + 0.6d0 *((N_Re)**HALF * (N_Pr)**THIRD)
! wakao 1979
         CASE (WAKAO)
            N_Nu = 2.0d0 + 1.1d0 *((N_Re)**0.6 * (N_Pr)**THIRD)
! GUNN model
         CASE (GUNN)
            Epg=Ep_g(IJK)
            N_Nu = (7.0d0-10.0*epg+5.0*epg*epg)*(1.0d0+0.7d0 *((N_Re)**0.2*(N_Pr)**THIRD)) &
                   + (1.33d0-2.40d0*epg+1.20d0*epg*epg)*(N_Re)**0.7*(N_Pr)**THIRD
! Tavassoli modeL, a model modified based on GUNN
         CASE (TAVASSOLI)
            Epg=Ep_g(IJK)
            N_Nu = (7.0d0-10.0*epg+5.0*epg*epg)*(1.0d0+0.1d0 *((N_Re)**0.2*(N_Pr)**THIRD)) &
                   + (1.33d0-2.19d0*epg+1.15d0*epg*epg)*(N_Re)**0.7*(N_Pr)**THIRD
         CASE DEFAULT
            WRITE(ERR_MSG,*)'INVALID DES CONVECTION MODEL'
            call log_error()
            ERROR_STOP 'INVALID DES CONVECTION MODEL'
      END SELECT
! Calculate the convective heat transfer coefficient
         pGAMMA = (N_Nu * K_G(IJK))/DP

      RETURN

     END SUBROUTINE CALC_GAMMA_DES

END MODULE CALC_GAMMA_DES_MOD
