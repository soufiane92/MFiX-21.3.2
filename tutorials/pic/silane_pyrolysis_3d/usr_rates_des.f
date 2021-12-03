!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: USR_RATES                                              !
!                                                                      !
!  Purpose: Hook for user defined reaction rates.                      !
!                                                                      !
!  Author: J.Musser                                   Date: 10-Oct-12  !
!                                                                      !
!  Comments: Write reaction rates in amount of chemical substance      !
!  per time. For CGS this has units mol/s. For _MFIX_ SI this has      !
!  units kmol/s.                                                       !
!                                                                      !
!  WARNING: Only discrete phase reactions should be specified here.     !
!  Homogeneous gas phase reactions in DEM simulations must be given    !
!  in the continuum reaction hook (usr_rates.f).                       !
!                                                                      !
!  The call to usr_rates_des is made from inside a particle loop which !
!  is nested inside an IJK loop. Fluid grid calculations independent   !
!  of particle properties can be carried out in des/usr4_des.f to      !
!  reduce redundant calculations.                                      !
!                                                                      !
!  Example: Evaporation                                                !
!                                                                      !
!  mfix.dat input:                                                     !
!``````````````````````````````````````````````````````````````````````!
!    @(DES_RXNS)                                                       !
!      Evap { chem_eq = "Liquid --> Vapor" }                           !
!    @(DES_END)                                                        !
!``````````````````````````````````````````````````````````````````````!
!                                                                      !
!  des/usr_rates_des.f input:                                          !
!``````````````````````````````````````````````````````````````````````!
!    Sa = Pi*(2.0d0*DES_RADIUS(NP))**2    ! Particle surface area      !
!    H2O_xfr = ...  ! An expression for mass transfer coefficient      !
!    Cmg_H2O = ...  ! Molar concentration grad of water vapor          !
!    DES_RATES(EVAP) = Sa * H2O_xfr * Cmg_H2O                          !
!``````````````````````````````````````````````````````````````````````!
!  * Species alias and reaction names given in the data file can be    !
!    used in reference to the reaction index in DES_RATES and a        !
!    species index in gas/solids phase variables.                      !
!                                                                      !
!  * Additional information is provided in section 4.11 of the code    !
!    Readme.                                                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_RATES_DES(NP, pM, IJK, DES_RATES)

      USE compar
      USE constant
      USE des_thermo
      USE des_rxns
      USE discretelement
      USE energy
      USE fldvar
      USE funits
      USE geometry
      USE indices
      USE param
      USE param1
      USE physprop
      USE rxns
      USE run
      USE usr
      USE fun_avg
      USE functions

      USE parallel
      USE sendrecv
      USE toleranc



      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NP  ! Global index of particle
      INTEGER, INTENT(IN) :: pM  ! Solid phase index of particle NP
      INTEGER, INTENT(IN) :: IJK ! Fluid cell index containing NP

! Calculated reaction rates. (reacted moles per sec)
      DOUBLE PRECISION, INTENT(OUT) :: DES_RATES(NO_OF_DES_RXNS)


! Reaction specific variables:
!`````````````````````````````````````````````````````````````````````//
! Solids Diameter (m)
      DOUBLE PRECISION :: Dp0
! Bounded phase temperatures (K)
      DOUBLE PRECISION xTg   ! gas phase
      DOUBLE PRECISION xTs   ! solids phase
! Gas phase pressure in KPa
      DOUBLE PRECISION Pg_KPa
! Gas phase concentrations (mol/m^3)
      DOUBLE PRECISION c_SiH4, c_SiH2, c_Si2H6, c_H2
! Partial pressures (KPa)
      DOUBLE PRECISION p_SiH4, p_SiH2, p_H2
! Total solids surface area per volume (1/m)
      DOUBLE PRECISION Sa
! Diffusion coefficient for SiH2 (m^2/sec)
      DOUBLE PRECISION DIFF_SiH2
! SiH2 gas constant (KPa.m^3)/(kg-SiH2.K)
      DOUBLE PRECISION R_SiH2
! Calculate film diffusion resistance. (kg-SiH2/(KPa.sec.m^2)
      DOUBLE PRECISION K_f
! Apparent first order heterogeneous reation rate constant  (m/sec)
      DOUBLE PRECISION k_so
! Constant of hydroggen inhibition of heterogeneous reaction (1/KPa)
      DOUBLE PRECISION K_H
! Constant of silane inhibition of heterogeneous reaction (1/KPa)
      DOUBLE PRECISION Ks
! Temporary values for reversible reactions
      DOUBLE PRECISION FWD, RVS, NET


! Prevent reactions when mass fraction falls below c_Limiter.
      DOUBLE PRECISION, parameter :: c_Limiter = 1.0d-7

!     Uncomment the following line when customizing this usr_rates_des.f to include generated file species.inc:
!
     INCLUDE 'species.inc'


! Reaction rates:
!`````````````````````````````````````````````````````````````````````//
! Include reaction rates here. Reaction rates should be stored in the
! variable DES_RATES. The reaction name given in the data file can be
! used to store the rate in the appropriate array location. Additional
! input format parameters are given in Section 4.11 of the code Readme.

! Calculate particle diameter.
      Dp0 = 2.0d0 * DES_RADIUS(NP)

! Particle surface area (m^2)
      Sa = Pi* Dp0 * Dp0   ! (m^2)

! Calculate the bounded temperatures.
      xTg = max(min(T_g(IJK),   TMAX), 300.0d0)
      xTs = max(min(DES_T_s(NP), TMAX), 300.0d0)

! Convert gas pressure to KPa
      Pg_KPa = 1.0d-3 * P_g(IJK)  !  KPa

! Initialize partial pressures and concentrations.
      c_SiH4  = ZERO;   p_SiH4 = ZERO;
      c_SiH2  = ZERO;   p_SiH2 = ZERO;
      c_H2    = ZERO;   p_H2   = ZERO;
      c_Si2H6 = ZERO;

! Calculate the partial pressure and molar concentration of SiH4
      IF(X_g(IJK,SiH4) > c_Limiter) then
         p_SiH4 = Pg_KPa * X_g(IJK,SiH4) * (MW_MIX_g(IJK) / MW_g(SiH4))
         c_SiH4 = RO_g(IJK) * X_g(IJK,SiH4) / Mw_g(SiH4)
      ENDIF

! Calculate the partial pressure and molar concentration of SiH2
      IF(X_g(IJK,SiH2) > c_Limiter) then
         p_SiH2 = Pg_KPa * X_g(IJK,SiH2) * (MW_MIX_g(IJK) / MW_g(SiH2))
         c_SiH2 = RO_g(IJK)*X_g(IJK,SiH2)/Mw_g(SiH2)
      ENDIF

! Calculate the molar concentration of Si2H6
      IF(X_g(IJK,Si2H6) > c_Limiter) then
         c_Si2H6 = RO_g(IJK)*X_g(IJK,Si2H6)/Mw_g(Si2H6)
      ENDIF

! Calculate the partial pressure and molar concentration of H2
      IF(X_g(IJK,H2) > c_Limiter) then
         p_H2 = Pg_KPa * X_g(IJK,H2) * (MW_MIX_g(IJK) / MW_g(H2))
         c_H2 = RO_g(IJK) * X_g(IJK,H2) / Mw_g(H2)
      ENDIF

!**********************************************************************!
!                        Heterogeneous Reactions                       !
!**********************************************************************!
      IF(X_g(IJK,SiH4) > c_Limiter) THEN


! SiH4 --> Si + 2H2  (kg-mole/m^3.s)
!---------------------------------------------------------------------//
! Ref. Furusawa, Kojima, and Hiroha (1988)
! Apparent first order heterogeneous reation rate constant  (m/sec)
         k_so = 2.15d8 * exp(-2.3016d4/xTg)
! Constant of hydroggen inhibition of heterogeneous reaction (1/KPa)
         K_H  = 3.4d-2
! Constant of silane inhibition of heterogeneous reaction (1/KPa)
         KS  = 7.6d-3*exp(3.96d3/xTg)
! Reaction rate:
         DES_RATES(RX3)  = (Sa*k_so*c_SiH4) / (ONE + K_H*p_H2 + KS*p_SiH4)

      ELSE

         DES_RATES(RX3)  = ZERO

      ENDIF

! SiH2 --> Si + H2  (kg-mole/m^3.s)
!---------------------------------------------------------------------//
! REF: Caussat, Hemati, and Couderc (1995)
! The reference states that 'all authors' consider their heterogeneous
! decomposition rate as infinite. This reaction rate is based on film
! layer resistance.

      IF(X_g(IJK,SiH2) > c_Limiter) THEN
! Diffusion coefficient.
! This is based on Chapman-Enskog theory for diffusion in gases at low
! density. Approximately self-diffusivity of N2.
         DIFF_SiH2 = 3.45d-9*(xTg**1.5)  ! (m^2/sec)
! SiH2 Gas constant. (Pa.m^3)/(kmol.K) == (KPa.cm^3)/(mol.K)
         R_SiH2 = (8.3147295/MW_g(SiH2)) ! (KPa.m^3)/(kg-SiH2.K)
! Calculate film diffusion resistance. (kg-SiH2/(KPa.sec.m^2)
         K_f = DIFF_SiH2 * N_sh(IJK,1) / (Dp0 * R_SiH2 * xTg)
! Calculate the reaction rate.
         DES_RATES(RX4)  =  Sa * K_f * (p_SiH2 / MW_g(SiH2))

      ELSE

         DES_RATES(RX4)  = ZERO

      ENDIF



      RETURN

      END SUBROUTINE USR_RATES_DES
