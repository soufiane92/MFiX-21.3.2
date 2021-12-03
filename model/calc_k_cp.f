MODULE CALC_K_cp_MOD
CONTAINS

! TO DO:
! 1. Kcp needs to be defined for each solids phase (?).
! 2. The part of Kcp from P_star should be based on the
!    sum of EP_s of close-packed solids.

! Comments:
! Why are the calculations done over all fluid and flow_at cells?
!  Consider calc_mu_s loops only over fluid...
! If the loop was only over fluid cells then a little rearranging
! would allow one to invoke the field variables rather than
! repeating entire sections of code.

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine:  CALC_K_cp                                              C
!  Purpose: Calculate and store dPodEp_s                               C
!                                                                      C
!  Notes: MCP must be defined to call this routine.                    C
!                                                                      C
!  Author: M. Syamlal                                 Date: 5-FEB-97   C
!  Reviewer:                                          Date:            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

   SUBROUTINE CALC_K_cp(Kcp)

! Modules
!---------------------------------------------------------------------
      use constant, only: switch
      USE constant, only: alpha
      USE constant, only: eta
      USE constant, only: sqrt_pi, pi
      USE constant, only: to_si
      USE constant, only: sin_phi
      USE constant, only: eps_f_min
      USE constant, only: fr, n_pc, d_pc, n_pf, delta

      USE fldvar, only: ep_g, ep_s
      USE fldvar, only: d_p, ro_s, ro_g
      USE fldvar, only: theta_m

      use param, only: dimension_3
      USE param1, only: zero, one, small_number, undefined_I

      USE physprop, only: smax, close_packed, blend_function

      USE rdf, only: g_0

      USE run, only: kt_type_enum, ghd_2007
      USE run, only: friction, savage, blending_stress, princeton, guo_boyce
      USE trace, only: trd_s2, trd_s_c
      USE visc_s, only: ep_star_array, ep_g_blend_end

      USE compar, only: ijkstart3, ijkend3
      USE functions, only: wall_at
      USE pscor, only: MCP
      USE solids_pressure, only: dPodEP_s

      USE sendrecv
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------
! dPodEP_s
      DOUBLE PRECISION :: Kcp(DIMENSION_3)
! Local variables
!---------------------------------------------------------------------
! indices
      INTEGER :: IJK, M, MM
! Other variables
      DOUBLE PRECISION :: dzdeps, zeta
      DOUBLE PRECISION :: Pc, dPcdeps, Pc_at, dPcdep_at
      DOUBLE PRECISION :: PfoPc, dPfoPcdeps, PfoPc_max, N_Pff
! Variables for savage=1
      DOUBLE PRECISION :: sum_epsg0, mu_sv, mu_bv, mu_dstar, mu_star, gama
! variable for ghd
      DOUBLE PRECISION :: dp_avg
! Blend Factor
      Double Precision :: blend

! Guo-Boyce model
      DOUBLE PRECISION :: n1_t
      DOUBLE PRECISION :: shearRate_gamma
      DOUBLE PRECISION, PARAMETER          :: a_ps = 1D25
      INTEGER, PARAMETER                   :: b_ps = 10

!---------------------------------------------------------------------


! initializing
      KCP(:) = ZERO

      IF (MCP == UNDEFINED_I) THEN
! this error should be caught earlier in the routines so that this
! branch should never be entered
         RETURN
      ELSE
! the lowest solids phase index of those solids phases that can close
! pack (i.e. close_packed=T) and the index of the solids phase that is
! used to form the solids correction equation.
         M = MCP
      ENDIF


! by definition M must be close_packed (this is a redundant check)
      IF(CLOSE_PACKED(M)) THEN

         DO IJK = ijkstart3, ijkend3

! most of this is taken directly from the appropriate subroutines in
! calc_mu_s
            IF(.NOT.WALL_AT(IJK))THEN

               IF (FRICTION) THEN
                  IF (EP_G(IJK).LT.(ONE-EPS_f_min)) THEN

                     IF (SAVAGE.EQ.1) THEN
! form of Savage (not to be used with GHD theory)
! Here S:S is determined based on simple shear granular flow (SSF)
! in which production due to shear work balances dissipation due to
! inelastic collisions. That equation holds for a granular flow where
! no interstitial fluid is involved. Otherwise some modifications
! would be needed to account for possible fluid effects..
!    dissipation = stress:grad(velocity)
!    gama = 2*mu*S:grad(vel)
!    gama/(2*mu)=S:grad(vel)

! Technically the original implementation is specific to the KT of
! Lun et al. (1984). So quantites are reproduced here based on this KT.
! A more generic implementation would simply assign mu_sv = mu_s_v(ijk)
! However, the dissipation equation may no longer be consistent with
! the selected KT and the evaluated derivative would be incorrect..
                        SUM_EpsG0 = ZERO
                        DO MM = 1, SMAX
                           SUM_EpsG0 =  SUM_EpsG0+EP_s(IJK,MM)*G_0(IJK,M,MM)
                        ENDDO

                        Mu_dstar = (5.d0*DSQRT(Pi*Theta_m(IJK,M))*D_p(IJK,M)*&
                           RO_S(IJK,M))/96.d0
                        Mu_bv = (256.d0*Mu_dstar*EP_s(IJK,M)*SUM_EpsG0)&
                           /(5.d0*Pi)
                        IF(SWITCH == ZERO .OR. RO_G(IJK) == ZERO) THEN
! this seems to be the proper extension if ro_g0=0 for M>1
                           Mu_star = EP_s(IJK,M)*G_0(IJK,M,M)*Mu_dstar/ &
                              SUM_EpsG0
                        ELSEIF(Theta_m(IJK,M) .LT. SMALL_NUMBER)THEN
                           Mu_star = ZERO
! there is no correction in the dilute limit
                        ELSE    ! ad-hoc mod for polydispersity w/o a fluid phase
                           Mu_star = EP_s(IJK,M)*G_0(IJK,M,M)*Mu_dstar/ &
                              SUM_EpsG0
                        ENDIF
! Shear viscosity
                        Mu_sv =((2.d0+ALPHA)/3.d0)*((Mu_star/(Eta*(2.d0-Eta)*&
                           G_0(IJK,M,M)))*(ONE+1.6d0*Eta*SUM_EpsG0)&
                           *(ONE+1.6d0*Eta*(3d0*Eta-2d0)*&
                           SUM_EpsG0)+(0.6d0*Mu_bv*Eta))
! dissipation due to collisions
                        gama = (48d0*Eta*(1d0-Eta)*RO_S(IJK,M)*EP_s(IJK,M)*&
                           SUM_EpsG0*Theta_m(IJK,M)**1.5d0)/&
                           (SQRT_Pi*D_p(IJK,M))
! in ssf S:grad(vel) ~ S:S = 1/2(du/dy)^2 = gama/(2*mu)
                        zeta = (gama / (2.d0*mu_sv))**0.5

                     ELSEIF (SAVAGE.EQ.0) THEN
! standard S:S form; but may evaluate to zero in regions of little/no flow
! S:S = SijSji = DijDji-1/3*tr(D)^2=tr(D^2)-1/3*tr(D)^2
                        zeta = (SMALL_NUMBER +&
                           trD_s2(IJK,M) - ((trD_s_C(IJK,M)*&
                           trD_s_C(IJK,M))/3.d0))**0.5d0

                     ELSE  ! combined form: srivastava & sundaresan (2003)
! to avoid above singularity in regions of little/no flow include a
! fluctuation term to standard S:S form
                        IF(KT_TYPE_ENUM == GHD_2007) THEN
! dp_avg calc. for ghd_07
                           dp_avg = ZERO
                           DO MM=1,SMAX
                              dp_avg = dp_avg + D_p(IJK,MM)
                           ENDDO
                           dp_avg = dp_avg/DBLE(SMAX)
                           zeta = ((Theta_m(IJK,M)/dp_avg**2) +&
                             (trD_s2(IJK,M) - ((trD_s_C(IJK,M)*&
                             trD_s_C(IJK,M))/3.d0)))**0.5d0
                        ELSE
                           zeta = ((Theta_m(IJK,M)/D_p(IJK,M)**2) +&
                             (trD_s2(IJK,M) - ((trD_s_C(IJK,M)*&
                             trD_s_C(IJK,M))/3.d0)))**0.5d0
                        ENDIF
                     ENDIF   ! end if/else savage

                     IF(GUO_BOYCE) THEN
                        shearRate_gamma=SQRT(2d0*(trD_s2(IJK,M) - ((trD_s_C(IJK,M)*trD_s_C(IJK,M))/3.d0)))
                        n1_t = 2d0*atan(10000d0*(ONE-EP_G(IJK)-EPS_f_min))/Pi
                     ENDIF

                     IF ((ONE-EP_G(IJK)).GT.((ONE-ep_star_array(ijk))-delta)) THEN
! Linearized form of Pc; this is more stable and provides continuous function.
! Linearize PC around small deviation from max. packing: eg=ep_star+delta
! Here we recognize  dpcdes = -dpcdep
                        IF(PRINCETON) THEN
                           dPcdep_at = -(to_SI*Fr)*&
                              ( ( N_Pc*((ONE-ep_star_array(IJK)-&
                              delta-eps_f_min)**(N_Pc-ONE))/(delta**D_Pc)) + &
                              ( D_Pc*((ONE-ep_star_array(ijk)-&
                              delta-eps_f_min)**N_pc)/(delta**(D_pc+ONE)) ) )
                           Pc_at = (to_SI*Fr)*&
                             ((ONE-ep_star_array(IJK)-delta-EPS_f_min)**N_Pc) / (delta**D_Pc)
                           Pc = Pc_at + dPcdep_at*(EP_G(IJK)-ep_star_array(IJK)-delta)
                           dPcdeps = -dPcdep_at
                        ELSEIF(GUO_BOYCE) THEN
                           Pc = (shearRate_gamma**2)*((ONE-ep_star_array(ijk)-EPS_f_min)**2)*(D_p(IJK,M)**2)*ro_s(IJK,M) / &
                                ( delta**2) + to_SI*a_ps * ((ONE-EP_G(IJK))-(ONE-ep_star_array(ijk))+delta)**b_ps

                           dPcdeps = to_SI*a_ps * b_ps * ((ONE-EP_G(IJK))-(ONE-ep_star_array(ijk))+delta)**(b_ps-1)
                        ENDIF

                     ELSE
                        IF(PRINCETON) THEN
                           Pc = (to_SI*Fr)*((ONE-EP_G(IJK)-EPS_f_min)**N_Pc) / &
                              ( ((ONE-ep_star_array(ijk)) - (ONE-EP_G(IJK)))**D_Pc)
                           dPcdeps = (to_SI*Fr)*&
                              ( ( N_Pc*((ONE-EP_G(IJK)-eps_f_min)**(N_Pc-ONE))/&
                              ((EP_G(IJK)-ep_star_array(ijk))**D_Pc) ) + &
                              ( D_Pc*((ONE-EP_G(IJK)-eps_f_min)**N_pc)/&
                              ((EP_G(IJK)-ep_star_array(IJK))**(D_Pc+ONE)) ) )
                        ELSEIF(GUO_BOYCE) THEN
                           Pc = (shearRate_gamma**2)*((ONE-ep_star_array(ijk)-EPS_f_min)**2)*(D_p(IJK,M)**2)*ro_s(IJK,M) / &
                                ( ((ONE-ep_star_array(ijk))-(ONE-EP_G(IJK)) )**2)*n1_t
                           dPcdeps = (20000*(ONE-ep_star_array(ijk)-EPS_f_min)**2*D_p(IJK,M)**2*ro_s(IJK,M)*shearRate_gamma**2)/ &
                                     (pi*((ONE-EP_G(IJK)) - (ONE-ep_star_array(ijk)))**2*((10000*(ONE-EP_G(IJK)) - 10000*EPS_f_min)**2 + 1)) - &
                                     (4*(ONE-ep_star_array(ijk)-EPS_f_min)**2*D_p(IJK,M)**2*ro_s(IJK,M)*shearRate_gamma**2* &
                                     atan(10000 *(ONE-EP_G(IJK)) - 10000*EPS_f_min))/ &
                                     (pi*((ONE-EP_G(IJK)) - (ONE-ep_star_array(ijk)))**3)

                        ENDIF

                     ENDIF

                     IF (trD_s_C(IJK,M) .GE. ZERO) THEN
                        N_Pff = DSQRT(3d0)/(2d0*Sin_Phi) !dilatation
                     ELSE
                        N_Pff = N_Pf !compaction
                     ENDIF

                     IF ((trD_s_C(IJK,M)/(zeta*N_Pff*DSQRT(2d0)*&
                          Sin_Phi)) .GT. 1d0) THEN
! avoid negative values in Pf
                        PfoPc = ZERO   !Pf = zero
                        dPfoPcdeps = ZERO
                     ELSEIF(trD_s_C(IJK,M) == ZERO) THEN
! avoid calculation in else branch below
                        PfoPc = ONE    !Pf = Pc
                        dPfoPcdeps = ZERO
                     ELSE
                        PfoPc = (1d0 - (trD_s_C(IJK,M)/(zeta*N_Pff*&
                           DSQRT(2d0)*Sin_Phi)))**(N_Pff-1d0)
                        dPfoPcdeps=ZERO

                        IF (SAVAGE.EQ.1) THEN
! additional work is needed given how zeta was evaluated
                           dzdeps = dzetadeps(EP_S(IJK,M), mu_dstar, &
                              gama, zeta, IJK, M)
                           dPfoPcdeps = (N_Pff-1d0)*&
                              (1d0 - (trD_s_C(IJK,M)/(zeta*N_Pff*&
                              DSQRT(2d0)*Sin_Phi)))**(N_Pff-2d0)*&
                              ( trD_s_C(IJK,M)/(N_Pff*DSQRT(2d0)*Sin_Phi*&
                              zeta*zeta) ) * dzdEps
                        ENDIF

! mu_s_f is set to zero if its evaluation results in a negative value.
! in this case set/solve for PfoPc (Pf) to make this true; a maximum
! allowed PfoPc exists to be to avoid negative viscosities..
                        PfoPc_max = ((N_Pf/(N_Pf-1d0))**(N_Pf-1d0))
                        IF (PfoPc > PfoPc_max) THEN
                           PfoPc = PfoPc_Max
                           dPfoPcdeps = ZERO
                        ENDIF
                     ENDIF

! Contributions to Kcp(IJK) from kinetic theory have been left out in
! the expressions below as they cause convergence problems at low solids
! volume fraction
                     Kcp(IJK) = PfoPc*dPcdeps + Pc*dPfoPcdeps

                  ELSE
! the solids are not sufficiently packed to invoke friction model
                     Kcp(IJK) = ZERO

                  ENDIF   ! end if/else branch (one-ep_g(ijk)>eps_f_min)
! end if friction and sufficiently packed to invoke friction

               ELSE ! FRICTION = .FALSE.


                  IF(EP_g(IJK) .LT. ep_g_blend_end(ijk)) THEN
! not friction but the solids are packed so that the plastic pressure
! model is invoked
! ---------------------------------------------------------------->>>

                     Kcp(IJK) = dPodEP_s(EP_s(IJK, M),ep_g_blend_end(ijk))

                     IF(BLENDING_STRESS) THEN
                        blend =  blend_function(IJK)
                        Kcp(IJK) = (1.0d0-blend) * Kcp(IJK)
                     ENDIF
                  ELSE
! the solids are not sufficiently packed to invoke the plastic stress
! model
                     Kcp(IJK) = ZERO
                  ENDIF
! end if not friction but sufficiently packed to invoke plastic pressure
! ----------------------------------------------------------------<<<

               ENDIF   ! end if/else friction

            ELSE    ! else branch of if (.not.wall_at(ijk))
               Kcp(IJK) = ZERO
            ENDIF   ! end if/else (.not.wall_at(ijk))

         ENDDO  ! do ijk=ijkstart3,ijkend3
      ENDIF   ! end if (close_packed(m))

      CALL send_recv(Kcp, 2)

      RETURN

   END SUBROUTINE CALC_K_cp


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Function: DZETADEPs                                                 C
!  Purpose: Calculate derivative of S:S like term w.r.t. solids        C
!           volume fraction                                            C
!                                                                      C
!  Author: A. Srivastava                              Date: 8-JUNE-98  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Modified: J. Carney 2018                                            C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      DOUBLE PRECISION FUNCTION DZETADEPs(EPs, mu_dstar, gama, zeta, IJK, M)

! Modules
!---------------------------------------------------------------------

      USE constant, only: sqrt_pi, pi
      USE constant, only: alpha, eta
      USE fldvar, only: ep_s, d_p, ro_s, theta_m
      USE param1, only: zero, one, small_number
      USE physprop, only: smax
      USE rdf, only: g_0, dG_0dnu
      IMPLICIT NONE

! Dummy Arguments
!---------------------------------------------------------------------
! solids volume fraction
      DOUBLE PRECISION, INTENT(IN) :: EPs
! component of solids viscosity
      DOUBLE PRECISION, INTENT(IN) :: mu_dstar
! collisional dissipation
      DOUBLE PRECISION, INTENT(IN) :: gama
! approximation for S:S based on granular SSF
      DOUBLE PRECISION, INTENT(IN) :: zeta
! indices
      INTEGER, INTENT(IN) :: IJK, M

! Local variables
!---------------------------------------------------------------------
      INTEGER :: MM
! various sum quantities
      DOUBLE PRECISION :: SUM_g0, SUM_EpsG0, SUM_EpsdG0
! variables for evaluating d(gama)/deps
      DOUBLE PRECISION :: dgama_t1, dgama_t2, dgamadeps
! variables for evaluating d(mu)/deps
      DOUBLE PRECISION :: C1, C2, C3, dmu_t1, dmu_t2, dmu_t3
      DOUBLE PRECISION :: dmu_t4, dmu_t5, dmu_t6, dmudeps
!--------------------------------------------------------------------

      SUM_EpsG0 = ZERO
      SUM_G0= ZERO
      SUM_EpsdG0 = ZERO
      DO MM = 1, SMAX
         SUM_EpsG0 =  SUM_EpsG0+EP_s(IJK,MM)*G_0(IJK,M,MM)
         SUM_G0 = SUM_G0+G_0(IJK,M,MM)
! so here we should be able to take the derivative of each
! dgMM_deps, however, dg_0dnu is limited...
         SUM_EpsdG0 =  SUM_EpsdG0+EP_s(IJK,MM)*DG_0DNU(eps)
      ENDDO

! here zeta=sqrt(S:S)=sqrt(gama/2.mu)
! d/deps (zeta) = d/deps ((gama/2.mu)^0.5)
!    =1/2 (gama/(2.mu))^-0.5*( 1/(2.mu)*d(gama)/deps - gama/(2.mu^2)*d(mu)/deps)
!    =1/(2*gama)*(zeta*d(gama)/deps - 2*zeta*d(mu)/deps)
! requires evaluation of d(gama)/deps and d(mu)/deps

! evaluate d(gama)/deps
      dgama_t1 = (48d0*Eta*(1d0-Eta)*RO_S(IJK,M)*SUM_EpsG0*&
         Theta_m(IJK,M)**1.5d0)/(SQRT_Pi*D_p(IJK,M))
      dgama_t2 = (48d0*Eta*(1d0-Eta)*RO_S(IJK,M)*EP_S(IJK,M)*&
         Theta_m(IJK,M)**1.5d0)/(SQRT_Pi*D_p(IJK,M))
      dgamadeps = dgama_t1 + dgama_t2*(SUM_G0+SUM_EpsdG0)

! evaluate d(mu)/deps
! note mu_star has g0 in numerator so carry
      C1 = ((2.d0+ALPHA)/3.d0)*(Mu_dstar/(Eta*(2.d0-Eta)*SUM_EpsG0))
      C2 = (ONE+1.6d0*Eta*SUM_EpsG0)
      C3 = (ONE+1.6d0*Eta*(3d0*Eta-2d0)*SUM_EpsG0)

      dmu_t1 = C1*C2*C3
      dmu_t2 = -(EP_s(IJK,M)*C1*C2*C3)/SUM_EpsdG0
      dmu_t3 = EP_s(IJK,M)*C1*C3*(1.6d0*eta)
      dmu_t4 = EP_s(IJK,M)*C1*C2*(1.6d0*eta*(3.d0*eta-2.d0))
      dmu_t6 = ((2.d0+ALPHA)/3.d0)*0.6d0*eta*(256.d0/(5.d0*PI))*mu_dstar
      dmu_t5 = dmu_t6*EP_s(IJK,M)

      dmudeps = dmu_t1 + (&
         dmu_t2+dmu_t3+dmu_t4+dmu_t5)*(SUM_G0+SUM_EpsdG0) + &
         dmu_t6*SUM_EpsG0

      IF(SMAX==1) THEN
! enforce cancellation of terms
         dmudeps = -dmu_t2*SUM_EpsdG0 + (&
            dmu_t3+dmu_t4+dmu_t5)*(SUM_G0+SUM_EpsdG0)+&
            dmu_t6*SUM_EpsG0
      ENDIF

! d/deps (zeta) = d/deps ((gama/2.mu)^0.5)
!    =1/(2*gama)*(zeta*d(gama)/deps - 2*zeta*d(mu)/deps)
      IF (gama > ZERO) THEN
         dzetadeps = (ONE/(2.d0*gama))*(zeta*dgamadeps-&
            2.d0*zeta**3*dmudeps)
      ELSE
         dzetadeps = (ONE/(2.d0*(gama+SMALL_NUMBER)))*(zeta*dgamadeps-&
            2.d0*zeta**3*dmudeps)
      ENDIF

     RETURN
   END FUNCTION DZETADEPs

END MODULE CALC_K_cp_MOD
