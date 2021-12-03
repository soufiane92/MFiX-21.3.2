#include "error.inc"

MODULE CALC_GR_BOUNDARY

      use error_manager

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: CALC_GRBDRY                                             C
!  Purpose: Main controller subroutine for calculating coefficients    C
!     for momentum boundary conditions using kinetic & frictional      C
!     theory                                                           C
!                                                                      C
!  Author: K. Agrawal & A. Srivastava, Princeton Univ. Date: 19-JAN-98 C
!  Reviewer:                                           Date:           C
!                                                                      C
!                                                                      C
!  Literature/Document References:                                     C
!     Johnson, P. C., and Jackson, R., Frictional-collisional          C
!        constitutive relations for granular materials, with           C
!        application to plane shearing, Journal of Fluid Mechanics,    C
!        1987, 176, pp. 67-93                                          C
!     Jenkins, J. T., and Louge, M. Y., On the flux of fluctuating     C
!        energy in a collisional grain flow at a flat frictional wall, C
!        Physics of Fluids, 1997, 9(10), pp. 2835-2840                 C
!     See calc_mu_s.f for references on kinetic and static-friction    C
!        theory models                                                 C
!                                                                      C
!  Additional Notes:                                                   C
!     The JJ granular momentum BC is written as:                       C
!     usw/|usw|.tau.n+C+F=0                                            C
!       tau = the stress tensor (collisional+frictional)               C
!       n = outward normal vector                                      C
!       usw = slip velocity with wall                                  C
!       C = collisional force                                          C
!       F = frictional force                                           C
!                                                                      C
!                                                                      C
!    The granular momentum BC is written as the normal vector dot the  C
!    stress tensor. Besides the gradient in velocity of phase M, the   C
!    stress tensor expression may contain several additional terms     C
!    that would need to be accounted for when satisfying the BC.       C
!    SUCH modifications have NOT been rigorously addressed.            C
!                                                                      C
!                                                                      C
!  Notes:                                                              C
!    This routine will not be entered unless granular_energy so we     C
!    do not need to check for its condition!                           C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE CALC_GRBDRY(IJK1, IJK2, FCELL, COM, M, L, Gw, Hw, Cw)

! Global variables
!---------------------------------------------------------------------
      USE calc_s_ddot_mod, only: calc_s_ddot_s
      USE constant, only: eps_f_min
      USE fldvar, only: ep_g
      USE run, only: friction
      USE param, only: dimension_m
      USE param1, only: zero, one
      IMPLICIT NONE

! Dummy Arguments
!---------------------------------------------------------------------
! IJK indices for wall cell (ijk1) and fluid cell (ijk2)
      INTEGER, INTENT(IN) :: IJK1, IJK2
! The location (e,w,n...) of fluid cell
      CHARACTER, INTENT(IN) :: FCELL
! Velocity component (U, V, W)
      CHARACTER :: COM
! Solids phase index
      INTEGER, INTENT(IN) :: M
! Index corresponding to boundary condition
      INTEGER, INTENT(IN) ::  L
! Wall momentum coefficients:
! 1st term on LHS
      DOUBLE PRECISION, INTENT(INOUT) :: Gw
! 2nd term on LHS
      DOUBLE PRECISION, INTENT(INOUT) :: Hw
! all terms appearing on RHS
      DOUBLE PRECISION, INTENT(INOUT) :: Cw

! Local variables
!---------------------------------------------------------------------
! Average scalars for gas phase
      DOUBLE PRECISION :: EPg, MUg, ROg
! Average void fraction at packing
      DOUBLE PRECISION :: ep_star
! Average scalars for solids phases
      DOUBLE PRECISION :: EPs(DIMENSION_M), &
                          DP(DIMENSION_M),&
                          TH(DIMENSION_M),&
                          ROS(DIMENSION_M)
! average radial distribution function at contact and sum of avg eps* avg G_0
      DOUBLE PRECISION :: g0(DIMENSION_M), SUM_epsxg0
! Average Simonin and Ahmadi variables (sof)
      DOUBLE PRECISION :: aK_12, aTau_12, aTau_1

! wall velocity of solids (from designated BC)
      DOUBLE PRECISION :: WVELS
! velocity of solids
      DOUBLE PRECISION :: VELS

! Magnitude of gas-solids relative velocity
      DOUBLE PRECISION :: VREL
! slip velocity between wall and particles for Jenkins bc (sof)
      DOUBLE PRECISION :: VSLIP

! del.u
      DOUBLE PRECISION :: DEL_DOT_U
! S:S
      DOUBLE PRECISION :: S_DDOT_S
! S_dd (d can be x, y or z)
      DOUBLE PRECISION :: S_dd
! this is approximately the sqrt(S:S)
      DOUBLE PRECISION :: zeta

! solids viscosity and pressure from kinetic theory at given location
       DOUBLE PRECISION :: MUsv, Psv
! solids viscosity and pressure from frictional theory at given location
       DOUBLE PRECISION :: MUsf, Psf
! collisional and frictional contributions to wall bc based on bc model
       DOUBLE PRECISION :: COLL_COMP, FRIC_COMP

!---------------------------------------------------------------------

! get quantities at location of interest via averaging. also calculate
! relative velocity and wall-slip velocity
      CALL CALC_BDRY_QOI(IJK1, IJK2, FCELL, COM, M, L, g0, eps, &
             epg, ep_star, sum_epsxg0, mug, rog, dp, th, ros,&
             ak_12, atau_12, atau_1, wvels, vels, vrel, vslip)

! calculate the viscosity and pressure from KT at boundary
! this routine needs to be called before others to allow possible
! modification of TH & sum_epsxg0
      CALL CALC_MU_SV_BDRY(M, g0, EPs, EPg, ep_star, SUM_EPsxg0, &
            DP, TH, ROs, aK_12, aTau_12, aTau_1, &
            MUg, ROg, VREL, MUsv, Psv)

! calculate the collisional contribution to the wall BC
      CALL CALC_GRBDRY_COLLISIONAL(M, g0, EPS, ep_star, &
            DP, TH, ROs, VSLIP, Psv, COLL_COMP)

      IF (FRICTION .AND. ((ONE-EP_G(IJK2)) > EPS_F_MIN) ) THEN
         IF ( (ONE-EPg) > EPS_F_MIN) THEN
            CALL CALC_S_DDOT_S(IJK1, IJK2, FCELL, COM, M, &
                  DEL_DOT_U, S_DDOT_S, S_dd)

            CALL CALC_MU_SF_BDRY(M, g0, EPs, EPg, ep_star, &
                sum_epsxg0, DP, TH, ROs, Mug, ROg, VREL, &
                DEL_DOT_U, S_DDOT_S, musf, psf, zeta)

            CALL CALC_GRBDRY_FRICTIONAL(musf, psf, s_dd, FRIC_COMP)
         ELSE
            FRIC_COMP = ZERO
            MUsf = zero
         ENDIF
         IF (VSLIP == ZERO .OR. zeta == zero) THEN
            HW = zero
         ELSE
            HW = (COLL_COMP+FRIC_COMP)/(MUsv+MUsf)
         ENDIF
      ELSE
         Hw = COLL_COMP/MUsv
      ENDIF
      GW = 1D0
      CW = HW*WVELS


      RETURN
      END SUBROUTINE CALC_GRBDRY


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: CALC_BDRY_QOI                                           C
!  Purpose: Determine all needed variables at the indicated boundary   C
!     location through appropriate averaging or assignment. Also       C
!     returns two calculated quantities: the relative velocity and     C
!     the solids slip velocity.                                        C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE CALC_BDRY_QOI(IJK1, IJK2, FCELL, COM, M, L, g0_avg, eps_avg, &
                  epg_avg, ep_star_avg, sum_epsg0_avg, mu_g_avg, ro_g_avg, &
                  dp_avg, th_avg, ros_avg, &
                  k_12_avg, tau_12_avg, tau_1_avg, &
                  wvels, vels, vrel, vslip)

! Global variables
!---------------------------------------------------------------------
      USE bc, only: bc_uw_s, bc_vw_s, bc_ww_s
      USE constant, only: to_si
      USE compar, only: myPE
      USE error_manager, only: err_msg
      USE write_error_mod, only: write_error
      USE mpi_utility, only: exitMPI

      USE fldvar, only: u_s, v_s, w_s
      USE fldvar, only: u_g, v_g, w_g
      USE fldvar, only: ep_g, ep_s, ro_g
      USE fldvar, only: ro_s, d_p, theta_m

      USE fun_avg, only: avg_x, avg_x_e
      USE fun_avg, only: avg_y, avg_y_n
      USE fun_avg, only: avg_z, avg_z_t
      USE functions, only: im_of, jm_of, km_of
      USE functions, only: ip_of, jp_of, kp_of
      USE functions, only: east_of, north_of, top_of

      USE indices, only: i_of, j_of, k_of
      USE param1, only: zero
      USE param, only: dimension_m

      USE physprop, only: smax, mu_g
      USE rdf, only: g_0avg

! kinetic theories
      USE run, only: kt_type_enum
      USE run, only: simonin_1996
      USE run, only: ahmadi_1995

      USE toleranc, only: zero_ep_s
      USE turb, only: k_12, tau_1, tau_12
      USE visc_s, only: ep_star_array
   IMPLICIT NONE

! Dummy Arguments
!---------------------------------------------------------------------
! IJK indices for wall cell (ijk1) and fluid cell (ijk2)
      INTEGER, INTENT(IN) :: IJK1, IJK2
! The location (e,w,n...) of fluid cell
      CHARACTER, INTENT(IN) :: FCELL
! Velocity component (U, V, W)
      CHARACTER :: COM
! Solids phase index
      INTEGER, INTENT(IN) :: M
! Index corresponding to boundary condition
      INTEGER, INTENT(IN) ::  L
! radial distribution function at contact and sum of eps*G_0
      DOUBLE PRECISION, INTENT(OUT) :: g0_avg(DIMENSION_M), &
                                       sum_epsg0_avg
! Average scalars for gas phase
      DOUBLE PRECISION, INTENT(OUT) :: EPg_avg, Mu_g_avg, RO_g_avg
! Average void fraction at packing
      DOUBLE PRECISION, INTENT(OUT) :: ep_star_avg
! Average scalars for solids phases
      DOUBLE PRECISION, INTENT(OUT) :: EPs_avg(DIMENSION_M), &
                                       DP_avg(DIMENSION_M),&
                                       TH_avg(DIMENSION_M),&
                                       ROS_AVG(DIMENSION_M)
! Average Simonin and Ahmadi variables (sof)
      DOUBLE PRECISION, INTENT(OUT) :: K_12_avg, Tau_12_avg, Tau_1_avg
! wall velocity of solids
      DOUBLE PRECISION, INTENT(OUT) :: WVELS
! velocity of solids
      DOUBLE PRECISION, INTENT(OUT) :: VELS
! Magnitude of gas-solids relative velocity
      DOUBLE PRECISION, INTENT(OUT) :: VREL
! slip velocity between wall and particles for Jenkins bc (sof)
      DOUBLE PRECISION, INTENT(OUT) :: VSLIP

! Local Variables
!---------------------------------------------------------------------
! Other indices
      INTEGER :: IJK2E, IPJK2, IPJKM2, IPJKP2, IPJMK2, IPJPK2
      INTEGER :: IJK2N, IJPK2, IJPKM2, IJPKP2, IMJPK2
      INTEGER :: IJK2T, IJKP2, IJMKP2, IMJKP2
! Solids phase index
      INTEGER :: MM
!
      DOUBLE PRECISION :: smallTheta
! intermediate average to adjust for possible corner cell condition
      DOUBLE PRECISION :: AVGX1, AVGX2

! Average velocities
! values of U_sm, V_sm, W_sm at appropriate place on boundary wall
      DOUBLE PRECISION :: USCM, VSCM, WSCM
      DOUBLE PRECISION :: USCM1, USCM2, VSCM1, VSCM2, WSCM1, WSCM2
! values of U_g, V_g, W_g at appropriate place on boundary wall
      DOUBLE PRECISION :: UGC, VGC, WGC
      DOUBLE PRECISION :: WGC1, WGC2, VGC1, VGC2, UGC1, UGC2
! Error message
      CHARACTER(LEN=80) :: LINE

!---------------------------------------------------------------------

! Note: EP_s, MU_g, and RO_g are undefined at IJK1 (wall cell).
!       Hence IJK2 (fluid cell) is used in averages.

      smallTheta = (to_SI)**4 * ZERO_EP_S

!-----------------------------------------------
! Calculations for U momentum equation
      IF (COM .EQ. 'U')THEN
        IJK2E = EAST_OF(IJK2)
        IPJK2 = IP_OF(IJK2)

        EPg_avg = AVG_X(EP_g(IJK2), EP_g(IJK2E), I_OF(IJK2))
        ep_star_avg = AVG_X(EP_star_array(IJK2), EP_star_array(IJK2E), I_OF(IJK2))
        Mu_g_avg = AVG_X(Mu_g(IJK2), Mu_g(IJK2E), I_OF(IJK2))
        RO_g_avg = AVG_X(RO_g(IJK2), RO_g(IJK2E), I_OF(IJK2))

        K_12_avg = ZERO
        Tau_12_avg = ZERO
        Tau_1_avg = ZERO
        IF(KT_TYPE_ENUM == SIMONIN_1996 .OR. &
           KT_TYPE_ENUM == AHMADI_1995) THEN
           Tau_1_avg = AVG_X(Tau_1(IJK2), Tau_1(IJK2E), I_OF(IJK2))
! Simonin only:
           K_12_avg = AVG_X(K_12(IJK2), K_12(IJK2E), I_OF(IJK2))
           Tau_12_avg = AVG_X(Tau_12(IJK2), Tau_12(IJK2E), I_OF(IJK2))
        ENDIF

        sum_epsg0_avg = ZERO
        DO MM = 1, SMAX
           g0_avg(MM)      = G_0AVG(IJK2, IJK2E, 'X', I_OF(IJK2), M, MM)
           EPs_avg(MM) = AVG_X(EP_s(IJK2, MM), EP_s(IJK2E, MM), I_OF(IJK2))
           DP_avg(MM)  = AVG_X(D_P(IJK2,MM), D_P(IJK2E,MM), I_OF(IJK2))
           sum_epsg0_avg   = sum_epsg0_avg + G_0AVG(IJK2, IJK2E, 'X', I_OF(IJK2), M, MM) &
                       * AVG_X(EP_s(IJK2, MM), EP_s(IJK2E, MM), I_OF(IJK2))
           ROs_avg(MM) = AVG_X(RO_S(IJK2, MM), RO_S(IJK2E, MM), I_OF(IJK2))

           TH_avg(MM) = AVG_X(THETA_M(IJK2,MM), THETA_M(IJK2E,MM), I_OF(IJK2))
           IF(TH_avg(MM) < ZERO) TH_avg(MM) = smallTheta
        ENDDO

        WVELS = BC_Uw_s(L,M)


        IF(FCELL .EQ. 'N')THEN
          IPJMK2 = JM_OF(IPJK2)

! code modified for some corner cells
          DO MM = 1, SMAX
             AVGX1 = AVG_X(Theta_m(IJK1,MM), Theta_m(IPJMK2,MM), I_OF(IJK1))
             AVGX2 = AVG_X(Theta_m(IJK2,MM), Theta_m(IPJK2,MM), I_OF(IJK2))
             IF(AVGX1 < ZERO .AND. AVGX2 > ZERO) AVGX1 = AVGX2
             IF(AVGX2 < ZERO .AND. AVGX1 > ZERO) AVGX2 = AVGX1
             IF(AVGX1 < ZERO .AND. AVGX2 < ZERO) THEN
                TH_avg(MM) = smallTheta
             ELSE
                TH_avg(MM) = AVG_Y(AVGX1, AVGX2, J_OF(IJK1))
             ENDIF
          ENDDO

! Calculate velocity components at i+1/2, j+1/2, k (relative to IJK1)
          UGC  = AVG_Y(U_g(IJK1), U_g(IJK2),J_OF(IJK1))
          VGC  = AVG_X(V_g(IJK1), V_g(IPJMK2),I_OF(IJK1))
          WGC1 = AVG_X(AVG_Z_T(W_g(KM_OF(IJK2)), W_g(IJK2)),&
                       AVG_Z_T(W_g(KM_OF(IPJK2)), W_g(IPJK2)),&
                       I_OF(IJK2))
          WGC2 = AVG_X(AVG_Z_T(W_g(KM_OF(IJK1)), W_g(IJK1)),&
                       AVG_Z_T(W_g(KM_OF(IPJMK2)), W_g(IPJMK2)),&
                       I_OF(IJK1))
          WGC  = AVG_Y(WGC2, WGC1, J_OF(IJK1))
          USCM = AVG_Y(U_s(IJK1,M), U_s(IJK2,M),J_OF(IJK1))
          VSCM = AVG_X(V_s(IJK1,M), V_s(IPJMK2,M),I_OF(IJK1))
          WSCM1= AVG_X(AVG_Z_T(W_s(KM_OF(IJK2),M),W_s(IJK2,M)),&
                       AVG_Z_T(W_s(KM_OF(IPJK2),M),W_s(IPJK2,M)),&
                       I_OF(IJK2))
          WSCM2= AVG_X(AVG_Z_T(W_s(KM_OF(IJK1),M),W_s(IJK1,M)),&
                       AVG_Z_T(W_s(KM_OF(IPJMK2),M),W_s(IPJMK2,M)),&
                       I_OF(IJK1))
          WSCM = AVG_Y(WSCM2, WSCM1, J_OF(IJK1))
          VELS = USCM

        ELSEIF(FCELL .EQ. 'S')THEN
          IPJPK2= JP_OF(IPJK2)

! code modified for some corner cells
          DO MM = 1, SMAX
             AVGX1 = AVG_X(Theta_m(IJK2,MM),Theta_m(IPJK2,MM),I_OF(IJK2))
             AVGX2 = AVG_X(Theta_m(IJK1,MM),Theta_m(IPJPK2,MM),I_OF(IJK1))
             IF(AVGX1 < ZERO .AND. AVGX2 > ZERO) AVGX1 = AVGX2
             IF(AVGX2 < ZERO .AND. AVGX1 > ZERO) AVGX2 = AVGX1
             IF(AVGX1 < ZERO .AND. AVGX2 < ZERO) THEN
                TH_avg(MM) = smallTheta
             ELSE
                TH_avg(MM) = AVG_Y(AVGX1, AVGX2, J_OF(IJK2))
             ENDIF
          ENDDO


! Calculate velocity components at i+1/2, j+1/2, k relative to IJK2
          UGC  = AVG_Y(U_g(IJK2),U_g(IJK1),J_OF(IJK2))
          VGC  = AVG_X(V_g(IJK2),V_g(IPJK2),I_OF(IJK2))
          WGC1 = AVG_X(AVG_Z_T(W_g(KM_OF(IJK2)), W_g(IJK2)),&
                       AVG_Z_T(W_g(KM_OF(IPJK2)), W_g(IPJK2)),&
                       I_OF(IJK2))
          WGC2 = AVG_X(AVG_Z_T(W_g(KM_OF(IJK1)), W_g(IJK1)),&
                       AVG_Z_T(W_g(KM_OF(IPJPK2)), W_g(IPJPK2)),&
                       I_OF(IJK1))
          WGC  = AVG_Y(WGC1, WGC2, J_OF(IJK2))
          USCM = AVG_Y(U_s(IJK2, M),U_s(IJK1, M),J_OF(IJK2))
          VSCM = AVG_X(V_s(IJK2, M),V_s(IPJK2, M),I_OF(IJK2))
          WSCM1= AVG_X(AVG_Z_T(W_s(KM_OF(IJK2),M),W_s(IJK2,M)),&
                       AVG_Z_T(W_s(KM_OF(IPJK2),M),W_s(IPJK2,M)),&
                       I_OF(IJK2))
          WSCM2= AVG_X(AVG_Z_T(W_s(KM_OF(IJK1),M),W_s(IJK1,M)),&
                       AVG_Z_T(W_s(KM_OF(IPJPK2),M),W_s(IPJPK2,M)),&
                       I_OF(IJK1))
          WSCM = AVG_Y(WSCM1, WSCM2, J_OF(IJK2))
          VELS = USCM

        ELSEIF(FCELL .EQ. 'T')THEN
          IPJKM2= KM_OF(IPJK2)

! code modified for some corner cells
          DO MM = 1, SMAX
             AVGX1 = AVG_X(Theta_m(IJK1,MM),Theta_m(IPJKM2,MM),I_OF(IJK1))
             AVGX2 = AVG_X(Theta_m(IJK2,MM),Theta_m(IPJK2,MM),I_OF(IJK2))
             IF(AVGX1 < ZERO .AND. AVGX2 > ZERO) AVGX1 = AVGX2
             IF(AVGX2 < ZERO .AND. AVGX1 > ZERO) AVGX2 = AVGX1
             IF(AVGX1 < ZERO .AND. AVGX2 < ZERO) THEN
                TH_avg(MM) = smallTheta
             ELSE
                TH_avg(MM) = AVG_Z(AVGX1, AVGX2, K_OF(IJK1))
             ENDIF
          ENDDO

! Calculate velocity components at i+1/2, j, k+1/2 relative to IJK1
          UGC  = AVG_Z(U_g(IJK1), U_g(IJK2), K_OF(IJK1))
          VGC1 = AVG_X(AVG_Y_N(V_g(JM_OF(IJK2)),V_g(IJK2)),&
                       AVG_Y_N(V_g(JM_OF(IPJK2)),V_g(IPJK2)),&
                       I_OF(IJK2))
          VGC2 = AVG_X(AVG_Y_N(V_g(JM_OF(IJK1)),V_g(IJK1)),&
                       AVG_Y_N(V_g(JM_OF(IPJKM2)),V_g(IPJKM2)),&
                       I_OF(IJK1))
          VGC  = AVG_Z(VGC2,VGC1,K_OF(IJK1))
          WGC  = AVG_X(W_g(IJK1), W_g(IPJKM2),I_OF(IJK1))
          USCM = AVG_Z(U_s(IJK1,M), U_s(IJK2,M), K_OF(IJK1))
          VSCM1= AVG_X(AVG_Y_N(V_s(JM_OF(IJK2),M),V_s(IJK2,M)),&
                       AVG_Y_N(V_s(JM_OF(IPJK2),M),V_s(IPJK2,M)),&
                       I_OF(IJK2))
          VSCM2= AVG_X(AVG_Y_N(V_s(JM_OF(IJK1),M),V_s(IJK1,M)),&
                       AVG_Y_N(V_s(JM_OF(IPJKM2),M),V_s(IPJKM2,M)),&
                       I_OF(IJK1))
          VSCM  = AVG_Z(VSCM2,VSCM1,K_OF(IJK1))
          WSCM = AVG_X(W_s(IJK1,M), W_s(IPJKM2,M), I_OF(IJK1))
          VELS = USCM

        ELSEIF(FCELL .EQ. 'B')THEN
          IPJKP2= KP_OF(IPJK2)

! code modified for some corner cells
          DO MM = 1, SMAX
             AVGX1 = AVG_X(Theta_m(IJK1,MM), Theta_m(IPJKP2,MM),I_OF(IJK1))
             AVGX2 = AVG_X(Theta_m(IJK2,MM), Theta_m(IPJK2,MM),I_OF(IJK2))
             IF(AVGX1 < ZERO .AND. AVGX2 > ZERO) AVGX1 = AVGX2
             IF(AVGX2 < ZERO .AND. AVGX1 > ZERO) AVGX2 = AVGX1
             IF(AVGX1 < ZERO .AND. AVGX2 < ZERO) THEN
                TH_avg(MM) = smallTheta
             ELSE
                TH_avg(MM) = AVG_Z(AVGX1, AVGX2, K_OF(IJK2))
             ENDIF
          ENDDO

! Calculate velocity components at i+1/2, j, k+1/2 relative to IJK2
          UGC  = AVG_Z(U_g(IJK2), U_g(IJK1), K_OF(IJK2))
          VGC1 = AVG_X(AVG_Y_N(V_g(JM_OF(IJK2)),V_g(IJK2)),&
                       AVG_Y_N(V_g(JM_OF(IPJK2)),V_g(IPJK2)),&
                       I_OF(IJK2))
          VGC2 = AVG_X(AVG_Y_N(V_g(JM_OF(IJK1)),V_g(IJK1)),&
                       AVG_Y_N(V_g(JM_OF(IPJKP2)),V_g(IPJKP2)),&
                       I_OF(IJK1))
          VGC  = AVG_Z(VGC1, VGC2, K_OF(IJK2))
          WGC  = AVG_X(W_g(IJK2), W_g(IPJK2),I_OF(IJK2))
          USCM = AVG_Z(U_s(IJK2, M), U_s(IJK1, M), K_OF(IJK2))
          VSCM1= AVG_X(AVG_Y_N(V_s(JM_OF(IJK2),M),V_s(IJK2,M)),&
                       AVG_Y_N(V_s(JM_OF(IPJK2),M),V_s(IPJK2,M)),&
                       I_OF(IJK2))
          VSCM2= AVG_X(AVG_Y_N(V_s(JM_OF(IJK1),M),V_s(IJK1,M)),&
                       AVG_Y_N(V_s(JM_OF(IPJKP2),M),V_s(IPJKP2,M)),&
                       I_OF(IJK1))
          VSCM = AVG_Z(VSCM1, VSCM2, K_OF(IJK2))
          WSCM = AVG_X(W_s(IJK2, M), W_s(IPJK2, M),I_OF(IJK2))
          VELS = USCM

        ELSE
          WRITE(LINE,'(A, A)') 'Error: Unknown FCELL'
          CALL WRITE_ERROR('CALC_GRBDRY_QOI', LINE, 1)
          CALL exitMPI(myPE)
        ENDIF

!-----------------------------------------------
! Calculations for V momentum equation
      ELSEIF (COM .EQ. 'V')THEN

        IJK2N = NORTH_OF(IJK2)
        IJPK2 = JP_OF(IJK2)

        EPg_avg = AVG_Y(EP_g(IJK2), EP_g(IJK2N), J_OF(IJK2))
        ep_star_avg = AVG_Y(EP_star_array(IJK2), EP_star_array(IJK2N), J_OF(IJK2))
        Mu_g_avg = AVG_Y(Mu_g(IJK2), Mu_g(IJK2N), J_OF(IJK2))
        RO_g_avg = AVG_Y(RO_g(IJK2), RO_g(IJK2N), J_OF(IJK2))

        K_12_avg = ZERO
        Tau_12_avg = ZERO
        Tau_1_avg = ZERO
        IF(KT_TYPE_ENUM == SIMONIN_1996 .OR. &
           KT_TYPE_ENUM == AHMADI_1995) THEN
           Tau_1_avg = AVG_Y(Tau_1(IJK2), Tau_1(IJK2N), J_OF(IJK2))
! Simonin only:
           K_12_avg = AVG_Y(K_12(IJK2), K_12(IJK2N), J_OF(IJK2))
           Tau_12_avg = AVG_Y(Tau_12(IJK2), Tau_12(IJK2N), J_OF(IJK2))
        ENDIF


        sum_epsg0_avg = ZERO
        DO MM = 1, SMAX
           g0_avg(MM)      = G_0AVG(IJK2, IJK2N, 'Y', J_OF(IJK2), M, MM)
           EPs_avg(MM) = AVG_Y(EP_s(IJK2, MM), EP_s(IJK2N, MM), J_OF(IJK2))
           DP_avg(MM)  = AVG_Y(D_p(IJK2,MM), D_p(IJK2N,MM), J_OF(IJK2))
           sum_epsg0_avg   = sum_epsg0_avg + G_0AVG(IJK2, IJK2N, 'Y', J_OF(IJK2), M, MM) &
                        * AVG_Y(EP_s(IJK2, MM), EP_s(IJK2N, MM), J_OF(IJK2))
           ROS_avg(MM) = AVG_Y(RO_S(IJK2, MM), RO_S(IJK2N, MM), J_OF(IJK2))
           TH_avg(MM) = AVG_Y(THETA_M(IJK2,MM), THETA_M(IJK2N,MM), J_OF(IJK2))
           IF(TH_avg(MM) < ZERO) TH_avg(MM) = smallTheta
        ENDDO

        WVELS = BC_Vw_s(L, M)

        IF(FCELL .EQ. 'T')THEN
          IJPKM2 = KM_OF(IJPK2)

          DO MM = 1, SMAX
             AVGX1 = AVG_Z(Theta_m(IJK1,MM), Theta_m(IJK2,MM), K_OF(IJK1))
             AVGX2 = AVG_Z(Theta_m(IJPKM2,MM), Theta_m(IJPK2,MM), K_OF(IJPKM2))
             IF(AVGX1 < ZERO .AND. AVGX2 > ZERO) AVGX1 = AVGX2
             IF(AVGX2 < ZERO .AND. AVGX1 > ZERO) AVGX2 = AVGX1
             IF(AVGX1 < ZERO .AND. AVGX2 < ZERO) THEN
                TH_avg(MM) = smallTheta
             ELSE
                TH_avg(MM) = AVG_Y(AVGX1, AVGX2, J_OF(IJK1))
             ENDIF
          ENDDO

! Calculate velocity components at i, j+1/2, k+1/2 (relative to IJK1)
          UGC1 = AVG_X_E(&
                         AVG_Y(U_g(IM_OF(IJK1)), U_g(IM_OF(IJPKM2)),&
                         J_OF(IM_OF(IJK1))),&
                         AVG_Y(U_g(IJK1), U_g(IJPKM2),J_OF(IJK1)),&
                         I_OF(IJK1))
          UGC2 = AVG_X_E(&
                         AVG_Y(U_g(IM_OF(IJK2)), U_g(IM_OF(IJPK2)),&
                         J_OF(IM_OF(IJK2))),&
                         AVG_Y(U_g(IJK2), U_g(IJPK2),J_OF(IJK2)),&
                         I_OF(IJK2))
          UGC  = AVG_Z(UGC1, UGC2, K_OF(IJK1))
          VGC  = AVG_Z(V_g(IJK1), V_g(IJK2),K_OF(IJK1))
          WGC  = AVG_Y(W_g(IJK1), W_g(IJPKM2), J_OF(IJK1))
          USCM1= AVG_X_E(&
                       AVG_Y(U_s(IM_OF(IJK1),M),U_s(IM_OF(IJPKM2),M),&
                       J_OF(IM_OF(IJK1))),&
                       AVG_Y(U_s(IJK1,M), U_s(IJPKM2,M),J_OF(IJK1)),&
                       I_OF(IJK1))
          USCM2= AVG_X_E(&
                       AVG_Y(U_s(IM_OF(IJK2),M),U_s(IM_OF(IJPK2),M),&
                       J_OF(IM_OF(IJK2))),&
                       AVG_Y(U_s(IJK2,M), U_s(IJPK2,M),J_OF(IJK2)),&
                       I_OF(IJK2))
          USCM = AVG_Z(USCM1, USCM2, K_OF(IJK1))
          VSCM = AVG_Z(V_s(IJK1,M), V_s(IJK2,M),K_OF(IJK1))
          WSCM = AVG_Y(W_s(IJK1,M), W_s(IJPKM2,M), J_OF(IJK1))
          VELS = VSCM

        ELSEIF(FCELL .EQ. 'B')THEN
          IJPKP2 = KP_OF(IJPK2)

          DO MM = 1, SMAX
             AVGX1 = AVG_Z(Theta_m(IJK2,MM), Theta_m(IJK1,MM), K_OF(IJK2))
             AVGX2 = AVG_Z(Theta_m(IJPK2,MM), Theta_m(IJPKP2,MM), K_OF(IJPK2))
             IF(AVGX1 < ZERO .AND. AVGX2 > ZERO) AVGX1 = AVGX2
             IF(AVGX2 < ZERO .AND. AVGX1 > ZERO) AVGX2 = AVGX1
             IF(AVGX1 < ZERO .AND. AVGX2 < ZERO) THEN
                TH_avg(MM) = smallTheta
             ELSE
                TH_avg(MM) = AVG_Y(AVGX1, AVGX2, J_OF(IJK2))
             ENDIF
          ENDDO

! Calculate velocity components at i, j+1/2, k+1/2 (relative to IJK2)
          UGC1 = AVG_X_E(&
                         AVG_Y(U_g(IM_OF(IJK1)), U_g(IM_OF(IJPKP2)),&
                         J_OF(IM_OF(IJK1))),&
                         AVG_Y(U_g(IJK1), U_g(IJPKP2),J_OF(IJK1)),&
                         I_OF(IJK1))
          UGC2 = AVG_X_E(&
                         AVG_Y(U_g(IM_OF(IJK2)), U_g(IM_OF(IJPK2)),&
                         J_OF(IM_OF(IJK2))),&
                         AVG_Y(U_g(IJK2), U_g(IJPK2),J_OF(IJK2)),&
                         I_OF(IJK2))
          UGC  = AVG_Z(UGC2, UGC1, K_OF(IJK2))
          VGC  = AVG_Z(V_g(IJK2), V_g(IJK1),K_OF(IJK2))
          WGC  = AVG_Y(W_g(IJK2), W_g(IJPK2), J_OF(IJK2))
          USCM1= AVG_X_E(&
                       AVG_Y(U_s(IM_OF(IJK1),M),U_s(IM_OF(IJPKP2),M),&
                       J_OF(IM_OF(IJK1))),&
                       AVG_Y(U_s(IJK1,M), U_s(IJPKP2,M),J_OF(IJK1)),&
                       I_OF(IJK1))
          USCM2= AVG_X_E(&
                       AVG_Y(U_s(IM_OF(IJK2),M),U_s(IM_OF(IJPK2),M),&
                       J_OF(IM_OF(IJK2))),&
                       AVG_Y(U_s(IJK2,M), U_s(IJPK2,M),J_OF(IJK2)),&
                       I_OF(IJK2))
          USCM = AVG_Z(USCM2, USCM1, K_OF(IJK2))
          VSCM = AVG_Z(V_s(IJK2,M), V_s(IJK1,M),K_OF(IJK2))
          WSCM = AVG_Y(W_s(IJK2,M), W_s(IJPK2,M), J_OF(IJK2))
          VELS = VSCM

        ELSEIF(FCELL .EQ. 'E')THEN
          IMJPK2= IM_OF(IJPK2)

          DO MM = 1, SMAX
             AVGX1 = AVG_X(Theta_m(IJK1,MM),Theta_m(IJK2,MM),I_OF(IJK1))
             AVGX2 = AVG_X(Theta_m(IMJPK2,MM),Theta_m(IJPK2,MM),I_OF(IMJPK2))
             IF(AVGX1 < ZERO .AND. AVGX2 > ZERO) AVGX1 = AVGX2
             IF(AVGX2 < ZERO .AND. AVGX1 > ZERO) AVGX2 = AVGX1
             IF(AVGX1 < ZERO .AND. AVGX2 < ZERO) THEN
                TH_avg(MM) = smallTheta
             ELSE
                TH_avg(MM) = AVG_Y(AVGX1, AVGX2, J_OF(IJK1))
             ENDIF
          ENDDO

! Calculate velocity components at i+1/2, j+1/2, k relative to IJK1
          UGC  = AVG_Y(U_g(IJK1), U_g(IMJPK2), J_OF(IJK1))
          VGC  = AVG_X(V_g(IJK1), V_g(IJK2), I_OF(IJK1))
          WGC1 = AVG_Y(AVG_Z_T(W_g(KM_OF(IJK1)), W_g(IJK1)),&
                       AVG_Z_T(W_g(KM_OF(IMJPK2)), W_g(IMJPK2)),&
                       J_OF(IJK1))
          WGC2 = AVG_Y(AVG_Z_T(W_g(KM_OF(IJK2)), W_g(IJK2)),&
                       AVG_Z_T(W_g(KM_OF(IJPK2)), W_g(IJPK2)),&
                       J_OF(IJK2))
          WGC  = AVG_X(WGC1, WGC2, I_OF(IJK1))
          USCM = AVG_Y(U_s(IJK1,M), U_s(IMJPK2,M), J_OF(IJK1))
          VSCM = AVG_X(V_s(IJK1, M), V_s(IJK2, M), I_OF(IJK1))
          WSCM1= AVG_Y(AVG_Z_T(W_s(KM_OF(IJK1),M), W_s(IJK1,M)),&
                       AVG_Z_T(W_s(KM_OF(IMJPK2),M), W_s(IMJPK2,M)),&
                       J_OF(IJK1))
          WSCM2 = AVG_Y(AVG_Z_T(W_s(KM_OF(IJK2),M), W_s(IJK2,M)),&
                       AVG_Z_T(W_s(KM_OF(IJPK2),M), W_s(IJPK2,M)),&
                       J_OF(IJK2))
          WSCM  = AVG_X(WSCM1, WSCM2, I_OF(IJK1))
          VELS = VSCM

        ELSEIF(FCELL .EQ. 'W')THEN
          IPJPK2= IP_OF(IJPK2)

          DO MM = 1, SMAX
             AVGX1 = AVG_X(Theta_m(IJK2,MM),Theta_m(IJK1,MM),I_OF(IJK2))
             AVGX2 = AVG_X(Theta_m(IJPK2,MM),Theta_m(IPJPK2,MM),I_OF(IJPK2))
             IF(AVGX1 < ZERO .AND. AVGX2 > ZERO) AVGX1 = AVGX2
             IF(AVGX2 < ZERO .AND. AVGX1 > ZERO) AVGX2 = AVGX1
             IF(AVGX1 < ZERO .AND. AVGX2 < ZERO) THEN
                TH_avg(MM) = smallTheta
             ELSE
                TH_avg(MM) = AVG_Y(AVGX1, AVGX2, J_OF(IJK2))
             ENDIF
          ENDDO

! Calculate velocity components at i+1/2, j+1/2, k relative to IJK2
          UGC  = AVG_Y(U_g(IJK2), U_g(IJPK2), J_OF(IJK2))
          VGC  = AVG_X(V_g(IJK2), V_g(IJK1), I_OF(IJK2))
          WGC1 = AVG_Y(AVG_Z_T(W_g(KM_OF(IJK1)), W_g(IJK1)),&
                       AVG_Z_T(W_g(KM_OF(IPJPK2)), W_g(IPJPK2)),&
                       J_OF(IJK1))
          WGC2 = AVG_Y(AVG_Z_T(W_g(KM_OF(IJK2)), W_g(IJK2)),&
                       AVG_Z_T(W_g(KM_OF(IJPK2)), W_g(IJPK2)),&
                       J_OF(IJK2))
          WGC  = AVG_X(WGC2, WGC1, I_OF(IJK2))
          USCM = AVG_Y(U_s(IJK2,M), U_s(IJPK2,M), J_OF(IJK2))
          VSCM = AVG_X(V_s(IJK2, M), V_s(IJK1, M), I_OF(IJK2))
          WSCM1= AVG_Y(AVG_Z_T(W_s(KM_OF(IJK1),M), W_s(IJK1,M)),&
                       AVG_Z_T(W_s(KM_OF(IPJPK2),M), W_s(IPJPK2,M)),&
                       J_OF(IJK1))
          WSCM2 = AVG_Y(AVG_Z_T(W_s(KM_OF(IJK2),M), W_s(IJK2,M)),&
                       AVG_Z_T(W_s(KM_OF(IJPK2),M), W_s(IJPK2,M)),&
                       J_OF(IJK2))
          WSCM  = AVG_X(WSCM2, WSCM1, I_OF(IJK2))
          VELS = VSCM

        ELSE
          WRITE(LINE,'(A, A)') 'Error: Unknown FCELL'
          CALL WRITE_ERROR('CALC_GRBDRY_QOI', LINE, 1)
          CALL exitMPI(myPE)
        ENDIF


!-----------------------------------------------
! Calculations for W momentum equation
      ELSEIF (COM .EQ. 'W')THEN
        IJK2T = TOP_OF(IJK2)
        IJKP2 = KP_OF(IJK2)

        EPg_avg = AVG_Z(EP_g(IJK2), EP_g(IJK2T), K_OF(IJK2))
        Mu_g_avg = AVG_Z(Mu_g(IJK2), Mu_g(IJK2T), K_OF(IJK2))
        RO_g_avg = AVG_Z(RO_g(IJK2), RO_g(IJK2T), K_OF(IJK2))
        ep_star_avg = AVG_Z(EP_star_array(IJK2), EP_star_array(IJK2T), K_OF(IJK2))

        K_12_avg = ZERO
        Tau_12_avg = ZERO
        Tau_1_avg = ZERO
        IF(KT_TYPE_ENUM == SIMONIN_1996 .OR. &
           KT_TYPE_ENUM == AHMADI_1995) THEN
           Tau_1_avg = AVG_Z(Tau_1(IJK2), Tau_1(IJK2T), K_OF(IJK2))
! Simonin only:
           K_12_avg = AVG_Z(K_12(IJK2), K_12(IJK2T), K_OF(IJK2))
           Tau_12_avg = AVG_Z(Tau_12(IJK2), Tau_12(IJK2T), K_OF(IJK2))
        ENDIF

        sum_epsg0_avg = ZERO
        DO MM = 1, SMAX
           g0_avg(MM)      = G_0AVG(IJK2, IJK2T, 'Z', K_OF(IJK2), M, MM)
           EPs_avg(MM) = AVG_Z(EP_s(IJK2,MM), EP_s(IJK2T,MM), K_OF(IJK2))
           DP_avg(MM)  = AVG_Z(D_p(IJK2,MM), D_p(IJK2T,MM), K_OF(IJK2))
           sum_epsg0_avg   = sum_epsg0_avg + G_0AVG(IJK2, IJK2T, 'Z', K_OF(IJK2), M, MM) &
                       * AVG_Z(EP_s(IJK2, MM), EP_s(IJK2T, MM), K_OF(IJK2))
           ROs_avg(MM) = AVG_Z(RO_S(IJK2,MM), RO_S(IJK2T,MM), K_OF(IJK2))
           TH_avg(MM) = AVG_Z(THETA_M(IJK2,MM), THETA_M(IJK2T,MM), K_OF(IJK2))
           IF(TH_avg(MM) < ZERO) TH_avg(MM) = smallTheta
        ENDDO

        WVELS = BC_Ww_s(L,M)

        IF(FCELL .EQ. 'N')THEN
          IJMKP2 = JM_OF(IJKP2)

          DO MM = 1, SMAX
             AVGX1 = AVG_Z(Theta_m(IJK1,MM), Theta_m(IJMKP2,MM), K_OF(IJK1))
             AVGX2 = AVG_Z(Theta_m(IJK2,MM), Theta_m(IJKP2,MM), K_OF(IJK2))
             IF(AVGX1 < ZERO .AND. AVGX2 > ZERO) AVGX1 = AVGX2
             IF(AVGX2 < ZERO .AND. AVGX1 > ZERO) AVGX2 = AVGX1
             IF(AVGX1 < ZERO .AND. AVGX2 < ZERO) THEN
               TH_avg(MM) = smallTheta
             ELSE
               TH_avg(MM) = AVG_Y(AVGX1, AVGX2, J_OF(IJK1))
             ENDIF
          ENDDO

! Calculate velocity components at i, j+1/2, k+1/2 (relative to IJK1)
          UGC1 = AVG_X_E(&
                         AVG_Z(U_g(IM_OF(IJK1)), U_g(IM_OF(IJMKP2)),&
                         K_OF(IM_OF(IJK1)) ),&
                         AVG_Z(U_g(IJK1), U_g(IJMKP2), K_OF(IJK1)),&
                         I_OF(IJK1))
          UGC2 = AVG_X_E(&
                         AVG_Z(U_g(IM_OF(IJK2)), U_g(IM_OF(IJKP2)),&
                         K_OF(IM_OF(IJK2))),&
                         AVG_Z(U_g(IJK2), U_g(IJKP2), K_OF(IJK2)),&
                         I_OF(IJK2))
          UGC  = AVG_Y(UGC1, UGC2, J_OF(IJK1))
          VGC  = AVG_Z(V_g(IJK1), V_g(IJMKP2),K_OF(IJK1))
          WGC  = AVG_Y(W_g(IJK1), W_g(IJK2), J_OF(IJK1))
          USCM1= AVG_X_E(&
                       AVG_Z(U_s(IM_OF(IJK1),M),U_s(IM_OF(IJMKP2),M),&
                       K_OF(IM_OF(IJK1))),&
                       AVG_Z(U_s(IJK1,M), U_s(IJMKP2,M),K_OF(IJK1)),&
                       I_OF(IJK1))
          USCM2= AVG_X_E(&
                       AVG_Z(U_s(IM_OF(IJK2),M),U_s(IM_OF(IJKP2),M),&
                       K_OF(IM_OF(IJK2))),&
                       AVG_Z(U_s(IJK2,M), U_s(IJKP2,M),K_OF(IJK2)),&
                       I_OF(IJK2))
          USCM = AVG_Y(USCM1, USCM2, J_OF(IJK1))
          VSCM = AVG_Z(V_s(IJK1,M), V_s(IJMKP2,M),K_OF(IJK1))
          WSCM = AVG_Y(W_s(IJK1,M), W_s(IJK2,M), J_OF(IJK1))
          VELS = WSCM

        ELSEIF(FCELL .EQ. 'S')THEN
          IJPKP2 = JP_OF(IJKP2)

          DO MM = 1, SMAX
             AVGX1 = AVG_Z(Theta_m(IJK2,MM), Theta_m(IJKP2,MM), K_OF(IJK2))
             AVGX2 = AVG_Z(Theta_m(IJK1,MM), Theta_m(IJPKP2,MM), K_OF(IJK1))
             IF(AVGX1 < ZERO .AND. AVGX2 > ZERO) AVGX1 = AVGX2
             IF(AVGX2 < ZERO .AND. AVGX1 > ZERO) AVGX2 = AVGX1
             IF(AVGX1 < ZERO .AND. AVGX2 < ZERO) THEN
               TH_avg(MM) = smallTheta
             ELSE
               TH_avg(MM) = AVG_Y(AVGX1, AVGX2, J_OF(IJK2))
             ENDIF
           ENDDO

! Calculate velocity components at i, j+1/2, k+1/2 (relative to IJK2)
          UGC1 = AVG_X_E(&
                         AVG_Z(U_g(IM_OF(IJK1)), U_g(IM_OF(IJPKP2)),&
                         K_OF(IM_OF(IJK1))),&
                         AVG_Z(U_g(IJK1), U_g(IJPKP2), K_OF(IJK1)),&
                         I_OF(IJK1))
          UGC2 = AVG_X_E(&
                         AVG_Z(U_g(IM_OF(IJK2)), U_g(IM_OF(IJKP2)),&
                         K_OF(IM_OF(IJK2))),&
                         AVG_Z(U_g(IJK2), U_g(IJKP2), K_OF(IJK2)),&
                         I_OF(IJK2))
          UGC  = AVG_Y(UGC2, UGC1, J_OF(IJK2))
          VGC  = AVG_Z(V_g(IJK2), V_g(IJKP2),K_OF(IJK2))
          WGC  = AVG_Y(W_g(IJK2), W_g(IJK1), J_OF(IJK2))
          USCM1= AVG_X_E(&
                       AVG_Z(U_s(IM_OF(IJK1),M),U_s(IM_OF(IJPKP2),M),&
                       K_OF(IM_OF(IJK1))),&
                       AVG_Z(U_s(IJK1,M), U_s(IJPKP2,M),K_OF(IJK1)),&
                       I_OF(IJK1))
          USCM2= AVG_X_E(&
                       AVG_Z(U_s(IM_OF(IJK2),M),U_s(IM_OF(IJKP2),M),&
                       K_OF(IM_OF(IJK2))),&
                       AVG_Z(U_s(IJK2,M), U_s(IJKP2,M),K_OF(IJK2)),&
                       I_OF(IJK2))
          USCM = AVG_Y(USCM2, USCM1, J_OF(IJK2))
          VSCM = AVG_Z(V_s(IJK2,M), V_s(IJKP2,M),K_OF(IJK2))
          WSCM = AVG_Y(W_s(IJK2,M), W_s(IJK1,M), J_OF(IJK2))
          VELS = WSCM

        ELSEIF(FCELL .EQ. 'E')THEN
          IMJKP2 = IM_OF(IJKP2)

          DO MM = 1, SMAX
             AVGX1 = AVG_X(Theta_m(IJK1,MM),Theta_m(IJK2,MM),I_OF(IJK1))
             AVGX2 = AVG_X(Theta_m(IMJKP2,MM),Theta_m(IJKP2,MM),I_OF(IMJKP2))
             IF(AVGX1 < ZERO .AND. AVGX2 > ZERO) AVGX1 = AVGX2
             IF(AVGX2 < ZERO .AND. AVGX1 > ZERO) AVGX2 = AVGX1
             IF(AVGX1 < ZERO .AND. AVGX2 < ZERO) THEN
               TH_avg(MM) = smallTheta
             ELSE
               TH_avg(MM) = AVG_Z(AVGX1, AVGX2, K_OF(IJK1))
             ENDIF
          ENDDO

! Calculate velocity components at i+1/2, j, k+1/2 relative to IJK1
          UGC  = AVG_Z(U_g(IJK1), U_g(IMJKP2), K_OF(IJK1))
          VGC1 = AVG_Z(AVG_Y_N(V_g(JM_OF(IJK1)),V_g(IJK1)),&
                       AVG_Y_N(V_g(JM_OF(IMJKP2)),V_g(IMJKP2)),&
                       K_OF(IJK1))
          VGC2 = AVG_Z(AVG_Y_N(V_g(JM_OF(IJK2)),V_g(IJK2)),&
                       AVG_Y_N(V_g(JM_OF(IJKP2)),V_g(IJKP2)),&
                       K_OF(IJK2))
          VGC  = AVG_X(VGC1,VGC2,I_OF(IJK1))
          WGC  = AVG_X(W_g(IJK1), W_g(IJK2),I_OF(IJK1))
          USCM = AVG_Z(U_s(IJK1,M), U_s(IMJKP2,M), K_OF(IJK1))
          VSCM1= AVG_Z(AVG_Y_N(V_s(JM_OF(IJK1),M),V_s(IJK1,M)),&
                       AVG_Y_N(V_s(JM_OF(IMJKP2),M),V_s(IMJKP2,M)),&
                       K_OF(IJK1))
          VSCM2= AVG_Z(AVG_Y_N(V_s(JM_OF(IJK2),M),V_s(IJK2,M)),&
                       AVG_Y_N(V_s(JM_OF(IJKP2),M),V_s(IJKP2,M)),&
                       K_OF(IJK2))
          VSCM  = AVG_X(VSCM1,VSCM2,I_OF(IJK1))
          WSCM = AVG_X(W_s(IJK1,M), W_s(IJK2,M), I_OF(IJK1))
          VELS = WSCM

        ELSEIF(FCELL .EQ. 'W')THEN
          IPJKP2= IP_OF(IJKP2)

          DO MM = 1, SMAX
             AVGX1 = AVG_X(Theta_m(IJK2,MM),Theta_m(IJK1,MM),I_OF(IJK2))
             AVGX2 = AVG_X(Theta_m(IJKP2,MM),Theta_m(IPJKP2,MM),I_OF(IJKP2))
             IF(AVGX1 < ZERO .AND. AVGX2 > ZERO) AVGX1 = AVGX2
             IF(AVGX2 < ZERO .AND. AVGX1 > ZERO) AVGX2 = AVGX1
             IF(AVGX1 < ZERO .AND. AVGX2 < ZERO) THEN
               TH_avg(MM) = smallTheta
             ELSE
               TH_avg(MM) = AVG_Z(AVGX1, AVGX2, K_OF(IJK2))
             ENDIF
          ENDDO

! Calculate velocity components at i+1/2, j, k+1/2 relative to IJK2
          UGC  = AVG_Z(U_g(IJK2), U_g(IJKP2), K_OF(IJK2))
          VGC1 = AVG_Z(AVG_Y_N(V_g(JM_OF(IJK1)),V_g(IJK1)),&
                       AVG_Y_N(V_g(JM_OF(IPJKP2)),V_g(IPJKP2)),&
                       K_OF(IJK1))
          VGC2 = AVG_Z(AVG_Y_N(V_g(JM_OF(IJK2)),V_g(IJK2)),&
                       AVG_Y_N(V_g(JM_OF(IJKP2)),V_g(IJKP2)),&
                       K_OF(IJK2))
          VGC  = AVG_X(VGC2,VGC1,I_OF(IJK2))
          WGC  = AVG_X(W_g(IJK2), W_g(IJK1),I_OF(IJK2))
          USCM = AVG_Z(U_s(IJK2,M), U_s(IJKP2,M), K_OF(IJK2))
          VSCM1= AVG_Z(AVG_Y_N(V_s(JM_OF(IJK1),M),V_s(IJK1,M)),&
                       AVG_Y_N(V_s(JM_OF(IPJKP2),M),V_s(IPJKP2,M)),&
                       K_OF(IJK1))
          VSCM2= AVG_Z(AVG_Y_N(V_s(JM_OF(IJK2),M),V_s(IJK2,M)),&
                       AVG_Y_N(V_s(JM_OF(IJKP2),M),V_s(IJKP2,M)),&
                       K_OF(IJK2))
          VSCM  = AVG_X(VSCM2,VSCM1,I_OF(IJK2))
          WSCM = AVG_X(W_s(IJK2,M), W_s(IJK1,M), I_OF(IJK2))
          VELS = WSCM

        ELSE
          WRITE(LINE,'(A, A)') 'Error: Unknown FCELL'
          CALL WRITE_ERROR('CALC_GRBDRY_QOI', LINE, 1)
          CALL exitMPI(myPE)
        ENDIF


      ELSE
         WRITE(LINE,'(A, A)') 'Error: Unknown COM'
         CALL WRITE_ERROR('CALC_GRBDRY_QOI', LINE, 1)
         CALL exitMPI(myPE)
      ENDIF

! magnitude of gas-solids relative velocity
      VREL =&
         DSQRT( (UGC - USCM)**2 + (VGC - VSCM)**2 + (WGC - WSCM)**2 )

! slip velocity for use in Jenkins bc (sof)
      VSLIP= DSQRT( (USCM-BC_UW_S(L,M))**2 + (VSCM-BC_VW_S(L,M))**2 &
         + (WSCM-BC_WW_S(L,M))**2 )


      RETURN
      END SUBROUTINE CALC_BDRY_QOI


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: CALC_MU_SV_BDRY                                         C
!  Purpose: calculate mu_s and p_s resulting from KT only (does NOT    C
!  calculate any contributions from the quasi-static/friction regime)  C
!  at the given boundary location for evaluation of boundary           C
!  condition                                                           C
!                                                                      C
!  Addition notes:                                                     C
!     See calc_mu_s for details and references                         C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CALC_MU_SV_BDRY(M, g0, EPs, EPg, ep_star, &
            SUM_EPSxG0, DP, TH, ROs, aK_12, aTau_12, aTau_1, &
            MUg, ROg, VREL, mu_sv, p_sv)

! Global variables
!---------------------------------------------------------------------
      USE constant, only: pi, alpha, c_e, eta
      USE constant, only: switch, switch_ia
      USE error_manager, only: err_msg
      USE kintheory, only: epm, k_phi, s_star
      USE param, only: dimension_m
      USE param1, only: half, one, large_number, small_number, zero
      USE physprop, only: smax

! kinetic theories
      USE run, only: kt_type_enum
      USE run, only: kt_type
      USE run, only: lun_1984
      USE run, only: simonin_1996
      USE run, only: ahmadi_1995
      USE run, only: gd_1999
      USE run, only: gtsh_2012
      USE run, only: ia_2005
      IMPLICIT NONE

! Dummy Arguments
!---------------------------------------------------------------------
! Solids phase index
      INTEGER, INTENT(IN) :: M
! Radial distribution function of solids phase M with each
! other solids phase
      DOUBLE PRECISION, INTENT(IN) :: g0(DIMENSION_M)
! Average solids volume fraction of each solids phase
      DOUBLE PRECISION, INTENT(IN) :: EPS(DIMENSION_M)
! Average and gas volume fraction, and avg gas volume fraction at max. packing
      DOUBLE PRECISION, INTENT(IN) :: EPG, ep_star
! Sum of avg eps* avg G_0
      DOUBLE PRECISION, INTENT(INOUT) :: SUM_EpsxG0
! Average particle diameter of each solids phase
      DOUBLE PRECISION, INTENT(IN) :: Dp(DIMENSION_M)
! Average theta_m
      DOUBLE PRECISION, INTENT(INOUT) :: TH(DIMENSION_M)
! Average solids density
      DOUBLE PRECISION, INTENT(IN) :: ROs(DIMENSION_M)
! Average cross-correlation K_12 and lagrangian integral time-scale
      DOUBLE PRECISION, INTENT(IN) :: aK_12, aTau_12, aTau_1
! Average gas viscosity
      DOUBLE PRECISION, INTENT(IN) :: MUg
! Average gas density
      DOUBLE PRECISION, INTENT(IN) :: ROg
! Magnitude of slip velocity between two phases
      DOUBLE PRECISION, INTENT(IN) :: VREL
! Viscosity from kinetic theory
      DOUBLE PRECISION, INTENT(OUT) :: Mu_sv
! Pressure from kinetic theory
      DOUBLE PRECISION, INTENT(OUT) :: P_sv

! Local Variables
!---------------------------------------------------------------------
! Solids phase index
      INTEGER :: LL
! Viscosity
      DOUBLE PRECISION :: Mu
! Bulk viscosity
      DOUBLE PRECISION :: Mu_b
! Viscosity corrected for interstitial fluid effects
      DOUBLE PRECISION :: Mu_star
! Reynolds number based on slip velocity
      DOUBLE PRECISION :: Re_g
! Friction Factor in drag coefficient
      DOUBLE PRECISION :: C_d
! Drag Coefficient
      DOUBLE PRECISION :: DgA
! Constants in Simonin or Ahmadi model
      DOUBLE PRECISION :: Sigma_c, Tau_2_c, Tau_12_st, Nu_t
      DOUBLE PRECISION :: Tau_2, zeta_c_2, MU_2_T_Kin, Mu_2_Col
      DOUBLE PRECISION :: Tmp_Ahmadi_Const
! Variables for Iddir & Arastoopour model
      DOUBLE PRECISION :: MU_sM_sum, MU_s_MM, MU_s_LM, MU_sM_ip, MU_common_term,&
                          MU_sM_LM
      DOUBLE PRECISION :: M_PM, M_PL, MPSUM, NU_PL, NU_PM, D_PM, D_PL, DPSUMo2
      DOUBLE PRECISION :: Ap_lm, Dp_lm, R1p_lm, Bp_lm
! Variables for Garzo and Dufty model
      DOUBLE PRECISION :: c_star, zeta0_star, nu_eta_star, press_star, &
                          gamma_star, eta_k_star, eta_star, eta0
! Variables for GTSH theory
      DOUBLE PRECISION :: Chi, RdissP, Re_T, Rdiss, GamaAvg, A2_gtshW, &
                          zeta_star, nu0, nuN, NuK, EDT_s, zeta_avg, etaK, &
                          Mu_b_avg, mu2_0, mu4_0, mu4_1
!---------------------------------------------------------------------

! initialize
      MU_sv = zero
      p_sv = zero

! This is done here similar to bc_theta to avoid small negative values of
! Theta coming most probably from linear solver
      IF(TH(M) .LE. ZERO)THEN
         TH(M) = 1D-8
         WRITE (ERR_MSG,*) 'CALC_GRBDRY => CALC_MU_SV_BDRY '//&
            'Warning: Negative granular temp at wall set to 1e-8'
         CALL log_error()
      ENDIF

! common variables
      D_PM = Dp(M)
      M_PM = (PI/6.d0)*(D_PM**3)*ROs(M)
      NU_PM = (EPS(M)*ROs(M))/M_PM

! This is from Wen-Yu correlation, you can put here your own single particle drag
      Re_g = EPG*ROg*D_PM*VREL/MUg
      IF (Re_g .lt. 1000.d0) THEN
         C_d = (24.d0/(Re_g+SMALL_NUMBER))*(ONE + 0.15d0 * Re_g**0.687d0)
      ELSE
         C_d = 0.44d0
      ENDIF
      DgA = 0.75d0*C_d*ROg*EPG*VREL/(D_PM*EPG**(2.65d0))
      IF(VREL == ZERO) DgA = LARGE_NUMBER
!      Beta = EPS(M)*DgA !this is equivalent to F_gs(ijk,m)


! Defining viscosity according to each KT for later use in JJ BC
! Also defining solids pressure according to each KT for use if JENKINS
! -------------------------------------------------------------------
      SELECT CASE (KT_TYPE_ENUM)
      CASE (LUN_1984)
         Mu = (5d0/96d0)*Dp(M)*ROs(M)*DSQRT(Pi*TH(M))
         Mu_b = (256d0*Mu*EPS(M)*SUM_EpsxG0)/(5d0*Pi)

         IF(SWITCH == ZERO .OR. ROg == ZERO)THEN
            Mu_star = Mu
! this seems to be the proper extension for M>1 when ro_g=0
!            Mu_star = Mu*g0(M)*EPS(M)/SUM_EpsxG0
         ELSEIF(TH(M) .LT. SMALL_NUMBER)THEN
            MU_star = ZERO
         ELSE
            Mu_star = ROs(M)*EPS(M)* g0(M)*TH(M)* Mu/ &
               (ROs(M)*SUM_EpsxG0*TH(M) + 2.0d0*DgA*Mu/ROs(M))
         ENDIF

         MU_sv = ((2d0+ALPHA)/3d0)*((Mu_star/(Eta*(2d0-Eta)*&
            g0(M)))*(ONE+1.6d0*Eta*SUM_EpsxG0)*&
            (ONE+1.6d0*Eta*(3d0*Eta-2d0)*&
            SUM_EpsxG0)+(0.6d0*Mu_b*Eta))

! defining granular pressure (for Jenkins BC)
         P_sv = ROs(M)*EPS(M)*TH(M)*(ONE+4.D0*Eta*SUM_EpsxG0)


      CASE (SIMONIN_1996)
! particle relaxation time
         Tau_12_st = ROs(M)/(DgA+small_number)

         Sigma_c = (ONE+ C_e)*(3.d0-C_e)/5.d0
         Tau_2_c = Dp(M)/(6.d0*EPS(M)*g0(M)*DSQRT(16.d0*(TH(M)+Small_number)/PI))
         zeta_c_2= 2.D0/5.D0*(ONE+ C_e)*(3.d0*C_e-ONE)
         Nu_t =  aTau_12/Tau_12_st
         Tau_2 = ONE/(2.D0/Tau_12_st+Sigma_c/Tau_2_c)
         MU_2_T_Kin = (2.0D0/3.0D0*aK_12*Nu_t + TH(M) * &
              (ONE+ zeta_c_2*EPS(M)*g0(M)))*Tau_2
         Mu_2_Col = 8.D0/5.D0*EPS(M)*g0(M)*Eta* (MU_2_T_Kin+ &
               Dp(M)*DSQRT(TH(M)/PI))
         MU_sv = EPS(M)*ROs(M)*(MU_2_T_Kin + Mu_2_Col)

! defining granular pressure (for Jenkins BC)
         P_sv = ROs(M)*EPS(M)*TH(M)*(ONE+4.D0*Eta*SUM_EpsxG0)


      CASE (AHMADI_1995)
! particle relaxation time
         Tau_12_st = ROs(M)/(DgA+small_number)

         IF(EPS(M) < (ONE-ep_star)) THEN
            Tmp_Ahmadi_Const = ONE/&
               (ONE+ aTau_1/Tau_12_st * (ONE-EPS(M)/(ONE-ep_star))**3)
         ELSE
            Tmp_Ahmadi_Const = ONE
         ENDIF
         MU_sv = Tmp_Ahmadi_Const &
            *0.1045D0*(ONE/g0(M)+3.2D0*EPS(M)+12.1824D0*g0(M)*EPS(M)*EPS(M))  &
            *Dp(M)*ROs(M)* DSQRT(TH(M))

! defining granular pressure (for Jenkins BC)
         P_sv = ROs(M)*EPS(M)*TH(M)*&
             ((ONE + 4.0D0*SUM_EpsxG0)+HALF*(ONE -C_e*C_e))


      CASE(IA_2005)
! Use original IA theory if SWITCH_IA is false
         IF(.NOT. SWITCH_IA) SUM_EpsxG0 = EPS(M)*g0(M)

         Mu = (5.d0/96.d0)*D_PM* ROs(M)*DSQRT(PI*TH(M)/M_PM)

         IF(SWITCH == ZERO .OR. ROg == ZERO)THEN
            Mu_star = Mu
! this seems to be the proper extension for M>1 when ro_g=0
            Mu_star = Mu*g0(M)*EPS(M)/SUM_EpsxG0
         ELSEIF(TH(M) .LT. SMALL_NUMBER)THEN
            MU_star = ZERO
         ELSE
            Mu_star = Mu*EPS(M)*g0(M)/ (SUM_EpsxG0+ 2.0d0*DgA*Mu / &
               (ROs(M)**2 *(TH(M)/M_PM)))
         ENDIF

         MU_s_MM = (Mu_star/g0(M))*(1.d0+(4.d0/5.d0)*(1.d0+C_E)*SUM_EpsxG0)**2
         Mu_sM_sum = ZERO

         DO LL = 1, SMAX
            D_PL = Dp(LL)
            M_PL = (PI/6.d0)*(D_PL**3.)*ROs(LL)
            MPSUM = M_PM + M_PL
            DPSUMo2 = (D_PM+D_PL)/2.d0
            NU_PL = (EPS(LL)*ROs(LL))/M_PL

            IF ( LL .eq. M) THEN
               Ap_lm = MPSUM/(2.d0)
               Dp_lm = M_PL*M_PM/(2.d0*MPSUM)
               R1p_lm = ONE/( Ap_lm**1.5 * Dp_lm**3 )
               MU_s_LM = DSQRT(PI)*(DPSUMo2**4 / (48.d0*5.d0))*&
                  g0(LL)*(M_PL*M_PM/MPSUM)*(M_PL*M_PM/&
                  MPSUM)*((M_PL*M_PM)**1.5)*NU_PM*NU_PL*&
                  (1.d0+C_E)*R1p_lm*DSQRT(TH(M))

! solids phase 'viscosity' associated with the divergence
! of solids phase M
               MU_sM_ip = (MU_s_MM + MU_s_LM)

            ELSE
               Ap_lm = (M_PM*TH(LL)+M_PL*TH(M))/&
                  (2.d0)
               Bp_lm = (M_PM*M_PL*(TH(LL)-TH(M) ))/&
                  (2.d0*MPSUM)
               Dp_lm = (M_PL*M_PM*(M_PM*TH(M)+M_PL*TH(LL) ))/&
                  (2.d0*MPSUM*MPSUM)
               R1p_lm = ( ONE/( Ap_lm**1.5 * Dp_lm**3 ) ) + &
                  ( (9.d0*Bp_lm*Bp_lm)/( Ap_lm**2.5 * Dp_lm**4 ) )+&
                  ( (30.d0*Bp_lm**4)/( 2.d0*Ap_lm**3.5 * Dp_lm**5 ) )
               MU_common_term = DSQRT(PI)*(DPSUMo2**4 / (48.d0*5.d0))*&
                  g0(LL)*(M_PL*M_PM/MPSUM)*(M_PL*M_PM/&
                  MPSUM)*((M_PL*M_PM)**1.5)*NU_PM*NU_PL*&
                  (1.d0+C_E)*R1p_lm
               MU_sM_LM = MU_common_term*(TH(M)*TH(M)*TH(LL)*TH(LL)*TH(LL) )

! solids phase 'viscosity' associated with the divergence
! of solids phase M
               MU_sM_ip = MU_sM_LM

            ENDIF
            MU_sM_sum = MU_sM_sum + MU_sM_ip

          ENDDO

! Find the term proportional to the gradient in velocity
! of phase M  (viscosity in the Mth solids phase)
          MU_sv = MU_sM_sum


      CASE(GD_1999)
! Pressure/Viscosity/Bulk Viscosity
! Note: k_boltz = M_PM

! Find pressure in the Mth solids phase
         press_star = 1.d0 + 2.d0*(1.d0+C_E)*EPS(M)*G0(M)

! find bulk and shear viscosity
         c_star = 32.0d0*(1.0d0 - C_E)*(1.d0 - 2.0d0*C_E*C_E) &
            / (81.d0 - 17.d0*C_E + 30.d0*C_E*C_E*(1.0d0-C_E))

         zeta0_star = (5.d0/12.d0)*G0(M)*(1.d0 - C_E*C_E) &
            * (1.d0 + (3.d0/32.d0)*c_star)

         nu_eta_star = G0(M)*(1.d0 - 0.25d0*(1.d0-C_E)*(1.d0-C_E)) &
            * (1.d0-(c_star/64.d0))

         gamma_star = (4.d0/5.d0)*(32.d0/PI)*EPS(M)*EPS(M) &
            * G0(M)*(1.d0+C_E) * (1.d0 - (c_star/32.d0))

         eta_k_star = (1.d0 - (2.d0/5.d0)*(1.d0+C_E)*(1.d0-3.d0*C_E) &
            * EPS(M)*G0(M) ) / (nu_eta_star - 0.5d0*zeta0_star)

         eta_star = eta_k_star*(1.d0 + (4.d0/5.d0)*EPS(M)*G0(M) &
            * (1.d0+C_E) ) + (3.d0/5.d0)*gamma_star

         eta0 = 5.0d0*M_PM*DSQRT(TH(M)/PI) / (16.d0*D_PM*D_PM)

! added Ro_g = 0 for granular flows (no gas).
         IF(SWITCH == ZERO .OR. ROg == ZERO) THEN
            Mu_star = eta0
         ELSEIF(TH(M) .LT. SMALL_NUMBER)THEN
            Mu_star = ZERO
         ELSE
            Mu_star = ROs(M)*EPS(M)*G0(M)*TH(M)*eta0 / &
               ( ROs(M)*EPS(M)*G0(M)*TH(M) + &
               (2.d0*DgA*eta0/ROs(M)) )     ! Note dgA is ~F_gs/ep_s
         ENDIF

!  shear viscosity in Mth solids phase  (add to frictional part)
         MU_sv = Mu_star * eta_star

! defining granular pressure (for Jenkins BC)
         P_sv = ROs(M)*EPS(M)*TH(M)*(ONE+2.d0*(ONE+C_E)*SUM_EpsxG0)
                    ! ~ROs(m)*EPS(M)*TH(M)*press_star


      CASE (GTSH_2012)
         Chi = g0(M)

         RdissP = one
         IF(EPS(M) > small_number) RdissP = &
            one + 3d0*dsqrt(EPS(M)/2d0) + 135d0/64d0*EPS(M)*dlog(EPS(M)) + &
            11.26d0*EPS(M)*(one-5.1d0*EPS(M)+16.57d0*EPS(M)**2-21.77d0*    &
            EPS(M)**3) - EPS(M)*Chi*dlog(epM)

         Re_T = ROg*D_pm*dsqrt(TH(M)) / MUg
         Re_g = EPG*ROg*D_PM*VREL/MUg
         Rdiss = RdissP + Re_T * K_phi(EPS(M))
         GamaAvg = 3d0*Pi*MUg*D_pm*Rdiss
         mu2_0 = dsqrt(2d0*pi) * Chi * (one-C_E**2)
         mu4_0 = (4.5d0+C_E**2) * mu2_0
         mu4_1 = (6.46875d0+0.9375d0*C_E**2)*mu2_0 + 2d0*dsqrt(2d0*pi)* &
            Chi*(one+C_E)
         A2_gtshW = zero ! for eps = zero

         if((EPS(M)> small_number) .and. (TH(M) > small_number)) then
            zeta_star = 4.5d0*dsqrt(2d0*Pi)*(ROg/ROs(M))**2*Re_g**2 * &
                        S_star(EPS(M),Chi) / (EPS(M)*(one-EPS(M))**2 * Re_T**4)
            A2_gtshW = (5d0*mu2_0 - mu4_0) / (mu4_1 - 5d0* &
                           (19d0/16d0*mu2_0 - 1.5d0*zeta_star))
         endif

         eta0 = 0.3125d0/(dsqrt(pi)*D_PM**2)*M_pm*dsqrt(TH(M))
         nu0 = (96.d0/5.d0)*(EPS(M)/D_PM)*DSQRT(TH(M)/PI)
         nuN = 0.25d0*nu0*Chi*(3d0-C_E)*(one+C_E) * &
            (one+0.7375d0*A2_gtshW)
         NuK = nu0*(one+C_E)/3d0*Chi*( one+2.0625d0*(one-C_E)+ &
            ((947d0-579*C_E)/256d0*A2_gtshW) )
         EDT_s = 4d0/3d0*dsqrt(pi)*(one-C_E**2)*Chi* &
            (one+0.1875d0*A2_gtshW)*NU_PM*D_PM**2*dsqrt(TH(M))

         if((TH(m) > small_number) .and. (EPS(M) > small_number)) then
            zeta_avg = one/6d0*D_PM*VREL**2*(3d0*pi*MUg*D_PM/&
               M_pm)**2 / dsqrt(TH(m)) * S_star(EPS(M), Chi)
            etaK = ROs(M)*EPS(M)*TH(m) / (nuN-0.5d0*( &
               EDT_s-zeta_avg/TH(m) - 2d0*GamaAvg/M_PM)) * &
               (one -0.4d0 * (one+C_E)*(one-3d0*C_E)*EPS(M)*Chi)
         else
            etaK = zero
         endif

         Mu_b_avg = 25.6d0/pi * EPS(M)**2 * Chi *(one+C_E) * &
            (one - A2_gtshW/16d0)*eta0

         MU_sv = etaK*(one+0.8d0*EPS(M)*Chi*(one+C_E)) + &
            0.6d0*Mu_b_avg

! defining granular pressure (for Jenkins BC)
         P_sv = ROs(M)*EPS(M)*TH(M)*(ONE+2.d0*(ONE+C_E)*SUM_EpsxG0)

      CASE DEFAULT
! should never hit this
         WRITE (ERR_MSG, *) 'CALC_GRBDRY => CALC_MU_SV_BDRY '//&
            'Unknown KT_TYPE: ', KT_TYPE
         call log_error()
      END SELECT

      RETURN
      END SUBROUTINE CALC_MU_SV_BDRY


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: calc_grbdry_collisional                                 C
!  Purpose: calculate the collisional contribution to the boundary     C
!  condition as based on selected KT                                   C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE CALC_GRBDRY_COLLISIONAL(M, g0, EPS, ep_star, &
                    DP, TH, ROs, VSLIP, P_sv, COLL_COMP)

! Global variables
!---------------------------------------------------------------------
      USE bc_theta_mod, only: phip_jj
      USE constant, only: tan_phi_w, phip, pi
      USE error_manager, only: err_msg
      USE param, only: dimension_m
      USE param1, only: zero, one
      USE run, only: bc_jj_m, jenkins
! kinetic theories
      USE run, only: kt_type_enum
      USE run, only: kt_type
      USE run, only: lun_1984
      USE run, only: simonin_1996
      USE run, only: ahmadi_1995
      USE run, only: gd_1999
      USE run, only: gtsh_2012
      USE run, only: ia_2005
      IMPLICIT NONE

! Dummy Arguments
!---------------------------------------------------------------------
! Solids phase index
      INTEGER, INTENT(IN) :: M
! Radial distribution function of solids phase M with each
! other solids phase
      DOUBLE PRECISION, INTENT(IN) :: g0(DIMENSION_M)
! Average solids volume fraction of each solids phase
      DOUBLE PRECISION, INTENT(IN) :: EPS(DIMENSION_M)
! Average solids and gas volume fraction
      DOUBLE PRECISION, INTENT(IN) :: ep_star
! Average particle diameter
      DOUBLE PRECISION, INTENT(IN) :: DP(DIMENSION_M)
! Average theta_m
      DOUBLE PRECISION, INTENT(IN) :: TH(DIMENSION_M)
! Average solids density
      DOUBLE PRECISION, INTENT(IN) :: ROs(DIMENSION_M)
! Slip velocity between wall and particles
      DOUBLE PRECISION, INTENT(IN) :: VSLIP
! Solids phase pressure from KT
      DOUBLE PRECISION, INTENT(IN) :: P_sv
! the collisional contribution in the granular boundary condition model
      DOUBLE PRECISION, INTENT(OUT) :: COLL_COMP

! Local Variables
!---------------------------------------------------------------------
      DOUBLE PRECISION :: D_PM, M_PM
!---------------------------------------------------------------------

! common variables
      D_PM = Dp(M)
      M_PM = (PI/6.d0)*(D_PM**3)*ROs(M)

      SELECT CASE (KT_TYPE_ENUM)

         CASE (LUN_1984, SIMONIN_1996, AHMADI_1995, GD_1999, GTSH_2012)
! KT without particle mass in their definition of granular temperature
! and theta = granular temperature  OR
! KT with particle mass in their definition of granular temperature
! and theta = granular temperature/mass

! Jenkins BC is implemented for these KT
            IF(JENKINS) THEN
               IF (VSLIP == ZERO) THEN
! if solids velocity field is initialized to zero, use free slip bc
                  COLL_COMP = zero
               ELSE
! As I understand from soil mechanic papers, the coefficient mu in
! Jenkins paper is tan_Phi_w. T.  See for example, G.I. Tardos, PT,
! 92 (1997), 61-74, equation (1). sof

! here COLL_COMP divided by VSLIP to use the same bc as Johnson&Jackson
                  COLL_COMP = tan_Phi_w*P_sv/VSLIP

               ENDIF
            ELSE
               IF(.NOT. BC_JJ_M) THEN
                  COLL_COMP = (PHIP*DSQRT(3d0*TH(M))*Pi*ROs(M)*EPS(M)*&
                     g0(M))/(6d0*(ONE-ep_star))
               ELSE
                  COLL_COMP = (PHIP_JJ(vslip,th(m))*DSQRT(3d0*TH(M))*Pi*&
                     ROs(M)*EPS(M)*g0(M))/(6d0*(ONE-ep_star))
               ENDIF
            ENDIF   ! end if(Jenkins)/else


        CASE (IA_2005)
! KTs with particle mass in their definition of granular temperature
! and theta = granular temperature
            IF(.NOT. BC_JJ_M) THEN
               COLL_COMP = (PHIP*DSQRT(3.d0*TH(M)/M_PM)*PI*ROs(M)*EPS(M)*&
                  g0(M))/(6.d0*(ONE-ep_star))
            ELSE
               COLL_COMP = (PHIP_JJ(vslip,th(m))*DSQRT(3.d0*TH(M)/M_PM)*PI*&
                  ROs(M)*EPS(M)*g0(M))/(6.d0*(ONE-ep_star))
            ENDIF


         CASE DEFAULT
! should never hit this
            WRITE (ERR_MSG,*) 'CALC_GRBDRY => CALC_GRBDRY_COLLISIONAL '//&
               'Unknown KT_TYPE: ', KT_TYPE
            call log_error()
      END SELECT


      RETURN
      END SUBROUTINE CALC_GRBDRY_COLLISIONAL



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: CALC_MU_SF_BDRY                                         C
!  Purpose: calculate mu_s and p_s resulting from quasi-static/        C
!  friction regime at the given boundary location for evaluation       C
!  of boundary condition                                               C
!                                                                      C
!  Addition notes:                                                     C
!     See calc_mu_s for details and references                         C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE CALC_MU_SF_BDRY(M, G0, EPs, EPg, ep_star, &
                    SUM_EPsxG0, DP, TH, ROs, MUg, ROg, VREL, &
                    trcD, SddS, mu_sf, p_sf, zeta)

! Global variables
!---------------------------------------------------------------------
      USE constant, only: pi, sqrt_pi, alpha, switch, eta
      USE constant, only: eps_f_min, sin_phi
      USE constant, only: Fr, N_Pc, D_Pc, N_pf, delta
      USE constant, only: to_si

      USE param1, only: zero, one, small_number
      USE param, only: dimension_m
      USE physprop, only: smax, close_packed
      USE run, only: savage, princeton, guo_boyce
      IMPLICIT NONE

! Dummy Arguments
!---------------------------------------------------------------------
! Solids phase index
      INTEGER, INTENT(IN) :: M

! Radial distribution function of solids phase M with each
! other solids phase
      DOUBLE PRECISION, INTENT(IN) :: g0(DIMENSION_M)
! Average solids volume fraction of each solids phase
      DOUBLE PRECISION, INTENT(IN) :: EPs(DIMENSION_M)
! Average solids and gas volume fraction
      DOUBLE PRECISION, INTENT(IN) :: EPg, ep_star
! Sum of avg eps* avg G0
      DOUBLE PRECISION, INTENT(IN) :: sum_epsxg0
! Average particle diameter of each solids phase
      DOUBLE PRECISION, INTENT(IN) :: DP(DIMENSION_M)
! Average theta_m, already adjusted if small neg. values
      DOUBLE PRECISION, INTENT(IN) :: TH(DIMENSION_M)
! Average solids density
      DOUBLE PRECISION, INTENT(IN) :: ROs(DIMENSION_M)
! Average gas viscosity
      DOUBLE PRECISION, INTENT(IN) :: Mug
! Average gas density
      DOUBLE PRECISION, INTENT(IN) :: ROg
! Magnitude of slip velocity between two phases
      DOUBLE PRECISION, INTENT(IN) :: VREL
! trace(D)=trace(grad(u))=del.u
      DOUBLE PRECISION, INTENT(IN) :: trcD
! S:S
      DOUBLE PRECISION, INTENT(IN) :: SddS
! frictional viscosity and pressure at location
      DOUBLE PRECISION, INTENT(OUT) :: mu_sf, p_sf
! this is approximately the sqrt(S:S)
      DOUBLE PRECISION, INTENT(OUT) :: zeta

! Local Variables
!---------------------------------------------------------------------
! used to compute frictional terms
      DOUBLE PRECISION :: Pc, PfoPc, N_Pff, PfoPc_max
! sum of all solids volume fractions
      DOUBLE PRECISION :: SUM_EPS_CP
! parameters in pressure linearization
      DOUBLE PRECISION :: dpcdep_at, Pc_at
! calculation quantities
      DOUBLE PRECISION :: MU_sv, MU_bv, Mu_star, gama

! Reynolds number based on slip velocity
      DOUBLE PRECISION :: Re_g
! Friction Factor in drag coefficient
      DOUBLE PRECISION :: C_d
! Drag Coefficient
      DOUBLE PRECISION :: DgA
! index
      INTEGER :: MM

! Guo-Boyce model
      DOUBLE PRECISION :: n1_t
      DOUBLE PRECISION, PARAMETER          :: a_ps = 1D25
      INTEGER, PARAMETER                   :: b_ps = 10

!---------------------------------------------------------------------

      IF (SAVAGE.EQ.1) THEN
         Mu_sv = (5.d0*DSQRT(Pi*TH(M))*Dp(M)*&
            ROs(M))/96.d0
         Mu_bv = (256.d0*Mu_sv*EPs(M)*SUM_EPSxG0)&
            /(5.d0*Pi)
         IF(SWITCH == ZERO .OR. ROg == ZERO) THEN
            Mu_star = Mu_sv
! this seems to be the proper extension if ro_g0=0 for M>1
            Mu_star = EPs(M)*G0(M)*Mu_sv/ SUM_EPSxG0
         ELSEIF(TH(M) .LT. SMALL_NUMBER)THEN
            Mu_star = ZERO
         ELSE
! This is from Wen-Yu correlation, you can put here your own single particle drag
            Re_g = EPG*ROg*DP(M)*VREL/MUg
            IF (Re_g .lt. 1000.d0) THEN
               C_d = (24.d0/(Re_g+SMALL_NUMBER))*(ONE + 0.15d0 * Re_g**0.687d0)
            ELSE
               C_d = 0.44d0
            ENDIF
            DgA = 0.75d0*C_d*ROg*EPG*VREL/(DP(M)*EPG**(2.65d0))
!           Beta = EPS(M)*DgA !this is equivalent to F_gs(ijk,m)

            Mu_star = ROs(M)*EPs(M)* G0(M)*&
               TH(M)*Mu_sv/ (ROs(M)*SUM_EPSxG0*TH(M)+ &
               (2.d0*DgA*Mu_sv/ROs(M)) )
         ENDIF
! Shear viscosity
         Mu_sv =((2.d0+ALPHA)/3.d0)*((Mu_star/(Eta*(2.d0-Eta)*&
            G0(M)))*(ONE+1.6d0*Eta*SUM_EPSxG0)&
            *(ONE+1.6d0*Eta*(3d0*Eta-2d0)*&
            SUM_EPSxG0)+(0.6d0*Mu_bv*Eta))
! dissipation due to collisions
         gama = (48d0*Eta*(1d0-Eta)*ROs(M)*EPs(M)*&
            SUM_EPSxG0*TH(M)**1.5d0)/(SQRT_Pi*Dp(M))
! in ssf S:grad(vel) ~ S:S = 1/2(du/dy)^2 = gama/(2*mu)
         zeta = (gama / (2.d0*mu_sv))**0.5

      ELSEIF (SAVAGE.EQ.0) THEN
! standard S:S form
         zeta = dsqrt(SddS)
      ELSE
! combined form: srivastava & sundaresan (2003)
         zeta = dsqrt((TH(M)/Dp(M)**2) + Sdds)
      ENDIF

      IF ((ONE-EPG) .GT. ((ONE-ep_star)-delta)) THEN
! Linearized form of Pc; this is more stable and provides continuous function.
! Linearize PC around small deviation from max. packing: eg=ep_star+delta
         IF(PRINCETON) THEN
            dpcdep_at = -(to_SI*Fr)*&
               ( ( N_Pc*((ONE-ep_star-delta-eps_f_min)**(N_Pc-ONE))/(delta**D_Pc)) + &
                 ( D_Pc*((ONE-ep_star-delta-eps_f_min)**N_pc)/(delta**(D_pc+ONE)) ) )
            Pc_at = (to_SI*Fr)*&
              ((ONE-ep_star-delta-EPS_f_min)**N_Pc) / (delta**D_Pc)
            Pc = Pc_at + dPcdep_at*(EPg-ep_star-delta)
         ELSEIF(GUO_BOYCE) THEN
            Pc = 2d0*SddS*((ONE-ep_star-EPS_f_min)**2)*(Dp(M)**2)*ros(M) / &
                 ( delta**2) + to_SI*a_ps * ((ONE-EPG)-(ONE-ep_star)+delta)**b_ps
         ENDIF

      ELSE
         IF(PRINCETON) THEN
            Pc = (to_SI*Fr)*((ONE-EPg-EPS_f_min)**N_Pc) / &
               ( ((ONE-ep_star)-(ONE-EPg) )**D_Pc)
         ELSEIF(GUO_BOYCE) THEN
            Pc = 2d0*SddS*((ONE-ep_star-EPS_f_min)**2)*(Dp(M)**2)*ros(M) / &
                 ( ((ONE-ep_star)-(ONE-EPG) )**2)*n1_t
         ENDIF
      ENDIF

      IF (trcD .GE. ZERO) THEN
         N_Pff = DSQRT(3d0)/(2d0*Sin_Phi) !dilatation
      ELSE
         N_Pff = N_Pf !compaction
      ENDIF

      IF ((trcD/(zeta*N_Pff*DSQRT(2d0)*Sin_Phi)) .GT. 1d0) THEN
! to avoid negative values in P_f
         PfoPc = ZERO
         P_sf = ZERO
      ELSEIF(trcD == ZERO) THEN
! simply to avoid calculation in else branch below
         PfoPc = ONE
         P_sf = Pc
      ELSE
         PfoPc = (1d0 - (trcD/&
               (zeta*N_Pff*DSQRT(2d0)*Sin_Phi)))**(N_Pff-1d0)
         P_sf = Pc*PfoPc
      ENDIF

      mu_sf = DSQRT(2d0)*P_sf*Sin_Phi*(N_Pff - (N_Pff-1d0)*&
         (PfoPc)**(1d0/(N_Pff-1d0)))

      PfoPc_max = ((N_Pff/(N_Pff-1d0))**(N_Pff-1d0))
      IF (PfoPc > PfoPc_max) THEN
         PfoPc = PfoPc_max
         P_sf = Pc*PfoPc
         mu_sf = zero
      ENDIF

! recall mfix stress expression uses stress=2*mu*S.
      Mu_sf = mu_sf/(2d0*zeta)
! sum_eps_cp calculation for polydispersity
      SUM_EPS_CP = ZERO
      DO MM=1,SMAX
         IF (CLOSE_PACKED(MM)) SUM_EPS_CP = SUM_EPS_CP + EPS(MM)
      ENDDO

! Modification of the stresses when more than one solids phase is used:
      P_sf = P_sf * (EPS(M)/SUM_EPS_CP)
      Mu_sf = Mu_sf * (EPS(M)/SUM_EPS_CP)

      RETURN
      END SUBROUTINE CALC_MU_SF_BDRY


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: CALC_GRBDRY_FRICTIONAL                                  C
!  Purpose: calculate the frictional contribution to the boundary      C
!  condition as based on given frictional theory                       C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE CALC_GRBDRY_FRICTIONAL(mu_sf, p_sf, s_dd, FRIC_COMP)

! Global variables
!---------------------------------------------------------------------
      USE constant, only: tan_phi_w
      IMPLICIT NONE

! Dummy Arguments
!---------------------------------------------------------------------
! frictional viscosity and pressure
      DOUBLE PRECISION, INTENT(IN) :: mu_sf, p_sf
! s:S in the relevant direction
      DOUBLE PRECISION, INTENT(IN) :: s_dd
! the frictional contribution in the granular boundary condition model
      DOUBLE PRECISION, INTENT(OUT) :: FRIC_COMP
!---------------------------------------------------------------------

      FRIC_COMP = tan_phi_w*(P_sf - 2.d0*mu_sf*S_dd)

      RETURN
      END SUBROUTINE CALC_GRBDRY_FRICTIONAL


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine CG_CALC_GRBDRY                                           C
!  Purpose: Cut cell version                                           C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE CG_CALC_GRBDRY(IJK, TYPE_OF_CELL, M, L, F_2)

! Global variables
!---------------------------------------------------------------------
      USE bc, only: bc_uw_s, bc_vw_s, bc_ww_s
      USE constant, only: to_si
      USE error_manager, only: err_msg

      USE fldvar, only: u_s, v_s, w_s
      USE fldvar, only: u_g, v_g, w_g
      USE fldvar, only: ep_g, ep_s, ro_g
      USE fldvar, only: ro_s, d_p, theta_m

      USE fun_avg, only: avg_x, avg_x_e
      USE fun_avg, only: avg_y, avg_y_n
      USE fun_avg, only: avg_z, avg_z_t
      USE functions, only: im_of, jm_of, km_of
      USE functions, only: ip_of, jp_of, kp_of
      USE functions, only: east_of, north_of, top_of

      USE geometry, only: vol

      USE indices, only: i_of, j_of, k_of
      USE param1, only: zero
      USE param, only: dimension_m

      USE physprop, only: mu_g, smax
      USE rdf, only: g_0avg

! kinetic theories
      USE run, only: kt_type_enum
      USE run, only: simonin_1996
      USE run, only: ahmadi_1995

      USE toleranc, only: zero_ep_s
      USE turb, only: k_12, tau_1, tau_12
      USE visc_s, only: ep_star_array
      IMPLICIT NONE

! Dummy Arguments
!---------------------------------------------------------------------
! IJK indices
      INTEGER, INTENT(IN) :: IJK

      CHARACTER (LEN=*) :: TYPE_OF_CELL
! Solids phase index
      INTEGER, INTENT(IN) :: M
! Index corresponding to boundary condition
      INTEGER, INTENT(IN) ::  L

      DOUBLE PRECISION :: F_2

! Local Variables
!---------------------------------------------------------------------
! IJK indices
      INTEGER          IJK1, IJK2
! Other indices
      INTEGER          IJK2E, IPJK2, IPJMK2
!
      DOUBLE PRECISION :: smallTheta
! Average scalars for gas phase
      DOUBLE PRECISION EPg_avg, Mu_g_avg, RO_g_avg
! Average void fraction at packing
      DOUBLE PRECISION ep_star_avg
! Average scalars modified to include all solid phases
      DOUBLE PRECISION EPs_avg(DIMENSION_M)
! Average particle diameter
       DOUBLE PRECISION DP_avg(DIMENSION_M)
! Average granular temperature
       DOUBLE PRECISION TH_avg(DIMENSION_M)
! Average solids density
      DOUBLE PRECISION ROS_AVG(DIMENSION_M)
! Average Simonin and Ahmadi variables (sof)
      DOUBLE PRECISION K_12_avg, Tau_12_avg, Tau_1_avg
! Average velocities
      DOUBLE PRECISION WGC1, WGC2
! Solids phase index
      INTEGER          MM
! values of U_sm, V_sm, W_sm at appropriate place on boundary wall
      DOUBLE PRECISION USCM, VSCM,WSCM
      DOUBLE PRECISION WSCM1,WSCM2
! values of U_g, V_g, W_g at appropriate place on boundary wall
      DOUBLE PRECISION UGC, VGC, WGC
! Magnitude of gas-solids relative velocity
      DOUBLE PRECISION VREL
! slip velocity between wall and particles for Jenkins bc (sof)
      DOUBLE PRECISION VSLIP
! radial distribution function at contact
      DOUBLE PRECISION g0_avg(DIMENSION_M)
! Sum of eps*G_0
      DOUBLE PRECISION sum_epsg0_avg
!---------------------------------------------------------------------

!  Note:  EP_s, MU_g, and RO_g are undefined at IJK1 (wall cell).  Hence
!         IJK2 (fluid cell) is used in averages.
!

      smallTheta = (to_SI)**4 * ZERO_EP_S

      SELECT CASE (TYPE_OF_CELL)

         CASE('U_MOMENTUM')

            IJK2 = EAST_OF(IJK)
            IJK2E = EAST_OF(IJK2)

         CASE('V_MOMENTUM')

            IJK2 = NORTH_OF(IJK)
            IJK2E = NORTH_OF(IJK2)

         CASE('W_MOMENTUM')

            IJK2 = TOP_OF(IJK)
            IJK2E = TOP_OF(IJK2)


         CASE DEFAULT
            WRITE(ERR_MSG,*) 'CG_CALC_GRBDRY'//&
               'UNKNOWN TYPE OF CELL:',TYPE_OF_CELL//&
               'ACCEPTABLE TYPES ARE:'//&
               'U_MOMENTUM'//&
               'V_MOMENTUM'//&
               'W_MOMENTUM'
            call log_error()
          END SELECT


          EPg_avg = (VOL(IJK)*EP_g(IJK) + VOL(IJK2)*EP_g(IJK2))/(VOL(IJK) + VOL(IJK2))
          ep_star_avg = (VOL(IJK)*EP_star_array(IJK) + VOL(IJK2)*EP_star_array(IJK2))/(VOL(IJK) + VOL(IJK2))
          Mu_g_avg = (VOL(IJK)*Mu_g(IJK) + VOL(IJK2)*Mu_g(IJK2))/(VOL(IJK) + VOL(IJK2))
          RO_g_avg = (VOL(IJK)*RO_g(IJK) + VOL(IJK2)*RO_g(IJK2))/(VOL(IJK) + VOL(IJK2))

            K_12_avg = ZERO
            Tau_12_avg = ZERO
            Tau_1_avg = ZERO
            IF(KT_TYPE_ENUM == SIMONIN_1996 .OR.&
               KT_TYPE_ENUM == AHMADI_1995) THEN  ! not converted to CG
               Tau_1_avg = AVG_X(Tau_1(IJK2), Tau_1(IJK2E), I_OF(IJK2))
! Simonon only:
               K_12_avg = AVG_X(K_12(IJK2), K_12(IJK2E), I_OF(IJK2))
               Tau_12_avg = AVG_X(Tau_12(IJK2), Tau_12(IJK2E), I_OF(IJK2))
            ENDIF

            sum_epsg0_avg = ZERO
            DO MM = 1, SMAX
               g0_avg(MM)      = G_0AVG(IJK, IJK, 'X', I_OF(IJK), M, MM)
               EPs_avg(MM) = (VOL(IJK)*EP_s(IJK, MM) + VOL(IJK2)*EP_s(IJK2, MM))/(VOL(IJK) + VOL(IJK2))
               DP_avg(MM)  = (VOL(IJK)*D_P(IJK, MM) + VOL(IJK2)*D_P(IJK2, MM))/(VOL(IJK) + VOL(IJK2))
               ROS_avg(MM) = (VOL(IJK)*RO_S(IJK, MM) + VOL(IJK2)*RO_S(IJK2, MM))/(VOL(IJK) + VOL(IJK2))
               sum_epsg0_avg   = sum_epsg0_avg + G_0AVG(IJK, IJK, 'X', I_OF(IJK), M, MM) &
                           * (VOL(IJK)*EP_s(IJK, MM) + VOL(IJK2)*EP_s(IJK2, MM))/(VOL(IJK) + VOL(IJK2))

               TH_avg(MM)  = (VOL(IJK)*Theta_m(IJK,MM) + VOL(IJK2)*Theta_m(IJK2,MM))/(VOL(IJK) + VOL(IJK2))
               IF(TH_avg(MM) < ZERO) TH_avg(MM) = smallTheta
            ENDDO

!*********************THIS IS NOT CORRECT FOR EVERY DIRECTION***********************
! Calculate velocity components at i+1/2, j+1/2, k (relative to IJK1)    ! not converted to CG
            UGC  = AVG_Y(U_g(IJK1), U_g(IJK2),J_OF(IJK1))
            VGC  = AVG_X(V_g(IJK1), V_g(IPJMK2),I_OF(IJK1))
            WGC1 = AVG_X(AVG_Z_T(W_g(KM_OF(IJK2)), W_g(IJK2)),&
                         AVG_Z_T(W_g(KM_OF(IPJK2)), W_g(IPJK2)),&
                         I_OF(IJK2))
            WGC2 = AVG_X(AVG_Z_T(W_g(KM_OF(IJK1)), W_g(IJK1)),&
                         AVG_Z_T(W_g(KM_OF(IPJMK2)), W_g(IPJMK2)),&
                         I_OF(IJK1))
            WGC  = AVG_Y(WGC2, WGC1, J_OF(IJK1))
            USCM = AVG_Y(U_s(IJK1,M), U_s(IJK2,M),J_OF(IJK1))
            VSCM = AVG_X(V_s(IJK1,M), V_s(IPJMK2,M),I_OF(IJK1))
            WSCM1= AVG_X(AVG_Z_T(W_s(KM_OF(IJK2),M),W_s(IJK2,M)),&
                         AVG_Z_T(W_s(KM_OF(IPJK2),M),W_s(IPJK2,M)),&
                         I_OF(IJK2))
            WSCM2= AVG_X(AVG_Z_T(W_s(KM_OF(IJK1),M),W_s(IJK1,M)),&
                         AVG_Z_T(W_s(KM_OF(IPJMK2),M),W_s(IPJMK2,M)),&
                         I_OF(IJK1))
            WSCM = AVG_Y(WSCM2, WSCM1, J_OF(IJK1))

! magnitude of gas-solids relative velocity
            VREL = DSQRT( (UGC - USCM)**2 + (VGC - VSCM)**2 + &
                          (WGC - WSCM)**2 )

! slip velocity for use in Jenkins bc (sof)
            VSLIP= DSQRT( (USCM-BC_UW_S(L,M))**2 + (VSCM-BC_VW_S(L,M))**2 &
                        + (WSCM-BC_WW_S(L,M))**2 )


! why bother with this since it should not come here...
            CALL CG_CALC_GRBDRY_F2(g0_avg, EPs_avg, EPg_avg, ep_star_avg, &
                      sum_epsg0_avg, TH_avg, Mu_g_avg, RO_g_avg, ROS_AVG,&
                      DP_avg, K_12_avg, Tau_12_avg, Tau_1_avg, &
                      VREL, VSLIP, M,F_2)

      RETURN

      END SUBROUTINE CG_CALC_GRBDRY


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: CG_CALC_GRBDRY_F2                                       C
!  Purpose: Compute F_2 for cut cell version                           C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE CG_CALC_GRBDRY_F2(g0,EPS,EPG, ep_star_avg, &
                                     sum_epsg0_avg,TH,Mu_g_avg,RO_g_avg,Ros_avg, &
                                     DP_avg,K_12_avg, Tau_12_avg, Tau_1_avg, &
                                     VREL, VSLIP, M,F_2)

! Global variables
!---------------------------------------------------------------------
      USE bc_theta_mod, only: phip_jj
      use constant, only: pi, tan_phi_w, phip, C_e, eta
      USE error_manager, only: err_msg
      USE param1, only: zero, half, one
      USE param, only: dimension_m
      USE run, only: bc_jj_m, jenkins
! kinetic theories
      USE run, only: kt_type_enum
      USE run, only: kt_type
      USE run, only: lun_1984
      USE run, only: simonin_1996
      USE run, only: ahmadi_1995
      USE run, only: gd_1999
      USE run, only: gtsh_2012
      USE run, only: ia_2005
      IMPLICIT NONE

! Dummy Arguments
!---------------------------------------------------------------------
! Radial distribution function of solids phase M with each
! other solids phase
      DOUBLE PRECISION, INTENT(IN) :: g0(DIMENSION_M)
! Average solids volume fraction of each solids phase
      DOUBLE PRECISION, INTENT(IN) :: EPS(DIMENSION_M)
! Average solids and gas volume fraction
      DOUBLE PRECISION, INTENT(IN) :: EPG, ep_star_avg
! Sum of eps*G_0
      DOUBLE PRECISION, INTENT(INOUT) :: sum_epsg0_avg
! Average theta_m
      DOUBLE PRECISION, INTENT(INOUT) :: TH(DIMENSION_M)
! Average gas viscosity
      DOUBLE PRECISION, INTENT(IN) :: Mu_g_avg
! Average gas density
      DOUBLE PRECISION, INTENT(IN) :: RO_g_avg
!QX: Average solids density
      DOUBLE PRECISION, INTENT(IN) :: ROS_avg(DIMENSION_M)
! Average particle diameter of each solids phase
      DOUBLE PRECISION, INTENT(IN) :: DP_avg(DIMENSION_M)
! Average cross-correlation K_12 and lagrangian integral time-scale
      DOUBLE PRECISION, INTENT(IN) :: K_12_avg, Tau_12_avg, Tau_1_avg
! Magnitude of slip velocity between two phases
      DOUBLE PRECISION, INTENT(IN) :: VREL
! Slip velocity between wall and particles
      DOUBLE PRECISION, INTENT(IN) :: VSLIP
! Solids phase index
      INTEGER, INTENT(IN) :: M

! Local Variables
!---------------------------------------------------------------------
! Solids pressure
      DOUBLE PRECISION :: Ps
      DOUBLE PRECISION :: F_2
      DOUBLE PRECISION :: D_PM, M_PM
!---------------------------------------------------------------------

! This is done here similar to bc_theta to avoid small negative values of
! Theta coming most probably from linear solver
      IF(TH(M) .LE. ZERO)THEN
         TH(M) = 1D-8
         WRITE (ERR_MSG,*) 'CG_CALC_GRBDRY => CG_CALC_GRBDRY_F2 '//&
            'Warning: Negative granular temp at wall set to 1e-8'
         CALL log_error()
      ENDIF

! common variables
      D_PM = DP_avg(M)
      M_PM = (PI/6.d0)*(D_PM**3)*ROS_avg(M)

! Defining viscosity according to each KT for later use in JJ BC
! Also defining solids pressure according to each KT for use if JENKINS
! -------------------------------------------------------------------
      SELECT CASE (KT_TYPE_ENUM)
      CASE (LUN_1984)
! defining granular pressure (for Jenkins BC)
         Ps = ROs_avg(M)*EPS(M)*TH(M)*(ONE+4.D0*Eta*sum_epsg0_avg)


      CASE (SIMONIN_1996)
! defining granular pressure (for Jenkins BC)
         Ps = ROs_avg(M)*EPS(M)*TH(M)*(ONE+4.D0*Eta*sum_epsg0_avg)


      CASE (AHMADI_1995)
! defining granular pressure (for Jenkins BC)
         Ps = ROs_avg(M)*EPS(M)*TH(M)*&
             ((ONE + 4.0D0*sum_epsg0_avg)+HALF*(ONE -C_e*C_e))


      CASE(IA_2005)
! Use original IA theory if SWITCH_IA is false
! no ps since no jenkins for this theory

      CASE(GD_1999)

! defining granular pressure (for Jenkins BC)
         Ps = ROs_avg(M)*EPS(M)*TH(M)*(ONE+2.d0*(ONE+C_E)*sum_epsg0_avg)
                    ! ~ROs_avg(m)*EPS(M)*TH(M)*press_star


      CASE (GTSH_2012)
! defining granular pressure (for Jenkins BC)
         Ps = ROs_avg(M)*EPS(M)*TH(M)*(ONE+2.d0*(ONE+C_E)*sum_epsg0_avg)


      CASE DEFAULT
! should never hit this
         WRITE (ERR_MSG,*) 'CG_CALC_GRBDRY => GET_CG_F2 '//&
            'Unknown KT_TYPE: ', KT_TYPE
         call log_error()
      END SELECT


! setting the coefficients for JJ BC
! -------------------------------------------------------------------
      SELECT CASE (KT_TYPE_ENUM)

         CASE (LUN_1984, SIMONIN_1996, AHMADI_1995, GD_1999, GTSH_2012)
! KT without particle mass in their definition of granular temperature
! and theta = granular temperature  OR
! KT with particle mass in their definition of granular temperature
! and theta = granular temperature/mass

! Jenkins BC is implemented for these KT
            IF(JENKINS) THEN
               IF (VSLIP == ZERO) THEN
! if solids velocity field is initialized to zero, use free slip bc
                  F_2 = zero
               ELSE
! As I understand from soil mechanic papers, the coefficient mu in
! Jenkins paper is tan_Phi_w. T.  See for example, G.I. Tardos, PT,
! 92 (1997), 61-74, equation (1). sof

! here F_2 divided by VSLIP to use the same bc as Johnson&Jackson
                  F_2 = tan_Phi_w*Ps/VSLIP

               ENDIF
            ELSE
               IF(.NOT. BC_JJ_M) THEN
                  F_2 = (PHIP*DSQRT(3d0*TH(M))*Pi*ROS_avg(M)*EPS(M)*&
                     g0(M))/(6d0*(ONE-ep_star_avg))
               ELSE
                  F_2 = (PHIP_JJ(vslip,th(m))*DSQRT(3d0*TH(M))*Pi*&
                     ROS_avg(M)*EPS(M)*g0(M))/(6d0*(ONE-ep_star_avg))
               ENDIF
            ENDIF   ! end if(Jenkins)/else

         CASE(IA_2005)
! KTs with particle mass in their definition of granular temperature
! and theta = granular temperature
            IF(.NOT. BC_JJ_M) THEN
               F_2 = (PHIP*DSQRT(3.d0*TH(M)/M_PM)*PI*ROS_avg(M)*&
                  EPS(M)*g0(M))/(6.d0*(ONE-ep_star_avg))
            ELSE
               F_2 = (PHIP_JJ(vslip,th(m))*DSQRT(3.d0*TH(M)/M_PM)*&
                  PI*ROS_avg(M)*EPS(M)*g0(M))/(6.d0*(ONE-ep_star_avg))
            ENDIF


         CASE DEFAULT
! should never hit this
            WRITE (ERR_MSG,*) 'CG_CALC_GRBDRY => CG_CALC_GRBDRY_F2 '//&
               'Unknown KT_TYPE: ', KT_TYPE
            call log_error()
      END SELECT

! all the code used to calculate Mu_s was deleted from this routine
!      F_HW =  F_2/Mu_s  ! Only F_2 is actually needed

      RETURN

   END SUBROUTINE CG_CALC_GRBDRY_F2

END MODULE CALC_GR_BOUNDARY
