#include "error.inc"

MODULE CONV_DIF_V_S_Mod

   USE compar, only: ijkstart3, ijkend3
   USE cutcell, only: alpha_ve_c, alpha_vn_c, alpha_vt_c
   USE cutcell, only: cut_v_treatment_at
   USE cutcell, only: oneodx_e_v, oneody_n_v, oneodz_t_v
   USE cutcell, only: theta_v_ne, theta_v_se
   USE cutcell, only: theta_v_nt, theta_v_st
   USE cutcell, only: theta_vn, theta_vn_bar
   USE dif_v_is_mod, only: dif_v_is
   USE fldvar, only: u_s, v_s, w_s
   USE fun_avg, only: avg_x_h, avg_y_h, avg_z_h
   USE fun_avg, only: avg_y_n, avg_y
   USE functions, only: east_of, north_of, top_of
   USE functions, only: flow_at_n, wall_at
   USE functions, only: im_of, jm_of, km_of
   USE functions, only: ip_of, jp_of, kp_of
   USE functions, only: west_of, bottom_of
   USE geometry, only: ayz_v, axz_v, axy_v
   USE geometry, only: odx_e, ody, odz_t, do_k, ox
   USE get_cut_cell_volume_and_areas_mod, only: get_interpolation_terms_s
   USE indices, only: i_of, j_of, k_of
   USE indices, only: jp1, im1, km1
   USE mflux, only: flux_se, flux_sn, flux_st
   USE param, only: dimension_3, dimension_m, bottom, east, north, south, top, west
   USE param1, only: half, one, zero
   USE physprop, only: mmax
   USE run, only: def_cor, discretize, ghd_2007, kt_type_enum, momentum_y_eq
   USE visc_s, only: mu_s
   USE xsi, only: calc_xsi

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: CONV_DIF_V_s                                            C
!  Purpose: Determine convection diffusion terms for V_s momentum eqs  C
!           The off-diagonal coefficients calculated here must be      C
!           positive. The center coefficient and the source vector     C
!           are negative;                                              C
!           See source_v_s                                             C
!                                                                      C
!  Author: M. Syamlal                                 Date: 24-DEC-96  C
!  Reviewer:                                          Date:            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CONV_DIF_V_S(A_M, B_M, IER)

      IMPLICIT NONE

! Dummy Arguments
!---------------------------------------------------------------------//
! Septadiagonal matrix A_m
      DOUBLE PRECISION, INTENT(INOUT) :: A_m(DIMENSION_3, -3:3, 0:DIMENSION_M)
! Vector b_m
      DOUBLE PRECISION, INTENT(INOUT) :: B_m(DIMENSION_3, 0:DIMENSION_M)
! Error index
      INTEGER, INTENT(INOUT) :: IER

! Local Variables
!---------------------------------------------------------------------//
! Solids phase index
      INTEGER :: M
!---------------------------------------------------------------------//

      DO M = 1, MMAX
        IF(KT_TYPE_ENUM /= GHD_2007 .OR. &
           (KT_TYPE_ENUM == GHD_2007 .AND. M==MMAX)) THEN

          IF  (MOMENTUM_Y_EQ(M)) THEN

             IF (DEF_COR) THEN
! USE DEFERRED CORRECTION TO SOLVE V_S
                CALL STORE_A_V_S0 (A_M(1,-3,M), M)
                IF (DISCRETIZE(4) > 1)CALL STORE_A_V_SDC (M, B_M)

             ELSE
! DO NOT USE DEFERRED CORRECTION TO SOLVE FOR V_S
                IF (DISCRETIZE(4) == 0) THEN         ! 0 & 1 => FOUP
                   CALL STORE_A_V_S0 (A_M(1,-3,M), M)
                ELSE
                   CALL STORE_A_V_S1 (A_M(1,-3,M), M)
                ENDIF
             ENDIF

             CALL DIF_V_IS (MU_S(1,M), A_M, M)
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END SUBROUTINE CONV_DIF_V_S

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Calculate the components of velocity on the east, north,   C
!  and top face of a v-momentum cell                                   C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE GET_VCELL_SVTERMS(U, V, WW, M)

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! phase index
      INTEGER, INTENT(IN) :: M
! velocity components
      DOUBLE PRECISION, INTENT(OUT) :: U(DIMENSION_3)
      DOUBLE PRECISION, INTENT(OUT) :: V(DIMENSION_3)
      DOUBLE PRECISION, INTENT(OUT) :: WW(DIMENSION_3)

! Local variables
!---------------------------------------------------------------------//
! indices
      INTEGER :: IJK, J, IJPK
! for cartesian grid
      DOUBLE PRECISION :: AW, HW, VELW
!---------------------------------------------------------------------//

!!!$omp parallel do private(IJK,J,IJPK)
      DO IJK = ijkstart3, ijkend3
         J = J_OF(IJK)
         IJPK = JP_OF(IJK)

         IF(CUT_V_TREATMENT_AT(IJK)) THEN

! East face (i+1/2, j+1/2, k)
            U(IJK) = (Theta_V_se(IJK) * U_S(IJK,M) + &
                      Theta_V_ne(IJK) * U_S(IJPK,M))
            CALL GET_INTERPOLATION_TERMS_S(IJK, M, 'V_MOMENTUM', &
               ALPHA_Ve_c(IJK), AW, HW, VELW)
            U(IJK) = U(IJK) * AW

! North face (i, j+1, k)
            V(IJK) = (Theta_Vn_bar(IJK) * V_s(IJK,M) + &
                      Theta_Vn(IJK) * V_s(IJPK,M))
            CALL GET_INTERPOLATION_TERMS_S(IJK, M, 'V_MOMENTUM',&
               alpha_Vn_c(IJK), AW, HW, VELW)
            V(IJK) = V(IJK) * AW

! Top face (i, j+1/2, k+1/2)
            IF (DO_K) THEN
               WW(IJK) = (Theta_V_nt(IJK) * W_s(IJK,M) + &
                          Theta_V_st(IJK) * W_s(IJPK,M))
               CALL GET_INTERPOLATION_TERMS_S(IJK, M, 'V_MOMENTUM',&
                  ALPHA_Vt_c(IJK), AW, HW, VELW)
               WW(IJK) = WW(IJK) * AW
            ENDIF

         ELSE
            U(IJK) = AVG_Y(U_S(IJK,M),U_S(IJPK,M),J)
            V(IJK) = AVG_Y_N(V_S(IJK,M),V_S(IJPK,M))
            IF (DO_K) WW(IJK) = AVG_Y(W_S(IJK,M),W_S(IJPK,M),J)
         ENDIF
      ENDDO   ! end do ijk

      RETURN
      END SUBROUTINE GET_VCELL_SVTERMS



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Calculate the convective fluxes through the faces of a     C
!  v-momentum cell. Note the fluxes are calculated at all faces        C
!  regardless of flow_at_n of condition of the west, south, or         C
!  bottom face.                                                        C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE GET_VCELL_SCFLUX_TERMS(FLUX_E, FLUX_W, FLUX_N, &
         FLUX_S, FLUX_T, FLUX_B, IJK, M)

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! phase index
      INTEGER, INTENT(IN) :: M
! fluxes through faces of given ijk u-momentum cell
      DOUBLE PRECISION, INTENT(OUT) :: flux_e, flux_w
      DOUBLE PRECISION, INTENT(OUT) :: flux_n, flux_s
      DOUBLE PRECISION, INTENT(OUT) :: flux_t, flux_b
! indices
      INTEGER, INTENT(IN) :: ijk

! Local variables
!---------------------------------------------------------------------//
      INTEGER :: imjk, ijmk, ijkm
      INTEGER :: ijpk, imjpk, ijpkm

! for cartesian grid
      DOUBLE PRECISION :: AW, HW, VELW
!---------------------------------------------------------------------//
! indices
      IJPK = JP_OF(IJK)
      IMJK = IM_OF(IJK)
      IJMK = JM_OF(IJK)
      IJKM = KM_OF(IJK)
      IJPKM = JP_OF(IJKM)
      IMJPK = JP_OF(IMJK)

      IF(CUT_V_TREATMENT_AT(IJK)) THEN
! East face (i+1/2, j+1/2, k)
         Flux_e = (Theta_V_se(IJK) * Flux_sE(IJK,M) + &
                 Theta_V_ne(IJK) * Flux_sE(IJPK,M))
         CALL GET_INTERPOLATION_TERMS_S(IJK, M, 'V_MOMENTUM',&
            ALPHA_Ve_c(IJK), AW, HW, VELW)
! West face (i-1/2, j+1/2, k)
         Flux_e = Flux_e * AW
         Flux_w = (Theta_V_se(IMJK) * Flux_sE(IMJK,M) + &
                 Theta_V_ne(IMJK) * Flux_sE(IMJPK,M))
         CALL GET_INTERPOLATION_TERMS_S(IJK, M, 'V_MOMENTUM',&
            ALPHA_Ve_c(IMJK), AW, HW, VELW)
         Flux_w = Flux_w * AW

! North face (i, j+1, k)
         Flux_n = (Theta_Vn_bar(IJK) * Flux_sN(IJK,M) + &
                 Theta_Vn(IJK) * Flux_sN(IJPK,M))
         CALL GET_INTERPOLATION_TERMS_S(IJK, M, 'V_MOMENTUM',&
            alpha_Vn_c(IJK), AW, HW, VELW)
         Flux_n = Flux_n * AW
! South face (i, j, k)
         Flux_s = (Theta_Vn_bar(IJMK) * Flux_sN(IJMK,M) + &
                 Theta_Vn(IJMK) * Flux_sN(IJK,M))
         CALL GET_INTERPOLATION_TERMS_S(IJK, M, 'V_MOMENTUM',&
            alpha_Vn_c(IJMK), AW, HW, VELW)
         Flux_s = Flux_s * AW

         IF (DO_K) THEN
! Top face (i, j+1/2, k+1/2)
            Flux_t = (Theta_V_nt(IJK) * Flux_sT(IJK,M) + &
                    Theta_V_st(IJK) * Flux_sT(IJPK,M))
            CALL GET_INTERPOLATION_TERMS_S(IJK, M, 'V_MOMENTUM',&
               ALPHA_Vt_c(IJK), AW, HW, VELW)
            Flux_t = Flux_t * AW
! Bottom face (i, j+1/2, k-1/2)
            Flux_b = (Theta_V_nt(IJKM) * Flux_sT(IJKM,M) + &
                    Theta_V_st(IJKM) * Flux_sT(IJPKM,M))
            CALL GET_INTERPOLATION_TERMS_S(IJK, M, 'V_MOMENTUM',&
               ALPHA_Vt_c(IJKM), AW, HW, VELW)
            Flux_b = Flux_b * AW
         ENDIF
      ELSE
         Flux_e = HALF * (Flux_sE(IJK,M)  + Flux_sE(IJPK,M))
         Flux_w = HALF * (Flux_sE(IMJK,M) + Flux_sE(IMJPK,M))
         Flux_n = HALF * (Flux_sN(IJK,M)  + Flux_sN(IJPK,M))
         Flux_s = HALF * (Flux_sN(IJMK,M) + Flux_sN(IJK,M))

         IF (DO_K) THEN
            Flux_t = HALF * (Flux_sT(IJK,M)  + Flux_sT(IJPK,M))
            Flux_b = HALF * (Flux_sT(IJKM,M) + Flux_sT(IJPKM,M))
         ENDIF
      ENDIF   ! end if/else cut_v_treatmeant_at

      RETURN
      END SUBROUTINE GET_VCELL_SCFLUX_TERMS


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Calculate the components of diffusive flux through the     C
!  the faces of a v-momentum cell. Note the fluxes are calculated at   C
!  all faces regardless of flow_at_n of condition of the west, south   C
!  or bottom face.                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE GET_VCELL_SDIFF_TERMS(D_FE, D_FW, D_FN, D_FS, &
         D_FT, D_FB, IJK, M)

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! phase index
      INTEGER, INTENT(IN) :: M
! diffusion through faces of given ijk v-momentum cell
      DOUBLE PRECISION, INTENT(OUT) :: d_fe, d_fw
      DOUBLE PRECISION, INTENT(OUT) :: d_fn, d_fs
      DOUBLE PRECISION, INTENT(OUT) :: d_ft, d_fb
! ijk index
      INTEGER, INTENT(IN) :: ijk

! Local variables
!---------------------------------------------------------------------//
! indices
      INTEGER :: imjk, ijmk, ijkm
      INTEGER :: i, j, k, jp, im, km
      INTEGER :: ijkc, ijkn, ijke, ijkne, ijkw, ijknw
      INTEGER :: ijkt, ijktn, ijkb, ijkbn
! length terms
      DOUBLE PRECISION :: C_AE, C_AW, C_AN, C_AS, C_AT, C_AB
!---------------------------------------------------------------------//

      IMJK = IM_OF(IJK)
      IJMK = JM_OF(IJK)
      IJKM = KM_OF(IJK)

      I = I_OF(IJK)
      J = J_OF(IJK)
      K = K_OF(IJK)
      IM = IM1(I)
      JP = JP1(J)
      KM = KM1(K)

      IJKN = NORTH_OF(IJK)
      IF (WALL_AT(IJK)) THEN
         IJKC = IJKN
      ELSE
         IJKC = IJK
      ENDIF
      IJKE = EAST_OF(IJK)
      IJKNE = EAST_OF(IJKN)
      IJKW = WEST_OF(IJK)
      IJKNW = NORTH_OF(IJKW)

      IF(CUT_V_TREATMENT_AT(IJK)) THEN
         C_AE = ONEoDX_E_V(IJK)
         C_AW = ONEoDX_E_V(IMJK)
         C_AN = ONEoDY_N_V(IJK)
         C_AS = ONEoDY_N_V(IJMK)
         C_AT = ONEoDZ_T_V(IJK)
         C_AB = ONEoDZ_T_V(IJKM)
      ELSE
         C_AE = ODX_E(I)
         C_AW = ODX_E(IM)
         C_AN = ODY(JP)
         C_AS = ODY(J)
         C_AT = ODZ_T(K)
         C_AB = ODZ_T(KM)
      ENDIF

! East face (i+1/2, j+1/2, k)
      D_Fe = AVG_Y_H(AVG_X_H(MU_S(IJKC,M),MU_S(IJKE,M),I),&
                     AVG_X_H(MU_S(IJKN,M),MU_S(IJKNE,M),I),J)*&
             C_AE*AYZ_V(IJK)
! West face (i-1/2, j+1/2, k)
      D_Fw = AVG_Y_H(AVG_X_H(MU_S(IJKW,M),MU_S(IJKC,M),IM),&
                     AVG_X_H(MU_S(IJKNW,M),MU_S(IJKN,M),IM),J)*&
             C_AW*AYZ_V(IMJK)

! North face (i, j+1, k)
      D_Fn = MU_S(IJKN,M)*C_AN*AXZ_V(IJK)
! South face (i, j, k)
      D_Fs = MU_S(IJKC,M)*C_AS*AXZ_V(IJMK)

      IF (DO_K) THEN
         IJKT = TOP_OF(IJK)
         IJKTN = NORTH_OF(IJKT)
         IJKB = BOTTOM_OF(IJK)
         IJKBN = NORTH_OF(IJKB)

! Top face (i, j+1/2, k+1/2)
         D_Ft = AVG_Y_H(AVG_Z_H(MU_S(IJKC,M),MU_S(IJKT,M),K),&
                        AVG_Z_H(MU_S(IJKN,M),MU_S(IJKTN,M),K),J)*&
                OX(I)*C_AT*AXY_V(IJK)
! Bottom face (i, j+1/2, k-1/2)
         D_Fb = AVG_Y_H(AVG_Z_H(MU_S(IJKB,M),MU_S(IJKC,M),KM),&
                        AVG_Z_H(MU_S(IJKBN,M),MU_S(IJKN,M),KM),J)*&
                OX(I)*C_AB*AXY_V(IJKM)
      ENDIF   ! end if (do_k)

      RETURN
      END SUBROUTINE GET_VCELL_SDIFF_TERMS


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: STORE_A_V_s0                                            C
!  Purpose: Determine convection diffusion terms for V_s momentum eqs  C
!           The off-diagonal coefficients calculated here must be      C
!           positive. The center coefficient and the source vector     C
!           are negative. FOUP                                         C
!           See source_v_s                                             C
!                                                                      C
!  Author: M. Syamlal                                 Date: 7-JUN-96   C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number: 1                                                  C
!  Purpose: To incorporate Cartesian grid modifications                C
!  Author: Jeff Dietiker                              Date: 01-Jul-09  C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE STORE_A_V_S0(A_V_S, M)

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! Solids phase index
      INTEGER, INTENT(IN) :: M
! Septadiagonal matrix A_V_s
      DOUBLE PRECISION, INTENT(INOUT) :: A_V_s(DIMENSION_3, -3:3, M:M)

! Local variables
!---------------------------------------------------------------------//
! Indices
      INTEGER :: IJK
      INTEGER :: IMJK, IPJK, IJMK, IJPK, IJKM, IJKP
! Face mass flux
      DOUBLE PRECISION :: flux_e, flux_w, flux_n, flux_s
      DOUBLE PRECISION :: flux_t, flux_b
! Diffusion parameter
      DOUBLE PRECISION :: D_fe, d_fw, d_fn, d_fs, d_ft, d_fb

!---------------------------------------------------------------------//

!$omp     parallel do default(none)                              &
!$omp     private(IJK, IMJK, IJMK, IPJK, IJPK, IJKM, IJKP,       &
!$omp             D_fe, d_fw, d_fn, d_fs, d_ft, d_fb,            &
!$omp             flux_e, flux_w, flux_n, flux_s, flux_t,        &
!$omp             flux_b)                                        &
!$omp     shared(ijkstart3, ijkend3, do_k, a_v_s, m)
      DO IJK = ijkstart3, ijkend3

         IF (FLOW_AT_N(IJK)) THEN

! Calculate convection-diffusion fluxes through each of the faces
            CALL GET_VCELL_SCFLUX_TERMS(flux_e, flux_w, flux_n, &
               flux_s, flux_t, flux_b, ijk, M)
            CALL GET_VCELL_SDIFF_TERMS(d_fe, d_fw, d_fn, d_fs, &
               d_ft, d_fb, ijk, M)
            IPJK = IP_OF(IJK)
            IJPK = JP_OF(IJK)
            IMJK = IM_OF(IJK)
            IJMK = JM_OF(IJK)

! East face (i+1/2, j+1/2, k)
            IF (Flux_e >= ZERO) THEN
               A_V_s(IJK,east,M) = D_Fe
               A_V_s(IPJK,west,M) = D_Fe + Flux_e
            ELSE
               A_V_s(IJK,east,M) = D_Fe - Flux_e
               A_V_s(IPJK,west,M) = D_Fe
            ENDIF
! West face (i-1/2, j+1/2, k)
            IF (.NOT.FLOW_AT_N(IMJK)) THEN
               IF (Flux_w >= ZERO) THEN
                  A_V_s(IJK,west,M) = D_Fw + Flux_w
               ELSE
                  A_V_s(IJK,west,M) = D_Fw
               ENDIF
            ENDIF


! North face (i, j+1, k)
            IF (Flux_n >= ZERO) THEN
               A_V_s(IJK,north,M) = D_Fn
               A_V_s(IJPK,south,M) = D_Fn + Flux_n
            ELSE
               A_V_s(IJK,north,M) = D_Fn - Flux_n
               A_V_s(IJPK,south,M) = D_Fn
            ENDIF
! South face (i, j, k)
            IF (.NOT.FLOW_AT_N(IJMK)) THEN
               IF (Flux_s >= ZERO) THEN
                  A_V_s(IJK,south,M) = D_Fs + Flux_s
               ELSE
                  A_V_s(IJK,south,M) = D_Fs
               ENDIF
            ENDIF


            IF (DO_K) THEN
               IJKP = KP_OF(IJK)
               IJKM = KM_OF(IJK)

! Top face (i, j+1/2, k+1/2)
               IF (Flux_t >= ZERO) THEN
                  A_V_s(IJK,top,M) = D_Ft
                  A_V_s(IJKP,bottom,M) = D_Ft + Flux_t
               ELSE
                  A_V_s(IJK,top,M) = D_Ft - Flux_t
                  A_V_s(IJKP,bottom,M) = D_Ft
               ENDIF
! Bottom face (i, j+1/2, k-1/2)
               IF (.NOT.FLOW_AT_N(IJKM)) THEN
                  IF (Flux_b >= ZERO) THEN
                     A_V_s(IJK,bottom,M) = D_Fb + Flux_b
                  ELSE
                     A_V_s(IJK,bottom,M) = D_Fb
                  ENDIF
               ENDIF
            ENDIF   ! end if (do_k)

         ENDIF   ! end if (flow_at_n)
      ENDDO   ! end do ijk
!$omp end parallel do

      RETURN
      END SUBROUTINE STORE_A_V_S0


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: STORE_A_V_sdc                                           C
!  Purpose: To use deferred correction method to solve the v-momentum  C
!           equation. This method combines first order upwind and a    C
!           user specified higher order method                         C
!                                                                      C
!  Author: C. GUENTHER                                 Date: 8-APR-99  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number: 1                                                  C
!  Purpose: To incorporate Cartesian grid modifications                C
!  Author: Jeff Dietiker                              Date: 01-Jul-09  C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE STORE_A_V_SDC(M, B_M)

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! Solids phase index
      INTEGER, INTENT(IN) :: M
! Vector b_m
      DOUBLE PRECISION, INTENT(INOUT) :: B_m(DIMENSION_3, 0:DIMENSION_M)

! Local variables
!---------------------------------------------------------------------//
! Indices
      INTEGER :: IJK
      INTEGER :: IPJK, IMJK, IJMK, IJPK, IJKM, IJKP
! indication for shear
      INTEGER :: incr
! deferred corrction contribution form high order method
      DOUBLE PRECISION :: MOM_HO
! low order approximation
      DOUBLE PRECISION :: MOM_LO
! convection factor at each face
      DOUBLE PRECISION :: flux_e, flux_w, flux_n, flux_s
      DOUBLE PRECISION :: flux_t, flux_b
! deferred correction contributions from each face
      DOUBLE PRECISION :: EAST_DC
      DOUBLE PRECISION :: WEST_DC
      DOUBLE PRECISION :: NORTH_DC
      DOUBLE PRECISION :: SOUTH_DC
      DOUBLE PRECISION :: TOP_DC
      DOUBLE PRECISION :: BOTTOM_DC

! temporary use of global arrays:
! array1 (locally u)  - the x directional velocity
      DOUBLE PRECISION :: U(DIMENSION_3)
! array2 (locally v)  - the y directional velocity
      DOUBLE PRECISION :: V(DIMENSION_3)
! array3 (locally ww) - the z directional velocity
      DOUBLE PRECISION :: WW(DIMENSION_3)
!---------------------------------------------------------------------//
      DOUBLE PRECISION, DIMENSION(DIMENSION_3) :: XSI_e, XSI_n, XSI_t

      CALL GET_VCELL_SVTERMS(U, V, WW, M)

! shear indicator:
      incr=2
      CALL CALC_XSI (DISCRETIZE(4), V_S(1,M), U, V, WW, XSI_E, XSI_N, &
                     XSI_T, incr)

!!!$omp      parallel do                                             &
!!!$omp&     private( I, J, K, IJK, IPJK, IMJK, IJPK, IJMK,          &
!!!$omp&             flux_e, flux_w, flux_n, flux_s, flux_b, flux_t, &
!!!$omp&             MOM_HO, MOM_LO, EAST_DC, WEST_DC, NORTH_DC,     &
!!!$omp&             SOUTH_DC, TOP_DC, BOTTOM_DC)
      DO IJK = ijkstart3, ijkend3

         IF (FLOW_AT_N(IJK)) THEN

! Calculate convection fluxes through each of the faces
            CALL GET_VCELL_SCFLUX_TERMS(flux_e, flux_w, flux_n, &
               flux_s, flux_t, flux_b, ijk, M)

            IPJK = IP_OF(IJK)
            IMJK = IM_OF(IJK)
            IJPK = JP_OF(IJK)
            IJMK = JM_OF(IJK)
            IJKP = KP_OF(IJK)
            IJKM = KM_OF(IJK)

! East face (i+1/2, j+1/2, k)
            IF(U(IJK) >= ZERO)THEN
               MOM_LO = V_S(IJK,M)
            ELSE
                MOM_LO = V_S(IPJK,M)
            ENDIF
            MOM_HO = XSI_E(IJK)*V_S(IPJK,M)+(1.0-XSI_E(IJK))*V_S(IJK,M)
            EAST_DC = Flux_e*(MOM_LO-MOM_HO)

! West face (i-1/2, j+1/2, k)
            IF(U(IMJK) >= ZERO)THEN
              MOM_LO = V_S(IMJK,M)
            ELSE
               MOM_LO = V_S(IJK,M)
            ENDIF
            MOM_HO = XSI_E(IMJK)*V_S(IJK,M)+(1.0-XSI_E(IMJK))*V_S(IMJK,M)
            WEST_DC = Flux_w*(MOM_LO-MOM_HO)


! North face (i, j+1, k)
            IF(V(IJK) >= ZERO)THEN
               MOM_LO = V_S(IJK,M)
            ELSE
               MOM_LO = V_S(IJPK,M)
            ENDIF
            MOM_HO = XSI_N(IJK)*V_S(IJPK,M)+(1.0-XSI_N(IJK))*V_S(IJK,M)
            NORTH_DC = Flux_n*(MOM_LO-MOM_HO)

! South face (i, j, k)
            IF(V(IJMK) >= ZERO)THEN
               MOM_LO = V_S(IJMK,M)
            ELSE
               MOM_LO = V_S(IJK,M)
            ENDIF
            MOM_HO = XSI_N(IJMK)*V_S(IJK,M)+(1.0-XSI_N(IJMK))*V_S(IJMK,M)
            SOUTH_DC = Flux_s*(MOM_LO-MOM_HO)


            IF (DO_K) THEN

! Top face (i, j+1/2, k+1/2)
               IF(WW(IJK) >= ZERO)THEN
                  MOM_LO = V_S(IJK,M)
               ELSE
                  MOM_LO = V_S(IJKP,M)
               ENDIF
               MOM_HO = XSI_T(IJK)*V_S(IJKP,M)+(1.0-XSI_T(IJK))*V_S(IJK,M)
               TOP_DC = Flux_t*(MOM_LO-MOM_HO)

! Bottom face (i, j+1/2, k-1/2)
               IF(WW(IJK) >= ZERO)THEN
                  MOM_LO = V_S(IJKM,M)
               ELSE
                  MOM_LO = V_S(IJK,M)
               ENDIF
               MOM_HO = XSI_T(IJKM)*V_S(IJK,M)+(1.0-XSI_T(IJKM))*V_S(IJKM,M)
               BOTTOM_DC = Flux_b*(MOM_LO-MOM_HO)
            ELSE
               TOP_DC = ZERO
               BOTTOM_DC = ZERO
            ENDIF   ! end if do_k


! CONTRIBUTION DUE TO DEFERRED CORRECTION
            B_M(IJK,M) = B_M(IJK,M)+WEST_DC-EAST_DC+SOUTH_DC-NORTH_DC+&
                         BOTTOM_DC-TOP_DC

         ENDIF   ! end if flow_at_n
      ENDDO   ! end do ijk

      RETURN
      END SUBROUTINE STORE_A_V_SDC

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: STORE_A_V_s1                                            C
!  Purpose: Determine convection diffusion terms for V_s momentum eqs  C
!           The off-diagonal coefficients calculated here must be      C
!           positive. The center coefficient and the source vector     C
!           are negative. Higher order                                 C
!  See source_v_s                                                      C
!                                                                      C
!  Author: M. Syamlal                                 Date: 20-MAR-97  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number: 1                                                  C
!  Purpose: To incorporate Cartesian grid modifications                C
!  Author: Jeff Dietiker                              Date: 01-Jul-09  C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE STORE_A_V_S1(A_V_S, M)

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! phase index
      INTEGER, INTENT(IN) :: M
! Septadiagonal matrix A_V_s
      DOUBLE PRECISION, INTENT(INOUT) :: A_V_s(DIMENSION_3, -3:3, M:M)

! Local variables
!---------------------------------------------------------------------//
! Indices
      INTEGER :: IJK, IPJK, IMJK, IJPK, IJMK, IJKP, IJKM
! indicator for shear
      INTEGER :: incr
! Diffusion parameter
      DOUBLE PRECISION :: d_fe, d_fw, d_fn, d_fs, d_ft, d_fb
! Face mass flux
      DOUBLE PRECISION :: Flux_e, flux_w, flux_n, flux_s
      DOUBLE PRECISION :: flux_t, flux_b

! temporary use of global arrays:
! array1 (locally u)  - the x directional velocity
      DOUBLE PRECISION :: U(DIMENSION_3)
! array2 (locally v)  - the y directional velocity
      DOUBLE PRECISION :: V(DIMENSION_3)
! array3 (locally ww) - the z directional velocity
      DOUBLE PRECISION :: WW(DIMENSION_3)
!---------------------------------------------------------------------//
      DOUBLE PRECISION, DIMENSION(DIMENSION_3) :: XSI_e, XSI_n, XSI_t

      CALL GET_VCELL_SVTERMS(U, V, WW,M)

! shear indicator:
      incr=2
      CALL CALC_XSI (DISCRETIZE(4), V_S(1,M), U, V, WW, XSI_E, XSI_N, &
                     XSI_T, incr)

!!!$omp      parallel do                                             &
!!!$omp&     private(IJK, IMJK, IJMK, IPJK, IJPK, IJKM, IJKP,        &
!!!$omp&             d_fe, d_fw, d_fn, d_fs, d_ft, d_fb,             &
!!!$omp&             flux_e, flux_w, flux_n, flux_s, flux_t, flux_b)
      DO IJK = ijkstart3, ijkend3

         IF (FLOW_AT_N(IJK)) THEN

! Calculate convection-diffusion fluxes through each of the faces
            CALL GET_VCELL_SCFLUX_TERMS(flux_e, flux_w, flux_n, &
               flux_s, flux_t, flux_b, ijk, M)
            CALL GET_VCELL_SDIFF_TERMS(d_fe, d_fw, d_fn, d_fs, &
               d_ft, d_fb, ijk, M)

            IMJK = IM_OF(IJK)
            IJMK = JM_OF(IJK)
            IPJK = IP_OF(IJK)
            IJPK = JP_OF(IJK)

! East face (i+1/2, j+1/2, k)
            A_V_S(IJK,east,M) = D_Fe - XSI_E(IJK)*Flux_e
            A_V_S(IPJK,west,M) = D_Fe + (ONE - XSI_E(IJK))*Flux_e
! West face (i-1/2, j+1/2, k)
            IF (.NOT.FLOW_AT_N(IMJK)) THEN
               A_V_S(IJK,west,M) = D_Fw + (ONE - XSI_E(IMJK))*Flux_w
            ENDIF


! North face (i, j+1, k)
            A_V_S(IJK,north,M) = D_Fn - XSI_N(IJK)*Flux_n
            A_V_S(IJPK,south,M) = D_Fn + (ONE - XSI_N(IJK))*Flux_n
! South face (i, j, k)
            IF (.NOT.FLOW_AT_N(IJMK)) THEN
               A_V_S(IJK,south,M) = D_Fs + (ONE - XSI_N(IJMK))*Flux_s
            ENDIF


            IF (DO_K) THEN
               IJKP = KP_OF(IJK)
               IJKM = KM_OF(IJK)
! Top face (i, j+1/2, k+1/2)
               A_V_S(IJK,top,M) = D_Ft - XSI_T(IJK)*Flux_t
               A_V_S(IJKP,bottom,M) = D_Ft + (ONE - XSI_T(IJK))*Flux_t
! Bottom face (i, j+1/2, k-1/2)
               IF (.NOT.FLOW_AT_N(IJKM)) THEN
                  A_V_S(IJK,bottom,M) = D_Fb + (ONE - XSI_T(IJKM))*Flux_b
               ENDIF
            ENDIF   ! end if (do_k)

         ENDIF   ! end if flow_at_n
      ENDDO   ! end do ijk

      RETURN

   END SUBROUTINE STORE_A_V_S1

END MODULE CONV_DIF_V_S_Mod
