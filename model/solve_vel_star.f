MODULE solve_vel_star_mod

   use adjust_a_g, only: adjust_a_u_g, adjust_a_v_g, adjust_a_w_g
   use adjust_a_s, only: adjust_a_u_s, adjust_a_v_s, adjust_a_w_s
   use adjust_leq_mod, only: adjust_leq
   use calc_d_e_mod, only: calc_d_e
   use calc_d_ghd_mod, only: calc_d_ghd_e
   use calc_d_ghd_mod, only: calc_d_ghd_n
   use calc_d_ghd_mod, only: calc_d_ghd_t
   use calc_d_n_mod, only: calc_d_n
   use calc_d_t_mod, only: calc_d_t
   use calc_e_mod, only: calc_e_e
   use calc_e_mod, only: calc_e_n
   use calc_e_mod, only: calc_e_t
   use calc_external_forces_mod, only: calc_external_forces
   use calc_resid_mod, only: calc_resid_u, calc_resid_v, calc_resid_w
   use conv_dif_u_g_mod, only: conv_dif_u_g
   use conv_dif_u_s_mod, only: conv_dif_u_s
   use conv_dif_v_g_mod, only: conv_dif_v_g
   use conv_dif_v_s_mod, only: conv_dif_v_s
   use conv_dif_w_g_mod, only: conv_dif_w_g
   use conv_dif_w_s_mod, only: conv_dif_w_s
   use gas_drag_mod, only: gas_drag_u, gas_drag_v, gas_drag_w
   use ghdmassflux_mod, only: ghdmassflux, updatespeciesvelocities
   use init_ab_m_mod, only: init_ab_m
   use partial_elim_ghd_mod, only: partial_elim_ghd_u, partial_elim_ghd_v, partial_elim_ghd_w
   use partial_elim_mod, only: partial_elim_u, partial_elim_v, partial_elim_w
   use qmomk_gas_drag_mod, only: qmomk_gas_drag
   use solid_drag_mod, only: solid_drag_u, solid_drag_v, solid_drag_w
   use solve_lin_eq_mod, only: solve_lin_eq
   use source_u_g_mod, only: source_u_g, point_source_u_g
   use source_u_s_mod, only: source_u_s, point_source_u_s
   use source_v_g_mod, only: source_v_g, point_source_v_g
   use source_v_s_mod, only: source_v_s, point_source_v_s
   use source_w_g_mod, only: source_w_g, point_source_w_g
   use source_w_s_mod, only: source_w_s, point_source_w_s
   use under_relax_mod, only: under_relax_u, under_relax_v, under_relax_w
   use vf_gs_x_mod, only: vf_gs_x, vf_ss_x
   use vf_gs_y_mod, only: vf_gs_y, vf_ss_y
   use vf_gs_z_mod, only: vf_gs_z, vf_ss_z

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: SOLVE_VEL_STAR                                          C
!  Purpose: Solve starred velocity components                          C
!                                                                      C
!  Author: M. Syamlal                                 Date: 25-APR-96  C
!                                                                      C
!  Revision Number: 2                                                  C
!  Purpose: allow multiparticle in D_E, D_N and D_T calculations       C
!           and account for the averaged Solid-Solid drag              C
!  Author: S. Dartevelle, LANL                        Date: 28-FEb-04  C
!                                                                      C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SOLVE_VEL_STAR(IER)

      ! Use ambm
      ! Use tmp_array,  VxF_ss => ArrayLM
      ! Use tmp_array1,  VxF_gs => Arraym1
      use compar, only: ijkstart3, ijkend3
      use discretelement, only: des_continuum_hybrid, discrete_element, des_continuum_coupled
      use fldvar, only: u_g, v_g, w_g, u_s, v_s, w_s
      use geometry, only: do_k, ijkmax2
      use leqsol, only: leq_sweep, leq_tol, leq_pc, leq_it, leq_method
      use param, only: dimension_3, dimension_m
      use param1, only: zero, dimension_lm
      use pgcor, only: d_e, d_t, d_n
      use physprop,only: mmax
      use ps, only: point_source
      use pscor, only: e_e, e_t, e_n, mcp
      use qmom_kinetic_equation
      use residual, only: resid_u, resid_v, resid_w, num_resid, den_resid, resid, max_resid, ijk_resid
      use run, only: kt_type_enum, GHD_2007
      use run, only: momentum_x_eq, momentum_y_eq, momentum_z_eq
      use ur_facs, only: ur_fac
      use usr_src, only: call_usr_source, calc_usr_source
      use usr_src, only: gas_u_mom, gas_v_mom, gas_w_mom
      use usr_src, only: solids_u_mom, solids_v_mom, solids_w_mom

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! Error index
      INTEGER, INTENT(INOUT) :: IER

! Local variables
!---------------------------------------------------------------------//
! fluid cell index
      INTEGER :: IJK
! temporary velocity arrays
      DOUBLE PRECISION, DIMENSION(:), allocatable :: U_gtmp,  V_gtmp, W_gtmp
      DOUBLE PRECISION, DIMENSION(:,:), allocatable :: U_stmp, V_stmp, W_stmp
! linear equation solver method and iterations
      INTEGER :: LEQM, LEQI

      LOGICAL :: DO_SOLIDS

! temporary use of global arrays:
! arraym1 (locally vxf_gs)
! the volume x average gas-solids drag at momentum cell centers
!      DOUBLE PRECISION :: VXF_GS(DIMENSION_3, DIMENSION_M)
! arraylm (locally vxf_ss)
! the volume x average solids-solids drag at momentum cell centers
!      DOUBLE PRECISION :: VXF_SS(DIMENSION_3, DIMENSION_LM)
! Septadiagonal matrix A_m, vector b_m
!      DOUBLE PRECISION A_m(DIMENSION_3, -3:3, 0:DIMENSION_M)
!      DOUBLE PRECISION B_m(DIMENSION_3, 0:DIMENSION_M)
!---------------------------------------------------------------------//

      allocate(U_gtmp(DIMENSION_3))
      allocate(V_gtmp(DIMENSION_3))
      allocate(W_gtmp(DIMENSION_3))
      allocate(U_stmp(DIMENSION_3,DIMENSION_M))
      allocate(V_stmp(DIMENSION_3,DIMENSION_M))
      allocate(W_stmp(DIMENSION_3,DIMENSION_M))

      ! call lock_ambm        ! locks arrays a_m and b_m
      ! call lock_tmp_array1  ! locks arraym1 (locally vxf_gs)
      ! call lock_tmp_array2  ! locks arraylm (locally vxf_ss)

      CALL init(U_gtmp, V_gtmp, W_gtmp, U_stmp, V_stmp, W_stmp)

      DO_SOLIDS = .NOT.(DISCRETE_ELEMENT .OR. QMOMK) .OR. &
         DES_CONTINUUM_HYBRID

!!$omp parallel sections default(shared)
      CALL U_m_star(U_gtmp,U_stmp)
!!$omp section
      CALL V_m_star(V_gtmp,V_stmp)
!!$omp section
      CALL W_m_star(W_gtmp,W_stmp)
!!$omp end parallel sections

      CALL save(U_gtmp, V_gtmp, W_gtmp, U_stmp, V_stmp, W_stmp)

! modification for GHD theory to compute species velocity: Ui = Joi/(mi ni) + U.
      IF(KT_TYPE_ENUM == GHD_2007) THEN
         CALL calc_external_forces()
         CALL GHDMassFlux() ! to compute solid species mass flux
         CALL UpdateSpeciesVelocities() ! located at end of ghdMassFlux.f file
      ENDIF

      ! call unlock_ambm
      ! call unlock_tmp_array1
      ! call unlock_tmp_array2

      deallocate(U_gtmp)
      deallocate(V_gtmp)
      deallocate(W_gtmp)
      deallocate(U_stmp)
      deallocate(V_stmp)
      deallocate(W_stmp)

      RETURN

    CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE init(U_g_tmp, V_g_tmp, W_g_tmp, U_s_tmp, V_s_tmp, W_s_tmp)
        IMPLICIT NONE
!---------------------------------------------------------------------//
        DOUBLE PRECISION, DIMENSION(:), intent(out) :: U_g_tmp,  V_g_tmp, W_g_tmp
        DOUBLE PRECISION, DIMENSION(:,:), intent(out) :: U_s_tmp, V_s_tmp, W_s_tmp
!---------------------------------------------------------------------//
        ! solids phase index
        INTEGER :: M
!---------------------------------------------------------------------//

        ! Store the velocities so that the order of solving the momentum
        ! equations does not matter
        DO IJK = ijkstart3, ijkend3
           U_g_tmp(IJK) = U_g(IJK)
           V_g_tmp(IJK) = V_g(IJK)
           W_g_tmp(IJK) = W_g(IJK)
        ENDDO
        DO M = 1, MMAX
           IF(KT_TYPE_ENUM /= GHD_2007 .OR. &
                (KT_TYPE_ENUM == GHD_2007 .AND. M==MMAX)) THEN
              DO IJK = ijkstart3, ijkend3
                 U_s_tmp(IJK, M) = U_s(IJK, M)
                 V_s_tmp(IJK, M) = V_s(IJK, M)
                 W_s_tmp(IJK, M) = W_s(IJK, M)
              ENDDO
           ENDIF
        ENDDO

      END SUBROUTINE init

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE save(U_g_tmp, V_g_tmp, W_g_tmp, U_s_tmp, V_s_tmp, W_s_tmp)
        IMPLICIT NONE
!---------------------------------------------------------------------//
        DOUBLE PRECISION, DIMENSION(:), intent(in) :: U_g_tmp,  V_g_tmp, W_g_tmp
        DOUBLE PRECISION, DIMENSION(:,:), intent(in) :: U_s_tmp, V_s_tmp, W_s_tmp
!---------------------------------------------------------------------//
        ! solids phase index
        INTEGER :: M
!---------------------------------------------------------------------//

        ! Now update all velocity components
        DO IJK = ijkstart3, ijkend3
           U_g(IJK) = U_g_tmp(IJK)
           V_g(IJK) = V_g_tmp(IJK)
           W_g(IJK) = W_g_tmp(IJK)
        ENDDO
        DO M = 1, MMAX
           IF(KT_TYPE_ENUM /= GHD_2007 .OR. &
                (KT_TYPE_ENUM == GHD_2007 .AND. M==MMAX)) THEN
              DO IJK = ijkstart3, ijkend3
                 U_s(IJK, M) = U_s_tmp(IJK, M)
                 V_s(IJK, M) = V_s_tmp(IJK, M)
                 W_s(IJK, M) = W_s_tmp(IJK, M)
              ENDDO
           ENDIF
        ENDDO

      END SUBROUTINE save

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE U_m_star(U_g_tmp, U_s_tmp)
        IMPLICIT NONE
!---------------------------------------------------------------------//
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: U_g_tmp
        DOUBLE PRECISION, DIMENSION(:, :), INTENT(OUT) :: U_s_tmp
!---------------------------------------------------------------------//
        ! solids phase index
        INTEGER :: M
        DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: VXF_GS, VXF_SS, B_M
        DOUBLE PRECISION, DIMENSION(:, :, :), ALLOCATABLE :: A_M
!---------------------------------------------------------------------//

        allocate(vxf_gs(DIMENSION_3,DIMENSION_M))
        allocate(vxf_ss(DIMENSION_3,DIMENSION_LM))
        ALLOCATE(A_M(DIMENSION_3, -3:3, 0:DIMENSION_M))
        ALLOCATE(B_M(DIMENSION_3, 0:DIMENSION_M))

! Calculate U_m_star and residuals
! ---------------------------------------------------------------->>>
      DO M = 0, MMAX
         CALL INIT_AB_M (A_M, B_M, IJKMAX2, M)
         IF (M >= 1) VXF_GS(:,M) = ZERO
      ENDDO

! calculate the convection-diffusion terms for the gas and solids phase
! u-momentum equations
      CALL CONV_DIF_U_G (A_M, B_M)
      IF(DO_SOLIDS) CALL CONV_DIF_U_S (A_M, B_M)

! calculate the source terms for the gas and solids phase u-momentum
! equations
      CALL SOURCE_U_G (A_M, B_M)
      IF(POINT_SOURCE) CALL POINT_SOURCE_U_G (A_M, B_M)
      IF(CALL_USR_SOURCE(3)) CALL CALC_USR_SOURCE(GAS_U_MOM, A_M, B_M)
      IF(DO_SOLIDS) THEN
         CALL SOURCE_U_S (A_M, B_M)
         IF(POINT_SOURCE) CALL POINT_SOURCE_U_S (A_M, B_M)
         IF(CALL_USR_SOURCE(3)) CALL CALC_USR_SOURCE(SOLIDS_U_MOM, A_M, B_M)
      ENDIF

! evaluate local variable vxf_gs and vxf_ss.  both terms are sent to the
! subroutine calc_d (pressure correction equation coefficients).  the
! former is also used in the subroutine partial_elim_u while the latter
! is effectively re-evaluated within said subroutine
      CALL VF_GS_X (VXF_GS)
      IF(DO_SOLIDS .AND. (KT_TYPE_ENUM /= GHD_2007)) THEN
         IF (MMAX > 0) CALL VF_SS_X (VXF_SS)
      ENDIF

! calculate coefficients for the pressure correction equation
      IF(KT_TYPE_ENUM == GHD_2007) THEN
         CALL CALC_D_GHD_E (A_M, VXF_GS, D_E)
      ELSE
         CALL CALC_D_E (A_M, VXF_GS, VXF_SS, D_E, IER)
      ENDIF

      IF(DO_SOLIDS) THEN
! calculate coefficients for a solids volume correction equation
         IF (MMAX > 0) CALL CALC_E_E (A_M, MCP, E_E)

! calculate modifications to the A matrix center coefficient and B
! source vector for partial elimination
         IF(KT_TYPE_ENUM == GHD_2007) THEN
            IF (MMAX > 0) CALL PARTIAL_ELIM_GHD_U (U_G,U_S,VXF_GS,A_M,B_M)
         ELSE
            IF (MMAX > 0) CALL PARTIAL_ELIM_U (U_G,U_S,VXF_GS,A_M,B_M)
         ENDIF
      ENDIF

! handle special case where center coefficient is zero
      CALL ADJUST_A_U_G (A_M, B_M)
      IF(DO_SOLIDS) CALL ADJUST_A_U_S (A_M, B_M)

! calculate modifications to the A matrix center coefficient and B
! source vector for treating DEM drag terms
      IF(DES_CONTINUUM_COUPLED) THEN
         CALL GAS_DRAG_U(A_M, B_M, IER)
         IF (DES_CONTINUUM_HYBRID) &
            CALL SOLID_DRAG_U(A_M, B_M)
      ENDIF

      IF(QMOMK .AND. QMOMK_COUPLED) THEN
         CALL QMOMK_GAS_DRAG(A_M, B_M, IER, 1, 0, 0)
      ENDIF

      IF (MOMENTUM_X_EQ(0)) THEN
         CALL CALC_RESID_U (U_G, V_G, W_G, A_M, B_M, 0, &
            NUM_RESID(RESID_U,0), DEN_RESID(RESID_U,0), &
            RESID(RESID_U,0), MAX_RESID(RESID_U,0), &
            IJK_RESID(RESID_U,0))
         CALL UNDER_RELAX_U (U_G, A_M, B_M, 0, UR_FAC(3))
!         call check_ab_m(a_m, b_m, 0, .false., ier)
!         call write_ab_m(a_m, b_m, ijkmax2, 0)
!         write(*,*) &
!            resid(resid_u, 0), max_resid(resid_u, 0), &
!            ijk_resid(resid_u, 0)
      ENDIF

      IF(DO_SOLIDS) THEN
         DO M = 1, MMAX
            IF(KT_TYPE_ENUM /= GHD_2007 .OR. &
              (KT_TYPE_ENUM == GHD_2007 .AND. M==MMAX)) THEN
               IF (MOMENTUM_X_EQ(M)) THEN
                  CALL CALC_RESID_U (U_S(1,M), V_S(1,M), W_S(1,M), A_M,&
                     B_M, M, NUM_RESID(RESID_U,M), &
                     DEN_RESID(RESID_U,M), RESID(RESID_U,M), &
                     MAX_RESID(RESID_U,M), IJK_RESID(RESID_U,M))
                  CALL UNDER_RELAX_U (U_S(1,M), A_M, B_M, M, &
                     UR_FAC(3))
!                  call check_ab_m(a_m, b_m, m, .false., ier)
!                  write(*,*) &
!                     resid(resid_u, m), max_resid(resid_u, m), &
!                     ijk_resid(resid_u, m)
!                  call write_ab_m(a_m, b_m, ijkmax2, m)
               ENDIF   ! end if (momentum_x_eq(m))
            ENDIF ! end if check for GHD Theory
         ENDDO   ! end do (m=1,mmax)
      ENDIF

      IF (MOMENTUM_X_EQ(0)) THEN
!         call test_lin_eq(ijkmax2, ijmax2, imax2, a_m(1, -3, 0), 1,&
!            DO_K,ier)
         CALL ADJUST_LEQ (RESID(RESID_U,0), LEQ_IT(3), LEQ_METHOD(3), &
            LEQI, LEQM)
         CALL SOLVE_LIN_EQ ('U_g', 3, U_G_tmp, A_M, B_M, 0, LEQI, LEQM, &
            LEQ_SWEEP(3), LEQ_TOL(3),  LEQ_PC(3), IER)
!         call out_array(u_g, 'u_g')
      ENDIF

      IF(DO_SOLIDS) THEN
         DO M = 1, MMAX
            IF(KT_TYPE_ENUM /= GHD_2007 .OR. &
              (KT_TYPE_ENUM == GHD_2007 .AND. M==MMAX)) THEN
               IF (MOMENTUM_X_EQ(M)) THEN
!                  call test_lin_eq(ijkmax2, ijmax2, imax2, &
!                     a_m(1, -3, M), 1, DO_K,ier)
                  CALL ADJUST_LEQ (RESID(RESID_U,M), LEQ_IT(3),&
                     LEQ_METHOD(3), LEQI, LEQM)
                  CALL SOLVE_LIN_EQ ('U_s', 3, U_S_tmp(:,M), A_M, &
                     B_M, M, LEQI, LEQM, LEQ_SWEEP(3), LEQ_TOL(3),&
                     LEQ_PC(3), IER)
!                  call out_array(u_s(1,m), 'u_s')
               ENDIF   ! end if (momentum_x_eq(m))
            ENDIF ! end if check for GHD Theory
         ENDDO   ! end do (m=1,mmax)
      ENDIF
! End U_m_star and residuals
! ----------------------------------------------------------------<<<

      deallocate(vxf_gs)
      deallocate(vxf_ss)
      deallocate(a_m)
      deallocate(b_m)

    END SUBROUTINE U_m_star

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE V_m_star(V_g_tmp, V_s_tmp)
        IMPLICIT NONE
!---------------------------------------------------------------------//
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: V_g_tmp
        DOUBLE PRECISION, DIMENSION(:, :), INTENT(OUT) :: V_s_tmp
!---------------------------------------------------------------------//
        ! solids phase index
        INTEGER :: M
        DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: VXF_GS, VXF_SS, B_M
        DOUBLE PRECISION, DIMENSION(:, :, :), ALLOCATABLE :: A_M
!---------------------------------------------------------------------//

        allocate(vxf_gs(DIMENSION_3,DIMENSION_M))
        allocate(vxf_ss(DIMENSION_3,DIMENSION_LM))
        ALLOCATE(A_M(DIMENSION_3, -3:3, 0:DIMENSION_M))
        ALLOCATE(B_M(DIMENSION_3, 0:DIMENSION_M))

! Calculate V_m_star and residuals
! ---------------------------------------------------------------->>>
      DO M = 0, MMAX
         CALL INIT_AB_M (A_M, B_M, IJKMAX2, M)
         IF (M >= 1) VXF_GS(:,M) = ZERO
      ENDDO

! convection-diffusion terms
      CALL CONV_DIF_V_G (A_M, B_M, IER)
!      call write_ab_m(a_m, b_m, ijkmax2, 0)
      IF(DO_SOLIDS) CALL CONV_DIF_V_S (A_M, B_M, IER)

! source terms
      CALL SOURCE_V_G (A_M, B_M)
      IF(POINT_SOURCE) CALL POINT_SOURCE_V_G (A_M, B_M)
      IF(CALL_USR_SOURCE(4)) CALL CALC_USR_SOURCE(GAS_V_MOM, A_M, B_M)
!      call write_ab_m(a_m, b_m, ijkmax2, 0)
      IF(DO_SOLIDS) THEN
         CALL SOURCE_V_S (A_M, B_M)
         IF(POINT_SOURCE) CALL POINT_SOURCE_V_S (A_M, B_M)
         IF(CALL_USR_SOURCE(4)) CALL CALC_USR_SOURCE(SOLIDS_V_MOM, A_M, B_M)
      ENDIF

! evaluate local vxf_gs and vxf_ss
      CALL VF_GS_Y (VXF_GS)
      IF(DO_SOLIDS .AND. (KT_TYPE_ENUM /= GHD_2007)) THEN
         IF (MMAX > 0) CALL VF_SS_Y (VXF_SS)
      ENDIF

! calculate coefficients for the pressure correction equation
      IF(KT_TYPE_ENUM == GHD_2007) THEN
         CALL CALC_D_GHD_N (A_M, VXF_GS, D_N)
      ELSE
         CALL CALC_D_N (A_M, VXF_GS, VXF_SS, D_N, IER)
      ENDIF

      IF(DO_SOLIDS) THEN
! calculate coefficients for a solids pressure correction equation
         IF (MMAX > 0) CALL CALC_E_N (A_M, MCP, E_N)

! calculate modifications to the A matrix center coefficient and B
! source vector for partial elimination
         IF(KT_TYPE_ENUM == GHD_2007) THEN
           IF (MMAX > 0) CALL PARTIAL_ELIM_GHD_V (V_G,V_S,VXF_GS,A_M,B_M)
         ELSE
           IF (MMAX > 0) CALL PARTIAL_ELIM_V (V_G,V_S,VXF_GS,A_M,B_M)
         ENDIF
      ENDIF

! handle special case where center coefficient is zero
      CALL ADJUST_A_V_G (A_M, B_M)
!      call write_ab_m(a_m, b_m, ijkmax2, 0)
      IF(DO_SOLIDS) CALL ADJUST_A_V_S (A_M, B_M)
!      call write_ab_m(a_m, b_m, ijkmax2, 0)

! modification to matrix equation for DEM drag terms
      IF(DES_CONTINUUM_COUPLED) THEN
         CALL GAS_DRAG_V(A_M, B_M, IER)
         IF (DES_CONTINUUM_HYBRID) &
            CALL SOLID_DRAG_V(A_M, B_M)
      ENDIF

      IF(QMOMK .AND. QMOMK_COUPLED) THEN
         CALL QMOMK_GAS_DRAG(A_M, B_M, IER, 0, 1, 0)
      ENDIF


      IF (MOMENTUM_Y_EQ(0)) THEN
         CALL CALC_RESID_V (U_G, V_G, W_G, A_M, B_M, 0, &
            NUM_RESID(RESID_V,0), DEN_RESID(RESID_V,0), &
            RESID(RESID_V,0), MAX_RESID(RESID_V,0), &
            IJK_RESID(RESID_V,0))
         CALL UNDER_RELAX_V (V_G, A_M, B_M, 0, UR_FAC(4))
!         call check_ab_m(a_m, b_m, 0, .false., ier)
!         call write_ab_m(a_m, b_m, ijkmax2, 0)
!         write(*,*) &
!            resid(resid_v, 0), max_resid(resid_v, 0), &
!            ijk_resid(resid_v, 0)
      ENDIF

      IF(DO_SOLIDS) THEN
         DO M = 1, MMAX
            IF(KT_TYPE_ENUM /= GHD_2007 .OR. &
              (KT_TYPE_ENUM == GHD_2007 .AND. M==MMAX)) THEN
               IF (MOMENTUM_Y_EQ(M)) THEN
                  CALL CALC_RESID_V (U_S(1,M), V_S(1,M), W_S(1,M), A_M,&
                     B_M, M, NUM_RESID(RESID_V,M), &
                     DEN_RESID(RESID_V,M),RESID(RESID_V,M), &
                     MAX_RESID(RESID_V,M), IJK_RESID(RESID_V,M))
                  CALL UNDER_RELAX_V (V_S(1,M),A_M,B_M,M,UR_FAC(4))
!                  call check_ab_m(a_m, b_m, m, .false., ier)
!                  write(*,*) &
!                     resid(resid_v, m), max_resid(resid_v, m),
!                     ijk_resid(resid_v, m)
!                  call write_ab_m(a_m, b_m, ijkmax2, m)
               ENDIF   ! end if (momentum_y_eq(m))
            ENDIF ! end if check for GHD Theory
         ENDDO   ! end do (m=1,mmax)
      ENDIF

      IF (MOMENTUM_Y_EQ(0)) THEN
!         call test_lin_eq(ijkmax2, ijmax2, imax2, a_m(1, -3, 0), &
!            1, DO_K, ier)
         CALL ADJUST_LEQ (RESID(RESID_V,0), LEQ_IT(4), LEQ_METHOD(4), &
            LEQI, LEQM)
         CALL SOLVE_LIN_EQ ('V_g', 4, V_G_tmp, A_M, B_M, 0, LEQI, LEQM, &
            LEQ_SWEEP(4), LEQ_TOL(4),  LEQ_PC(4), IER)
!         call out_array(v_g, 'v_g')
      ENDIF

      IF(DO_SOLIDS) THEN
         DO M = 1, MMAX
            IF(KT_TYPE_ENUM /= GHD_2007 .OR. &
              (KT_TYPE_ENUM == GHD_2007 .AND. M==MMAX)) THEN
               IF (MOMENTUM_Y_EQ(M)) THEN
!                  call test_lin_eq(ijkmax2, ijmax2, imax2, &
!                     a_m(1, -3, M), 1, DO_K, ier)
                  CALL ADJUST_LEQ (RESID(RESID_V,M), LEQ_IT(4), &
                     LEQ_METHOD(4), LEQI, LEQM)
                  CALL SOLVE_LIN_EQ ('V_s', 4, V_S_tmp(:,M), A_M, &
                     B_M, M, LEQI, LEQM, LEQ_SWEEP(4), LEQ_TOL(4), &
                     LEQ_PC(4), IER)
!                  call out_array(v_s(1,m), 'v_s')
               ENDIF   ! end if (momentum_y_eq(m))
            ENDIF ! end if check for GHD Theory
         ENDDO   ! end do (m=1,mmax)
      ENDIF
! End V_m_star and residuals
! ----------------------------------------------------------------<<<

      deallocate(vxf_gs)
      deallocate(vxf_ss)
      deallocate(a_m)
      deallocate(b_m)

    END SUBROUTINE V_m_star

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE W_m_star(W_g_tmp, W_s_tmp)
        IMPLICIT NONE
!---------------------------------------------------------------------//
        DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: W_g_tmp
        DOUBLE PRECISION, DIMENSION(:, :), INTENT(OUT) :: W_s_tmp
!---------------------------------------------------------------------//
        ! solids phase index
        INTEGER :: M
        DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: VXF_GS, VXF_SS, B_M
        DOUBLE PRECISION, DIMENSION(:, :, :), ALLOCATABLE :: A_M
!---------------------------------------------------------------------//

        ALLOCATE(VXF_GS(DIMENSION_3,DIMENSION_M))
        ALLOCATE(VXF_SS(DIMENSION_3,DIMENSION_LM))
        ALLOCATE(A_M(DIMENSION_3, -3:3, 0:DIMENSION_M))
        ALLOCATE(B_M(DIMENSION_3, 0:DIMENSION_M))

! Calculate W_m_star and residuals
! ---------------------------------------------------------------->>>
      IF (DO_K)THEN
         DO M = 0, MMAX
            CALL INIT_AB_M (A_M, B_M, IJKMAX2, M)
            IF (M >= 1) VXF_GS(:,M) = ZERO
         ENDDO

! convection diffusion
         CALL CONV_DIF_W_G (A_M, B_M)
         IF(DO_SOLIDS) CALL CONV_DIF_W_S (A_M, B_M)

! source terms
         CALL SOURCE_W_G (A_M, B_M)
         IF(POINT_SOURCE) CALL POINT_SOURCE_W_G (A_M, B_M)
         IF(CALL_USR_SOURCE(5)) CALL CALC_USR_SOURCE(GAS_W_MOM, A_M, B_M)
!         call write_ab_m(a_m, b_m, ijkmax2, 0)
         IF(DO_SOLIDS) THEN
            CALL SOURCE_W_S (A_M, B_M)
            IF(POINT_SOURCE) CALL POINT_SOURCE_W_S (A_M, B_M)
            IF(CALL_USR_SOURCE(5)) CALL CALC_USR_SOURCE(SOLIDS_W_MOM, A_M, B_M)
         ENDIF
!        call write_ab_m(a_m, b_m, ijkmax2, 0)

! evaluate local variable vxf_gs and vxf_ss
         CALL VF_GS_Z (VXF_GS)
         IF(DO_SOLIDS .AND. (KT_TYPE_ENUM /= GHD_2007)) THEN
            IF (MMAX > 0) CALL VF_SS_Z (VXF_SS)
         ENDIF

! calculate coefficients for the pressure correction equation
         IF(KT_TYPE_ENUM == GHD_2007) THEN
            CALL CALC_D_GHD_T (A_M, VXF_GS, D_T)
         ELSE
            CALL CALC_D_T (A_M, VXF_GS, VXF_SS, D_T, IER)
         ENDIF

         IF(DO_SOLIDS) THEN
! calculate coefficients for a solids pressure correction equation
            IF (MMAX > 0) CALL CALC_E_T (A_M, MCP, E_T)

! calculate modifications to the A matrix center coefficient and B
! source vector for partial elimination
            IF(KT_TYPE_ENUM == GHD_2007) THEN
               IF (MMAX > 0) CALL PARTIAL_ELIM_GHD_W (W_G, W_S, VXF_GS, A_M, B_M)
            ELSE
               IF (MMAX > 0) CALL PARTIAL_ELIM_W (W_G, W_S, VXF_GS, A_M, B_M)
            ENDIF
         ENDIF

! handle special case where center coefficient is zero
         CALL ADJUST_A_W_G (A_M, B_M)
         IF(DO_SOLIDS) THEN
            CALL ADJUST_A_W_S (A_M, B_M)
         ENDIF

! modifications to matrix equation for DEM
         IF(DES_CONTINUUM_COUPLED) THEN
            CALL GAS_DRAG_W(A_M, B_M, IER)
            IF (DISCRETE_ELEMENT .AND. DES_CONTINUUM_HYBRID) &
               CALL SOLID_DRAG_W(A_M, B_M)
         ENDIF

         IF(QMOMK .AND. QMOMK_COUPLED) THEN
            CALL QMOMK_GAS_DRAG(A_M, B_M, IER, 0, 0, 1)
         ENDIF

         IF (MOMENTUM_Z_EQ(0)) THEN
            CALL CALC_RESID_W (U_G, V_G, W_G, A_M, B_M, 0, &
               NUM_RESID(RESID_W,0), DEN_RESID(RESID_W,0), &
               RESID(RESID_W,0), MAX_RESID(RESID_W,0), &
               IJK_RESID(RESID_W,0))
            CALL UNDER_RELAX_W (W_G, A_M, B_M, 0, UR_FAC(5))
!            call check_ab_m(a_m, b_m, 0, .false., ier)
!            write(*,*) &
!               resid(resid_w, 0), max_resid(resid_w, 0), &
!               ijk_resid(resid_w, 0)
         ENDIF

         IF(DO_SOLIDS) THEN
            DO M = 1, MMAX
               IF(KT_TYPE_ENUM /= GHD_2007 .OR. &
                 (KT_TYPE_ENUM == GHD_2007 .AND. M==MMAX)) THEN
                  IF (MOMENTUM_Z_EQ(M)) THEN
                     CALL CALC_RESID_W (U_S(1,M), V_S(1,M), W_S(1,M),&
                        A_M, B_M, M, NUM_RESID(RESID_W,M), &
                        DEN_RESID(RESID_W,M), RESID(RESID_W,M), &
                        MAX_RESID(RESID_W,M), IJK_RESID(RESID_W,M))
                     CALL UNDER_RELAX_W (W_S(1,M), A_M, B_M, M, &
                        UR_FAC(5))
!                     call check_ab_m(a_m, b_m, m, .false., ier)
!                     write(*,*) &
!                        resid(resid_w, m), max_resid(resid_w, m), &
!                        ijk_resid(resid_w, m)
!                     call write_ab_m(a_m, b_m, ijkmax2, m)
                  ENDIF   ! end if (momentum_z_eq(m))
               ENDIF ! end if check for GHD Theory
            ENDDO   ! end do (m=1,mmax)
         ENDIF

         IF (MOMENTUM_Z_EQ(0)) THEN
!            call test_lin_eq(ijkmax2, ijmax2, imax2, a_m(1, -3, 0), &
!               1, DO_K, ier)
            CALL ADJUST_LEQ (RESID(RESID_W,0), LEQ_IT(5), &
               LEQ_METHOD(5), LEQI, LEQM)
            CALL SOLVE_LIN_EQ ('W_g', 5, W_G_tmp, A_M, B_M, 0, LEQI,&
               LEQM, LEQ_SWEEP(5), LEQ_TOL(5), LEQ_PC(5), IER)
!            call out_array(w_g, 'w_g')
         ENDIF

         IF(DO_SOLIDS) THEN
            DO M = 1, MMAX
               IF(KT_TYPE_ENUM /= GHD_2007 .OR. &
                 (KT_TYPE_ENUM == GHD_2007 .AND. M==MMAX)) THEN
                  IF (MOMENTUM_Z_EQ(M)) THEN
!                     call test_lin_eq(ijkmax2, ijmax2, imax2, &
!                        a_m(1, -3, M), 1, DO_K, ier)
                     CALL ADJUST_LEQ (RESID(RESID_W,M), LEQ_IT(5), &
                        LEQ_METHOD(5), LEQI, LEQM)
                     CALL SOLVE_LIN_EQ ('W_s', 5, W_S_tmp(:,M), &
                        A_M, B_M, M, LEQI, LEQM, LEQ_SWEEP(5), &
                        LEQ_TOL(5), LEQ_PC(5), IER)
!                     call out_array(w_s(1,m), 'w_s')
                  ENDIF   ! end if (momentum_z_eq(m))
               ENDIF ! end if check for GHD Theory
            ENDDO   ! end do (m=1,mmax)
         ENDIF
      ENDIF   ! end if (do_k)
! End W_m_star and residuals
! ----------------------------------------------------------------<<<

      deallocate(vxf_gs)
      deallocate(vxf_ss)
      deallocate(a_m)
      deallocate(b_m)

    END SUBROUTINE W_M_STAR

  END SUBROUTINE SOLVE_VEL_STAR

END MODULE solve_vel_star_mod
