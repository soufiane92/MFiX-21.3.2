#include "error.inc"

MODULE SET_BC0_MOD

   use bc, only: IJK_P_g, bc_thetaw_m
   use bc, only: bc_ep_g, bc_rop_s
   use bc, only: bc_i_w, bc_i_e
   use bc, only: bc_i_w, bc_i_e
   use bc, only: bc_j_s, bc_j_n
   use bc, only: bc_j_s, bc_j_n
   use bc, only: bc_k_b, bc_k_t
   use bc, only: bc_k_b, bc_k_t
   use bc, only: bc_k_turb_g, bc_e_turb_g
   use bc, only: bc_p_g
   use bc, only: bc_plane
   use bc, only: bc_t_g, bc_t_s, bc_theta_m
   use bc, only: bc_tw_g, bc_tw_s
   use bc, only: bc_x_g, bc_x_s, bc_scalar
   use bc, only: bc_xw_g, bc_xw_s, bc_scalarw
   use bc, only: ijk_p_g, bc_defined, bc_type_enum, free_slip_wall, no_slip_wall, par_slip_wall
   use bc, only: p_outflow, mass_outflow, outflow, mass_inflow, p_inflow
   use compar, only: dead_cell_at, mype, numpes
   use constant, only: pi
   use error_manager, only: err_msg, loglevel_error, loglevel_info, log_message
   use fldvar, only: d_p, ro_s
   use fldvar, only: ep_g, rop_s
   use fldvar, only: k_turb_g, e_turb_g
   use fldvar, only: p_g, p_star
   use fldvar, only: t_g, t_s, theta_m
   use fldvar, only: x_g, x_s, scalar
   use functions, only: fluid_at, is_on_mype_owns
   use functions, only: im_of, ip_of, jm_of, jp_of, km_of, kp_of
   use functions, only: is_on_mype_plus2layers, bound_funijk, funijk
   use funits, only: DMP_LOG
   use geometry, only: CYCLIC_X, CYCLIC_X_PD, CYCLIC_X_MF
   use geometry, only: CYCLIC_Y, CYCLIC_Y_PD, CYCLIC_Y_MF
   use geometry, only: CYCLIC_Z, CYCLIC_Z_PD, CYCLIC_Z_MF
   use geometry, only: do_K
   use geometry, only: iMAX1, iMin1
   use geometry, only: jMAX1, jMin1
   use geometry, only: kMAX1, kMin1
   use indices, only: i_of, j_of, k_of
   use mms, only: calculate_mms, calculate_mms_source, use_mms
   use mms, only: mms_p_g, mms_t_g, mms_ep_g, mms_theta_m, mms_rop_s, mms_t_s
   use mpi_utility, only: global_all_sum, bcast
   use param, only: DIMENSION_BC
   use param1, only: undefined, zero, undefined_i
   use physprop, only: RO_G0, smax, mmax, nmax
   use run, only: kt_type_enum, ghd_2007
   use scalars, only: nscalar
   use scales, only: scale_pressure
   use sendrecv, only: send_recv
   use toleranc, only: tmin
   use turb, only: k_epsilon

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: set_bc0                                                 C
!  Purpose: This subroutine does the initial setting of all boundary   C
!  conditions. The user specifications of the boundary conditions are  C
!  checked for veracity in various check_data/ routines:               C
!  (e.g., check_boundary_conditions).                                  C
!                                                                      C
!  Author: M. Syamlal                                 Date: 29-JAN-92  C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
SUBROUTINE SET_BC0

      IMPLICIT NONE

! Local variables
!--------------------------------------------------------------------//
! Local index for boundary condition
      INTEGER ::  L
!--------------------------------------------------------------------//

! Incompressible cases require that Ppg specified for one cell.
! The following attempts to pick an appropriate cell.
      CALL SET_IJK_P_G

      IF(USE_MMS) THEN
! IJK_P_G is set as UNDEFINED for MMS since current IJK_P_G setting
! is not a second order operation.
         IJK_P_G = UNDEFINED_I
! Calculate MMS variables. Better place might be inside iterate before
! every time-step. (?)
         CALL CALCULATE_MMS
         CALL CALCULATE_MMS_SOURCE
      END IF

      DO L = 1, DIMENSION_BC
         IF (BC_DEFINED(L)) THEN

            SELECT CASE (BC_TYPE_ENUM(L))

            CASE (FREE_SLIP_WALL)
               CALL SET_BC0_WALLS(L)
            CASE (NO_SLIP_WALL)
               CALL SET_BC0_WALLS(L)
            CASE (PAR_SLIP_WALL)
               CALL SET_BC0_WALLS(L)

            CASE (P_OUTFLOW)
               CALL SET_BC0_OUTFLOWA(L)
               CALL SET_BC0_EP(L)

            CASE (MASS_OUTFLOW)
               CALL SET_BC0_OUTFLOWA(L)
               CALL SET_BC0_EP(L)

            CASE (OUTFLOW)
               CALL SET_BC0_OUTFLOWA(L)
               CALL SET_BC0_EP(L)

            CASE (MASS_INFLOW)
               CALL SET_BC0_INFLOW(L)

            CASE (P_INFLOW)
               CALL SET_BC0_INFLOW(L)
               CALL SET_BC0_EP(L)

            END SELECT
         ENDIF
      ENDDO

! Make T_g nonzero in k=0,1 ghost layers when k-decomposition employed
      call send_recv(T_G,2)
      call send_recv(P_G,2)
      call send_recv(X_G,2)

      RETURN
      END SUBROUTINE SET_BC0


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: set_bc0_walls                                           C
!  Purpose: Define certain field variables at the wall boundaries      C
!  according to the user specifications. These are not the real        C
!  values in the wall cells, only initial guesses.                     C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SET_BC0_WALLS(BCV)

      IMPLICIT NONE

! Dummy arguments
!--------------------------------------------------------------------//
! index of boundary
      INTEGER, INTENT(IN) :: BCV

! Local variables
!--------------------------------------------------------------------//
! indices
      INTEGER :: I, J, K, IJK, M
!--------------------------------------------------------------------//

      DO K = BC_K_B(BCV), BC_K_T(BCV)
      DO J = BC_J_S(BCV), BC_J_N(BCV)
      DO I = BC_I_W(BCV), BC_I_E(BCV)

         IF (.NOT.IS_ON_myPE_plus2layers(I,J,K)) CYCLE
         IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
         IJK = BOUND_FUNIJK(I,J,K)

         IF(BC_Tw_g(BCV) /= UNDEFINED) T_g(IJK) = BC_Tw_g(BCV)

         IF(NMAX(0) > 0) &
            WHERE (BC_Xw_G(BCV,:NMAX(0)) /= UNDEFINED) &
            X_G(IJK,:NMAX(0)) = BC_Xw_G(BCV,:NMAX(0))

         IF(SMAX > 0) &
            WHERE(BC_Tw_s(BCV,:SMAX) /= UNDEFINED) &
            T_s(IJK,:SMAX) = BC_Tw_s(BCV,:SMAX)

         IF(MMAX > 0) &
            WHERE(BC_Thetaw_m(BCV,:MMAX) /= UNDEFINED) &
            Theta_m(IJK,:MMAX) = BC_Thetaw_m(BCV,:MMAX)

         DO M = 1, SMAX
            IF(NMAX(M) > 0) &
               WHERE (BC_Xw_s(BCV,M,:NMAX(M)) /= UNDEFINED) &
               X_s(IJK,M,:NMAX(M)) = BC_Xw_s(BCV,M,:NMAX(M))
         ENDDO

         IF(NScalar > 0) &
            WHERE (BC_ScalarW(BCV,:NScalar) /= UNDEFINED) &
            Scalar(IJK,:NScalar) = BC_ScalarW(BCV,:NScalar)

      ENDDO   ! end do (i=bc_i_w(l),bc_i_e(l))
      ENDDO   ! end do (j=bc_j_s(l),bc_j_n(l))
      ENDDO   ! end do (k=bc_k_b(l),bc_k_t(l))

      RETURN
      END SUBROUTINE SET_BC0_WALLS


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: set_bc0_outflowa                                        C
!  Purpose: Set the initial value of boundary cell SCALARS based on    C
!  the BOUNDARY CONDITION VALUE (if it is defined) for the following   C
!  boundary types: pressure outflow (PO), outflow (O), and mass        C
!  outflow (MO). An additional routine is needed to set velocity       C
!  values needed by the MO boundary.                                   C
!                                                                      C
!  Comments: For a new run the field variables are undefined in the    C
!  boundary cell locations, while for a restart run the field variable C
!  may have an existing value based on the preceding simulation.       C
!  Regardless, a user defined BC value will supersede any existing     C
!  value.                                                              C
!                                                                      C
!  Note: Scalar field quantities (i.e., T_G, T_s, x_g, x_s, Theta_m,   C
!  scalar, k_turb_g, e_turb_g) do not (should not) need to be defined  C
!  in any type of outflow boundary as the boundary values are unused   C
!  by the corresponding matrix equation (bc_phi). The only reason      C
!  this is potentially necessary is during a calculation another cell  C
!  references the value of this boundary cell before the corresponding C
!  governing equation solver is called.                                C
!                                                                      C
!  For example, this can cause an issue if we have variable density    C
!  and we need to calculate ro_g in the boundary which could be used   C
!  for a PO boundary.                                                  C
!                                                                      C
!  An exception is made here to set P_g in the boundary to the         C
!  neighbor fluid cell if bc_p_g is undefined. Otherwise one has to    C
!  set_bc1->set_outflow->set_outflow_misc for this to occur..          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SET_BC0_OUTFLOWA(BCV)

      IMPLICIT NONE

! Dummy arguments
!--------------------------------------------------------------------//
! index of boundary condition
      INTEGER, INTENT(IN) :: BCV

! Local variables
!--------------------------------------------------------------------//
! indices
      INTEGER :: I, J, K, IJK, M
! number densities for use in GHD theory only
      DOUBLE PRECISION :: nM, nTOT
!--------------------------------------------------------------------//

      DO K = BC_K_B(BCV), BC_K_T(BCV)
      DO J = BC_J_S(BCV), BC_J_N(BCV)
      DO I = BC_I_W(BCV), BC_I_E(BCV)
         IF (.NOT.IS_ON_myPE_plus2layers(I,J,K)) CYCLE
         IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
         IJK = BOUND_FUNIJK(I,J,K)

         P_STAR(IJK) = ZERO
         P_G(IJK) = SCALE_PRESSURE(BC_P_G(BCV))
         IF (BC_EP_G(BCV) /= UNDEFINED) EP_G(IJK) = BC_EP_G(BCV)

         T_G(IJK)= merge(BC_T_G(BCV), TMIN,&
            BC_T_G(BCV) /= UNDEFINED)

         IF (NMAX(0) > 0) &
            WHERE (BC_X_G(BCV,:NMAX(0)) /= UNDEFINED) &
            X_G(IJK,:NMAX(0)) = BC_X_G(BCV,:NMAX(0))

         IF (NScalar > 0) &
            WHERE (BC_Scalar(BCV,:NScalar) /= UNDEFINED) &
            Scalar(IJK,:NScalar) = BC_Scalar(BCV,:NScalar)

         IF (K_Epsilon) THEN
            IF (BC_K_Turb_G(BCV) /= UNDEFINED) &
               K_Turb_G(IJK) = BC_K_Turb_G(BCV)
            IF (BC_E_Turb_G(BCV) /= UNDEFINED) &
                E_Turb_G(IJK) = BC_E_Turb_G(BCV)
         ENDIF

         DO M = 1, SMAX
            IF (BC_ROP_S(BCV,M) /= UNDEFINED) &
               ROP_S(IJK,M) = BC_ROP_S(BCV,M)
            IF(BC_T_S(BCV,M) /= UNDEFINED) &
               T_S(IJK,M)=BC_T_S(BCV,M)
            IF (BC_THETA_M(BCV,M) /= UNDEFINED) &
               THETA_M(IJK,M) = BC_THETA_M(BCV,M)

            IF (NMAX(M) > 0) &
               WHERE (BC_X_S(BCV,M,:NMAX(M)) /= UNDEFINED) &
               X_S(IJK,M,:NMAX(M)) = BC_X_S(BCV,M,:NMAX(M))
         ENDDO

! for GHD theory to compute mixture BC of velocity and density
         IF(KT_TYPE_ENUM == GHD_2007) THEN
            ROP_S(IJK,MMAX) = ZERO
            nTOT = ZERO
            THETA_M(IJK,MMAX) = ZERO
            DO M = 1, SMAX
               IF (BC_ROP_S(BCV,M) /= UNDEFINED) THEN
                  ROP_S(IJK,MMAX) = ROP_S(IJK,MMAX) + &
                     BC_ROP_S(BCV,M)
                  nM = BC_ROP_S(BCV,M)*6d0 / &
                     (PI*D_p(IJK,M)**3*RO_S(IJK,M))
                  nTOT = nTOT + nM
                  IF (BC_THETA_M(BCV,M) /= UNDEFINED) &
                     THETA_M(IJK,MMAX) = THETA_M(IJK,MMAX) + &
                     nM*BC_THETA_M(BCV,M)
               ENDIF
            ENDDO
            IF(ROP_S(IJK,MMAX) > ZERO) &
               THETA_M(IJK,MMAX) = THETA_M(IJK,MMAX) / nTOT
         ENDIF   ! end if(kt_type_enum== ghd_2007)

! Set MMS BCs when PO boundary condition is used.
         IF (USE_MMS) THEN
            P_G(IJK) = SCALE_PRESSURE(MMS_P_G(IJK))
            EP_G(IJK) = MMS_EP_G(IJK)
            T_G(IJK) = MMS_T_G(IJK)

            ROP_S(IJK,:SMAX) = MMS_ROP_S(IJK)
            T_S(IJK,:SMAX) = MMS_T_S(IJK)
            THETA_M(IJK,:SMAX) = MMS_THETA_M(IJK)
         ENDIF ! end if(USE_MMS)

      ENDDO   ! do i
      ENDDO   ! do j
      ENDDO   ! do k

      RETURN
      END SUBROUTINE SET_BC0_OUTFLOWA


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: set_bc0_inflow                                          C
!  Purpose: Set the initial value of boundary cell SCALARS based on    C
!  the BOUNDARY CONDITION VALUE (if defined) for the following         C
!  boundary types: pressure inflow (PI) and mass inflow (MI).          C
!                                                                      C
!  Comments: Unlike the treament of PO, MO, O boundary types, for      C
!  these boundary types (PI/MI) no checks are made to determine        C
!  whether the given BC value is defined before it is assigned to the  C
!  field variable. However, the corresponding check routines generally C
!  ensure such BC quantities are defined for MI or PI boundaries if    C
!  they are needed for the simulation.                                 C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SET_BC0_INFLOW(BCV)

      IMPLICIT NONE

! Dummy arguments
!--------------------------------------------------------------------//
! index for boundary condition
      INTEGER, INTENT(IN) :: BCV

! Local variables
!--------------------------------------------------------------------//
! indices
      INTEGER :: I, J, K, IJK, M
! number densities for use in GHD theory only
      DOUBLE PRECISION :: nM, nTOT
!--------------------------------------------------------------------//

      DO K = BC_K_B(BCV), BC_K_T(BCV)
      DO J = BC_J_S(BCV), BC_J_N(BCV)
      DO I = BC_I_W(BCV), BC_I_E(BCV)
         IF (.NOT.IS_ON_myPE_plus2layers(I,J,K)) CYCLE
         IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
         IJK = BOUND_FUNIJK(I,J,K)

         P_STAR(IJK) = ZERO
         P_G(IJK) = SCALE_PRESSURE(BC_P_G(BCV))
         EP_G(IJK) = BC_EP_G(BCV)
         T_G(IJK) = BC_T_G(BCV)

         IF (NMAX(0) > 0) &
           X_G(IJK,:NMAX(0)) = BC_X_G(BCV,:NMAX(0))

         IF (NScalar > 0) &
           Scalar(IJK,:NScalar) = BC_Scalar(BCV,:NScalar)

         IF (K_Epsilon) THEN
           K_Turb_G(IJK) = BC_K_Turb_G(BCV)
           E_Turb_G(IJK) = BC_E_Turb_G(BCV)
         ENDIF

         DO M = 1, SMAX
           ROP_S(IJK,M) = BC_ROP_S(BCV,M)
           T_S(IJK,M) = BC_T_S(BCV,M)
           THETA_M(IJK,M) = BC_THETA_M(BCV,M)
           IF (NMAX(M) > 0) &
              X_S(IJK,M,:NMAX(M)) = BC_X_S(BCV,M,:NMAX(M))
         ENDDO

! for GHD theory to compute mixture BC of velocity and density
         IF(KT_TYPE_ENUM == GHD_2007) THEN
            ROP_S(IJK,MMAX) = ZERO
            nTOT = ZERO
            THETA_M(IJK,MMAX) = ZERO
            DO M = 1, SMAX
               ROP_S(IJK,MMAX) = ROP_S(IJK,MMAX) + &
                  BC_ROP_S(BCV,M)
! this will cause issue if any variable density model is used as ro_s
! will not yet be defined
               nM = BC_ROP_S(BCV,M)*6d0/ &
                  (PI*D_p(IJK,M)**3*RO_S(IJK,M))
               nTOT = nTOT + nM
               THETA_M(IJK,MMAX) = THETA_M(IJK,MMAX) + &
                  nM*BC_THETA_M(BCV,M)
            ENDDO
            IF(ROP_S(IJK,MMAX) > ZERO) THEN
               THETA_M(IJK,MMAX) = THETA_M(IJK,MMAX) / nTOT
            ENDIF
         ENDIF  ! end if (trim(kt_type) ='ghd')

! Set MMS BCs when MI boundary condition is used.
         IF (USE_MMS) THEN
            P_G(IJK) = SCALE_PRESSURE(MMS_P_G(IJK))
            EP_G(IJK) = MMS_EP_G(IJK)
            T_G(IJK) = MMS_T_G(IJK)

            DO M = 1, SMAX
               ROP_S(IJK,M) = MMS_ROP_S(IJK)
               T_S(IJK,M) = MMS_T_S(IJK)
               THETA_M(IJK,M) = MMS_THETA_M(IJK)
            ENDDO
         ENDIF ! end if(USE_MMS)

      ENDDO   ! do i
      ENDDO   ! do j
      ENDDO   ! do k

      RETURN
      END SUBROUTINE SET_BC0_INFLOW


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: set_bc0_ep                                              C
!  Purposes: Generally, outflow boundaries should not need bc_ep_g/    C
!  bc_rop_s specified at the boundary as these should come from        C
!  upstream flow conditions. However, the check_data_20 routine does   C
!  not discriminate for cell types and checks that ep_g and rop_s are  C
!  defined for all non-wall cells. So, this routine sets ep_g and/or   C
!  rop_s in outflow boundary cells according to the neighbor fluid     C
!  cell when bc_ep_g and/or bc_rop_s are not defined..                 C
!                                                                      C
!  MO, O, PO, and PI boundaries may or may not have these quantities   C
!  set. Undefined values are effectively overcome by a call to         C
!  set_bc1 but that call is done later. By doing this earlier (here),  C
!  then calls to set_ro_g and set_ro_s may also be done earlier.       C
!  Note that these routines loop over all flow/fluid cells including   C
!  flow boundary cells; they require values of the field variables     C
!  ep_g/rop_s in those cells. Otherwise, set_ro routines cannot be     C
!  safely called until after set_bc1 wherein the the boundary values   C
!  are initialized according to their neighbor fluid values..          C
!                                                                      C
!  Note this does not overcome the problem of a usr_ros function       C
!  call wherein bc_rop_s will not be defined nor will rop_s yet be     C
!  defined                                                             C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SET_BC0_EP(BCV)

      IMPLICIT NONE

! Dummy arguments
!--------------------------------------------------------------------//
! index for boundary condition
      INTEGER, INTENT(IN) :: BCV

! Local variables
!--------------------------------------------------------------------//
! indices
      INTEGER :: I, J, K, IJK, M
! ijk index for setting normal component of velocity
      INTEGER :: FIJK
!--------------------------------------------------------------------//

      DO K = BC_K_B(BCV), BC_K_T(BCV)
      DO J = BC_J_S(BCV), BC_J_N(BCV)
      DO I = BC_I_W(BCV), BC_I_E(BCV)
         IF (.NOT.IS_ON_myPE_plus2layers(I,J,K)) CYCLE
         IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
         IJK = FUNIJK(I,J,K)

         SELECT CASE (TRIM(BC_PLANE(BCV)))
            CASE ('W')
               FIJK = IM_OF(IJK)
            CASE('E')
               FIJK = IP_OF(IJK)
            CASE('S')
               FIJK = JM_OF(IJK)
            CASE('N')
               FIJK = JP_OF(IJK)
            CASE('B')
               FIJK = KM_OF(IJK)
            CASE('T')
               FIJK = KP_OF(IJK)
            CASE DEFAULT
         END SELECT

         IF (BC_EP_G(BCV) == UNDEFINED) EP_G(IJK) = EP_G(FIJK)

         DO M = 1, SMAX
            IF (BC_ROP_S(BCV,M) == UNDEFINED) ROP_S(IJK,M) = ROP_S(FIJK,M)
         ENDDO

      ENDDO   ! do i
      ENDDO   ! do j
      ENDDO   ! do k

      RETURN
      END SUBROUTINE SET_BC0_EP


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: SET_IJK_P_G                                             !
!  Purpose: Pick an appropriate control volume to specify Ppg.         !
!                                                                      !
!  Author: J. Musser                                  Date: 07-Nov-13  !
!  Reviewer:                                          Date:            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE SET_IJK_P_G

      implicit none
!--------------------------------------------------------------------//
      INTEGER :: BCV

      CHARACTER(len=7) :: Map
      CHARACTER(len=128) :: lMsg

      INTEGER :: l3
      INTEGER :: l2, u2
      INTEGER :: l1, u1

      INTEGER :: iErr

!--------------------------------------------------------------------//

! Initialize.
      iErr = 0
      IJK_P_G = UNDEFINED_I

! This is not needed for compressible cases.
      IF(RO_G0 == UNDEFINED) THEN
         write(ERR_MSG,"(3x,A)") 'Compressible: IJK_P_g remaining undefined.'
         call log_info()
         return
      ELSEIF(RO_G0 == 0.0d0) THEN
         write(ERR_MSG,"(3x,A)") 'No gas phase: IJK_P_g remaining undefined.'
         call log_info()
         return
      ENDIF

! If there are no cyclic boundaries, look for a pressure outflow.
      lpBCV: DO BCV = 1, DIMENSION_BC
         IF (.NOT.BC_DEFINED(BCV)) cycle lpBCV
         IF (BC_TYPE_ENUM(BCV) == P_OUTFLOW .OR. &
             BC_TYPE_ENUM(BCV) == P_INFLOW) THEN
            write(ERR_MSG,"(3x,A)") 'Outflow PC defined: IJK_P_g remaining undefined.'
            call log_info()
            RETURN
         ENDIF
      ENDDO lpBCV

! Initialize.
         l3 = UNDEFINED_I
         l2 = UNDEFINED_I; u2=l2
         l1 = UNDEFINED_I; u1=l1

! If there are cyclic boundaries, flag a cell along the positive
! domain extreme in the cyclic direction (e.g., JMAX1).
      IF(CYCLIC_Y .OR. CYCLIC_Y_PD .OR. CYCLIC_Y_MF) THEN

         Map = 'JKI_MAP'
         l3 = JMAX1
         l2 = KMIN1;  u2 = KMAX1
         l1 = IMIN1;  u1 = IMAX1
         lMsg='Cyclic in Y'

      ELSEIF(CYCLIC_X .OR. CYCLIC_X_PD .OR. CYCLIC_X_MF) THEN

         Map = 'IKJ_MAP'
         l3 = IMAX1
         l2 = KMIN1;  u2 = KMAX1
         l1 = JMIN1;  u1 = JMAX1
         lMsg='Cyclic in X'

      ELSEIF(CYCLIC_Z .OR. CYCLIC_Z_PD .OR. CYCLIC_Z_MF) THEN

         Map = 'KIJ_MAP'
         l3 = KMAX1
         l2 = IMIN1;  u2 = IMAX1
         l1 = JMIN1;  u1 = JMAX1
         lMsg='Cyclic in Z'

      ENDIF

! No cyclic boundaries or pressure outflows. The IJ plane is used in
! this case to maximize search region for 2D problems.
      IF(l3 == UNDEFINED_I) THEN
         Map = 'KIJ_MAP'
         l3 = merge(max((KMAX1-KMIN1)/2+1,2), KMIN1, do_K)
         l2 = IMIN1;  u2 = IMAX1
         l1 = JMIN1;  u1 = JMAX1
         lMsg='Center of domain'
      ENDIF

! Debugging messages.
      IF(DMP_LOG) THEN
         write(ERR_MSG,"(3/,3x,'Map: ',A)") Map
         write(ERR_MSG,"(/5x,'l3:',2x,I4)") l3
         write(ERR_MSG,"( 5x,'l2:',2(2x,I4))") l2, u2
         write(ERR_MSG,"( 5x,'l1:',2(2x,I4))") l1, u1
         write(ERR_MSG,"( 5x,'Msg: ',A)") trim(lMsg)
         call log_info()
      ENDIF

! Invoke the search routine.
      CALL IJK_Pg_SEARCH(l3, l2, u2, l1, u1, MAP, DMP_LOG, iErr)

      IF(iErr == 0) RETURN

! Error management.
      IF(DMP_LOG) THEN
         SELECT CASE (iErr)
         CASE ( 1001);  WRITE(err_msg, 1001); WRITE(*,1001)
         CASE ( 2000);  WRITE(err_msg, 2000); WRITE(*,2000)
         CASE ( 2001);  WRITE(err_msg, 2001); WRITE(*,2001)
         CASE ( 2002);  WRITE(err_msg, 2002); WRITE(*,2002)
         CASE DEFAULT
            WRITE(err_msg, 1000) iErr
         END SELECT

         WRITE(err_msg, 9000) MAP(1:1), l3, MAP(2:2),                 &
            l2, u2, MAP(3:3), l1, u1

         WRITE(err_msg, 9999)

      ENDIF

      call log_error()

 1000 FORMAT(//1X,/' From: SET_IJK_Pg',/,                       &
         ' Error 1000: Unknown error reported. x', I4.4)

 1001 FORMAT(//1X,/' From: SET_IJK_Pg',/,                       &
         ' Error 1001: Invalid mapping function.')

 2000 FORMAT(//1X,/' From: SET_IJK_Pg > IJK_Pg_SEARCH',/,       &
         ' Error 2000: Unknown error reported from IJK_Pg_SEARCH.')

 2001 FORMAT(//1X,/' From: SET_IJK_Pg > IJK_Pg_SEARCH',/,       &
         ' Error 2001: Unable to locate fluid cell in search region.')

 2002 FORMAT(//1X,/' From: SET_IJK_Pg > IJK_Pg_SEARCH',/,       &
         ' Error 2002: Unable to locate fluid cell owner.')

 9000 FORMAT(/' Search plane information:',/,3x,A1,': ',I8,            &
          2(/3x,A1,': ',I8,' x ',I8))

 9999 FORMAT(/' Fatal Error --> Invoking MFIX_EXIT',/1x,2/)

      END SUBROUTINE SET_IJK_P_G

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Author: J. Musser                                  Date: 07-Nov-13  !
!  Reviewer:                                          Date:            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE IJK_Pg_SEARCH(ll3, ll2, lu2, ll1, lu1, lMAP,          &
         ldFlag, iErr)

      implicit none

! Dummy arguments
!--------------------------------------------------------------------//
      INTEGER, INTENT(IN)  :: ll3
      INTEGER, INTENT(IN)  :: ll2, lu2
      INTEGER, INTENT(IN)  :: ll1, lu1
      LOGICAL, INTENT(IN) :: ldFlag
      INTEGER, INTENT(OUT)  :: iErr
      CHARACTER(len=*), INTENT(IN) :: lMAP

! Local variables
!--------------------------------------------------------------------//
      INTEGER :: lc2, lS2, lE2
      INTEGER :: lc1, lS1, lE1
      INTEGER :: I, J, K, IJK
      LOGICAL :: recheck
      INTEGER :: IJK_Pg_Owner, proc
      INTEGER :: gIJK(0:numPEs-1)
      INTEGER :: I_J_K_Pg(3)
      INTEGER :: lpCnt

      CHARACTER(len=32) :: cInt

!--------------------------------------------------------------------//

! Initialize Error Flag
      iErr = 2000

! Initialize the Owner ID
      IJK_Pg_Owner = UNDEFINED_I

! Set the initial search region, a single cell.
      lS1 = ll1 + (lu1-ll1)/2 + 1; lE1 = lS1
      lS2 = ll2 + (lu2-ll2)/2 + 1; lE2 = lS2

      lpCnt = 1
      recheck = .TRUE.
      do while(recheck)

! Initialize the global IJK array to zero. Resetting this array inside
! this do-loop is most likely overkill. This loop should only cycle
! if gIJK is zero.
         gIJK = 0

! Report debugging information for the search region.
         if(ldFlag) then
            write(*,"(/5x,'Pass: ',I4)") lpCnt
            write(*,"( 5x,'lp2 bounds:',2(2x,I4))")lS2, lE2
            write(*,"( 5x,'lp1 bounds:',2(2x,I4))")lS1, lE1
         endif

         lp2: do lc2 = lS2, lE2
         lp1: do lc1 = lS1, lE1
! Map the loop counters to I/J/K indices.
            SELECT CASE (lMap)
            CASE ('JKI_MAP')
               I=lc1; J=ll3; K=lc2
            CASE ('IKJ_MAP')
               I=ll3; J=lc1; K=lc2
            CASE ('KIJ_MAP')
               I=lc2; J=lc1; K=ll3
            CASE DEFAULT
               iErr = 1001
            END SELECT

! Only the rank that owns this I/J/K proceeds.
            if(.NOT.IS_ON_myPE_owns(I,J,K)) cycle
! Calculate the triple loop index.
            IJK = funijk(I,J,K)
! If there is fluid at this location, store the IJK and exit loops.
            if(fluid_at(IJK)) then
               gIJK(myPE) = IJK
               exit lp2
            endif
         enddo lp1
         enddo lp2

! Sync gIJK across all processes. Select the lowest ranked process that
! has gIJK defined. This choice is arbitrary and doesn't really matter.
! It just needs to be consistent.
         CALL global_all_sum(gIJK)
         proc_lp: do proc=0, numPEs-1
            if(gIJK(proc) /= 0) then
               IJK_P_g = gIJK(proc)
               IJK_Pg_Owner = proc
               recheck = .FALSE.
               exit proc_lp
            endif
         enddo proc_lp

! If the proceeding section did not find a fluid cell, expand the search
! area and try again.
         if(recheck) then
            if(lS1 > ll1 .OR. lE1 < lu1 .OR.                           &
               lS2 > ll2 .OR. lE2 < lu2) then
! Expand the 1-axis
               lS1 = max((lS1-1), ll1)
               lE1 = min((lE1+1), lu1)
! Expand the 2-axis
               lS2 = max((lS2-1), ll2)
               lE2 = min((lE2+1), lu2)
! The entire search plane was checked with no fluid cell identified.
! Force IJK_P_g to undefined for later error checking.
            else
               recheck = .FALSE.
               IJK_P_g = UNDEFINED_I
            endif
         endif
      enddo

! Verify that one fluid cell was detected. Otherwise flag the possible
! errors and return.
      if(IJK_P_G == UNDEFINED_I) then
         iErr = 2001
         return
      elseif(IJK_Pg_Owner == UNDEFINED_I) then
         iErr = 2002
         return
      endif


! The owner if the IJK_Pg gets the global I/J/K values and sends
! them to all ranks.
      if(myPE == IJK_Pg_Owner) then
         I_J_K_Pg(1) = I_OF(IJK_P_G)
         I_J_K_Pg(2) = J_OF(IJK_P_G)
         I_J_K_Pg(3) = K_OF(IJK_P_G)
      endif
      CALL BCAST(I_J_K_Pg, IJK_Pg_Owner)

      I = I_J_K_Pg(1)
      J = I_J_K_Pg(2)
      K = I_J_K_Pg(3)

! If debugging, have PE_IO report some information before the
! data is overwritten.
      if(ldFlag) then
         write(*,"(/3x,'IJK_P_g successfully identified!')")
         cInt=''; write(cInt,*) IJK_Pg_Owner
         write(*,"( 5x,'Owner Rank: ',A)")trim(adjustl(cInt))
         cInt=''; write(cInt,*) IJK_P_G
         write(*,"(5x, 'IJK: ',A)", advance='no') trim(adjustl(cInt))
         write(*,"(' :: ')", advance='no')
         cInt=''; write(cInt,*) I
         write(*,"('(',A)",advance='no') trim(adjustl(cInt))
         cInt=''; write(cInt,*) J
         write(*,"(',',A)",advance='no') trim(adjustl(cInt))
         cInt=''; write(cInt,*) K
         write(*,"(',',A,')',2/)") trim(adjustl(cInt))
      endif

! Ranks that 'see' IJK_P_g store their local IJK value. Everyone else
! resets IJK_P_g to UNDEFINED_I. This removes the need for getting
! I/J/K values later on in source_PPg.
!      IJK_P_g = merge(funijk(I,J,K), UNDEFINED_I,                      &
!         IS_ON_myPE_plus2layers(I,J,K))

      IF(IS_ON_myPE_plus2layers(I,J,K)) THEN
         IJK_P_g = funijk(I,J,K)
      ELSE
         IJK_P_g = UNDEFINED_I
      ENDIF

      IERR = 0
      RETURN
      END SUBROUTINE IJK_Pg_SEARCH
END MODULE SET_BC0_MOD
