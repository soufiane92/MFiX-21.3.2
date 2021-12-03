#include "error.inc"

MODULE SET_BC0_FLOW_MOD

   use bc, only: bc_defined, bc_type_enum
   use bc, only: p_outflow, mass_outflow, outflow, mass_inflow, p_inflow
   use bc, only: cg_nsw
   use error_manager

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: set_bc0_flow                                            C
!  Purpose: This subroutine does additional initial setting of flow    C
!  boundary conditions (velocities).  The user specifications of the   C
!  boundary conditions are checked for veracity in various check_data  C
!  routines: (e.g., check_boundary_conditions).                        C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
SUBROUTINE SET_BC0_FLOW

! Modules
!--------------------------------------------------------------------//
! Total number of (actual) continuum solids.
      use physprop, only: SMAX
! Total number of discrete solids.
      use discretelement, only: DES_MMAX
! Maximum number of disperse phases
      use param, only: DIM_M

      use param, only: dimension_bc
      use param1, only: undefined_i
      use param1, only: zero, one, undefined

! User specified BC solids bulk density
      use bc, only: BC_ROP_s
! Solids volume fraction at BC
      use bc, only: BC_EP_s
      use bc, only: BC_EP_g
      use bc, only: cg_mi_converted_to_ps

      use cutcell, only: cartesian_grid

!
      use check_bc0_flow_mod, only: check_bc_vel_inflow
      use check_bc0_flow_mod, only: check_bc_vel_outflow
      use flow_to_vel_mod, only: flow_to_vel
      use flow_to_vel_mod, only: convert_cg_mi_to_ps

      use mpi_utility
      use sendrecv
      IMPLICIT NONE

! Local variables
!--------------------------------------------------------------------//
! Local index for boundary condition
      INTEGER ::  L

! Total number of solids phases (continuum + discrete)
      INTEGER :: MMAX_TOT
! Flag to skip checks on indexed solid phase.
      LOGICAL :: SKIP(1:DIM_M)
! Logical on whether to perform a check on the direction (+/-; in/out)
! of any specified bc velocity for the given bc plane
      LOGICAL :: CHECK_VEL
!......................................................................!

! Total number of solids.
      MMAX_TOT = SMAX + DES_MMAX

      CHECK_VEL = .TRUE.
      IF (CARTESIAN_GRID) CHECK_VEL = .FALSE.

      DO L = 1, DIMENSION_BC
         IF (BC_DEFINED(L)) THEN

! Determine which solids phases are present.
         SKIP=(BC_ROP_S(L,:)==UNDEFINED.OR.BC_ROP_S(L,:)==ZERO) &
            .AND.(BC_EP_S(L,:)==UNDEFINED.OR.BC_EP_S(L,:)==ZERO)

         IF(MMAX_TOT == 1 .AND. BC_EP_g(L)/=ONE) SKIP(1) = .FALSE.

            SELECT CASE (BC_TYPE_ENUM(L))

! it appears set_bc0_vel_inflow=set_bc0_vel_outflow
            CASE (P_OUTFLOW)
               CALL SET_BC0_DT_CALCS(L)
               CALL SET_BC0_OUTFLOWB(L)

            CASE (MASS_OUTFLOW)
               CALL FLOW_TO_VEL(CHECK_VEL, MMAX_TOT, SKIP, L)
               CALL CHECK_BC_VEL_OUTFLOW(MMAX_TOT, SKIP, L)
               CALL SET_BC0_VEL_OUTFLOW(L)
               CALL SET_BC0_OUTFLOWB(L)

            CASE (OUTFLOW)
               CALL SET_BC0_DT_CALCS(L)
               CALL SET_BC0_OUTFLOWB(L)

            CASE (MASS_INFLOW)
               CALL FLOW_TO_VEL(CHECK_VEL, MMAX_TOT, SKIP, L)
               CALL CHECK_BC_VEL_INFLOW(MMAX_TOT, SKIP, L)

               CALL SET_BC0_JET(L)
               CALL SET_BC0_VEL_INFLOW(L)

            CASE (P_INFLOW)
               CALL SET_BC0_VEL_INFLOW(L)

            CASE (CG_NSW)
               IF (CG_MI_CONVERTED_TO_PS(L)) THEN
                  CALL CONVERT_CG_MI_TO_PS
               ENDIF
            END SELECT
         ENDIF
      ENDDO

      RETURN
      END SUBROUTINE SET_BC0_FLOW


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: set_bc0_vel_outflow                                     C
!  Purpose: Additional quantities need to be defined for mass outflow  C
!  (MO) boundaries compared to other outflow boundaries. Specifically, C
!  velocities need to be set. These velocities may have been           C
!  calculated from specified mass or volumetric flow rates.            C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SET_BC0_VEL_OUTFLOW(BCV)

! Modules
!--------------------------------------------------------------------//
      use bc, only: bc_plane
      use bc, only: bc_k_b, bc_k_t
      use bc, only: bc_j_s, bc_j_n
      use bc, only: bc_i_w, bc_i_e
      use bc, only: bc_u_g, bc_v_g, bc_w_g
      use bc, only: bc_u_s, bc_v_s, bc_w_s

      use fldvar, only: u_g, v_g, w_g
      use fldvar, only: u_s, v_s, w_s
      use physprop, only: smax

      use mms, only: use_mms
      use mms, only: mms_u_g, mms_v_g, mms_w_g
      use mms, only: mms_u_s, mms_v_s, mms_w_s

! following needed for ghd adjustments to mixture phase
      use bc, only: bc_rop_s
      use fldvar, only: rop_s
      use param1, only: zero, undefined
      use physprop, only: mmax
      use run, only: kt_type_enum, ghd_2007

      use indices, only: im1, jm1, km1
      use functions, only: is_on_mype_plus2layers, bound_funijk
      use compar, only: dead_cell_at
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
! calculation for normal component of mixture velocity
      DOUBLE PRECISION :: lvel_s
!--------------------------------------------------------------------//

      DO K = BC_K_B(BCV), BC_K_T(BCV)
      DO J = BC_J_S(BCV), BC_J_N(BCV)
      DO I = BC_I_W(BCV), BC_I_E(BCV)
         IF (.NOT.IS_ON_myPE_plus2layers(I,J,K)) CYCLE
         IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
         IJK = BOUND_FUNIJK(I,J,K)

         U_G(IJK) = BC_U_G(BCV)
         V_G(IJK) = BC_V_G(BCV)
         W_G(IJK) = BC_W_G(BCV)
         IF (SMAX > 0) THEN
            U_S(IJK,:SMAX) = BC_U_S(BCV,:SMAX)
            V_S(IJK,:SMAX) = BC_V_S(BCV,:SMAX)
            W_S(IJK,:SMAX) = BC_W_S(BCV,:SMAX)
         ENDIF

! When the boundary plane is located on the E, N, T side of the domain
! (fluid cell is located w, s, b), set the component of velocity normal
! to the boundary plane of the adjacent fluid cell
         SELECT CASE (TRIM(BC_PLANE(BCV)))
            CASE ('W')
               FIJK = BOUND_FUNIJK(IM1(I),J,K)
               U_G(FIJK) = BC_U_G(BCV)
               IF (SMAX >0) U_S(FIJK,:SMAX) = BC_U_S(BCV,:SMAX)
            CASE ('S')
               FIJK = BOUND_FUNIJK(I,JM1(J),K)
               V_G(FIJK) = BC_V_G(BCV)
               IF(SMAX>0) V_S(FIJK,:SMAX) = BC_V_S(BCV,:SMAX)
            CASE ('B')
               FIJK = BOUND_FUNIJK(I,J,KM1(K))
               W_G(FIJK) = BC_W_G(BCV)
               IF (SMAX>0) W_S(FIJK,:SMAX) = BC_W_S(BCV,:SMAX)
         END SELECT

! for GHD theory to compute mixture BC of velocity and density
         IF(KT_TYPE_ENUM == GHD_2007) THEN
            ROP_S(IJK,MMAX) = ZERO
            U_S(IJK,MMAX) =  ZERO
            V_S(IJK,MMAX) =  ZERO
            W_S(IJK,MMAX) =  ZERO
            lvel_s = zero
! accumulate mixture phase contributions
            DO M = 1, SMAX
               IF (BC_ROP_S(BCV,M) /= UNDEFINED) THEN
                  ROP_S(IJK,MMAX) = ROP_S(IJK,MMAX) + &
                     BC_ROP_S(BCV,M)
! note if a udf for density is requested then this section will be
! skipped for that phase as bc_rop_s is not permitted...
                  U_S(IJK,MMAX) = U_S(IJK,MMAX) + &
                     BC_ROP_S(BCV,M)*BC_U_S(BCV,M)
                  V_S(IJK,MMAX) = V_S(IJK,MMAX) + &
                     BC_ROP_S(BCV,M)*BC_V_S(BCV,M)
                  W_S(IJK,MMAX) = W_S(IJK,MMAX) + &
                     BC_ROP_S(BCV,M)*BC_W_S(BCV,M)
! set velocity component normal to plane in adjacent fluid cell
                  SELECT CASE (TRIM(BC_PLANE(BCV)))
                     CASE ('W')
                        lvel_s = lvel_s + BC_ROP_S(BCV,M)*BC_U_S(BCV,M)
                     CASE ('S')
                        lvel_s = lvel_s + BC_ROP_S(BCV,M)*BC_V_S(BCV,M)
                     CASE ('B')
                        lvel_s = lvel_s + BC_ROP_S(BCV,M)*BC_W_S(BCV,M)
                  END SELECT
               ENDIF
            ENDDO
! evaluate and assign mixture phase
            IF(ROP_S(IJK,MMAX) > ZERO) THEN
               U_S(IJK,MMAX) = U_S(IJK,MMAX) / ROP_S(IJK,MMAX)
               V_S(IJK,MMAX) = V_S(IJK,MMAX) / ROP_S(IJK,MMAX)
               W_S(IJK,MMAX) = W_S(IJK,MMAX) / ROP_S(IJK,MMAX)
               lvel_s = lvel_s/ROP_S(IJK,MMAX)
            ENDIF
            SELECT CASE (TRIM(BC_PLANE(BCV)))
               CASE ('W')
                 FIJK = BOUND_FUNIJK(IM1(I),J,K)
                 U_S(FIJK,MMAX) = lvel_s
               CASE ('S')
                 FIJK = BOUND_FUNIJK(I,JM1(J),K)
                 V_S(FIJK,MMAX) = lvel_s
               CASE ('B')
                 FIJK = BOUND_FUNIJK(I,J,KM1(K))
                 W_S(FIJK,MMAX) = lvel_s
            END SELECT

         ENDIF  ! end if (trim(kt_type) ='ghd')

! Set MMS BCs when MI boundary condition is used.
         IF (USE_MMS) THEN
            U_G(IJK) = MMS_U_G(IJK)
            V_G(IJK) = MMS_V_G(IJK)
            W_G(IJK) = MMS_W_G(IJK)
            IF (SMAX > 0) THEN
               U_S(IJK,:SMAX) = MMS_U_S(IJK)
               V_S(IJK,:SMAX) = MMS_V_S(IJK)
               W_S(IJK,:SMAX) = MMS_W_S(IJK)
            ENDIF
! When the boundary plane is W, S, or B the normal component of velocity
! needs to be set for both sides of the boundary cell.
            SELECT CASE (TRIM(BC_PLANE(BCV)))
               CASE ('W')
                 FIJK = BOUND_FUNIJK(IM1(I),J,K)
                 U_G(FIJK) = MMS_U_G(FIJK)
                 IF(SMAX>0) U_S(FIJK,:SMAX) = MMS_U_S(FIJK)
               CASE ('S')
                 FIJK = BOUND_FUNIJK(I,JM1(J),K)
                 V_G(FIJK) = MMS_V_G(FIJK)
                 IF(SMAX>0) V_S(FIJK,:SMAX) = MMS_V_S(FIJK)
               CASE ('B')
                 FIJK = BOUND_FUNIJK(I,J,KM1(K))
                 W_G(FIJK) = MMS_W_G(FIJK)
                 IF(SMAX>0) W_S(FIJK,:SMAX) = MMS_W_S(FIJK)
            END SELECT
         ENDIF ! end if(USE_MMS)

      ENDDO   ! do i
      ENDDO   ! do j
      ENDDO   ! do k

      RETURN
      END SUBROUTINE SET_BC0_VEL_OUTFLOW


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: set_bc0_vel_inflow                                      C
!  Purpose: Set the initial settings of the boundary conditions        C
!  pressure inflow (PI) and. mass inflow (MI) boundary types.          C
!                                                                      C
!  Comments: Unlike the treament of PO, MO, O boundary types, for      C
!  these boundary types (PI/MI) no checks are made to determine        C
!  whether the given BC value is defined before it is assigned to the  C
!  field variable. However, the corresponding check routines generally C
!  ensure such BC quantities are defined for MI or PI boundaries if    C
!  they are needed for the simulation.                                 C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SET_BC0_VEL_INFLOW(BCV)

! Modules
!--------------------------------------------------------------------//
      use bc, only: bc_plane
      use bc, only: bc_k_b, bc_k_t
      use bc, only: bc_j_s, bc_j_n
      use bc, only: bc_i_w, bc_i_e
      use bc, only: bc_u_g, bc_v_g, bc_w_g
      use bc, only: bc_u_s, bc_v_s, bc_w_s

      use fldvar, only: u_g, v_g, w_g
      use fldvar, only: u_s, v_s, w_s
      use physprop, only: smax

      use mms, only: use_mms
      use mms, only: mms_u_g, mms_v_g, mms_w_g
      use mms, only: mms_u_s, mms_v_s, mms_w_s

! following needed for ghd adjustments to mixture phase
      use bc, only: bc_rop_s
      use fldvar, only: rop_s
      use param1, only: zero, undefined
      use physprop, only: mmax
      use run, only: kt_type_enum, ghd_2007

      use indices, only: im1, jm1, km1
      use functions, only: is_on_mype_plus2layers, bound_funijk
      use compar, only: dead_cell_at
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
! calculation for normal component of mixture velocity
      DOUBLE PRECISION :: lvel_s
!--------------------------------------------------------------------//

      DO K = BC_K_B(BCV), BC_K_T(BCV)
      DO J = BC_J_S(BCV), BC_J_N(BCV)
      DO I = BC_I_W(BCV), BC_I_E(BCV)
         IF (.NOT.IS_ON_myPE_plus2layers(I,J,K)) CYCLE
         IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
         IJK = BOUND_FUNIJK(I,J,K)

         U_G(IJK) = BC_U_G(BCV)
         V_G(IJK) = BC_V_G(BCV)
         W_G(IJK) = BC_W_G(BCV)
         IF (SMAX > 0) THEN
            U_S(IJK,:SMAX) = BC_U_S(BCV,:SMAX)
            V_S(IJK,:SMAX) = BC_V_S(BCV,:SMAX)
            W_S(IJK,:SMAX) = BC_W_S(BCV,:SMAX)
         ENDIF

! When the boundary plane is located on the E, N, T side of the domain
! (fluid cell is located w,s, b), set the component of velocity normal
! to the boundary plane of the adjacent fluid cell
         SELECT CASE (TRIM(BC_PLANE(BCV)))
            CASE ('W')
               FIJK = BOUND_FUNIJK(IM1(I),J,K)
               U_G(FIJK) = BC_U_G(BCV)
               IF (SMAX >0) U_S(FIJK,:SMAX) = BC_U_S(BCV,:SMAX)
            CASE ('S')
               FIJK = BOUND_FUNIJK(I,JM1(J),K)
               V_G(FIJK) = BC_V_G(BCV)
               IF(SMAX>0) V_S(FIJK,:SMAX) = BC_V_S(BCV,:SMAX)
            CASE ('B')
               FIJK = BOUND_FUNIJK(I,J,KM1(K))
               W_G(FIJK) = BC_W_G(BCV)
               IF (SMAX>0) W_S(FIJK,:SMAX) = BC_W_S(BCV,:SMAX)
         END SELECT

! for GHD theory to compute mixture BC of velocity and density
         IF(KT_TYPE_ENUM == GHD_2007) THEN
            ROP_S(IJK,MMAX) = ZERO
            U_S(IJK,MMAX) =  ZERO
            V_S(IJK,MMAX) =  ZERO
            W_S(IJK,MMAX) =  ZERO
            lvel_s = zero
            DO M = 1, SMAX
               IF (BC_ROP_S(BCV,M) /= UNDEFINED) THEN
                  ROP_S(IJK,MMAX) = ROP_S(IJK,MMAX) + &
                     BC_ROP_S(BCV,M)
! note if a udf for density is requested then this section will be
! skipped for that phase as bc_rop_s is not permitted...
                  U_S(IJK,MMAX) = U_S(IJK,MMAX) + &
                     BC_ROP_S(BCV,M)*BC_U_S(BCV,M)
                  V_S(IJK,MMAX) = V_S(IJK,MMAX) + &
                     BC_ROP_S(BCV,M)*BC_V_S(BCV,M)
                  W_S(IJK,MMAX) = W_S(IJK,MMAX) + &
                     BC_ROP_S(BCV,M)*BC_W_S(BCV,M)
! set velocity component normal to plane in adjacent fluid cell
                  SELECT CASE (TRIM(BC_PLANE(BCV)))
                     CASE ('W')
                        lvel_s = lvel_s + BC_ROP_S(BCV,M)*BC_U_S(BCV,M)
                     CASE ('S')
                        lvel_s = lvel_s + BC_ROP_S(BCV,M)*BC_V_S(BCV,M)
                     CASE ('B')
                        lvel_s = lvel_s + BC_ROP_S(BCV,M)*BC_W_S(BCV,M)
                  END SELECT
               ENDIF
            ENDDO
            IF(ROP_S(IJK,MMAX) > ZERO) THEN
               U_S(IJK,MMAX) = U_S(IJK,MMAX) / ROP_S(IJK,MMAX)
               V_S(IJK,MMAX) = V_S(IJK,MMAX) / ROP_S(IJK,MMAX)
               W_S(IJK,MMAX) = W_S(IJK,MMAX) / ROP_S(IJK,MMAX)
               lvel_s = lvel_s/ROP_S(IJK,MMAX)
            ENDIF
            SELECT CASE (TRIM(BC_PLANE(BCV)))
               CASE ('W')
                 FIJK = BOUND_FUNIJK(IM1(I),J,K)
                 U_S(FIJK,MMAX) = lvel_s
               CASE ('S')
                 FIJK = BOUND_FUNIJK(I,JM1(J),K)
                 V_S(FIJK,MMAX) = lvel_s
               CASE ('B')
                 FIJK = BOUND_FUNIJK(I,J,KM1(K))
                 W_S(FIJK,MMAX) = lvel_s
            END SELECT

         ENDIF  ! end if (trim(kt_type) ='ghd')

! Set MMS BCs when MI boundary condition is used.
         IF (USE_MMS) THEN
            U_G(IJK) = MMS_U_G(IJK)
            V_G(IJK) = MMS_V_G(IJK)
            W_G(IJK) = MMS_W_G(IJK)
            IF (SMAX > 0) THEN
               U_S(IJK,:SMAX) = MMS_U_S(IJK)
               V_S(IJK,:SMAX) = MMS_V_S(IJK)
               W_S(IJK,:SMAX) = MMS_W_S(IJK)
            ENDIF
! When the boundary plane is W, S, or B the normal component of velocity
! needs to be set for both sides of the boundary cell.
            SELECT CASE (TRIM(BC_PLANE(BCV)))
               CASE ('W')
                 FIJK = BOUND_FUNIJK(IM1(I),J,K)
                 U_G(FIJK) = MMS_U_G(FIJK)
                 IF(SMAX>0) U_S(FIJK,:SMAX) = MMS_U_S(FIJK)
               CASE ('S')
                 FIJK = BOUND_FUNIJK(I,JM1(J),K)
                 V_G(FIJK) = MMS_V_G(FIJK)
                 IF(SMAX>0) V_S(FIJK,:SMAX) = MMS_V_S(FIJK)
               CASE ('B')
                 FIJK = BOUND_FUNIJK(I,J,KM1(K))
                 W_G(FIJK) = MMS_W_G(FIJK)
                 IF(SMAX>0) W_S(FIJK,:SMAX) = MMS_W_S(FIJK)
            END SELECT
         ENDIF ! end if(USE_MMS)

      ENDDO   ! do i
      ENDDO   ! do j
      ENDDO   ! do k

      RETURN
      END SUBROUTINE SET_BC0_VEL_INFLOW


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      C
!  Subroutine: set_bc0_outflowB                                        C
!  Purpose: Provide a mechanism to initialize certain field variables  C
!  in an outflow boundary cell to the value of the NEIGHBORING FLUID   C
!  CELL.                                                               C
!                                                                      C
!  It is not clear to what extent initialization is necessary as the   C
!  governing matrix equation for each of the field variables is set    C
!  such that the bc cells are not actually solved but are set to the   C
!  value of the adjacent fluid cell(see bc_phi). However, there are    C
!  some instances in the code where the bc cells are referenced        C
!  before the code has called the governing equation routine that set  C
!  such cell values.                                                   C
!                                                                      C
!  This subroutine illustrates how these particular variables may not  C
!  need (or use) bc values defined by the user (i.e., implying         C
!  much of the set_bc0_outflowa routine is unnecessary except for      C
!  calculating initial densities...?)                                  C
!                                                                      C
!  Comments: this call replaces some initializations that had been     C
!  done by set_bc1                                                     C
!                                                                      C
!  WARNING: this routine only serves to initialize field variables     C
!  whose governing solver routine invokes bc_phi! do not insert any    C
!  other initializations here as it is likely not appropriate!!        C
!  An exception is made here to set P_g in a MO/O boundary to the      C
!  neighbor fluid cell if bc_p_g is undefined. Otherwise one has to    C
!  wait for set_bc1->set_outflow->set_outflow_misc for this to become  C
!  initialized.                                                        C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE set_bc0_outflowB(BCV)

! Modules
!--------------------------------------------------------------------//
      use bc, only: bc_plane
      use bc, only: bc_k_b, bc_k_t
      use bc, only: bc_j_s, bc_j_n
      use bc, only: bc_i_w, bc_i_e
      use bc, only: bc_p_g

      use fldvar, only: t_g, t_s, theta_m
      use fldvar, only: x_g, x_s, scalar
      use fldvar, only: p_g
      use fldvar, only: k_turb_g, e_turb_g

      use param1, only: undefined
      use physprop, only: smax, mmax
      use physprop, only: nmax
      use run, only: kt_type_enum, ghd_2007
      use scalars, only: nscalar
      use turb, only: k_epsilon

      use indices, only: im1, ip1, jm1, jp1, km1, kp1
      use functions, only: is_on_mype_plus2layers, bound_funijk
      use compar, only: dead_cell_at
      IMPLICIT NONE

! Dummy arguments
!--------------------------------------------------------------------//
! index of boundary
      INTEGER, INTENT(IN) :: BCV

! Local variables
!--------------------------------------------------------------------//
! indices of current boundary cell
      INTEGER :: IJK, I, J, K
! index of neighboring fluid cell
      INTEGER :: FIJK
! local indices
      INTEGER :: M
!--------------------------------------------------------------------//

      DO K = BC_K_B(BCV), BC_K_T(BCV)
      DO J = BC_J_S(BCV), BC_J_N(BCV)
      DO I = BC_I_W(BCV), BC_I_E(BCV)
         IF (.NOT.IS_ON_myPE_plus2layers(I,J,K)) CYCLE
         IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
         IJK = BOUND_FUNIJK(I,J,K)

         SELECT CASE (TRIM(BC_PLANE(BCV)))
            CASE ('W')   ! fluid cell at west
               FIJK = BOUND_FUNIJK(IM1(I),J,K)
            CASE ('E')
               FIJK = BOUND_FUNIJK(IP1(I),J,K)
            CASE ('S')   ! fluid cell at south
               FIJK = BOUND_FUNIJK(I,JM1(J),K)
            CASE ('N')
               FIJK = BOUND_FUNIJK(I,JP1(J),K)
            CASE ('B')   ! fluid cell at bottom
               FIJK = BOUND_FUNIJK(I,J,KM1(K))
            CASE ('T')
               FIJK = BOUND_FUNIJK(I,J,KP1(K))
         END SELECT

         T_G(IJK) = T_G(FIJK)
         IF (NMAX(0) > 0) &
            X_G(IJK,:NMAX(0)) = X_G(FIJK,:NMAX(0))

! Inserting this here gets around the awkwardness of waiting for
! set_bc1 to be called which is done after the call to set_ro_g.
         IF (BC_P_G(BCV) == UNDEFINED) THEN
            P_G(IJK) = P_G(FIJK)
         ENDIF

! setting scalar quantities
         IF (NScalar >0) &
            Scalar(IJK, :NScalar) = Scalar(FIJK, :NScalar)

! setting turbulence quantities
         IF(K_Epsilon) THEN
            K_Turb_G(IJK) = K_Turb_G(FIJK)
            E_Turb_G(IJK) = E_Turb_G(FIJK)
         ENDIF

! this should only loop of tfm phases .. hmm
         DO M = 1, SMAX
            T_S(IJK,M) = T_S(FIJK,M)
            THETA_M(IJK,M) =  THETA_M(FIJK,M)
            IF (NMAX(M) > 0) &
               X_S(IJK,M,:NMAX(M)) = X_S(FIJK,M,:NMAX(M))
         ENDDO

         IF(KT_TYPE_ENUM == GHD_2007) &
             THETA_M(IJK,MMAX) =  THETA_M(FIJK,MMAX)

      ENDDO
      ENDDO
      ENDDO
      RETURN
      END SUBROUTINE set_bc0_outflowB


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: set_bc0_jet                                             C
!  Purpose: initializing time dependent jet conditions                 C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SET_BC0_JET(BCV)

! Modules
!--------------------------------------------------------------------//
      use bc, only: bc_plane
      use bc, only: bc_jet_g, bc_jet_g0
      use bc, only: bc_dt_0, bc_time
      use bc, only: bc_u_g, bc_v_g, bc_w_g
      use param1, only: undefined
      use run, only: time
      IMPLICIT NONE

! Dummy arguments
!--------------------------------------------------------------------//
! index of boundary
      INTEGER, INTENT(IN) :: BCV
!--------------------------------------------------------------------//

      BC_JET_G(BCV) = UNDEFINED
      IF (BC_DT_0(BCV) /= UNDEFINED) THEN
         BC_TIME(BCV) = TIME + BC_DT_0(BCV)
         BC_JET_G(BCV) = BC_JET_G0(BCV)
         IF (BC_JET_G(BCV) /= UNDEFINED) THEN
            SELECT CASE (TRIM(BC_PLANE(BCV)))
            CASE ('W', 'E')
               BC_U_G(BCV) = BC_JET_G(BCV)
            CASE ('S', 'N')
               BC_V_G(BCV) = BC_JET_G(BCV)
            CASE ('B', 'T')
               BC_W_G(BCV) = BC_JET_G(BCV)
            END SELECT
         ENDIF
      ELSE
         BC_TIME(BCV) = UNDEFINED
      ENDIF
      RETURN
      END SUBROUTINE SET_BC0_JET


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: set_bc0_dt_calcs                                        C
!  Purpose: initializing time dependent outflow calculations for       C
!  modifying outflow conditions (MO type) or simple reporting          C
!  outflow conditions (PO or O types)                                  C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SET_BC0_DT_CALCS(BCV)

! Modules
!--------------------------------------------------------------------//
      use bc, only: bc_dt_0, bc_time
      use bc, only: bc_out_n
      use bc, only: bc_mout_g, bc_mout_s
      use bc, only: bc_vout_g, bc_vout_s
      use physprop, only: smax
      use param1, only: undefined, zero
      use run, only: time
      IMPLICIT NONE
! Dummy arguments
!--------------------------------------------------------------------//
! index of boundary
      INTEGER, INTENT(IN) :: BCV
!--------------------------------------------------------------------//

! initializing for time dependent outflow reporting calculation
      IF (BC_DT_0(BCV) /= UNDEFINED) THEN
         BC_TIME(BCV) = TIME + BC_DT_0(BCV)
         BC_OUT_N(BCV) = 0
         BC_MOUT_G(BCV) = ZERO
         BC_VOUT_G(BCV) = ZERO
         IF (SMAX > 0) THEN
            BC_MOUT_S(BCV,:SMAX) = ZERO
            BC_VOUT_S(BCV,:SMAX) = ZERO
         ENDIF
      ELSE
         BC_TIME(BCV) = UNDEFINED
      ENDIF
      RETURN
      END SUBROUTINE SET_BC0_DT_CALCS


END MODULE SET_BC0_FLOW_MOD
