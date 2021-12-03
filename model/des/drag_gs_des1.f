#include "error.inc"

MODULE DRAG_GS_DES1_MOD

   use compar, only: IJKStart3, IJKEnd3
   use cutcell, only: CUT_U_TREATMENT_AT, THETA_UE, THETA_UE_BAR
   use cutcell, only: CUT_V_TREATMENT_AT, THETA_VN, THETA_VN_BAR
   use cutcell, only: CUT_W_TREATMENT_AT, THETA_WT, THETA_WT_BAR
   use des_drag_gp_mod, only: des_drag_gp
   use discretelement, only: DES_EXPLICITLY_COUPLED
   use discretelement, only: DES_VEL_NEW, DRAG_BM, DRAG_FC
   use discretelement, only: DRAG_FC
   use discretelement, only: FC, F_GDS, F_GP, MAX_PIP, PIJK, PVOL, P_FORCE
   use error_manager
   use fldvar, only: U_G, V_G, W_G, EP_G
   use fun_avg, only: AVG_X_E, AVG_Y_N, AVG_Z_T
   use functions, only: FLUID_AT, IS_NORMAL
   use functions, only: IM_OF, JM_OF, KM_OF, FLUID_AT
   use geometry, only: DO_K, VOL
   use indices, only: I_OF
   use mfix_pic, only: DES_STAT_WT, MPPIC
   use param, only: dimension_3, dimension_3_alloc
   use param1, only: ZERO, ONE
   use particle_filter, only: FILTER_CELL, FILTER_WEIGHT, DES_INTERP_ON
   use run, only: MODEL_B
   use sendrecv, only: SEND_RECV
   use sendrecvnode, only: DES_COLLECT_gDATA

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CALC_GS_DES1                                            !
!  Author: J.Musser                                   Date: 21-NOV-14  !
!                                                                      !
!  Purpose: This routine is called from the DISCRETE side to calculate !
!  the gas-based forces acting on each particle using interpolated     !
!  values for gas velocity, gas pressure, and gas volume fraction.     !
!                                                                      !
!  Notes:                                                              !
!                                                                      !
!   F_gp is obtained from des_drag_gp subroutine is given as:          !
!    F_GP = beta*VOL_P/EP_s where VOL_P is the particle volume.        !
!                                                                      !
!  The drag force on each particle is equal to:                        !
!    D_FORCE = beta*VOL_P/EP_s*(Ug - Us) = F_GP *(Ug - Us)             !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE DRAG_GS_DES1

      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------//
!     Cell-center gas velocities.
      DOUBLE PRECISION :: UGC(DIMENSION_3)
      DOUBLE PRECISION :: VGC(DIMENSION_3)
      DOUBLE PRECISION :: WGC(DIMENSION_3)
! Loop counters: Particle, fluid cell, neighbor cells
      INTEGER :: NP, IJK, LC
! Interpolation weight
      DOUBLE PRECISION :: WEIGHT
! Interpolated gas phase quantities and particle velocity.
      DOUBLE PRECISION :: lEPg, VELFP(3), lPF(3), vel_p(3)
! Drag force acting on each particle.
      DOUBLE PRECISION :: D_FORCE(3)
! Flag for Model A momentum equation
      LOGICAL :: MODEL_A
! Loop bound for filter
      INTEGER :: LP_BND
!......................................................................!

! Set flag for Model A momentum equation.
      MODEL_A = .NOT.MODEL_B
! Loop bounds for interpolation.
      LP_BND = merge(27,9,DO_K)

! Calculate the cell center gas velocities.
      CALL CALC_CELL_CENTER_GAS_VEL(U_G, V_G, W_G, UGC, VGC, WGC)

! Calculate the gas phase forces acting on each particle.

!$omp parallel default(none) private(np,lepg,velfp,ijk,weight,lpf,d_force,vel_p)    &
!$omp          shared(max_pip,des_interp_on,lp_bnd,filter_cell,filter_weight, &
!$omp          ep_g,pijk,des_vel_new,f_gp,mppic,ugc,vgc,wgc,p_force,    &
!$omp          des_explicitly_coupled,drag_fc,fc,model_a,pvol)
!$omp do
      DO NP=1,MAX_PIP
         IF(.NOT.IS_NORMAL(NP)) CYCLE

         lEPG = ZERO
         VELFP = ZERO
         lPF = ZERO

! Calculate the gas volume fraction, velocity, and pressure force at
! the particle's position.
         IF(DES_INTERP_ON) THEN
            DO LC=1,LP_BND
               IJK = FILTER_CELL(LC,NP)
               WEIGHT = FILTER_WEIGHT(LC,NP)
! Gas phase volume fraction.
               lEPG = lEPG + EP_G(IJK)*WEIGHT
! Gas phase velocity.
               VELFP(1) = VELFP(1) + UGC(IJK)*WEIGHT
               VELFP(2) = VELFP(2) + VGC(IJK)*WEIGHT
               VELFP(3) = VELFP(3) + WGC(IJK)*WEIGHT
! Gas pressure force.
               lPF = lPF + P_FORCE(:,IJK)*WEIGHT
            ENDDO
         ELSE
            IJK = PIJK(NP,4)
            lEPG = EP_G(IJK)
            VELFP(1) = UGC(IJK)
            VELFP(2) = VGC(IJK)
            VELFP(3) = WGC(IJK)
            lPF = P_FORCE(:,IJK)
         ENDIF

! This avoids FP exceptions for some ghost particles.
! Using hard-coded tolerance. Comparing to ZERO led to overflow when lEPg is
! around 1D-15
         ! IF(lEPg == ZERO) lEPG = EP_g(PIJK(NP,4))
         IF(lEPg <1.0d-6) lEPG = EP_g(PIJK(NP,4))


! Avoid drag calculations in cells without fluid (cut-cell)
         IF(.NOT.FLUID_AT(PIJK(NP,4))) THEN
            DRAG_FC(NP,:) = 0.0d0
            F_GP(NP) = 0.0d0

! For explicit coupling, use the drag coefficient calculated for the
! gas phase drag calculations.
         ELSEIF(DES_EXPLICITLY_COUPLED) THEN
            IF(MPPIC) THEN
               DRAG_FC(NP,:) = F_GP(NP)*VELFP
            ELSE
               DRAG_FC(NP,:) = F_GP(NP)*(VELFP - DES_VEL_NEW(NP,:))
            ENDIF
         ELSE

! Calculate the drag coefficient.
            vel_p = DES_VEL_NEW(NP,:)
            CALL DES_DRAG_GP(NP, vel_p, VELFP, lEPg)

! Calculate the gas-solids drag force on the particle
            IF(MPPIC) THEN
               D_FORCE = F_GP(NP)*VELFP
            ELSE
               D_FORCE = F_GP(NP)*(VELFP - DES_VEL_NEW(NP,:))
            ENDIF

! Update the contact forces (FC) on the particle to include gas
! pressure and gas-solids drag
            FC(NP,:) = FC(NP,:) + D_FORCE(:)
            IF(MODEL_A) FC(NP,:) = FC(NP,:) + lPF*PVOL(NP)

         ENDIF

      ENDDO
!$omp end parallel

      RETURN
      END SUBROUTINE DRAG_GS_DES1

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: DRAG_GS_GAS1                                            !
!  Author: J.Musser                                   Date: 21-NOV-14  !
!                                                                      !
!                                                                      !
!  Purpose: This routine is called from the CONTINUUM. It calculates   !
!  the scalar cell center drag force acting on the fluid using         !
!  interpolated values for the gas velocity and volume fraction. The   !
!  The resulting sources are interpolated back to the fluid grid.      !
!                                                                      !
!  NOTE: The loop over particles includes ghost particles so that MPI  !
!  communications are needed to distribute overlapping force between   !
!  neighboring grid cells. This is possible because only cells "owned" !
!  by the current process will have non-zero weights.                  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE DRAG_GS_GAS1

      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------//
!     Cell-center gas velocities.
      DOUBLE PRECISION :: UGC(DIMENSION_3)
      DOUBLE PRECISION :: VGC(DIMENSION_3)
      DOUBLE PRECISION :: WGC(DIMENSION_3)
! Loop counters: Particle, fluid cell, neighbor cells
      INTEGER :: NP, IJK, LC
! Interpolation weight
      DOUBLE PRECISION :: WEIGHT
! Interpolated gas phase quantities and particle velocity
      DOUBLE PRECISION :: lEPg, VELFP(3), vel_p(3)
! Loop bound for filter
      INTEGER :: LP_BND
! Drag force (intermediate calculation)
      DOUBLE PRECISION :: lFORCE
! Drag sources for fluid (intermediate calculation)
      DOUBLE PRECISION :: lDRAG_BM(3)
!......................................................................!

! Initialize fluid cell values.
      F_GDS = ZERO
      DRAG_BM = ZERO

! Loop bounds for interpolation.
      LP_BND = merge(27,9,DO_K)

! Calculate the cell center gas velocities.
      CALL CALC_CELL_CENTER_GAS_VEL(U_G, V_G, W_G, UGC, VGC, WGC)

! Calculate the gas phase forces acting on each particle.

!$omp parallel default(none) private(np,lepg,velfp,ijk,weight,ldrag_bm,lforce,vel_p) &
!$omp          shared(max_pip,des_interp_on,lp_bnd,filter_cell,filter_weight, &
!$omp          ep_g,pijk,des_vel_new,f_gp,vol,des_stat_wt,mppic,drag_bm,f_gds,ugc,vgc,wgc)
!$omp do
      DO NP=1,MAX_PIP

         IF(.NOT.IS_NORMAL(NP)) CYCLE
         IF(.NOT.FLUID_AT(PIJK(NP,4))) CYCLE

         lEPG = ZERO
         VELFP = ZERO

! Calculate the gas volume fraction, velocity, and at the
! particle's position.
         IF(DES_INTERP_ON) THEN
            DO LC=1,LP_BND
               IJK = FILTER_CELL(LC,NP)
               WEIGHT = FILTER_WEIGHT(LC,NP)
! Gas phase volume fraction.
               lEPG = lEPG + EP_G(IJK)*WEIGHT
! Gas phase velocity.
               VELFP(1) = VELFP(1) + UGC(IJK)*WEIGHT
               VELFP(2) = VELFP(2) + VGC(IJK)*WEIGHT
               VELFP(3) = VELFP(3) + WGC(IJK)*WEIGHT
            ENDDO
         ELSE
            IJK = PIJK(NP,4)
            lEPG = EP_G(IJK)
            VELFP(1) = UGC(IJK)
            VELFP(2) = VGC(IJK)
            VELFP(3) = WGC(IJK)
         ENDIF

! This avoids FP exceptions for some ghost particles.
! Using hard-coded tolerance. Comparing to ZERO led to overflow when lEPg is
! around 1D-15
         ! IF(lEPg == ZERO) lEPG = EP_g(PIJK(NP,4))
         IF(lEPg <1.0d-6) lEPG = EP_g(PIJK(NP,4))

! Calculate drag coefficient
         vel_p = DES_VEL_NEW(NP,:)
         CALL DES_DRAG_GP(NP, vel_p, VELFP, lEPg)

         lFORCE = F_GP(NP)
         IF(MPPIC) lFORCE = lFORCE*DES_STAT_WT(NP)

         lDRAG_BM = lFORCE*DES_VEL_NEW(NP,:)

         IF(DES_INTERP_ON) THEN
            DO LC=1,LP_BND
               IJK = FILTER_CELL(LC,NP)
               WEIGHT = FILTER_WEIGHT(LC,NP)/VOL(IJK)

!$omp atomic
               DRAG_BM(IJK,1) = DRAG_BM(IJK,1) + lDRAG_BM(1)*WEIGHT
!$omp atomic
               DRAG_BM(IJK,2) = DRAG_BM(IJK,2) + lDRAG_BM(2)*WEIGHT
!$omp atomic
               DRAG_BM(IJK,3) = DRAG_BM(IJK,3) + lDRAG_BM(3)*WEIGHT
!$omp atomic
               F_GDS(IJK) = F_GDS(IJK) + lFORCE*WEIGHT
            ENDDO
         ELSE
            IJK = PIJK(NP,4)
            WEIGHT = ONE/VOL(IJK)

!$omp atomic
            DRAG_BM(IJK,1) = DRAG_BM(IJK,1) + lDRAG_BM(1)*WEIGHT
!$omp atomic
            DRAG_BM(IJK,2) = DRAG_BM(IJK,2) + lDRAG_BM(2)*WEIGHT
!$omp atomic
            DRAG_BM(IJK,3) = DRAG_BM(IJK,3) + lDRAG_BM(3)*WEIGHT

!$omp atomic
            F_GDS(IJK) = F_GDS(IJK) + lFORCE*WEIGHT
         ENDIF

      ENDDO
!$omp end parallel


! Add in data stored in ghost cells from interpolation. This call must
! precede the SEND_RECV to avoid overwriting ghost cell data.
      IF(DES_INTERP_ON) THEN
         CALL DES_COLLECT_gDATA(F_GDS)
         CALL DES_COLLECT_gDATA(DRAG_BM)
      ENDIF

! Update the drag force and sources in ghost layers.
      CALL SEND_RECV(F_GDS, 2)
      CALL SEND_RECV(DRAG_BM, 2)

      RETURN
      END SUBROUTINE DRAG_GS_GAS1

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CALC_CELL_CENTER_GAS_VEL                                !
!  Author: J.Musser                                   Date: 07-NOV-14  !
!                                                                      !
!  Purpose: Calculate the scalar cell center gas velocity. This code   !
!  is common to the DEM and GAS calls for non-interpolated drag        !
!  routines.                                                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CALC_CELL_CENTER_GAS_VEL(lUg, lVg, lWg, UGC, VGC, WGC)

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      DOUBLE PRECISION, INTENT(IN) :: lUg(DIMENSION_3)
      DOUBLE PRECISION, INTENT(IN) :: lVg(DIMENSION_3)
      DOUBLE PRECISION, INTENT(IN) :: lWg(DIMENSION_3)
      DOUBLE PRECISION, INTENT(OUT) :: UGC(DIMENSION_3)
      DOUBLE PRECISION, INTENT(OUT) :: VGC(DIMENSION_3)
      DOUBLE PRECISION, INTENT(OUT) :: WGC(DIMENSION_3)
! Local variables:
!---------------------------------------------------------------------//
! Indices of adjacent cells
      INTEGER :: IJK, IMJK, IJMK, IJKM
!......................................................................!


! Calculate the cell center gas velocity components.
      DO IJK=IJKSTART3, IJKEND3
         IF(FLUID_AT(IJK)) THEN
            IMJK = IM_OF(IJK)
            IF(CUT_U_TREATMENT_AT(IMJK)) THEN
               UGC(IJK) = (THETA_UE_BAR(IMJK)*lUG(IMJK) +              &
                  THETA_UE(IMJK)*lUg(IJK))
            ELSE
               UGC(IJK) = AVG_X_E(lUG(IMJK),lUG(IJK),I_OF(IJK))
            ENDIF

            IJMK = JM_OF(IJK)
            IF(CUT_V_TREATMENT_AT(IJMK)) THEN
               VGC(IJK) = (THETA_VN_BAR(IJMK)*lVG(IJMK) +              &
                  THETA_VN(IJMK)*lVg(IJK))
            ELSE
               VGC(IJK) = AVG_Y_N(lVg(IJMK),lVg(IJK))
            ENDIF

            IF(DO_K) THEN
               IJKM = KM_OF(IJK)
               IF(CUT_W_TREATMENT_AT(IJKM)) THEN
                  WGC(IJK) = (THETA_WT_BAR(IJKM)*lWg(IJKM) +           &
                     THETA_WT(IJKM)* lWg(IJK))
               ELSE
                  WGC(IJK) = AVG_Z_T(lWg(IJKM),lWg(IJK))
               ENDIF
            ELSE
               WGC(IJK) = ZERO
            ENDIF
         ELSE
            UGC(IJK) = ZERO
            VGC(IJK) = ZERO
            WGC(IJK) = ZERO
         ENDIF
      ENDDO

      RETURN

   END SUBROUTINE CALC_CELL_CENTER_GAS_VEL

END MODULE DRAG_GS_DES1_MOD
