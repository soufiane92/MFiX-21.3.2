#include "version.inc"
#include "error.inc"

MODULE RXNS_GS_DES1_MOD

   use calc_rrates_des_mod, only: zero_rrate_des, calc_rrates_des
   use vtk

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: CONV_GS_DES1                                           !
!  Author: J.Musser: 16-Jun-10                                         !
!                                                                      !
!  Purpose: This routine is called from the DISCRETE side to calculate !
!  the gas-particle convective heat transfer.                          !
!                                                                      !
!  Comments: Explicitly coupled simulations use a stored convective    !
!  heat transfer coefficient. Otherwise, the convective heat transfer  !
!  coeff is calculated every time step and the total interphase energy !
!  transferred is 'stored' and used explicitly in the gas phase. The     !
!  latter conserves all energy
!                                                                      !
!  REF: Zhou, Yu, and Zulli, "Particle scale study of heat transfer in !
!       packed and bubbling fluidized beds," AIChE Journal, Vol. 55,   !
!       no 4, pp 868-884, 2009.                                        !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE RXNS_GS_DES1

      use constant, only: Pi
! Flag: The fluid and discrete solids are explicitly coupled.
      use discretelement, only: DES_EXPLICITLY_COUPLED
      use discretelement, only: DTSOLID
      use discretelement, only: PIJK
      use discretelement, only: MAX_PIP

      use des_rxns, only: DES_R_gp, DES_R_gc
      use des_rxns, only: DES_R_PHASE, DES_SUM_R_g
      use des_rxns, only: DES_HOR_G

      use physprop, only: NMAX

      use functions, only: FLUID_AT
      use functions, only: IS_NORMAL

      use param1, only: ZERO
      use des_rxns, only: DES_R_s
      use des_thermo, only: RXNS_Qs

      use param1, only: DIMENSION_LM
      use param, only: DIMENSION_M

      IMPLICIT NONE

      INTEGER :: IJK, NP

! Local gas phase values.
      DOUBLE PRECISION :: lRgp(NMAX(0)) ! Rate of species production
      DOUBLE PRECISION :: lRgc(NMAX(0)) ! Rate of species consumption
      DOUBLE PRECISION :: lHoRg         ! Heat of reaction
      DOUBLE PRECISION :: lSUMRg        ! lSUMRg

! Interphase mass transfer
      DOUBLE PRECISION :: lRPhase(DIMENSION_LM+DIMENSION_M-1)

      DOUBLE PRECISION :: WEIGHT

      IF(DES_EXPLICITLY_COUPLED) RETURN

      IF(SAVE_DES_RRATES) Des_rrates_out(:,:) = ZERO
      IF(SAVE_PART_RRATES) Part_rrates_out(:,:) = ZERO

      DO NP=1,MAX_PIP
         IF(.NOT.IS_NORMAL(NP)) CYCLE

! Avoid convection calculations in cells without fluid (cut-cell)
         IF(.NOT.FLUID_AT(PIJK(NP,4))) THEN
            DES_R_s(NP,:) = ZERO
            RXNS_Qs(NP) = ZERO

! No additional calculations are needed for explicitly coupled
         ELSE

! Calculate the heat transfer coefficient.
            CALL CALC_RRATES_DES(NP, lRgp, lRgc, lRPhase, lHoRg, lSUMRg)

! Integrate over solids time step and store in global array.
!---------------------------------------------------------------------//
! Store the gas phase source terms.
            IJK = PIJK(NP,4)
            WEIGHT = DTSOLID

            DES_R_gp(IJK,:) = DES_R_gp(IJK,:) + lRgp*WEIGHT
            DES_R_gc(IJK,:) = DES_R_gc(IJK,:) + lRgc*WEIGHT
            DES_R_PHASE(IJK,:) = DES_R_PHASE(IJK,:) + lRPhase*WEIGHT
            DES_HOR_G(IJK) = DES_HOR_G(IJK) + lHoRg*WEIGHT
            DES_SUM_R_g(IJK) = DES_SUM_R_g(IJK) + lSUMRg*WEIGHT
         ENDIF
      ENDDO

! Note that MPI sync is managed at the end of des_time_march for
! non-explicitly coupled cases that use interpolation.

      RETURN
   END SUBROUTINE RXNS_GS_DES1


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: RXNS_GS_GAS1                                            !
!  Author: J.Musser                                   Date: 21-NOV-14  !
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
   SUBROUTINE RXNS_GS_GAS1

! Size of particle array on this process.
      use discretelement, only: MAX_PIP
! Flag to use interpolation
      use particle_filter, only: DES_INTERP_ON
! IJK of fluid cell containing particles center
      use discretelement, only: PIJK
! Gas phase mass, species, and energy equation sources
      use des_rxns, only: DES_R_gp, DES_R_gc, DES_SUM_R_g
      use des_rxns, only: DES_R_PHASE, DES_HOR_g
! Number of species for each phase
      use physprop, only: NMAX
! Function for identifying fluid cells and normal particles.
      use functions, only: FLUID_AT
      use functions, only: IS_NORMAL
! MPI function for collecting interpolated data from ghost cells.
      use sendrecvnode, only: DES_COLLECT_gDATA
! MPI wrapper for halo exchange.
      use sendrecv, only: SEND_RECV
! Fluid time step size.
      use run, only: DT
! Flag to use stiff chemistry solver
      use stiff_chem, only: stiff_chemistry
! Routine to sync stiff solver results
      use stiff_chem, only: FINALIZE_STIFF_SOLVER

! Global Parameters:
!---------------------------------------------------------------------//
! Double precision values.
      use param1, only: ZERO, ONE
      use param1, only: DIMENSION_LM
      use param, only: DIMENSION_M

      IMPLICIT NONE

! Loop counters: Particle, fluid cell, neighbor cells
      INTEGER :: NP, IJK
! Local gas phase values.
      DOUBLE PRECISION :: lRgp(NMAX(0)) ! Rate of species production
      DOUBLE PRECISION :: lRgc(NMAX(0)) ! Rate of species consumption
      DOUBLE PRECISION :: lHoRg         ! Heat of reaction
      DOUBLE PRECISION :: lSUMRg        ! lSUMRg

! Interphase mass transfer
      DOUBLE PRECISION :: lRPhase(DIMENSION_LM+DIMENSION_M-1)

         DES_R_gp    = ZERO
         DES_R_gc    = ZERO
         DES_R_PHASE = ZERO
         DES_HOR_G   = ZERO
         DES_SUM_R_g = ZERO

         IF(SAVE_DES_RRATES) Des_rrates_out(:,:) = ZERO
         IF(SAVE_PART_RRATES) Part_rrates_out(:,:) = ZERO

! Directly integrate fluid cell reaction sources
      IF(STIFF_CHEMISTRY) THEN
         CALL DES_STIFF_CHEM
         CALL FINALIZE_STIFF_SOLVER

! Store the gas phase source terms.
      ELSE

! Initialize fluid cell values.
         DES_R_gp    = ZERO
         DES_R_gc    = ZERO
         DES_R_PHASE = ZERO
         DES_HOR_G   = ZERO
         DES_SUM_R_g = ZERO

! Calculate the gas phase forces acting on each particle.
         DO NP=1,MAX_PIP

   ! Only calculate chemical reactions for normal particles that are inside
   ! fluid cells. Ex: Skip ghost particles or particles that are in a cut-
   ! cell dead space.
            IF(.NOT.IS_NORMAL(NP)) CYCLE
            IF(.NOT.FLUID_AT(PIJK(NP,4))) CYCLE

            IJK = PIJK(NP,4)

! Calculate the rates of species formation/consumption.
            CALL CALC_RRATES_DES(NP, lRgp, lRgc, lRPhase, lHoRg, lSUMRg)


! Store the gas phase source terms.
            DES_R_gp(IJK,:) = DES_R_gp(IJK,:) + lRgp
            DES_R_gc(IJK,:) = DES_R_gc(IJK,:) + lRgc
            DES_R_PHASE(IJK,:) = DES_R_PHASE(IJK,:) + lRPhase
            DES_HOR_G(IJK) = DES_HOR_G(IJK) + lHoRg
            DES_SUM_R_g(IJK) = DES_SUM_R_g(IJK) + lSUMRg
         ENDDO

! Add in data stored in ghost cells from interpolation. This call must
! precede the SEND_RECV to avoid overwriting ghost cell data.
         IF(DES_INTERP_ON) THEN
            CALL DES_COLLECT_gDATA(DES_R_gp)
            CALL DES_COLLECT_gDATA(DES_R_gc)
            CALL DES_COLLECT_gDATA(DES_R_PHASE)
            CALL DES_COLLECT_gDATA(DES_HOR_g)
            CALL DES_COLLECT_gDATA(DES_SUM_R_g)
         ENDIF

! Update the species mass sources in ghost layers.
         CALL SEND_RECV(DES_R_gp, 2)
         CALL SEND_RECV(DES_R_gc, 2)
         CALL SEND_RECV(DES_R_PHASE, 2)
         CALL SEND_RECV(DES_HOR_g, 2)
         CALL SEND_RECV(DES_SUM_R_g, 2)
      ENDIF

      RETURN
   END SUBROUTINE RXNS_GS_GAS1


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: DES_STIFF_CHEM                                          !
!  Author: J.Musser                                   Date:  7-FEB-16  !
!                                                                      !
!  Purpose: This routine updates a fluid cell by directly integrating  !
!  the source terms due to reaction. Note that this routine is called  !
!  for each particle to prevent over consumption of reactants.         !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE DES_STIFF_CHEM

      use compar, only: PE_IO, myPE, numPEs
      USE constant, only: GAS_CONST
! Size of particle array on this process.
      use discretelement, only: MAX_PIP
! IJK of fluid cell containing particles center
      use discretelement, only: PIJK
      use error_manager
      USE fldvar, only: EP_g, RO_g, ROP_g, T_g, X_g, P_g
! Function for identifying fluid cells and normal particles.
      use functions, only: FLUID_AT
      use functions, only: IS_NORMAL
      USE geometry, only: VOL
      USE param1, only: ZERO, SMALL_NUMBER, ONE
      use param1, only: DIMENSION_LM
      use param, only: DIMENSION_M
      USE physprop, only: C_pg, MW_g, MW_MIX_g
      use toleranc, only: ZERO_X_gs
      use physprop, only: NMAX
! Fluid time step size.
      use run, only: DT

      IMPLICIT NONE

! Passed variables
!---------------------------------------------------------------------//
      DOUBLE PRECISION :: lRgp(NMAX(0)) ! Rate of species production
      DOUBLE PRECISION :: lRgc(NMAX(0)) ! Rate of species consumption
      DOUBLE PRECISION :: lHoRg         ! Heat of reaction
      DOUBLE PRECISION :: lSUMRg        ! lSUMRg

! Local variables
!---------------------------------------------------------------------//
! Loop counters: Particle, fluid cell, species index
      INTEGER :: NP, IJK, N
      DOUBLE PRECISION :: lRg(NMAX(0)), sumXg
      DOUBLE PRECISION :: lDToVOL
! Interphase mass transfer
      DOUBLE PRECISION :: lRPhase(DIMENSION_LM+DIMENSION_M-1)
! CPU timing
      DOUBLE PRECISION :: des_stiff_chem_start_time, des_stiff_chem_end_time, des_stiff_chem_cpu_time
! Message buffer.
      CHARACTER(LEN=64) :: lMsg0, lMsg1
!......................................................................!

      write(ERR_MSG,"(/3x,'Lagrangian stiff chemistry...')")
      call log_status()
      CALL CPU_TIME(des_stiff_chem_start_time)

      DO NP=1,MAX_PIP

! Only calculate chemical reactions for normal particles that are inside
! fluid cells. Ex: Skip ghost particles or particles that are in a cut-
! cell dead space.
         IF(.NOT.IS_NORMAL(NP)) CYCLE
         IF(.NOT.FLUID_AT(PIJK(NP,4))) CYCLE

         IJK = PIJK(NP,4)

! Calculate the rates of species formation/consumption.
         CALL CALC_RRATES_DES(NP, lRgp, lRgc, lRPhase, lHoRg, lSUMRg)

! Time step divided by cell volume.
         lDToVOL = DT/VOL(IJK)
! Net change in species mass
         lRg = lRgp - lRgc

! Gas phase density: ROP_g
         ROP_G(IJK) = ROP_G(IJK) + lDToVOL*lSUMRg

! Temperature: T_g
         T_g(IJK) = T_g(IJK) - lDToVOL*(lHORg/(ROP_g(IJK)*C_pg(IJK)))

! Species mass fractions: X_g
         DO N=1,NMAX(0)
            X_g(IJK,N) = X_g(IJK,N) + lDToVOL * &
               (lRg(N) - X_g(IJK,N)*lSUMRg)/(ROP_g(IJK))
         ENDDO

! Cleanup over/under shoots.
         X_g(IJK,:) = min(max(X_g(IJK,:), ZERO), ONE)
         sumXg = sum(X_g(IJK,:))
         X_g(IJK,:) = X_g(IJK,:)/sumXg

! Gas phase bulk density is updated within the stiff solver (lVar(1)).
! Now that the gas phase volume fraction is updated, the gas phase
! density can be backed out. RO_g * EP_g = ROP_g
         IF(EP_g(IJK) > SMALL_NUMBER) THEN
            RO_g(IJK) = ROP_g(IJK) / EP_g(IJK)
         ELSE
! This case shouldn't happen, however HUGE is used to aid in tracking
! errors should this somehow become and issue.
            RO_g(IJK) = huge(0.0)
         ENDIF

! Calculate the mixture molecular weight.
         MW_MIX_G(IJK) = sum(X_G(IJK,1:NMAX(0))/MW_g(1:NMAX(0)))
         MW_MIX_G(IJK) = ONE/MW_MIX_G(IJK)

! Calculate the gas phase pressure.
         P_G(IJK) = (RO_G(IJK)*GAS_CONST*T_G(IJK))/MW_MIX_G(IJK)
      ENDDO

      CALL CPU_TIME(des_stiff_chem_end_time)
      des_stiff_chem_cpu_time = des_stiff_chem_end_time - des_stiff_chem_start_time 

! Display stiff solver summary.
      IF(myPE == PE_IO) THEN


         IF(des_stiff_chem_cpu_time > 3.6d3) THEN
            lMsg0=''; write(lMsg0,"(f8.4)") des_stiff_chem_cpu_time/3.6d3
            lMsg1='hrs'
         ELSEIF(des_stiff_chem_cpu_time > 6.0d1) THEN
            lMsg0=''; write(lMsg0,"(f8.4)") des_stiff_chem_cpu_time/6.0d1
            lMsg1='min'
         ELSE
            lMsg0=''; write(lMsg0,"(f8.4)") des_stiff_chem_cpu_time
            lMsg1='sec'
         ENDIF
         write(ERR_MSG,1003) trim(adjustl(lMsg0)), trim(adjustl(lMsg1))
         CALL LOG_STATUS()

      ENDIF

      RETURN

      1003 Format(5x,'DONE. Lagrangian stiff chemistry CPU Time Used: ',A,' ',A)

   END SUBROUTINE DES_STIFF_CHEM

END MODULE RXNS_GS_DES1_MOD
