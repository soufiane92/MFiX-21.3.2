#include "error.inc"

MODULE CONV_GS_DES1_MOD

   use calc_gamma_des_mod, only: calc_gamma_des

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
   SUBROUTINE CONV_GS_DES1

      use constant, only: Pi
      use des_drag_gp_mod, only: des_drag_gp
      use des_thermo, only: GAMMAXSA, CONV_QS, Q_SOURCE, CONV_SC, DES_T_S
      use discretelement
      use fldvar
      use functions, only: FLUID_AT
      use functions, only: IS_NORMAL
      use interpolation
      use mfix_pic, only: MPPIC, DES_STAT_WT
      use param1

      IMPLICIT NONE

      DOUBLE PRECISION :: lTg, GAMMA
      DOUBLE PRECISION :: Qcv, STAT_WT
      DOUBLE PRECISION :: l4Pi
      INTEGER :: IJK, NP

      l4Pi = 4.0d0*Pi

      DO NP=1,MAX_PIP
         IF(.NOT.IS_NORMAL(NP)) CYCLE

! Calculate the gas temperature.
         IJK = PIJK(NP,4)
         lTg = T_G(IJK)

! PIC multiplier to scale surface area
         IF(MPPIC) THEN
            STAT_WT = DES_STAT_WT(NP)
         ELSE
            STAT_WT = 1.0d0
         ENDIF

! Avoid convection calculations in cells without fluid (cut-cell)
         IF(.NOT.FLUID_AT(IJK)) THEN
            GAMMAxSA(NP) = ZERO
            CONV_Qs(NP) = ZERO

! For explicit coupling, use the heat transfer coefficient calculated
! for the gas phase heat transfer calculations.
         ELSEIF(DES_EXPLICITLY_COUPLED) THEN
            CONV_Qs(NP) = GAMMAxSA(NP)*(lTg - DES_T_s(NP))

         ELSE

! Calculate the heat transfer coefficient.
            CALL CALC_GAMMA_DES(NP, GAMMA)

! Calculate heat transfer for a real particle in the parcel, then scale W times
            if(CGDEM) then
               GAMMAxSA(NP) = GAMMA* l4Pi*DES_CGP_RPR(NP)*DES_CGP_RPR(NP) *DES_CGP_STW(NP)
            else
               GAMMAxSA(NP) = GAMMA* l4Pi*DES_RADIUS(NP)*DES_RADIUS(NP)
            endif

! Calculate the rate of heat transfer to the particle
            Qcv = GAMMAxSA(NP)*(lTg - DES_T_s(NP))

! Store convection source in global energy source array.
            Q_Source(NP) = Q_Source(NP) + Qcv

! Calculate the gas phase source term components.
            CONV_SC(IJK) = CONV_Sc(IJK) - STAT_WT*Qcv*DTSOLID
         ENDIF
      ENDDO

! Note that MPI sync is managed at the end of des_time_march for
! non-explicitly coupled cases that use interpolation.

      RETURN
      END SUBROUTINE CONV_GS_DES1


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CONV_GS_GAS1                                            !
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
      SUBROUTINE CONV_GS_GAS1

! Size of particle array on this process.
      use discretelement, only: MAX_PIP
! IJK of fluid cell containing particles center
      use discretelement, only: PIJK
! Particle temperature
      use des_thermo, only: DES_T_s
! Gas phase energy equation sources
      use des_thermo, only: CONV_Sp, CONV_Sc
      Use discretelement, only: DES_RADIUS, DES_CGP_RPR, DES_CGP_STW, CGDEM
! Heat transfer coefficint (GAMMA) multiplied by sufrace area
      use des_thermo, only: GAMMAxSA
! Function for identifying fluid cells and normal particles.
      use functions, only: FLUID_AT
      use functions, only: IS_NORMAL
! MPI function for collecting interpolated data from ghost cells.
      use sendrecvnode, only: DES_COLLECT_gDATA
! MPI wrapper for halo exchange.
      use sendrecv, only: SEND_RECV
! PIC Flag and multiplier
      use mfix_pic, only: MPPIC, DES_STAT_WT

! Global Parameters:
!---------------------------------------------------------------------//
! Double precision values.
      use param1, only: ZERO, ONE
      use constant, only: Pi

      IMPLICIT NONE

! Loop counters: Particle, fluid cell, neighbor cells
      INTEGER :: NP, IJK
! Interpolation weight
      DOUBLE PRECISION :: STAT_WT
      DOUBLE PRECISION :: GAMMAxSAxTp, GAMMA
      DOUBLE PRECISION :: l4Pi

      l4Pi = 4.0d0*Pi

! Initialize fluid cell values.
      CONV_Sc = ZERO
      CONV_Sp = ZERO

! Calculate the gas phase forces acting on each particle.
      DO NP=1,MAX_PIP

         IJK = PIJK(NP,4)

         IF(.NOT.IS_NORMAL(NP)) CYCLE
         IF(.NOT.FLUID_AT(IJK)) CYCLE

! Calculate the heat transfer coefficient.
         CALL CALC_GAMMA_DES(NP, GAMMA)

! Calculate the surface area of the particle

! Calculate heat transfer for a real particle in the parcel, then scale W times
         if(CGDEM) then
            GAMMAxSA(NP) = GAMMA* l4Pi*DES_CGP_RPR(NP)*DES_CGP_RPR(NP) *DES_CGP_STW(NP)
         else
            GAMMAxSA(NP) = GAMMA* l4Pi*DES_RADIUS(NP)*DES_RADIUS(NP)
         endif

         GAMMAxSAxTp = GAMMAxSA(NP)*DES_T_s(NP)

! PIC multiplier to scale surface area
         IF(MPPIC) THEN
            STAT_WT = DES_STAT_WT(NP)
         ELSE
            STAT_WT = 1.0d0
         ENDIF

         CONV_Sc(IJK) = CONV_Sc(IJK) + STAT_WT*GAMMAxSAxTp
         CONV_Sp(IJK) = CONV_Sp(IJK) + STAT_WT*GAMMAxSA(NP)

      ENDDO

! Update the drag force and sources in ghost layers.
      CALL SEND_RECV(CONV_SC, 2)
      CALL SEND_RECV(CONV_SP, 2)

      RETURN
      END SUBROUTINE CONV_GS_GAS1


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Subroutine: ZERO_ENERGY_SOURCE                                      !
!                                                                      !
!  Purpose: ZERO out the array that passes energy source terms back to !
!  the continuum model. Additional entries may be needed to include    !
!  heat transfer to the hybrid mode.                                   !
!                                                                      !
!  Author: J.Musser                                   Date: 15-Jan-11  !
!                                                                      !
!  Comments:                                                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE ZERO_ENERGY_SOURCE

      Use des_thermo
      Use param1

      IMPLICIT NONE

      CONV_Sc = ZERO
      CONV_Sp = ZERO

      RETURN

   END SUBROUTINE ZERO_ENERGY_SOURCE

END MODULE CONV_GS_DES1_MOD
