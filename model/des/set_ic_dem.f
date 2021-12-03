#include "error.inc"

MODULE SET_IC_DEM_MOD

   use error_manager

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE Name: DES_SET_IC                                         !
!                                                                      !
!  Purpose: Assign initial conditions to particles based on their      !
!  location within the domain.                                         !
!                                                                      !
!  Author: J.Musser                                   Date: 15-Feb-11  !
!                                                                      !
!  Comments:                                                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE SET_IC_DEM

      use run, only: ENERGY_EQ, SPECIES_EQ

      use ic

      use des_thermo, only: DES_T_s

      use derived_types, only: PIC
      use discretelement, only : gener_part_config, P_INPUT_DAT_VERSION
      use discretelement, only: MAX_PIP
      use discretelement, only: PINC
      use discretelement, only: PIJK

      USE des_rxns, only: DES_X_s

      use mfix_pic, only: MPPIC

      use param1, only: undefined, zero

      use physprop, only: C_PS0
      use physprop, only: NMAX

      USE compar
      use indices
      use geometry

      use functions
      use toleranc

      IMPLICIT NONE

! Dummy indices
      INTEGER :: ICV
      INTEGER :: I, J, K, IJK
      INTEGER :: M, NN
      INTEGER :: NP
      INTEGER :: NINDX

! Particle temperature and species only need to be set here
! from IC region settings if using particle_input.dat version 1.0
! If using particle_input.dat version 2.0 and above, temperature
! and species are set from data in particle_input.dat.
! If using IC region settings (gener_part_config=.true.), temperature
! and species are set in ADD_PARTICLE subroutine (generate_particles_mod.f).
! Temperature and species are also set here when using MPPIC

      IF ((.NOT.GENER_PART_CONFIG.AND.trim(P_INPUT_DAT_VERSION)=='1.0').OR.MPPIC) THEN

         DO ICV = 1, DIMENSION_IC
            IF(.NOT.IC_DEFINED(ICV)) CYCLE

            DO K = IC_K_B(ICV), IC_K_T(ICV)
            DO J = IC_J_S(ICV), IC_J_N(ICV)
            DO I = IC_I_W(ICV), IC_I_E(ICV)

! Set the initial conditions for particles in cells that are
! not dead and that this rank owns.
               IF (.NOT.IS_ON_myPE_plus2layers(I,J,K)) CYCLE
               IF (DEAD_CELL_AT(I,J,K)) CYCLE

               IJK = FUNIJK(I,J,K)

! Loop through particles in cell IJK.
               DO NINDX = 1,PINC(IJK)
                  NP = PIC(IJK)%P(NINDX)

! Shift the phase index to the absolute phase index.
                  M = PIJK(NP,5)

! Set the initial particle temperature.
                  IF(ENERGY_EQ) THEN
                     DES_T_s(NP) = IC_T_s(ICV,M)
                  ENDIF

! Set the initial species composition.
                  IF((ENERGY_EQ .AND. C_Ps0(M) == UNDEFINED) .OR.         &
                     SPECIES_EQ(M)) THEN
                     DES_X_s(NP,:) = ZERO
                     DO NN = 1, NMAX(M)
                        DES_X_s(NP,NN) = IC_X_s(ICV,M,NN)
                     ENDDO
                  ENDIF
               ENDDO
            ENDDO
            ENDDO
            ENDDO
         ENDDO
      ENDIF


! Verify that all particles have a specified temperature and species
! mass fractions that sum to one. These checks are needed as the
! basic checks for IC_T_s and IC_X_s can be skipped if the IC region
! is specified with EPg = 1.
      DO NP = 1, MAX_PIP
! skipping non-existent particles
         IF(IS_NONEXISTENT(NP)) CYCLE
! skipping ghost particles
         IF(IS_GHOST(NP) .OR. IS_ENTERING_GHOST(NP) .OR. IS_EXITING_GHOST(NP)) CYCLE
         IF(IS_GHOST(NP)) CYCLE

         M = PIJK(NP,5)

! Check that the temperature is specified.
         IF(ENERGY_EQ) THEN
            IF(DES_T_s(NP) == ZERO) THEN
               WRITE(ERR_MSG, 2000) trim(iVal(NP)), trim(iVal(M))
               CALL LOG_ERROR()
            ENDIF
         ENDIF

 2000 FORMAT('Error 2000: Particle ',A,' does not have a specified ',  &
         'initial',/'temperature. Verify that the IC region ',         &
         'containing this particle',/'has a solids temperature ',      &
         'defined: IC_T_s(ICV,',A,').')

! Check that the species mass fractions are specified.
         IF((ENERGY_EQ .AND. C_Ps0(M) == UNDEFINED) .OR.               &
            SPECIES_EQ(M)) THEN
            IF(.NOT.COMPARE(sum(DES_X_s(NP,1:NMAX(M))),ONE)) THEN
               WRITE(ERR_MSG, 2001) trim(iVal(NP)), trim(iVal(M))
               CALL LOG_ERROR()
            ENDIF
         ENDIF

 2001 FORMAT('Error 2001: The initial species mass fraction for ',     &
         'particle ',A,/'does not sum to one. Verify that the IC ',    &
         'region containing this particle',/'has the solids species ', &
         'mass fractions defined: IC_X_s(ICV,',A,',:).')

      ENDDO

      RETURN

   END SUBROUTINE SET_IC_DEM

END MODULE SET_IC_DEM_MOD
