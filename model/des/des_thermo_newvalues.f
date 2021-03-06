MODULE DES_THERMO_NEWVALUES_MOD

   use des_reaction_model_mod, only: des_reaction_model

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: DES_THERMO_NEWVALUES                                   !
!                                                                      !
!  Purpose:                                                            !
!                                                                      !
!                                                                      !
!  Author: J.Musser                                   Date: 16-Jun-10  !
!                                                                      !
!  Comments:                                                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE DES_THERMO_NEWVALUES

      USE des_thermo, only: q_source, q_source0, des_t_s, des_c_ps
      USE des_thermo_cond, only: DES_QW_cond
      USE discretelement, only: intg_euler, intg_adams_bashforth, max_pip, des_explicitly_coupled
      USE discretelement, only: normal_particle, particle_state, pmass, dtsolid
      USE param1, only: zero
      USE run, only: ANY_SPECIES_EQ, ENERGY_EQ

      IMPLICIT NONE

! Passed variables
!-----------------------------------------------
! NONE

! Local variables
!---------------------------------------------------------------------//
! Logical for Adams-Bashfort integration.
      LOGICAL,SAVE:: FIRST_PASS = .TRUE.
!---------------------------------------------------------------------//

      IF(ENERGY_EQ) THEN

! Second-order Adams-Bashforth scheme defaults to Euler on first pass.
         IF(FIRST_PASS .AND. INTG_ADAMS_BASHFORTH) THEN
            WHERE(PARTICLE_STATE(:MAX_PIP) == NORMAL_PARTICLE) &
                 Q_Source0(:MAX_PIP) = Q_Source(:MAX_PIP)/       &
                 (PMASS(:MAX_PIP)*DES_C_ps(:MAX_PIP))
         ENDIF
         FIRST_PASS = .FALSE.

! First-order method
         IF (INTG_EULER) THEN
            WHERE(PARTICLE_STATE(:MAX_PIP) == NORMAL_PARTICLE)   &
                 DES_T_s(:MAX_PIP) = DES_T_s(:MAX_PIP) +   &
                 DTSOLID*(Q_Source(:MAX_PIP)/(PMASS(:MAX_PIP)*     &
                 DES_C_ps(:MAX_PIP)))

! Second-order Adams-Bashforth scheme
         ELSE
            WHERE(PARTICLE_STATE(:MAX_PIP) == NORMAL_PARTICLE)
               DES_T_s(:MAX_PIP) = DES_T_s(:MAX_PIP) + DTSOLID *         &
                    (1.5d0*Q_Source(:MAX_PIP) -0.5d0*Q_Source0(:MAX_PIP))/ &
                    (PMASS(:MAX_PIP)*DES_C_ps(:MAX_PIP))
               Q_Source0(:MAX_PIP) = Q_Source(:MAX_PIP)
            ENDWHERE
         ENDIF

         Q_Source(:) = ZERO
         IF(ALLOCATED(DES_QW_Cond)) DES_QW_Cond(:,:) = ZERO

      ENDIF

! Update particle from reactive chemistry process.
      IF(ANY_SPECIES_EQ .AND. .NOT.DES_EXPLICITLY_COUPLED)&
         CALL DES_REACTION_MODEL

      RETURN

   END SUBROUTINE DES_THERMO_NEWVALUES

END MODULE DES_THERMO_NEWVALUES_MOD
