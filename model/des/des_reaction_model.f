MODULE DES_REACTION_MODEL_MOD
CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: DES_REACTION_MODEL                                     !
!                                                                      !
!  Purpose:                                                            !
!                                                                      !
!                                                                      !
!  Author: J.Musser                                   Date: 16-Jun-10  !
!                                                                      !
!  Comments:                                                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE DES_REACTION_MODEL

      USE constant, only: pi
      USE des_rxns, only: des_r_s, des_x_s
      USE discretelement, only: des_radius, pvol, ro_sol, pijk, pmass, omoi, intg_euler, dtsolid
      USE discretelement, only: max_pip, des_explicitly_coupled, particle_state, normal_particle
      USE functions, only: is_normal
      USE param, only: dimension_n_s
      USE param1, only: zero
      USE run, only: ANY_SPECIES_EQ
      USE run, only: DT
      USE run, only: SOLVE_ROs
      USE discretelement, only: CGDEM, DES_CGP_RPR, DES_CGP_STW
      IMPLICIT NONE

! Loop counter
      INTEGER :: NN
! total rate of consumption/production of species (g/sec)
      DOUBLE PRECISION  :: SUM_DES_Rs(1:MAX_PIP)

      DOUBLE PRECISION :: PIx4o3
      DOUBLE PRECISION :: o3 = 1.0d0/3.0d0

      DOUBLE PRECISION :: lDT, lOoDT
! Logical for Adams-Bashfort integration.
      LOGICAL,SAVE:: FIRST_PASS = .TRUE.

!---------------------------------------------------------------------//

      IF(.NOT.ANY_SPECIES_EQ) RETURN

      PIx4o3 = Pi*4.0d0/3.0d0

      lDT = merge(DT, DTSOLID, DES_EXPLICITLY_COUPLED)
      lOoDT = -1.0d0/lDT

! Bound the amount of mass loss. The definition of lOodT as a negative
! value allows the max function evaluation here
      FORALL(NN=1:DIMENSION_N_S)
         WHERE(PARTICLE_STATE(:MAX_PIP) == NORMAL_PARTICLE)         &
            DES_R_s(:MAX_PIP,NN) = max(DES_R_s(:MAX_PIP,NN),        &
            DES_X_s(:MAX_PIP,NN)*lOoDT)
      END FORALL

! First-order method: Euler
      IF(INTG_EULER) THEN
         WHERE(PARTICLE_STATE(:MAX_PIP) == NORMAL_PARTICLE)            &
            SUM_DES_Rs(:MAX_PIP) = sum(DES_R_s(:MAX_PIP,:),DIM=2)

! sum_des_rs is the rate (unit amount of sustance per time) divided
! by pmass
         WHERE(PARTICLE_STATE(:MAX_PIP) == NORMAL_PARTICLE)            &
            PMASS(:MAX_PIP) = PMASS(:MAX_PIP) + lDT*                   &
               SUM_DES_Rs(:MAX_PIP)*PMASS(:MAX_PIP)

         FORALL(NN=1:DIMENSION_N_S)
            WHERE(PARTICLE_STATE(:MAX_PIP) == NORMAL_PARTICLE)         &
               DES_X_s(:MAX_PIP,NN) = max(DES_X_s(:MAX_PIP,NN) + lDT*  &
                  (DES_R_s(:MAX_PIP,NN) - DES_X_s(:MAX_PIP,NN)*        &
                  SUM_DES_Rs(:MAX_PIP)), ZERO)
         END FORALL

      ELSE
         IF(FIRST_PASS) THEN
         ENDIF
      ENDIF

      DO NN=1,MAX_PIP
         IF(IS_NORMAL(NN)) THEN
            IF(SOLVE_ROs(PIJK(NN,5))) THEN
               RO_Sol(NN)= PMASS(NN)/PVOL(NN)
            ELSE
               DES_RADIUS(NN) = (PMASS(NN)/(Pix4o3*RO_SOL(NN)))**o3
               PVOL(NN) = PMASS(NN)/RO_SOL(NN)
!llu, 11/7/18, update real particle size
               if(CGDEM) DES_CGP_RPR(NN) = DES_RADIUS(NN) / DES_CGP_STW(NN)**o3
            ENDIF
            OMOI(NN) = 2.5D0/(PMASS(NN)*DES_RADIUS(NN)**2) !UPDATE ONE OVER MOI
         ENDIF
      ENDDO

! Clear the necessary variables.
      DES_R_s = ZERO

! Flag that the first pass is over
      FIRST_PASS = .FALSE.

      RETURN

   END SUBROUTINE DES_REACTION_MODEL

END MODULE DES_REACTION_MODEL_MOD
