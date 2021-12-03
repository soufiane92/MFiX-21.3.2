#include "error.inc"

MODULE CHECK_SOLIDS_DEM_MOD

   USE constant, only: pi

   use des_thermo, only: SB_CONST
   use des_thermo, only: DES_Em
   USE des_thermo, only: CALC_COND_DES
   USE des_thermo, only: CALC_RADT_DES
   USE des_thermo, only: DES_MIN_COND_DIST
   USE des_thermo_cond, only: DO_AREA_CORRECTION
   USE discretelement
   USE param1, only: zero, one, half, undefined

   USE physprop, only: mmax, d_p0, ro_s0
   use physprop, only: K_S0
   use run, only: ks_model, ks_model_enum, ks_none, ks_musser
   USE run, only: time, units, solids_model, energy_eq

   USE error_manager

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: CHECK_DES_SOLIDS                                        !
!  Author: J.Musser                                   Date: 02-FEB-14  !
!                                                                      !
!  Purpose:                                                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE CHECK_SOLIDS_DEM

      implicit none

! Skip the check when using DLB and the partition is adjusted
      IF(ADJUST_PARTITION) return

! Get reference radius and mass for each phase
      CALL GET_REF_PARTICLE_SIZE_AND_MASS
! Particle-particle collision parameters.
      CALL CHECK_SOLIDS_DEM_COLLISION
! DES cohesion model parameters.
      CALL CHECK_SOLIDS_DEM_COHESION
! Particle-particle conduction model parameters.
      IF (ENERGY_EQ) CALL CHECK_SOLIDS_DEM_ENERGY

      RETURN

      END SUBROUTINE CHECK_SOLIDS_DEM

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE CHECK_SOLIDS_DEM_ENERGY                                 !
!  Author: J.Musser                                   Date: 02-FEB-14  !
!                                                                      !
!  Purpose:                                                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_SOLIDS_DEM_ENERGY


      IMPLICIT NONE
!......................................................................!
      INTEGER :: M, L  ! Loop indices for DEM solids
! Calculate phase index offset for certain inputs until it can be
! addressed in other ways. should not matter unlesss hybrid
      CHARACTER(len=64) :: MSG
      INTEGER :: MMAX_TOT
      LOGICAL :: ANY_CONDUCTION = .FALSE.

      DOUBLE PRECISION :: MASS_L, MASS_M, MASS_EFF
      DOUBLE PRECISION :: R_EFF, E_EFF
      INTEGER :: zero_ks0, undef_ks0, no_dem


! Radiation Equation:
!'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
! Set flags for radiation in dem specific routine
      DO M = MMAX+1, MMAX+DES_MMAX
         IF(.NOT.(SOLIDS_MODEL(M) == 'DEM'.OR.SOLIDS_MODEL(M) == 'CGP')) CYCLE
         IF(DES_Em(M) == UNDEFINED) THEN
            WRITE(ERR_MSG,1000) trim(iVar('DES_Em',M))
            CALL LOG_ERROR()
         ELSEIF(DES_Em(M) > ONE .OR. DES_Em(M) < ZERO) THEN
            WRITE(ERR_MSG, 1001) trim(iVar('DES_Em',M)), &
               ival(DES_EM(M))
            CALL LOG_ERROR()
         ELSEIF(DES_Em(M) > ZERO) THEN
! Flag to calculate radiation.
            CALC_RADT_DES(M) = .TRUE.
         ENDIF
      ENDDO

! Set the value of the Stefan-Boltzman Constant based on the units
      IF(UNITS == 'SI')THEN
         SB_CONST = 5.6704d0*(10.0d0**(-8)) ! W/((m^2).K^4)
      ELSE
         SB_CONST = 1.355282d0*(10.0d0**(-12)) ! cal/((cm^2).sec.K^4)
      ENDIF


! Conduction Equations:
!'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

! At this point, ks_model must be defined for all dem solids
      zero_ks0 = 0
      undef_ks0 = 0
      no_dem = 0   ! distinguish different types of discrete phases
      ANY_CONDUCTION = .TRUE.

      DO M = MMAX+1, MMAX+DES_MMAX
         IF(.NOT.(SOLIDS_MODEL(M) == 'DEM'.OR.SOLIDS_MODEL(M) == 'CGP')) CYCLE

         no_dem = no_dem + 1
         SELECT CASE (trim(adjustl(KS_MODEL(M))))
         CASE ('MUSSER')
            KS_MODEL_ENUM(M) = ks_musser
            IF (K_S0(M) == UNDEFINED .OR. K_S0(M) < ZERO) THEN
! while k_s0=0 is permissible, it would involve an unnecessary computationally
! intensive for a result that would be zero for any/all conductive heat
! transfer involving a particle of that phase. however, one could permit this
! to allow a case where some particles have ks=0 and others ks>0
               WRITE(ERR_MSG, 1001) trim(iVar('K_s0',M)), iVal(K_s0(M))
               CALL LOG_ERROR()
            ELSEIF (K_S0(M) == ZERO) THEN
               zero_ks0 = zero_ks0+1
            ENDIF
! Flag to calculate conduction.
            CALC_COND_DES(M) = .TRUE.

         CASE ('NONE')
! in this case all particle conductive heat transfer must be neglected. given
! the way particle-particle conduction is resolved (through the neighbor list)
! if any particle conduction is skipped then for consistency all particle
! conduction should be skipped
            KS_MODEL_ENUM(M) = ks_none
            IF (K_S0(M) /= UNDEFINED .AND. K_S0(M) /= ZERO) THEN
! flag warning k_s0 will be overwritten to 0
               WRITE(ERR_MSG, 1201) trim(iVar('K_S0',M)), K_s0(M), &
                  trim(iVar('KS_MODEL',M)), trim(adjustl(KS_MODEL(M)))
               CALL LOG_WARNING()
               K_S0(M) = zero
            ENDIF
            undef_ks0 = undef_ks0 + 1
            CALC_COND_DES(M) = .FALSE.
            ANY_CONDUCTION = .FALSE.
         CASE DEFAULT
            WRITE(ERR_MSG, 1001) trim(adjustl(KS_MODEL(M))), &
               trim(iVar('KS_MODEL',M))
            CALL LOG_ERROR()
         END SELECT
      ENDDO

! check only against dem discrete phases
      IF (zero_ks0>0 .AND. zero_ks0 == no_dem) THEN
          WRITE(ERR_MSG, 1202)
          CALL LOG_WARNING()
      ENDIF
      IF (undef_ks0>0 .AND. undef_ks0 /= no_dem) THEN
         WRITE(ERR_MSG, 1203)
         CALL LOG_ERROR()
      ENDIF


      IF (.NOT.ANY_CONDUCTION) RETURN

! Set the default value for the minimum distance separating particles'
! surfaces.
      IF(DES_MIN_COND_DIST == UNDEFINED)THEN
         DES_MIN_COND_DIST = 1.0D-04 ! cm
         IF (UNITS == 'SI') DES_MIN_COND_DIST = &
            DES_MIN_COND_DIST/100.0  ! m
      ENDIF

! Setup code for conduction correction terms for artificial softening
      ! Calculate masses used for collision calculations.

! Shift the phase index for certain inputs to match the global phase
! index until this matter can be addressed otherwise (i.e., require
! the user specify correct indexing in project settings). This should have no
! impact if not running a hybrid case
      MMAX_TOT = DES_MMAX+MMAX
      e_young_actual((MMAX+1):MMAX_TOT) = e_young_actual(1:DES_MMAX)
      v_poisson_actual((MMAX+1):MMAX_TOT) = v_poisson_actual(1:DES_MMAX)
      DO_AREA_CORRECTION = .TRUE.
      DO M=MMAX+1,MMAX_TOT
         IF(.NOT.(SOLIDS_MODEL(M) == 'DEM'.OR.SOLIDS_MODEL(M) == 'CGP')) CYCLE
         IF(E_YOUNG_ACTUAL(M) == UNDEFINED) THEN
            MSG=''; WRITE(MSG,"('Phase ',I2,' Actual EYoungs')") M
            WRITE(ERR_MSG,2002) 'E_YOUNG_ACTUAL', MSG
            DO_AREA_CORRECTION = .FALSE.
            CALL LOG_WARNING()
         ENDIF
         IF(V_POISSON_ACTUAL(M) == UNDEFINED) THEN
            MSG=''; WRITE(MSG,"('Phase ',I2,' Actual Poisson''s ratio')") M
            WRITE(ERR_MSG,2002) 'V_POISSON_ACTUAL', MSG
            DO_AREA_CORRECTION = .FALSE.
            CALL LOG_WARNING()
         ELSEIF(V_POISSON_ACTUAL(M) > 0.5d0 .OR. &
                V_POISSON_ACTUAL(M) <= -ONE) THEN
            WRITE(ERR_MSG,1001) trim(iVar('V_POISSON_ACTUAL',M)),  &
               iVal(V_POISSON_ACTUAL(M))
            CALL LOG_ERROR()
         ENDIF
      ENDDO

      IF(EW_YOUNG_ACTUAL == UNDEFINED) THEN
         MSG=''; WRITE(MSG,"('Actual EYoungs (wall)')")
         WRITE(ERR_MSG,2002) 'EW_YOUNG_ACTUAL', MSG
         DO_AREA_CORRECTION = .FALSE.
         CALL LOG_WARNING()
      ENDIF
      IF(VW_POISSON_ACTUAL == UNDEFINED) THEN
         MSG=''; WRITE(MSG,"(' Actual Poisson''s ratio (wall)')")
         WRITE(ERR_MSG,2002) 'VW_POISSON_ACTUAL', MSG
         DO_AREA_CORRECTION = .FALSE.
         CALL LOG_WARNING()
      ENDIF

      DO M=MMAX+1,MMAX_TOT
         IF(.NOT.(SOLIDS_MODEL(M) == 'DEM'.OR.SOLIDS_MODEL(M) == 'CGP')) CYCLE
         IF(.NOT.DO_AREA_CORRECTION)CYCLE
! Calculate the mass of a phase M particle.
         ! MASS_M = (PI/6.d0)*(D_P0(M)**3)*RO_S0(M)
         MASS_M = REF_MASS(M)

! Particle-Particle Collision Parameters ------------------------------>
         DO L=M,MMAX_TOT
            ! MASS_L = (PI/6.d0)*(D_P0(L)**3)*RO_S0(L)
            MASS_L = REF_MASS(L)
            MASS_EFF = (MASS_M*MASS_L)/(MASS_M+MASS_L)

! Calculate the effective radius, Young's modulus, and shear modulus.
            ! R_EFF = 0.5d0*(D_P0(M)*D_P0(L)/(D_P0(M) + D_P0(L)))
            R_EFF = REF_RADIUS(M)*REF_RADIUS(L)/(REF_RADIUS(M)+REF_RADIUS(L))
            E_EFF = E_YOUNG_ACTUAL(M)*E_YOUNG_ACTUAL(L) /  &
            &  (E_YOUNG_ACTUAL(M)*(1.d0 - V_POISSON_ACTUAL(L)**2) + &
            &   E_YOUNG_ACTUAL(L)*(1.d0 - V_POISSON_ACTUAL(M)**2))

! Calculate the spring properties and store in symmetric matrix format.
            HERT_KN_ACTUAL(M,L)=(4.d0/3.d0)*SQRT(R_EFF)*E_EFF

! Compute baseline for Hertzian collision time
            TAU_C_BASE_ACTUAL(M,L)=3.21D0*(MASS_Eff/HERT_KN_ACTUAL(M,L))**0.4
            ! Can compute actual collision time via:
            !    TAU_C_ACTUAL = TAU_C_BASE_ACTUAL * (1/ImpactVel)^0.2

! Compute base for simulated collision time.  If Hertzian, only include
! base (without impact vel component).
            IF (DES_COLL_MODEL_ENUM .EQ. HERTZIAN)THEN
               TAU_C_BASE_SIM(M,L)=3.21D0*(MASS_Eff/HERT_KN(M,L))**0.4
            ELSE
               TAU_C_BASE_SIM(M,L)=PI/SQRT(KN/MASS_EFF - &
               ((DES_ETAN(M,L)/MASS_EFF)**2)/4.d0)
            ENDIF
         ENDDO

! Do particle-wall calculations
         MASS_EFF = MASS_M
         ! R_EFF = 0.5d0*D_P0(M)
         R_EFF = REF_RADIUS(M)
         E_EFF = E_YOUNG_ACTUAL(M)*EW_YOUNG_ACTUAL /  &
         &  (E_YOUNG_ACTUAL(M)*(1.d0 - VW_POISSON_ACTUAL**2) + &
         &   EW_YOUNG_ACTUAL*(1.d0 - V_POISSON_ACTUAL(M)**2))

         HERT_KWN_ACTUAL(M) = (4.d0/3.d0)*SQRT(R_EFF)*E_EFF
         TAUW_C_BASE_ACTUAL(M) = 3.21D0 * (MASS_Eff/HERT_KWN_ACTUAL(M))**0.4

         IF (DES_COLL_MODEL_ENUM .EQ. HERTZIAN)THEN
            TAUW_C_BASE_SIM(M)=3.21D0*(MASS_Eff/HERT_KWN(M))**0.4
         ELSE
            TAUW_C_BASE_SIM(M)=PI/SQRT(KN_w/MASS_EFF - &
            ((DES_ETAN_WALL(M)/MASS_EFF)**2)/4.d0)
         ENDIF

      ENDDO

      RETURN

 1000 FORMAT('Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

 1001 FORMAT('Illegal or unknown input: ',A,' = ',A,/      &
         'Please correct the project settings.')

 2002 FORMAT('Recommended input not specified: ',A,/       &
         'Description:',A,/'Not correcting contact area.')

 1201 FORMAT('WARNING 1201: Inconsistent settings: ',A,' = ',g14.4,/,&
        'with ',A,' = ',A, /,'Overwriting this value with zero.')

 1202 FORMAT('WARNING 1202: All K_s0(M)=0 but have requested a KS_MODEL(M) ',&
        'other than',/,'''NONE''. Suggest revising project settings so that ',&
        'KS_MODEL(M)=''NONE'' for ',/,'all M.')

 1203 FORMAT('ERROR 1203: If any KS_MODEL(M)=''NONE'', all must be NONE.')



      END SUBROUTINE CHECK_SOLIDS_DEM_ENERGY


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: CHECK_SOLIDS_DEM_COHESION                              !
!  Author: J.Musser                                   Date: 11-DEC-13  !
!                                                                      !
!  Purpose: Check/set parameters for DES cohesion models.              !
!                                                                      !
!  Comments: Original code moved from CHECK_DES_DATA                   !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_SOLIDS_DEM_COHESION

      IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//
! Neighborhood size for Van der Waals force.
      DOUBLE PRECISION :: VDW_NEIGHBORHOOD
!......................................................................!


! Override the following settings if cohesion not used.
      IF(.NOT.USE_COHESION) THEN
!No more square well in the code
         SQUARE_WELL = .FALSE.
         VAN_DER_WAALS = .FALSE.
         WALL_VDW_OUTER_CUTOFF = ZERO
         RETURN
      ENDIF

! Verify that only one cohesion model is specified.
      IF (SQUARE_WELL .AND. VAN_DER_WAALS) THEN
         WRITE(ERR_MSG,1002)
         CALL LOG_ERROR()

! Verify that at a cohesion model is specified.
      ELSEIF(.NOT.SQUARE_WELL .AND. .NOT.VAN_DER_WAALS) THEN
         WRITE(ERR_MSG,1003)
         CALL LOG_ERROR()
      ENDIF


! Van der Waals model checks.
      IF (VAN_DER_WAALS) THEN

         IF (VDW_INNER_CUTOFF .EQ. UNDEFINED) THEN
            WRITE(ERR_MSG,1201) 'VDW_INNER_CUTOFF'
            CALL LOG_ERROR()
         ENDIF

         IF(VDW_OUTER_CUTOFF .EQ. UNDEFINED) THEN
            WRITE(ERR_MSG,1201) 'VDW_OUTER_CUTOFF'
            CALL LOG_ERROR()
         ENDIF

         IF(HAMAKER_CONSTANT .EQ. UNDEFINED) THEN
            WRITE(ERR_MSG,1201) 'HAMAKER_CONSTANT'
            CALL LOG_ERROR()
         ENDIF

         IF (WALL_VDW_INNER_CUTOFF .EQ. UNDEFINED)THEN
            WRITE(ERR_MSG,1201) 'WALL_VDW_INNER_CUTOFF'
            CALL LOG_ERROR()
         ENDIF

         IF (WALL_VDW_OUTER_CUTOFF .EQ. UNDEFINED)THEN
            WRITE(ERR_MSG,1201) 'WALL_VDW_OUTER_CUTOFF'
            CALL LOG_ERROR()
         ENDIF

         IF(WALL_HAMAKER_CONSTANT .EQ. UNDEFINED) THEN
            WRITE(ERR_MSG,1201) 'WALL_HAMAKER_CONSTANT'
            CALL LOG_ERROR()
         ENDIF

         VDW_NEIGHBORHOOD = 1.0d0 + (VDW_OUTER_CUTOFF/(2.d0*MAX_RADIUS))
         IF (FACTOR_RLM < VDW_NEIGHBORHOOD) THEN
            WRITE(ERR_MSG,1202)
            CALL LOG_ERROR()
         ENDIF

         IF (ASPERITIES < ZERO) THEN
            WRITE(ERR_MSG,1001) 'ASPERITIES', trim(iVal(ASPERITIES))
            CALL LOG_ERROR()
         ENDIF

         SURFACE_ENERGY=HAMAKER_CONSTANT/&
            (24.d0*Pi*VDW_INNER_CUTOFF**2)

         WALL_SURFACE_ENERGY=WALL_HAMAKER_CONSTANT/&
            (24.d0*Pi*WALL_VDW_INNER_CUTOFF**2)

      ENDIF

      RETURN

1001  FORMAT('Illegal or unknown input: ',A, ' = ',A,/     &
         'Please correct the project settings.')

1002  FORMAT('Cannot use SQUARE_WELL and VAN_DER_WAALS ',  &
         'cohesion',/'models simultaneously.')

1003  FORMAT('A cohesion model was not selected. Specify ',&
         'one of the available models in the project settings.')


!<------------------- Van der Waals model messages. ----------------->!

1201  FORMAT('Missing input data for Van der Waals ',      &
         'cohesion model.',/'Input parameter ',A,' is UNDEFINED.')

1202  FORMAT('VDW_OUTER_CUTOFF outside of the neighbor ',  &
         'search distance.',/'Increase FACTOR_RLM to increase the ',   &
         'search distance.')

      END SUBROUTINE CHECK_SOLIDS_DEM_COHESION


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_SOLIDS_DEM_COLLISION                              !
!  Author: J.Musser                                   Date: 11-Dec-13  !
!                                                                      !
!  Purpose: Check user input data for DES collision calculations.      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_SOLIDS_DEM_COLLISION

      IMPLICIT NONE

! Check coefficient friction
      IF(MEW == UNDEFINED) THEN
         WRITE(ERR_MSG,1000) 'MEW'
         CALL LOG_ERROR()
      ELSEIF (MEW < ZERO .OR. MEW_W > ONE) THEN
         WRITE(ERR_MSG,1001) 'MEW', trim(iVal(MEW))
         CALL LOG_ERROR()
      ENDIF

      IF(MEW_W == UNDEFINED) THEN
         WRITE(ERR_MSG,1000) 'MEW_W'
         CALL LOG_ERROR()
      ELSEIF(MEW_w < ZERO .OR. MEW_W > ONE) THEN
         WRITE(ERR_MSG,1001) 'MEW_W', trim(iVal(MEW_W))
         CALL LOG_ERROR()
      ENDIF

! Check rolling coefficient friction
      IF(MEW_R == UNDEFINED) THEN
         WRITE(ERR_MSG,1000) 'MEW_R'
         CALL LOG_ERROR()
      ELSEIF (MEW_R < ZERO .OR. MEW_W > ONE) THEN
         WRITE(ERR_MSG,1001) 'MEW_R', trim(iVal(MEW_R))
         CALL LOG_ERROR()
      ENDIF

      IF(MEW_RW == UNDEFINED) THEN
         WRITE(ERR_MSG,1000) 'MEW_RW'
         CALL LOG_ERROR()
      ELSEIF(MEW_RW < ZERO .OR. MEW_W > ONE) THEN
         WRITE(ERR_MSG,1001) 'MEW_RW', trim(iVal(MEW_RW))
         CALL LOG_ERROR()
      ENDIF

      IF(DTSOLID_FACTOR == UNDEFINED) THEN
         WRITE(ERR_MSG,1000) 'DTSOLID_FACTOR'
         CALL LOG_ERROR()
      ELSEIF(DTSOLID_FACTOR < 10.0D0) THEN
         WRITE(ERR_MSG,1001) 'DT_SOLID_FACTOR', trim(iVal(DTSOLID_FACTOR))
         CALL LOG_ERROR()
      ENDIF

! Check collision model specific parameters.
      SELECT CASE (trim(DES_COLL_MODEL))
! Linear spring-dashpot model.
      CASE('LSD')
        DES_COLL_MODEL_ENUM = LSD
        CALL CHECK_SOLIDS_DEM_COLL_LSD
! Hertzian collision model.
      CASE('HERTZIAN')
         DES_COLL_MODEL_ENUM = HERTZIAN
         CALL CHECK_SOLIDS_DEM_COLL_HERTZ
! Unknown collision model.
      CASE DEFAULT
         WRITE(ERR_MSG,2000) TRIM(DES_COLL_MODEL)
         CALL LOG_ERROR()
      END SELECT

2000  FORMAT('Invalid particle-particle collision model:',&
         A,/'Please correct the project settings.')

      RETURN

1000  FORMAT('Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

1001  FORMAT('Illegal or unknown input: ',A,' = ',A,/      &
         'Please correct the project settings.')

      END SUBROUTINE CHECK_SOLIDS_DEM_COLLISION


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_SOLIDS_DEM_COLL_LSD                               !
!  Author: J.Musser                                   Date: 11-Dec-13  !
!                                                                      !
!  Purpose: Check user input data for DES collision calculations.      !
!                                                                      !
!  References:                                                         !
!   - Schafer et al., J. Phys. I France, 1996, 6, 5-20 (see page 7&13) !
!   -  Van der Hoef et al., Advances in Chemical Engineering, 2006, 31,!
!      65-149 (pages 94-95)                                            !
!   - Silbert et al., Physical Review E, 2001, 64, 051302 1-14 (page 5)!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_SOLIDS_DEM_COLL_LSD

      IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//
! Loop index.
      INTEGER :: M, L, LC, MMAX_TOT
! Calculate phase index offset for certain inputs until it can be
! addressed in other ways. should not matter unlesss hybrid
      INTEGER :: lent, lend, lenc
! Flag to warn user.
      LOGICAL :: FLAG_WARN1, FLAG_WARN2
! Collision length scale.
      DOUBLE PRECISION :: TCOLL, TCOLL_TMP
! Collision length scale.
      DOUBLE PRECISION :: MASS_M, MASS_L, MASS_EFF
! Alias for coefficient restitution
      DOUBLE PRECISION :: EN
!......................................................................!

! Initialize.
      TCOLL = UNDEFINED

! Check for particle-particle normal spring constants.
      IF(KN == UNDEFINED) THEN
         WRITE(ERR_MSG, 1000) 'KN'
         CALL LOG_ERROR()
      ENDIF

! Check for particle-particle tangential spring constant factors.
      IF(KT_FAC == UNDEFINED) THEN
         WRITE (ERR_MSG, 2100) 'KT_FAC'
         CALL LOG_WARNING()
         KT_FAC = 2.0d0/7.0d0
      ELSEIF(KT_FAC > ONE .OR. KT_FAC < ZERO) THEN
         WRITE(ERR_MSG,1001) 'KT_FAC', trim(iVal(KT_FAC))
         CALL LOG_ERROR()
      ENDIF
! Calculate the particle-particle tangential spring factor.
      KT = KT_FAC*KN

! Check for particle-wall normal spring constants.
      IF(KN_W == UNDEFINED) THEN
         WRITE(ERR_MSG, 1000) 'KN_W'
         CALL LOG_ERROR()
      ENDIF

! Check for particle-wall tangential spring constant factors.
      IF(KT_W_FAC == UNDEFINED) THEN
         WRITE (ERR_MSG, 2100) 'KT_W_FAC'
         CALL LOG_WARNING()
         KT_W_FAC = 2.0d0/7.0d0
      ELSEIF(KT_W_FAC > ONE .OR. KT_W_FAC < ZERO) THEN
         WRITE(ERR_MSG,1001) 'KT_W_FAC', trim(iVal(KT_W_FAC))
         CALL LOG_ERROR()
      ENDIF
! Calculate the particle-wall tangential spring factor.
      KT_W = KT_W_FAC*KN_W

2100  FORMAT('Tangential spring factor ',A,' not ',      &
         'specified in project settings.',/'Setting to default: (2/7).')

! Check for particle-particle tangential damping coefficients
      IF(DES_ETAT_FAC == UNDEFINED) THEN
         WRITE (ERR_MSG, 2101) 'DES_ETAT_FAC'
         CALL LOG_WARNING()
         DES_ETAT_FAC = HALF
      ELSEIF(DES_ETAT_FAC > ONE .OR. DES_ETAT_FAC < ZERO) THEN
         WRITE(ERR_MSG,1001) 'DES_ETAT_FAC', iVal(DES_ETAT_FAC)
         CALL LOG_ERROR()
      ENDIF

! Check for particle-wall tangential damping coefficients
      IF(DES_ETAT_W_FAC == UNDEFINED) THEN
         WRITE (ERR_MSG, 2101) 'DES_ETAT_W_FAC'
         CALL LOG_WARNING()
         DES_ETAT_W_FAC = HALF
      ELSEIF(DES_ETAT_W_FAC > ONE .OR. DES_ETAT_W_FAC < ZERO) THEN
         WRITE(ERR_MSG,1001) 'DES_ETAT_W_FAC', iVal(DES_ETAT_W_FAC)
         CALL LOG_ERROR()
      ENDIF

2101  FORMAT('Tangential damping factor ',A,' not ', &
         'specified, setting to default: (1/2).')

! Shift the phase index for certain inputs to match the global phase
! index until this matter can be addressed otherwise (i.e., require
! the user specify correct indexing in project settings). This should have no
! impact if not running a hybrid case
      MMAX_TOT = DES_MMAX+MMAX
      des_en_wall_input((MMAX+1):MMAX_TOT) = des_en_wall_input(1:DES_MMAX)
      des_et_wall_input((MMAX+1):MMAX_TOT) = des_et_wall_input(1:DES_MMAX)
      lent = MMAX_TOT+MMAX_TOT*(MMAX_TOT-1)/2
      lend = DES_MMAX+DES_MMAX*(DES_MMAX-1)/2
      lenc = lent-lend
      des_en_input((lenc+1):lent) = des_en_input(1:lend)
      des_et_input((lenc+1):lent) = des_et_input(1:lend)
      LC = lenc

! if below indicated quantities are assigned warn user they are discarded
      FLAG_WARN1 = .FALSE.
      FLAG_WARN2 = .FALSE.

      DO M = MMAX+1, MMAX_TOT
         IF(.NOT.(SOLIDS_MODEL(M) == 'DEM'.OR.SOLIDS_MODEL(M) == 'CGP')) CYCLE

! Calculate the mass of a phase M particle.
         ! MASS_M = (PI/6.d0)*(D_P0(M)**3)*RO_S0(M)
         MASS_M = REF_MASS(M)

! Particle-Particle Collision Parameters ------------------------------>
         DO L = M, MMAX_TOT

! not necessary to cycle here given requirements on ordering of solids input
! (all dem phase indices are contiguous)
            LC = LC+1

! Check particle-particle normal restitution coefficient
            IF(DES_EN_INPUT(LC) == UNDEFINED) THEN
               WRITE(ERR_MSG,1000) trim(iVar('DES_EN_INPUT',LC))
               CALL LOG_ERROR()
            ELSEIF(DES_EN_INPUT(LC) > ONE .OR. &
                   DES_EN_INPUT(LC) < ZERO) THEN
               WRITE(ERR_MSG,1001) trim(iVar('DES_EN_INPUT',LC)), &
                  trim(iVal(DES_EN_INPUT(LC)))
               CALL LOG_ERROR()
            ENDIF
            EN = DES_EN_INPUT(LC)

! warn user that particle-particle tangential restitution coefficient
! is discarded for this model
            IF(DES_ET_INPUT(LC) .NE. UNDEFINED) FLAG_WARN1 = .TRUE.

! Calculate masses used for collision calculations.
            ! MASS_L = (PI/6.d0)*(D_P0(L)**3)*RO_S0(L)
            MASS_L = REF_MASS(L)
            MASS_EFF = MASS_M*MASS_L/(MASS_M+MASS_L)

! Calculate the M-L normal and tangential damping coefficients.
            IF(EN .NE. ZERO) THEN
               DES_ETAN(M,L) = 2.0D0*SQRT(KN*MASS_EFF) * ABS(LOG(EN))
               DES_ETAN(M,L) = DES_ETAN(M,L)/SQRT(PI*PI + (LOG(EN)**2))
            ELSE
               DES_ETAN(M,L) = 2.0D0*SQRT(KN*MASS_EFF)
            ENDIF
            DES_ETAT(M,L) = DES_ETAT_FAC*DES_ETAN(M,L)

! Store the entries in the symmetric matrix.
            DES_ETAN(L,M) = DES_ETAN(M,L)
            DES_ETAT(L,M) = DES_ETAT(M,L)

! Calculate the collision time scale.
            TCOLL_TMP = PI/SQRT(KN/MASS_EFF - &
               ((DES_ETAN(M,L)/MASS_EFF)**2)/4.d0)
            TCOLL = MIN(TCOLL_TMP, TCOLL)
         ENDDO


! Particle-Wall Collision Parameters ---------------------------------->
! Check particle-wall normal restitution coefficient.
         IF(DES_EN_WALL_INPUT(M) == UNDEFINED) THEN
            WRITE(ERR_MSG,1000) trim(iVar('DES_EN_WALL_INPUT',M))
            CALL LOG_ERROR()
         ELSEIF(DES_EN_WALL_INPUT(M) > ONE .OR. &
            DES_EN_WALL_INPUT(M) < ZERO) THEN
            WRITE(ERR_MSG,1001) trim(iVar('DES_EN_WALL_INPUT',M)), &
               trim(iVal(DES_EN_WALL_INPUT(M)))
            CALL LOG_ERROR()
         ENDIF
         EN = DES_EN_WALL_INPUT(M)

         IF(DES_ET_WALL_INPUT(M) .NE. UNDEFINED) FLAG_WARN2 = .TRUE.

! Calculate masses used for collision calculations.
         MASS_EFF = MASS_M

! Calculate the M-Wall normal and tangential damping coefficients.
         IF(EN .NE. ZERO) THEN
            DES_ETAN_WALL(M) = 2.d0*SQRT(KN_W*MASS_EFF)*ABS(LOG(EN))
            DES_ETAN_WALL(M) = DES_ETAN_WALL(M)/SQRT(PI*PI+(LOG(EN))**2)
         ELSE
            DES_ETAN_WALL(M) = 2.D0*SQRT(KN_W*MASS_EFF)
         ENDIF
         DES_ETAT_WALL(M) = DES_ETAT_W_FAC*DES_ETAN_WALL(M)

! Calculate the collision time scale.
         TCOLL_TMP = PI/SQRT(KN_W/MASS_EFF -                           &
            ((DES_ETAN_WALL(M)/MASS_EFF)**2.d0)/4.d0)
!         TCOLL = MIN(TCOLL_TMP, TCOLL)
      ENDDO

      IF (FLAG_WARN1) THEN
         WRITE(ERR_MSG,2102) 'DES_ET_INPUT'
         CALL LOG_WARNING()
      ENDIF
      IF (FLAG_WARN2)THEN
         WRITE(ERR_MSG,2102) 'DES_ET_WALL_INPUT'
         CALL LOG_WARNING()
      ENDIF

2102  FORMAT(A,' values are not used ',/' with the',  &
         ' linear spring-dashpot collision model.')

! Store the smalled calculated collision time scale. This value is used
! in time-marching the DEM solids.
      DTSOLID = TCOLL/DTSOLID_FACTOR

      WRITE(ERR_MSG,1100) TIME, DTSOLID
      CALL LOG_INFO()


      RETURN

1000  FORMAT('Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

1001  FORMAT('Illegal or unknown input: ',A,' = ',A,/      &
         'Please correct the project settings.')
1100  FORMAT('Info: At time = ',E14.6, ' sec., setting DEM solids time step, DTSOLID (sec) = ', E14.6)

      END SUBROUTINE CHECK_SOLIDS_DEM_COLL_LSD


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_SOLIDS_DEM_COLL_HERTZ                             !
!  Author: J.Musser                                   Date: 11-Dec-13  !
!                                                                      !
!  Purpose: Check user input data for Hertzian collisions.             !
!                                                                      !
!  References:                                                         !
!   - Schafer et al., J. Phys. I France, 1996, 6, 5-20 (see page 7&13) !
!   -  Van der Hoef et al., Advances in Chemical Engineering, 2006, 31,!
!      65-149 (pages 94-95)                                            !
!   - Silbert et al., Physical Review E, 2001, 64, 051302 1-14 (page 5)!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_SOLIDS_DEM_COLL_HERTZ

      IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//
! Loop index.
      INTEGER :: M, L, LC, MMAX_TOT
! Calculate phase index offset for certain inputs until it can be
! addressed in other ways. should not matter unlesss hybrid
      INTEGER :: lent, lend, lenc
! Message for formatted output.
      CHARACTER(len=64) :: MSG
! Collision length scale.
      DOUBLE PRECISION :: TCOLL, TCOLL_TMP
! Particle and effective mass.
      DOUBLE PRECISION :: MASS_M, MASS_L, MASS_EFF
! Effective physical quantities. Radius, Young's, Shear
      DOUBLE PRECISION :: R_EFF, E_EFF, G_MOD_EFF, RED_MASS_EFF
! Alias for coefficient restitution
      DOUBLE PRECISION :: EN, ET
! shear modulus
      DOUBLE PRECISION :: G_MOD(DIM_M)
! Shear modules for wall
      DOUBLE PRECISION :: G_MOD_WALL
!......................................................................!

! Initialize.
      TCOLL = UNDEFINED

! check young's modulus and poisson ratio
      IF(Ew_YOUNG == UNDEFINED ) THEN
         MSG="Wall value for Young's modulus"
         WRITE(ERR_MSG,1002) 'Ew_YOUNG', MSG
         CALL LOG_ERROR()
      ENDIF

      IF(Vw_POISSON == UNDEFINED) THEN
         MSG="Wall value for Poisson's ratio"
         WRITE(ERR_MSG,1002) 'Vw_POISSON', MSG
         CALL LOG_ERROR()
      ELSEIF (Vw_POISSON > 0.5d0 .OR. Vw_POISSON <= -ONE) THEN
         WRITE(ERR_MSG,1001) 'Vw_POISSON',iVal(Vw_POISSON)
         CALL LOG_ERROR()
      ENDIF

      G_MOD_WALL = 0.5d0*Ew_YOUNG/(1.d0+Vw_POISSON)

! Shift the phase index for certain inputs to match the global phase
! index until this matter can be addressed otherwise (i.e., require
! the user specify correct indexing in project settings). This should have no
! impact if not running a hybrid case
      MMAX_TOT = DES_MMAX+MMAX
      e_young((MMAX+1):MMAX_TOT) = e_young(1:DES_MMAX)
      v_poisson((MMAX+1):MMAX_TOT) = v_poisson(1:DES_MMAX)
      des_en_wall_input((MMAX+1):MMAX_TOT) = des_en_wall_input(1:DES_MMAX)
      des_et_wall_input((MMAX+1):MMAX_TOT) = des_et_wall_input(1:DES_MMAX)
      lent = MMAX_TOT+MMAX_TOT*(MMAX_TOT-1)/2
      lend = DES_MMAX+DES_MMAX*(DES_MMAX-1)/2
      lenc = lent-lend
      des_en_input((lenc+1):lent) = des_en_input(1:lend)
      des_et_input((lenc+1):lent) = des_et_input(1:lend)

      DO M=MMAX+1,MMAX_TOT
         IF(.NOT.(SOLIDS_MODEL(M) == 'DEM'.OR.SOLIDS_MODEL(M) == 'CGP')) CYCLE

         IF(E_YOUNG(M) == UNDEFINED) THEN
            MSG=''; WRITE(MSG,"('Phase ',I2,' Young''s modulus')") M
            WRITE(ERR_MSG,1002) 'E_YOUNG', MSG
            CALL LOG_ERROR()
         ENDIF
         IF(V_POISSON(M) == UNDEFINED) THEN
            MSG=''; WRITE(MSG,"('Phase ',I2,' Poisson''s ratio')") M
            WRITE(ERR_MSG,1002) 'V_POISSON', MSG
            CALL LOG_ERROR()
         ELSEIF(V_POISSON(M) > 0.5d0 .OR. &
                V_POISSON(M) <= -ONE) THEN
            WRITE(ERR_MSG,1001) trim(iVar('V_POISSON',M)),  &
               iVal(V_POISSON(M))
            CALL LOG_ERROR()
         ENDIF
! Calculate the shear modulus for phase M.
         G_MOD(M) = 0.5d0*E_YOUNG(M)/(1.d0+V_POISSON(M))
      ENDDO

! see above index shift
      LC = LENC
      DO M=MMAX+1,MMAX_TOT
         IF(.NOT.(SOLIDS_MODEL(M) == 'DEM'.OR.SOLIDS_MODEL(M) == 'CGP')) CYCLE

! Calculate the mass of a phase M particle.
         ! MASS_M = (PI/6.d0)*(D_P0(M)**3)*RO_S0(M)
         MASS_M = REF_MASS(M)

! Particle-Particle Collision Parameters ------------------------------>
         DO L=M,MMAX_TOT
! not necessary to cycle here given requirements on ordering of solids input
            LC = LC+1

! Check particle-particle normal restitution coefficient
            IF(DES_EN_INPUT(LC) == UNDEFINED) THEN
               WRITE(ERR_MSG,1000) trim(iVar('DES_EN_INPUT',LC))
               CALL LOG_ERROR()
            ELSEIF(DES_EN_INPUT(LC) > ONE .OR. &
               DES_EN_INPUT(LC) < ZERO) THEN
               WRITE(ERR_MSG,1001) trim(iVar('DES_EN_INPUT',LC)), &
                  trim(iVal(DES_EN_INPUT(LC)))
               CALL LOG_ERROR()
            ENDIF
            EN = DES_EN_INPUT(LC)

! Check particle-particle tangential restitution coefficient
            IF(DES_ET_INPUT(LC) == UNDEFINED) THEN
               WRITE(ERR_MSG,1000) trim(iVar('DES_ET_INPUT',LC))
               CALL LOG_ERROR()
            ELSEIF(DES_ET_INPUT(LC) > ONE .OR. &
                   DES_ET_INPUT(LC) < ZERO) THEN
               WRITE(ERR_MSG,1001) trim(iVar('DES_ET_INPUT',LC)), &
                  iVal(DES_ET_INPUT(LC))
               CALL LOG_ERROR()
            ENDIF
            ET = DES_ET_INPUT(LC)


! Calculate masses used for collision calculations.
            ! MASS_L = (PI/6.d0)*(D_P0(L)**3)*RO_S0(L)
            MASS_L = REF_MASS(L)
            MASS_EFF = (MASS_M*MASS_L)/(MASS_M+MASS_L)
            RED_MASS_EFF = (2.d0/7.d0)*MASS_EFF
! Calculate the effective radius, Young's modulus, and shear modulus.
            ! R_EFF = 0.5d0*(D_P0(M)*D_P0(L)/(D_P0(M) + D_P0(L)))
            R_EFF = REF_RADIUS(M)*REF_RADIUS(L)/(REF_RADIUS(M)+REF_RADIUS(L))
            E_EFF = E_YOUNG(M)*E_YOUNG(L) /  &
               (E_YOUNG(M)*(1.d0 - V_POISSON(L)**2) + &
                E_YOUNG(L)*(1.d0 - V_POISSON(M)**2))
            G_MOD_EFF = G_MOD(M)*G_MOD(L)/ &
               (G_MOD(M)*(2.d0 - V_POISSON(L)) + &
                G_MOD(L)*(2.d0 - V_POISSON(M)))

! Calculate the spring properties and store in symmetric matrix format.
            HERT_KN(M,L)=(4.d0/3.d0)*SQRT(R_EFF)*E_EFF
            HERT_KT(M,L)= 8.d0*SQRT(R_EFF)*G_MOD_EFF

            HERT_KN(L,M) = HERT_KN(M,L)
            HERT_KT(L,M) = HERT_KT(M,L)

! Calculate the normal damping coefficient.
            IF(EN .NE. ZERO) THEN
               DES_ETAN(M,L) = 2.d0*SQRT(HERT_KN(M,L)*MASS_EFF)* &
                  ABS(LOG(EN))
               DES_ETAN(M,L) = DES_ETAN(M,L)/ &
                  SQRT(PI*PI + (LOG(EN))**2)
            ELSE
               DES_ETAN(M,L) = 2.d0*SQRT(HERT_KN(M,L)*MASS_EFF)
            ENDIF
            DES_ETAN(L,M) = DES_ETAN(M,L)

! Calculate the tangential coefficients.
            IF(ET .NE. ZERO) THEN
               DES_ETAT(M,L) = 2.d0*SQRT(HERT_KT(M,L)*RED_MASS_EFF)* &
                  ABS(LOG(ET))
               DES_ETAT(M,L) = DES_ETAT(M,L)/ SQRT(PI*PI+(LOG(ET))**2)
            ELSE
               DES_ETAT(M,L) = 2.d0*SQRT(HERT_KT(M,L)*RED_MASS_EFF)
            ENDIF
            DES_ETAT(L,M) = DES_ETAT(M,L)

            TCOLL_TMP = PI/SQRT(HERT_KN(M,L)/MASS_EFF - &
               ((DES_ETAN(M,L)/MASS_EFF)**2)/4.d0)
            TCOLL = MIN(TCOLL_TMP, TCOLL)
         ENDDO

! Particle-Wall Collision Parameters ---------------------------------->
! Check particle-wall normal restitution coefficient.
         IF(DES_EN_WALL_INPUT(M) == UNDEFINED) THEN
            WRITE(ERR_MSG,1000) trim(iVar('DES_EN_WALL_INPUT',M))
            CALL LOG_ERROR()
         ELSEIF(DES_EN_WALL_INPUT(M) > ONE .OR. &
                DES_EN_WALL_INPUT(M) < ZERO) THEN
            WRITE(ERR_MSG,1001) trim(iVar('DES_EN_WALL_INPUT',M)), &
               trim(iVal(DES_EN_WALL_INPUT(M)))
            CALL LOG_ERROR()
         ENDIF
         EN = DES_EN_WALL_INPUT(M)

! Check particle-wall tangential restitution coefficient
         IF(DES_ET_WALL_INPUT(M) == UNDEFINED) THEN
            WRITE(ERR_MSG,1000) trim(iVar('DES_ET_WALL_INPUT',M))
            CALL LOG_ERROR()
         ELSEIF(DES_ET_WALL_INPUT(M) > ONE .OR. &
                DES_ET_WALL_INPUT(M) < ZERO) THEN
            WRITE(ERR_MSG,1001) trim(iVar('DES_ET_WALL_INPUT',M)), &
               trim(iVal(DES_ET_WALL_INPUT(M)))
            CALL LOG_ERROR()
         ENDIF
         ET = DES_ET_WALL_INPUT(M)

! Calculate masses used for collision calculations.
         MASS_EFF = MASS_M
         RED_MASS_EFF = (2.d0/7.d0)*MASS_EFF
! Calculate the effective radius, Young's modulus, and shear modulus.
         ! R_EFF = 0.5d0*D_P0(M)
         R_EFF = REF_RADIUS(M)
         E_EFF = E_YOUNG(M)*Ew_YOUNG /                                 &
            (E_YOUNG(M)*(1.d0-Vw_POISSON**2) +                         &
             Ew_YOUNG  *(1.d0-V_POISSON(M)**2))
         G_MOD_EFF = G_MOD(M)*G_MOD_WALL /                             &
            (G_MOD(M)*(2.d0 - Vw_POISSON) +                            &
             G_MOD_WALL*(2.d0 - V_POISSON(M)))

! Calculate the spring properties.
         HERT_Kwn(M) = (4.d0/3.d0)*SQRT(R_EFF)*E_EFF
         HERT_Kwt(M) = 8.0*SQRT(R_EFF)*G_MOD_EFF

! Calculate the tangential coefficients.
         IF(EN /= ZERO) THEN
            DES_ETAN_WALL(M) = 2.d0*SQRT(HERT_Kwn(M)*MASS_EFF)*&
               ABS(LOG(EN))
            DES_ETAN_WALL(M) = DES_ETAN_WALL(M)/&
               SQRT(PI*PI + (LOG(EN))**2)
         ELSE
            DES_ETAN_WALL(M) = 2.d0*SQRT(HERT_Kwn(M)*MASS_EFF)
         ENDIF

         IF(ET /= ZERO) THEN
            DES_ETAT_WALL(M) = 2.d0*SQRT(HERT_Kwt(M)*RED_MASS_EFF)*    &
                ABS(LOG(ET))
            DES_ETAT_WALL(M) = DES_ETAT_WALL(M)/SQRT(PI*PI+(LOG(ET))**2)
         ELSE
            DES_ETAT_WALL(M) = 2.d0*SQRT(HERT_Kwt(M)*RED_MASS_EFF)
         ENDIF

! Calculate the collision time scale.
         TCOLL_TMP = PI/SQRT(HERT_Kwn(M)/MASS_EFF -                    &
            ((DES_ETAN_WALL(M)/MASS_EFF)**2)/4.d0)
      ENDDO


! Store the smalled calculated collision time scale. This value is used
! in time-marching the DEM solids.
      DTSOLID = TCOLL/DTSOLID_FACTOR

      RETURN

1000  FORMAT('Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

1001  FORMAT('Illegal or unknown input: ',A,' = ',A,/      &
         'Please correct the project settings.')

1002  FORMAT('Required input not specified: ',A,/          &
         'Description:',A,/'Please correct the project settings.')

   END SUBROUTINE CHECK_SOLIDS_DEM_COLL_HERTZ

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: GET_REF_PARTICLE_SIZE_AND_MASS                          !
!  Author: J.-F. Dietiker                             Date: 11-Sep-20  !
!                                                                      !
!  Purpose: Get reference particle size and mass for each DEM phase    !
!           This will be used to compute the DTSOLID at the beginning  !
!           of the simulation and at regular intervals (when particle  !
!           size/mass changes in time.                                 !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE GET_REF_PARTICLE_SIZE_AND_MASS

      use functions, only: IS_NORMAL
      USE mpi_utility, only: global_all_min
      use run, only: RUN_TYPE, TIME

      IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//
! Loop index.
      INTEGER :: M, L, LC, MMAX_TOT, NP
! Reference radius and mass used to compute solids time step
      DOUBLE PRECISION :: lradius_min(DIM_M), lmass_min(DIM_M)
! Message for formatted output.
      CHARACTER(len=64) :: MSG
!......................................................................!

! Initialize with baseline values.

      MMAX_TOT = DES_MMAX+MMAX

      DO M=MMAX+1,MMAX_TOT
         IF(.NOT.(SOLIDS_MODEL(M) == 'DEM'.OR.SOLIDS_MODEL(M) == 'CGP')) CYCLE

         REF_RADIUS(M) = HALF * D_P0(M)
         REF_MASS(M)   = (PI/6.d0)*(D_P0(M)**3)*RO_S0(M)

      ENDDO


      IF(PIP>0) THEN

! Get the min particle size for each solids phase
         lradius_min(MMAX+1: MMAX_TOT) = REF_RADIUS(MMAX+1: MMAX_TOT) !UNDEFINED
         lmass_min(MMAX+1: MMAX_TOT) = REF_MASS(MMAX+1: MMAX_TOT) !UNDEFINED

          DO NP=1,MAX_PIP
             IF(.NOT.IS_NORMAL(NP)) CYCLE
! Solids phase
                M              = PIJK(NP,5)
                lradius_min(M) = MIN(lradius_min(M),DES_RADIUS(NP))
                lmass_min(M)   = MIN(lmass_min(M),PMASS(NP))
          ENDDO


          call global_all_min(lradius_min(MMAX+1: MMAX_TOT), REF_RADIUS(MMAX+1: MMAX_TOT))
          call global_all_min(lmass_min(MMAX+1: MMAX_TOT), REF_MASS(MMAX+1: MMAX_TOT))


      ENDIF

               ! WRITE(ERR_MSG,1000) trim(iVar('DES_ET_INPUT',LC))
               ! CALL LOG_ERROR()
      RETURN

1000  FORMAT('Required input not specified: ',A,/'Please ',&
         'correct the project settings.')


   END SUBROUTINE GET_REF_PARTICLE_SIZE_AND_MASS


   SUBROUTINE UPDATE_DTSOLID

   use run, only: time
   IMPLICIT NONE


   IF(DTSOLID_UPDATE_DT<=ZERO.OR.DTSOLID_UPDATE_DT==UNDEFINED) RETURN
   IF(DTSOLID_UPDATE_TIME<=ZERO) RETURN

! Update DEM time step
   IF(TIME>=DTSOLID_UPDATE_TIME) THEN
      CALL CHECK_SOLIDS_DEM

! Set time for the next update
      DTSOLID_UPDATE_TIME = DTSOLID_UPDATE_TIME + DTSOLID_UPDATE_DT

   ENDIF


   RETURN
   END SUBROUTINE UPDATE_DTSOLID


END MODULE CHECK_SOLIDS_DEM_MOD
