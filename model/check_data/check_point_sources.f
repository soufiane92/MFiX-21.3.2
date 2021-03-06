#include "error.inc"

MODULE CHECK_POINT_SOURCES_MOD

   use error_manager
   use get_ps_mod, only: get_ps

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: CHECK_POINT_SOURCES                                     !
!  Author: J. Musser                                  Date: 10-JUN-13  !
!                                                                      !
!  Purpose: Check point source specifications.                         !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_POINT_SOURCES

! Global Variables:
!---------------------------------------------------------------------//
! Flag: PS geometry was detected.
      use ps, only: PS_DEFINED

! Global Parameters:
!---------------------------------------------------------------------//
! Maximum number of PS.
      use param, only: DIMENSION_PS

! Skip data check when doing pre-processing only
      USE run, only:ppo

      implicit none

! Local Variables:
!---------------------------------------------------------------------//
! Loop counter for BCs
      INTEGER :: PSV
!......................................................................!

      IF(PPO) RETURN

! Determine which PSs are DEFINED
      CALL CHECK_PS_GEOMETRY

! Loop over all PS arrays.
      DO PSV = 1, DIMENSION_PS

! Verify user input for defined defined PS.
         IF(PS_DEFINED(PSV)) THEN
            CALL GET_PS(PSV)
            CALL CHECK_PS_GAS_PHASE(PSV)
            CALL CHECK_PS_SOLIDS_PHASES(PSV)
         ELSE
! Verify that no data was defined for unspecified PS.
            CALL CHECK_PS_OVERFLOW(PSV)
         ENDIF
      ENDDO

      RETURN
      END SUBROUTINE CHECK_POINT_SOURCES


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: CHECK_PS_GEOMETRY                                       !
!  Author: J. Musser                                  Date: 10-JUN-13  !
!                                                                      !
!  Purpose: Check point source specifications.                         !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_PS_GEOMETRY


! Global Variables:
!---------------------------------------------------------------------//
! Flag: PS contains geometric data and/or specified type
      use ps, only: PS_DEFINED, POINT_SOURCE
! User specified: PS geometry
      use ps, only: PS_X_e, PS_X_w, PS_I_e, PS_I_w
      use ps, only: PS_Y_n, PS_Y_s, PS_J_n, PS_J_s
      use ps, only: PS_Z_t, PS_Z_b, PS_K_t, PS_K_b
! User specified: System geometry
      use geometry, only: NO_I
      use geometry, only: NO_J
      use geometry, only: NO_K
      use geometry, only: X_MIN, X_MAX, Y_MIN, Y_MAX, Z_MIN, Z_MAX

! Global Parameters:
!---------------------------------------------------------------------//
! The max number of BCs.
      use param, only: DIMENSION_PS
! Parameter constants
      use param1, only: ZERO, UNDEFINED, UNDEFINED_I

      implicit none

! Local Variables:
!---------------------------------------------------------------------//
! PS loop counter.
      INTEGER :: PSV
!......................................................................!

! Initialize the PS runtime flag.
      POINT_SOURCE = .FALSE.
      PS_DEFINED(:) = .FALSE.

! Determine which point source indices have values.
      PSV_LP: do PSV = 1, DIMENSION_PS

         IF (PS_X_W(PSV) /= UNDEFINED)   PS_DEFINED(PSV) = .TRUE.
         IF (PS_X_E(PSV) /= UNDEFINED)   PS_DEFINED(PSV) = .TRUE.
         IF (PS_Y_S(PSV) /= UNDEFINED)   PS_DEFINED(PSV) = .TRUE.
         IF (PS_Y_N(PSV) /= UNDEFINED)   PS_DEFINED(PSV) = .TRUE.
         IF (PS_Z_B(PSV) /= UNDEFINED)   PS_DEFINED(PSV) = .TRUE.
         IF (PS_Z_T(PSV) /= UNDEFINED)   PS_DEFINED(PSV) = .TRUE.
         IF (PS_I_W(PSV) /= UNDEFINED_I) PS_DEFINED(PSV) = .TRUE.
         IF (PS_I_E(PSV) /= UNDEFINED_I) PS_DEFINED(PSV) = .TRUE.
         IF (PS_J_S(PSV) /= UNDEFINED_I) PS_DEFINED(PSV) = .TRUE.
         IF (PS_J_N(PSV) /= UNDEFINED_I) PS_DEFINED(PSV) = .TRUE.
         IF (PS_K_B(PSV) /= UNDEFINED_I) PS_DEFINED(PSV) = .TRUE.
         IF (PS_K_T(PSV) /= UNDEFINED_I) PS_DEFINED(PSV) = .TRUE.

! Skip consistency checks if nothing was defined.
         IF (.NOT.PS_DEFINED(PSV)) cycle PSV_LP

! Flag that one or more point sources has been detected.
         POINT_SOURCE = .TRUE.

         IF(PS_X_W(PSV)==UNDEFINED .AND. PS_I_W(PSV)==UNDEFINED_I) THEN
            IF(NO_I) THEN
               PS_X_W(PSV) = X_MIN
            ELSE
               WRITE(ERR_MSG,1101) PSV, 'PS_X_w and PS_I_w '
               CALL LOG_ERROR()
            ENDIF
         ENDIF
         IF(PS_X_E(PSV)==UNDEFINED .AND. PS_I_E(PSV)==UNDEFINED_I) THEN
            IF(NO_I) THEN
               PS_X_E(PSV) = X_MAX
            ELSE
               WRITE(ERR_MSG,1101) PSV, 'PS_X_e and PS_I_e '
               CALL LOG_ERROR()
            ENDIF
         ENDIF
         IF(PS_Y_S(PSV)==UNDEFINED .AND. PS_J_S(PSV)==UNDEFINED_I) THEN
            IF(NO_J) THEN
               PS_Y_S(PSV) = Y_MIN
            ELSE
               WRITE(ERR_MSG,1101) PSV, 'PS_Y_s and PS_J_s '
               CALL LOG_ERROR()
            ENDIF
         ENDIF
         IF(PS_Y_N(PSV)==UNDEFINED .AND. PS_J_N(PSV)==UNDEFINED_I) THEN
            IF(NO_J) THEN
               PS_Y_N(PSV) = Y_MAX
            ELSE
               WRITE(ERR_MSG,1101) PSV, 'PS_Y_n and PS_J_n '
               CALL LOG_ERROR()
            ENDIF
         ENDIF
         IF(PS_Z_B(PSV)==UNDEFINED .AND. PS_K_B(PSV)==UNDEFINED_I) THEN
            IF(NO_K) THEN
               PS_Z_B(PSV) = Z_MIN
            ELSE
               WRITE(ERR_MSG,1101) PSV, 'PS_Z_b and PS_K_b '
               CALL LOG_ERROR()
            ENDIF
         ENDIF
         IF(PS_Z_T(PSV)==UNDEFINED .AND. PS_K_T(PSV)==UNDEFINED_I) THEN
            IF(NO_K) THEN
               PS_Z_T(PSV) = Z_MAX
            ELSE
               WRITE(ERR_MSG,1101) PSV, 'PS_Z_t and PS_K_t '
               CALL LOG_ERROR()
            ENDIF
         ENDIF

 1101 FORMAT('Error 1101: Point source ',I3,' is ill-defined.',/A,     &
         ' are not specified.',/'Please correct the project settings.')

      ENDDO PSV_LP

      RETURN
      END SUBROUTINE CHECK_PS_GEOMETRY


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: CHECK_PS_GAS_PHASE                                      !
!  Author: J. Musser                                  Date: 10-JUN-13  !
!                                                                      !
!  Purpose: Check point source specifications.                         !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_PS_GAS_PHASE(PSV)

! Global Variables:
!---------------------------------------------------------------------//
! Gas phase mass flowrate for PS
      use ps, only: PS_MASSFLOW_G
! Gas phase velocity for PS
      use ps, only: PS_U_g, PS_V_g, PS_W_g
! Gas phase temperature and species mass fractions
      use ps, only: PS_T_g, PS_X_g
! Flag: Solve energy equations.
      use run, only: ENERGY_EQ
! Flag: Solve species equations.
      use run, only: SPECIES_EQ
! Number of species.
      use physprop, only: NMAX

! Global Parameters:
!---------------------------------------------------------------------//
! Parameter constants
      use param1, only: ZERO, ONE, UNDEFINED

      use toleranc

      implicit none

! Dummy Arguments:
!---------------------------------------------------------------------//
      INTEGER, INTENT(in) :: PSV

! Local Variables:
!---------------------------------------------------------------------//
! Loop counter
      INTEGER :: N
! Sum of solids mass fractions.
      DOUBLE PRECISION :: SUM
!......................................................................!

! Check mass flow and velocity
      IF(PS_MASSFLOW_G(PSV) == UNDEFINED) THEN
         IF(PS_U_g(PSV) /= UNDEFINED .OR. &
            PS_V_g(PSV) /= UNDEFINED .OR. &
            PS_W_g(PSV) /= UNDEFINED) THEN

            WRITE(ERR_MSG,1100) PSV, trim(iVar('PS_MASSFLOW_G',PSV))
            CALL LOG_ERROR()

 1100 FORMAT('Error 1100: Invalid specification for point source ',I3,&
         '.',/A,' is undefined but velocity is given.',/'Please ',    &
         'correct the project settings.')

         ELSE
            PS_MASSFLOW_G(PSV) = ZERO
            PS_U_g(PSV) = ZERO
            PS_V_g(PSV) = ZERO
            PS_W_g(PSV) = ZERO
         ENDIF

      ELSEIF(PS_MASSFLOW_G(PSV) == ZERO) THEN
         IF(PS_U_g(PSV) /= ZERO .OR. &
            PS_V_g(PSV) /= ZERO .OR. &
            PS_W_g(PSV) /= ZERO) THEN

            WRITE(ERR_MSG,1101) PSV, trim(iVar('PS_MASSFLOW_G',PSV))
            CALL LOG_ERROR()
         ENDIF

 1101 FORMAT('Error 1101: Invalid specification for point source ',I3,&
         '.',/A,' is zero but velocity is given.',/'Please correct ', &
         'the project settings.')

! Verify a physical mass flow
      ELSEIF(PS_MASSFLOW_G(PSV) < ZERO) THEN
         WRITE(ERR_MSG,1102) PSV, trim(iVar('PS_MASSFLOW_G',PSV))
         CALL LOG_ERROR()

 1102 FORMAT('Error 1102: Invalid specifications for point source ',I3,&
         '.',/A,' < 0.0. Point sources can only add mass to a system',/&
         'Please correct the project settings.')


! Mass flow is specified:
      ELSE

! Velocity does not have to be defined (no momentum source). If the
! components are UNDEFINED, zero them out.
         IF(PS_U_g(PSV) == UNDEFINED) PS_U_g(PSV) = ZERO
         IF(PS_V_g(PSV) == UNDEFINED) PS_V_g(PSV) = ZERO
         IF(PS_W_g(PSV) == UNDEFINED) PS_W_g(PSV) = ZERO

! Sum together defined gas phase species mass fractions.
         SUM = ZERO
         DO N = 1, NMAX(0)
            IF(PS_X_G(PSV,N) /= UNDEFINED) THEN
               SUM = SUM + PS_X_G(PSV,N)
            ELSE
               PS_X_G(PSV,N) = ZERO
            ENDIF
         ENDDO

! Enforce that the species mass fractions must sum to one.
         IF(.NOT.COMPARE(ONE,SUM)) THEN

            IF(SPECIES_EQ(0)) THEN
               WRITE(ERR_MSG, 1110) PSV
               CALL LOG_ERROR()

 1110 FORMAT('Error 1110: PS_X_g(',I3,',:) do NOT sum to ONE and the ',&
         'gas phase',/'species equations are solved. Please correct ', &
         'the project settings.')

            ELSEIF(.NOT.COMPARE(SUM,ZERO)) THEN
               WRITE(ERR_MSG, 1111) PSV
               CALL LOG_ERROR()

 1111 FORMAT('Error 1111: PS_X_g(',I3,',:) do not sum to ONE or ZERO ',&
         'and they',/'are not needed. Please correct the project settings.')

            ELSE
               PS_X_G(PSV,:) = ZERO
               PS_X_G(PSV,1) = ONE
            ENDIF

         ENDIF

! Verify that a temperature is provided.
         IF(ENERGY_EQ)THEN
            IF(PS_T_g(PSV) == UNDEFINED) THEN
               WRITE(ERR_MSG,1000) trim(iVar('PS_T_g',PSV))
               CALL LOG_ERROR()

! Verify that a given temperature is physical.
            ELSEIF(PS_T_g(PSV) <= ZERO) THEN
               WRITE(ERR_MSG,1001) PSV, trim(iVar('PS_T_g',PSV)),      &
                  trim(iVal(PS_T_g(PSV)))
               CALL LOG_ERROR()
            ENDIF
         ENDIF

      ENDIF

      RETURN

 1000 FORMAT('Error 1000: Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

 1001 FORMAT('Error 1001: Illegal or unknown input: ',A,' = ',A,/      &
         'Please correct the project settings.')

      END SUBROUTINE CHECK_PS_GAS_PHASE


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: CHECK_PS_SOLIDS_PHASES                                  !
!  Author: J. Musser                                  Date: 10-JUN-13  !
!                                                                      !
!  Purpose: Check point source specifications.                         !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_PS_SOLIDS_PHASES(PSV)

! Global Variables:
!---------------------------------------------------------------------//
! Solids phase mass flowrate for PS
      use ps, only: PS_MASSFLOW_S
! Solids phase velocity for PS
      use ps, only: PS_U_s, PS_V_s, PS_W_s
! Solids phase temperature and species mass fractions
      use ps, only: PS_T_s, PS_X_s
! Flag: Solve energy equations.
      use run, only: ENERGY_EQ
! Flag: Solve species equations.
      use run, only: SPECIES_EQ
! Type of each solids phase.
      use run, only: SOLIDS_MODEL
! Number of (TFM) solids.
      use physprop, only: SMAX
! Number of discrete solids phases.
      use discretelement, only: DES_MMAX
! Number of solids species.
      use physprop, only: SMAX, NMAX

! Global Parameters:
!---------------------------------------------------------------------//
! Parameter constants
      use param1, only: ZERO, ONE, UNDEFINED

      use toleranc

      implicit none

! Dummy Arguments:
!---------------------------------------------------------------------//
      INTEGER, INTENT(in) :: PSV

! Local Variables:
!---------------------------------------------------------------------//
! Total number of solid phases
      INTEGER :: MMAX_TOT
! Loop counters
      INTEGER :: M, N
! Sum of solids mass fractions.
      DOUBLE PRECISION :: SUM
!......................................................................!

! The total number of solids phases (all models).
      MMAX_TOT = SMAX + DES_MMAX

      DO M=1, MMAX_TOT

! Check mass flow and velocity
         IF(PS_MASSFLOW_S(PSV,M) == UNDEFINED) THEN
            IF(PS_U_s(PSV,M) /= UNDEFINED .OR. &
               PS_V_s(PSV,M) /= UNDEFINED .OR. &
               PS_W_s(PSV,M) /= UNDEFINED) THEN

               WRITE(ERR_MSG,1100)PSV, trim(iVar('PS_MASSFLOW_S',PSV,M))
               CALL LOG_ERROR()

 1100 FORMAT('Error 1100: Invalid specification for point source ',I3,&
         '.',/A,' is undefined but velocity is given.',/'Please ',    &
         'correct the project settings.')

            ELSE
               PS_MASSFLOW_S(PSV,M) = ZERO
               PS_U_s(PSV,M) = ZERO
               PS_V_s(PSV,M) = ZERO
               PS_W_s(PSV,M) = ZERO
            ENDIF

         ELSEIF(PS_MASSFLOW_S(PSV,M) == ZERO) THEN
            IF(PS_U_s(PSV,M) /= ZERO .OR. &
               PS_V_s(PSV,M) /= ZERO .OR. &
               PS_W_s(PSV,M) /= ZERO) THEN

               WRITE(ERR_MSG,1101)PSV, trim(iVar('PS_MASSFLOW_S',PSV,M))
               CALL LOG_ERROR()
            ENDIF

 1101 FORMAT('Error 1100: Invalid specification for point source ',I3,&
         '.',/A,' is zero but velocity is given.',/'Please correct ', &
         'the project settings.')

         ELSEIF(PS_MASSFLOW_S(PSV,M) < ZERO) THEN
            WRITE(ERR_MSG,1102) PSV, trim(iVar('PS_MASSFLOW_S',PSV,M))
            CALL LOG_ERROR()

 1102 FORMAT('Error 1102: Invalid specifications for point source ',I3,&
         '.',/A,' < 0.0. Point sources can only add mass to a system',/&
         'Please correct the project settings.')


! Mass flow is specified:
         ELSE

! Currently, only TFM solids can be used with point sources. However,
! the could be implemented for PIC solids as well.
            SELECT CASE(SOLIDS_MODEL(M))
            CASE ('DEM','CGP','PIC')
               WRITE(ERR_MSG, 1110) PSV, SOLIDS_MODEL(M)
               CALL LOG_ERROR()
            CASE DEFAULT
            END SELECT

 1110 FORMAT('Error 1110: Invalid specifications for point source ',I3,&
         '.',/'Point sources are not supported for ',A,' solids.',/    &
         'Please correct the project settings.')

! Velocity does not have to be defined (no momentum source). If the
! components are UNDEFINED, zero them out.
            IF(PS_U_s(PSV,M) == UNDEFINED) PS_U_s(PSV,M) = ZERO
            IF(PS_V_s(PSV,M) == UNDEFINED) PS_V_s(PSV,M) = ZERO
            IF(PS_W_s(PSV,M) == UNDEFINED) PS_W_s(PSV,M) = ZERO

! Sum together defined gas phase species mass fractions.
            SUM = ZERO
            DO N = 1, NMAX(M)
               IF(PS_X_S(PSV,M,N) /= UNDEFINED) THEN
               SUM = SUM + PS_X_S(PSV,M,N)
               ELSE
                  PS_X_S(PSV,M,N) = ZERO
               ENDIF
            ENDDO

! Enforce that the species mass fractions must sum to one.
            IF(.NOT.COMPARE(ONE,SUM)) THEN

               IF(SPECIES_EQ(M)) THEN
                  WRITE(ERR_MSG, 1120) PSV,M
                  CALL LOG_ERROR()

 1120 FORMAT('Error 1120: PS_X_s(',I3,',',I2,',:) do NOT sum to ONE ', &
         'and the solids phase',/'species equations are solved. ',     &
         'Please correct the project settings.')

               ELSEIF(.NOT.COMPARE(SUM,ZERO)) THEN
                  WRITE(ERR_MSG, 1121) PSV,M
                  CALL LOG_ERROR()

 1121 FORMAT('Error 1121: PS_X_s(',I3,',',I2,',:) do not sum to ONE ', &
         'or ZERO and they',/'are not needed. Please correct the ',    &
         'project settings.')

               ELSE
                  PS_X_S(PSV,M,1)  = ONE
                  PS_X_S(PSV,M,2:) = ZERO
               ENDIF

            ENDIF

! Verify that a temperature is provided.
            IF(ENERGY_EQ)THEN
               IF(PS_T_s(PSV,M) == UNDEFINED) THEN
                  WRITE(ERR_MSG,1000) trim(iVar('PS_T_s',PSV,M))
                  CALL LOG_ERROR()

! Verify that a given temperature is physical.
               ELSEIF(PS_T_s(PSV,M) <= ZERO) THEN
                  WRITE(ERR_MSG,1001) PSV, trim(iVar('PS_T_s',PSV,M)), &
                     trim(iVal(PS_T_s(PSV,M)))
                  CALL LOG_ERROR()
               ENDIF
            ENDIF
         ENDIF
      ENDDO

      RETURN

 1000 FORMAT('Error 1000: Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

 1001 FORMAT('Error 1001: Illegal or unknown input: ',A,' = ',A,/      &
         'Please correct the project settings.')

      END SUBROUTINE CHECK_PS_SOLIDS_PHASES



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: CHECK_PS_OVERFLOW                                       !
!  Author: J. Musser                                  Date: 10-JUN-13  !
!                                                                      !
!  Purpose: Check point source specifications.                         !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_PS_OVERFLOW(PSV)

! Global Variables:
!---------------------------------------------------------------------//
! Gas phase mass flowrate for PS and velocities
      use ps, only: PS_MASSFLOW_G, PS_U_g, PS_V_g, PS_W_g
! Gas phase temperature and species mass fractions
      use ps, only: PS_T_g, PS_X_g
! Solids phase mass flowrate and velocity
      use ps, only: PS_MASSFLOW_S, PS_U_s, PS_V_s, PS_W_s
! Solids phase temperature and species mass fractions
      use ps, only: PS_T_s, PS_X_s

! Global Parameters:
!---------------------------------------------------------------------//
! Maximum input array sizes.
      use param, only: DIM_M, DIM_N_g, DIM_N_s
! Parameter constants
      use param1, only: UNDEFINED

      implicit none

! Dummy Arguments:
!---------------------------------------------------------------------//
      INTEGER, INTENT(in) :: PSV

! Local Variables:
!---------------------------------------------------------------------//
! Loop counters
      INTEGER :: M, N
!......................................................................!

      IF(PS_MASSFLOW_G(PSV) /= UNDEFINED) THEN
         WRITE(ERR_MSG,1010) trim(iVar('PS_MASSFLOW_G',PSV))
         CALL LOG_ERROR()
      ELSEIF(PS_U_g(PSV) /= UNDEFINED) THEN
         WRITE(ERR_MSG,1010) trim(iVar('PS_U_g',PSV))
         CALL LOG_ERROR()
      ELSEIF(PS_V_g(PSV) /= UNDEFINED) THEN
         WRITE(ERR_MSG,1010) trim(iVar('PS_V_g',PSV))
         CALL LOG_ERROR()
      ELSEIF(PS_W_g(PSV) /= UNDEFINED) THEN
         WRITE(ERR_MSG,1010) trim(iVar('PS_W_g',PSV))
         CALL LOG_ERROR()
      ELSEIF(PS_T_g(PSV) /= UNDEFINED) THEN
         WRITE(ERR_MSG,1010) trim(iVar('PS_T_g',PSV))
         CALL LOG_ERROR()
      ENDIF
      DO N = 1, DIM_N_G
         IF(PS_X_G(PSV,N) /= UNDEFINED) THEN
            WRITE(ERR_MSG,1010) trim(iVar('PS_X_G',PSV,N))
            CALL LOG_ERROR()
         ENDIF
      ENDDO

      DO M=1, DIM_M
         IF(PS_MASSFLOW_S(PSV,M) /= UNDEFINED) THEN
            WRITE(ERR_MSG,1010) trim(iVar('PS_MASSFLOW_S',PSV,M))
            CALL LOG_ERROR()
         ELSEIF(PS_U_s(PSV,M) /= UNDEFINED) THEN
            WRITE(ERR_MSG,1010) trim(iVar('PS_U_s',PSV,M))
            CALL LOG_ERROR()
         ELSEIF(PS_V_s(PSV,M) /= UNDEFINED) THEN
            WRITE(ERR_MSG,1010) trim(iVar('PS_V_s',PSV,M))
            CALL LOG_ERROR()
         ELSEIF(PS_W_s(PSV,M) /= UNDEFINED) THEN
            WRITE(ERR_MSG,1010) trim(iVar('PS_W_s',PSV,M))
            CALL LOG_ERROR()
         ELSEIF(PS_T_s(PSV,M) /= UNDEFINED) THEN
            WRITE(ERR_MSG,1010) trim(iVar('PS_T_s',PSV,M))
            CALL LOG_ERROR()
         ENDIF
         DO N = 1, DIM_N_S
            IF(PS_X_S(PSV,M,N) /= UNDEFINED) THEN
               WRITE(ERR_MSG,1010) trim(iVar('PS_X_S',PSV,M,N))
               CALL LOG_ERROR()
            ENDIF
         ENDDO
      ENDDO

      RETURN

 1010 FORMAT('Error 1010: ',A,' specified in an undefined PS region.')

   END SUBROUTINE CHECK_PS_OVERFLOW

END MODULE CHECK_POINT_SOURCES_MOD
