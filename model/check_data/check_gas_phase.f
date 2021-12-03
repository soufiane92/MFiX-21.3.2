#include "error.inc"

MODULE CHECK_GAS_PHASE_MOD

   USE error_manager
   USE read_database_mod, only: read_database

CONTAINS
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: CHECK_GAS_PHASE                                        !
!  Purpose: Check the gas phase input section                          !
!                                                                      !
!  Author: P.Nicoletti                                Date: 02-DEC-91  !
!          J.Musser                                   Date: 01-FEB-14  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_GAS_PHASE(MFIX_DAT)


! Global Variables:
!---------------------------------------------------------------------//
! Flag: Solve species equations.
      use run, only: SPECIES_EQ, ENERGY_EQ
! Flag: Use legacy reaction rates implementation
      use rxns, only: USE_RRATES
! User specified: Constant gas viscosity
      use physprop, only: MU_G0
! User specified: Constant gas thermal conductivity
      use physprop, only: K_G0
! User specified: Constant gas mixture diffusion coefficient
      use physprop, only: DIF_G0
! User specified: Constant gas specific heat
      use physprop, only: C_PG0
! User specified: Constant gas density
      use physprop, only: RO_G0
! User specified: Constant gas mixture molecular weight
      use physprop, only: MW_AVG

      use mms, only: use_mms
! Global Parameters:
!---------------------------------------------------------------------//
! Parameter constants
      use param1, only: UNDEFINED, ZERO

! Skip data check when doing pre-processing only
      USE run, only:ppo

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

! Local Variables:
!---------------------------------------------------------------------//
! NONE

!......................................................................!

      IF(PPO) RETURN

! CHECK MU_g0
      IF (MU_G0 < ZERO) THEN
         WRITE(ERR_MSG,1001) 'MU_G0', iVal(MU_G0)
         CALL LOG_ERROR()
      ENDIF

! CHECK K_g0
      IF (K_G0 < ZERO) THEN
         WRITE(ERR_MSG,1001) 'K_G0', iVal(K_G0)
         CALL LOG_ERROR()
      ENDIF

! CHECK C_pg0
      IF (C_PG0 < ZERO) THEN
         WRITE(ERR_MSG,1001) 'C_PG0', iVal(C_PG0)
         CALL LOG_ERROR()
      ENDIF

! CHECK DIF_g0
      IF (DIF_G0 < ZERO) THEN
         WRITE(ERR_MSG,1001) 'DIF_g0', iVal(DIF_g0)
         CALL LOG_ERROR()
      ENDIF

! Check the input specifications for gas species.
      IF(USE_RRATES)THEN
         CALL CHECK_GAS_SPECIES_LEGACY
      ELSE
         CALL CHECK_GAS_SPECIES(MFIX_DAT)
      ENDIF

! Currently MMS uses constant properties. These are in place simply
! to give the developer a heads-up that the code/setup may not fully
! encompass the use of non-constant properties
      IF (USE_MMS) THEN
         IF (MU_G0 == UNDEFINED) THEN
            WRITE(ERR_MSG, 1200) 'MU_G0'
            CALL LOG_ERROR()
         ENDIF
         IF (K_G0 == UNDEFINED .AND. ENERGY_EQ) THEN
            WRITE(ERR_MSG, 1200) 'K_G0'
            CALL LOG_ERROR()
         ENDIF
         IF (DIF_G0 == UNDEFINED .AND. SPECIES_EQ(0)) THEN
            WRITE(ERR_MSG, 1200) 'DIF_G0'
            CALL LOG_ERROR()
         ENDIF

 1200 FORMAT('Error 1200: ',A,' must be defined when USE_MMS is T.',/,&
         'Please correct the project settings.')
      ENDIF

! CHECK MW_AVG
      IF (SPECIES_EQ(0)) THEN
! MW_AVG is defined and the gas phase species equations are solved, then
! the user specified average molecular weight is ignored. The gas phase
! mixture molecular weight (MW_MIX_g) is used instead.
         IF (MW_AVG /= UNDEFINED) THEN
            WRITE (ERR_MSG, 1100) 'solving species equations'
            CALL LOG_WARNING()
            MW_AVG = UNDEFINED
         ENDIF
      ELSE
! When the species equations are not solved and the gas phase is
! compressible, verify that the user provided average molecular weight
! has a physical value. (This does not include the case where MW_AVG
! is UNDEFINED.)
         IF (RO_G0 == UNDEFINED) THEN
            IF (MW_AVG <= ZERO) THEN
               WRITE(ERR_MSG, 1001) 'MW_AVG', iVal(MW_AVG)
               CALL LOG_ERROR()
            ENDIF
         ELSE
! Gas density for incompressible flows must be positive.
            IF (RO_G0 < ZERO) THEN
               WRITE(ERR_MSG, 1001) 'RO_G0', iVal(RO_G0)
               CALL LOG_ERROR()
            ENDIF
! Incompressible simulations do not need MW_AVG. Notify the user that
! the provided data is ignored.
            IF (MW_AVG /= UNDEFINED)THEN
               WRITE(ERR_MSG, 1100) 'RO_g0 is specified'
               CALL LOG_WARNING()
            ENDIF

         ENDIF
      ENDIF

      RETURN

 1001 FORMAT('Error 1001: Illegal or unknown input: ',A,' = ',A,/      &
         'Please correct the project settings.')


 1100 FORMAT('Message 2000: MW_AVG is not needed when ',A,'.')

      END SUBROUTINE CHECK_GAS_PHASE



!----------------------------------------------------------------------!
! Subroutine: CHECK_GAS_SPECIES                                        !
! Purpose: Gas phase species checks.                                   !
!                                                                      !
! Author: J. Musser                                  Date: 07-FEB-14   !
!----------------------------------------------------------------------!
      SUBROUTINE CHECK_GAS_SPECIES(MFIX_DAT)


! Global Variables:
!---------------------------------------------------------------------//
! Flag: Solve energy equations
      use run, only: ENERGY_EQ
! Flag: Solve species equations
      use run, only: SPECIES_EQ
! Flag: Database for phase X was read for species Y
      use rxns, only: rDatabase
! Flag: Code is reinitializing
      use run, only: REINITIALIZING
! Gas phase species database names.
      use rxns, only: SPECIES_g
! Gas phase molecular weights.
      use physprop, only: MW_g
! Number of gas phase species.
      use physprop, only: NMAX, NMAX_g
! User specified: Constant gas phase specific heat
      use physprop, only: C_PG0
! User specified: Constant gas density
      use physprop, only: RO_G0
! User specified: Constant gas phase mixture molecular weight
      use physprop, only: MW_AVG


! Global Parameters:
!---------------------------------------------------------------------//
! Maximum number of gas phase species.
      USE param, only: DIM_N_g
! Constants.
      USE param1, only: UNDEFINED, UNDEFINED_I, UNDEFINED_C
      USE param1, only: ZERO

      implicit none

      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

! Local Variables:
!---------------------------------------------------------------------//
! Loop counter.
      INTEGER :: N

! Flag that the energy equations are solved and constant gas phase
! specific heat is undefined.
! If true, a call to the thermochemical database is made.
      LOGICAL EEQ_CPG

! Flag that the average molecular weight (MW_AVG) and constant gas
! phase density are undefined.
! If true, a call to the thermochemical database is made.
      LOGICAL MWg_ROg

! Flag that the gas phase species equations are solved and the
! molecular weight for a species is not given in the data file.
! If true, a call to the thermochemical database is made.
      LOGICAL SEQ_MWg


!......................................................................!

! Reconcile the new species input method with the legacy input method.
      IF(SPECIES_EQ(0)) THEN

         IF(NMAX_g == UNDEFINED_I) THEN
            WRITE(ERR_MSG,1000) 'NMAX_g'
            CALL LOG_ERROR()
         ELSEIF(NMAX_g > DIM_N_G) THEN
            WRITE(ERR_MSG,1001) 'NMAX_g', iVal(NMAX_g)
            CALL LOG_ERROR()
         ELSE
            NMAX(0) = NMAX_g
         ENDIF
! Set the number of species to one if the species equations are not solved and
! the number of species is not specified.
      ELSE
         NMAX(0) = merge(1, NMAX_g, NMAX_g == UNDEFINED_I)
      ENDIF

! Flag that the energy equations are solved and specified solids phase
! specific heat is undefined.
      EEQ_CPG = (ENERGY_EQ .AND. C_PG0 == UNDEFINED)
      IF(EEQ_CPG .AND. .NOT.REINITIALIZING) THEN
         WRITE(ERR_MSG,2000)
         CALL LOG_WARNING()
      ENDIF

 2000 FORMAT('Message: 2000 The energy equations are being solved ',   &
         '(ENERGY_EQ) and',/'the constant gas specific heat is ',      &
         'undefined (C_PG0). Thus, the thermo-',/'chemical database ', &
         'will be used to gather specific heat data on the',/          &
         'individual gas phase species.')

      MWg_ROg = .FALSE.
      SEQ_MWg = .FALSE.
      IF(MW_AVG == UNDEFINED) THEN
         DO N=1,NMAX(0)
            IF(MW_g(N) == UNDEFINED) THEN
               IF(RO_G0 == UNDEFINED) MWg_ROg = .TRUE.
               IF(SPECIES_EQ(0)) SEQ_MWg = .TRUE.
            ENDIF
         ENDDO
      ENDIF

      IF(MWg_ROg .AND. REINITIALIZING) THEN
         WRITE(ERR_MSG, 2001)
         CALL LOG_WARNING()
      ENDIF

 2001 FORMAT('Message 2001: MW_AVG and RO_G0 are undefined and one or',&
         ' more species',/'molecular weights are undefined. The therm',&
         'ochemical database will be',/'used in an attempt to gather ',&
         'missing molecular weight data.')

      IF(SEQ_MWg .AND. REINITIALIZING) THEN
         WRITE(ERR_MSG, 2002)
         CALL LOG_WARNING()
      ENDIF

 2002 FORMAT('Message 2002: One or more species molecular weights are',&
         ' undefined and',/'the gas phase species equations are being',&
         ' solved (SOLVE_EQ(0)). The',/'thermochemical database will ',&
         'be used in an attempt to gather missing',/'molecular weight',&
         ' data.')

! Initialize flag indicating the database was read for a species.
      rDatabase(0,:) = .FALSE.

      IF(EEQ_CPG .OR. SEQ_MWg .OR. MWg_ROg) THEN

         IF(.NOT.REINITIALIZING) THEN
            WRITE(ERR_MSG, 3000)
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, FOOTER=.FALSE.)
         ENDIF

 3000 FORMAT('Message 3000: Searching thermochemical databases for ',  &
         'gas phase',/'species data.',/'  ')

         DO N = 1, NMAX(0)
            IF(EEQ_CPG .OR. MW_g(N) == UNDEFINED) THEN
! Notify the user of the reason the thermochemical database is used.
! Flag that the species name is not provided.
               IF(SPECIES_g(N) == UNDEFINED_C) THEN
                  WRITE(ERR_MSG,1000) trim(iVar('SPECIES_g',N))
                  CALL LOG_ERROR()
               ENDIF
! Update the log files.
               IF(.NOT.REINITIALIZING) THEN
                  WRITE(ERR_MSG, 3001) N, trim(SPECIES_g(N))
                  CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
               ENDIF
               3001 FORMAT(/2x,'>',I3,': Species: ',A)
! Read the database.
               CALL READ_DATABASE(MFIX_DAT, 0, N, SPECIES_g(N), MW_g(N))
! Flag variable to stating that the database was read.
               rDatabase(0,N) = .TRUE.
            ENDIF

         ENDDO ! Loop over species

         IF(.NOT.REINITIALIZING) THEN
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE.)
         ENDIF

      ENDIF

! Verify that no additional species information was given.
      DO N = NMAX(0) + 1, DIM_N_G
         IF(MW_G(N) /= UNDEFINED) THEN
            WRITE(ERR_MSG, 1002) trim(iVar('MW_g',N))
            CALL LOG_ERROR()
         ENDIF
      ENDDO

      RETURN

 1000 FORMAT('Error 1000: Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

 1001 FORMAT('Error 1001: Illegal or unphysical input: ',A,' = ',A,/   &
         'Please correct the project settings.')

 1002 FORMAT('Error 1002: Illegal input: ',A,' specified out of range.',&
         'Please correct the project settings.')

      END SUBROUTINE CHECK_GAS_SPECIES


!----------------------------------------------------------------------!
! Subroutine: CHECK_GAS_SPECIES_LEGACY                                 !
! Purpose: These are legacy checks for using rrates.f to specify       !
! chemical reactions.                                                  !
!                                                                      !
! Author: J. Musser                                  Date: 03-FEB-14   !
!----------------------------------------------------------------------!
      SUBROUTINE CHECK_GAS_SPECIES_LEGACY


! Global Variables:
!---------------------------------------------------------------------//
! Flag: Solve species equations
      use run, only: SPECIES_EQ
! Gas phase molecular weights.
      use physprop, only: MW_g
! Number of gas phase species.
      use physprop, only: NMAX, NMAX_g
! Flag: Database was read. (legacy)
      use physprop, only: DATABASE_READ


! Global Parameters:
!---------------------------------------------------------------------//
! Maximum number of gas phase species.
      USE param, only: DIM_N_g
! Constants.
      USE param1, only: UNDEFINED_I, UNDEFINED, ZERO

      implicit none

! Local Variables:
!---------------------------------------------------------------------//
! Loop counter.
      INTEGER :: N

!......................................................................!

! Reconcile the new species input method with the legacy input method.
      IF(SPECIES_EQ(0)) THEN
! Legacy checks for species equations.
         IF(NMAX_g /= UNDEFINED_I) THEN
            WRITE(ERR_MSG,2000) 'NMAX_g', 'undefined'
            CALL LOG_ERROR()
         ELSEIF(NMAX(0) == UNDEFINED_I) THEN
            WRITE(ERR_MSG,2000) trim(iVar('NMAX',0)), 'specified'
            CALL LOG_ERROR()
         ELSEIF(NMAX(0) > DIM_N_G) THEN
            WRITE(ERR_MSG,1001) trim(iVar('NMAX',0)), iVal(NMAX(0))
            CALL LOG_ERROR()
         ENDIF
! Set the number of species to one if the species equations are not
! solved and the number of species is not specified.
      ELSE
         IF(NMAX(0) == UNDEFINED_I) NMAX(0) = 1
      ENDIF

! Check MW_g if solids species are present
      DO N = 1, NMAX(0)
         IF(MW_G(N) == UNDEFINED) THEN
            WRITE(ERR_MSG,2000)trim(iVar('MW_g',N)), 'specified'
            CALL LOG_ERROR()
         ELSEIF(MW_G(N) <= ZERO) THEN
            WRITE(ERR_MSG,1001)trim(iVar('MW_g',N)), iVal(MW_G(N))
            CALL LOG_ERROR()
         ENDIF
      ENDDO ! Loop over species
      DO N = NMAX(0) + 1, DIM_N_G
         IF(MW_G(N) /= UNDEFINED) THEN
            WRITE(ERR_MSG,1001)trim(iVar('MW_g',N)), iVal(MW_G(N))
            CALL LOG_ERROR()
         ENDIF
      ENDDO

! Set the legacy database flag. (Also in check_solids_common_all)
      DATABASE_READ = .FALSE.

      RETURN

 1001 FORMAT('Error 1001: Illegal or unphysical input: ',A,' = ',A,/   &
         'Please correct the project settings.')

 2000 FORMAT('Error 2000: Invalid input. ',A,' must be ',A,/'when ',    &
         'USE_RRATES is .TRUE.'/,'Please correct the project settings')

   END SUBROUTINE CHECK_GAS_SPECIES_LEGACY

END MODULE CHECK_GAS_PHASE_MOD
