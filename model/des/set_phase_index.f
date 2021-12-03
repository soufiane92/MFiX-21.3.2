#include "error.inc"

MODULE SET_PHASE_INDEX_MOD

   USE error_manager
   USE open_files_mod, only: open_pe_log

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: SET_PHASE_INDEX                                         !
!                                                                      !
!  Purpose: Set the index of all particles based on their diameter and !
!  density.                                                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE SET_PHASE_INDEX

! Modules
!---------------------------------------------------------------------//
      use discretelement, only: PIJK
      USE discretelement, only: DES_RADIUS, RO_SOL
      USE discretelement, only: DES_MMAX
      USE discretelement, only: MAX_PIP
      USE discretelement, only: P_INPUT_DAT_VERSION
      use discretelement, only: CGDEM, cgp_stat_wt
      USE functions, only: IS_NONEXISTENT, IS_GHOST, IS_ENTERING_GHOST
      USE functions, only: IS_EXITING_GHOST
      USE error_manager
      use mpi_utility
      use param, only: DIM_M
      use param1, only: small_number, one
      USE physprop, only: MMAX, D_p0, RO_s0
      USE run, only: RUN_TYPE
      USE run, only: ANY_SPECIES_EQ
      use sendrecv
      IMPLICIT NONE

! Local Variables
!---------------------------------------------------------------------//
! particle no.
      INTEGER :: L
! solids phase no.
      INTEGER :: M
! IER for error reporting
      INTEGER :: IER
! Difference between a particles diameter (density) and the diameter
! (density) of a phase specified in the data file.
      DOUBLE PRECISION dDp, dRho
! Relative tolerance when comparing diameter or density
! We cannot use SMALL_NUMBER (1.0D-15) to do the comparison
! when using variable solids density because the value entered
! in particle_input.dat may not have sufficient precision
! compared with the initial computed value.
! It is also better to normalize the difference because
! the diameter and density do not typically have the
! same order of magnitude (especially in SI units).
      DOUBLE PRECISION, PARAMETER:: RELATIVE_TOL = 1.0D-4
! Correction for CGDEM particel size
      DOUBLE PRECISION ::  WT(DIM_M)
!......................................................................!

      IF(P_INPUT_DAT_VERSION/='1.0') RETURN

! The restart file contains the phase index for reacting cases as the
! diameter and/or density of the particle may have changed.
      IF(RUN_TYPE /= 'NEW' .AND. ANY_SPECIES_EQ) RETURN

! Initialize the error flag.
      IER = 0

! solids phase index of particle.
! ---------------------------------------------------------------->>>
      DO M = MMAX+1, MMAX+DES_MMAX
         IF(CGDEM) THEN
            WT(M) = CGP_STAT_WT(M)**(1.0D0/3.0D0)
         ELSE
            WT(M) = ONE
         ENDIF
      ENDDO

      DO L = 1, MAX_PIP
         IF(IS_NONEXISTENT(L)) CYCLE
         IF(IS_GHOST(L) .OR. IS_ENTERING_GHOST(L) .OR. IS_EXITING_GHOST(L)) CYCLE

! Determining the solids phase of each particle by matching the diameter
! and density to those specified in the data file.

! First, try to match diameter and density with absolute error and
! strict tolerance (SMALL_NUMBER). This is useful when two solids
! phases are defined with nearly the same diameter and densities
! say D_p0(1) = 0.001 and D_p0(2) = 0.0010001
         IF(PIJK(L,5).EQ.0) THEN
            M_LP_ABS: DO M = MMAX+1, MMAX+DES_MMAX
               dDp  = ABS(2.0d0*DES_RADIUS(L)-D_P0(M)*WT(M))
               dRho = ABS( RO_Sol(L)-RO_S0(M))
               IF( dDp < SMALL_NUMBER .AND. dRho < SMALL_NUMBER) THEN
                  PIJK(L,5) = M
                  EXIT M_LP_ABS
               ENDIF
            ENDDO M_LP_ABS
         ENDIF

! Next, if a nearly exact match (above) wasn't found,
! (PIJK(L,5) would still be zero), try to find
! a less strict match using relative error. This is useful when using
! variable solids density, where the density in particle_input.dat
! cannot be written with same precision as what is initially
! computed in the code.
! Note: Polydisperse simulation (from 20.3 onward) won't set the phase index
! in the above loop since diameter varies within a given solids phase.
! However, the phase index should already be set from the initial seeding
! or from reading the restart file.
         IF(PIJK(L,5).EQ.0) THEN
            M_LP_REL: DO M = MMAX+1, MMAX+DES_MMAX
               dDp  = ABS(2.0d0*DES_RADIUS(L)-D_P0(M)*WT(M))/(D_P0(M)*WT(M))
               dRho = ABS( RO_Sol(L)-RO_S0(M))/RO_S0(M)
               IF( dDp < RELATIVE_TOL .AND. dRho < RELATIVE_TOL) THEN
                  PIJK(L,5) = M
                  EXIT M_LP_REL
               ENDIF
            ENDDO M_LP_REL
         ENDIF
! Flag error if no match is found.
         IF(PIJK(L,5).EQ.0) IER = 1
      ENDDO

! Sync up the error flag across all processes.
      CALL GLOBAL_ALL_SUM(IER)
      IF(IER == 0) RETURN

! Point of no return: Report errors and abort
!----------------------------------------------------------------------

      CALL OPEN_PE_LOG(IER)

      WRITE(ERR_MSG, 1100)
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, FOOTER=.FALSE.)

 1100 FORMAT('Error 1100: Unable to determine the phase of one or ',&
         'more particles.',/8x,'ID',4X,'Diameter',6x,'Density',/)

      DO L = 1, MAX_PIP
! skipping particles that do not exist
         IF(IS_NONEXISTENT(L)) CYCLE
         IF(IS_GHOST(L) .OR. IS_ENTERING_GHOST(L) .OR. IS_EXITING_GHOST(L)) CYCLE

! Flag as an error if no match is found.
         IF(PIJK(L,5).EQ.0) THEN
            WRITE(ERR_MSG,9000) L,  2.0*DES_RADIUS(L), Ro_Sol(L)
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
         ENDIF
      ENDDO

      WRITE(ERR_MSG, 1101)
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

 1101 FORMAT(' ',/'Defined phase parameters (keywords or initial density):',/3x,'ID',&
         5X,'Diameter',5x,'Density')

      DO M = MMAX+1, DES_MMAX+MMAX
         WRITE(ERR_MSG, 9000) M, D_P0(M), RO_S0(M)
         CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
      ENDDO

      call log_error()

 9000 FORMAT(I10,2(2x,g12.5))

   END SUBROUTINE SET_PHASE_INDEX

END MODULE SET_PHASE_INDEX_MOD
