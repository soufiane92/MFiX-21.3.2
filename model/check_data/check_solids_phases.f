#include "error.inc"

MODULE CHECK_SOLIDS_PHASES_MOD

   use check_solids_common_all_mod, only:  check_solids_common_all
   use check_solids_common_discrete_mod, only:  check_solids_common_discrete
   use check_solids_continuum_mod, only:  check_solids_continuum
   use check_solids_dem_mod, only: check_solids_dem
   use check_solids_mppic_mod, only: check_solids_mppic
   use error_manager

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  SUBROUTINE: CHECK_SOLIDS_PHASES                                     !
!                                                                      !
!  Purpose: Driver routine for calls to solids phase checks.           !
!                                                                      !
!  Author: J.Musser                                   Date: 16-Jan-14  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE CHECK_SOLIDS_PHASES(MFIX_DAT)


! Global Variables:
!---------------------------------------------------------------------//
! Runtime flag specifying TFM solids
      use run, only: TFM_SOLIDS
! Runtime flag specifying DEM solids
      use run, only: DEM_SOLIDS
! Runtime flag specifying MPPIC solids
      use run, only: PIC_SOLIDS

! Skip data check when doing pre-processing only
      USE run, only:ppo

      implicit none

      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

!......................................................................!

      IF(PPO) RETURN

! Impose the various model limitations.
      CALL CHECK_SOLIDS_MODEL_LIMITATIONS

! Checks common to all solids models.
      CALL CHECK_SOLIDS_COMMON_ALL(MFIX_DAT)

! Checks common to discrete solids phases (DEM, MPPIC).
      IF(DEM_SOLIDS .OR. PIC_SOLIDS) &
         CALL CHECK_SOLIDS_COMMON_DISCRETE

! Checks specific to the particular solids phase.
      IF(TFM_SOLIDS) CALL CHECK_SOLIDS_CONTINUUM
      IF(DEM_SOLIDS) CALL CHECK_SOLIDS_DEM
      IF(PIC_SOLIDS) CALL CHECK_SOLIDS_MPPIC

      RETURN

      END SUBROUTINE CHECK_SOLIDS_PHASES


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  SUBROUTINE: CHECK_SOLIDS_MODEL_LIMITATIONS                          !
!                                                                      !
!  Purpose: Impose the limitations of the various solids models. These !
!  checks should be 'high level' in that they only ensure that models  !
!  are only used with phases that support them.                        !
!                                                                      !
!  Author: J.Musser                                   Date: 28-Feb-14  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_SOLIDS_MODEL_LIMITATIONS

! Global Variables:
!---------------------------------------------------------------------//
! Runtime Flag: TFM solids present.
      use run, only: TFM_SOLIDS
! Runtime Flag: DEM solids present.
      use run, only: DEM_SOLIDS
! Runtime Flag: PIC solids present.
      use run, only: PIC_SOLIDS
! Runtime Flag: Invoke a cohesion model for DES simulation.
      use discretelement, only: USE_COHESION
! Runtime Flag: Solve energy equations
      use run, only: ENERGY_EQ
! Runtime Flag: Solve species equations
      use run, only: SPECIES_EQ

! Number of solid phases specified by the user/TFM model.
      use physprop, only: SMAX
! Number of discrete solids.
      use discretelement, only: DES_MMAX

      implicit none

! Local Variables:
!---------------------------------------------------------------------//
! Total number of solids phases.
      INTEGER :: MMAX_TOT

! Set up the total number of solids.
      MMAX_TOT = SMAX + DES_MMAX

! The cohesion model is only implemented for DEM simulations
      IF(USE_COHESION) THEN
         IF(TFM_SOLIDS .OR. PIC_SOLIDS) THEN
            WRITE(ERR_MSG, 2000)
            CALL LOG_ERROR()
         ENDIF

 2000 FORMAT('Error 2000: The solids cohesion model is only available',&
         ' for DEM',/' solids. Please correct the project settings.')
      ENDIF

! Place holder
      IF(ENERGY_EQ .AND. (TFM_SOLIDS .OR. PIC_SOLIDS)) THEN
!         WRITE(ERR_MSG, 2002)
!         CALL LOG_ERROR()

 ! 2002 FORMAT('Error 2002: The solids-solids conduction model is only', &
 !         ' available',/' for DEM only. Please correct the project settings.')
      ENDIF


! This is only implemented for pure TFM or pure DEM simulations.
      IF(any(SPECIES_EQ(1:MMAX_TOT))) THEN
         IF(TFM_SOLIDS .AND. DEM_SOLIDS) THEN
            WRITE(ERR_MSG, 5000)
            CALL LOG_ERROR()
         ENDIF

 5000 FORMAT('Error 5000: Species equations are not available with',   &
         ' the hybrid',/'solids model. Please correct the project settings.')
      ENDIF

      RETURN

   END SUBROUTINE CHECK_SOLIDS_MODEL_LIMITATIONS

END MODULE CHECK_SOLIDS_PHASES_MOD
