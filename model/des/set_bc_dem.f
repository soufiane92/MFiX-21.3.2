#include "error.inc"

MODULE SET_BC_DEM_MOD
CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: SET_BC_DEM                                              !
!  Author: J.Musser                                   Date: 13-Jul-09  !
!                                                                      !
!  Purpose: Check the data provided for the des mass inflow boundary   !
!  condition and flag errors if the data is improper.  This module is  !
!  also used to convert the proveded information into the format       !
!  necessary for the dependent subrountines to function properly.      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE SET_BC_DEM

      USE bc
      USE constant
      USE des_bc
      USE discretelement
      USE error_manager
      USE funits
      USE geometry
      USE indices
      USE mfix_pic
      USE mpi_utility
      USE param
      USE param1
      USE physprop
      USE run
      USE set_bc_dem_mi_mod, only: set_bc_dem_mi
      USE set_bc_dem_mo_mod, only: set_bc_dem_mo

      IMPLICIT NONE

! The variable PARTICLES should already be set by this point if using
! gener_part_config option
      IF(PARTICLES == UNDEFINED_I) THEN
         PARTICLES = 0
      ENDIF

! If the system is started without any particles and an inlet is not
! specified, the run is likely aborted.
! Inlet/outlet for MPPIC are based off the regular mfix declarations,
! and so DEM_BCMI could still be zero.
! Only check for new runs.
      IF(RUN_TYPE(1:3) == 'NEW'.AND.PARTICLES == 0 .AND. DEM_BCMI == 0) THEN
         WRITE(ERR_MSG, 1202)
         CALL LOG_WARNING()
      ENDIF

 1202 FORMAT('WARNING 1202: The system is initiated with no particles',&
         ' and no',/'solids inlet was detected.')

      IF(DEM_BCMI > 0) CALL SET_BC_DEM_MI
      IF(DEM_BCMO > 0) CALL SET_BC_DEM_MO

      RETURN

   END SUBROUTINE SET_BC_DEM

END MODULE SET_BC_DEM_MOD
