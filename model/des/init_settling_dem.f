MODULE INIT_SETTLING_DEM_MOD

   use calc_force_dem_mod, only: calc_force_dem
   use cfnewvalues_mod, only: cfnewvalues
   use desgrid, only: desgrid_pic
   use discretelement, only: des_continuum_coupled, do_nsearch
   use discretelement, only: neighbor_search_n, nfactor, particles, print_des_data
   use error_manager
   use functions, only: is_nonexistent
   use geometry, only: x_max, x_min, y_max, y_min, z_max, z_min
   use mpi_funs_des, only: des_par_exchange
   use neighbour_mod, only: neighbour
   use particles_in_cell_mod, only: particles_in_cell
   use run, only: run_type
   use write_des_data_mod, only: write_des_data

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Module name: MAKE_ARRAYS_DES                                        !
!  Author: Jay Boyalakuntla                           Date: 12-Jun-04  !
!                                                                      !
!  Purpose: DES - allocating DES arrays
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

#include "version.inc"

   SUBROUTINE INIT_SETTLING_DEM

      IMPLICIT NONE
!-----------------------------------------------
! Local variables
!-----------------------------------------------
      INTEGER :: FACTOR

      real :: mins(3), maxs(3)

!-----------------------------------------------
! Include statement functions
!-----------------------------------------------


! Skip this routine if there are no particles.
      IF(PARTICLES == 0) RETURN
! Skip this routine if not a new run.
      IF(RUN_TYPE /= 'NEW') RETURN

! Skip if not coupled.
      IF(.NOT.DES_CONTINUUM_COUPLED) RETURN

! Write the initial configuration before settling
      IF(PRINT_DES_DATA .AND. NFACTOR>0) CALL WRITE_DES_DATA

      WRITE(ERR_MSG, 1100) trim(iVal(NFACTOR))
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
 1100 FORMAT('Beginning DEM settling period: ',A,' steps.')

! Disable the coupling flag.
      DES_CONTINUUM_COUPLED = .FALSE.

      mins(1) = X_MIN
      mins(2) = Y_MIN
      mins(3) = Z_MIN
      maxs(1) = X_MAX
      maxs(2) = Y_MAX
      maxs(3) = Z_MAX

      DO FACTOR = 1, NFACTOR
! calculate forces

         CALL CALC_FORCE_DEM
! update particle position/velocity

         CALL CFNEWVALUES
! set the flag do_nsearch before calling particle in cell (for mpi)
         DO_NSEARCH = (MOD(FACTOR,NEIGHBOR_SEARCH_N)==0)

! Bin the particles to the DES grid.
         CALL DESGRID_PIC(.TRUE.)
! exchange particle crossing boundaries and updates ghost particles
         CALL DES_PAR_EXCHANGE
! find particles on grid
         CALL PARTICLES_IN_CELL
! perform neighbor search
         IF(DO_NSEARCH) CALL NEIGHBOUR
      ENDDO

! Reset the comoupling flag.
      DES_CONTINUUM_COUPLED = .TRUE.

      WRITE(ERR_MSG, 1200)
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
 1200 FORMAT('DEM settling period complete.')

! this write_des_data is needed to properly show the initial state of
! the simulation (granular or coupled). In the coupled case, the
! particles may have 'settled' according to above.  In the granular
! case, the initial state won't be written until after the particles
! have moved without this call.
!      IF(PRINT_DES_DATA) CALL WRITE_DES_DATA

      RETURN

   END SUBROUTINE INIT_SETTLING_DEM

END MODULE INIT_SETTLING_DEM_MOD
