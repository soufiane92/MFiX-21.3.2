#include "error.inc"

MODULE qmomk_make_arrays_mod

   USE compar
   USE error_manager
   USE funits
   USE geometry
   USE param1
   USE qmom_kinetic_equation
   USE qmomk_read_restart_mod, only: qmomk_read_restart
   USE run

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: QMOMK_MAKE_ARRAYS                                      C
!  Purpose: DES - Initialize/Restart QMOMK arrays                      C
!                                                                      C
!                                                                      C
!  Author: Alberto Passalacqua                        Date:            C
!  Reviewer:                                          Date:            C
!  Comments:                                                           C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

   SUBROUTINE qmomk_make_arrays

      IMPLICIT NONE

      INTEGER CHECK_MPI

      IF(COORDINATES == 'CYLINDRICAL') THEN
         WRITE (err_msg, *) ' '
         WRITE (err_msg, *) 'Cylindrical coordinates are being used. STOP'
         WRITE (err_msg, *) 'QMOMK should only be run using cartesian coordinates.'
         call log_error()
      END IF

      CHECK_MPI = NODESI * NODESJ * NODESK
      IF((CHECK_MPI.NE.1).AND.(QMOMK)) THEN
         WRITE (err_msg, *) ' '
         WRITE (err_msg, *) 'QMOMK being run on multiple processors. STOP'
         WRITE (err_msg, *) 'QMOMK should only be run serially on one processor.'
         call log_error()
      END IF

      IF(RUN_TYPE == 'RESTART_1') THEN !  Read Restart
         CALL QMOMK_READ_RESTART
         WRITE(err_msg,*) 'QMOMK_RES file read at Time= ', TIME
      ELSE IF (RUN_TYPE == 'RESTART_2') THEN
         WRITE(err_msg,*) 'Restart 2 is not implemented with QMOMK'
         call log_error()
      END IF

      RETURN

   END SUBROUTINE qmomk_make_arrays

END MODULE qmomk_make_arrays_mod
