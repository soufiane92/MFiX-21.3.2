#include "error.inc"

MODULE CHECK_PLANE_MOD
   use error_manager

   CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: CHECK_PLANE                                            C
!  Purpose: make sure the flow boundary condition or internal surface  C
!           is a plane                                                 C
!                                                                      C
!  Author: P. Nicoletti                               Date: 10-DEC-91  C
!  Reviewer: M.SYAMLAL, W.ROGERS, P.NICOLETTI         Date: 24-JAN-92  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE CHECK_PLANE(X_CONSTANT, Y_CONSTANT, Z_CONSTANT, BC, NAME)

         USE compar
         USE funits

      IMPLICIT NONE

!-----------------------------------------------
! Dummy arguments
!-----------------------------------------------
! surface indicators
      LOGICAL, INTENT(IN) :: X_CONSTANT,Y_CONSTANT,Z_CONSTANT
! boundary condition or internal surface index
      INTEGER, INTENT(IN) ::  BC
! BC or IS
      CHARACTER(LEN=2), INTENT(IN) :: NAME
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! number of directions that are not constant (must equal 2)
      INTEGER :: N
!-----------------------------------------------

! number of directions that are not constant (must equal 2)
      N = 3
      IF (X_CONSTANT) N = N - 1
      IF (Y_CONSTANT) N = N - 1
      IF (Z_CONSTANT) N = N - 1

      IF (N /= 2) THEN
         WRITE (err_msg, 1000) NAME, BC
         call log_error()
      ENDIF

      RETURN

 1000 FORMAT(/' From: CHECK_PLANE',/'Message: ',A,' No ',I3,&
         ' is not a plane',/)

   END SUBROUTINE CHECK_PLANE

END MODULE CHECK_PLANE_MOD
