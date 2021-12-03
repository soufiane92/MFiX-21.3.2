!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR3                                                   C
!  Purpose: This routine is called after the time loop ends and is
!           user-definable.  The user may insert code in this routine
!           or call appropriate user defined subroutines.
!           This routine is not called from an IJK loop, hence
!           all indices are undefined.                                 C
!                                                                      C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced:                                               C
!  Variables modified:                                                 C
!                                                                      C
!  Local variables:                                                    C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
      SUBROUTINE USR3

      USE fldvar, only: v_g, rop_g, x_g
      USE functions, only: funijk
      USE geometry, only: jmax1

      IMPLICIT NONE

!-----------------------------------------------
!
!  Include files defining common blocks here
!
!
!  Define local variables here
!
      DOUBLE PRECISION conv
      INTEGER IJK, IJK2, IJK1
!
!  Include files defining statement functions here
!
!  Insert user-defined code here
!
      IJK1 = FUNIJK(2, 1, 1)
      IJK2 = FUNIJK(2, JMAX1, 1)
      conv =  1. -  ( (X_g(IJK2, 1) * ROP_g(IJK2) * V_g(IJK2))   &
                      /(X_g(IJK1, 1) * ROP_g(IJK1) * V_g(IJK1))  &
                    )
      Open(5,File='POST_Conversion.dat')
      write(5,'(//A,G12.5//)')' Conversion = ', conv
      Close(5)
      RETURN

      END SUBROUTINE USR3
