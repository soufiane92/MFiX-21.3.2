!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR0                                                   C
!  Purpose: This routine is called before the time loop starts and is  C
!           user-definable.  The user may insert code in this routine  C
!           or call appropriate user defined subroutines.  This        C
!           can be used for setting constants and checking errors in   C
!           data.  This routine is not called from an IJK loop, hence  C
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
      SUBROUTINE USR0

      USE USR
      USE discretelement, only:DES_POS_NEW,DES_USR_VAR

      IMPLICIT NONE

!-----------------------------------------------
!
!  Include files defining common blocks here
!
!
!  Define local variables here
      integer :: allocstatus

! Initial particles were read from particle_input.dat.
! Particles below y=0.1 represent the screw.
! They are flagged with a value of DES_USR_VAR = 1.0
! Regular particles are set a value of DES_USR_VAR = 0.0
! The screw particles will be assign a velocity in cfnewvalues.f

      DES_USR_VAR(1,:) = 0.0D0
      WHERE(DES_POS_NEW(:,2)<0.1D0) DES_USR_VAR(1,:) = 1.0D0



! Read and store keyframe data information
      call read_keyframe_data

! We now have kf_count keyframe data available
! Define conveyor belt directions and velocities
      if(kf_count>0) then
         allocate(screw_omega(kf_count), stat = allocstatus)
      endif



      RETURN
      END SUBROUTINE USR0
