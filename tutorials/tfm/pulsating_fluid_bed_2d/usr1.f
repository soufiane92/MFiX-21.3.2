!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR1                                                   C
!  Purpose: This routine is called from the time loop and is           C
!           user-definable.  The user may insert code in this routine  C
!           or call appropriate user defined subroutines.  This        C
!           can be used for setting or checking errors in quantities   C
!           that vary with time.  This routine is not called from an   C
!           IJK loop, hence all indices are undefined.                 C               C
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
      SUBROUTINE USR1

      USE USR
      use set_bc0_flow_mod, only: set_bc0_vel_inflow
      use bc, only: bc_v_g
      use run, only: time

      IMPLICIT NONE

!-----------------------------------------------
!
!  Include files defining common blocks here
!
!
!  Define local variables here
        integer :: i
!
!  Include files defining statement functions here
!
!
!  Insert user-defined code here
!
! Interpolate data only in root process
        if(mype == pe_io) then
          call interpolate_keyframe_data(time)
          do i = 1, kf_count
! Use the interpolated data as per user needs
! kf_data(i)%var_current(:) holds a list of variables at the current time
! The number of variable corresponds to the number of columns in each
! keyframe file (2 columns means 1 variable since the first column represents
! time)
! Here, rotational velocity is computed for the drum(s)
            bc_v_g(i) = kf_data(i)%var_current(1)
          end do
        end if
        
        call set_bc0_vel_inflow(1)
      RETURN
      END SUBROUTINE USR1
