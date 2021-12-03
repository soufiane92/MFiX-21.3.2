!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: URS1_DES                                               !
!                                                                      !
!  Purpose: This routine is called within the discrete phase time loop !
!  after the source terms have been calculated but before they are     !
!  applied. The user may insert code in this routine or call user      !
!  defined subroutines.                                                !
!                                                                      !
!  This routine is called from the time loop, but no indicies (fluid   !
!  cell or particle) are defined.                                      !
!                                                                      !
!  Author: J.Musser                                   Date: 06-Nov-12  !
!                                                                      !
!  Comments:                                                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

!----------------------------------------------------------------------!
!                                                                      !
!  Subroutine: usr1_des                                                !
!  Author: Sathish Sanjeevi                         Date:23-Oct-2019   !
!                                                                      !
!  Purpose: Interpolate keyframe data from root process and broadcast  !
!  information to all other processes                                  !
!                                                                      !
!----------------------------------------------------------------------!

      subroutine usr1_des

        use compar, only: mype, pe_io
        use mpi_utility, only: bcast
        use des_rxns
        use des_thermo
        use discretelement
        use run
        use usr
        use constant, only: pi
  
        implicit none
        
        integer :: i

! Interpolate data only in root process
        if(mype == pe_io) then
          call interpolate_keyframe_data(time)
          do i = 1, kf_count
! Use the interpolated data as per user needs
! kf_data(i)%var_current(:) holds a list of variables at the current time
! The number of variable corresponds to the number of columns in each
! keyframe file (2 columns means 1 variable since the first column represents
! time)
! Here, interpolated data is converted from rotations per second to
! radians per second
            screw_omega(1) = kf_data(i)%var_current(1)*2.0*pi
          end do
        end if

! Broadcast the computed screw rotational velocity to other processes
        call bcast(screw_omega, pe_io)
        return
      end subroutine usr1_des
