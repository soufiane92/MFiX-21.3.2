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
#include "error.inc"

      subroutine usr1_des

        use compar, only: mype, pe_io
        use mpi_utility, only: bcast
        use des_rxns
        use des_thermo
        use discretelement
!        use error_manager
        use run
        use usr
        use constant, only: pi
        use stl_preproc_des
        use GET_STL_DATA_MOD
  
        implicit none
        
        integer :: i,f
        DOUBLE PRECISION :: TWOPI


        if(.not.read_kf(1)) then
           write(err_msg, '(A)') 'Keyframe data was not read. Make sure read(kf(1) is set to .true.'
           call log_error()
        endif


! Interpolate data only in root process
        if(mype == pe_io) then
          call interpolate_keyframe_data(time)
! Use the interpolated data as per user needs
! kf_data(i)%var_current(:) holds a list of variables at the current time
! The number of variable corresponds to the number of columns in each
! keyframe file (2 columns means 1 variable since the first column represents
! time)
! Here, interpolated data is converted from rotations per second to
! radians per second

          TWOPI = 2.0D0*DACOS(-1.0D0)

! Get rotation angles
          do i = 1, 2
            IF(TIME>0) THEN
               rot_angle(i)   = rot_angle(i) + kf_data(1)%var_current(i) * TWOPI *DTSOLID  !  rotation angle in radians
            ELSE
               rot_angle(i)   =  0.0D0
            ENDIF
          end do
        end if

! Broadcast the computed screw rotational velocity to other processes
        call bcast(rot_angle, pe_io)

        if(mod(msg_counter,10)==0) then
           write(err_msg, '(1x,"Time=",F6.4," sec. Rotation angles (deg) = ", F8.2," ; ",F8.2)') time, rot_angle(1)/TWOPI*360.0, rot_angle(2)/TWOPI*360.0
           call log_status()
        endif
        msg_counter = msg_counter + 1

! Rotate STL files        
        CALL Rotate_STL(is_1_group_id, 'Z', cyl_center(:), rot_angle(1))
        CALL Rotate_STL(is_2_group_id, 'Z', cyl_center(:), rot_angle(2))

! Bin the STL to the DES grid (brute force, this could be optimizedto only rebin
! the moving facets).
        CALL BIN_FACETS_TO_DG


        return
      end subroutine usr1_des
