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
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C


!----------------------------------------------------------------------!
!                                                                      !
!  Subroutine: usr0                                                    !
!  Author: Sathish Sanjeevi                         Date:23-Oct-2019   !
!                                                                      !
!  Purpose: Read keyframe data from root process and broadcast         !
!  information to all other processes                                  !
!                                                                      !
!----------------------------------------------------------------------!

      subroutine usr0

        use des_rxns
        use des_thermo
        use discretelement
        use run
        use usr

        implicit none
        integer :: i, allocstatus
        double precision, allocatable, dimension(:) :: axisnorm


! Note that dynamic BCs and keyframe flags are in same order
! in our case. This can be changed as per user needs, but
! consistency needs to maintained while accessing the indices
! Each dynamic bc (i.e. moving STL wall) is linked to a keyframe file.
        dynamic_bc = read_kf

! Read and store keyframe data information
        call read_keyframe_data

! We now have kf_count keyframe data available
! Define conveyor belt directions and velocities
        if(kf_count>0) then
           allocate(belt_dir(kf_count, dimn), stat = allocstatus)
           allocate(belt_vel(kf_count, dimn), stat = allocstatus)
           allocate(axisnorm(kf_count), stat = allocstatus)

! Assign the belt direction. Refer to list and rlist in usr_mod.f
           if(list(3)>0) belt_dir(list(3), :) = (/0., 0., -1./)
           if(list(4)>0) belt_dir(list(4), :) = (/-1., 0., 0./)


! Normalize the direction of the belt if not done already
           axisnorm = norm2(belt_dir, dim=2)
           do i = 1, kf_count
             belt_dir(i,:) = belt_dir(i,:)/axisnorm(i)
           end do

        endif
        return
      end subroutine usr0



