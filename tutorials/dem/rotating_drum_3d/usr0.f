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
! Allocate center endpoints, axis and omega for cylinder(s) 
        if(kf_count>0) then
           allocate(cyl_center1(kf_count, dimn), stat = allocstatus)
           allocate(cyl_center2(kf_count, dimn), stat = allocstatus)
           allocate(cyl_axis(kf_count, dimn), stat = allocstatus)
           allocate(cyl_omega(kf_count, dimn), stat = allocstatus)

! Assign the center endpoints of cylinder(s)
! In this case, there is only one cylinder
           if(list(1)>0) then
             cyl_center1(list(1), :) = (/0., 0., 0./)
             cyl_center2(list(1), :) = (/0., 0., 0.1/)
           end if
           cyl_axis = cyl_center2 - cyl_center1

! Normalize the cylinder axis if not done already
           axisnorm = norm2(cyl_axis, dim=2)
           do i = 1, kf_count
             cyl_axis(i,:) = cyl_axis(i,:)/axisnorm(i)
           end do

        endif
        return
      end subroutine usr0



