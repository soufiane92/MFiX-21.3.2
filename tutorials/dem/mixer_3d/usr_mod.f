#include "error.inc"

    MODULE usr
!
!     Declare the user-defined namelist variables (usrnlst.inc) in this module.
!     Also Include user-defined variables in this module.  To access the
!     variables from a subroutine add the statement "Use usr".  If allocatable
!     arrays are defined in this module allocate them in usr0.  To turn on the
!     user defined subroutines (usr0, usr1, and usr2) set keyword CALL_USR to true.

      use error_manager
      use compar, only: mype, pe_io
      use mpi_utility, only: bcast
      use discretelement, only: dimn, des_crossprdct
      use fs_util, only: file_exists
      use compar, only: mype, pe_io
      use stl
      use param1, only: zero, one
      use exit, only: mfix_exit

    IMPLICIT NONE
! Cylinder center
        double precision, dimension(3) :: cyl_center
! Axis of cylinder
        double precision, dimension(3) :: cyl_axis
! Rotational velocity of cylinder(s)
        double precision, dimension(10,3) :: cyl_omega
! Rotation angle (deg)
        double precision,dimension(10) :: rot_angle

! Maximum number of keyframes (for corresponding BC IDs) allowed
      integer, parameter :: maxcount = 100
! Keyframe flag
      logical, dimension(maxcount) ::  read_kf
! Total number of keyframe tables
      integer :: kf_count
! Flag for dynamic wall boundaries
      logical, dimension(maxcount) ::  dynamic_bc
! Message display counter
       integer :: msg_counter=0


! Struct for storing keyframe data
      type kf_struct
        integer :: ID
        integer :: nrows, nvars
        integer :: timeindex
        character(len=512) :: filename
        character(len=10) :: interp_method
        double precision, allocatable :: time(:)
        double precision, allocatable :: var(:,:)
        double precision, allocatable :: var_current(:)
      end type kf_struct


      type(kf_struct), allocatable,  dimension(:) :: kf_data

      integer, dimension(maxcount) :: list
      integer, dimension(maxcount) :: rlist

      integer :: is_1_group_id, is_2_group_id



!     Vertex Coordinates X ,Y and Z
      ! DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:) :: VERTEX_INIT
!     Face normal vector (normalized)
      ! DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: NORM_FACE_INIT
      !     Moving facets staring and ending indices
      ! INTEGER, DIMENSION(100) :: MV_STL_START, MV_STL_END

contains





!----------------------------------------------------------------------!
! Below is the code for keyframe data management.                      !
! It is anticipated that users won't need to modify it.                !
!----------------------------------------------------------------------!

!----------------------------------------------------------------------!
!                                                                      !
!  Subroutine: read_keyframe_data                                      !
!  Author: Sathish Sanjeevi                         Date:23-Oct-2019   !
!                                                                      !
!  Purpose: Keyframes are data tables provided by user to perform user-!
!           defined operations such as ramping or other time-dependent !
!           operations. This functions reads keyframe files provided   !
!           by the user.                                               !
!                                                                      !
!----------------------------------------------------------------------!

      subroutine read_keyframe_data

! Dummy arguments:
!---------------------------------------------------------------------
      integer i, j, k
      integer id, fileunit, allocstatus, io
      character(len = 8) :: formt, xi
      double precision :: timeinit


! Read and store keyframe data in root process
      if(myPE == pe_io) then


         i = 0
         list = 0
         rlist = 0

! Dry run to count actual number of keyframes
         do id = 1, maxcount
           if(read_kf(id) .eqv. .true.) then
             i = i+1

! Keyframe IDs can be non-contiguous. Efficient storage needs contiguous
! structs. Store indices based on KF IDs and vice versa.
             list(id) = i
             rlist(i) = id
           end if
         end do
         kf_count = i
         print *, "Total number of keyframe files : ", kf_count
         allocate(kf_data(kf_count), stat = allocstatus)

! Loop to read each keyframe file
         do i = 1, kf_count
             id = rlist(i)
             write(xi, '(I0.4)') id
             kf_data(i)%filename = 'data_kf_'//trim(xi)//'.txt'
             print *, 'Current keyframe file is : ', kf_data(i)%filename

! Report error if file does not exists
             if (.not. file_exists(kf_data(i)%filename)) then
                write(err_msg, 9999)  trim(kf_data(i)%filename)
9999            format(2/,1x,70('*')/' from: usr_mod.f',/' error 9999: ',    &
                     'input file does not exist: ',a/,'aborting.',/1x,   &
                     70('*'),2/)
                call log_error()
             endif

! Read keyframe file
             fileunit = 17
             open(newunit = fileunit, file = kf_data(i)%filename)
             read(fileunit, *) kf_data(i)%nrows, kf_data(i)%nvars
             read(fileunit, *) kf_data(i)%interp_method
             read(fileunit, *)   ! Dummy read to ignore header
             kf_data(i)%timeindex = 1
             allocate(kf_data(i)%time(kf_data(i)%nrows))
             allocate(kf_data(i)%var(kf_data(i)%nrows, &
                 kf_data(i)%nvars))
! Actual reading of keyframe table
             do j = 1, kf_data(i)%nrows
               read(fileunit, *, iostat = io) kf_data(i)%time(j), &
                 (kf_data(i)%var(j,k), k = 1, kf_data(i)%nvars)
               ! check if file is in proper condition
               if (io>0) then
                 write(err_msg, 1111) j+3, kf_data(i)%filename
1111             format('Check if data is a valid floating point at line : ', i5/,&
                        'in file : ', a)
                 call log_error()
               else if (io<0) then
                 write(err_msg, 2222) kf_data(i)%filename
2222             format('End of file reached earlier in file : ', a)
                 call log_error()
               end if
             end do
             close(fileunit)

! Important: check if keyframe times are sorted in ascending order
             timeinit = 0.D0
             do j = 1, kf_data(i)%nrows
               if(kf_data(i)%time(j) .ge. timeinit) then
                 timeinit = kf_data(i)%time(j)
               else
                write(err_msg, 9555) trim(kf_data(i)%filename), j+2
9555            format('Time cannot be decreasing. Check keyframe file : ',a/,&
                       'first column (time) at line : ', i5)
                call log_error()
               end if
             end do

! Initialize starting keyframe variable to the first one
             kf_data(i)%var_current = kf_data(i)%var(1,:)
             kf_data(i)%timeindex = 1
         end do

         print *, "Keyframe read complete!"

      end if

! Broadcast information to all processes from root
      call bcast(kf_count, pe_io)
      call bcast(list, pe_io)
      call bcast(rlist, pe_io)
      end subroutine read_keyframe_data



!----------------------------------------------------------------------!
!                                                                      !
!  Subroutine: interpolate_keyframe_data                               !
!  Author: Sathish Sanjeevi                         Date:23-Oct-2019   !
!                                                                      !
!  Purpose: Interpolate keyframe data based on current simulation      !
!           timestep                                                   !
!                                                                      !
!----------------------------------------------------------------------!


      subroutine interpolate_keyframe_data(time)
      double precision, intent(in) :: time
      integer i
      integer tindex, nrows
      character(len=10) :: interp
      double precision :: begintime, timeend
      double precision :: kftime, kftime_next
      double precision :: denom
      double precision, allocatable :: kfvar(:), kfvar_next(:)
      logical :: flag

! Interpolate keyframe value based on current timestep
      do i = 1, kf_count
        nrows  = kf_data(i)%nrows
        interp = kf_data(i)%interp_method
        begintime = kf_data(i)%time(1)
        timeend   = kf_data(i)%time(nrows)
        tindex = kf_data(i)%timeindex
        kftime = kf_data(i)%time(tindex)
        kftime_next = kf_data(i)%time(tindex+1)
        kfvar = kf_data(i)%var(tindex,:)
        kfvar_next = kf_data(i)%var(tindex+1,:)


! Interpolate only if the simulation time lies within bounds of
! keyframe time data
        if(time.gt.begintime.and.time.lt.timeend) then
          flag = time.gt.kf_data(i)%time(tindex).and.&
            time.le.kf_data(i)%time(tindex+1)

! Loop repeatedly until the simulation time falls within the
! corresponding keyframe time intervals. This can happen if keyframe
! timesteps are smaller than the simulation timesteps
          do while(.not.flag)
            tindex = tindex + 1
            kf_data(i)%timeindex = tindex
            kftime = kf_data(i)%time(tindex)
            kftime_next = kf_data(i)%time(tindex+1)
            kfvar = kf_data(i)%var(tindex,:)
            kfvar_next = kf_data(i)%var(tindex+1,:)
            flag = time.gt.kf_data(i)%time(tindex).and.&
              time.le.kf_data(i)%time(tindex+1)
          end do

! Interpolation can be a `linear` one or `step` (floor) function
          if(time.gt.kftime.and.time.le.kftime_next) then
            if(interp.eq.'linear') then
              denom = kftime_next - kftime
! Check if denominator is zero
              if(denom.gt.0.D0) then
                kf_data(i)%var_current(:) = kfvar(:) + &
                  (time - kftime)/denom * (kfvar_next(:) - kfvar(:))
               ! print*,'iterpolate',time,kftime,i,kf_data(i)%var_current(:)
              else
                kf_data(i)%var_current(:) = kfvar(:)
              end if
            elseif(interp.eq.'step') then
              kf_data(i)%var_current(:) = kfvar(:)
            else
              write(*,5555)
 5555         FORMAT('Enter a valid interpolation scheme: linear or step')
            end if
          end if
        end if

! Use the first or last keyframe time when simulation time is outside
! keyframe bounds
        if(time.le.begintime) &
          kf_data(i)%var_current(:) = kf_data(i)%var(1,:)
        if(time.ge.timeend) &
          kf_data(i)%var_current(:) = kf_data(i)%var(nrows,:)
      end do
      end subroutine interpolate_keyframe_data







      END MODULE usr
