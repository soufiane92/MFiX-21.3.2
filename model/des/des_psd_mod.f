#include "error.inc"
!-------------------------------------------------------------------------!
!                                                                         !
! Purpose:                                                                !
!    This module contains modules and subroutines relevant to the         !
!    use of custom distribution and other distributions for particle      !
!    initialization and mass inflow routines.                             !
!                                                                         !
! Author: Sathish Sanjeevi                                                !
!                                                                         !
!-------------------------------------------------------------------------!

module des_psd ! psd - particle size distribution
#ifdef MPI
   use mpi
#endif
   use error_manager
   use param, only: dimension_ic, dimension_bc, dim_m
   use compar, only: PE_IO, myPE
! particle size distribution: IC
   use ic, only: IC_PSD_TYPE
   use ic, only: IC_PSD_MEAN_DP, IC_PSD_STDEV,IC_PSD_MU,IC_PSD_SIGMA
   use ic, only: IC_PSD_MIN_DP
   use ic, only: IC_PSD_MAX_DP
! particle size distribution: BC
   use bc, only: BC_PSD_TYPE
   use bc, only: BC_PSD_MEAN_DP, BC_PSD_STDEV,BC_PSD_MU, BC_PSD_SIGMA
   use bc, only: BC_PSD_MIN_DP
   use bc, only: BC_PSD_MAX_DP
! CGDEM   
   use discretelement, only: CGDEM, cgp_stat_wt, cgp_scaling_method
! MPPIC   
   use mfix_pic, only: MPPIC
! Parameter for small numbers
   use param1, only: SMALL_NUMBER, UNDEFINED, HALF
! Utility to check if file exists
   use fs_util, only: file_exists
! Random number functions
   use randomno

  IMPLICIT NONE
! Max number of "data" rows allowed for custom distribution
     integer, parameter :: max_rows = 100
! Max number of custom distributions allowed for ICVs and BCVs separately
     integer, parameter :: max_dist = 10
! Max number of CVs allowed (=2. ICV and BCV)
     integer, parameter :: max_cv = 2

! Total number of keyframe tables (aka custom distributions)
     integer :: kf_count(2) ! Index 1 for ICV and 2 for BCV
! Struct for storing custom distribution keyframe data
     type dist_struct
       ! Number of data rows
       integer :: nrows
       ! Diameter (x) of the distribution
       double precision :: x(max_rows)
       ! PDF/CDF data (y) corresponding to diameter (x)
       double precision :: y(max_rows)
     end type dist_struct

     type(dist_struct), dimension(max_dist,2) :: dist_data

! Lookup table for contiguous indexing (from non-contiguous ICVs/BCVs and phase M)
! Here, dimension_ic also refers dimension_bc since both default values are equal (=500)

! Lookup table for each phase M and IC or BC value (1 or 2 respectively) to index contiguously
     integer, dimension(dim_m, dimension_ic, 2)  :: list
! Reverse lookup table to get ICVs/BCVs and phase number M (from respective contiguous indices)
     integer, dimension(max_dist, 2)      :: rlist_cv
     integer, dimension(max_dist, 2)      :: rlist_m
! Pointer to the custom datatype
     integer :: cust_datatype
contains

  subroutine create_custom_datatype
#ifdef MPI
! Temporary variables to create MPI derived datatype
     type(dist_struct) :: temp
     integer(kind=mpi_address_kind) :: displ(4), base
     integer, parameter :: struct_vars = 3
! Define the block_length with the number of elements in struct
     integer, parameter :: block_length(3) = (/1, max_rows, max_rows/)
! Define the block_type with the elements of the struct
     integer, parameter :: block_type(3) = (/mpi_integer, mpi_double_precision, mpi_double_precision/)
     integer :: ierr

! Get the extents for different datatypes in struct
     call mpi_get_address(temp%nrows, displ(1), ierr)
     call mpi_get_address(temp%x(1),  displ(2), ierr)
     call mpi_get_address(temp%y(1),  displ(3), ierr)
! Compute the block displacements
     base = displ(1)
     displ(1) = displ(1) - base     ! displ(1) = 0
     displ(2) = displ(2) - base
     displ(3) = displ(3) - base

     call mpi_type_create_struct(struct_vars, block_length, displ, block_type, cust_datatype, ierr)
     call mpi_type_commit(cust_datatype, ierr)
#endif
  end subroutine create_custom_datatype



!----------------------------------------------------------------------!
!                                                                      !
!  Subroutine: prepare_custom_dist                                     !
!  Author: Sathish Sanjeevi                         Date:30-Jul-2020   !
!                                                                      !
!  Purpose : Perform dry run to count number of 'custom' distributions !
!    for ICVs and BCVs, and create a lookup table to save struct data  !
!    contiguously for efficient use of memory                          !
!                                                                      !
!----------------------------------------------------------------------!

      subroutine prepare_custom_dist(lIndx)
      use mpi_utility, only: bcast
      implicit none
! lIndx=1 for ICV and lIndx=2 for BCV
      integer, intent(in) ::  lIndx
! Local variables
      integer :: lCV, lM, lCount, allocstatus
      character(64) :: tmp_string
      integer :: ierr

      if(myPE == pe_io) then ! if root process
        lCount = 0
        !list_cv(:, lIndx) = 0
        list(:, :, lIndx)  = 0
        rlist_cv(:, lIndx) = 0
        rlist_m(:, lIndx) = 0

! Dry run to count actual number of custom distributions
        do lCV = 1, dimension_ic ! or dimension_bc, since both are equal
          do lM = 1, dim_m
            if (lIndx == 1) then
              tmp_string = IC_PSD_TYPE(lCV, lM)
            else if (lIndx == 2) then
              tmp_string = BC_PSD_TYPE(lCV, lM)
            end if
            if (tmp_string == 'CUSTOM') then
              lCount = lCount+1

! Keyframe IDs can be non-contiguous. Efficient storage needs contiguous
! array of structs. Store indices based on contiguous IDs and vice versa.
!              list_cv(lCV, lIndx) = lCount
              list(lM, lCV, lIndx)   = lCount
              rlist_cv(lCount, lIndx) = lCV
              rlist_m(lCount, lIndx)  = lM
            end if
          end do
        end do
        kf_count(lIndx) = lCount

        if(lIndx == 1) tmp_string = 'ICV(s)'
        if(lIndx == 2) tmp_string = 'BCV(s)'

        if(kf_count(lIndx) > max_dist) then
          write(err_msg, 1010) trim(tmp_string), max_dist
          call log_error()
1010      format('Total number of custom distributions for all ',a,&
                ' exceeds max_dist (=', I3, '). Increase max_dist in des/des_psd_mod.f')
        end if


        if(kf_count(lIndx) > 0) then
          write(err_msg, 1015)
1015      format(76('='),/1x)
          call log_status()
          write(err_msg, 1020) trim(tmp_string), kf_count(lIndx)
          call log_status()
!          CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
1020      format('Total number of custom distributions for all ',a, ' is : ', I2)

! Read the custom distributions
          call read_custom_dist(lIndx)
        end if
      end if ! if root process
#ifdef MPI
! Broadcast information to all processes from root
      ! call bcast(kf_count, pe_io)
      ! call bcast(list_cv , pe_io)
      ! call bcast(list_m  , pe_io)
      ! call bcast(rlist_cv, pe_io)
      ! call bcast(rlist_m , pe_io)
      ! call mpi_bcast(dist_data, kf_count, cust_datatype, pe_io, MPI_COMM_WORLD, ierr)
      call bcast(kf_count(lIndx), pe_io)
      call bcast(list(:,:,lIndx) , pe_io)
!      call bcast(list_m(:,lIndx)  , pe_io)
      call bcast(rlist_cv(:,lIndx), pe_io)
      call bcast(rlist_m(:,lIndx) , pe_io)
      call mpi_bcast(dist_data(:,lIndx), kf_count(lIndx), cust_datatype, pe_io, MPI_COMM_WORLD, ierr)
#endif
      end subroutine prepare_custom_dist


      subroutine bcast_custom_dist_data
      end subroutine bcast_custom_dist_data


!----------------------------------------------------------------------!
!                                                                      !
!  Subroutine: read_custom_dist                                        !
!  Author: Sathish Sanjeevi                         Date:23-May-2020   !
!                                                                      !
!  Purpose: Keyframes are data tables provided by user to perform user-!
!     defined functions. Here, it is used to read custom probability   !
!     distribution. The user-specified table contains particle diameter!
!     against CDF                                                      !
!----------------------------------------------------------------------!

      subroutine read_custom_dist(lIndx)
      implicit none
! lIndx=1 for ICV and lIndx=2 for BCV
      integer, intent(in) :: lIndx

! Dummy arguments:
!---------------------------------------------------------------------
      integer i, j, k
      integer lCV, lM, fileunit, io
      character(len = 32) :: xi, dist_type, tmp_string
      character(len = 64) :: filename
      integer xindex
      double precision :: x_init, y_init
      character(len = 3):: IC_or_BC


! Read and store keyframe data in root process
!      if(myPE == pe_io) then

        if(lIndx == 1) tmp_string = 'IC'
        if(lIndx == 2) tmp_string = 'BC'

! Loop to read each keyframe file
         do i = 1, kf_count(lIndx)
             lM = rlist_m(i, lIndx)
             lCV = rlist_cv(i, lIndx)
             write(xi, '(I0.4, 1A, I0.4)') lCV,'_', lM
             filename = trim(tmp_string)//'_PSD_'//trim(xi)//'.txt'
             write(err_msg, 1030) trim(filename)
             CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
1030         format('Current custom distribution file read is : ',a)

! Report error if file does not exists
             if (.not. file_exists(filename)) then
                write(err_msg, 9999)  trim(filename)
9999            format(2/,1x,70('*')/' from: usr_mod.f',/' error 9999: ',    &
                     'input file does not exist: ',a/,'aborting.',/1x,   &
                     70('*'),2/)
                call log_error()
             endif

! Read keyframe file
             open(newunit = fileunit, file = filename)
             read(fileunit, *) dist_data(i,lIndx)%nrows
             read(fileunit, *) dist_type
             read(fileunit, *)   ! Dummy read to ignore header
             ! dist_data(i,lIndx)%xindex = 1

!             allocate(dist_data(i,lIndx)%x(dist_data(i,lIndx)%nrows))
!             allocate(dist_data(i,lIndx)%y(dist_data(i,lIndx)%nrows))
! Actual reading of keyframe table
             do j = 1, dist_data(i,lIndx)%nrows
               read(fileunit, *, iostat = io) dist_data(i,lIndx)%x(j), &
                 dist_data(i,lIndx)%y(j)
               ! check if file is in proper condition
               if (io>0) then
                 write(err_msg, 1111) j+3, filename
1111             format('Check if data is a valid floating point at line : ', i3/,&
                        'in file : ', a)
                 call log_error()
               else if (io<0) then
                 write(err_msg, 2222) filename
2222             format('End of file reached earlier in file : ', a)
                 call log_error()
               end if
             end do
             close(fileunit)

! Check distribution type is CDF or PDF
             if(.not.(dist_type.eq.'CDF' .or. dist_type.eq.'PDF')) then
               write(err_msg, 9455) trim(filename)
9455           format('Enter a valid input distribution type (PDF or CDF).'/'Check keyframe file : ',a/)
               call log_error()
             end if

! First distribution value must start with zero to indicate the lower bound of particle diameters
             if(dist_data(i,lIndx)%y(1) .ne. 0.D0) then
               write(err_msg, 9550) trim(filename)
9550           format('First value of PDF/CDF must start with zero.'/ &
                'This is to indicate the lower diameter bound of the custom distribution'/ &
                'Check keyframe file : ', a)
               call log_error()
             end if

! Check if diameter values are sorted in ascending order
             x_init = 0.D0
             do j = 1, dist_data(i,lIndx)%nrows
               if(dist_data(i,lIndx)%x(j) .ge. x_init) then
                 x_init = dist_data(i,lIndx)%x(j)
               else
                 write(err_msg, 9555) trim(filename), j+2, j+3
9555             format('Sort the diameters in increasing order.'/'Check keyframe file : ',a/,&
                        'Check first column (Particle diameter) at lines :', i3, '  and', i3)
                 call log_error()
               end if
             end do

! Ensure PDF/CDF does not have negative values
             do j = 1, dist_data(i,lIndx)%nrows
               if(dist_data(i,lIndx)%y(j) .lt. 0.D0) then
                 write(err_msg, 9565) trim(filename), j+3
9565             format('PDF/CDF cannot have negative values.'/'Check keyframe file : ',a,&
                        ' at line : ', i3)
                 call log_error()
               end if
             end do

! Check if CDFs are in ascending order
             if(dist_type == 'CDF') then
               y_init = 0.D0
               do j = 1, dist_data(i,lIndx)%nrows
                 if(dist_data(i,lIndx)%y(j) .ge. y_init) then
                   y_init = dist_data(i,lIndx)%y(j)
                 else
                   write(err_msg, 9755) trim(filename), j+2, j+3
9755               format('CDF cannot be decreasing. Check keyframe file : ',a/,&
                          'Check second column (CDF) at lines : ', i3, '  and', i3)
                   call log_error()
                 end if
               end do
             end if


! Convert distribution type from PDF to CDF, if given is a PDF
             if(dist_type == 'PDF') then
! Normalize the PDF such that area under PDF is 1
               dist_data(i,lIndx)%y = dist_data(i,lIndx)%y/sum(dist_data(i,lIndx)%y)
! Cumulative sum to achieve CDF
               do j=1, dist_data(i,lIndx)%nrows-1
                 dist_data(i,lIndx)%y(j+1) = dist_data(i,lIndx)%y(j)+dist_data(i,lIndx)%y(j+1)
               end do
             end if

!Sum of CDF=1 check (with some tolerance). Last line should equal 1
             if(abs(1.D0-dist_data(i,lIndx)%y(dist_data(i,lIndx)%nrows))>small_number) then
               write(err_msg, 9760) trim(filename), dist_data(i,lIndx)%nrows+3
9760           format('Area under PDF (i.e. final value of CDF) does not equal 1.'/&
                'Check keyframe file : ',a, '  at line :',i3)
               call log_error()
             end if

!! Initialize starting keyframe variable to the first one
!             dist_data(i,lIndx)%y_current = dist_data(i,lIndx)%y(1)
             !dist_data(i,lIndx)%xindex = 1


! Resising the PSD for CGDEM               
            IF(CGDEM .and. .not. MPPIC) THEN
                  IF(CGP_SCALING_METHOD(lM)==1) THEN
                   do j = 1, dist_data(i,lIndx)%nrows
                      dist_data(i,lIndx)%x(j) = dist_data(i,lIndx)%x(j) * CGP_STAT_WT(lM)**(1.d0/3.d0)
                   enddo

                  if (lIndx == 1) then
                    IC_or_BC = 'ICV'
                  else if (lIndx == 2) then
                    IC_or_BC = 'BCV'
                  end if
                  WRITE(ERR_MSG,400)IC_or_BC, lCV,lM,CGP_STAT_WT(lM)
                  CALL LOG_INFO()
               ENDIF
            ENDIF

400 FORMAT('Info: Using Coarse Grain DEM, Custom PSD is scaled up for ',A3, ' = ',I3, ', solids phase: ',I2/, &
           ' and a statistical weight of:', G9.2)
         end do

          write(err_msg, 9765)
9765      format(76('='),/1x)
          call log_status()

!      end if ! if root process
      end subroutine read_custom_dist



!----------------------------------------------------------------------!
!                                                                      !
!  Subroutine: inverse_transform_sampling                              !
!  Author: Sathish Sanjeevi                         Date:23-May-2020   !
!                                                                      !
!  Purpose: Compute "x" given a cumulative distribution function F(x)  !
!           and CDF value F'                                           !
!                                                                      !
!----------------------------------------------------------------------!


      function inverse_transform_sampling(unirno, lIndx, lCV, lM)
      double precision :: inverse_transform_sampling
! Input to the function is a uniformly distributed random number
      double precision, intent(in) :: unirno
! Index to denote IC or BC (1 or 2 respectively)
      integer, intent(in) :: lIndx
! (Initial or Boundary) Condition variable
      integer, intent(in) :: lCV
! Phase number M
      integer, intent(in) :: lM
      integer xindex, nrows, i
      double precision :: xstart, xend
      double precision :: kfx, kfx_next
      double precision :: denom
      double precision :: kfy, kfy_next
      logical :: flag

! Condition value CV can be non-contiguous. Access corresponding continuously indexed linked list


! Phase number M can be non-contiguous. Access corresponding continuously indexed linked list
      i = list(lM, lCV, lIndx)
! Interpolate keyframe value based on current x valuestep
!        i = 1
      nrows  = dist_data(i,lIndx)%nrows
      xstart = dist_data(i,lIndx)%x(1)
      xend   = dist_data(i,lIndx)%x(nrows)
      !xindex = dist_data(i,lIndx)%xindex
      !kfx = dist_data(i,lIndx)%x(xindex)
      !kfx_next = dist_data(i,lIndx)%x(xindex+1)
      !kfy = dist_data(i,lIndx)%y(xindex)
      !kfy_next = dist_data(i,lIndx)%y(xindex+1)




! Loop repeatedly until the simulation y value falls within the
! corresponding keyframe y value intervals.
        do xindex = 1, dist_data(i,lIndx)%nrows-1
          kfx = dist_data(i,lIndx)%x(xindex)
          kfx_next = dist_data(i,lIndx)%x(xindex+1)
          kfy = dist_data(i,lIndx)%y(xindex)
          kfy_next = dist_data(i,lIndx)%y(xindex+1)
          if(unirno.gt.kfy.and.unirno.le.kfy_next) then
            denom = kfy_next - kfy
            ! Check if denominator is zero
            if(denom.gt.0.D0) then
              inverse_transform_sampling = kfx + &
                (unirno - kfy)/denom * (kfx_next - kfx)
            else
              inverse_transform_sampling = kfx
            end if
          end if
        end do

 !     end do
      end function inverse_transform_sampling



!----------------------------------------------------------------------!
!                                                                      !
!  Subroutine: find_max_dia                                            !
!  Author: Sathish Sanjeevi                         Date:10-Aug-2020   !
!                                                                      !
!  Purpose : Find max diameter of a custom distribution of phase M     !
!                                                                      !
!----------------------------------------------------------------------!
      function find_max_dia(lIndx, lCV, lM)

! Index to denote IC or BC (1 or 2 respectively)
      integer, intent(in) :: lIndx
! (Initial or Boundary) Condition variable
      integer, intent(in) :: lCV
! Phase number M
      integer, intent(in) :: lM
      integer :: id
      double precision :: find_max_dia

      id = list(lM, lCV, lIndx)
      find_max_dia = min(dist_data(id,lIndx)%x(dist_data(id,lIndx)%nrows), UNDEFINED)

      end function find_max_dia



!......................................................................!
! Function: mean_volume                                                !
! Purpose:  Given a distribution, the mean volume of particles is not  !
!           equal to volume based on mean particle diameter. This      !
!           function computes mean volume of particles for a given     !
!           distribution                                               !
!......................................................................!

      function mean_volume(lIndx, lCV, lM)
      use constant, only: PI

      implicit none
! Index to denote IC or BC (1 or 2 respectively)
      integer, intent(in) :: lIndx
! (Initial or Boundary) Condition variable
      integer, intent(in) :: lCV
! Phase number M
      integer, intent(in) :: lM
      integer :: loop_length             ! Length of sampling loop
      double precision :: mean_volume    ! Variable for saving mean volume
      double precision :: d_cubed        ! Diameter to cubic power
      integer :: jj
      double precision :: mean, stdev, mu, sigma, min_dp, max_dp
      double precision, dimension(1) :: tmp_diameter
      character(len=32) :: CV_type

      d_cubed = 0.D0
      loop_length = 100000

      if(lIndx==1) then
        CV_type = trim(IC_PSD_TYPE(lCV, lM))
        if(CV_type == 'NORMAL' .or. CV_type == 'UNIFORM') then
          mean   = IC_PSD_MEAN_DP(lCV, lM)
          stdev  = IC_PSD_STDEV(lCV, lM)
          min_dp = IC_PSD_MIN_DP(lCV, lM)
          max_dp = IC_PSD_MAX_DP(lCV, lM)
        elseif(CV_type == 'LOG_NORMAL') then
          mean   = IC_PSD_MU(lCV, lM)
          stdev  = IC_PSD_SIGMA(lCV, lM)
          min_dp = IC_PSD_MIN_DP(lCV, lM)
          max_dp = IC_PSD_MAX_DP(lCV, lM)
        end if
      end if
      if(lIndx==2) then
        CV_type = trim(BC_PSD_TYPE(lCV, lM))
        if(CV_type == 'NORMAL' .or. CV_type == 'UNIFORM') then
          mean   = BC_PSD_MEAN_DP(lCV, lM)
          stdev  = BC_PSD_STDEV(lCV, lM)
          min_dp = BC_PSD_MIN_DP(lCV, lM)
          max_dp = BC_PSD_MAX_DP(lCV, lM)
        elseif(CV_type == 'LOG_NORMAL') then
          mean   = BC_PSD_MU(lCV, lM)
          stdev  = BC_PSD_SIGMA(lCV, lM)
          min_dp = BC_PSD_MIN_DP(lCV, lM)
          max_dp = BC_PSD_MAX_DP(lCV, lM)
        end if
      end if


! The mean volume is computed by picking diameters repeatedly with in
! the given distribution for "loop_length" times. Statiscally, this would
! return the mean particle volume provided "loop_length" is large enough

      if(CV_type == 'LOG_NORMAL') then
        do jj = 1, loop_length
          call log_nor_rno(tmp_diameter, mean, stdev)
          do while(tmp_diameter(1) < min_dp .or. tmp_diameter(1) > max_dp)
             call log_nor_rno(tmp_diameter, mean, stdev)
          enddo
          d_cubed = d_cubed+tmp_diameter(1)**3.0D0
        end do ! jj = loop_length
      elseif(CV_type == 'NORMAL') then
        do jj = 1, loop_length
          call nor_rno(tmp_diameter, mean, stdev)
          do while(tmp_diameter(1) < min_dp .or. tmp_diameter(1) > max_dp)
             call nor_rno(tmp_diameter, mean, stdev)
          enddo
          d_cubed = d_cubed+tmp_diameter(1)**3.0D0
        end do ! jj = loop_length
      elseif(CV_type == 'UNIFORM') then
        do jj = 1, loop_length
          call uni_rno(tmp_diameter)
          tmp_diameter(1) = (min_dp*(1.d0-tmp_diameter(1))+max_dp*tmp_diameter(1))
          d_cubed = d_cubed+tmp_diameter(1)**3.0D0
        end do ! jj = loop_length
      elseif(CV_type == 'CUSTOM') then
        do jj = 1, loop_length
          call uni_rno(tmp_diameter)
          tmp_diameter(1) = inverse_transform_sampling(tmp_diameter(1), lIndx, lCV, lM)
          d_cubed = d_cubed+tmp_diameter(1)**3.0D0
        end do ! jj = loop_length
!      else
!        d_cubed = d_p0(m)**3.0D0 * loop_length
      endif

      d_cubed = d_cubed/loop_length
      mean_volume = pi*d_cubed/6.0d0

      end function mean_volume



      function psd_radius(lIndx, lCV, lM)
      implicit none
! Variable to save the output radius
      double precision    :: psd_radius
! Index to denote IC or BC (1 or 2 respectively)
      integer, intent(in) :: lIndx
! (Initial or Boundary) Condition variable
      integer, intent(in) :: lCV
! Phase number M
      integer, intent(in) :: lM
      double precision :: mean, stdev, min_dp, max_dp
      double precision, dimension(1) :: tmp_diameter
      character(len=32) :: CV_type
      if(lIndx==1) then
        CV_type = trim(IC_PSD_TYPE(lCV, lM))
        if(CV_type == 'NORMAL' .or. CV_type == 'UNIFORM') then
          mean   = IC_PSD_MEAN_DP(lCV, lM)
          stdev  = IC_PSD_STDEV(lCV, lM)
          min_dp = IC_PSD_MIN_DP(lCV, lM)
          max_dp = IC_PSD_MAX_DP(lCV, lM)
        elseif(CV_type == 'LOG_NORMAL') then
          mean   = IC_PSD_MU(lCV, lM)
          stdev  = IC_PSD_SIGMA(lCV, lM)
          min_dp = IC_PSD_MIN_DP(lCV, lM)
          max_dp = IC_PSD_MAX_DP(lCV, lM)
        end if
      end if
      if(lIndx==2) then
        CV_type = trim(BC_PSD_TYPE(lCV, lM))
        if(CV_type == 'NORMAL' .or. CV_type == 'UNIFORM') then
          mean   = BC_PSD_MEAN_DP(lCV, lM)
          stdev  = BC_PSD_STDEV(lCV, lM)
          min_dp = BC_PSD_MIN_DP(lCV, lM)
          max_dp = BC_PSD_MAX_DP(lCV, lM)
        elseif(CV_type == 'LOG_NORMAL') then
          mean   = BC_PSD_MU(lCV, lM)
          stdev  = BC_PSD_SIGMA(lCV, lM)
          min_dp = BC_PSD_MIN_DP(lCV, lM)
          max_dp = BC_PSD_MAX_DP(lCV, lM)
        end if
      end if

      if(CV_type == 'LOG_NORMAL') then
         call log_nor_rno(tmp_diameter, mean, stdev)
         do while(tmp_diameter(1) < min_dp .or. tmp_diameter(1) > max_dp)
            call log_nor_rno(tmp_diameter, mean, stdev)
         enddo
         psd_radius = tmp_diameter(1)*half
      elseif(CV_type == 'NORMAL') then
         call nor_rno(tmp_diameter, mean, stdev)
         do while(tmp_diameter(1) < min_dp .or. tmp_diameter(1) > max_dp)
            call nor_rno(tmp_diameter, mean, stdev)
         enddo
         psd_radius = tmp_diameter(1)*half
      elseif(CV_type == 'UNIFORM') then
         call uni_rno(tmp_diameter)
         psd_radius = (min_dp*(1.d0-tmp_diameter(1))+max_dp*tmp_diameter(1))*half
      elseif(CV_type == 'CUSTOM') then
         call uni_rno(tmp_diameter)
         psd_radius = inverse_transform_sampling(tmp_diameter(1), lIndx, lCV, lM)*half
      endif
      end function psd_radius
end module des_psd
