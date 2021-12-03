!----------------------------------------------------------------------!
! Module: MPI_FUNS_DES                                                 !
! Author: Pradeep Gopalakrishnan                                       !
!                                                                      !
! Purpose: This module contains the subroutines and functions for MPI  !
! communications in DES simulations.                                   !
!----------------------------------------------------------------------!
module mpi_funs_pic

  private
  public :: pic_par_exchange

contains

!----------------------------------------------------------------------!
! Subroutine: DES_PAR_EXCHANGE                                         !
! Author: Pradeep Gopalakrishnan                                       !
!                                                                      !
! Purpose: This subroutine controls entire exchange of particles       !
!    between processors.                                               !
!                                                                      !
! Steps:                                                               !
! 2) Check if the send and recv buffer size is large enough            !
! 3) Pack and send active particles located in ghost cells to the      !
!    processors that own the ghost cells. The exchange occurs in       !
!    the following order to take care of particles crossing at corners !
!    (e.g., crossing directly into the northwest block):               !
!    a.) top-bottom interface                                          !
!    b.) north-south interface                                         !
!    c.) east-west interface                                           !
! 4) Bin the particles (if required)                                   !
! 5) Pack and send particles adjacent to neighboring processes. The    !
!    exchange occurs in the following order:                           !
!    a.) east-west interface                                           !
!    b.) north-south interface                                         !
!    c.) top-bottom interface                                          !
!                                                                      !
! Comments: The DO_NSEARCH flag should be set before calling           !
!   DES_PAR_EXCHANGE; When DO_NSEARCH is true, ghost particles are     !
!   updated and later used  to generate the PAIR lists.                !
!----------------------------------------------------------------------!
  subroutine pic_par_exchange()

     use desmpi, only: iexchflag
    use desmpi, only: dsendbuf, drecvbuf
    use desmpi, only: ispot

    use geometry, only: NO_K

    use desmpi, only: iParticlePacketSize

    use desgrid, only: dg_xstart, dg_ystart, dg_zstart
    use desgrid, only: dg_xend,   dg_yend,   dg_zend

    ! Number of particles on the process (max particle array size)
    use discretelement, only: MAX_PIP

    use desmpi, only: isendcnt
    use desmpi, only: ibufoffset

    ! Particle position
    use discretelement, only: particle_state
    use discretelement, only: DES_POS_NEW
    use discretelement, only: RESIDENCE_TIME

    use compar, only: numPEs


! Module procedures
!---------------------------------------------------------------------//
    use mpi_pic, only: mpi_pic_pack
    use mpi_pic, only: mpi_pic_unpack

    use mpi_comm_des, only: desmpi_sendrecv_init
    use mpi_comm_des, only: desmpi_sendrecv_wait

    use desgrid,        only: desgrid_pic
    use desmpi_wrapper, only: des_mpi_barrier
    use pic_flow_bcs,   only: delete_parcel

    use mpi_utility, only: global_max
    use discretelement, only: des_periodic_walls

    implicit none

! Local variables:
!---------------------------------------------------------------------//
! Loop counters.
    integer :: axis, lo_face, hi_face, lface, lbuf, lcurpar, lc
    integer :: lo_parcnt, hi_parcnt
    logical :: lo_send, hi_send
    double precision :: fmin, fmax

    integer :: lmaxpar
!......................................................................!

    if ((numpes==1 .and. .not.des_periodic_walls)) return

! Check that the send/recv buffer is sufficient every 5 calls
    call check_sendrecvbuf

! Initialize search position for unpacking parcels
    ispot = 1

    lmaxpar = 0

! Pack the high and low faces (T-B; N-S; E-W)
    do axis = merge(2,3,no_k),1,-1

       hi_face = axis*2-1
       lo_face = axis*2

       hi_parcnt = 0
       lo_parcnt = 0

       hi_send = iexchflag(hi_face)
       lo_send = iexchflag(lo_face)

       select case(axis)
       case(1)
          fmin = dg_xstart
          fmax = dg_xend
       case(2)
          fmin = dg_ystart
          fmax = dg_yend
       case(3)
          fmin = dg_zstart
          fmax = dg_zend
       end select

       ! Initialize send/recv buffer counts
       do lc=1, size(dsendbuf)
          dsendbuf(lc)%facebuf(1) = 0
          drecvbuf(lc)%facebuf(1) = 0
       enddo

       do lcurpar = 1, max_pip

          if(particle_state(lcurpar) == 1) then

             if(hi_send .and. des_pos_new(lcurpar, axis) > fmax) then

                lbuf = ibufoffset + hi_parcnt*iParticlePacketSize
                call mpi_pic_pack(hi_face, lcurpar, lbuf)
                hi_parcnt = hi_parcnt + 1

             else if(lo_send .and. des_pos_new(lcurpar, axis) <= fmin) then

                lbuf = ibufoffset + lo_parcnt*iParticlePacketSize
                call mpi_pic_pack(lo_face, lcurpar, lbuf)
                lo_parcnt = lo_parcnt + 1

             endif
          endif
       enddo

       lmaxpar = max(lmaxpar, hi_parcnt, lo_parcnt)

       if(hi_send) then
          dsendbuf(2)%facebuf(1) = hi_parcnt
          isendcnt(hi_face) = ibufoffset + hi_parcnt*iParticlePacketSize
          call desmpi_sendrecv_init(hi_face)
       endif

       if(lo_send) then
          dsendbuf(1)%facebuf(1) = lo_parcnt
          isendcnt(lo_face) = ibufoffset + lo_parcnt*iParticlePacketSize
          call desmpi_sendrecv_init(lo_face)
       endif

       do lface = axis*2-1,axis*2
          if(iexchflag(lface)) then
             call desmpi_sendrecv_wait(lface)
             call mpi_pic_unpack(lface)
          endif
       end do

    end do

! Remove parcels that are not on this process. This means that a parcel
! crossed more than one MPI boundary in one step.
    if(no_k) then
       do lcurpar = 1, max_pip
          if(particle_state(lcurpar) == 1) then
             if(des_pos_new(lcurpar,1) < dg_xstart .or. &
                des_pos_new(lcurpar,1) > dg_xend   .or. &
                des_pos_new(lcurpar,2) < dg_ystart .or. &
                des_pos_new(lcurpar,2) > dg_yend) then
                call delete_parcel(lcurpar)
                cycle
             endif
          endif
       enddo
    else
       do lcurpar = 1, max_pip
          if(particle_state(lcurpar) == 1) then
             if(des_pos_new(lcurpar,1) < dg_xstart .or. &
                des_pos_new(lcurpar,1) > dg_xend   .or. &
                des_pos_new(lcurpar,2) < dg_ystart .or. &
                des_pos_new(lcurpar,2) > dg_yend   .or. &
                des_pos_new(lcurpar,3) < dg_zstart .or. &
                des_pos_new(lcurpar,3) > dg_zend) then
                call delete_parcel(lcurpar)
                cycle
             endif
          endif
       enddo
    endif


    ! call grow_sendrecvbuf(lmaxpar*iParticlePacketSize, 1.2d0)

    call des_mpi_barrier

!   call des_dbgmpi(5)

  end subroutine pic_par_exchange

!----------------------------------------------------------------------!
! subroutine: check_sendrecvbuf                                        !
!                                                                      !
! Purpose: This routine mimics the mpi pack logic for parcels to       !
!    approximate the needed MPI buffer size.                           !
!----------------------------------------------------------------------!
  subroutine check_sendrecvbuf

    use desmpi, only: iexchflag

    use geometry, only: NO_K

    use desmpi, only: iParticlePacketSize

    use desgrid, only: dg_xstart, dg_ystart, dg_zstart
    use desgrid, only: dg_xend,   dg_yend,   dg_zend

    ! Number of particles on the process (max particle array size)
    use discretelement, only: MAX_PIP

    use desmpi, only: ibufoffset

    ! Particle position
    use discretelement, only: particle_state
    use discretelement, only: des_pos_new
    use discretelement, only: RESIDENCE_TIME

    use compar, only: myPE, PE_IO
    use mpi_utility, only: global_all_max

    implicit none

! Local variables:
!---------------------------------------------------------------------//
! Loop counters.
    integer :: axis, lcurpar

    integer :: hi_face,   lo_face
    integer :: hi_parcnt, lo_parcnt
    integer :: hi_buf,    lo_buf
    logical :: hi_send,   lo_send

    double precision :: fmin, fmax

    integer :: lmaxbuf
    logical, parameter :: ldebug = .false.
!......................................................................!

    lmaxbuf = 0

! Pack the high and low faces (T-B; N-S; E-W)
    do axis = merge(2,3,no_k),1,-1

       hi_face = axis*2-1
       lo_face = axis*2

       hi_parcnt = 0
       lo_parcnt = 0

       hi_send = iexchflag(hi_face)
       lo_send = iexchflag(lo_face)

       select case(axis)
       case(1)
          fmin = dg_xstart
          fmax = dg_xend
       case(2)
          fmin = dg_ystart
          fmax = dg_yend
       case(3)
          fmin = dg_zstart
          fmax = dg_zend
       end select

       do lcurpar = 1, max_pip

          if(particle_state(lcurpar) == 1) then

             if(hi_send .and. des_pos_new(lcurpar, axis) > fmax) then
                hi_parcnt = hi_parcnt + 1

             else if(lo_send .and. des_pos_new(lcurpar, axis) <= fmin) then
                hi_parcnt = hi_parcnt + 1

             endif
          endif
       enddo

       hi_buf = ibufoffset + hi_parcnt
       lo_buf = ibufoffset + lo_parcnt

       lmaxbuf = max(lmaxbuf, hi_buf, lo_buf)

    end do

    call global_all_max(lmaxbuf)
    if(myPE == PE_IO .and. ldebug) write(*,*) "Max Particle send/recv", lmaxbuf

    call grow_sendrecvbuf(lmaxbuf*iParticlePacketSize, 2.0d0)

end subroutine check_sendrecvbuf


!----------------------------------------------------------------------!
! Subroutine: grow_sendrecvbuf                                         !
!                                                                      !
!----------------------------------------------------------------------!
subroutine grow_sendrecvbuf(minbuf, scale)

  use desmpi, only: imaxbuf
  use desmpi, only: dsendbuf, drecvbuf

  use error_manager

  implicit none

  integer,          intent(in   ) :: minbuf
  double precision, intent(in   ) :: scale

! Local variables:
!---------------------------------------------------------------------//
! Loop counters
  integer :: lface
! Previous Buffer
  integer :: pbuf

  double precision, parameter :: onembo8 = 131072.0
!......................................................................!

  if (imaxbuf < scale*minbuf) then

     pbuf = imaxbuf
     imaxbuf = scale*minbuf

     do lface = 1,2
        if(allocated(dsendbuf(lface)%facebuf)) &
             deallocate(dsendbuf(lface)%facebuf)

        allocate(dsendbuf(lface)%facebuf(imaxbuf))

        if(allocated(drecvbuf(lface)%facebuf)) &
             deallocate(drecvbuf(lface)%facebuf)

        allocate(drecvbuf(lface)%facebuf(imaxbuf))
     end do

     ! write(err_msg, 1000) imaxbuf/onembo8, &
     !     100.0d0+100.0d0*dble(imaxbuf-pbuf)/dble(pbuf)
     ! CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

  endif

! 1000 FORMAT(/'Resizing PIC MPI buffers: ',F7.1,' MB  (+',F5.1, '%)')

end subroutine grow_sendrecvbuf

end module mpi_funs_pic
