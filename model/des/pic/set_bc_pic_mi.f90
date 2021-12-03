#include "error.inc"

module bc_pic_mi

  public :: set_bc_pic_mi

  logical, parameter :: ldebug = .false.

contains

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: SET_BC_PIC_MI                                           !
!  Author: R. Garg                                    Date: 11-Jun-14  !
!                                                                      !
!  Purpose:                                                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
SUBROUTINE SET_BC_PIC_MI

!-----------------------------------------------
! Modules
!-----------------------------------------------

  use discretelement, only: particles

  use mfix_pic, only: pic_bcmi, pic_bcmi_map
  use mfix_pic, only: pic_mi

  use bc, only: bc_area

  use param, only: dim_m
  use param1, only: undefined_i

  use cutcell, only: use_stl

  use error_manager

  IMPLICIT NONE

!-----------------------------------------------
! Local variables
!-----------------------------------------------
  INTEGER :: bcv, bcv_i
  integer :: phase

  double precision :: procArea
  real :: pflow, frac

!-----------------------------------------------

  ! The variable PARTICLES should already be set by this point if using
  ! gener_part_config option.  They are.
  if(particles == undefined_i) particles = 0

  ! If the system is started without any particles and an inlet is not
  ! specified, the run is likely aborted.
  ! Inlet/outlet for MPPIC are based off the regular mfix declarations,
  ! and so PIC_BCMI could still be zero.
  if(particles == 0 .and. pic_bcmi == 0) then
     write(err_msg, 1202)
     call log_warning()
  endif

1202 FORMAT('WARNING 1202: The system is initiated with no particles',&
       ' and no',/'solids inlet was detected.')

  if(pic_bcmi > 0) then

     ! Loop over BCs that are flagged for PIC mass inflow.
     do bcv_i = 1, pic_bcmi

        ! Get the user defined BC ID.
        bcv = pic_bcmi_map(bcv_i)

        pic_mi(bcv_i)%pflow(:) =   0  ! parcels/second
        pic_mi(bcv_i)%remdr(:) = 0.0  ! underflow carried over

        pic_mi(bcv_i)%offset(:) =   0 ! offset for seeding
        pic_mi(bcv_i)%length(:) = 0.0 ! length for placement

        if(bc_area(bcv) > epsilon(0.0d0)) then

           procArea = getBCProcArea(bcv)
           do phase=1,dim_m

              pflow = getBCPFlow(bcv,phase)

              if(pflow > 0.0d0) then
                 frac = procArea/bc_area(bcv)
                 pic_mi(bcv_i)%pflow(phase) = pflow*frac
              endif
           enddo
           if(procArea > 0.0d0) then
              call set_bc_pic_offset(bcv_i, bcv)
              if(use_stl) call block_dg_cells(bcv_i, bcv)
           endif
        endif
     enddo
  endif

  return
end subroutine set_bc_pic_mi


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: getBCPFlow                                              !
!                                                                      !
!  Purpose: Return the area of a BC local to the processor             !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
function getBCPFlow(lbcv, lphase) result(pflow)

  use physprop, only: d_p0

  use bc, only: bc_pic_mi_const_statwt
  use bc, only: bc_ep_s, bc_massflow_s, bc_volflow_s
  use bc, only: bc_u_s, bc_v_s, bc_w_s
  use bc, only: bc_plane, bc_area
  use bc, only: bc_ro_s

  USE constant, only: pi
  use param1, only: zero, one, undefined

  implicit none

  integer, intent(in   ) :: lbcv, lphase

  real :: pflow
  double precision :: mflow, vel, ro_sb

  ! Get the density for the
  ro_sb = bc_ro_s(lbcv, lphase)

! Mass flow of particle at BC
  if(bc_ep_s(lbcv,lphase) >  zero .and. &
     bc_ep_s(lbcv,lphase) <=  one) then

     if(bc_massflow_s(lbcv,lphase) /= undefined) then

        mflow = bc_massflow_s(lbcv,lphase)

     else

        if(bc_volflow_s(lbcv,lphase) /= undefined) then
           mflow = bc_volflow_s(lbcv,lphase)*bc_ro_s(lbcv,lphase)

        else
           select case(bc_plane(lbcv))
           case('E','W'); vel = abs(bc_u_s(lbcv,lphase))
           case('N','S'); vel = abs(bc_v_s(lbcv,lphase))
           case('T','B'); vel = abs(bc_w_s(lbcv,lphase))
           end select

           mflow = vel*bc_area(lbcv)*bc_ep_s(lbcv,lphase)*bc_ro_s(lbcv,lphase)
        endif
     endif
  else
     mflow = 0.0d0

  endif

  ! Flow of particles at BC
  if(mflow>zero) then
     pflow = mflow/((pi/6.0d0)*(d_p0(lphase)**3)*ro_sb*&
             bc_pic_mi_const_statwt(lbcv, lphase))
  else
     pflow=zero
  endif

end function getBCPFlow


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: getBCProcArea                                           !
!                                                                      !
!  Purpose: Return the area of a BC local to the processor             !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
function getBCProcArea(lbcv) result(area)

  use compar, only: istart, jstart, kstart
  use compar, only: iend,   jend,   kend

  use bc, only: bc_plane
  use bc, only: bc_i_e, bc_j_n, bc_k_t
  use bc, only: bc_i_w, bc_j_s, bc_k_b

  use geometry, only: ayz, axz, axy

  use functions, only: funijk, fluid_at

  implicit none

  integer, intent(in   ) :: lbcv

  integer :: i1, i2, j1, j2, k1, k2, i, j, k, ijk
  double precision :: area

  i1 = max(bc_i_w(lbcv), istart)
  j1 = max(bc_j_s(lbcv), jstart)
  k1 = max(bc_k_b(lbcv), kstart)

  i2 = min(bc_i_e(lbcv), iend)
  j2 = min(bc_j_n(lbcv), jend)
  k2 = min(bc_k_t(lbcv), kend)

  area = 0.0d0
  do k=k1,k2
     do i=i1,i2
        do j=j1,j2

           select case(bc_plane(lbcv))

           case('E')
              ijk = funijk(i+1,j,k)
              if(fluid_at(ijk)) then
                 area = area + ayz(ijk)
              endif

           case('W')
              ijk = funijk(i-1,j,k)
              if(fluid_at(ijk)) then
                 area = area + ayz(ijk)
              endif

           case('N')
              ijk = funijk(i,j+1,k)
              if(fluid_at(ijk)) then
                 area = area + axz(ijk)
              endif

           case('S')
              ijk = funijk(i,j-1,k)
              if(fluid_at(ijk)) then
                 area = area + axz(ijk)
              endif

           case('T')
              ijk = funijk(i,j,k+1)
              if(fluid_at(ijk)) then
                 area = area + axy(ijk)
              endif

           case('B')
              ijk = funijk(i,j,k-1)
              if(fluid_at(ijk)) then
                 area = area + axy(ijk)
              endif
           end select
        enddo
     enddo
  enddo

end function getBCProcArea


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: getBCProcArea                                           !
!                                                                      !
!  Purpose: Return the area of a BC local to the processor             !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
subroutine block_dg_cells(lbcv, lbc_map)

! Modules
!---------------------------------------------------------------------//
  USE param1, only: half
  use physprop, only: D_p0
  use cutcell, only: cut_cell_at

  use discretelement, only: des_mmax
  use discretelement, only: xe, yn, zt

  use compar,   only: istart,   jstart,   kstart ! proc own start
  use compar,   only: iend,     jend,     kend   ! proc own end
  use geometry, only: ayz,      axz,      axy    ! cross section area
  use geometry, only: oDX,      oDY,      oDZ    ! one over dx

  use bc, only: bc_ro_s, bc_pic_mi_const_statwt
  use bc, only: bc_plane
  use bc, only: bc_i_e, bc_j_n, bc_k_t
  use bc, only: bc_i_w, bc_j_s, bc_k_b

  use functions, only: set_normal
  use functions, only: funijk, fluid_at

  use mfix_pic, only: pic_mi

  use mpi_utility, only: global_all_sum

  use desgrid, only: dg_funijk
  use desgrid, only: dg_istart,    dg_jstart,    dg_kstart
  use desgrid, only: dg_iend,      dg_jend,      dg_kend
  use desgrid, only: dg_istart1,   dg_jstart1,   dg_kstart1
  use desgrid, only: dg_xstart,    dg_ystart,    dg_zstart
  use desgrid, only: dg_dxinv,     dg_dyinv,     dg_dzinv
  use desgrid, only: iofpos,       jofpos,       kofpos

  use stl, only: facets_at_dg
  use stl, only: stl_start, stl_end, default_stl

  use funits, only: newUnit

  implicit none


  integer, intent(in) :: lbcv, lbc_map


! Local Variables
!---------------------------------------------------------------------//
  integer :: phase, i,j,k,ijk, lc1
  integer :: i_lo, i_hi, j_hi, j_lo, k_hi, k_lo

  integer :: ii, jj, kk

  integer :: lo(3), hi(3)
  integer :: dg_lo(3), dg_hi(3)

  double precision :: pflow, area_inv

  double precision :: lrad, lstat_wt
  double precision :: eff_rad
  double precision :: bc_lo(3), bc_hi(3)

  double precision :: covered
  double precision, allocatable :: coverage(:,:,:)

  double precision :: fbox_lo(3), fbox_hi(3)
  double precision :: dbox_lo(3), dbox_hi(3)
  double precision :: dg_dx, dg_dy, dg_dz

  double precision, parameter :: cover_limit = 0.30d0
  double precision, parameter :: cover_limit_max = 0.99d0

  logical :: all_default

  integer :: lunit
  ! character(len=2) :: cPE

  lunit = 6!newUnit()
  ! write(cPE,"(i2.2)") myPE

  ! open(unit=lunit, file='picbc_'//cPE//'.txt', &
  !      status='unknown', position='append')

  ! For complex boundaries defined by STLs, exclude any mass inflow cells
  ! that intersect the boundary and all cells opposite the normal. The MI
  ! cell sizes are increased by 10% to provide a small buffer.

  ! Find the largest effective radius for the mass inflow
  eff_rad = 0.0
  do phase=1,des_mmax
     pflow = pic_mi(lbcv)%pflow(phase)
     if(pflow > 0.0d0) then
        lrad  = d_p0(phase)*half
        lstat_wt = bc_pic_mi_const_statwt(lbc_map,phase)
        eff_rad = max(lrad*(lstat_wt)**(1.0/3.0), eff_rad)
     endif
  enddo

  dg_dx = 1.0d0/dg_dxinv
  dg_dy = 1.0d0/dg_dyinv
  dg_dz = 1.0d0/dg_dzinv

  ! Set local bounds for fluid grid over BC region
  lo(1) = max(bc_i_w(lbc_map), istart)
  lo(2) = max(bc_j_s(lbc_map), jstart)
  lo(3) = max(bc_k_b(lbc_map), kstart)

  hi(1) = min(bc_i_e(lbc_map), iend)
  hi(2) = min(bc_j_n(lbc_map), jend)
  hi(3) = min(bc_k_t(lbc_map), kend)

  if(ldebug) then
     write(lUnit,*) ' '
     write(lUnit,*) ' Setting up BC: ',lbc_map
     write(lUnit,*) '         Plane: ',bc_plane(lbc_map)
     write(lUnit,*) ' '
  endif

  ! Shift flow cells into the domain
  select case(bc_plane(lbc_map))
  case('E'); lo(1) = lo(1)+1; hi(1) = lo(1)
  case('W'); lo(1) = lo(1)-1; hi(1) = lo(1)
  case('N'); lo(2) = lo(2)+1; hi(2) = lo(2)
  case('S'); lo(2) = lo(2)-1; hi(2) = lo(2)
  case('T'); lo(3) = lo(3)+1; hi(3) = lo(3)
  case('B'); lo(3) = lo(3)-1; hi(3) = lo(3)
  end select

  if(ldebug) then
     write(lUnit,*) 'Fluid grid bounds:'
     write(lUnit,*) 'lo/hi(1)', lo(1), hi(1)
     write(lUnit,*) 'lo/hi(2)', lo(2), hi(2)
     write(lUnit,*) 'lo/hi(3)', lo(3), hi(3)
     write(lUnit,*) '        '
     write(lUnit,*) '        '
     write(lUnit,*) '        '

     write(lUnit,*) 'Fluid grid extents'
     write(lUnit,*) ' x lo/hi', xe(lo(1)-1), xe(hi(1))
     write(lUnit,*) ' y lo/hi', yn(lo(2)-1), yn(hi(2))
     write(lUnit,*) ' z lo/hi', zt(lo(3)-1), zt(hi(3))
     write(lUnit,*) '        '
     write(lUnit,*) '        '
     write(lUnit,*) '        '
  endif

  i_lo = min(dg_iend,max(dg_istart, iofpos(xe(lo(1)-1))))
  j_lo = min(dg_jend,max(dg_jstart, jofpos(yn(lo(2)-1))))
  k_lo = min(dg_kend,max(dg_kstart, kofpos(zt(lo(3)-1))))

  i_hi = min(dg_iend,max(dg_istart, iofpos(xe(hi(1))))+1)
  j_hi = min(dg_jend,max(dg_jstart, jofpos(yn(hi(2))))+1)
  k_hi = min(dg_kend,max(dg_kstart, kofpos(zt(hi(3))))+1)

  if(ldebug) then
     write(lUnit,*) 'DES grid bounds'
     write(lUnit,*) ' i_lo/hi', i_lo, i_hi
     write(lUnit,*) ' j_lo/hi', j_lo, j_hi
     write(lUnit,*) ' k_lo/hi', k_lo, k_hi
     write(lUnit,*) '        '
     write(lUnit,*) '        '
     write(lUnit,*) '        '

     write(lUnit,*) 'DES grid extents'
     write(lUnit,*) ' x lo/hi', dg_xstart + (i_lo - dg_istart1)*dg_dx, &
          dg_xstart + (i_hi - dg_istart1)*dg_dx
     write(lUnit,*) ' y lo/hi', dg_ystart + (j_lo - dg_jstart1)*dg_dy, &
          dg_ystart + (j_hi - dg_jstart1)*dg_dy
     write(lUnit,*) ' z lo/hi', dg_zstart + (k_lo - dg_kstart1)*dg_dz, &
          dg_zstart + (k_hi - dg_kstart1)*dg_dz
     write(lUnit,*) '        '
     write(lUnit,*) '        '
     write(lUnit,*) '        '
  endif

  allocate(pic_mi(lbcv)%blocked(i_lo:i_hi,j_lo:j_hi,k_lo:k_hi))

  allocate(coverage(i_lo:i_hi,j_lo:j_hi,k_lo:k_hi))

  coverage = 0.0d0
  pic_mi(lbcv)%blocked = .false.

  select case(bc_plane(lbc_map))

  case('E','W')

     i = merge(lo(1), hi(1), bc_plane(lbc_map) == 'E')
     area_inv = dg_dyinv*dg_dzinv

     do k= lo(3), hi(3)

        fbox_lo(3) = zt(k-1)
        fbox_hi(3) = zt(k  )

        dg_lo(3) = min(dg_kend,max(dg_kstart, kofpos(zt(k-1))))
        dg_hi(3) = min(dg_kend,max(dg_kstart, kofpos(zt(k  ))))

        do j = lo(2), hi(2)

           fbox_lo(2) = yn(j-1)
           fbox_hi(2) = yn(j  )

           dg_lo(2) = min(dg_jend,max(dg_jstart, jofpos(yn(j-1))))
           dg_hi(2) = min(dg_jend,max(dg_jstart, jofpos(yn(j  ))))

           ijk = funijk(i,j,k)

           do kk=dg_lo(3), dg_hi(3)

              dbox_lo(3) = dg_zstart + (kk     - dg_kstart1)*dg_dz
              dbox_hi(3) = dg_zstart + (kk + 1 - dg_kstart1)*dg_dz

              do jj=dg_lo(2), dg_hi(2)

                 dbox_lo(2) = dg_ystart + (jj     - dg_jstart1)*dg_dy
                 dbox_hi(2) = dg_ystart + (jj + 1 - dg_jstart1)*dg_dy

                 covered = area_inv * &
                      max(0.0d0, min(dbox_hi(3),fbox_hi(3)) -   &
                                 max(dbox_lo(3),fbox_lo(3))) *  &
                      max(0.0d0, min(dbox_hi(2),fbox_hi(2)) -   &
                                 max(dbox_lo(2),fbox_lo(2)))


                 ! Disable cells that are covered by non-fluid cells
                 if(.not.fluid_at(ijk) .and. covered > 0.1d0) then
                    pic_mi(lbcv)%blocked(:,jj,kk) = .true.
                 else
                    if(cut_cell_at(ijk)) then
                       coverage(i,jj,kk) = coverage(i,jj,kk) + covered*&
                            ayz(ijk)*ody(j)*odz(k)
                    else
                       coverage(i,jj,kk) = coverage(i,jj,kk) + covered
                    endif
                 endif

              enddo
           enddo
        enddo
     enddo

     do k= k_lo, k_hi
        do j= j_lo, j_hi

           ijk = dg_funijk(i,j,k)

           if(coverage(i,j,k) < cover_limit) then

              if(facets_at_dg(ijk)%count == 0) then
                 pic_mi(lbcv)%blocked(i,j,k) = .true.
              else
                 all_default = .true.
                 do lc1=1,facets_at_dg(ijk)%count
                    if(facets_at_dg(ijk)%id(lc1) < stl_start(default_stl) .or. &
                         facets_at_dg(ijk)%id(lc1) > stl_end(default_stl)) &
                         all_default = .false.
                 enddo
                 if(all_default) pic_mi(lbcv)%blocked(:,j,k) = .true.
              endif
           endif
        enddo
     enddo

     ! if any blocked "i" flag is set, set them all.
     do k= k_lo, k_hi
        do j= j_lo, j_hi
           if(any(pic_mi(lbcv)%blocked(:,j,k))) &
                pic_mi(lbcv)%blocked(:,j,k) = .true.
        enddo
     enddo

     if(ldebug) write(lUnit,*) 'coverage', sum(coverage)

     bc_lo = pic_mi(lbcv)%offset(:)
     bc_hi = bc_lo + pic_mi(lbcv)%length(:)

     do k= k_lo, k_hi
        if(ldebug) write(lUnit,*) "test (k+):", k
        if( .not.all(pic_mi(lbcv)%blocked(i,:,k)) ) then
           bc_lo(3) = max(bc_lo(3), dg_zstart+(k  -dg_kstart1)*dg_dz)
           exit
        endif
     enddo

     do k= k_hi, k_lo, -1
        if(ldebug) write(lUnit,*) "test (k-):", k
        if( .not.all(pic_mi(lbcv)%blocked(i,:,k)) ) then
           bc_hi(3) = min(bc_hi(3), dg_zstart+(k+1-dg_kstart1)*dg_dz)
           exit
        endif
     enddo

     do j= j_lo, j_hi
        if(ldebug) write(lUnit,*) "test (j+):", j
        if( .not.all(pic_mi(lbcv)%blocked(i,j,:)) ) then
           bc_lo(2) = max(bc_lo(2), dg_ystart+(j  -dg_jstart1)*dg_dy)
           exit
        endif
     enddo

     do j= j_hi, j_lo, -1
        if(ldebug) write(lUnit,*) "test (j-):", j
        if( .not.all(pic_mi(lbcv)%blocked(i,j,:)) ) then
           bc_hi(2) = min(bc_hi(2), dg_ystart+(j+1-dg_jstart1)*dg_dy)
           exit
        endif
     enddo


     pic_mi(lbcv)%offset(:) = bc_lo
     pic_mi(lbcv)%length(:) = bc_hi - bc_lo


     if(ldebug) then
        write(lUnit,*) '        '
        write(lUnit,*) '        '
        write(lUnit,*) 'BC seed extents'
        write(lUnit,*) 'Sx lo/hi', bc_lo(1), bc_hi(1)
        write(lUnit,*) 'Sy lo/hi', bc_lo(2), bc_hi(2)
        write(lUnit,*) 'Sz lo/hi', bc_lo(3), bc_hi(3)
        write(lUnit,*) '        '
        write(lUnit,*) '        '
        write(lUnit,*) ' Covered'

        do k= k_hi, k_lo, -1
           write(lUnit,"(2x,'k =',i3)",advance='no')k
           do j= j_lo, j_hi
              if(coverage(i,j,k) < 0.001) then
                 write(lUnit,"(' .')",advance='no')
              else if(coverage(i,j,k) < cover_limit_max) then
                 write(lUnit,"(' #')",advance='no')
              else
                 write(lUnit,"(' *')",advance='no')
              endif
           enddo
           write(lUnit,*)' '
        enddo


        write(lUnit,*) '        '
        write(lUnit,*) '        '
        write(lUnit,*) ' Blocked Flag '

        do k= k_hi, k_lo, -1
           write(lUnit,"(2x,'k =',i3)",advance='no')k
           do j= j_lo, j_hi
              if(pic_mi(lbcv)%blocked(i,j,k)) then
                 write(lUnit,"(' .')",advance='no')
              else
                 write(lUnit,"(' *')",advance='no')
              endif
           enddo
           write(lUnit,*)' '
        enddo

        write(lUnit,*) '        '
        write(lUnit,*) '        '
        write(lUnit,*) ' facets_at_dg'

        do k= k_hi, k_lo, -1
           write(lUnit,"(2x,'k =',i3)",advance='no')k
           do j= j_lo, j_hi

              ijk = dg_funijk(i,j,k)

              if(facets_at_dg(ijk)%count == 0) then
                 write(lUnit,"(' .')",advance='no')
              else if(facets_at_dg(ijk)%count == -1) then
                 write(lUnit,"(' #')",advance='no')
              else
                 write(lUnit,"(' *')",advance='no')
              endif
           enddo
           write(lUnit,*)' '
        enddo
     endif



  case('N','S')

     j = merge(lo(2), hi(2), bc_plane(lbc_map) == 'N')
     area_inv = dg_dxinv*dg_dzinv

     pic_mi(lbcv)%blocked = .false.
     coverage = 0.0d0

     do k= lo(3), hi(3)

        fbox_lo(3) = zt(k-1)
        fbox_hi(3) = zt(k  )

        dg_lo(3) = min(dg_kend,max(dg_kstart, kofpos(zt(k-1))))
        dg_hi(3) = min(dg_kend,max(dg_kstart, kofpos(zt(k  ))))

        do i = lo(1), hi(1)

           fbox_lo(1) = xe(i-1)
           fbox_hi(1) = xe(i  )

           dg_lo(1) = min(dg_iend,max(dg_istart, iofpos(xe(i-1))))
           dg_hi(1) = min(dg_iend,max(dg_istart, iofpos(xe(i  ))))

           ijk = funijk(i,j,k)

           do kk=dg_lo(3), dg_hi(3)

              dbox_lo(3) = dg_zstart + (kk     - dg_kstart1)*dg_dz
              dbox_hi(3) = dg_zstart + (kk + 1 - dg_kstart1)*dg_dz

              do ii=dg_lo(1), dg_hi(1)

                 dbox_lo(1) = dg_xstart + (ii     - dg_istart1)*dg_dx
                 dbox_hi(1) = dg_xstart + (ii + 1 - dg_istart1)*dg_dx

                 covered = area_inv * &
                      max(0.0d0, min(dbox_hi(3),fbox_hi(3)) -   &
                                 max(dbox_lo(3),fbox_lo(3))) *  &
                      max(0.0d0, min(dbox_hi(1),fbox_hi(1)) -   &
                                 max(dbox_lo(1),fbox_lo(1)))

                 ! Disable cells that are covered by non-fluid cells
                 if(.not.fluid_at(ijk) .and. covered > 0.1d0) then
                    pic_mi(lbcv)%blocked(ii,:,kk) = .true.
                 else
                    if(cut_cell_at(ijk)) then
                       coverage(ii,j,kk) = coverage(ii,j,kk) + covered*&
                            axz(ijk)*odx(i)*odz(k)
                    else
                       coverage(ii,j,kk) = coverage(ii,j,kk) + covered
                    endif
                 endif

              enddo
           enddo
        enddo
     enddo

     do k= k_lo, k_hi
        do i= i_lo, i_hi

           ijk = dg_funijk(i,j,k)

           if(coverage(i,j,k) < cover_limit) then

              if(facets_at_dg(ijk)%count == 0) then
                 pic_mi(lbcv)%blocked(i,:,k) = .true.
              else
                 all_default = .true.
                 do lc1=1,facets_at_dg(ijk)%count
                    if(facets_at_dg(ijk)%id(lc1) < stl_start(default_stl) .or. &
                         facets_at_dg(ijk)%id(lc1) > stl_end(default_stl)) &
                         all_default = .false.
                 enddo
                 if(all_default) pic_mi(lbcv)%blocked(i,:,k) = .true.
              endif
           endif
        enddo
     enddo

     do k= k_lo, k_hi
        do i= i_lo, i_hi
           if(any(pic_mi(lbcv)%blocked(i,:,k))) &
                pic_mi(lbcv)%blocked(i,:,k) = .true.
        enddo
     enddo

     if(ldebug) write(lUnit,*) 'coverage', sum(coverage)

     bc_lo = pic_mi(lbcv)%offset(:)
     bc_hi = bc_lo + pic_mi(lbcv)%length(:)

     do k= k_lo, k_hi
        if(ldebug) write(lUnit,*) "test (k+):", k
        if( .not.all(pic_mi(lbcv)%blocked(:,j,k)) ) then
           bc_lo(3) = max(bc_lo(3), dg_zstart+(k  -dg_kstart1)*dg_dz)
           exit
        endif
     enddo

     do k= k_hi, k_lo, -1
        if(ldebug) write(lUnit,*) "test (k-):", k
        if( .not.all(pic_mi(lbcv)%blocked(:,j,k)) ) then
           bc_hi(3) = min(bc_hi(3), dg_zstart+(k+1-dg_kstart1)*dg_dz)
           exit
        endif
     enddo

     do i= i_lo, i_hi
        if(ldebug) write(lUnit,*) "test (i+):", i
        if( .not.all(pic_mi(lbcv)%blocked(i,j,:)) ) then
           bc_lo(1) = max(bc_lo(1), dg_xstart+(i  -dg_istart1)*dg_dx)
           exit
        endif
     enddo

     do i= i_hi, i_lo, -1
        if(ldebug) write(lUnit,*) "test (i-):", i
        if( .not.all(pic_mi(lbcv)%blocked(i,j,:)) ) then
           bc_hi(1) = min(bc_hi(1), dg_xstart+(i+1-dg_istart1)*dg_dx)
           exit
        endif
     enddo


     pic_mi(lbcv)%offset = bc_lo
     pic_mi(lbcv)%length = bc_hi - bc_lo



     if(ldebug) then
        write(lUnit,*) '        '
        write(lUnit,*) '        '
        write(lUnit,*) 'BC seed extents'
        write(lUnit,*) 'Sx lo/hi', bc_lo(1), bc_hi(1)
        write(lUnit,*) 'Sy lo/hi', bc_lo(2), bc_hi(2)
        write(lUnit,*) 'Sz lo/hi', bc_lo(3), bc_hi(3)
        write(lUnit,*) '        '
        write(lUnit,*) '        '
        write(lUnit,*) ' Covered'

        do k= k_hi, k_lo, -1
           write(lUnit,"(2x,'k =',i3)",advance='no')k
           do i= i_lo, i_hi
              if(coverage(i,j,k) < 0.001) then
                 write(lUnit,"(' .')",advance='no')
              else if(coverage(i,j,k) < cover_limit_max) then
                 write(lUnit,"(' #')",advance='no')
              else
                 write(lUnit,"(' *')",advance='no')
              endif
           enddo
           write(lUnit,*)' '
        enddo

        write(lUnit,*) '        '
        write(lUnit,*) '        '
        write(lUnit,*) ' Blocked Flag '

        do k= k_hi, k_lo, -1
           write(lUnit,"(2x,'k =',i3)",advance='no')k
           do i= i_lo, i_hi
              if(pic_mi(lbcv)%blocked(i,j,k)) then
                 write(lUnit,"(' .')",advance='no')
              else
                 write(lUnit,"(' *')",advance='no')
              endif
           enddo
           write(lUnit,*)' '
        enddo

        write(lUnit,*) '        '
        write(lUnit,*) '        '
        write(lUnit,*) ' facets_at_dg'

        do k= k_hi, k_lo, -1
           write(lUnit,"(2x,'k =',i3)",advance='no')k
           do i= i_lo, i_hi

              ijk = dg_funijk(i,j,k)

              if(facets_at_dg(ijk)%count == 0) then
                 write(lUnit,"(' .')",advance='no')
              else if(facets_at_dg(ijk)%count == -1) then
                 write(lUnit,"(' #')",advance='no')
              else
                 write(lUnit,"(' *')",advance='no')
              endif
           enddo
           write(lUnit,*)' '
        enddo
     endif


  case('T','B')

     k = merge(lo(3), hi(3), bc_plane(lbc_map) == 'T')
     area_inv = dg_dxinv*dg_dyinv

     pic_mi(lbcv)%blocked = .false.
     coverage = 0.0d0

     do i = lo(1), hi(1)

        fbox_lo(1) = xe(i-1)
        fbox_hi(1) = xe(i  )

        dg_lo(1) = min(dg_iend,max(dg_istart, iofpos(xe(i-1))))
        dg_hi(1) = min(dg_iend,max(dg_istart, iofpos(xe(i  ))))

        do j = lo(2), hi(2)

           fbox_lo(2) = yn(j-1)
           fbox_hi(2) = yn(j  )

           dg_lo(2) = min(dg_jend,max(dg_jstart, jofpos(yn(j-1))))
           dg_hi(2) = min(dg_jend,max(dg_jstart, jofpos(yn(j  ))))

           ijk = funijk(i,j,k)

           do ii=dg_lo(1), dg_hi(1)

              dbox_lo(1) = dg_xstart + (ii     - dg_istart1)*dg_dx
              dbox_hi(1) = dg_xstart + (ii + 1 - dg_istart1)*dg_dx

              do jj=dg_lo(2), dg_hi(2)

                 dbox_lo(2) = dg_ystart + (jj     - dg_jstart1)*dg_dy
                 dbox_hi(2) = dg_ystart + (jj + 1 - dg_jstart1)*dg_dy

                 covered = area_inv * &
                      max(0.0d0, min(dbox_hi(1),fbox_hi(1)) -   &
                                 max(dbox_lo(1),fbox_lo(1))) *  &
                      max(0.0d0, min(dbox_hi(2),fbox_hi(2)) -   &
                                 max(dbox_lo(2),fbox_lo(2)))

                 ! Disable cells that are covered by non-fluid cells
                 if(.not.fluid_at(ijk) .and. covered > 0.1d0) then
                    pic_mi(lbcv)%blocked(ii,jj,:) = .true.
                 else
                    if(cut_cell_at(ijk)) then
                       coverage(ii,jj,k) = coverage(ii,jj,k) + covered*&
                            axy(ijk)*odx(i)*ody(j)
                    else
                       coverage(ii,jj,k) = coverage(ii,jj,k) + covered
                    endif
                 endif

              enddo
           enddo
        enddo
     enddo

     do i= i_lo, i_hi
        do j= j_lo, j_hi

           ijk = dg_funijk(i,j,k)

           if(coverage(i,j,k) < cover_limit) then

              if(facets_at_dg(ijk)%count == 0) then
                 pic_mi(lbcv)%blocked(i,j,k) = .true.
              else
                 all_default = .true.
                 do lc1=1,facets_at_dg(ijk)%count
                    if(facets_at_dg(ijk)%id(lc1) < stl_start(default_stl) .or. &
                         facets_at_dg(ijk)%id(lc1) > stl_end(default_stl)) &
                         all_default = .false.
                 enddo
                 if(all_default) pic_mi(lbcv)%blocked(i,j,:) = .true.
              endif
           endif
        enddo
     enddo

     do i= i_lo, i_hi
        do j= j_lo, j_hi
           if(any(pic_mi(lbcv)%blocked(i,j,:))) &
                pic_mi(lbcv)%blocked(i,j,:) = .true.
        enddo
     enddo


     if(ldebug) write(lUnit,*) 'coverage', sum(coverage)


     bc_lo = pic_mi(lbcv)%offset(:)
     bc_hi = bc_lo + pic_mi(lbcv)%length(:)

     do i= i_lo, i_hi
        if(ldebug) write(lUnit,*) "test (j+):", j
        if( .not.all(pic_mi(lbcv)%blocked(i,:,k)) ) then
           bc_lo(1) = max(bc_lo(1), dg_xstart+(i  -dg_istart1)*dg_dx)
           exit
        endif
     enddo

     do i= i_hi, i_lo, -1
        if(ldebug) write(lUnit,*) "test (j-):", j
        if( .not.all(pic_mi(lbcv)%blocked(i,:,k)) ) then
           bc_hi(1) = min(bc_hi(1), dg_xstart+(i+1-dg_istart1)*dg_dx)
           exit
        endif
     enddo


     do j= j_lo, j_hi
        if(ldebug) write(lUnit,*) "test (j+):", j
        if( .not.all(pic_mi(lbcv)%blocked(:,j,k)) ) then
           bc_lo(2) = max(bc_lo(2), dg_ystart+(j  -dg_jstart1)*dg_dy)
           exit
        endif
     enddo

     do j= j_hi, j_lo, -1
        if(ldebug) write(lUnit,*) "test (j-):", j
        if( .not.all(pic_mi(lbcv)%blocked(:,j,k)) ) then
           bc_hi(2) = min(bc_hi(2), dg_ystart+(j+1-dg_jstart1)*dg_dy)
           exit
        endif
     enddo


     pic_mi(lbcv)%offset(:) = bc_lo
     pic_mi(lbcv)%length(:) = bc_hi - bc_lo


     if(ldebug) then
        write(lUnit,*) '        '
        write(lUnit,*) '        '
        write(lUnit,*) 'BC seed extents'
        write(lUnit,*) 'Sx lo/hi', bc_lo(1), bc_hi(1)
        write(lUnit,*) 'Sy lo/hi', bc_lo(2), bc_hi(2)
        write(lUnit,*) 'Sz lo/hi', bc_lo(3), bc_hi(3)
        write(lUnit,*) '        '
        write(lUnit,*) '        '
        write(lUnit,*) ' Covered'

        do j= j_hi, j_lo, -1
           write(lUnit,"(2x,'k =',i3)",advance='no')k
           do i= i_lo, i_hi
              if(coverage(i,j,k) < 0.001) then
                 write(lUnit,"(' .')",advance='no')
              else if(coverage(i,j,k) < cover_limit_max) then
                 write(lUnit,"(' #')",advance='no')
              else
                 write(lUnit,"(' *')",advance='no')
              endif
           enddo
           write(lUnit,*)' '
        enddo


        write(lUnit,*) '        '
        write(lUnit,*) '        '
        write(lUnit,*) ' Blocked Flag '

        do j= j_hi, j_lo, -1
           write(lUnit,"(2x,'k =',i3)",advance='no')k
           do i= i_lo, i_hi
              if(pic_mi(lbcv)%blocked(i,j,k)) then
                 write(lUnit,"(' .')",advance='no')
              else
                 write(lUnit,"(' *')",advance='no')
              endif
           enddo
           write(lUnit,*)' '
        enddo

        write(lUnit,*) '        '
        write(lUnit,*) '        '
        write(lUnit,*) ' facets_at_dg'

        do j= j_hi, j_lo, -1
           write(lUnit,"(2x,'k =',i3)",advance='no')k
           do i= i_lo, i_hi

              ijk = dg_funijk(i,j,k)

              if(facets_at_dg(ijk)%count == 0) then
                 write(lUnit,"(' .')",advance='no')
              else if(facets_at_dg(ijk)%count == -1) then
                 write(lUnit,"(' #')",advance='no')
              else
                 write(lUnit,"(' *')",advance='no')
              endif
           enddo
           write(lUnit,*)' '
        enddo
     endif
  end select

end subroutine block_dg_cells



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: getBCProcArea                                           !
!                                                                      !
!  Purpose: Return the area of a BC local to the processor             !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
subroutine set_bc_pic_offset(lbcv, lbc_map)

! Modules
!---------------------------------------------------------------------//
  USE param1, only: half
  use physprop, only: D_p0

  use discretelement, only: des_mmax
  use discretelement, only: xe, yn, zt

  use bc, only: bc_plane, bc_pic_mi_const_statwt
  use bc, only: bc_i_e, bc_j_n, bc_k_t
  use bc, only: bc_i_w, bc_j_s, bc_k_b

  use compar, only: istart, jstart, kstart
  use compar, only: iend,   jend,   kend

  use mfix_pic, only: pic_mi

  use funits, only: newUnit

  implicit none

  integer, intent(in) :: lbcv, lbc_map


! Local Variables
!---------------------------------------------------------------------//
  integer :: phase

  double precision :: lrad, lstat_wt, eff_rad, pflow
  double precision :: bc_lo(3), bc_hi(3)

  integer :: lunit

  ! For complex boundaries defined by STLs, exclude any mass inflow cells
  ! that intersect the boundary and all cells opposite the normal. The MI
  ! cell sizes are increased by 10% to provide a small buffer.

  ! Find the largest effective radius for the mass inflow
  eff_rad = 0.0
  do phase=1,des_mmax
     pflow = pic_mi(lbcv)%pflow(phase)
     if(pflow > 0.0d0) then
        lrad  = d_p0(phase)*half
        lstat_wt = bc_pic_mi_const_statwt(lbc_map,phase)
        eff_rad = max(lrad*(lstat_wt)**(1.0/3.0), eff_rad)
     endif
  enddo

  bc_lo(1) = xe(max(bc_i_w(lbc_map)-1, istart-1))
  bc_lo(2) = yn(max(bc_j_s(lbc_map)-1, jstart-1))
  bc_lo(3) = zt(max(bc_k_b(lbc_map)-1, kstart-1))

  bc_hi(1) = xe(min(bc_i_e(lbc_map), iend))
  bc_hi(2) = yn(min(bc_j_n(lbc_map), jend))
  bc_hi(3) = zt(min(bc_k_t(lbc_map), kend))

  select case(bc_plane(lbc_map))
  case('E')

     bc_hi(1) = bc_hi(1) + eff_rad*1.01d0
     bc_lo(1) = bc_hi(1)

     if(bc_lo(2) == yn(bc_j_s(lbc_map)-1)) bc_lo(2) = bc_lo(2) + eff_rad
     if(bc_hi(2) == yn(bc_j_n(lbc_map)  )) bc_hi(2) = bc_hi(2) - eff_rad

     if(bc_lo(3) == zt(bc_k_b(lbc_map)-1)) bc_lo(3) = bc_lo(3) + eff_rad
     if(bc_hi(3) == zt(bc_k_t(lbc_map)  )) bc_hi(3) = bc_hi(3) - eff_rad

  case('N')

     if(bc_lo(1) == xe(bc_i_w(lbc_map)-1)) bc_lo(1) = bc_lo(1) + eff_rad
     if(bc_hi(1) == xe(bc_i_e(lbc_map)  )) bc_hi(1) = bc_hi(1) - eff_rad

     bc_hi(2) = bc_hi(2) + eff_rad*1.01d0
     bc_lo(2) = bc_hi(2)

     if(bc_lo(3) == zt(bc_k_b(lbc_map)-1)) bc_lo(3) = bc_lo(3) + eff_rad
     if(bc_hi(3) == zt(bc_k_t(lbc_map)  )) bc_hi(3) = bc_hi(3) - eff_rad

  case('T')

     if(bc_lo(1) == xe(bc_i_w(lbc_map)-1)) bc_lo(1) = bc_lo(1) + eff_rad
     if(bc_hi(1) == xe(bc_i_e(lbc_map)  )) bc_hi(1) = bc_hi(1) - eff_rad

     if(bc_lo(2) == yn(bc_j_s(lbc_map)-1)) bc_lo(2) = bc_lo(2) + eff_rad
     if(bc_hi(2) == yn(bc_j_n(lbc_map)  )) bc_hi(2) = bc_hi(2) - eff_rad

     bc_hi(3) = bc_hi(3) + eff_rad*1.01d0
     bc_lo(3) = bc_hi(3)

  case('W')

     bc_lo(1) = bc_lo(1) - eff_rad*1.01d0
     bc_hi(1) = bc_lo(1)

     if(bc_lo(2) == yn(bc_j_s(lbc_map)-1)) bc_lo(2) = bc_lo(2) + eff_rad
     if(bc_hi(2) == yn(bc_j_n(lbc_map)  )) bc_hi(2) = bc_hi(2) - eff_rad

     if(bc_lo(3) == zt(bc_k_b(lbc_map)-1)) bc_lo(3) = bc_lo(3) + eff_rad
     if(bc_hi(3) == zt(bc_k_t(lbc_map)  )) bc_hi(3) = bc_hi(3) - eff_rad

  case('S')

     if(bc_lo(1) == xe(bc_i_w(lbc_map)-1)) bc_lo(1) = bc_lo(1) + eff_rad
     if(bc_hi(1) == xe(bc_i_e(lbc_map)  )) bc_hi(1) = bc_hi(1) - eff_rad

     bc_lo(2) = bc_lo(2) - eff_rad*1.01d0
     bc_hi(2) = bc_lo(2)

     if(bc_lo(3) == zt(bc_k_b(lbc_map)-1)) bc_lo(3) = bc_lo(3) + eff_rad
     if(bc_hi(3) == zt(bc_k_t(lbc_map)  )) bc_hi(3) = bc_hi(3) - eff_rad

  case('B')

     if(bc_lo(1) == xe(bc_i_w(lbc_map)-1)) bc_lo(1) = bc_lo(1) + eff_rad
     if(bc_hi(1) == xe(bc_i_e(lbc_map)  )) bc_hi(1) = bc_hi(1) - eff_rad

     if(bc_lo(2) == yn(bc_j_s(lbc_map)-1)) bc_lo(2) = bc_lo(2) + eff_rad
     if(bc_hi(2) == yn(bc_j_n(lbc_map)  )) bc_hi(2) = bc_hi(2) - eff_rad

     bc_lo(3) = bc_lo(3) - eff_rad*1.01d0
     bc_hi(3) = bc_lo(3)

  end select

  pic_mi(lbcv)%offset(:) = bc_lo
  pic_mi(lbcv)%length(:) = bc_hi - bc_lo




  if(ldebug) then

     lunit = 6!newUnit()
     ! write(cPE,"(i2.2)") myPE
     ! open(unit=lunit, file='picbc_'//cPE//'.txt', &
     !      status='unknown', position='append')
     write(lUnit,*) '        '
     write(lUnit,*) '        '
     write(lUnit,*) '        '
     write(lUnit,*) '      bc', lbcv, lbc_map
     write(lUnit,*) 'bc_plane', '   ',bc_plane(lbc_map)
     write(lUnit,*) '        '
     write(lUnit,*) 'X  bc_lo', bc_lo(1), bc_hi(1)
     write(lUnit,*) 'Y  bc_hi', bc_lo(2), bc_hi(2)
     write(lUnit,*) 'Z  bc_hi', bc_lo(3), bc_hi(3)
     write(lUnit,*) '        '
     write(lUnit,*) '        '
     write(lUnit,*) '        '

     ! close(lunit)
  endif

end subroutine set_bc_pic_offset



end module bc_pic_mi
