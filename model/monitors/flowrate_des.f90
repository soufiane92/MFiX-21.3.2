module monitor_flowrate_des_mod

  implicit none

  private
  public :: monitor_flowrate_des

contains

  !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
  !                                                                      !
  !  Author: J.Musser                                     Date:  7-2019  !
  !                                                                      !
  !  Modified: J.-F. Dietiker                             Date: 10/7/2021!
  !                                                                      !
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
  subroutine monitor_flowrate_des(id, data)

    use monitor, only: monitor_type, monitor_var_count
    use monitor, only: monitor_x_w, monitor_y_s, monitor_z_b
    use monitor, only: monitor_x_e, monitor_y_n, monitor_z_t
    use param1,  only: zero, undefined

    use mpi_utility,           only: global_sum
    use monitor_functions_des, only: monitor_set_mask
    use monitor_functions_des, only: monitor_set_mask_flow
    use monitor_functions_des, only: monitor_set_weight
    use monitor_functions_des, only: monitor_collect_data_des

    use run,               only: odt
    use discretelement,    only: pvol

    integer,          intent(in   ) :: id
    double precision, intent(inout) :: data(:)

    double precision, allocatable   :: values(:,:)
    double precision, allocatable   :: weight(:)
    logical,          allocatable   :: mask(:)
    double precision                :: x_w, x_e, y_s, y_n, z_b, z_t
    double precision, allocatable   :: data_r(:,:)


    integer :: vsize, psize, plane,dsize,i, region

    vsize = monitor_var_count(id)
    allocate(values(vsize,2))
    values = zero

    psize = size(pvol)
    allocate(weight(psize))
    allocate(mask(psize))

    weight(:) = 0.0D0
    mask(:) = .false.

    dsize = size(data)
    allocate(data_r(2,dsize))


    x_w = monitor_x_w(id)
    x_e = monitor_x_e(id)
    y_s = monitor_y_s(id)
    y_n = monitor_y_n(id)
    z_b = monitor_z_b(id)
    z_t = monitor_z_t(id)

    if(x_w == x_e) then
       plane=1
       x_w = -undefined
       x_e =  undefined

    else if(y_s == y_n) then
       plane=2
       y_s = -undefined
       y_n =  undefined

    else if(z_b == z_t) then
       plane=3
       z_b = -undefined
       z_t =  undefined

    else
       return
    endif


    ! The original implementation only looked at particles that were
    ! projected to have crossed the plane over the previous DT,
    ! i.e., particles moving away from the monitor plane.
    ! This meant we always got a zero mass flow rate along outflow BC planes.
    ! Results were also dependent on DT (fluid time step) and particle size.
    ! This version (21.3.1) attempts to correct this.
    ! First we look at particles both in the forward and backward regions
    ! Region 1 = east/north/front of the x/y/z plane,
    ! Region 2 = west/south/back of the x/y/z plane.
    ! Particles moving both away and towards the plane are contributing
    ! to the mass flow rate in each region (the flow rate is a signed quantity).
    ! Internal planes will have non-zero flow rates for both regions 1 and 2
    ! (as long as particles cross the plane). The flow rates in regions 1 and 2
    ! should be very close to each other.
    ! Planes along boundaries will have region 1 or region 2 flow rate always
    ! equal to zero. For example an outflow at y=ymax will always have a zero
    ! flow rate in region 1 (region 1 is above y=ymax and is outside the
    ! domain, so it does not contain normal particles). Region 2 will get a
    ! positive flow rates when particles are moving towards the plane (up).
    ! Then we choose the flow rate between region 1 and 2 that is maximum
    ! in magnitude.

    do region = 1,2

       values    = zero
       weight(:) = zero
       mask(:)   = .false.

       ! Set the mask to skip particles not in the collection region
       call monitor_set_mask(id, psize, mask, x_w, x_e, y_s, y_n, z_b, z_t)

       ! Set the weight.
       call monitor_set_weight(id, psize, mask, weight)

       call monitor_set_mask_flow(id, plane, region, psize, mask, weight)

       call monitor_collect_data_des(id, monitor_calc_flowrate_des, &
            psize, mask, weight, vsize, values)

       call global_sum(values(:,1), data_r(region,:))

    enddo

    ! Pick up the max (in absolute value) among region 1 and region 2 flow
    ! rates.
    do i = 1,dsize
       if(abs(data_r(1,i))>abs(data_r(2,i))) then
          data(i) = data_r(1,i)
       else
          data(i) = data_r(2,i)
       endif
    enddo

    if(allocated(values)) deallocate(values)
    if(allocated(weight)) deallocate(weight)
    if(allocated( mask )) deallocate( mask )
    if(allocated( data_r )) deallocate( data_r )

  end subroutine monitor_flowrate_des

  !....................................................................!
  !                                                                    !
  ! Copy value from field array to io array. Do some checks to make    !
  ! sure you will not read/write out of array space.                   !
  !                                                                    !
  !....................................................................!
  subroutine monitor_calc_flowrate_des (lc, psize, mask, weight, &
       vsize, values, var)

    use discretelement, only: max_pip

    integer,          intent(inout) :: lc, vsize, psize
    logical,          intent(in   ) :: mask(psize)
    double precision, intent(in   ) :: weight(psize)
    double precision, intent(inout) :: values(vsize,2)
    double precision, intent(in   ) :: var(psize)

    integer :: np

    do np=1,max_pip

       if( mask(np) ) then

          values(lc,1) = values(lc,1) + weight(np)*var(np)
          values(lc,2) = values(lc,2) + weight(np)

       end if
    end do

    lc = lc+1
  end subroutine monitor_calc_flowrate_des

end module monitor_flowrate_des_mod
