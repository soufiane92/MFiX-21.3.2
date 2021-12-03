module monitor_average_des_mod

  implicit none

  private
  public :: monitor_average_des

contains

  !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
  !                                                                      !
  !  Author: J.Musser                                     Date:  7-2019  !
  !                                                                      !
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
  subroutine monitor_average_des(id, data, i_w, i_e, j_s, j_n, k_b, k_t)

    use monitor, only: monitor_type, monitor_var_count
    use param1,  only: zero, undefined
    use compar,  only: mype,pe_io

    use geometry, only: x_min, dx
    use geometry, only: y_min, dy
    use geometry, only: z_min, dz

    use discretelement, only: pvol
    use mpi_utility,           only: global_sum, global_all_sum
    use monitor_functions_des, only: monitor_set_mask
    use monitor_functions_des, only: calc_face_lo
    use monitor_functions_des, only: monitor_set_weight
    use monitor_functions_des, only: monitor_collect_data_des


    integer,          intent(in   ) :: id
    integer,          intent(in   ) :: i_w, i_e, j_s, j_n, k_b, k_t
    double precision, intent(inout) :: data(:)

    integer :: lc1, vsize, psize

    double precision              :: x_w, x_e, y_s, y_n, z_b, z_t
    double precision, allocatable :: weight(:)
    logical,          allocatable :: mask(:)
    double precision, allocatable :: gvalues(:,:)
    double precision, allocatable :: lvalues(:,:)

    vsize = monitor_var_count(id)

    allocate(gvalues(vsize,2))
    allocate(lvalues(vsize,2))
    gvalues = zero
    lvalues = zero

    psize = size(pvol)
    allocate(weight(psize))
    allocate(mask(psize))

    ! Calculate region for DEM/PIC averaging
    x_w = calc_face_lo(x_min, dx, i_w  )
    x_e = calc_face_lo(x_min, dx, i_e+1)
    y_s = calc_face_lo(y_min, dy, j_s  )
    y_n = calc_face_lo(y_min, dy, j_n+1)
    z_b = calc_face_lo(z_min, dz, k_b  )
    z_t = calc_face_lo(z_min, dz, k_t+1)

    ! Set the mask to skip particles not in the collection region
    call monitor_set_mask(id, psize, mask, x_w, x_e, y_s, y_n, z_b, z_t)

    call monitor_set_weight(id, psize, mask, weight)

    call monitor_collect_data_des(id, monitor_calc_avg_des, &
         psize, mask, weight, vsize, gvalues)

    select case(monitor_type(id))

    case(101) ! value and sum
       ! 101: Sum over region

       call global_sum(gvalues(:,1), data)

    case(104,106,107)

       ! 104: Arithmetic average over region
       ! 106: Mass-weighted arithmetic average over region
       ! 107: Volume-weighted arithmetic average over region

       call global_sum(gvalues, lvalues)

       if(mype == pe_io) then
          do lc1=1, monitor_var_count(id)
             if(lvalues(lc1,2) > 0.0d0) then
                data(lc1) = lvalues(lc1,1)/lvalues(lc1,2)
             else
                data(lc1) = -undefined
             endif
          enddo
       endif

    case(105)

       ! 105: Standard deviation over region

       call global_all_sum(gvalues)

       do lc1=1, monitor_var_count(id)
          if(gvalues(lc1,2) > 0.0d0) then
             gvalues(lc1,2) = gvalues(lc1,1)/gvalues(lc1,2)
          else
             gvalues(lc1,2) = -undefined
          endif
          gvalues(lc1,1) = 0.0d0
       enddo

       call monitor_collect_data_des(id, monitor_calc_std_des, &
            psize, mask, weight, vsize, gvalues)

       call global_sum(gvalues, lvalues)

       if(mype == pe_io) then
          do lc1=1, monitor_var_count(id)
             if(lvalues(lc1,2) > 0.0d0) then
                data(lc1) = sqrt(lvalues(lc1,1)/lvalues(lc1,2))
             else
                data(lc1) = -undefined
             endif
          enddo
       endif

    case default
       data = undefined
    end select


    if(allocated(gvalues)) deallocate(gvalues)
    if(allocated(lvalues)) deallocate(lvalues)
    if(allocated(weight )) deallocate(weight )
    if(allocated(mask   )) deallocate(mask   )

  end subroutine monitor_average_des

  !....................................................................!
  !                                                                    !
  ! Copy value from field array to io array. Do some checks to make    !
  ! sure you will not read/write out of array space.                   !
  !                                                                    !
  !....................................................................!
  subroutine monitor_calc_avg_des(lc, psize, mask, weight, &
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

  end subroutine monitor_calc_avg_des

  !....................................................................!
  !                                                                    !
  ! Copy value from field array to io array. Do some checks to make    !
  ! sure you will not read/write out of array space.                   !
  !                                                                    !
  !....................................................................!
  subroutine monitor_calc_std_des (lc, psize, mask, weight, &
       vsize, values, var)

    use param1,         only: undefined
    use discretelement, only: max_pip

    integer,          intent(inout) :: lc, vsize, psize
    logical,          intent(in   ) :: mask(psize)
    double precision, intent(in   ) :: weight(psize)
    double precision, intent(inout) :: values(vsize,2)
    double precision, intent(in   ) :: var(psize)

    integer :: np
    double precision :: mean

    if(values(lc,2) /= -undefined) then

       mean = values(lc,2)
       values(lc,1) = 0.0d0
       values(lc,2) = 0.0d0

       do np=1,max_pip

          if( mask(np) ) then

             values(lc,1) = values(lc,1) + weight(np)*(var(np) - mean)**2
             values(lc,2) = values(lc,2) + weight(np)

          end if
       end do
    end if

    lc = lc+1

  end subroutine monitor_calc_std_des

end module monitor_average_des_mod
