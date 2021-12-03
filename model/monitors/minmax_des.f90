module monitor_minmax_des_mod

  implicit none

  private
  public :: monitor_minmax_des

contains

  !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
  !                                                                      !
  !  Author: J.Musser                                     Date: 11-2017  !
  !                                                                      !
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
  subroutine monitor_minmax_des(id, data, i_w, i_e, j_s, j_n, k_b, k_t)

    use monitor,        only: monitor_type, monitor_var_count
    use param1,         only: undefined
    use discretelement, only: pvol

    use geometry,       only: x_min, dx
    use geometry,       only: y_min, dy
    use geometry,       only: z_min, dz

    use mpi_utility,           only: global_min
    use mpi_utility,           only: global_max
    use monitor_functions_des, only: monitor_collect_data_des
    use monitor_functions_des, only: monitor_set_mask
    use monitor_functions_des, only: calc_face_lo

    integer,          intent(in   ) :: id
    integer,          intent(in   ) :: i_w, i_e, j_s, j_n, k_b, k_t
    double precision, intent(inout) :: data(:)

    integer :: vsize, psize

    double precision              :: x_w, x_e, y_s, y_n, z_b, z_t
    double precision, allocatable :: weight(:)
    logical,          allocatable :: mask(:)
    double precision, allocatable :: gvalues(:,:)

    vsize = monitor_var_count(id)
    allocate(gvalues(vsize,2))
    gvalues(:,1) =  undefined
    gvalues(:,2) = -undefined

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

    ! No need to set the weight to anything other than one
    weight = 1.0d0

    call monitor_collect_data_des(id, monitor_calc_minmax_des, &
         psize, mask, weight, vsize, gvalues)

    if(monitor_type(id) == 102) then
       call global_min(gvalues(:,1), data)
    else
       call global_max(gvalues(:,2), data)
    endif

    if(allocated(weight )) deallocate(weight )
    if(allocated(mask   )) deallocate(mask   )
    if(allocated(gvalues)) deallocate(gvalues)

  end subroutine monitor_minmax_des

  !....................................................................!
  !                                                                    !
  ! Copy value from field array to io array. Do some checks to make    !
  ! sure you will not read/write out of array space.                   !
  !                                                                    !
  !....................................................................!
  subroutine monitor_calc_minmax_des (lc, psize, mask, weight, &
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

          values(lc,1) = min(values(lc,1), var(np))
          values(lc,2) = max(values(lc,2), var(np))

       end if
    end do

    lc = lc+1
  end subroutine monitor_calc_minmax_des

end module monitor_minmax_des_mod
