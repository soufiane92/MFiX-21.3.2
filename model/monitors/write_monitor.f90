module write_monitor_mod

   use calc_cell_mod, only: calc_cell

contains

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Author: J.Musser                                     Date: 11-2017  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
  subroutine write_monitor(id)

    use monitor, only: monitor_type, monitor_var_count
    use monitor, only: monitor_x_w, monitor_x_e
    use monitor, only: monitor_y_s, monitor_y_n
    use monitor, only: monitor_z_b, monitor_z_t

    use geometry, only: x_min, imax, dx
    use geometry, only: y_min, jmax, dy
    use geometry, only: z_min, kmax, dz, do_k

    use compar, only: mype,pe_io

    use monitor_functions, only: monitor_open_file
    use monitor_functions, only: monitor_write_values

    use monitor_average_mod,       only: monitor_average
    use monitor_minmax_mod,        only: monitor_minmax
    use monitor_area_avg_mod,      only: monitor_area_avg
    use monitor_volume_avg_mod,    only: monitor_volume_avg

    use monitor_average_des_mod,   only: monitor_average_des
    use monitor_minmax_des_mod,    only: monitor_minmax_des
    use monitor_flowrate_des_mod,  only: monitor_flowrate_des


    integer, intent(in   ) :: id

    integer          :: i_w, i_e, j_s, j_n, k_b, k_t
    double precision :: x_w, x_e, y_s, y_n, z_b, z_t

    integer                       :: funit
    double precision, allocatable :: data(:)

    ! Calculate the cells in the monitor region
    call calc_cell (x_min, monitor_x_w(id), dx, imax, i_w)
    call calc_cell (x_min, monitor_x_e(id), dx, imax, i_e)

    call calc_cell (y_min, monitor_y_s(id), dy, jmax, j_s)
    call calc_cell (y_min, monitor_y_n(id), dy, jmax, j_n)

    if (do_k) then
       call calc_cell (z_min, monitor_z_b(id), dz, kmax, k_b)
       call calc_cell (z_min, monitor_z_t(id), dz, kmax, k_t)
    else
       k_b = 1
       k_t = 1
    endif

    if(monitor_var_count(id) <= 0) then
       return ! Error
    else
       allocate(data(monitor_var_count(id)))
    endif

    if(mype == pe_io) then
       call monitor_open_file(id, funit)
    endif

    select case(monitor_type(id))

    case(0,1,4,5)
       !  0: Point value
       !  1: Sum over region
       !  4: Arithmetic average over region
       !  5: Standard deviation over region
       call monitor_average(id, data, i_w, i_e, j_s, j_n, k_b, k_t)

    case(2,3)
       !  2: Minimum over region
       !  3: Maximum over region
       call monitor_minmax(id, data, i_w, i_e, j_s, j_n, k_b, k_t)

    case(6,7,8,9,10)
       !  6: Area-weighted average over surface
       !  7: Flow rate across surface
       !  8: Mass flow rate across surface
       !  9: Mass-weighted average over surface
       ! 10: Volumetric flow rate over surface
       call monitor_area_avg(id, data, i_w, i_e, j_s, j_n, k_b, k_t)

    case(11,12,13,14)
       ! 11: Volume integral
       ! 12: Volume-weighted average
       ! 13: Mass-weighted volume integral
       ! 14: Mass-weighted volume average
       call monitor_volume_avg(id, data, i_w, i_e, j_s, j_n, k_b, k_t)

    case(101,104,105,106,107)
       ! 101: Sum over region (DEM/PIC)
       ! 104: Arithmetic average over region (DEM/PIC)
       ! 105: Standard deviation over region (DEM/PIC)
       ! 106: Mass-weighted average over region (DEM/PIC)
       ! 107: Volume-weighted average over region (DEM/PIC)
       call monitor_average_des(id, data, i_w, i_e, j_s, j_n, k_b, k_t)

    case(102,103)
       ! 102: Minimum over region (DEM/PIC)
       ! 103: Maximum over region (DEM/PIC)
       call monitor_minmax_des(id, data, i_w, i_e, j_s, j_n, k_b, k_t)

    case(108,109,110)
       ! 108: Flow rate (DEM/PIC)
       ! 109: Mass-weighted flow rate (DEM/PIC)
       ! 110: Volume-weighted flow rate (DEM/PIC)
       call monitor_flowrate_des(id, data)

    end select

    if(mype == pe_io) then
       call monitor_write_values(funit, data)
       close(funit)
    endif

    if(allocated(data)) deallocate(data)

  end subroutine write_monitor


end module write_monitor_mod
