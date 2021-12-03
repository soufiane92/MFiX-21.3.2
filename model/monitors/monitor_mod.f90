MODULE monitor

  Use param1
! Maximum number of solids phases.
  use param, only: DIM_M
! Maximum number of gas phase species
  use param, only: DIM_N_g
! Maximum number of solids phase species
  use param, only: DIM_N_s
! Maximum number of scalar equations
  use param, only: DIM_Scalar
! Maximum number of DEM solids phase species
  use param, only: DIMENSION_N_S
! Maximum number of reaction rates saved in monitors
  use rxns, only: nrrmax

  integer, parameter :: dimension_monitor = 100

  character(len=256) :: monitor_name(dimension_monitor)
  integer :: monitor_type(dimension_monitor)
  double precision :: monitor_dt(dimension_monitor)

  logical :: monitor_defined (dimension_monitor)
  double precision :: monitor_time(dimension_monitor)
  integer :: monitor_var_count(dimension_monitor)

! Spatial location of monitors
  double precision :: monitor_x_w(dimension_monitor)
  double precision :: monitor_x_e(dimension_monitor)
  double precision :: monitor_y_s(dimension_monitor)
  double precision :: monitor_y_n(dimension_monitor)
  double precision :: monitor_z_b(dimension_monitor)
  double precision :: monitor_z_t(dimension_monitor)

! Fluid phase variables
  logical :: monitor_ep_g(dimension_monitor)
  logical :: monitor_ro_g(dimension_monitor)
  logical :: monitor_p_g(dimension_monitor)
  logical :: monitor_u_g(dimension_monitor)
  logical :: monitor_v_g(dimension_monitor)
  logical :: monitor_w_g(dimension_monitor)
  logical :: monitor_t_g(dimension_monitor)
  logical :: monitor_mw_mix_g(dimension_monitor)
  logical :: monitor_x_g(dimension_monitor, dim_n_g)
  logical :: monitor_y_g(dimension_monitor, dim_n_g)

  logical :: monitor_k_turb_g(dimension_monitor)
  logical :: monitor_e_turb_g(dimension_monitor)

! Eulerian solids variables
  logical :: monitor_ep_s (dimension_monitor, dim_m)
  logical :: monitor_rop_s(dimension_monitor, dim_m)
  logical :: monitor_ro_s(dimension_monitor, dim_m)
  logical :: monitor_p_star(dimension_monitor)
  logical :: monitor_p_s(dimension_monitor, dim_m)
  logical :: monitor_u_s(dimension_monitor, dim_m)
  logical :: monitor_v_s(dimension_monitor, dim_m)
  logical :: monitor_w_s(dimension_monitor, dim_m)
  logical :: monitor_t_s(dimension_monitor, dim_m)
  logical :: monitor_x_s(dimension_monitor, dim_m, dim_n_s)
  logical :: monitor_theta_m(dimension_monitor, dim_m)

  ! Other Field variables
  logical :: monitor_scalar(dimension_monitor, dim_scalar)
  logical :: monitor_rrate(dimension_monitor,nrrmax)
  logical :: monitor_fluid_rrate(dimension_monitor,nrrmax)
  logical :: monitor_des_rrate(dimension_monitor,nrrmax)

  ! DES specific variables
  logical :: monitor_part_phase(dimension_monitor, dim_m)

  logical :: monitor_radius(dimension_monitor)
  logical :: monitor_pmass(dimension_monitor)
  logical :: monitor_pvol(dimension_monitor)
  logical :: monitor_ro_p(dimension_monitor)

  logical :: monitor_vel_x(dimension_monitor)
  logical :: monitor_vel_y(dimension_monitor)
  logical :: monitor_vel_z(dimension_monitor)

  logical :: monitor_rot_x(dimension_monitor)
  logical :: monitor_rot_y(dimension_monitor)
  logical :: monitor_rot_z(dimension_monitor)

  logical :: monitor_t_p(dimension_monitor)
  logical :: monitor_x_p(dimension_monitor, dim_n_s)

  logical :: monitor_des_usr_var(dimension_monitor, 100)
  logical :: monitor_part_rrate(dimension_monitor, 100)

! Residence time
  logical :: monitor_part_residence_time(dimension_monitor)

end module monitor
