module monitor_functions_des

  implicit none

  private

  public :: monitor_collect_data_des
  public :: monitor_set_mask
  public :: monitor_set_mask_flow
  public :: monitor_set_weight
  public :: calc_face_lo

contains
  !....................................................................!
  !                                                                    !
  ! Populate *values* array with correct field variable data.          !
  !                                                                    !
  !....................................................................!
  subroutine monitor_set_mask(lid, psize, mask, &
       xw, xe, ys, yn, zb, zt)

    use discretelement, only: pijk, max_pip, des_mmax
    use functions,      only: is_normal

    use discretelement, only: des_pos_new

    use monitor,        only: monitor_part_phase

    integer,          intent(in   ) :: lid, psize
    logical,          intent(  out) :: mask(psize)
    double precision, intent(in   ) :: xw, xe, ys, yn, zb, zt

    integer :: lc1, phase

    do lc1=1,max_pip

       ! Default the mask to false
       mask(lc1) = .false.

       if ( is_normal(lc1) ) then
          ! Phase of current particle
          phase = pijk(lc1,5)

          if( monitor_part_phase(lid, phase) ) then


             if ( xw <= des_pos_new(lc1,1) .and. &
                  xe >= des_pos_new(lc1,1) .and. &
                  ys <= des_pos_new(lc1,2) .and. &
                  yn >= des_pos_new(lc1,2) .and. &
                  zb <= des_pos_new(lc1,3) .and. &
                  zt >= des_pos_new(lc1,3)) then

                mask(lc1) = .true.

             end if

          end if

       end if

    end do

  end subroutine monitor_set_mask


  !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
  !                                                                      !
  !  Author: J.Musser                                     Date:  7-2019  !
  !                                                                      !
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
  subroutine monitor_set_weight(id, psize, mask, weight)

    use monitor,        only: monitor_type
    use discretelement, only: max_pip
    use discretelement, only: pmass, pvol
    use mfix_pic,       only: des_stat_wt, mppic

    integer,          intent(in   ) :: id, psize
    logical,          intent(in   ) :: mask(psize)
    double precision, intent(  out) :: weight(psize)

    integer  :: lc1

    logical :: use_stat_wt

    select case(monitor_type(id))

    case(106,109) ! Mass-weighted average / flow

       use_stat_wt = mppic
       do lc1=1,max_pip
          weight(lc1) = merge(pmass(lc1), 0.0d0, mask(lc1))
       end do

    case(107,110) ! Volume-weighted average / flow

       use_stat_wt = mppic
       do lc1=1,max_pip
          weight(lc1) = merge(pvol(lc1), 0.0d0, mask(lc1))
       end do

    case(102,103)! Max/min of value

       use_stat_wt = .false.
       weight = 1.0d0

    case default  ! Not weighted

       use_stat_wt = mppic
       weight = 1.0d0

    end select

    ! Include the stat weight
    if ( use_stat_wt ) then
       do lc1=1,max_pip
          weight(lc1) = weight(lc1)*&
               merge(des_stat_wt(lc1), 0.0d0, mask(lc1))
       end do
    end if

  end subroutine monitor_set_weight


  !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
  !                                                                      !
  !  Author: J.Musser                                     Date:  7-2019  !
  !                                                                      !
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
  subroutine monitor_set_mask_flow_0(id, plane, psize, mask, weight)

    use run,            only: odt
    use discretelement, only: max_pip
    use discretelement, only: des_pos_new, des_vel_new
    use monitor,        only: monitor_x_w, monitor_y_s, monitor_z_b

    integer,          intent(in   ) :: id, plane, psize
    logical,          intent(inout) :: mask(psize)
    double precision, intent(inout) :: weight(psize)

    integer          :: lc1, lc2
    double precision :: x0, xp, vp, dxdt

    select case(plane)
    case(1); lc2 = 1;   x0 = monitor_x_w(id)
    case(2); lc2 = 2;   x0 = monitor_y_s(id)
    case(3); lc2 = 3;   x0 = monitor_z_b(id)
    end select


    do lc1=1,max_pip

       if ( mask(lc1) ) then

          xp = des_pos_new(lc1,lc2)
          vp = des_vel_new(lc1,lc2)

          dxdt = (xp - x0) * odt

          ! Verify that they have the same sign
          if(dxdt*vp < 0.0d0) then

             mask(lc1) = .false.

          else if( abs(vp) < abs(dxdt)) then

             mask(lc1) = .false.

          end if

          ! The mask is fully set now. Adjust the weight to
          ! account for flow direction.
          if( mask(lc1) ) then

             weight(lc1) = dsign(weight(lc1), vp)

          else

             weight(lc1) = 0.0d0

          end if

       end if


    end do

  end subroutine monitor_set_mask_flow_0


  !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
  !                                                                      !
  !  Author: J.Musser                                     Date:  7-2019  !
  !                                                                      !
  !  Modified: J.-F. Dietiker                             Date: 10/7/2021!
  !                                                                      !
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
  subroutine monitor_set_mask_flow(id, plane, region, psize, mask, weight)

    use run,            only: odt
    use discretelement, only: max_pip
    use discretelement, only: des_pos_new, des_vel_new, des_radius
    use mfix_pic,       only: des_stat_wt, mppic
    use monitor,        only: monitor_x_w, monitor_y_s, monitor_z_b
    use param1,         only: zero

    integer,          intent(in   ) :: id, plane, region, psize
    logical,          intent(inout) :: mask(psize)
    double precision, intent(inout) :: weight(psize)

    integer          :: lc1, lc2
    double precision :: x0, xp, vp, rp, dxdt, s, tau, otau, x0c

    select case(plane)
    case(1); lc2 = 1;   x0 = monitor_x_w(id)
    case(2); lc2 = 2;   x0 = monitor_y_s(id)
    case(3); lc2 = 3;   x0 = monitor_z_b(id)
    end select

! Region 1 corresponds to coordinates larger than the plane coordinate
! Region 2 corresponds to coordinates smaller than the plane coordinate
    select case(region)
    case(1); s =  1.0D0
    case(2); s = -1.0D0
    end select


    do lc1=1,max_pip

       if ( mask(lc1) ) then

          xp = des_pos_new(lc1,lc2)
          vp = des_vel_new(lc1,lc2)
          rp = des_radius(lc1)

          if(mppic) rp = rp * des_stat_wt(lc1)**(1.0D0/3.0D0)

          if(vp==zero) then
             mask(lc1) = .false.
             weight(lc1) = zero
             cycle
          endif

! The original version (see monitor_set_mask_flow_0) was dependent on the
! fluid time step DT and the particle size, which made the result unreliable.
! For example, if DT<radius/velocity then the flow rate would be zero.
! The new version should be independent of time step and particle size.

! shift the plane so we can detect particles coming form a mass inlet BC plane
! Particle become "normal" once they are at least on radius away from the plane.
          x0c = x0 + s*rp

! Representative time to project if a particle has crossed the plane (when
! moving away from plane), or will cross the plane (when moving towards the
! plane).
          tau = 2.0D0*rp/abs(vp)   ! we cycle when vp=zero
          otau = 1.0D0/tau

! Velocity required to cross plane.
          dxdt = (xp - x0c) / tau
! the test dxdt*s>zero is used to differentiate region 1 and 2
          if( abs(vp) > abs(dxdt).and.dxdt*s>zero) then
! The weight includes the time scale so a mass flow rate is in kg/s
             if(vp>zero) then
                weight(lc1) = weight(lc1)*otau
             elseif(vp<zero) then
                weight(lc1) = -weight(lc1)*otau
             else
                weight(lc1) = zero
             endif
          else

             mask(lc1) = .false.
             weight(lc1) = zero

          end if

       end if

    end do

  end subroutine monitor_set_mask_flow

  !....................................................................!
  !                                                                    !
  ! Populate *values* array with correct field variable data.          !
  !                                                                    !
  !....................................................................!
  subroutine monitor_collect_data_des(lid, get_value, psize, mask, &
       weight, vsize, values)

    use param,          only: dimension_n_s

    use discretelement, only: des_radius, pmass, pvol, ro_sol
    use discretelement, only: des_vel_new, omega_new
    use discretelement, only: des_usr_var_size, des_usr_var
    use discretelement, only: residence_time
    use des_thermo,     only: des_t_s
    use des_rxns,       only: des_x_s, no_of_des_rxns, part_rrates_out

    use monitor,        only:                                        &
         monitor_radius, monitor_pmass, monitor_pvol,  monitor_ro_p, &
         monitor_vel_x,  monitor_vel_y, monitor_vel_z,               &
         monitor_rot_x,  monitor_rot_y, monitor_rot_z,               &
         monitor_t_p,    monitor_x_p,                                &
         monitor_des_usr_var, monitor_part_rrate,                    &
         monitor_part_residence_time

    integer,          intent(in   ) :: lid, vsize, psize
    logical,          intent(in   ) :: mask(  psize)
    double precision, intent(in   ) :: weight(psize)
    double precision, intent(inout) :: values(vsize,2)


    double precision, allocatable   :: tmpvar(:)

    integer :: lc, lc1

    lc = 1

    if(monitor_radius(lid)) &
         call get_value(lc, psize, mask, weight, vsize, values, des_radius)

    if(monitor_pmass (lid)) &
         call get_value(lc, psize, mask, weight, vsize, values, pmass)

    if(monitor_pvol  (lid)) &
         call get_value(lc, psize, mask, weight, vsize, values, pvol)

    if(monitor_ro_p  (lid)) &
         call get_value(lc, psize, mask, weight, vsize, values, ro_sol)

    if(monitor_vel_x (lid)) &
         call get_value(lc, psize, mask, weight, vsize, values, des_vel_new(:,1))

    if(monitor_vel_y (lid)) &
         call get_value(lc, psize, mask, weight, vsize, values, des_vel_new(:,2))

    if(monitor_vel_z (lid)) &
         call get_value(lc, psize, mask, weight, vsize, values, des_vel_new(:,3))

    if(monitor_rot_x (lid)) &
         call get_value(lc, psize, mask, weight, vsize, values, omega_new(:,1))

    if(monitor_rot_y (lid)) &
         call get_value(lc, psize, mask, weight, vsize, values, omega_new(:,2))

    if(monitor_rot_z (lid)) &
         call get_value(lc, psize, mask, weight, vsize, values, omega_new(:,3))

    if(monitor_t_p   (lid)) &
         call get_value(lc, psize, mask, weight, vsize, values, des_t_s)

    do lc1 = 1, dimension_n_s
       if(monitor_x_p(lid,lc1)) &
            call get_value(lc, psize, mask, weight, vsize, values, des_x_s(:,lc1))
    end do

    do lc1 = 1, des_usr_var_size
       if(monitor_des_usr_var(lid,lc1)) &
            call get_value(lc, psize, mask, weight, vsize, values, des_usr_var(lc1,:))
    end do

    do lc1 = 1, NO_OF_DES_RXNS
       if(monitor_part_rrate(lid,lc1)) &
            call get_value(lc, psize, mask, weight, vsize, values, Part_rrates_out(:,lc1))
    end do

    if(monitor_part_residence_time   (lid)) &
         call get_value(lc, psize, mask, weight, vsize, values, residence_time)

  end subroutine monitor_collect_data_des


  !....................................................................!
  !                                                                    !
  ! Local helper function to calculate the low side of a cell.         !
  !                                                                    !
  !....................................................................!
  function calc_face_lo(lmin, ldx, lindex ) result( face )

    double precision, intent(in   ) :: lmin
    double precision, intent(in   ) :: ldx(:)
    integer,          intent(in   ) :: lindex

    double precision :: face

    integer :: lc

    face = lmin
    do lc=2, lindex-1
       face = face + ldx(lc)
    end do

  end function calc_face_lo


  !....................................................................!
  !                                                                    !
  ! Multiple passed variable by the statistical weight. This only      !
  ! makes sense for particle mass and volume.                          !
  !                                                                    !
  !....................................................................!
  subroutine scale_var(var_in, psize, mask, var_out)

    use discretelement, only: max_pip
    use mfix_pic,       only: des_stat_wt

    double precision,              intent(in   ) :: var_in(:)
    integer,                       intent(in   ) :: psize
    logical,                       intent(in   ) :: mask(psize)
    double precision, allocatable, intent(  out) :: var_out(:)

    integer :: lc1

    if(allocated(var_out)) deallocate(var_out)
    allocate(var_out(psize))

    do lc1=1, max_pip

       if( mask(lc1) ) var_out(lc1) = var_in(lc1) * des_stat_wt(lc1)

    end do

  end subroutine scale_var

end module monitor_functions_des
