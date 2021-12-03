MODULE CALC_GRAD_DES_MOD

  use geometry, only: do_k

  use functions, only: fluid_at
  use geometry,  only: flag

  use compar,    only: imap,          jmap,          kmap
  use indices,   only: i_of,          j_of,          k_of
  use geometry,  only: imax1,         jmax1,         kmax1

  use geometry,  only: dx,            dy,            dz

  use geometry,  only: cyclic_x_pd,   cyclic_y_pd,   cyclic_z_pd
  use bc,        only: delp_x,        delp_y,        delp_z

  use functions, only: east_of,       north_of,      top_of
  use functions, only: west_of,       south_of,      bottom_of

  use compar,    only: ijkstart3, ijkend3
  use param,     only: dimension_3

  use param1,    only: zero

  implicit none

  private
  public :: calc_grad_des


CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CALC_PG_GRAD                                            !
!  Purpose: Calculate cell centered pressure force exerted on the      !
!           particles in the cell by the gas/fluid phase               !
!           Note that P_force is evaluated as -dp/dx                   !
!                                                                      !
!  Notes: This pressure force only needs to be calculated once during  !
!         the DEM loop (at the beginning) since the gas/fluid phase    !
!         is essentially static at that point (i.e., gas field is not  !
!         updated during DEM loop                                      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   subroutine calc_grad_des(phi, del_phi)

! Dummy Arguments:
!---------------------------------------------------------------------//
      double precision, intent(in   ) :: phi(dimension_3)
      double precision, intent(  out) :: del_phi(3,dimension_3)

! Local variables
!---------------------------------------------------------------------//
! General i, j, k indices
      integer          :: i, j, k, ijk, lo, hi

      double precision :: phi_lo, phi_hi
      double precision ::  dx_lo,  dx_hi, dx_c

      double precision :: delp
!......................................................................!

      double precision :: ldelp_x, ldelp_y, ldelp_z

      ! Set local delp flags to reduce checks inside loop
      ldelp_x = merge(-delp_x, zero, cyclic_x_pd)
      ldelp_y = merge(-delp_y, zero, cyclic_y_pd)
      ldelp_z = merge(-delp_z, zero, cyclic_z_pd)


      do ijk = ijkstart3, ijkend3

         if(fluid_at(ijk)) then

            ! Gradient over X direction
            i  = i_of(ijk)
            hi = east_of(ijk)
            lo = west_of(ijk)

            dx_c  = dx(i)
            dx_lo = dx(i_of(lo))
            dx_hi = dx(i_of(hi))

            delp = merge(ldelp_x, zero, imap(i) == imax1)

            phi_lo = phi(lo)
            phi_hi = phi(hi) + delp

            del_phi(1,ijk) = calc_del_phi(flag(lo), flag(hi), &
                 phi(ijk), dx_c, phi_lo, dx_lo, phi_hi, dx_hi)



            ! Gradient over Y direction
            j  = j_of(ijk)
            hi = north_of(ijk)
            lo = south_of(ijk)

            dx_c  = dy(j)
            dx_lo = dy(j_of(lo))
            dx_hi = dy(j_of(hi))

            delp = merge(ldelp_y, zero, jmap(j) == jmax1)

            phi_lo = phi(lo)
            phi_hi = phi(hi) + delp

            del_phi(2,ijk) = calc_del_phi(flag(lo), flag(hi), &
                 phi(ijk), dx_c, phi_lo, dx_lo, phi_hi, dx_hi)



            if(do_k) then

               ! Gradient over Z direction
               k  = k_of(ijk)
               hi =    top_of(ijk)
               lo = bottom_of(ijk)

               dx_c  = dz(k)
               dx_lo = dz(k_of(lo))
               dx_hi = dz(k_of(hi))

               delp = merge(ldelp_z, zero, kmap(k) == kmax1)

               phi_lo = phi(lo)
               phi_hi = phi(hi) + delp

               del_phi(3,ijk) = calc_del_phi(flag(lo), flag(hi), &
                    phi(ijk), dx_c, phi_lo, dx_lo, phi_hi, dx_hi)

            end if

         else

            del_phi(:,ijk) = zero

         end if

      end do

      return


    end subroutine calc_grad_des


   function calc_del_phi(flg_lo, flg_hi, phi, dx, &
        phi_lo_in, dx_lo, phi_hi_in, dx_hi ) result(dphidx)

     implicit none

     ! Logical flags for fluid_at lo/hi cells
     integer         , intent(in) :: flg_lo,    flg_hi
     double precision, intent(in) :: phi_lo_in, phi_hi_in, phi
     double precision, intent(in) ::  dx_lo,     dx_hi,     dx

     ! Local copies to edit if needed
     double precision :: phi_lo, phi_hi

     ! d(phi)/dx
     double precision :: dphidx, x, x_hi
     logical :: invalid_lo, invalid_hi

     ! Set local flag for invalid pressure in lo/hi cells
     invalid_lo = .not.is_valid_pressure_cell(flg_lo)
     invalid_hi = .not.is_valid_pressure_cell(flg_hi)

     ! Set these now, so we can use them to calculate the extrapolated
     ! values for missing phi's.

     phi_lo = phi_lo_in
     phi_hi = phi_hi_in

     ! We need at least two adjacent cells to calculate a gradient.
     if(invalid_lo .and. invalid_hi) then

        dphidx = 0.0d0

     else

        ! extrapolate to get phi at low cell center
        if(invalid_lo) then
           phi_lo = phi_hi + (phi - phi_hi) * &
                (dx_hi + 2.0d0*dx + dx_lo)/(dx + dx_hi)
        endif

        ! extrapolate to get phi at high cell center
        if(invalid_hi) then
           phi_hi = phi_lo + (phi - phi_lo) * &
                (dx_hi + 2.0d0*dx + dx_lo)/(dx + dx_lo)
        endif

        ! Calculate the gradient:
        !
        ! f'(x) = [f(x+h) - f(x-h)] / 2*h + HOT(h^2)
        ! f(x+h) = phi_hi   where   h(lo) = 0.5*(dx_lo + dx)
        ! f(x-h) = phi_lo   where   h(hi) = 0.5*(dx_hi + dx)
        ! 2*h = (h(lo) + h(hi)) = 0.5*(dx_lo + 2*dx + dx_hi)
        ! dphidx = 2.0d0*(phi_hi - phi_lo)/(dx_lo + 2.0d0*dx + dx_hi)
        !
        ! The above approach isn't correct for stretched grids, although
        ! the error for "reasonable stretching"" is likely small. To account
        ! for all grids, we use Lagrange polynomials. Here we "shift" the
        ! axis to place the low side (xlo) at the origin (0.0) to avoid
        ! unnecessary calculations.
        !
        ! x_lo = 0.0d0
        x    = 0.5d0 * (dx_lo + dx)
        x_hi = 0.5d0 * (dx_hi + dx) + x

        ! dphidx = phi *(2.0d0*x - x_lo - x_hi)/((x - x_lo)*(x - x_hi)) + &
        !      phi_lo *(x - x_hi)/((x - x_lo)*(x_lo - x_hi)) + &
        !      phi_hi *(x - x_lo)/((x_hi - x_lo)*(x_hi - x))

        dphidx =  phi_lo * (   x - x_hi   )/(x_hi*x) + &
             &    phi    * (2.0d0*x - x_hi)/(x*(x-x_hi)) + &
             &    phi_hi * (      x       )/(x_hi*(x_hi - x))

     endif

   end function calc_del_phi


   pure function is_valid_pressure_cell(lflag) result(is_valid)

     implicit none

     integer, intent(in   ) :: lflag
     logical :: is_valid

     is_valid = (lflag ==   1 .or. &  ! fluid cell
          &      lflag ==  10 .or. &  ! pressure inflow cell
          &      lflag ==  11 .or. &  ! pressure outflow cell
          &      lflag == 106 .or. &  ! cyclic
          &      lflag == 107)        ! cyclic with pressure drop

   end function is_valid_pressure_cell

end module calc_grad_des_mod
