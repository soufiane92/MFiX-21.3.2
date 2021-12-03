!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: pic_bc_mod.f                                           !
!                                                                      !
!  Purpose: Common elements needed for the pic mass inflow boundary    !
!  condition.                                                          !
!                                                                      !
!  Author: R. Garg                                   Date: 11-Jun-14   !
!                                                                      !
!  Comments:                                                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
module pic_flow_bcs

!......................................................................!

  public :: mass_inflow_pic
  public :: delete_parcel

contains

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: MPPIC_MI_BC                                             C
!  Purpose:  seeds parcels for a mass inflow boundary condition        C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
SUBROUTINE MASS_INFLOW_PIC

! Modules
!---------------------------------------------------------------------//
  use physprop, only: D_p0, c_ps0, nmax

  use discretelement, only: des_mmax, max_pip, pip
  use discretelement, only: dtsolid
  use discretelement, only: des_pos_new
  use discretelement, only: des_vel_new
  use discretelement, only: ro_sol, des_radius
  use discretelement, only: pmass, pvol
  use discretelement, only: pijk
  use discretelement, only: xe, yn, zt
  use discretelement, only: iglobal_id, imax_global_id
  use discretelement, only: residence_time

  use des_rxns, only: des_x_s
  use des_thermo, only: des_t_s

  use desgrid, only: dg_istart2, dg_jstart2, dg_kstart2
  use desgrid, only: dg_iend2,   dg_jend2,   dg_kend2

  use mfix_pic, only: des_stat_wt

  use geometry, only: imin2, imax2
  use geometry, only: jmin2, jmax2
  use geometry, only: kmin2, kmax2

  use bc, only: bc_ro_s, bc_pic_mi_const_statwt
  use bc, only: bc_u_s, bc_v_s, bc_w_s
  use bc, only: bc_t_s, bc_x_s

  use functions, only: set_normal, fluid_at
  use functions, only: funijk
  use des_allocate, only: particle_grow

  use param, only: dimension_i, dimension_j, dimension_k

  use compar, only: numPEs,  myPE

  use mfix_pic, only: pic_bcmi, pic_mi
  use mfix_pic, only: pic_bcmi_map

  use desgrid, only: dg_funijk, iofpos, jofpos, kofpos

  use mpi_utility, only: global_all_sum

  use constant, only: pi
  use stl_functions_des, only: picSTLoverlap

  use run, only: energy_eq
  use run, only: any_species_eq

  USE param1, only: zero, half, undefined

  implicit none

! Local Variables
!---------------------------------------------------------------------//
  integer :: bcv, bc_map, phase, i,j,k, lc1, gid_offset

  double precision :: pflow
  integer :: start_pip, start_max_pip, lnmax
  integer :: iseed, seeded(0:numPEs-1)

  double precision :: lvol, lmass, lrad, lro, lstat_wt
  double precision :: eff_rad,  rand(3), pos(3)
  double precision :: offset(3), len(3)

  integer :: skipped, dg_ijk

  logical :: check_stl
  logical, parameter :: ldebug = .false.
!......................................................................!

  start_pip = pip
  start_max_pip = max_pip
  seeded = 0

  do bcv = 1, pic_bcmi

     bc_map = pic_bcmi_map(bcv)

     offset = pic_mi(bcv)%offset
     len    = pic_mi(bcv)%length

     check_stl = allocated(pic_mi(bcv)%blocked)

     if(ldebug) then
        write(*,*) ' '
        write(*,*) 'PIC MI:', bcv, bc_map
     endif

     do phase=1,des_mmax

        pflow = pic_mi(bcv)%pflow(phase)*dtsolid
        pflow = pflow + pic_mi(bcv)%remdr(phase)

        iseed = floor(pflow)

        pic_mi(bcv)%remdr(phase) = pflow - real(iseed)

        if(ldebug) then
           write(*,*) ' '
           write(*,*) '   phase:', phase
           write(*,*) '   pflow:', pic_mi(bcv)%pflow(phase)
           write(*,*) '    seed:', iseed
           write(*,*) '   remdr:', pic_mi(bcv)%remdr(phase)
        endif

        lro   = bc_ro_s(bc_map, phase)
        lrad  = d_p0(phase)*half
        lvol  = (4.0/3.0)*pi*lrad**3
        lmass = lvol*lro
        lnmax = nmax(phase)

        lstat_wt = bc_pic_mi_const_statwt(bc_map,phase)
        eff_rad = lrad*(lstat_wt)**(1.0/3.0)

        if(ldebug) then
           write(*,*) ' '
           write(*,*) '     lro:', lro
           write(*,*) '    lrad:', lrad
           write(*,*) '    lvol:', lvol
           write(*,*) '   lmass:', lmass
        endif

        pic_lp: do lc1=1, iseed

           call random_number(rand)
           pos = offset + len*rand

           ! Bin to the particle grid
           i = min(dg_iend2,max(dg_istart2, iofpos(pos(1))))
           j = min(dg_jend2,max(dg_jstart2, jofpos(pos(2))))
           k = min(dg_kend2,max(dg_kstart2, kofpos(pos(3))))
           dg_ijk = dg_funijk(i,j,k)

           skipped = 0
           if(check_stl) then
              do while(pic_mi(bcv)%blocked(i,j,k) .or. &
                   picSTLoverlap(dg_ijk, pos, eff_rad))

                 skipped = skipped + 1
                 call random_number(rand)
                 pos(:) = offset + len*rand

                 i = min(dg_iend2,max(dg_istart2, iofpos(pos(1))))
                 j = min(dg_jend2,max(dg_jstart2, jofpos(pos(2))))
                 k = min(dg_kend2,max(dg_kstart2, kofpos(pos(3))))
                 dg_ijk = dg_funijk(i,j,k)

                 if(skipped >= 10 ) cycle pic_lp
              enddo
           endif
           pip = pip + 1
           max_pip = max_pip + 1

           call particle_grow(max_pip)

           des_pos_new(max_pip,:) = pos(:)

           des_vel_new(max_pip,1) = bc_u_s(bc_map,phase)
           des_vel_new(max_pip,2) = bc_v_s(bc_map,phase)
           des_vel_new(max_pip,3) = bc_w_s(bc_map,phase)

           ro_sol(max_pip)      = lro      ! density
           des_radius(max_pip)  = lrad     ! particle radius
           pvol(max_pip)        = lvol     ! particle volume
           pmass(max_pip)       = lmass    ! particle mass
           des_stat_wt(max_pip) = lstat_wt ! parcel weight

           ! If solving the energy equations, set the temperature
           if(any_species_eq .or. energy_eq ) &
                des_t_s(max_pip) = bc_t_s(bc_map,phase)

           ! Set species mass fractions
           if((energy_eq .and. c_ps0(phase)==undefined) .or. any_species_eq)&
                des_x_s(max_pip,1:lnmax) = bc_x_s(bc_map,phase,1:lnmax)

! Residence time
           RESIDENCE_TIME(max_pip) = ZERO

           ! Bin to the fluid grid
           call pic_search(k, pos(3), zt, dimension_k, kmin2, kmax2)
           call pic_search(j, pos(2), yn, dimension_j, jmin2, jmax2)
           call pic_search(i, pos(1), xe, dimension_i, imin2, imax2)

           pijk(max_pip,1) = i
           pijk(max_pip,2) = j
           pijk(max_pip,3) = k
           pijk(max_pip,4) = funijk(i,j,k)
           pijk(max_pip,5) = phase

           call set_normal(max_pip)

           seeded(myPE) = seeded(myPE) + 1

        enddo pic_lp

     end do
  end do

  call global_all_sum(seeded)

  gid_offset = 0
  gid_offset = sum(seeded(0:mype-1))

  do lc1 = 1, seeded(mype)
     iglobal_id(start_max_pip + lc1) = &
          imax_global_id + lc1 + gid_offset
  enddo

  imax_global_id = imax_global_id + sum(seeded)

  return
end subroutine mass_inflow_pic



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: DELETE_PARCEL                                           !
!                                                                      !
!  Purpose:  Routine to delete a parcel.                               !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
subroutine delete_parcel(np)

  use discretelement, only: iglobal_id

  use discretelement, only: des_pos_new
  use discretelement, only: des_vel_new
  use discretelement, only: ro_sol, des_radius
  use discretelement, only: pmass, pvol, fc
  use discretelement, only: pip

  use mfix_pic, only: des_stat_wt

  use functions, only: set_nonexistent
  USE param1, only: zero

  implicit none

  integer, intent(in) :: np

  iglobal_id(np) = -1

  des_pos_new(np,:) = zero
  des_vel_new(np,:) = zero

  des_radius(np) = zero
  pmass(np) = zero
  pvol(np) = zero
  ro_sol(np) = zero

  des_stat_wt(np) = zero

  fc(np,:) = zero

  pip = pip - 1

  call set_nonexistent(np)

  return
end subroutine delete_parcel

end module pic_flow_bcs
