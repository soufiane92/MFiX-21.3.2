module pic_wall_bcs

  public :: apply_wall_bc_pic

contains

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: APPLY_WALL_BC_PIC                                       !
!  Author: R. Garg                                                     !
!  Review: M. Clarke                                                   !
!                                                                      !
!  Purpose: Detect collisions between PIC particles and STLs.          !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
SUBROUTINE APPLY_WALL_BC_PIC

! Global Variables:
!---------------------------------------------------------------------//
! Particle position and velocity:
  use discretelement, only: DES_POS_NEW, DES_VEL_NEW
  use discretelement, only: iglobal_id
! Particle radius
  use discretelement, only: DES_RADIUS
  ! Max number of particles on this process
  use discretelement, only: MAX_PIP
! Map from particle to DES grid cell.
  use discretelement, only: PIJK
  use discretelement, only: XE, YN, ZT
! Flag indicating that the index cell contains no STLs
  use stl, only: FACETS_AT_DG
! STL Vertices
  use stl, only: VERTEX
! STL Facet normals
  use stl, only: NORM_FACE
  use mfix_pic, only: DES_STAT_WT
  USE mfix_pic, only : En => MPPIC_COEFF_EN_WALL
  USE mfix_pic, only : Et => MPPIC_COEFF_ET_WALL

! Global Parameters:
!---------------------------------------------------------------------//
  use functions, only: im_of,       jm_of,       km_of
  use functions, only: ip_of,       jp_of,       kp_of
  use desgrid,   only: dg_istart2,  dg_jstart2,  dg_kstart2
  use desgrid,   only: dg_iend2,    dg_jend2,    dg_kend2
  USE geometry,  only: x_min,       y_min,       z_min
  USE geometry,  only: x_max,       y_max,       z_max
  use geometry,  only: cyclic_x,    cyclic_y,    cyclic_z
  use geometry,  only: do_k

! Module Procedures:
!---------------------------------------------------------------------//
  use stl_functions_des, only: closestPtPointTriangle
  use stl_functions_des, only: intersectLnPlane
  use stl_functions_des, only: rayTriangle
  use pic_flow_bcs,      only: delete_parcel
  use desgrid,           only: iofpos, jofpos, kofpos
  use desgrid,           only: dg_funijk
  use functions,         only: fluid_at
  use functions,         only: is_normal

  use quadric,           only: cross_product

  use error_manager

  use stl_dbg_des, only: write_this_stl

  IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//
! Loop counters.
  integer :: facet, np, lc1, lc2, i, j, k, ijk
  integer :: dg_i, dg_j, dg_k, dg_ijk

  ! distances from parcel center to wall and parcel surface to wall
  double precision :: dist(3), dmag, closest_pt(3), Nw(3), Nw_dist
  ! parcel radius and position
  double precision :: rad, pos(3)
  ! parcel normal and tangential velocity components
  double precision :: Vpn(3), Vpt(3), Nw_Vp
  double precision :: Nwp(3), NwxNwp(3)
  ! Count and list of facets a parcel "collided" with
  integer :: collisions, prev_facets(10)

  real, parameter  :: third = 1.0/3.0
  logical :: failed
  logical :: point_or_edge_int

  logical :: any_cyclic
!......................................................................!

  any_cyclic = (cyclic_x .or. cyclic_y .or. cyclic_z)

  do np = 1, max_pip

     ! Skip non-existent particles
     if(is_normal(np)) then

        i   = pijk(np,1)
        j   = pijk(np,2)
        k   = pijk(np,3)
        ijk = pijk(np,4)

        rad = des_radius(np)*(des_stat_wt(np)**third)

        if(any_cyclic) then

           if(cyclic_x) then
              dmag = des_pos_new(np,1) - x_min
              if(dmag .lt. rad) cycle

              dmag = x_max - des_pos_new(np,1)
              if(dmag .lt. rad) cycle
           end if

           if(cyclic_y) then
              dmag = des_pos_new(np,2) - y_min
              if(dmag .lt. rad) cycle

              dmag = y_max - des_pos_new(np,2)
              if(dmag .lt. rad) cycle
           end if

           if(cyclic_z) then
              dmag = des_pos_new(np,3) - z_min
              if(dmag .lt. rad) cycle

              dmag = z_max - des_pos_new(np,3)
              if(dmag .lt. rad) cycle
           end if

        end if

        if(pic_outflow_at(ijk)) then
           call delete_parcel(np)
           cycle
        end if

        if(pic_outflow_at(im_of(ijk))) then
           dmag = des_pos_new(np,1) - xe(i-1)
           if(dmag .lt. rad) then
              if(dmag .lt. 0.5*rad) call delete_parcel(np)
              cycle
           endif
        end if

        if(pic_outflow_at(ip_of(ijk))) then
           dmag = xe(i) - des_pos_new(np,1)
           if(dmag .lt. rad) then
              if(dmag .lt. 0.5d0*rad) call delete_parcel(np)
              cycle
           endif
        end if

        if(pic_outflow_at(jm_of(ijk))) then
           dmag = des_pos_new(np,2) - yn(j-1)
           if(dmag .lt. rad) then
              if(dmag .lt. 0.5d0*rad) call delete_parcel(np)
              cycle
           endif
        end if

        if(pic_outflow_at(jp_of(ijk))) then
           dmag = yn(j) - des_pos_new(np,2)
           if(dmag .lt. rad) then
              if(dmag .lt. 0.5d0*rad) call delete_parcel(np)
              cycle
           endif
        end if

        if(do_k .and. pic_outflow_at(km_of(ijk))) then
           dmag = des_pos_new(np,3) - zt(k-1)
           if(dmag .lt. rad) then
              if(dmag .lt. 0.5d0*rad) call delete_parcel(np)
              cycle
           endif
        end if

        if(do_k .and. pic_outflow_at(kp_of(ijk))) then
           dmag = zt(k) - des_pos_new(np,3)
           if(dmag .lt. rad) then
              if(dmag .lt. 0.5d0*rad) call delete_parcel(np)
              cycle
           endif
        end if


        if(pic_wall_at(ijk)) then
           call relocate_parcle(np, rad, ijk, i, j, k, failed)
           if(failed) then
              call delete_parcel(np)
              cycle
           endif
        endif

        ! Calculate des grid cell containing particle centroid
        dg_i = min(dg_iend2, max(dg_istart2, iofpos(des_pos_new(np,1))))
        dg_j = min(dg_jend2, max(dg_jstart2, jofpos(des_pos_new(np,2))))
        dg_k = min(dg_kend2, max(dg_kstart2, kofpos(des_pos_new(np,3))))

        dg_ijk = dg_funijk(i,j,k)

        ! Loop through the STLs associated with the DES grid cell.
        collisions = 0
        facet_lp: do lc1=1, facets_at_dg(dg_ijk)%count

           ! Parcel location
           pos = des_pos_new(np,:)

           ! facet ID
           facet = facets_at_dg(dg_ijk)%id(lc1)

           ! Wall normal vector
           Nw = norm_face(:,facet)

           ! Test if the parcel and the facet are aligned
           if(.not.RayTriangle(pos, Nw, vertex(:,:,facet))) cycle

           ! Skip "collisions" with facets that are within 5 degrees of
           ! facets that have already "collided" with this parcel.
           do lc2=1, collisions

              ! previous facet ID
              Nwp = norm_face(:,prev_facets(lc2))

              NwxNwp(1) = Nw(2) * Nwp(3) - Nw(3) * Nwp(2)
              NwxNwp(2) = Nw(3) * Nwp(1) - Nw(1) * Nwp(3)
              NwxNwp(3) = Nw(1) * Nwp(2) - Nw(2) * Nwp(1)

              if(dot_product(NwxNwp, NwxNwp) < 0.01d0) cycle facet_lp
           end do

           call ClosestPtPointTriangle(pos, vertex(:,:,facet), closest_pt, point_or_edge_int)

           ! Distance vector between contact point and facet
           dist = closest_pt - pos

           Nw_dist = dot_product(dist,Nw)

           if(Nw_dist > -rad) then

              collisions = min(collisions+1,10)
              prev_facets(collisions) = facet

              dmag = max(epsilon(0.0d0), sqrt(dot_product(dist, dist)))

              if(Nw_dist < 0.0d0) then
                 des_pos_new(np,:) = pos + Nw_dist*Nw - rad*dist/dmag
              else
                 des_pos_new(np,:) = pos + Nw_dist*Nw + rad*dist/dmag
              endif

              ! Plane ref point
              Nw_Vp = dot_product(Nw, des_vel_new(np,:))

              ! Rebound parcel if moving towards wall.
              if(Nw_Vp < 0.0d0) then
                 ! Parcel velocity normal and tangential to wall
                 Vpn = Nw_Vp*Nw
                 Vpt = des_vel_new(np,:) - Vpn

                 ! Post collision velocity
                 des_vel_new(np,:) = -En*Vpn + Et*Vpt

                 ! Rebound parcel if moving towards wall.
              else
                 ! Parcel velocity normal and tangential to wall
                 Vpn = Nw_Vp*Nw
                 Vpt = des_vel_new(np,:) - Vpn

                 ! Post collision velocity
                 des_vel_new(np,:) = Vpn + Et*Vpt

              endif

           endif

        enddo facet_lp

        if(pic_wall_at(im_of(ijk))) then
           dmag = des_pos_new(np,1) - xe(i-1)
           if(dmag .lt. rad) then
              des_pos_new(np,1) = des_pos_new(np,1) + (rad - dmag)
              if(des_vel_new(np,1) < 0.0d0) des_vel_new(np,1) = -des_vel_new(np,1)
           endif
        end if

        if(pic_wall_at(ip_of(ijk))) then
           dmag = xe(i) - des_pos_new(np,1)
           if(dmag .lt. rad) then
              des_pos_new(np,1) = des_pos_new(np,1) - (rad - dmag)
              if(des_vel_new(np,1) > 0.0d0) des_vel_new(np,1) = -des_vel_new(np,1)
           endif
        end if

        if(pic_wall_at(jm_of(ijk))) then
           dmag = des_pos_new(np,2) - yn(j-1)
           if(dmag .lt. rad) then
              des_pos_new(np,2) = des_pos_new(np,2) + (rad - dmag)
              if(des_vel_new(np,2) < 0.0d0) des_vel_new(np,2) = -des_vel_new(np,2)
           endif
        end if

        if(pic_wall_at(jp_of(ijk))) then
           dmag = yn(j) - des_pos_new(np,2)
           if(dmag .lt. rad) then
              des_pos_new(np,2) = des_pos_new(np,2) - (rad - dmag)
              if(des_vel_new(np,2) > 0.0d0) des_vel_new(np,2) = -des_vel_new(np,2)
           endif
        end if

        if(do_k .and. pic_wall_at(km_of(ijk))) then
           dmag = des_pos_new(np,3) - zt(k-1)
           if(dmag .lt. rad) then
              des_pos_new(np,3) = des_pos_new(np,3) + (rad - dmag)
              if(des_vel_new(np,3) > 0.0d0) des_vel_new(np,3) = -des_vel_new(np,3)
           endif
        end if

        if(do_k .and. pic_wall_at(kp_of(ijk))) then
           dmag = zt(k) - des_pos_new(np,3)
           if(dmag .lt. rad) then
              des_pos_new(np,3) = des_pos_new(np,3) - (rad - dmag)
              if(des_vel_new(np,3) < 0.0d0) des_vel_new(np,3) = -des_vel_new(np,3)
           endif
        end if

     endif

  end do

end subroutine apply_wall_bc_pic

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
logical function pic_wall_at(lijk)
  use geometry, only: flag
  integer, intent(in) :: lijk
  integer :: lflag

  lflag = flag(lijk)

  pic_wall_at = lflag ==  10 .or. & ! pressure inflow
                lflag ==  20 .or. & ! mass inflow
                lflag == 100 .or. & ! no slip wall
                lflag == 101 .or. & ! free slip wall
                lflag == 102        ! partial slip wall

end function pic_wall_at


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
logical function pic_outflow_at(lijk)
  use geometry, only: flag
  integer, intent(in) :: lijk
  integer :: lflag

  lflag = flag(lijk)
  pic_outflow_at = lflag ==  11 .or. & ! pressure outflow
                   lflag ==  21 .or. & ! mass outflow
                   lflag ==  31        ! outflow
end function pic_outflow_at


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
subroutine relocate_parcle(np, radius, ijk, i, j, k, failed)

! Global Variables:
!---------------------------------------------------------------------//
  use geometry,       only: no_k

  use discretelement, only: des_pos_new
  use discretelement, only: iglobal_id

! Fluid grid functions and look-ups
!---------------------------------------------------------------------//
  use functions,      only: fluid_at
  use indices,        only: i_of,   j_of,   k_of
  use functions,      only: im_of,  jm_of,  km_of
  use functions,      only: ip_of,  jp_of,  kp_of
  use discretelement, only: xe,     yn,     zt

! MPI domain bounds:
  use desgrid,        only: dg_xstart, dg_ystart, dg_zstart
  use desgrid,        only: dg_xend,   dg_yend,   dg_zend

  implicit none

  integer,          intent(in   ) :: np
  double precision, intent(in   ) :: radius

  integer,          intent(inout) :: ijk, i, j, k

  logical,          intent(  out) :: failed

  integer :: test_ijk, lc

  double precision :: mv, x, y, z, minval, lpos(3)

  lpos = des_pos_new(np,:)
  failed = .false.

  x = 1.0d20
  y = 1.0d20
  z = 1.0d20

  test_ijk = ip_of(ijk)
  if(fluid_at(test_ijk)) then
     ijk = test_ijk
     i = i_of(ijk)
     mv = xe(i) + radius
     if(mv <= dg_xend) x = mv
  endif

  test_ijk = jp_of(ijk)
  if(fluid_at(test_ijk)) then
     ijk = test_ijk
     j = j_of(ijk)
     mv = yn(j) + radius
     if(mv <= dg_yend) y = mv
  endif

  test_ijk = im_of(ijk)
  if(fluid_at(test_ijk)) then
     ijk = test_ijk
     i = i_of(ijk)
     mv = xe(i) - radius
     if(mv >= dg_xstart) x = min(x, mv)
  endif

  test_ijk = jm_of(ijk)
  if(fluid_at(test_ijk)) then
     ijk = test_ijk
     j = j_of(ijk)
     mv = yn(j) - radius
     if(mv >= dg_ystart) y = min(y, mv)
  endif


  lc=-1
  minval = 1.0d20

  ! Find the minimum distance need to move the particle in
  ! x and y to get into a valid fluid cell.

  if(x /= 1.0d20 .or. y /= 1.0d20) then
     if(x < y) then
        lc=1
        minval = x
     else
        lc=2
        minval = y
     endif
  endif

  ! No additional work for 2D setups. If no direction gets you into a
  ! valid cell (lc=3), then return that relocating the particle failed.

  if(no_k) then
     if(lc==3) then
        failed = .true.
     else
        failed = .false.
        des_pos_new(np,lc) = minval
     endif
     return
  endif

  ! Check minimum distance in z

  test_ijk = kp_of(ijk)
  if(fluid_at(test_ijk)) then
     ijk = test_ijk
     k = k_of(ijk)
     mv = zt(k) + radius
     if(mv <= dg_zend) z = mv
  endif

  test_ijk = km_of(ijk)
  if(fluid_at(test_ijk)) then
     ijk = test_ijk
     k = k_of(ijk)
     mv = zt(k) - radius
     if(mv >= dg_zstart) z = min(z, mv)
  endif

  if(z == 1.0d20) then
     if(lc==-1) then
        failed = .true.
     else
        failed = .false.
        des_pos_new(np,lc) = minval
     endif
  else
     if(z < minval) then
        lc = 3
        minval = z
     endif
     failed = .false.
     des_pos_new(np,lc) = minval
  endif

  if(.false.) then
     if(.not.failed) then
        write(*,*) "repositioned parcel ", iglobal_id(np)
        write(*,*) "   original pos:",lpos
        write(*,*) "        new pos:",des_pos_new(np,:)
        write(*,*) "          tests:",x,y,z
     else
        write(*,*) "repositioned parcel ", iglobal_id(np),"failed :("
     endif
  endif

  return
end subroutine relocate_parcle






!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
SUBROUTINE write_this_facet_and_parcel(FID, position, velocity)

  USE run
  USE param1
  USE discretelement
  USE geometry
  USE compar
  USE constant
  USE cutcell
  USE funits
  USE indices
  USE physprop
  USE parallel
  USE stl
  USE stl_functions_des
  Implicit none
  !facet id and particle id
  double precision, intent(in), dimension(dimn) :: position, velocity
  Integer, intent(in) :: fid
  Integer :: stl_unit, vtp_unit , k
  CHARACTER(LEN=100) :: stl_fname, vtp_fname
  real :: temp_array(3)

  stl_unit = 1001
  vtp_unit = 1002

  WRITE(vtp_fname,'(A,"_OFFENDING_PARTICLE",".vtp")') TRIM(RUN_NAME)
  WRITE(stl_fname,'(A,"_STL_FACE",".stl")') TRIM(RUN_NAME)

  open(vtp_unit, file = vtp_fname, form='formatted')
  open(stl_unit, file = stl_fname, form='formatted')

  write(vtp_unit,"(a)") '<?xml version="1.0"?>'
  write(vtp_unit,"(a,es24.16,a)") '<!-- time =',s_time,'s -->'
  write(vtp_unit,"(a,a)") '<VTKFile type="PolyData"',&
       ' version="0.1" byte_order="LittleEndian">'
  write(vtp_unit,"(3x,a)") '<PolyData>'
  write(vtp_unit,"(6x,a,i10.10,a,a)")&
       '<Piece NumberOfPoints="',1,'" NumberOfVerts="0" ',&
       'NumberOfLines="0" NumberOfStrips="0" NumberOfPolys="0">'
  write(vtp_unit,"(9x,a)")&
       '<PointData Scalars="Diameter" Vectors="Velocity">'
  write(vtp_unit,"(12x,a)")&
       '<DataArray type="Float32" Name="Diameter" format="ascii">'
  write (vtp_unit,"(15x,es13.6)") (1.d0)
  write(vtp_unit,"(12x,a)") '</DataArray>'

  temp_array = zero
  temp_array(1:DIMN) = real(velocity(1:dimn))
  write(vtp_unit,"(12x,a,a)") '<DataArray type="Float32" ',&
       'Name="Velocity" NumberOfComponents="3" format="ascii">'
  write (vtp_unit,"(15x,3(es13.6,3x))")&
       ((temp_array(k)),k=1,3)
  write(vtp_unit,"(12x,a,/9x,a)") '</DataArray>','</PointData>'
  ! skip cell data
  write(vtp_unit,"(9x,a)") '<CellData></CellData>'

  temp_array = zero
  temp_array(1:dimn) = real(position(1:dimn))
  write(vtp_unit,"(9x,a)") '<Points>'
  write(vtp_unit,"(12x,a,a)") '<DataArray type="Float32" ',&
       'Name="Position" NumberOfComponents="3" format="ascii">'
  write (vtp_unit,"(15x,3(es13.6,3x))")&
       ((temp_array(k)),k=1,3)
  write(vtp_unit,"(12x,a,/9x,a)")'</DataArray>','</Points>'
  ! Write tags for data not included (vtp format style)
  write(vtp_unit,"(9x,a,/9x,a,/9x,a,/9x,a)")'<Verts></Verts>',&
       '<Lines></Lines>','<Strips></Strips>','<Polys></Polys>'
  write(vtp_unit,"(6x,a,/3x,a,/a)")&
       '</Piece>','</PolyData>','</VTKFile>'

  !Now write the facet info

  write(stl_unit,*)'solid vcg'

  write(stl_unit,*) '   facet normal ', NORM_FACE(1:3,FID)
  write(stl_unit,*) '      outer loop'
  write(stl_unit,*) '         vertex ', VERTEX(1,1:3,FID)
  write(stl_unit,*) '         vertex ', VERTEX(2,1:3,FID)
  write(stl_unit,*) '         vertex ', VERTEX(3,1:3,FID)
  write(stl_unit,*) '      endloop'
  write(stl_unit,*) '   endfacet'

  write(stl_unit,*)'endsolid vcg'

  close(vtp_unit, status = 'keep')
  close(stl_unit, status = 'keep')
  write(*,*) 'wrote a facet and a parcel. now waiting'
  read(*,*)
  end SUBROUTINE write_this_facet_and_parcel



  SUBROUTINE OUT_THIS_STL(LC)

    Use usr

    ! STL Vertices
    use stl, only: VERTEX
    ! STL Facet normals
    use stl, only: NORM_FACE


    IMPLICIT NONE
    !-----------------------------------------------
    integer, INTENT(IN) :: lc

    logical :: lExists
    character(len=8) :: IDX


    write(idx,"(I8.8)") LC
    inquire(file='geo_'//idx//'.stl', EXIST=lExists)

    if(lexists) return

    open(unit=555, file='geo_'//idx//'.stl', status='UNKNOWN')
    write(555,*) 'solid vcg'
    write(555,*) '   facet normal ', norm_face(:,lc)
    write(555,*) '      outer loop'
    write(555,*) '         vertex ', vertex(1,1:3,lc)
    write(555,*) '         vertex ', vertex(2,1:3,lc)
    write(555,*) '         vertex ', vertex(3,1:3,lc)
    write(555,*) '      endloop'
    write(555,*) '   endfacet'
    close(555)

    RETURN
  END SUBROUTINE OUT_THIS_STL

end module pic_wall_bcs
