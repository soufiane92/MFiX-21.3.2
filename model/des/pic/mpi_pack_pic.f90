!----------------------------------------------------------------------!
!  Module: MPI_PACK_DES                                                !
!  Author: Pradeep Gopalakrishnan                                      !
!                                                                      !
!  Purpose: Contains routines for packing real and ghost particles     !
!     into the MPI send buffers.                                       !
!----------------------------------------------------------------------!
module mpi_pic

  private
  public :: mpi_pic_pack
  public :: mpi_pic_unpack

contains

!----------------------------------------------------------------------!
!  Subroutine: MPI_PACK_PIC                                            !
!  Author: Pradeep Gopalakrishnan                                      !
!                                                                      !
! Purpose: Packs real particle in the send buffer.                     !
!----------------------------------------------------------------------!
  subroutine mpi_pic_pack(pface, pcurpar, pbuf)

! Global Variables:
!---------------------------------------------------------------------//
! Runtime flag for solving the energy equations
    use run, only: ENERGY_EQ
! Runtime flag for solving species equations
    use run, only: ANY_SPECIES_EQ
! The statistical weight of each particle.
    use mfix_pic, only: DES_STAT_WT
! The global ID for each particle
    use discretelement, only: iGLOBAL_ID
! Particle positions: current/previous
    use discretelement, only: DES_POS_NEW
! Particle tangential velocities: current/previous
    use discretelement, only: DES_VEL_NEW
! Particle radius, volume, density, mass
    use discretelement, only: DES_RADIUS, PVOL, RO_SOL, PMASS
! Particle species composition
    use des_rxns, only: DES_X_s
! Particle temperatures. current/previous
    use des_thermo, only: DES_T_s
! Map to fluid grid cells and solids phase (I,J,K,IJK,M)
    use discretelement, only: PIJK
! User-defined variables for each particle.
    use discretelement, only: DES_USR_VAR, DES_USR_VAR_SIZE
! Flag indicating the the fluid-particle drag is explicitly coupled.
    use discretelement, only: des_explicitly_coupled
! Explicit particle drag force
    use discretelement, only: drag_fc
! Explicit convection and HOR
    use des_thermo, only: conv_qs, rxns_qs
! Number of particles on the process (max particle array size)
    use discretelement, only: pip
! Residence time
    use discretelement, only: RESIDENCE_TIME
! The neighbor processor's rank
    use desmpi, only: iNEIGHPROC

    use desgrid, only: icycoffset

    use functions, only: funijk_proc
    use functions, only: set_nonexistent

    use desmpi, only: dcycl_offset

    use mpi_pack_des, only: pack_dbuf

    use vtk

    implicit none

! Dummy arguments:
!---------------------------------------------------------------------//
! Processor boundary being packed (Top/Bottom/North/South/East/West)
    integer, intent(in   ) :: pface, pcurpar
    integer, intent(inout) :: pbuf

! Local variables
!---------------------------------------------------------------------//
    integer :: li, lj, lk

!......................................................................!

!  1) Global ID
    call pack_dbuf(pbuf,iglobal_id(pcurpar),pface)
!  2) Radius
    call pack_dbuf(pbuf,des_radius(pcurpar),pface)
!  3) Fluid cell I index with cycle offset
    li = pijk(pcurpar,1) + icycoffset(pface,1)
    call pack_dbuf(pbuf,li,pface)
!  4) Fluid cell J index with cycle offset
    lj = pijk(pcurpar,2) + icycoffset(pface,2)
    call pack_dbuf(pbuf,lj,pface)
!  5) Fluid cell K index with cycle offset
    lk = pijk(pcurpar,3) + icycoffset(pface,3)
    call pack_dbuf(pbuf,lk,pface)
!  6) Fluid cell IJK on destination process
    call pack_dbuf(pbuf,funijk_proc(li,lj,lk,                  &
         ineighproc(pface)),pface)
!  7) Particle solids phase index
    call pack_dbuf(pbuf,pijk(pcurpar,5),pface)
!  8) Density
    call pack_dbuf(pbuf,ro_sol(pcurpar),pface)
!  9) Volume
    call pack_dbuf(pbuf,pvol(pcurpar),pface)
! 10) Mass
    call pack_dbuf(pbuf,pmass(pcurpar),pface)
! 11) Position with cyclic shift
    call pack_dbuf(pbuf,des_pos_new(pcurpar,:) +               &
         dcycl_offset(pface,:),pface)
! 12) Translational velocity
    call pack_dbuf(pbuf,des_vel_new(pcurpar,:),pface)

    if(energy_eq) then
! 13) Temperature
       call pack_dbuf(pbuf,des_t_s(pcurpar),pface)
! 14) Species composition
       call pack_dbuf(pbuf,des_x_s(pcurpar,:),pface)
    endif

! 15) User defined variable
    if(des_usr_var_size > 0) &
         call pack_dbuf(pbuf, des_usr_var(:,pcurpar),pface)

    if(des_explicitly_coupled) then
! 16) Explicit drag force
       call pack_dbuf(pbuf, drag_fc(pcurpar,:),pface)
! 17) Explicit convective heat transfer
       if(energy_eq) call pack_dbuf(pbuf,conv_qs(pcurpar),pface)
! 18) Explicit heat of reaction
       if(any_species_eq) call pack_dbuf(pbuf, rxns_qs(pcurpar),pface)
    endif

! 19) Statistical weight
    call pack_dbuf(pbuf,des_stat_wt(pcurpar),pface)

! 20) Reaction rates
         IF(SAVE_PART_RRATES)  call pack_dbuf(pbuf,Part_RRates_out(pcurpar,:),pface)

! 21) Residence time
         call pack_dbuf(pbuf,RESIDENCE_TIME(pcurpar),pface)

    call set_nonexistent(pcurpar)
    pip = pip - 1

    return
  end subroutine mpi_pic_pack

!----------------------------------------------------------------------!
!  Subroutine: DESMPI_UNPACK_PARCROSS                                  !
!  Author: Pradeep Gopalakrishnan                                      !
!                                                                      !
! Purpose: Unpacks real particle from the recv buffer.                 !
!----------------------------------------------------------------------!
  subroutine mpi_pic_unpack(pface)

! Global Variables:
!---------------------------------------------------------------------//
! Size of ghost particle data packet
    use desmpi, only: iParticlePacketSize
! Index of last particle added to this process.
    use desmpi, only: iSPOT
! The MPI receive buffer
    use desmpi, only: dRECVBUF
! Buffer offset
    use desmpi, only: iBUFOFFSET
! Runtime flag for solving the energy equations
    use run, only: ENERGY_EQ
! Runtime flag for solving species equations
    use run, only: ANY_SPECIES_EQ
! The statistical weight of each particle.
    use mfix_pic, only: DES_STAT_WT
! The global ID for each particle
    use discretelement, only: iGLOBAL_ID
! Particle positions: current/previous
    use discretelement, only: DES_POS_NEW
! Particle tangential velocities: current/previous
    use discretelement, only: DES_VEL_NEW
! Particle radius, volume, density, mass
    use discretelement, only: DES_RADIUS, PVOL, RO_SOL, PMASS
! Particle species composition
    use des_rxns, only: DES_X_s
! Particle temperatures.
    use des_thermo, only: DES_T_s
! Map to fluid grid cells and solids phase (I,J,K,IJK,M)
    use discretelement, only: PIJK
! Number of particles on the process (max particle array size)
    use discretelement, only: PIP
! Flag indicating the the fluid-particle drag is explicitly coupled.
    use discretelement, only: DES_EXPLICITLY_COUPLED
! Explicit fluid-particle drag force
    use discretelement, only: DRAG_FC
! Explicit convection and HOR
    use des_thermo, only: CONV_Qs, RXNS_Qs
! User-defined variables for each particle.
    use discretelement, only: DES_USR_VAR, DES_USR_VAR_SIZE

    use discretelement, only: particle_state

! Residence time
    use discretelement, only: RESIDENCE_TIME
    use vtk

! Module Procedures:
!---------------------------------------------------------------------//
    use des_allocate
    use desmpi_wrapper, only: DES_MPI_STOP
    use discretelement, only: max_pip
    use functions, only: SET_NORMAL
    use mpi_unpack_des, only: unpack_dbuf

    implicit none

! Dummy arguments:
!---------------------------------------------------------------------//
! Processor boundary being packed (Top/Bottom/North/South/East/West)
    INTEGER, INTENT(IN) :: PFACE

! Local variables
!---------------------------------------------------------------------//
    integer :: lcurpar,lparcnt,llocpar
    integer :: lbuf
!......................................................................!

! loop through particles and locate them and make changes
    lparcnt = drecvbuf(1+mod(pface,2))%facebuf(1)

! if mppic make sure enough space available. The 1000 is added because
! ispot is only reset at the start of the send/recv.
    call particle_grow(pip + lparcnt + 1000)

    do lcurpar =1,lparcnt

       lbuf = (lcurpar-1)*iParticlePacketSize + ibufoffset

! PIC particles are always 'new' to the receiving process. Find the
! first available array position. Increment the PIP counter to include
! the new particle.
       do while(particle_state(ispot) == 1)
          ispot = ispot + 1
       enddo
       llocpar = ispot

       pip = pip + 1
       max_pip = max(max_pip, llocpar)

       call set_normal(llocpar)

! 1) Global ID
       call unpack_dbuf(lbuf, iglobal_id(llocpar), pface)
! 2) Radius
       call unpack_dbuf(lbuf,des_radius(llocpar),pface)
! 3-7) Fluid cell I,J,K,IJK indices and solids phase index
       call unpack_dbuf(lbuf,pijk(llocpar,:),pface)
!  8) Density
       call unpack_dbuf(lbuf,ro_sol(llocpar),pface)
!  9) Volume
       call unpack_dbuf(lbuf,pvol(llocpar),pface)
! 10) Mass
       call unpack_dbuf(lbuf,pmass(llocpar),pface)
! 11) Position with cyclic shift
       call unpack_dbuf(lbuf,des_pos_new(llocpar,:),pface)
! 12) Translational velocity
       call unpack_dbuf(lbuf,des_vel_new(llocpar,:),pface)

       if(energy_eq) then
! 13) Temperature
          call unpack_dbuf(lbuf,des_t_s(llocpar),pface)
! 14) Species composition
          call unpack_dbuf(lbuf,des_x_s(llocpar,:),pface)
       endif

! 15) User defined variable
       IF(DES_USR_VAR_SIZE > 0) &
            call unpack_dbuf(lbuf,des_usr_var(:,llocpar),pface)

       if(des_explicitly_coupled) then
! 16) Explicit drag force
          call unpack_dbuf(lbuf,drag_fc(llocpar,:),pface)
! 17) Explicit convective heat transfer
          if(energy_eq) call unpack_dbuf(lbuf,conv_qs(llocpar),pface)
! 18) Explicit heat of reaction
          if(any_species_eq) call unpack_dbuf(lbuf,rxns_qs(llocpar),pface)
       endif

! 19) Statistical weight
       call unpack_dbuf(lbuf,des_stat_wt(llocpar),pface)

! 20) Reaction rates
         IF(SAVE_PART_RRATES)  call unpack_dbuf(lbuf,Part_RRates_out(llocpar,:),pface)

! 21) Residence time
         call unpack_dbuf(lbuf,RESIDENCE_TIME(llocpar),pface)
    end do

  end subroutine mpi_pic_unpack

end module mpi_pic
