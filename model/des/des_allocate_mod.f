!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: DES_ALLOCATE                                           C
!                                                                      C
!  Purpose: subroutines to allocate all DEM arrays                     C
!                                                                      C
!  Author: Rahul Garg                               Date: 1-Dec-2013   C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

MODULE DES_ALLOCATE

   USE des_init_arrays_mod, only: des_init_arrays, des_init_particle_arrays

   PUBLIC:: DES_ALLOCATE_ARRAYS, ADD_PAIR, PARTICLE_GROW, ALLOCATE_DEM_MI

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: DES_ALLOCATE_ARRAYS                                     C
!  Purpose: Original allocate arrays subroutines for DES               C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE DES_ALLOCATE_ARRAYS

      USE compar
      USE constant
      USE cutcell
      USE des_psd
      USE derived_types, only: pic
      USE des_bc
      USE des_rxns
      USE des_thermo
      USE des_thermo_cond, only: DES_Qw_cond
      USE discretelement
      USE functions
      USE funits
      USE geometry
      USE indices
      USE mfix_pic, only: mppic, des_stat_wt, ps_grad, epg_p
      USE mfix_pic, only: pic_bcmi
      USE param
      USE param1
      USE particle_filter, only: DES_INTERP_DPVM
      USE particle_filter, only: DES_INTERP_GARG
      USE particle_filter, only: DES_INTERP_GAUSS
      USE particle_filter, only: DES_INTERP_LHAT
      USE particle_filter, only: DES_INTERP_SCHEME_ENUM
      USE particle_filter, only: FILTER_CELL
      USE particle_filter, only: FILTER_SIZE
      USE particle_filter, only: FILTER_WEIGHT
      USE physprop
      USE run, only: ENERGY_EQ, ANY_SPECIES_EQ
      USE VTK, ONLY: SAVE_PART_RRATES, PART_RRATES_OUT

! Use the error manager for posting error messages.
!---------------------------------------------------------------------//
      USE error_manager

      IMPLICIT NONE
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! indices
      INTEGER :: IJK
!-----------------------------------------------

! For parallel processing the array size required should be either
! specified by the user or could be determined from total particles
! with some factor.
      MAX_PIP = merge(0, PARTICLES/numPEs, PARTICLES==UNDEFINED_I)
      MAX_PIP = MAX(MAX_PIP,4)

      WRITE(ERR_MSG,1000) trim(iVal(MAX_PIP))
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

 1000 FORMAT('Initial DES Particle array size: ',A)

! DES Allocatable arrays
!-----------------------------------------------
! Dynamic particle info including another index for parallel
! processing for ghost
      ALLOCATE( PARTICLE_STATE (MAX_PIP) )
      ALLOCATE (iglobal_id(max_pip))

! R.Garg: Allocate necessary arrays for PIC mass inlet/outlet BCs
      IF(PIC_BCMI /= 0) CALL ALLOCATE_PIC_MI

! Particle attributes
! Radius, density, mass, moment of inertia
      Allocate(  DES_RADIUS (MAX_PIP) )
      Allocate(  RO_Sol (MAX_PIP) )
      Allocate(  PVOL (MAX_PIP) )
      Allocate(  PMASS (MAX_PIP) )
      Allocate(  OMOI (MAX_PIP) )

! Old and new particle positions, velocities (translational and
! rotational)
      Allocate(  DES_POS_NEW (MAX_PIP,DIMN) )
      Allocate(  DES_VEL_NEW (MAX_PIP,DIMN) )
      Allocate(  OMEGA_NEW (MAX_PIP,DIMN) )
      call create_custom_datatype

! Force chain arrays      
! Arrays are initially allocated with 1000 elements.
! Arrays are grown when needed in calc_force_dem.f
      Allocate(FCHAIN_MIDPOINT (1000,DIMN) )
      FCHAIN_MIDPOINT(:,:) = UNDEFINED
      Allocate(FCHAIN_FN (1000,DIMN) )
      Allocate(FCHAIN_LENGTH (1000))
      Allocate(FCHAIN_FN_MAG (1000))

! Residence time
      Allocate(RESIDENCE_TIME (MAX_PIP))
      RESIDENCE_TIME(:) = ZERO

      IF(PARTICLE_ORIENTATION) Allocate(  ORIENTATION (MAX_PIP,DIMN) )

      IF (DO_OLD) THEN
         Allocate(  DES_POS_OLD (MAX_PIP,DIMN) )
         Allocate(  DES_VEL_OLD (MAX_PIP,DIMN) )
         Allocate(  DES_ACC_OLD (MAX_PIP,DIMN) )
         Allocate(  OMEGA_OLD (MAX_PIP,DIMN) )
         Allocate(  ROT_ACC_OLD (MAX_PIP,DIMN))
      ENDIF

! Allocating user defined array
      IF(DES_USR_VAR_SIZE > 0) &
         Allocate( DES_USR_VAR(DES_USR_VAR_SIZE,MAX_PIP) )

! Particle positions at the last call neighbor search algorithm call
      Allocate(  PPOS (MAX_PIP,DIMN) )

! Total, normal and tangential forces
      Allocate(  FC (MAX_PIP,DIMN) )

! Torque
      Allocate(  TOW (MAX_PIP,DIMN) )


! allocate variable for des grid binning
      allocate(dg_pijk(max_pip)); dg_pijk=0
      allocate(dg_pijkprv(max_pip)); dg_pijkprv=0

! allocate variables related to ghost particles
      allocate(ighost_updated(max_pip))



      Allocate(  wall_collision_facet_id (COLLISION_ARRAY_MAX, MAX_PIP) )
      wall_collision_facet_id(:,:) = -1
      Allocate(  wall_collision_PFT (DIMN, COLLISION_ARRAY_MAX, MAX_PIP) )

! Temporary variables to store wall position, velocity and normal vector
      Allocate(  WALL_NORMAL  (NWALLS,DIMN) )

      Allocate(  NEIGHBOR_INDEX (MAX_PIP) )
      Allocate(  NEIGHBOR_INDEX_OLD (MAX_PIP) )
      Allocate(  NEIGHBORS (MAX_PIP) )
      NEIGHBORS(:) = 0

      Allocate(  NEIGHBORS_OLD (MAX_PIP) )
      Allocate(  PFT_NEIGHBOR (3,MAX_PIP) )
      Allocate(  PFT_NEIGHBOR_OLD (3,MAX_PIP) )

! Variable that stores the particle in cell information (ID) on the
! computational fluid grid defined by keywords IMAX, JMAX and KMAX.
      ALLOCATE(PIC(DIMENSION_3))
      DO IJK=1,DIMENSION_3
        NULLIFY(pic(ijk)%p)
      ENDDO

! Particles in a computational fluid cell (for volume fraction)
      Allocate(  PINC (DIMENSION_3) )

! Variables for masking and sorting particles
!Hari Sitaraman===============
      if(allocated(valid_fluid_indices)) deallocate(valid_fluid_indices)
      if(allocated(vol_surr_inv)) deallocate(vol_surr_inv)
      if(allocated(fluid_at_mask1)) deallocate(fluid_at_mask1)
      if(allocated(fluid_at_mask2)) deallocate(fluid_at_mask2)
      if(allocated(fluid_at_mask3)) deallocate(fluid_at_mask3)

      Allocate(valid_fluid_indices(DIMENSION_3))
      Allocate(vol_surr_inv(DIMENSION_3))
      Allocate(fluid_at_mask1(DIMENSION_3))
      Allocate(fluid_at_mask2(IEND2-ISTART2+1,JEND2-JSTART2+1,KEND2-KSTART2+1))
      Allocate(fluid_at_mask3(DIMENSION_3))
!============================

! Ghost Particles in a computational fluid cell (for volume fraction)
      Allocate(  GPINC (DIMENSION_3) )

! For each particle track its i,j,k location on computational fluid grid
! defined by keywords IMAX, JMAX and KMAX.
      Allocate(  PIJK (MAX_PIP,5) )

      ALLOCATE(DRAG_AM(DIMENSION_3))
      ALLOCATE(DRAG_BM(DIMENSION_3, DIMN))
      ALLOCATE(F_gp(MAX_PIP ))
      F_gp(1:MAX_PIP)  = ZERO

! Explicit drag force acting on a particle.
      Allocate(DRAG_FC (MAX_PIP,DIMN) )

! force due to gas-pressure gradient
      ALLOCATE(P_FORCE(DIMN, DIMENSION_3))

! Volume of nodes
      ALLOCATE(DES_VOL_NODE(DIMENSION_3))

      ALLOCATE(F_GDS(DIMENSION_3))
      ALLOCATE(VXF_GDS(DIMENSION_3))

      SELECT CASE(DES_INTERP_SCHEME_ENUM)
      CASE(DES_INTERP_DPVM, DES_INTERP_GAUSS, DES_INTERP_LHAT)
         ALLOCATE(FILTER_CELL(FILTER_SIZE, MAX_PIP))
         ALLOCATE(FILTER_WEIGHT(FILTER_SIZE, MAX_PIP))
      CASE(DES_INTERP_GARG)
         ALLOCATE(DES_ROPS_NODE(DIMENSION_3, DIMENSION_M))
         ALLOCATE(DES_VEL_NODE(DIMENSION_3, DIMN, DIMENSION_M))
      END SELECT

! Variables for hybrid model
      IF (DES_CONTINUUM_HYBRID) THEN
         ALLOCATE(SDRAG_AM(DIMENSION_3,DIMENSION_M))
         ALLOCATE(SDRAG_BM(DIMENSION_3, DIMN,DIMENSION_M))

         ALLOCATE(F_SDS(DIMENSION_3,DIMENSION_M))
         ALLOCATE(VXF_SDS(DIMENSION_3,DIMENSION_M))
      ENDIF

! coarse grained DEM
      IF (CGDEM) Allocate(  DES_CGP_STW (MAX_PIP) )
      IF (CGDEM) Allocate(  DES_CGP_RPR (MAX_PIP) )

! MP-PIC related
      IF(MPPIC) THEN
         ALLOCATE(DES_STAT_WT(MAX_PIP))
         ALLOCATE(DES_VEL_MAX(DIMN))
         ALLOCATE(PS_GRAD(3,MAX_PIP))
         ALLOCATE(EPG_P(MAX_PIP))
      ENDIF

! Averaged velocity obtained by averaging over all the particles
      ALLOCATE(DES_VEL_AVG(DIMN) )

! Global Granular Energy
      ALLOCATE(GLOBAL_GRAN_ENERGY(DIMN) )
      ALLOCATE(GLOBAL_GRAN_TEMP(DIMN) )

! variable for bed height of solids phase M
      ALLOCATE(BED_HEIGHT(DIMENSION_M))

! ---------------------------------------------------------------->>>
! BEGIN COHESION
      IF(USE_COHESION) THEN
! Matrix location of particle  (should be allocated in case user wishes
! to invoke routines in /cohesion subdirectory
         Allocate(  PostCohesive (MAX_PIP) )
      ENDIF
! END COHESION
! ----------------------------------------------------------------<<<

! ---------------------------------------------------------------->>>
! BEGIN Thermodynamic Allocation
      IF(ENERGY_EQ)THEN
! Particle temperature
         Allocate( DES_T_s( MAX_PIP ) )
! Species mass fractions comprising a particle. This array may not be
! needed for all thermo problems.
         Allocate( DES_X_s( MAX_PIP, DIMENSION_N_S))
! Specific heat
         Allocate( DES_C_PS( MAX_PIP ) )
! Total rate of heat transfer to individual particles.
         Allocate( Q_Source( MAX_PIP ) )
! Average solids temperature in fluid cell
         Allocate(avgDES_T_s(DIMENSION_3) )
! Gas/Solids convective heat transfer coupling
         IF(CALC_CONV_DES) THEN
! Fluid phase energy equation source terms
            Allocate(CONV_Sc(DIMENSION_3) )
            Allocate(CONV_Sp(DIMENSION_3) )
! Particle convection source term (explicit coupled)
            Allocate(CONV_Qs(MAX_PIP))
! Gas-particle heat transfer coefficient TIMES surface area
            Allocate(GAMMAxSA(MAX_PIP))
         ENDIF

! Allocate the history variables for Adams-Bashforth integration
         IF (INTG_ADAMS_BASHFORTH) &
            Allocate( Q_Source0( MAX_PIP ) )
! Allocate the array for storing particle-wall heat transfer per unit area
         IF (ANY(CALC_COND_DES)) &
            Allocate( DES_Qw_cond( DIMENSION_3, DIMENSION_M))
      ENDIF
! End Thermodynamic Allocation
! ----------------------------------------------------------------<<<


! ---------------------------------------------------------------->>>
! BEGIN Species Allocation
      IF(ANY_SPECIES_EQ)THEN
! Species mass fractions comprising a particle. This array may not be
! needed for all thermo problems.
         if(.not.allocated(DES_X_s)) Allocate( DES_X_s( MAX_PIP, DIMENSION_N_S))
! Rate of solids phase production/consumption for each species
         Allocate( DES_R_s( MAX_PIP, DIMENSION_N_s) )


         Allocate( DES_R_gp( DIMENSION_3, DIMENSION_N_g ) )
         Allocate( DES_R_gc( DIMENSION_3, DIMENSION_N_g ) )
         Allocate( DES_SUM_R_g( DIMENSION_3 ) )
         Allocate( DES_R_PHASE( DIMENSION_3, DIMENSION_LM+DIMENSION_M-1 ) )
         Allocate( DES_HOR_g( DIMENSION_3 ) )


! Allocate the history variables for Adams-Bashforth integration
         IF (INTG_ADAMS_BASHFORTH) THEN
! Rate of change of particle mass
            Allocate( dMdt_OLD( MAX_PIP ) )
! Rate of change of particle mass percent species
            Allocate( dXdt_OLD( MAX_PIP, DIMENSION_N_s) )
         ENDIF

! Energy generation from reaction (cal/sec)
         Allocate( RXNS_Qs( MAX_PIP ) )
      ENDIF
! End Species Allocation
! ----------------------------------------------------------------<<<


! Allocate DES reaction rates arrays used to write reaction rates in vtp files or monitors
      if (SAVE_PART_RRATES) then
         if(allocated(Part_RRates_out)) deallocate(Part_RRates_out)
         Allocate( Part_RRates_out( MAX_PIP, NO_OF_DES_RXNS))
         Part_RRates_out(:,:) = ZERO
      endif


      RETURN
      END SUBROUTINE DES_ALLOCATE_ARRAYS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: ALLOCATE_DEM_MIO                                        !
!                                                                      !
!  Purpose:                                                            !
!                                                                      !
!  Author: J.Musser                                   Date: 17-Aug-09  !
!                                                                      !
!  Comments:                                                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

      SUBROUTINE ALLOCATE_DEM_MI

!-----------------------------------------------
! Modules
!-----------------------------------------------
      USE param1, only: undefined
      USE des_bc, only: dem_bcmi
      USE des_bc, only: pi_factor, pi_count
      use des_bc, only: numfrac_limit
      use des_bc, only: dem_mi_time, dem_bc_poly_layout
      use des_bc, only: dem_mi
      use des_bc, only: dem_bcmi_ijkstart, dem_bcmi_ijkend
      IMPLICIT NONE
!-----------------------------------------------

! Particle injection factor
      Allocate( PI_FACTOR (DEM_BCMI) )
! Particle injection count (injection number)
      Allocate( PI_COUNT (DEM_BCMI) )
! Particle injection time scale
      Allocate( DEM_MI_TIME (DEM_BCMI) )
! Array used for polydisperse inlets: stores the particle number
! distribution of an inlet scaled with numfrac_limit
      Allocate( DEM_BC_POLY_LAYOUT( DEM_BCMI, NUMFRAC_LIMIT ) )
! Data structure for storing BC data.
      Allocate( DEM_MI(DEM_BCMI) )

! Initialization
! Integer arrays
      PI_FACTOR(:) = -1
      PI_COUNT(:) = -1
      DEM_BC_POLY_LAYOUT(:,:) = -1
! Double precision arrays
      DEM_MI_TIME(:) = UNDEFINED

      allocate( DEM_BCMI_IJKSTART(DEM_BCMI) )
      allocate( DEM_BCMI_IJKEND(DEM_BCMI) )

      DEM_BCMI_IJKSTART = -1
      DEM_BCMI_IJKEND   = -1

! Boundary classification
!         Allocate( PARTICLE_PLCMNT (DES_BCMI) )
! Character precision arrays
!         PARTICLE_PLCMNT(:) = UNDEFINED_C

      RETURN
      END SUBROUTINE ALLOCATE_DEM_MI


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: ALLOCATE_PIC_MI                                         !
!                                                                      !
!  Purpose:                                                            !
!                                                                      !
!  Author: R. Garg                                    Date: 11-Jun-14  !
!                                                                      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE ALLOCATE_PIC_MI

! Modules
!-----------------------------------------------
      USE mfix_pic, only: pic_bcmi

      use mfix_pic, only: pic_mi

      IMPLICIT NONE
!-----------------------------------------------

! Allocate/Initialize for inlets
      if(pic_bcmi /= 0) allocate( pic_mi(pic_bcmi))


      RETURN
      END SUBROUTINE ALLOCATE_PIC_MI



!``````````````````````````````````````````````````````````````````````!
! Subroutine: ADD_PAIR                                                 !
!                                                                      !
! Purpose: Adds a neighbor pair to the pairs array.                    !
!                                                                      !
!``````````````````````````````````````````````````````````````````````!
      DOUBLE PRECISION FUNCTION add_pair(ii,jj)
      USE discretelement
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: ii,jj

      CALL NEIGHBOR_GROW(NEIGHBOR_INDEX(ii))

      NEIGHBORS(NEIGHBOR_INDEX(ii)) = jj
      NEIGHBOR_INDEX(ii) = NEIGHBOR_INDEX(ii) + 1
      add_pair = NEIGHBOR_INDEX(ii)

      RETURN
      END FUNCTION add_pair

!``````````````````````````````````````````````````````````````````````!
! Subroutine: NEIGHBOR_GROW                                            !
!                                                                      !
! Purpose: Grow neighbors arrays to new_neigh_max. Note that neighbor      !
! max should be increased before calling this routine. Also, no        !
! assumption to the previous array size is made as needed for restarts.!
!``````````````````````````````````````````````````````````````````````!
      SUBROUTINE NEIGHBOR_GROW(new_neigh_max)
        USE discretelement
        USE geometry
        IMPLICIT NONE

        integer, intent(in) :: new_neigh_max

        INTEGER :: lSIZE1
        INTEGER, DIMENSION(:), ALLOCATABLE :: neigh_tmp
        DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: pf_tmp
        INTEGER new_size

        lSIZE1 = size(neighbors,1)

        IF ( new_neigh_max .le. lSIZE1 ) RETURN

        new_size = lSIZE1

        DO WHILE(new_size < new_neigh_max)
           new_size = 2*new_size
        ENDDO

        allocate(neigh_tmp(new_size))
        neigh_tmp(1:lSIZE1) = neighbors(1:lSIZE1)
        neigh_tmp(lSIZE1+1:) = 0
        call move_alloc(neigh_tmp,neighbors)

        allocate(neigh_tmp(new_size))
        neigh_tmp(1:lSIZE1) = neighbors_old(1:lSIZE1)
        neigh_tmp(lSIZE1+1:) = 0
        call move_alloc(neigh_tmp,neighbors_old)

        allocate(pf_tmp(3,new_size))
        pf_tmp(:,1:lSIZE1) = pft_neighbor(:,1:lSIZE1)
        pf_tmp(:,lSIZE1+1:) = 0
        call move_alloc(pf_tmp,pft_neighbor)

        allocate(pf_tmp(3,new_size))
        pf_tmp(:,1:lSIZE1) = pft_neighbor_old(:,1:lSIZE1)
        pf_tmp(:,lSIZE1+1:) = 0
        call move_alloc(pf_tmp,pft_neighbor_old)


      END SUBROUTINE NEIGHBOR_GROW

!``````````````````````````````````````````````````````````````````````!
! Subroutine: PARTICLE_GROW                                            !
!                                                                      !
! Purpose: Grow particle arrays to new_max_pip. Note that pair         !
! max should be increased before calling this routine. Also, no        !
! assumption to the previous array size is made as needed for restarts.!
!``````````````````````````````````````````````````````````````````````!
      SUBROUTINE PARTICLE_GROW(new_max_pip)

        USE des_rxns
        USE des_thermo
        USE discretelement
        USE mfix_pic
        USE param1, only: undefined
        USE particle_filter
        USE resize
        USE run
        USE VTK, ONLY: SAVE_PART_RRATES, PART_RRATES_OUT

        IMPLICIT NONE

        integer, intent(in) :: new_max_pip
        integer :: old_size, new_size

        max_pip = max(max_pip, new_max_pip)
        IF (new_max_pip .le. size(des_radius)) RETURN

        old_size = size(des_radius)

        new_size = old_size

        DO WHILE (new_size < new_max_pip)
           new_size = 2*new_size
        ENDDO

        call real_grow(des_radius,new_size)
        call real_grow(RO_Sol,new_size)
        call real_grow(PVOL,new_size)
        call real_grow(PMASS,new_size)
        call real_grow(OMOI,new_size)
        call real_grow2_reverse(DES_POS_NEW,new_size)
        call real_grow2_reverse(DES_VEL_NEW,new_size)
        call real_grow2_reverse(OMEGA_NEW,new_size)
        call real_grow2_reverse(PPOS,new_size)

! Force chain arrays are grown when needed in calc_force_dem.f
! Note that force chain arrays do not have the same size as particle arrays
! The number of active elements is the number of contacts between particle pairs
        ! call real_grow2_reverse(FCHAIN_MIDPOINT,new_size)
        ! FCHAIN_MIDPOINT(:,:) = UNDEFINED
        ! call real_grow2_reverse(FCHAIN_FN,new_size)
        ! call real_grow(FCHAIN_LENGTH,new_size)
        ! call real_grow(FCHAIN_FN_MAG,new_size)

! Residence time
        call real_grow(RESIDENCE_TIME,new_size)

        call byte_grow(PARTICLE_STATE,new_size)
        call integer_grow(iglobal_id,new_size)
        call integer_grow2_reverse(pijk,new_size)
        call integer_grow(dg_pijk,new_size)
        call integer_grow(dg_pijkprv,new_size)
        call logical_grow(ighost_updated,new_size)
        call real_grow2_reverse(FC,new_size)
        call real_grow2_reverse(TOW,new_size)
        call real_grow(F_GP,new_size)
        call integer_grow2(WALL_COLLISION_FACET_ID,new_size)
        call real_grow3(WALL_COLLISION_PFT,new_size)
        call real_grow2_reverse(DRAG_FC,new_size)

        call integer_grow(NEIGHBOR_INDEX,new_size)
        call integer_grow(NEIGHBOR_INDEX_OLD,new_size)

        IF(PARTICLE_ORIENTATION) call real_grow2_reverse(ORIENTATION,new_size)

        IF(FILTER_SIZE > 0) THEN
           call integer_grow2(FILTER_CELL,new_size)
           call real_grow2(FILTER_WEIGHT,new_size)
        ENDIF

        IF(CGDEM) call real_grow(des_cgp_stw,new_size)
        IF(CGDEM) call real_grow(des_cgp_rpr,new_size)

        IF(MPPIC) THEN
           call real_grow(DES_STAT_WT,new_size)
           call real_grow2(PS_GRAD,new_size)
           call real_grow(EPG_P,new_size)
        ENDIF

        IF(USE_COHESION) THEN
           call real_grow(PostCohesive,new_size)
        ENDIF

        IF (DO_OLD) THEN
           call real_grow2_reverse(DES_POS_OLD,new_size)
           call real_grow2_reverse(DES_VEL_OLD,new_size)
           call real_grow2_reverse(DES_ACC_OLD,new_size)
           call real_grow2_reverse(OMEGA_OLD,new_size)
           call real_grow2_reverse(ROT_ACC_OLD,new_size)
        ENDIF

        IF(ENERGY_EQ)THEN
           call real_grow(DES_T_s,new_size)
           call real_grow(DES_C_PS,new_size)
           call real_grow2_reverse(DES_X_s,new_size)
           call real_grow(Q_Source,new_size)
           IF(CALC_CONV_DES) THEN
              call real_grow(CONV_Qs, new_size)
              call real_grow(GAMMAxSA, new_size)
           ENDIF
           IF(INTG_ADAMS_BASHFORTH) &
              call real_grow(Q_Source0,new_size)
        ENDIF

        IF(ANY_SPECIES_EQ)THEN

           call real_grow2_reverse(DES_X_s,new_size)
           call real_grow2_reverse( DES_R_s, new_size )

           IF (INTG_ADAMS_BASHFORTH) THEN
              call real_grow( dMdt_OLD, new_size )
              call real_grow2_reverse( dXdt_OLD, new_size )
           ENDIF

           call real_grow( RXNS_Qs, new_size )
        ENDIF

        IF(SAVE_PART_RRATES) call real_grow2_reverse(PART_RRATES_OUT,new_size)

        IF(DES_USR_VAR_SIZE > 0) &
        call real_grow2(DES_USR_VAR,new_size)

        CALL DES_INIT_PARTICLE_ARRAYS(old_size+1,new_size)

        RETURN

      END SUBROUTINE PARTICLE_GROW

    END MODULE DES_ALLOCATE
