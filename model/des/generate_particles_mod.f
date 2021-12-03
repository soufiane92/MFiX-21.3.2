#include "error.inc"

MODULE GENERATE_PARTICLES

   USE error_manager

   DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: PARTICLE_COUNT

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  SUBROUTINE: GENERATE_PARTICLE_CONFIG                                C
!                                                                      C
!  Purpose: Generate particle configuration based on maximum particle  C
!           radius and filling from top to bottom within specified     C
!           bounds                                                     C
!                                                                      C
!                                                                      C
!  Authors: Rahul Garg                                Date: 19-Mar-14  C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE GENERATE_PARTICLE_CONFIG

      use mfix_pic, only: MPPIC
      use discretelement, only: PIP, PARTICLES
! Flag indicating that the IC region is defined.
      use ic, only: IC_DEFINED
! Parameter for detecting unspecified values, zero, and one
      use param1, only: ONE
! Maximum number of initial conditions
      use param, only: DIMENSION_IC
! IC Region gas volume fraction.
      use ic, only: IC_EP_G

      use mpi_utility

      IMPLICIT NONE

      INTEGER :: ICV

      DO ICV = 1, DIMENSION_IC

         IF(.NOT.IC_DEFINED(ICV)) CYCLE
         IF(IC_EP_G(ICV) == ONE) CYCLE

         IF(MPPIC) THEN
            CALL GENERATE_PARTICLE_CONFIG_MPPIC(ICV)
         ELSE
            CALL GENERATE_PARTICLE_CONFIG_DEM(ICV)
         ENDIF

      ENDDO

      CALL GLOBAL_SUM(PIP,PARTICLES)

      WRITE(ERR_MSG, 1004) PARTICLES
 1004 FORMAT(/,'Total number of particles in the system: ',I15)

      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

      RETURN
      END SUBROUTINE GENERATE_PARTICLE_CONFIG



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: GENERATE_PARTICLE_CONFIG_DEM                            !
!  Authors: Rahul Garg, Jeff Dietiker                Date: 21-Mar-2014 !
!                                                                      !
!  Purpose: Generate particle configuration for DEM solids for each IC !
!           region. Now using the particle linked lists for initial    !
!           build                                                      !
!           This routine will ultimately supersede the older routine   !
!           that has not been deleted yet                              !
!                                                                      !
!----------------------------------------------------------------------!
!   Revision Date    : 09/2016                                         !
!   Revision Purpose : Make data structure changes for polydispersity  !
!                                                                      !
!   Revision By      : ASU MFIX-DEM Phi Team (Shaohua Chen, Yang Jiao, !
!                      Aytekin Gel, Manogna Adepu, Heather Emady)      !
!                                                                      !
!----------------------------------------------------------------------!
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE GENERATE_PARTICLE_CONFIG_DEM(ICV)

! Global Variables:
!---------------------------------------------------------------------//
! particle radius and density
      use discretelement, only: DES_RADIUS, RO_Sol
! particle position new and old
      use discretelement, only: DES_POS_NEW, DES_POS_OLD
! particle velocity new and old
      use discretelement, only: DES_VEL_NEW, DES_VEL_OLD
! Number of particles in the system (current)
      use discretelement, only: PIP
! Number of DEM solids phases.
      use discretelement, only: DES_MMAX
! Flag to use _OLD variables
      use discretelement, only: DO_OLD
! Angular velocity
      use discretelement, only: OMEGA_OLD, OMEGA_NEW, PIJK
! solid phase diameters and densities.
      use physprop, only: D_p0, RO_s0, MMAX
! IC Region solids volume fraction.
      use ic, only: IC_EP_s, IC_RO_s
! particle size distribution:
      use ic, only: IC_PSD_TYPE
      use ic, only: IC_PSD_MEAN_DP, IC_PSD_STDEV
      use ic, only: IC_PSD_MIN_DP
      use ic, only: IC_PSD_MAX_DP

! Constant: 3.14159...
      use constant, only: PI
! min and max physical coordinates of IC regions in each direction
      use ic, only: IC_X_w, IC_X_e, IC_Y_s, IC_Y_n, IC_Z_b, IC_Z_t
! initially specified velocity field and granular temperature
      use ic, only: IC_U_s, IC_V_s, IC_W_s, IC_Theta_M
! Flag to extend the lattice distribution in a given IC to available area
      use ic, only: IC_DES_FIT_TO_REGION
      use ic, only: IC_DES_LATTICE, IC_DES_SPACING,IC_DES_SM,IC_DES_NP
      use ic, only: IC_DES_SPACE_FACTOR_X, IC_DES_SPACE_FACTOR_Y, IC_DES_SPACE_FACTOR_Z
      use ic, only: IC_DES_RAND, IC_DES_RAND_FACTOR_X, IC_DES_RAND_FACTOR_Y
      use ic, only: IC_DES_RAND_FACTOR_Z
      use ic, only: IC_DES_CHECK_STL_OVERLAP
      use ic, only: IC_WITH_DES_SOLID_PHASE
      use ic, only: DES_IC_POS_TMP
! Parameter for detecting unspecified values, zero, and one
      use param1, only: UNDEFINED, UNDEFINED_I, ZERO, ONE, Half
! Parameter for small numbers
      use param1, only: SMALL_NUMBER

! to access random number generator subroutines
      use randomno
      use mpi_utility

      use desgrid, only: dg_xstart, dg_ystart, dg_zstart
      use desgrid, only: dg_xend, dg_yend, dg_zend

! direction wise spans of the domain and grid spacing in each direction
      use geometry, only: xlength, ylength, zlength

      use cutcell, only: CARTESIAN_GRID
      use stl_functions_des, only: CHECK_IF_PARTICLE_OVERLAPS_STL
      use run, only: solids_model
      use des_allocate, only: PARTICLE_GROW

      use desgrid, only: IofPOS, JofPOS, KofPOS
      use desgrid, only: dg_is_ON_myPE_OWNs
      use toleranc, only: compare

      use discretelement, only: max_pip, max_radius, xe, yn, zt
      use discretelement, only: cgp_scaling_method, cgp_d_p0
      use functions
      use param, only: dim_m
      use param, only: dimension_i, dimension_j, dimension_k
      use param1, only: undefined
      use mass_outflow_dem_mod, only: delete_particle
      use des_psd

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      INTEGER, INTENT(IN) :: ICV

! Local variables
!---------------------------------------------------------------------//
! Starting positions in the axial directions
      DOUBLE PRECISION :: xINIT, yINIT, zINIT
! Fractor used to scale particle diameter
      DOUBLE PRECISION :: lFAC,lFAC_X,lFAC_Y,lFAC_Z
! Particle position and velocity
      DOUBLE PRECISION :: POS(3), VEL(3)
! Number of particles in the lattice
      INTEGER :: SEED_X, SEED_Y, SEED_Z
! Loop indices phase/fluid cell
      INTEGER :: M, MM, I, J, K, IJK
! Loop indices for seeding
      INTEGER :: II, JJ, KK, LL
! Start and end bound for IC region.
      DOUBLE PRECISION :: IC_START(3), IC_END(3)
! Volume and lengths of the IC Region
      DOUBLE PRECISION :: DOM_VOL, DOML(3)
! Flag to skip the particle
      LOGICAL :: SKIP
! Diameter,radius adjusted for space padding
      DOUBLE PRECISION :: ADJ_DIA, ADJ_RADIUS
      DOUBLE PRECISION :: ADJ_DIA_X, ADJ_RADIUS_X
      DOUBLE PRECISION :: ADJ_DIA_Y, ADJ_RADIUS_Y
      DOUBLE PRECISION :: ADJ_DIA_Z, ADJ_RADIUS_Z
! Number of particles calculated from volume fracton
      INTEGER :: rPARTS(DIM_M), tPARTS
! Spacing between particles.
      DOUBLE PRECISION :: lDEL, lDX, lDY, lDZ
! Flag that the setup failed to fit the particles to the IC region
      LOGICAL :: FIT_FAILED
! Number of seeded particles
      INTEGER :: pCOUNT(DIM_M), tCOUNT


      DOUBLE PRECISION :: SOLIDS_DATA(0:DIM_M), lRO(DIM_M), GLOBAL_SOLIDS_DATA !(0:DIM_M)

      LOGICAL :: VEL_FLUCT, XYZ_FLUCT
      DOUBLE PRECISION, ALLOCATABLE :: randVEL(:,:), randXYZ(:,:)

      !DOUBLE PRECISION :: BC_MAX_RADIUS

      DOUBLE PRECISION :: rfac, rfac_base, rfac_x, rfac_y, rfac_z
      DOUBLE PRECISION :: IC_VOL, P_VOL

      INTEGER :: LATTICE
      INTEGER, PARAMETER :: LATTICE_CUBIC = 1 , LATTICE_HEXA = 2

      LOGICAL :: NARROW_Z

      INTEGER :: ALL_PART_COUNT_INIT(0:numPEs-1)
      INTEGER :: ALL_PART_COUNT_ADJ(0:numPEs-1)

      INTEGER :: iproc, IERR
      INTEGER :: NP_to_remove, NR
      INTEGER :: OLD_PIP,PIP_INCREMENT, NEW_PIP
      DOUBLE PRECISION :: ECHO_EPS, ECHO_SM 
      INTEGER :: ECHO_NP
      CHARACTER(LEN=11) :: ECHO_INPUT_VAR, ECHO_INPUT_VALUE

! A variable for temporary storing particle diameter
      DOUBLE PRECISION, DIMENSION(1) :: TMP_DIAMETER

      DOUBLE PRECISION, DIMENSION(1) :: tmp_rand_number
      DOUBLE PRECISION :: tmp_prob_sum
! Variable to store volumetric mean diameter
      DOUBLE PRECISION :: vol_mean_diameter
      INTEGER :: phase_number
      INTEGER, ALLOCATABLE :: tmp_array(:)
!      DOUBLE PRECISION, ALLOCATABLE :: prob_for_each_phase(:)
! Max diameter for a given phase M for lattice spacing
      double precision :: phase_max_dia
      double precision :: total_volume
! Particles in system for phase M
      integer, dimension(dim_m) :: pip_m
      integer :: allocstatus
! PIP form previous IC
      integer :: PIP_PREVIOUS



      pip_m(:) = 0
      total_volume = 0.D0
!      totaltime = 0.D0
!......................................................................!

      WRITE(ERR_MSG,"(2/,'Generating initial particle configuration:')")
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

! Keep a copy of PIP from the previous IC seeding
      PIP_PREVIOUS = PIP

      SOLIDS_DATA = ZERO
      CALL GET_IC_VOLUME(ICV, SOLIDS_DATA(0))
      IC_VOL = SOLIDS_DATA(0)
      CALL GLOBAL_ALL_SUM(IC_VOL)

! setting particle seed spacing grid to be slightly greater than
! the maximum particle diameter. seed at ~particle radii
      lFAC = 1.05D0

      ! Verify that the IC region volume is not zero.
      IF(IC_VOL <= 0.0d0) THEN
         WRITE(ERR_MSG,1000) ICV, IC_VOL
         CALL LOG_ERROR()
      ENDIF

1000  FORMAT('Error 1000: Invalid IC region volume: IC=',I3,' VOL=', ES15.4)

! Setup local arrays with IC region bounds.
      IC_START(1)=IC_X_W(ICV);   IC_END(1)=IC_X_E(ICV)
      IC_START(2)=IC_Y_S(ICV);   IC_END(2)=IC_Y_N(ICV)
      IC_START(3)=IC_Z_B(ICV);   IC_END(3)=IC_Z_T(ICV)

      DOML = IC_END-IC_START
      IF(NO_K) DOML(3)=DZ(1)

      ! Create a local array of solids densities so we don't have to
      ! reevaluate the function for every particle. Doing this for each
      ! phase is a bit overkill as DEM says that each IC region can only
      ! have on solids.
      DO M=MMAX+1,MMAX+DES_MMAX
         IF((SOLIDS_MODEL(M) == 'DEM'.OR.SOLIDS_MODEL(M) == 'CGP') .and. IC_EP_S(ICV,M) > ZERO) THEN
            lRO(M) = IC_RO_S(ICV, M)
         ELSE
            lRO(M) = -UNDEFINED
         END IF
      END DO

      ! Volume of the IC region
      DOM_VOL = DOML(1)*DOML(2)*DOML(3)

      phase_number = 0
      rPARTS=0
      !BC_MAX_RADIUS = ZERO

      DO M=MMAX+1,MMAX+DES_MMAX
         IF(SOLIDS_MODEL(M) == 'DEM'.OR.SOLIDS_MODEL(M) == 'CGP') THEN
            phase_number = phase_number + 1

! Mean volume of particles given a distribution is not same as volume based on
! distribution's mean diameter. mean_volume() computes actual mean volume for a given distribution
            if(IC_PSD_TYPE(ICV,M) .eq. 'MONO') then
              p_vol = (pi*d_p0(m)**3.0D0)/6.0d0
            elseif(CGDEM.and.cgp_scaling_method(m)==2) then
              p_vol = (pi*cgp_d_p0(m)**3.0D0)/6.0d0
            else
              p_vol = mean_volume(1, ICV, M)  ! 1 selects IC; 2 selects BC
            end if
            call global_all_sum(p_vol)
            p_vol = p_vol/dble(numPEs)

! Use the computed particle volume to compute number of particles
! if solids volume fraction is given
            IF(IC_EP_S(ICV,M)>ZERO.AND.IC_EP_S(ICV,M)/=UNDEFINED) THEN
               rPARTS(M) = floor((IC_EP_S(ICV,M)*IC_VOL)/P_VOL)
! if solids mass is given
            ELSEIF(IC_DES_SM(ICV,M)>ZERO.AND.IC_DES_SM(ICV,M)/=UNDEFINED) THEN
               rPARTS(M) = floor(IC_DES_SM(ICV,M)/(RO_S0(M)*P_VOL))
! if number of particles are specified explicitly
            ELSEIF(IC_DES_NP(ICV,M)>0.AND.IC_DES_NP(ICV,M)/=UNDEFINED_I) THEN
               rPARTS(M) = IC_DES_NP(ICV,M)
            ELSE
               rPARTS(M) = 0
            ENDIF

! Negative values of rParts indicate overflow. Treat this as a fatal error.
            IF(rPARTS(M)<0) THEN
               WRITE(ERR_MSG,100) ICV, M, DOM_VOL,D_P0(M),&
                    (6.0d0*IC_EP_S(ICV,M)*DOM_VOL)/(PI*(D_P0(M)**3))
               CALL LOG_ERROR()
            ENDIF

! @JFD: max_radius is not used here in this function. Should be removed ?!?
! already, max_radius is searched and updated in 
            ! IF(IC_EP_S(ICV,M) > ZERO .AND. D_P0(M) > (2.D0*MAX_RADIUS)) then
            !    MAX_RADIUS = HALF * D_P0(M)
            !    print *, "higher max_radius : ", max_radius
            !    print *, "ICV and phase M : ", ICV, M
            !    print *, "diameter d_p0 : ", d_p0(m)                                                                                                                                                                         
            !    read (*,*)
            ! ENDIF


            
         ENDIF
      ENDDO


100 FORMAT('Error 100: Overflow in IC region, IC=',I3,' , M=',I3, &
      /'IC Volume=',ES15.4,' , Particle Diameter=', ES15.4, ' , Number of particles = ',ES15.4, &
      /'This error is usually triggered by attempting to initialize with a very large number of particles.' &
      /'Please verify your settings (Particle size and region extents).', &
      /'This is a fatal error. MFiX will exit now.')


      allocate(tmp_array(phase_number))
!      allocate(prob_for_each_phase(phase_number))

! Total number of particles in this IC region.
      tPARTS = sum(rPARTS)
      IF(tPARTS == 0) RETURN


      xINIT = IC_START(1)
      yINIT = IC_START(2)
      zINIT = IC_START(3)

      pCOUNT = 0
      tCOUNT = 0


!     get particle number fraction for each phase
      phase_number = 0
      DO M=MMAX+1,MMAX+DES_MMAX
         IF(SOLIDS_MODEL(M) == 'DEM'.OR.SOLIDS_MODEL(M) == 'CGP') THEN
               phase_number = phase_number + 1
               tmp_array(phase_number) = M
!               prob_for_each_phase(phase_number) = dble(rPARTS(M))/dble(tPARTS)
         ENDIF
      ENDDO

! Fill particles for each solids phase
      DO MM=1, PHASE_NUMBER
         M = tmp_array(MM)
! Only seed particles if number of particles is non-zero
         if(rparts(M)==0) cycle

! Seed particles from cubic of hexagonal arrangement               
            WRITE(ERR_MSG,"(2/,'Phase',I3,': Number of particles to seed = ',I12)") M,rpARTS(M)
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
            
            GLOBAL_SOLIDS_DATA = ZERO

            if(IC_PSD_TYPE(icv, m) == 'LOG_NORMAL' .or.&
               IC_PSD_TYPE(icv, m) == 'NORMAL'.or. &
               IC_PSD_TYPE(icv, m) == 'UNIFORM') then
               phase_max_dia = IC_PSD_MAX_DP(icv, m)
               if(CGDEM.and.cgp_scaling_method(m)==2) phase_max_dia = cgp_d_p0(m)
            else if(IC_PSD_TYPE(icv, m) == 'CUSTOM') then
! Either upper bound from the "CUSTOM" distribution itself or user-specified max diameter
               phase_max_dia = min(IC_PSD_MAX_DP(icv, m), find_max_dia(1, icv, m))
               max_radius = dmax1(max_radius,HALF*phase_max_dia)
            else
               phase_max_dia = d_p0(m)
            endif

! setting particle seed spacing grid to be slightly greater than
! the particle diameter. Spacing can vary in each direction
            lFAC         = ONE + IC_DES_SPACING(ICV,M)
            lFAC_X       = ONE + IC_DES_SPACING(ICV,M)*IC_DES_SPACE_FACTOR_X(ICV,M)
            lFAC_Y       = ONE + IC_DES_SPACING(ICV,M)*IC_DES_SPACE_FACTOR_Y(ICV,M)
            lFAC_Z       = ONE + IC_DES_SPACING(ICV,M)*IC_DES_SPACE_FACTOR_Z(ICV,M)
            ADJ_RADIUS   = 0.5d0*phase_max_dia*lFAC
            ADJ_DIA      = 2.0d0*ADJ_RADIUS
            ADJ_RADIUS_X = 0.5d0*phase_max_dia*lFAC_X
            ADJ_RADIUS_Y = 0.5d0*phase_max_dia*lFAC_Y
            ADJ_RADIUS_Z = 0.5d0*phase_max_dia*lFAC_Z
            ADJ_DIA_X    = 2.0d0*ADJ_RADIUS_X
            ADJ_DIA_Y    = 2.0d0*ADJ_RADIUS_Y
            ADJ_DIA_Z    = 2.0d0*ADJ_RADIUS_Z

! Compute seeding info based on lattice type
! Convert lattice keyword into integer for faster processing at the particle level (see triple nested loop below)
! Narrow z-domains with hexa lattice degenerate to triangular lattice

            NARROW_Z = .FALSE.
            
            SELECT CASE(TRIM(IC_DES_LATTICE(ICV,M)))
            
               CASE('CUBIC')
                  LATTICE = LATTICE_CUBIC
                  SEED_X = max(1,floor((IC_END(1)-IC_START(1)-ADJ_DIA_X)/ADJ_DIA_X))
! yinit instead of IC_START(2) to consider cases with multiple solids phases
! Note yInit is updated at the end after initializing each solids phase to next solids phase
                  !SEED_Y = max(1,floor((IC_END(2)-IC_START(2)-ADJ_DIA_Y)/ADJ_DIA_Y))
                  SEED_Y = max(1,floor((IC_END(2)-yInit-ADJ_DIA_Y)/ADJ_DIA_Y))
                  SEED_Z = max(1,floor((IC_END(3)-IC_START(3)-ADJ_DIA_Z)/ADJ_DIA_Z))
                  lDX = DOML(1)/dble(SEED_X)
                  !lDY = DOML(2)/dble(SEED_Y)
                  lDY = (IC_END(2)-yInit)/dble(SEED_Y)
                  IF(DO_K) THEN
                     lDZ = DOML(3)/dble(SEED_Z)
                  ELSE
                     lDZ = 0.0d0
                  ENDIF
                  rfac_base = ONE
            
               CASE('HEXA')
                  LATTICE = LATTICE_HEXA
                  IF(DO_K) THEN
                     SEED_X = (IC_END(1)-IC_START(1)-ADJ_RADIUS_X)/ADJ_RADIUS_X
                     SEED_X = floor(0.5d0*(SEED_X) - 1.5D0)
            
! yinit instead of IC_START(2) to consider cases with multiple solids phases
! Note yInit is updated at the end after initializing each solids phase to next solids phase
                     !SEED_Y = (IC_END(2)-IC_START(2)-ADJ_RADIUS_Y)/ADJ_RADIUS_Y
                     SEED_Y = (IC_END(2)-yInit-ADJ_RADIUS_Y)/ADJ_RADIUS_Y
                     SEED_Y = floor(1.5d0*(SEED_Y - 1.0D0)/dsqrt(6.0d0))
            
                     SEED_Z = (IC_END(3)-IC_START(3)-ADJ_RADIUS_Z)/ADJ_RADIUS_Z
                     SEED_Z = floor(1.0d0*(SEED_Z - 1.0D0)/dsqrt(3.0d0) - 1.0d0/3.0d0)
                     IF(SEED_Z<1) THEN
                        SEED_Z   = 1
                        NARROW_Z = .TRUE.
                        ZINIT    = 0.5D0*(IC_START(3)+IC_END(3))
                     ENDIF
                  ELSE
                     SEED_X = (IC_END(1)-IC_START(1)-ADJ_RADIUS_X)/ADJ_RADIUS_X
                     SEED_X = floor(0.5d0*(SEED_X) - 0.5D0)
            
! yinit instead of IC_START(2) to consider cases with multiple solids phases
! Note yInit is updated at the end after initializing each solids phase to next solids phase
                     !SEED_Y = (IC_END(2)-IC_START(2)-ADJ_RADIUS_X)/ADJ_RADIUS_X
                     SEED_Y = (IC_END(2)-yInit-ADJ_RADIUS_X)/ADJ_RADIUS_X
                     SEED_Y = floor(1.5d0*(SEED_Y - 1.0D0)/dsqrt(3.0d0))
            
                     SEED_Z = 1
                  ENDIF
                  rfac_base = HALF
            
            END SELECT

            SEED_X = MAX(SEED_X,1)
            SEED_Y = MAX(SEED_Y,1)
            SEED_Z = MAX(SEED_Z,1)

! Set velocity and position fluctuations if needed
            VEL_FLUCT = SET_VEL_FLUCT(ICV,M)
            XYZ_FLUCT = SET_XYZ_FLUCT(ICV,M)

! Go through the lattice vertically (JJ loop). 
! Find position of next particle, and add it.
! The whole lattice will seed more than needed (rPARTS(M) paticles)
! After each xz plane is seeded, count total number of particles
! and stop seeding if particle count is greater than rPARTS(M)
! Then each rank will remove particles until the exact count
! of rPARTS(M) is obtained.

            JJ_LP: DO JJ=1, SEED_Y
            KK_LP: DO KK=1, SEED_Z
            II_LP: DO II=1, SEED_X

               SELECT CASE(LATTICE)

                  CASE(LATTICE_CUBIC)
                     POS(1) = xINIT + (II-1)*lDX + HALF*lDX
                     POS(2) = YINIT + (JJ-1)*lDY + HALF*lDY
                     POS(3) = ZINIT + (KK-1)*lDZ + HALF*lDZ

                  CASE(LATTICE_HEXA)
                     IF(DO_K.AND..NOT.NARROW_Z) THEN
                        POS(1) = xINIT + ( 2*II + mod(JJ+KK,2) ) * ADJ_RADIUS_X 
                        POS(2) = YINIT + ( 2*JJ/3.0  ) * dsqrt(6.0d0) * ADJ_RADIUS_Y
                        POS(3) = ZINIT + ( KK + mod(JJ,2)/3.0 ) * dsqrt(3.0d0) * ADJ_RADIUS_Z
                     ELSE
                        POS(1) = xINIT + ( 2*II + mod(JJ,2) ) * ADJ_RADIUS_X 
                        POS(2) = YINIT + ( 2*JJ/2.0  ) * dsqrt(3.0d0) * ADJ_RADIUS_Y 
                        POS(3) = ZINIT 
                     ENDIF
               END SELECT


               ! print*,'lDx=',lDX,XYZ_FLUCT

               pCOUNT(M) = pCOUNT(M) + 1
               tCOUNT = tCOUNT + 1

! Add random position fluctuation if needed
               IF(XYZ_FLUCT) THEN
                  rfac   = rfac_base * IC_DES_RAND(ICV,M) * IC_DES_SPACING(ICV,M) * phase_max_dia
                  rfac_x = rfac * IC_DES_SPACE_FACTOR_X(ICV,M)
                  rfac_y = rfac * IC_DES_SPACE_FACTOR_Y(ICV,M)
                  rfac_z = rfac * IC_DES_SPACE_FACTOR_Z(ICV,M)

                  POS(1) = POS(1) + (randXYZ(pCOUNT(M),1)-half) * rfac * IC_DES_RAND_FACTOR_X(ICV,M)
                  POS(2) = POS(2) + (randXYZ(pCOUNT(M),2)-half) * rfac * IC_DES_RAND_FACTOR_Y(ICV,M)
                  POS(3) = POS(3) + (randXYZ(pCOUNT(M),3)-half) * rfac * IC_DES_RAND_FACTOR_Z(ICV,M)
               ENDIF

! Nudge particles that fall right on top of a des grid line                  
               IF(compare(POS(1),dg_xstart) .OR. compare(POS(1),dg_xend))    &
                  POS(1) = POS(1) + SMALL_NUMBER
               IF(compare(POS(2),dg_ystart) .OR. compare(POS(2),dg_yend))    &
                  POS(2) = POS(2) + SMALL_NUMBER
               IF(DO_K) THEN
                  IF(compare(POS(3),dg_zstart) .OR. compare(POS(3),dg_zend)) &
                     POS(3) = POS(3) + SMALL_NUMBER
               ENDIF


! Add the particle at coordinates POS. This excludes particles that are not in a fluid cell 
! or overlap with walls. All variables are set inside ADD_PARTICLE
               CALl ADD_PARTICLE(POS,IC_DES_CHECK_STL_OVERLAP(ICV,M))


            ENDDO II_LP
            ENDDO KK_LP
! Exit if all particles were seeded.
! The test is done at the JJ_LP loop level to minimize communication
! among processors when doing the global sum on SOLIDS_DATA
            GLOBAL_SOLIDS_DATA = SOLIDS_DATA(M)
            CALL GLOBAL_ALL_SUM(GLOBAL_SOLIDS_DATA)

            IF(GLOBAL_SOLIDS_DATA >= rPARTS(M)) THEN
! Get ready for the next phase. Will start just above the current xz plane
               YINIT = POS(2) + ADJ_RADIUS
               EXIT JJ_LP
            ENDIF

            ENDDO JJ_LP
!#if 0
! Since globlal sum was done at the JJ_LP loop level, the particle count will 
! most likely exceed the desired particle count. 
! It is adjusted below by removing particles belonging to each processors
! 1) Gather PIP from each PE onto head node
! 2) On head node, loop through PIP, and decrease count by one if PIP>0
! 3) Continue until sum of PIP matched the desire count
! 4) Scatter new PIP values to processors

            ALL_PART_COUNT_INIT(MyPE) = INT(SOLIDS_DATA(M))
            if(numPEs>1) then
              CALL allgather_1i (ALL_PART_COUNT_INIT(MyPE),ALL_PART_COUNT_ADJ,IERR)
            else
              all_part_count_adj = all_part_count_init
            end if 

            IF (myPE == PE_IO) THEN
               NP_to_remove = SUM(ALL_PART_COUNT_ADJ) - rPARTS(M)
               NR_LP: DO NR = 1, NP_to_remove
                  DO iproc = 0,NumPEs-1
                     IF(ALL_PART_COUNT_ADJ(iproc)>0) ALL_PART_COUNT_ADJ(iproc) = ALL_PART_COUNT_ADJ(iproc) - 1
                     IF(SUM(ALL_PART_COUNT_ADJ)<=rPARTS(M) ) EXIT NR_LP
                  ENDDO
               ENDDO NR_LP
            ENDIF

            CALL BCAST(ALL_PART_COUNT_ADJ)

! Now each rank knows how many particles it needs to remove from its local count
! Reset PIP and set type to NONEXISTENT for particles that are removed
            SOLIDS_DATA(M) = SOLIDS_DATA(M) - FLOAT(ALL_PART_COUNT_INIT(MyPE)-ALL_PART_COUNT_ADJ(MyPE))
            OLD_PIP = PIP
            NEW_PIP = PIP - ALL_PART_COUNT_INIT(MyPE) + ALL_PART_COUNT_ADJ(MyPE)
            IF(NEW_PIP<OLD_PIP) THEN
               DO LL=NEW_PIP+1,OLD_PIP
                  CALL DELETE_PARTICLE(LL)
               ENDDO
            ENDIF

      ENDDO   ! Loop over phase M


      pip_m(1:dim_m) = INT(SOLIDS_DATA(1:dim_m))

! Collect the data
      CALL GLOBAL_ALL_SUM(SOLIDS_DATA)

! Verify that the IC region volume is not zero.
      IF(SOLIDS_DATA(0) <= 0.0d0) THEN
         WRITE(ERR_MSG,1001) ICV, SOLIDS_DATA(0)
         CALL LOG_ERROR()
      ENDIF

1001 FORMAT('Error 1001: Invalid IC region volume: IC=',I3,' VOL=', ES15.4)


      WRITE(ERR_MSG,2000) ICV
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

      DO M=MMAX+1, MMAX+DES_MMAX
         IF(SOLIDS_DATA(M) < SMALL_NUMBER) CYCLE
         total_volume = 0.D0

         DO II = PIP_PREVIOUS + 1, MAX_PIP
           IF(PIJK(II,5) == M) THEN
!! Sum of radii to the power cubed         
             TOTAL_VOLUME = TOTAL_VOLUME + DES_RADIUS(II)**3.D0
           END IF
         END DO  
  
         CALL GLOBAL_ALL_SUM(total_volume)
! Compute the actual total volume from sum of radii cubed 
         total_volume  =  4.D0/3.D0*pi*total_volume

         ECHO_EPS = total_volume/SOLIDS_DATA(0)

         ECHO_SM = total_volume*RO_S0(M)
         ECHO_NP = INT(SOLIDS_DATA(M))

         IF(IC_EP_S(ICV,M)>ZERO.AND.IC_EP_S(ICV,M)/=UNDEFINED) THEN
            ECHO_INPUT_VAR = '   EP_S    '
            WRITE(ECHO_INPUT_VALUE,"(1x,ES9.2,1x)") IC_EP_S(ICV,M) 

         ELSEIF(IC_DES_SM(ICV,M)>ZERO.AND.IC_DES_SM(ICV,M)/=UNDEFINED) THEN
            ECHO_INPUT_VAR = ' SOL. MASS '
            WRITE(ECHO_INPUT_VALUE,"(1x,ES9.2,1x)") IC_DES_SM(ICV,M)

         ELSEIF(IC_DES_NP(ICV,M)>0.AND.IC_DES_NP(ICV,M)/=UNDEFINED_I) THEN
            ECHO_INPUT_VAR = '  PART NB  '
            WRITE(ECHO_INPUT_VALUE,"(1x,I9,1x)") IC_DES_NP(ICV,M)

         ELSE
            ECHO_INPUT_VAR   = '  UNKNOWN  '
            ECHO_INPUT_VALUE = '  UNKNOWN  '

         ENDIF


         WRITE(ERR_MSG,2010) M, ECHO_INPUT_VAR, ECHO_INPUT_VALUE, &
                             ECHO_NP, ECHO_EPS, ECHO_SM

         CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_WARNING, HEADER=.FALSE., FOOTER=.FALSE.)
      ENDDO

! Verif the seeding was successful
      DO M=MMAX+1, MMAX+DES_MMAX
         IF(SOLIDS_DATA(M) < SMALL_NUMBER) CYCLE

            IF(SOLIDS_DATA(M) == rPARTS(M)) THEN
               WRITE(ERR_MSG,"(2/,'Phase',I3,': Successful seeding')") M
               CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_WARNING, HEADER=.FALSE., FOOTER=.FALSE.)
            ELSE
               WRITE(ERR_MSG,"(2/,'Phase',I3,': Unsuccessful seeding')") M
               CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_WARNING, HEADER=.FALSE., FOOTER=.FALSE.)
            ENDIF

      ENDDO
!      IF(allocated(kf_data)) deallocate(kf_data)
      IF(allocated(tmp_array)) deallocate(tmp_array)
!      IF(allocated(prob_for_each_phase)) deallocate(prob_for_each_phase)

      IF(allocated(randVEL)) deallocate(randVEL)

      RETURN

! 2000 FORMAT(/2x,'|',43('-'),'|',/2x,'| IC Region: ',I3,28x,'|',/2x,   &
!         '|',43('-'),'|',/2x,'| Phase | Number of |    EPs    |    EP',&
!         's    |',/2x,'|   ID  | Particles | Specified |   Actual  |', &
!         /2x,'|-------|',3(11('-'),'|'))
!
! 2010 FORMAT(2x,'|  ',I3,'  |',1x,I9,1x,'|',2(1x,ES9.2,1x,'|'),/2x,    &
!         '|-------|',3(11('-'),'|'))

 2000 FORMAT(/2x,'|',67('-'),'|',              &
             /2x,'| IC Region: ',I3,52x,'|',   &
             /2x,'|',67('-'),'|',              & 
             /2x,'| Phase |   Input   |   Input   | Number of |    EPs    |   Solid   |', &
             /2x,'|   ID  | Specified |   Value   | Particles |           |   mass    |', &
             /2x,'|-------|',5(11('-'),'|'))

 2010 FORMAT(2x,'|  ',I3,'  |',A11,'|',A11,'|',1x,I9,1x,'|',2(1x,ES9.2,1x,'|'),/2x,    &
         '|-------|',5(11('-'),'|'))


      CONTAINS

!......................................................................!
! Function: SET_VEL_FLUCT                                              !
! Purpose: Set the flag for random velocity fluctuations. If needed    !
! the random velocities are calculated.                                !
!......................................................................!
      LOGICAL FUNCTION SET_VEL_FLUCT(lICV, lM)
      INTEGER, INTENT(IN) :: lICV, lM
      DOUBLE PRECISION :: VEL_SIG
      SET_VEL_FLUCT=(IC_Theta_M(lICV,lM) /= 0.0d0)
      IF(SET_VEL_FLUCT) THEN
         if(allocated(randVEL)) deallocate(randVEL)
         allocate(randVEL(100+int(rPARTS(lM)),3))
         VEL_SIG = sqrt(IC_Theta_M(lICV,lM))
         CALL NOR_RNO(randVEL(:,1), IC_U_s(lICV,lM),VEL_SIG)
         CALL NOR_RNO(randVEL(:,2), IC_V_s(lICV,lM),VEL_SIG)
         IF(DO_K) CALL NOR_RNO(randVEL(:,3),IC_W_s(lICV,lM),VEL_SIG)
      ENDIF

      END FUNCTION SET_VEL_FLUCT


!......................................................................!
! Function: SET_XYZ_FLUCT                                              !
! Purpose: Set the flag for random position fluctuations. If needed    !
! the random positions  are calculated.                                !
!......................................................................!
      LOGICAL FUNCTION SET_XYZ_FLUCT(lICV, lM)
      use randomno
      INTEGER, INTENT(IN) :: lICV, lM
      DOUBLE PRECISION :: XYZ_SIG
      SET_XYZ_FLUCT=(IC_DES_RAND(lICV,lM) /= 0.0d0)
      IF(SET_XYZ_FLUCT) THEN
         if(allocated(randXYZ)) deallocate(randXYZ)
         allocate(randXYZ(10*int(rPARTS(lM)),3))
         XYZ_SIG = IC_DES_RAND(ICV,M) * 0.5D0 * D_P0(lM) 
         CALL UNI_RNO(randXYZ(:,1)) !, 0.0D0,XYZ_SIG)
         CALL UNI_RNO(randXYZ(:,2)) !, 0.0D0,XYZ_SIG)
         IF(DO_K) CALL UNI_RNO(randXYZ(:,3)) !, 0.0D0,XYZ_SIG)
      ENDIF

      END FUNCTION SET_XYZ_FLUCT

!......................................................................!
! Function: SET_VEL_FLUCT_mod                                          !
! Purpose: Faster/optimized version of SET_VEL_FLUCT                   !
!......................................................................!
      LOGICAL FUNCTION SET_VEL_FLUCT_mod(lICV, lM)
      INTEGER, INTENT(IN) :: lICV, lM
      DOUBLE PRECISION :: VEL_SIG
      SET_VEL_FLUCT_mod=(IC_Theta_M(lICV,lM) /= 0.0d0)

      END FUNCTION SET_VEL_FLUCT_mod


!......................................................................!
! subroutine: ADD_PARTICLE                                             !
! Purpose: Add particle at position POS with additional checks         !
!......................................................................!

      SUBROUTINE ADD_PARTICLE(POS,CHECK_STL_OVERLAP)

      use cutcell, only: SMALL_CELL_AT
      USE des_rxns, only: DES_X_s
      use des_thermo, only: DES_T_s
      use discretelement, only: cgdem, cgp_stat_wt, cgp_d_p0, cgp_scaling_method
      use discretelement, only: des_cgp_stw, des_cgp_rpr, residence_time
      use ic
      use param1, only: undefined, zero
      use physprop, only: C_PS0
      use physprop, only: NMAX
      use run, only: ENERGY_EQ, SPECIES_EQ

      IMPLICIT NONE
! Particle position and velocity
      DOUBLE PRECISION :: POS(3), VEL(3)
! Flag to check if particle overlaps with STL
      LOGICAL :: CHECK_STL_OVERLAP
! Species loop index
      INTEGER :: NN

! Keep only particles that belong to this process.
      IF(.NOT.dg_is_ON_myPE_OWNs(IofPOS(POS(1)), &
         JofPOS(POS(2)),KofPOS(POS(3)))) RETURN

! Bin the parcel to the fuild grid.
      CALL PIC_SEARCH(I, POS(1), XE, DIMENSION_I, IMIN2, IMAX2)
      CALL PIC_SEARCH(J, POS(2), YN, DIMENSION_J, JMIN2, JMAX2)
      K=1
      IF(DO_K) CALL PIC_SEARCH(K, POS(3), ZT, DIMENSION_K, KMIN2, KMAX2)

! Skip cells that return invalid IJKs.
      IF(DEAD_CELL_AT(I,J,K)) RETURN

! Skip cells that are not part of the local fuild domain.
! Small cut cells are allowed here, if particles intersect the STL geometry,
! they can be removed below
      IJK = FUNIJK(I,J,K)
      IF(.NOT.FLUID_AT(IJK).AND..NOT.SMALL_CELL_AT(IJK)) RETURN

      IF(CARTESIAN_GRID.AND.CHECK_STL_OVERLAP) THEN
         CALL CHECK_IF_PARTICLE_OVERLAPS_STL(POS, I, J, K, SKIP)
         IF(SKIP) RETURN
      ENDIF

      PIP = PIP + 1
      !pip_m(m) = pip_m(m) + 1
      CALL PARTICLE_GROW(PIP)
      MAX_PIP = max(PIP,MAX_PIP)

      CALL SET_NORMAL(PIP)

      IF(VEL_FLUCT.and.pip<=size(randvel,1)) THEN  ! avoid getting out of bounds
         VEL(1) = randVEL(PIP,1)
         VEL(2) = randVEL(PIP,2)
         VEL(3) = randVEL(PIP,3)
      ELSE
         VEL(1) = IC_U_s(ICV,M)
         VEL(2) = IC_V_s(ICV,M)
         VEL(3) = IC_W_s(ICV,M)
      ENDIF
      IF(NO_K) VEL(3) = 0.0d0


!----------------------
      if(IC_PSD_TYPE(ICV,M) .eq. 'MONO') then
        DES_RADIUS(PIP) = D_P0(M)*HALF
        ! llu, d_p0 was already scaled to parcel size
        IF(CGP_SCALING_METHOD(M)==2) then
           DES_RADIUS(PIP) = DES_RADIUS(PIP)/CGP_STAT_WT(M)**(1.0d0/3.0d0)
        endif
      else
        DES_RADIUS(PIP) = psd_radius(1, ICV, M)
      end if

!----------------------


      DES_POS_NEW(PIP,:) = POS(:)
      DES_VEL_NEW(PIP,:) = VEL(:)
      OMEGA_NEW(PIP,:) = 0.0d0

      RO_SOL(PIP) =  RO_S0(M)

      PIJK(PIP,1) = I
      PIJK(PIP,2) = J
      PIJK(PIP,3) = K
      PIJK(PIP,4) = IJK
      PIJK(PIP,5) = M

      IF(DO_OLD) THEN
         DES_VEL_OLD(PIP,:) = DES_VEL_NEW(PIP,:)
         DES_POS_OLD(PIP,:) = DES_POS_NEW(PIP,:)
         OMEGA_OLD(PIP,:)   = ZERO
      ENDIF

      IF(CGDEM) THEN
         IF(CGP_SCALING_METHOD(M)==1) THEN
            DES_CGP_STW(PIP) = CGP_STAT_WT(M)
            DES_CGP_RPR(PIP) = DES_RADIUS(PIP)/DES_CGP_STW(PIP)**(1.0d0/3.0d0)
         ELSEIF(CGP_SCALING_METHOD(M)==2) THEN
            DES_CGP_STW(PIP) = (HALF*CGP_D_P0(M)/DES_RADIUS(PIP))**3
            DES_CGP_RPR(PIP) = DES_RADIUS(PIP)
            DES_RADIUS(PIP) = HALF*CGP_D_P0(M)
         ENDIF
      ENDIF

! Set the initial particle temperature.
      IF(ENERGY_EQ) THEN
         DES_T_s(PIP) = IC_T_s(ICV,M)
      ENDIF

! Set the initial species composition.
      IF((ENERGY_EQ .AND. C_Ps0(M) == UNDEFINED) .OR.         &
         SPECIES_EQ(M)) THEN
         DES_X_s(PIP,:) = ZERO
         DO NN = 1, NMAX(M)
            DES_X_s(PIP,NN) = IC_X_s(ICV,M,NN)
         ENDDO
      ENDIF

      SOLIDS_DATA(M) = SOLIDS_DATA(M) + 1.0

! Residence time
      RESIDENCE_TIME(PIP) = ZERO

      RETURN
      END SUBROUTINE ADD_PARTICLE


!#endif


      END SUBROUTINE GENERATE_PARTICLE_CONFIG_DEM







!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_PARTICLE_XYZ_IN_CSV                              C
!  Purpose: Write particles coordinates in a csv file                  C
!           Binary format                                              C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 16-Feb-18  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE WRITE_PARTICLE_XYZ_IN_CSV

!      use des_mpi
      use cdist
      use mpi_comm_des
      use mpi_utility
! Number of DEM solids phases.
      use discretelement, only: DES_MMAX
! Number of TFM solid phases
      use physprop, only:  MMAX
! Find available file unit      
      use funits, only: newunit
! Error manager
      use error_manager

      IMPLICIT NONE


      INTEGER :: LOCAL_CNT, GLOBAL_CNT


      DOUBLE PRECISION, ALLOCATABLE :: ltemp_array(:,:)  ! local
      DOUBLE PRECISION, ALLOCATABLE :: gtemp_array(:,:)  ! global

      LOGICAL, ALLOCATABLE :: WRITE_TO_CSV(:)

      INTEGER :: M,MM
      INTEGER :: LB, UB
      INTEGER :: PC, LC1, LC2
! Variables related to gather
      integer :: lgathercnts(0:numpes-1), lproc
! local unit
      INTEGER :: lunit
! local filename
       character(255) :: lfilename
! IO Status:
       INTEGER :: IOS
! Flag to indicate if file exists.
       LOGICAL :: lEXISTS


! Loop through all particles and kee a list of particles belonging to a VTK region

! Since the data is appended (i.e., written after all tags), the
! offset, in number of bytes must be specified.  The offset includes
! the size of the data for each field, plus the size of the integer
! that stores the number of bytes.  this is why the offset of a field
! equals the offset of the previous field plus sizeof(int) plus the
! number of bytes of the field.

! Next, the actual data is written for the geometry (PASS=WRITE_DATA)
! The DATA is converted to single precision to save memory.


!      CALL INIT_ERR_MSG("WRITE_PARTICLE_XYZ_IN_CSV")

      IF(ALLOCATED(WRITE_TO_CSV)) DEALLOCATE(WRITE_TO_CSV)                                             
      ALLOCATE(WRITE_TO_CSV(MAX_PIP))

      DO MM=MMAX+1,MMAX+DES_MMAX

         WRITE_TO_CSV(:) = .FALSE.

         IF (.NOT.BDIST_IO) THEN
! The number of particles

            LOCAL_CNT = 0
            GLOBAL_CNT = 10
            PC = 1 
            DO LC1 = 1, MAX_PIP
               IF(PC > PIP) EXIT
               IF(IS_NONEXISTENT(LC1)) CYCLE
               PC = PC+1
               IF(IS_GHOST(LC1) .OR. IS_ENTERING_GHOST(LC1) .OR. IS_EXITING_GHOST(LC1)) CYCLE

               M = PIJK(LC1,5)
               IF(M==MM) THEN
                  WRITE_TO_CSV(LC1) = .TRUE.
                  LOCAL_CNT = LOCAL_CNT + 1
               ENDIF
            ENDDO ! particle loop

! Calculate the total number of particles system-wide.
            call global_sum(LOCAL_CNT, GLOBAL_CNT)

! 1788 ! No need to set the send/reccv when using distributed IO
! 1789       IF (BDIST_IO) RETURN
! Set the send count from the local process.
            igath_sendcnt = LOCAL_CNT

! Collect the number of particles on each rank.all ranks.
            lgathercnts = 0
            lgathercnts(myPE) = LOCAL_CNT
            call global_sum(lgathercnts,igathercnts)
! Calculate the rank displacements.
            idispls(0) = 0
            DO lPROC = 1,NUMPEs-1
               idispls(lproc) = idispls(lproc-1) + igathercnts(lproc-1)
            ENDDO  

            LB = LBOUND(DES_POS_NEW,2) 
            UB = UBOUND(DES_POS_NEW,2) 

            ALLOCATE (dProcBuf(LOCAL_CNT) )
            ALLOCATE (ltemp_array((UB-LB)+1,LOCAL_CNT))
            IF(MYPE==PE_IO) THEN
               ALLOCATE (gtemp_array((UB-LB)+1,GLOBAL_CNT))
               ALLOCATE (dRootBuf(GLOBAL_CNT))
            ELSE
               ALLOCATE (gtemp_array((UB-LB)+1,10))
               ALLOCATE (dRootBuf(10))
            ENDIF

! Pack particle coordinates in a temporary local array
            PC = 0
            DO LC1 = 1, MAX_PIP
               IF(WRITE_TO_CSV(LC1)) THEN
                  PC =PC + 1
                  DO LC2=LB, UB
                     ltemp_array(LC2,PC) = DES_POS_NEW(LC1,LC2)
                  ENDDO
               ENDIF
               IF(PC==LOCAL_CNT) EXIT
            ENDDO

! For each coordinate (x,y, and z), gather the local list to global temporary array
            DO LC1 = LB, UB
               dprocbuf(1:LOCAL_CNT)=ltemp_array(LC1,1:LOCAL_CNT)
               CALL desmpi_gatherv(ptype=2)
               IF(MYPE == PE_IO) gtemp_array(LC1,:) = drootbuf(:)
            ENDDO

! Write the list of coordinates
            IF(myPE == PE_IO) THEN
               lFILENAME = ''  
               WRITE(lFILENAME,'("PART_XYZ_AT_TSTOP_M=",I2.2,".CSV")') MM
               INQUIRE(FILE=lFILENAME, EXIST=lEXISTS)
               IF(LEXISTS) THEN
                  WRITE(ERR_MSG, 1000) TRIM(lFILENAME)
                  CALL LOG_ERROR()
               ENDIF
               lUNIT = newunit()
               OPEN(UNIT=lUNIT, FILE=lFILENAME, FORM="FORMATTED")

1000 FORMAT('Warning 1100: ',A,' already exists and will be overwritten.')

               WRITE(lUNIT,*)  "x, y, z"
               DO LC1=1, GLOBAL_CNT
                  WRITE(lUNIT,1100)  gtemp_array(LB:UB,LC1)
               ENDDO
               CLOSE(lUNIT)
            ENDIF

1100 FORMAT(E14.8,' , ',E14.8,' , ',E14.8)         

            deallocate (dProcBuf, dRootBuf, ltemp_array,gtemp_array)




         ELSEIF(BDIST_IO.AND.LOCAL_CNT>0) THEN

            IF(LOCAL_CNT==0) RETURN

         ENDIF

      ENDDO ! MM Solids phase loop

!      CALL FINL_ERR_MSG
      RETURN

      END SUBROUTINE WRITE_PARTICLE_XYZ_IN_CSV





!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: GENERATE_PARTICLE_CONFIG (OLD, monodisperse version)    !
!  Authors: Rahul Garg                               Date: 21-Mar-2014 !
!                                                                      !
!  Purpose: Generate particle configuration for DEM solids for each IC !
!           region. Now using the particle linked lists for initial    !
!           build                                                      !
!           This routine will ultimately supersede the older routine   !
!           that has not been deleted yet                              !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE GENERATE_PARTICLE_CONFIG_DEM_0(ICV)

! Global Variables:
!---------------------------------------------------------------------//
! particle radius and density
      use discretelement, only: DES_RADIUS, RO_Sol
! particle position new and old
      use discretelement, only: DES_POS_NEW, DES_POS_OLD
! particle velocity new and old
      use discretelement, only: DES_VEL_NEW, DES_VEL_OLD
! Number of particles in the system (current)
      use discretelement, only: PIP
! Number of DEM solids phases.
      use discretelement, only: DES_MMAX
! Flag to use _OLD variables
      use discretelement, only: DO_OLD
! Angular velocity
      use discretelement, only: OMEGA_OLD, OMEGA_NEW, PIJK
! solid phase diameters and densities.
      use physprop, only: D_p0, RO_s0, MMAX
! IC Region solids volume fraction.
      use ic, only: IC_EP_s, IC_RO_s

! Constant: 3.14159...
      use constant, only: PI
! min and max physical coordinates of IC regions in each direction
      use ic, only: IC_X_w, IC_X_e, IC_Y_s, IC_Y_n, IC_Z_b, IC_Z_t
! initially specified velocity field and granular temperature
      use ic, only: IC_U_s, IC_V_s, IC_W_s, IC_Theta_M
! Flag to extend the lattice distribution in a given IC to available area
      use ic, only: IC_DES_FIT_TO_REGION
! Parameter for detecting unspecified values, zero, and one
      use param1, only: ZERO, Half
! Parameter for small numbers
      use param1, only: SMALL_NUMBER

! to access random number generator subroutines
      use randomno
      use mpi_utility

      use desgrid, only: dg_xstart, dg_ystart, dg_zstart
      use desgrid, only: dg_xend, dg_yend, dg_zend

! direction wise spans of the domain and grid spacing in each direction
      use geometry, only: xlength, ylength, zlength

      use cutcell, only: CARTESIAN_GRID
      use stl_functions_des, only: CHECK_IF_PARTICLE_OVERLAPS_STL
      use run, only: solids_model
      use des_allocate, only: PARTICLE_GROW

      use desgrid, only: IofPOS, JofPOS, KofPOS
      use desgrid, only: dg_is_ON_myPE_OWNs
      use toleranc, only: compare

      use discretelement, only: max_pip, xe, yn, zt
      use functions
      use param, only: dim_m
      use param, only: dimension_i, dimension_j, dimension_k
      use param1, only: undefined
      use ic, only: IC_PIC_CONST_STATWT
      use discretelement, only: cgdem, des_cgp_stw, des_cgp_rpr
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      INTEGER, INTENT(IN) :: ICV

! Local variables
!---------------------------------------------------------------------//
! Starting positions in the axial directions
      DOUBLE PRECISION :: xINIT, yINIT, zINIT
! Fractor used to scale particle diameter
      DOUBLE PRECISION :: lFAC
! Particle position and velocity
      DOUBLE PRECISION :: POS(3), VEL(3)
! Number of particles in the lattice
      INTEGER :: SEED_X, SEED_Y, SEED_Z
! Loop indices phase/fluid cell
      INTEGER :: M, MM, I, J, K, IJK
! Loop indices for seeding
      INTEGER :: II, JJ, KK
! Start and end bound for IC region.
      DOUBLE PRECISION :: IC_START(3), IC_END(3)
! Volume and lengths of the IC Region
      DOUBLE PRECISION :: DOM_VOL, DOML(3)
! Flag to skip the particle
      LOGICAL :: SKIP
! Diameter adjusted for space padding
      DOUBLE PRECISION :: ADJ_DIA
! Number of particles calculated from volume fracton
      INTEGER :: rPARTS(DIM_M), tPARTS
! Spacing between particles.
      DOUBLE PRECISION :: lDEL, lDX, lDY, lDZ
! Flag that the setup failed to fit the particles to the IC region
      LOGICAL :: FIT_FAILED
! Number of seeded particles
      INTEGER :: pCOUNT(DIM_M), tCOUNT

      DOUBLE PRECISION :: SOLIDS_DATA(0:DIM_M), lRO(DIM_M)

      LOGICAL :: VEL_FLUCT
      DOUBLE PRECISION, ALLOCATABLE :: randVEL(:,:)

      DOUBLE PRECISION :: BC_MAX_RADIUS

!......................................................................!

      WRITE(ERR_MSG,"(2/,'Generating initial particle configuration:')")
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

      SOLIDS_DATA = ZERO
      CALL GET_IC_VOLUME(ICV, SOLIDS_DATA(0))

! setting particle seed spacing grid to be slightly greater than
! the maximum particle diameter. seed at ~particle radii
      lFAC = 1.05D0

! Setup local arrays with IC region bounds.
      IC_START(1)=IC_X_W(ICV);   IC_END(1)=IC_X_E(ICV)
      IC_START(2)=IC_Y_S(ICV);   IC_END(2)=IC_Y_N(ICV)
      IC_START(3)=IC_Z_B(ICV);   IC_END(3)=IC_Z_T(ICV)

      DOML = IC_END-IC_START
      IF(NO_K) DOML(3)=DZ(1)

      ! Create a local array of solids densities so we don't have to
      ! reevaluate the function for every particle. Doing this for each
      ! phase is a bit overkill as DEM says that each IC region can only
      ! have on solids.
      DO M=MMAX+1,MMAX+DES_MMAX
         IF(SOLIDS_MODEL(M) == 'DEM' .and. IC_EP_S(ICV,M) > ZERO) THEN
            lRO(M) = IC_RO_S(ICV, M)
         ELSE
            lRO(M) = -UNDEFINED
         END IF
      END DO
      
! Volume of the IC region
      DOM_VOL = DOML(1)*DOML(2)*DOML(3)

      rPARTS=0
      BC_MAX_RADIUS = ZERO
      DO M=MMAX+1,MMAX+DES_MMAX
         IF(SOLIDS_MODEL(M) == 'DEM') THEN
            ! Number of particles for phase M
            rPARTS(M) = &
                 floor((6.0d0*IC_EP_S(ICV,M)*DOM_VOL)/(PI*(D_P0(M)**3)))
! Negative values of rParts indicate overflow. Treat this as a fatal error.
      IF(rPARTS(M)<0) THEN
         WRITE(ERR_MSG,100) ICV, M, DOM_VOL,D_P0(M),(6.0d0*IC_EP_S(ICV,M)*DOM_VOL)/(PI*(D_P0(M)**3))
         CALL LOG_ERROR()
      ENDIF

100 FORMAT('Error 100: Overflow in IC region, IC=',I3,' , M=',I3, &
      /'IC Volume=',ES15.4,' , Particle Diameter=', ES15.4, ' , Number of particles = ',ES15.4, &
      /'This error is usually triggered by attempting to initialize with a very large number of particles.' &
      /'Please verify your settings (Particle size and region extents).', &
      /'This is a fatal error. MFiX will exit now.')
            IF(IC_EP_S(ICV,M) > ZERO .AND. D_P0(M) > BC_MAX_RADIUS)&
                 BC_MAX_RADIUS = HALF * D_P0(M)
         ENDIF
      ENDDO

! Total number of particles in this IC region.
      tPARTS = sum(rPARTS)
      IF(tPARTS == 0) RETURN

      ADJ_DIA = 2.0d0*BC_MAX_RADIUS*lFAC

! Attempt to seed particle throughout the IC region
      FIT_FAILED=.FALSE.
      IF(IC_DES_FIT_TO_REGION(ICV)) THEN
         IF(NO_K) THEN
            lDEL = (DOML(1)-ADJ_DIA)*(DOML(2)-ADJ_DIA)
            lDEL = (lDEL/dble(tPARTS))**(1.0/2.0)
            SEED_X = max(1,ceiling((DOML(1)-ADJ_DIA)/lDEL))
            SEED_Y = max(1,ceiling((DOML(2)-ADJ_DIA)/lDEL))
            SEED_Z = 1
         ELSE
            lDEL = (DOML(1)-ADJ_DIA)*(DOML(2)-ADJ_DIA)*(DOML(3)-ADJ_DIA)
            lDEL = (lDEL/dble(tPARTS))**(1.0/3.0)
            SEED_X = max(1,ceiling((DOML(1)-ADJ_DIA)/lDEL))
            SEED_Y = max(1,ceiling((DOML(2)-ADJ_DIA)/lDEL))
            SEED_Z = max(1,ceiling((DOML(3)-ADJ_DIA)/lDEL))
         ENDIF
         FIT_FAILED=(dble(SEED_X*SEED_Y*SEED_Z) < tPARTS)
      ENDIF

! Generic filling
      IF(.NOT.IC_DES_FIT_TO_REGION(ICV) .OR. FIT_FAILED) THEN
         SEED_X = max(1,floor((IC_END(1)-IC_START(1)-ADJ_DIA)/ADJ_DIA))
         SEED_Y = max(1,floor((IC_END(2)-IC_START(2)-ADJ_DIA)/ADJ_DIA))
         SEED_Z = max(1,floor((IC_END(3)-IC_START(3)-ADJ_DIA)/ADJ_DIA))
      ENDIF

      lDX = DOML(1)/dble(SEED_X)
      lDY = DOML(2)/dble(SEED_Y)
      IF(DO_K) THEN
         lDZ = DOML(3)/dble(SEED_Z)
      ELSE
         lDZ = 0.0d0
      ENDIF

      xINIT = IC_START(1)+HALF*lDX
      yINIT = IC_START(2)+HALF*lDY
      zINIT = IC_START(3)+HALF*lDZ

      M=1
      pCOUNT = 0
      tCOUNT = 0

      VEL_FLUCT = SET_VEL_FLUCT(ICV,M)

      JJ_LP: DO JJ=1, SEED_Y
         POS(2) = YINIT + (JJ-1)*lDY
         IF(compare(POS(2),dg_ystart) .OR. compare(POS(2),dg_yend))    &
            POS(2) = POS(2) + SMALL_NUMBER

      KK_LP: DO KK=1, SEED_Z
         POS(3) = ZINIT + (KK-1)*lDZ
         IF(DO_K) THEN
            IF(compare(POS(3),dg_zstart) .OR. compare(POS(3),dg_zend)) &
               POS(3) = POS(3) + SMALL_NUMBER
         ENDIF

      II_LP: DO II=1, SEED_X
         POS(1) = xINIT + (II-1)*lDX
         IF(compare(POS(1),dg_xstart) .OR. compare(POS(1),dg_xend))    &
            POS(1) = POS(1) + SMALL_NUMBER

! Exit if all particles were seeded.
         IF(tCOUNT > int(tPARTS)) THEN
            EXIT JJ_LP
! Find the next phase that needs to be seeded
         ELSEIF((pCOUNT(M) > int(rPARTS(M))).OR.(rParts(M)==ZERO)) THEN
            MM_LP: DO MM=M+1,MMAX+DES_MMAX
               IF(rPARTS(MM) > 0.0) THEN
                  M=MM
                  EXIT MM_LP
               ENDIF
            ENDDO MM_LP
            IF(M > MMAX+DES_MMAX) EXIT JJ_LP
            VEL_FLUCT = SET_VEL_FLUCT(ICV,M)
         ENDIF

         pCOUNT(M) = pCOUNT(M) + 1
         tCOUNT = tCOUNT + 1

! Keep only particles that belong to this process.
         IF(.NOT.dg_is_ON_myPE_OWNs(IofPOS(POS(1)), &
            JofPOS(POS(2)),KofPOS(POS(3)))) CYCLE

! Bin the parcel to the fuild grid.
         K=1
         IF(DO_K) CALL PIC_SEARCH(K, POS(3), ZT, DIMENSION_K, KMIN2, KMAX2)
         CALL PIC_SEARCH(J, POS(2), YN, DIMENSION_J, JMIN2, JMAX2)
         CALL PIC_SEARCH(I, POS(1), XE, DIMENSION_I, IMIN2, IMAX2)

! Skip cells that return invalid IJKs.
         IF(DEAD_CELL_AT(I,J,K)) CYCLE

! Skip cells that are not part of the local fuild domain.
         IJK = FUNIJK(I,J,K)
         IF(.NOT.FLUID_AT(IJK)) CYCLE

         IF(CARTESIAN_GRID) THEN
            CALL CHECK_IF_PARTICLE_OVERLAPS_STL(POS, I, J, K, SKIP)
            IF(SKIP) CYCLE
         ENDIF

         PIP = PIP + 1
         CALL PARTICLE_GROW(PIP)
         MAX_PIP = max(PIP,MAX_PIP)

         CALL SET_NORMAL(PIP)

         IF(VEL_FLUCT) THEN
            VEL(1) = randVEL(pCOUNT(M),1)
            VEL(2) = randVEL(pCOUNT(M),2)
            VEL(3) = randVEL(pCOUNT(M),3)
         ELSE
            VEL(1) = IC_U_s(ICV,M)
            VEL(2) = IC_V_s(ICV,M)
            VEL(3) = IC_W_s(ICV,M)
         ENDIF
         IF(NO_K) VEL(3) = 0.0d0


         DES_POS_NEW(PIP,:) = POS(:)
         DES_VEL_NEW(PIP,:) = VEL(:)
         OMEGA_NEW(PIP,:) = 0.0d0

         DES_RADIUS(PIP) = D_P0(M)*HALF
         RO_SOL(PIP) =  lRO(M)

         PIJK(PIP,1) = I
         PIJK(PIP,2) = J
         PIJK(PIP,3) = K
         PIJK(PIP,4) = IJK
         PIJK(PIP,5) = M

         IF(DO_OLD) THEN
            DES_VEL_OLD(PIP,:) = DES_VEL_NEW(PIP,:)
            DES_POS_OLD(PIP,:) = DES_POS_NEW(PIP,:)
            OMEGA_OLD(PIP,:) = ZERO
         ENDIF

         print*,'CGDEM=',CGDEM
         IF(CGDEM) THEN
            DES_CGP_STW(PIP) = IC_PIC_CONST_STATWT(ICV,M)
            DES_CGP_RPR(PIP) = DES_RADIUS(PIP)/DES_CGP_STW(PIP)**(1.0d0/3.0d0)
            print*,'PIP,real radiu=',PIP,DES_CGP_RPR(PIP),DES_RADIUS(PIP),DES_CGP_STW(PIP)
      if(DES_CGP_STW(PIP) < 0.99) write(*,*) 'Error! Check IC_PIC_CONST_STATWT in initial domain: ', ICV, 'Phase: ', M
         ENDIF


         SOLIDS_DATA(M) = SOLIDS_DATA(M) + 1.0

      ENDDO II_LP
      ENDDO KK_LP
      ENDDO JJ_LP

! Collect the data
      CALL GLOBAL_ALL_SUM(SOLIDS_DATA)

! Verify that the IC region volume is not zero.
      IF(SOLIDS_DATA(0) <= 0.0d0) THEN
         WRITE(ERR_MSG,1000) ICV, SOLIDS_DATA(0)
         CALL LOG_ERROR()
      ENDIF

1000 FORMAT('Error 1000: Invalid IC region volume: IC=',I3,' VOL=', ES15.4)

      WRITE(ERR_MSG,2000) ICV
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

      DO M=MMAX+1, MMAX+DES_MMAX
         IF(SOLIDS_DATA(M) < SMALL_NUMBER) CYCLE
         WRITE(ERR_MSG,2010) M, int(SOLIDS_DATA(M)), IC_EP_S(ICV,M),   &
            (dble(SOLIDS_DATA(M))*(Pi/6.0d0)*D_P0(M)**3)/SOLIDS_DATA(0)
         CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
      ENDDO

      IF(allocated(randVEL)) deallocate(randVEL)

      RETURN

 2000 FORMAT(/2x,'|',43('-'),'|',/2x,'| IC Region: ',I3,28x,'|',/2x,   &
         '|',43('-'),'|',/2x,'| Phase | Number of |    EPs    |    EP',&
         's    |',/2x,'|   ID  | Particles | Specified |   Actual  |', &
         /2x,'|-------|',3(11('-'),'|'))

 2010 FORMAT(2x,'|  ',I3,'  |',1x,I9,1x,'|',2(1x,ES9.2,1x,'|'),/2x,    &
         '|-------|',3(11('-'),'|'))


      CONTAINS

!......................................................................!
! Function: SET_VEL_FLUCT                                              !
! Purpose: Set the flag for random velocity fluctuations. If needed    !
! the random velocities are calculated.                                !
!......................................................................!
      LOGICAL FUNCTION SET_VEL_FLUCT(lICV, lM)
      INTEGER, INTENT(IN) :: lICV, lM
      DOUBLE PRECISION :: VEL_SIG
      SET_VEL_FLUCT=(IC_Theta_M(lICV,lM) /= 0.0d0)
      IF(SET_VEL_FLUCT) THEN
         if(allocated(randVEL)) deallocate(randVEL)
         allocate(randVEL(100+int(rPARTS(lM)),3))
         VEL_SIG = sqrt(IC_Theta_M(lICV,lM))
         CALL NOR_RNO(randVEL(:,1), IC_U_s(lICV,lM),VEL_SIG)
         CALL NOR_RNO(randVEL(:,2), IC_V_s(lICV,lM),VEL_SIG)
         IF(DO_K) CALL NOR_RNO(randVEL(:,3),IC_W_s(lICV,lM),VEL_SIG)
      ENDIF

      END FUNCTION SET_VEL_FLUCT

      END SUBROUTINE GENERATE_PARTICLE_CONFIG_DEM_0




!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: GENERATE_PARTICLE_CONFIG_MMPPIC                         !
!  Author: Rahul Garg                                 Date: 3-May-2011 !
!                                                                      !
!  Purpose: Generates particle position distribution for MPPIC.        !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE GENERATE_PARTICLE_CONFIG_MPPIC(ICV)

! Global variables
!---------------------------------------------------------------------//
! Number of DES solids phases.
      use discretelement, only: DES_MMAX
! IC Region bulk density (RO_s * EP_s)
      use ic, only: IC_ROP_s
      use ic, only: IC_EP_s
      use ic, only: IC_PIC_CONST_STATWT

      use param1, only: ZERO
      use param, only: DIM_M
      use physprop, only: mmax

! The accumulated number of particles in each IJK.
      use mpi_utility, only: GLOBAL_ALL_SUM

      use run, only: solids_model
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      INTEGER, INTENT(IN) :: ICV

! Local variables
!---------------------------------------------------------------------//
! Generic loop counters
      INTEGER :: M
! Solids data in IC Region by phase:
      double precision :: solids_data(2*dim_m), ic_vol

      integer :: parcels
      double precision :: stat_wt, particles, ic_eps, act_eps

!......................................................................!

      SOLIDS_DATA = 0.0d0

      WRITE(ERR_MSG,"(2/,'Generating initial parcel configuration:')")
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

      CALL GET_IC_VOLUME(ICV, IC_VOL)

      ! Collect the data
      CALL GLOBAL_ALL_SUM(IC_VOL)

      ! Verify that the IC region volume is not zero.
      IF(IC_VOL <= 0.0d0) THEN
         WRITE(ERR_MSG,1000) ICV, IC_VOL
         CALL LOG_ERROR()
      ENDIF

1000  FORMAT('Error 1000: Invalid IC region volume: IC=',I3,' VOL=', ES15.4)

      WRITE(ERR_MSG,2000) ICV
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_WARNING, HEADER=.FALSE., FOOTER=.FALSE.)

! Set up the individual solids phases.
      DO M=MMAX+1, DES_MMAX+MMAX
         IF(SOLIDS_MODEL(M) == 'PIC') THEN
            IF(IC_ROP_S(ICV,M) == ZERO) CYCLE
! Seed parcels with constant statistical weight.
            CALL GPC_MPPIC_CONST_STATWT(ICV, M, &
                 SOLIDS_DATA((2*M-1):(2*M)))
         ENDIF
      ENDDO

      ! Collect the data
      CALL GLOBAL_ALL_SUM(solids_data)

! Report solids information for the IC region.
      DO M=MMAX+1, DES_MMAX+MMAX
         IF(SOLIDS_MODEL(M) == 'PIC') THEN

            stat_wt   = ic_pic_const_statwt(icv,m)
            ic_eps    = ic_ep_s(icv,m)
            parcels   = int(SOLIDS_DATA(2*M-1))
            particles = parcels*stat_wt
            act_eps   = solids_data(2*m)/ic_vol

            WRITE(ERR_MSG,2010) M, parcels, particles, &
                 stat_wt, ic_eps, act_eps
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
         ENDIF
      ENDDO

      RETURN

 2000 FORMAT(/2x,'|',67('-'),'|',/2x,'| IC Region: ',I3,52x,'|',/2x,   &
         '|',67('-'),'|',/2x,'| Phase | Num. Comp | Num. Real ',       &
         '|Statistical|    EPs    |    EPs    |',/2x,'|   ID  |  ',    &
         'Parcels  | Particles |   Weight  | Specified |   Actual  |', &
         /2x,'|-------|',5(11('-'),'|'))

2010  FORMAT(2x,'|  ',I3,'  |',1x,I9,1x,'|',es10.1,1x,'|',3(1x,ES9.2,1x,'|'),/2x,&
         '|-------|',5(11('-'),'|'))

      END SUBROUTINE GENERATE_PARTICLE_CONFIG_MPPIC



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: GENERATE_PARTICLE_CONFIG_MMPPIC                         !
!  Author: Rahul Garg                                 Date: 3-May-2011 !
!                                                                      !
!  Purpose: generates particle position distribution for MPPIC         !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE GPC_MPPIC_CONST_STATWT(ICV, M, sDATA)

! Global variables
!---------------------------------------------------------------------//
! Constant: 3.14159...
      use constant, only: PI
! Cut_cell identifier array
      use discretelement, only: XE, YN, ZT

      use des_allocate, only: PARTICLE_GROW
      use discretelement, only: PIJK, PIP, MAX_PIP
      use discretelement, only:  DES_VEL_NEW, DES_POS_NEW, DES_RADIUS, RO_SOL
      use discretelement, only:  RESIDENCE_TIME

! IC Region bulk density (RO_s * EP_s)
      use ic, only: IC_EP_s, IC_RO_s
      use ic, only: IC_I_w, IC_I_e, IC_J_s, IC_J_n, IC_K_b, IC_K_t

! initially specified velocity field and granular temperature
      use ic, only: IC_U_s, IC_V_s, IC_W_s, IC_Theta_M
      use ic, only: IC_PIC_CONST_STATWT

      use mfix_pic, only: des_stat_wt
      use mpi_utility

      use param1, only: ZERO, HALF

! solid phase diameters and densities.
      use physprop, only: D_p0

      use stl_functions_des, only: picSTLoverlap

      use desgrid, only: dg_funijk
      use desgrid, only: dg_istart2, dg_jstart2, dg_kstart2
      use desgrid, only: dg_iend2,   dg_jend2,   dg_kend2
      use desgrid, only: iofpos,     jofpos,     kofpos

      use randomno
      use functions
      use des_allocate, only: PARTICLE_GROW

      IMPLICIT NONE

! Dummy Arguments
!----------------------------------------------------------------------//
! Index of IC region and solids phase
      INTEGER, INTENT(IN) :: ICV, M
! Data about solids in the IC region.
      DOUBLE PRECISION, INTENT(OUT) :: sDATA(2)

! Local variables
!----------------------------------------------------------------------//

! Number of real and comp. particles in a cell.
      DOUBLE PRECISION ::  rPARTS
      INTEGER :: maxPARTS
      DOUBLE PRECISION :: DOML(3), IC_START(3)
! Parcel position with random
      DOUBLE PRECISION :: POS(3)
! Solids density for phase in IC region
      DOUBLE PRECISION :: lRO
! Average velocity and standard deivation
      DOUBLE PRECISION :: IC_VEL(3), VEL_SIG
! Arrasy for assigning random position and velocities
      DOUBLE PRECISION, ALLOCATABLE :: randVEL(:,:)
      DOUBLE PRECISION :: RAND(3)
! Statistical weights
      DOUBLE PRECISION :: STAT_WT
! Volume of a parcel and total solids volume
      DOUBLE PRECISION :: sVOL, sVOL_TOT, EFF_RAD, remainder
! Counter for seeded parcels.
      INTEGER :: SEEDED
! Generic loop indices and loop counters
      INTEGER :: I, J, K, IJK, LC, LC_MAX
      INTEGER :: dg_I, dg_J, dg_K, dg_IJK, skipped
!......................................................................!

      maxPARTS=25
      allocate(randVEL(maxPARTS,3))

      IC_VEL(1) = IC_U_s(ICV,M)
      IC_VEL(2) = IC_V_s(ICV,M)
      IC_VEL(3) = merge(IC_W_s(ICV,M),0.0d0,DO_K)

      VEL_SIG = sqrt(IC_Theta_M(ICV,M))

! Volume occupied by one particle
      sVOL = (Pi/6.0d0)*(D_P0(M)**3.d0)

      SEEDED = 0
      sVOL_TOT = 0.0d0

      remainder = 0.0d0

      STAT_WT = IC_PIC_CONST_STATWT(ICV,M)

      lRO   = IC_RO_S(ICV, M)

      DO K = IC_K_B(ICV), IC_K_T(ICV)
      DO J = IC_J_S(ICV), IC_J_N(ICV)
      DO I = IC_I_W(ICV), IC_I_E(ICV)

         IF(.not.IS_ON_myPE_wobnd(I,J,K)) cycle
         IF(DEAD_CELL_AT(I,J,K)) cycle

         IJK = FUNIJK(I,J,K)
         IF(.not.FLUID_AT(IJK)) cycle

         rPARTS = IC_EP_s(ICV,M)*VOL(IJK)/sVOL + remainder

! Seed parcels with a constant statistical weight
         LC_MAX = floor(rPARTS/STAT_WT)

         remainder = max(rPARTS - dble(LC_MAX)*STAT_WT, 0.0d0)

         IF(LC_MAX == 0) cycle

! Increase particle buffer
         IF(LC_MAX > maxPARTS) THEN
            maxPARTS = 2*LC_MAX
            if(allocated(randVEL)) deallocate(randVEL)
            allocate(randVEL(maxPARTS,3))
         ENDIF

         DO LC=1, merge(2,3,NO_K)
            IF(VEL_SIG > ZERO) THEN
               CALL NOR_RNO(randVEL(1:LC_MAX,LC), IC_VEL(LC), VEL_SIG)
            ELSE
               randVEL(1:LC_MAX,LC) = IC_VEL(LC)
            ENDIF
         ENDDO
         IF(NO_K) randVEL(1:LC_MAX,3) = 0.0d0

         IC_START(1) = XE(I-1)
         IC_START(2) = YN(J-1)
         IC_START(3) = ZERO;  IF(DO_K) IC_START(3) = ZT(K-1)

         DOML(1) = DX(I)
         DOML(2) = DY(J)
         DOML(3) = ZERO;  IF(DO_K) DOML(3) = DZ(K)

         EFF_RAD = D_P0(M)*HALF*(STAT_WT)**(1.0/3.0)

         PIC_LP: DO LC=1,LC_MAX

            CALL RANDOM_NUMBER(RAND)
            POS(:) = IC_START + DOML*RAND

            dg_i = min(dg_iend2,max(dg_istart2, iofpos(pos(1))))
            dg_j = min(dg_jend2,max(dg_jstart2, jofpos(pos(2))))
            dg_k = min(dg_kend2,max(dg_kstart2, kofpos(pos(3))))
            dg_ijk = dg_funijk(dg_i,dg_j,dg_k)

            skipped = 0
            do while(picSTLoverlap(dg_ijk, pos, eff_rad))

               skipped = skipped + 1
               call random_number(rand)
               pos(:) = ic_start + doml*rand

               dg_i = min(dg_iend2,max(dg_istart2, iofpos(pos(1))))
               dg_j = min(dg_jend2,max(dg_jstart2, jofpos(pos(2))))
               dg_k = min(dg_kend2,max(dg_kstart2, kofpos(pos(3))))
               dg_ijk = dg_funijk(dg_i,dg_j,dg_k)

               if(skipped >= 10 ) cycle pic_lp
            enddo


            PIP = PIP + 1
            CALL PARTICLE_GROW(PIP)
            MAX_PIP = max(PIP,MAX_PIP)

            DES_POS_NEW(PIP,:) = POS(:)
            DES_VEL_NEW(PIP,:) = 1.0e-4*rand!randVEL(LC,:)

            DES_RADIUS(PIP) = D_P0(M)*HALF
            RO_SOL(PIP) =  lRO

            PIJK(PIP,1) = I
            PIJK(PIP,2) = J
            PIJK(PIP,3) = K
            PIJK(PIP,4) = IJK
            PIJK(PIP,5) = M

            DES_STAT_WT(PIP) = STAT_WT
            sVOL_TOT = sVOL_TOT + sVOL*STAT_WT

            RESIDENCE_TIME(PIP) = ZERO

            CALL SET_NORMAL(PIP)

            SEEDED = SEEDED + 1

         ENDDO PIC_LP

      ENDDO
      ENDDO
      ENDDO

      IF(allocated(randVEL)) deallocate(randVEL)

      sDATA(1) = dble(SEEDED)
      sDATA(2) = sVOL_TOT

      RETURN
      END SUBROUTINE GPC_MPPIC_CONST_STATWT

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: GET_IC_VOLUME                                           !
!  Author: J.Musser                                 Date: 26-Aug-2015  !
!                                                                      !
!  Purpose: Calculate the actual volume of the IC region.              !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE GET_IC_VOLUME(ICV, IC_VOL)

! IC region index bounds
      use ic, only: IC_I_w, IC_I_e
      use ic, only: IC_J_s, IC_J_n
      use ic, only: IC_K_b, IC_K_t

! Volume of computational cells.
      use geometry, only: VOL

      use functions
      use compar, only: dead_cell_at

      IMPLICIT NONE

! Dummy Arguments
!---------------------------------------------------------------------//
! Index of IC region
      INTEGER, INTENT(IN) :: ICV
! Total calculated volume of IC region
      DOUBLE PRECISION, INTENT(OUT) :: IC_VOL

! Local variables
!---------------------------------------------------------------------//
      INTEGER :: I, J, K, IJK
!......................................................................!


      IC_VOL = 0.0d0
      DO K = IC_K_B(ICV), IC_K_T(ICV)
      DO J = IC_J_S(ICV), IC_J_N(ICV)
      DO I = IC_I_W(ICV), IC_I_E(ICV)

         IF(.NOT.IS_ON_MYPE_WOBND(I,J,K)) CYCLE
         IF(DEAD_CELL_AT(I,J,K)) CYCLE

         IJK = FUNIJK(I,J,K)
         IF(FLUID_AT(IJK)) IC_VOL = IC_VOL + VOL(IJK)

      ENDDO
      ENDDO
      ENDDO

      RETURN

   END SUBROUTINE GET_IC_VOLUME

END MODULE GENERATE_PARTICLES
