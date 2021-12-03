#include "error.inc"

MODULE CHECK_SOLIDS_COMMON_DISCRETE_MOD

   use discretelement, only: des_explicitly_coupled
   use error_manager

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: CHECK_SOLIDS_COMMON_DISCRETE                            !
!  Author: J.Musser                                   Date: 02-FEB-14  !
!                                                                      !
!  Purpose:                                                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_SOLIDS_COMMON_DISCRETE

! Modules
!---------------------------------------------------------------------//
! Runtime Flag: Generate initial particle configuration.
      USE discretelement, only: GENER_PART_CONFIG
! Runtime Flag: Store DES_*_OLD arrays.
      USE discretelement, only: DO_OLD
! Number of DEM solids phases.
      USE discretelement, only: DES_MMAX

! Particle size distribution under initial condition
      USE ic, only: IC_PSD_TYPE
      USE ic, only: IC_PSD_MEAN_DP
      USE ic, only: IC_PSD_STDEV
      USE ic, only: IC_PSD_MAX_DP
      USE ic, only: IC_PSD_MIN_DP
      USE ic, only: IC_PSD_MU
      USE ic, only: IC_PSD_SIGMA
      USE ic, only: IC_EP_g

! User specified integration method.
      USE discretelement, only: DES_INTG_METHOD
      USE discretelement, only: INTG_ADAMS_BASHFORTH
      USE discretelement, only: INTG_EULER
! User specified neighbor search method.
      USE discretelement, only: DES_NEIGHBOR_SEARCH
! User specified data out format (VTP, TecPlot)
      USE discretelement, only: DES_OUTPUT_TYPE
! Max/Min particle radii
      USE discretelement, only: MAX_RADIUS, MIN_RADIUS
! Runtime Flag: Periodic boundaries
      USE discretelement, only: DES_PERIODIC_WALLS
      USE discretelement, only: DES_PERIODIC_WALLS_X
      USE discretelement, only: DES_PERIODIC_WALLS_Y
      USE discretelement, only: DES_PERIODIC_WALLS_Z
! Runtime Flag: Invoke MPPIC model.
      USE mfix_pic, only: MPPIC
      USE mpi_utility

      use param1, only: undefined, undefined_c, zero
      use param, only: dim_m
! number of continuous solids phases and
! solids 'phase' diameters and densities
      USE physprop, only: MMAX, D_p0
! Calculated baseline variable solids density.
      USE physprop, only: CLOSE_PACKED
! Runtime Flag: Solve energy equations
      USE run, only: ENERGY_EQ
! Runtime Flag: One or more species equations are solved.
      use run, only: ANY_SPECIES_EQ
! Flag: Solve variable solids density.
      USE run, only: MOMENTUM_X_EQ
      USE run, only: MOMENTUM_Y_EQ
      USE run, only: MOMENTUM_Z_EQ
      use run, only: RUN_TYPE

! for CGDEM CHECK  
! Number of real particles in a parcel
      use ic, only: IC_PIC_CONST_STATWT, IC_DEFINED, IC_EP_S
      USE ic, only: IC_DES_SM, IC_DES_NP
      use discretelement, only: CGDEM, cgp_stat_wt,cgp_d_p0, cgp_scaling_method
      use physprop, only: d_p0,DIM_M
      use param, only: DIMENSION_IC
    
      use des_bc
      use bc, only:MASS_INFLOW, bc_type_enum, bc_pic_mi_const_statwt, bc_ep_s
      use bc, only: bc_type, bc_defined
      use run, only: SOLIDS_MODEL      
      implicit none

! Local Variables
!---------------------------------------------------------------------//
      INTEGER :: M
      INTEGER :: ICV, BCV
      DOUBLE PRECISION :: mu_star, sigma_star, mode,variance, skewness
!......................................................................!

! scale d_p0 according to CGP_STAT_WT
! Assume a same statwt is used for particles belong to a same phase
! D_p0 is then used to setup collision time, particle searching grids,  solids inlets

      IF(CGDEM .and. .not. MPPIC) THEN
         DO M = MMAX+1,DES_MMAX+MMAX

            IF(CGP_STAT_WT(M)>=1.0D0.AND.CGP_D_P0(M)>ZERO) THEN
               WRITE(ERR_MSG,400) M, CGP_STAT_WT(M),CGP_D_P0(M)
               CALL LOG_ERROR()
            ENDIF

            IF(CGP_STAT_WT(M)==ZERO.AND.CGP_D_P0(M)==ZERO) THEN
               WRITE(ERR_MSG,410) M, CGP_STAT_WT(M),CGP_D_P0(M)
               CALL LOG_ERROR()
            ENDIF

            IF(CGP_STAT_WT(M)>=1.0D0.AND.CGP_D_P0(M)==ZERO) THEN
               CGP_D_p0(M) = D_p0(M) * CGP_STAT_WT(M)**(1.d0/3.d0)
               CGP_SCALING_METHOD(M) = 1
               WRITE(ERR_MSG,420)M,D_p0(M),CGP_STAT_WT(M),CGP_D_P0(M)
               CALL LOG_INFO()
420 FORMAT('Info: Using Coarse Grain DEM, scaling particles using statistical weight for CGP solids phase: ',I2/, &
           ' Specified real particle diameter        = ', G9.2/,&
           ' Specified statistical weight            = ', G9.2/,&
           ' Computed Coarse Grain particle diameter = ', G9.2)
            ENDIF


            IF(CGP_STAT_WT(M)==ZERO.AND.CGP_D_P0(M)>ZERO) THEN

               IF(CGP_D_P0(M)<D_P0(M)) THEN
                  WRITE(ERR_MSG,440) M, D_P0(M), CGP_D_P0(M)
                  CALL LOG_ERROR()
               ENDIF

               CGP_STAT_WT(M) = (CGP_D_p0(M)/D_p0(M))**3
               CGP_SCALING_METHOD(M) = 2
               WRITE(ERR_MSG,430)M,D_p0(M),CGP_D_P0(M),CGP_STAT_WT(M)
               CALL LOG_INFO()
430 FORMAT('Info: Using Coarse Grain DEM, scaling particles using Coarse Grain Particle size for CGP solids phase: ',I2/, &
           ' Specified real particle diameter         = ', G9.2/,&
           ' Specified Coarse Grain particle diameter = ', G9.2/,&
           ' Computed statistical weight              = ', G9.2)
            ENDIF

            IF(CGP_STAT_WT(M)<1.0D0) THEN
               WRITE(ERR_MSG,500) M, CGP_STAT_WT(M) 
               CALL LOG_ERROR()
            ENDIF

            D_p0(M) = D_p0(M) * CGP_STAT_WT(M)**(1.d0/3.d0)
         ENDDO
      ENDIF

400 FORMAT('Error 400: It is not allowed to specify both a statistical weight and '/,&
           'a Coarse Grain particle size for Coarse Grain Solids Phase:',I2/, &
           'Current value of CGP_STAT_WT =', G9.2/, &
           'Current value of CGP_D_P0    =', G9.2)

410 FORMAT('Error 410: Either a statistical weight or a Coarse Grain particle size'/,&
           'must be specified for Coarse Grain Solids Phase:',I2/, &
           'Current value of CGP_STAT_WT =', G9.2/, &
           'Current value of CGP_D_P0    =', G9.2)

440 FORMAT('Error 440: The Coarse Grain particle size'/,&
           'must be larger than the real particle size for Solids Phase:',I2/, &
           'Current value of real particle size,             D_P0 =', G9.2/, &
           'Current value of Coarse Grain particle size, CGP_D_P0 =', G9.2)

500 FORMAT('Error 500: Enter a valid statistical weight for Coarse Grain Solids Phase:',I2/, &
           'Current value of CGP_STAT_WT =', G9.2/, &
           'Valid values must be larger than 1.0.')

      
      
! Determine the maximum particle size in the system (MAX_RADIUS), which
! in turn is used for various tasks
      MAX_RADIUS = -UNDEFINED
      MIN_RADIUS =  UNDEFINED
! For number of continuous solids phases (use MMAX rather than SMAX to
! accommodate GHD particularity)
      DO M = MMAX+1,DES_MMAX+MMAX
         MAX_RADIUS = MAX(MAX_RADIUS, 0.5d0*D_P0(M))
         MIN_RADIUS = MIN(MIN_RADIUS, 0.5d0*D_P0(M))
      ENDDO

      IC_LOOP: DO ICV=1, DIMENSION_IC

         DO M = MMAX+1,DES_MMAX+MMAX

! Resising the PSD for CGDEM               
            IF(CGDEM .and. .not. MPPIC.and.TRIM(IC_PSD_TYPE(ICV,M))/='MONO'.and.CGP_SCALING_METHOD(M)==1) THEN
               IF(IC_PSD_MEAN_DP(ICV,M) /= UNDEFINED) IC_PSD_MEAN_DP(ICV,M) = IC_PSD_MEAN_DP(ICV,M) * CGP_STAT_WT(M)**(1.d0/3.d0)
               IF(IC_PSD_STDEV(ICV,M) /= UNDEFINED) IC_PSD_STDEV(ICV,M) = IC_PSD_STDEV(ICV,M) * CGP_STAT_WT(M)**(1.d0/3.d0)
               IF(IC_PSD_MIN_DP(ICV,M) /= UNDEFINED) IC_PSD_MIN_DP(ICV,M) = IC_PSD_MIN_DP(ICV,M) * CGP_STAT_WT(M)**(1.d0/3.d0)
               IF(IC_PSD_MAX_DP(ICV,M) /= UNDEFINED) IC_PSD_MAX_DP(ICV,M) = IC_PSD_MAX_DP(ICV,M) * CGP_STAT_WT(M)**(1.d0/3.d0)
               WRITE(ERR_MSG,600)ICV,M,CGP_STAT_WT(M)
               CALL LOG_INFO()
            ENDIF

600 FORMAT('Info: Using Coarse Grain DEM, PSD is scaled up for ICV= ',I3, ', solids phase: ',I2/, &
           ' and a statistical weight of:', G9.2)

! Normal distribution
            if(IC_PSD_TYPE(ICV,M) == 'NORMAL') then

               if(IC_PSD_MEAN_DP(ICV,M) == UNDEFINED) then
                  WRITE(ERR_MSG,1000) trim(iVar('IC_PSD_MEAN_DP',ICV,M))
                  CALL LOG_ERROR()
               endif

               if(IC_PSD_STDEV(ICV,M) == UNDEFINED) then
                  WRITE(ERR_MSG,1000) trim(iVar('IC_PSD_STDEV',ICV,M))
                  CALL LOG_ERROR()
               endif


! For normal distribution without min/max bounds, assign 6-Sigma
               if(IC_PSD_MIN_DP(ICV,M) == UNDEFINED) &
                  IC_PSD_MIN_DP(ICV,M) = IC_PSD_MEAN_DP(ICV,M) - 3.D0*IC_PSD_STDEV(ICV,M)

               if(IC_PSD_MAX_DP(ICV,M) == UNDEFINED) &
                  IC_PSD_MAX_DP(ICV,M) = IC_PSD_MEAN_DP(ICV,M) + 3.D0*IC_PSD_STDEV(ICV,M)

               if(IC_PSD_MIN_DP(ICV, M).le.0.D0) then
                 WRITE(ERR_MSG,1500) ICV, M
                 CALL LOG_ERROR()
               ENDIF

               if(IC_PSD_MAX_DP(ICV, M).le.0.D0 .or. IC_PSD_MAX_DP(ICV, M)<IC_PSD_MIN_DP(ICV, M)) then
                 WRITE(ERR_MSG,1600) ICV, M
                 CALL LOG_ERROR()
               ENDIF

               WRITE(ERR_MSG,1100) ICV,M,IC_PSD_TYPE(ICV,M),IC_PSD_MEAN_DP(ICV,M),IC_PSD_STDEV(ICV,M),IC_PSD_MIN_DP(ICV,M),IC_PSD_MAX_DP(ICV,M), &
                     IC_PSD_MEAN_DP(ICV,M)-IC_PSD_STDEV(ICV,M),IC_PSD_MEAN_DP(ICV,M)+IC_PSD_STDEV(ICV,M),&            
                     IC_PSD_MEAN_DP(ICV,M)-2.0D0*IC_PSD_STDEV(ICV,M),IC_PSD_MEAN_DP(ICV,M)+2.0D0*IC_PSD_STDEV(ICV,M),&            
                     IC_PSD_MEAN_DP(ICV,M)-3.0D0*IC_PSD_STDEV(ICV,M),IC_PSD_MEAN_DP(ICV,M)+3.0D0*IC_PSD_STDEV(ICV,M)
               CALL LOG_INFO()

 1100 FORMAT( 'Info: DEM Particle size distribution within initial condition for ICV=',I3, ' M=',I3, &
             /'Type                       = ',A, &
             /'Mean                       = ',F14.8, &
             /'Standard deviation         = ',F14.8,&
             /'Minimum (clipped) diameter = ',F14.8,&
             /'Maximum (clipped) diameter = ',F14.8&
             /'Theoretical statistical values:',&
             /'68.0% distribution range   = [',F14.8,' ;',F14.8,']',&
             /'95.0% distribution range   = [',F14.8,' ;',F14.8,']',&
             /'99.7% distribution range   = [',F14.8,' ;',F14.8,']'&
             )

            endif

! Log-Normal distribution
            if(IC_PSD_TYPE(ICV,M) == 'LOG_NORMAL') then

               if(IC_PSD_MEAN_DP(ICV,M) == UNDEFINED) then
                  WRITE(ERR_MSG,1000) trim(iVar('IC_PSD_MEAN_DP',ICV,M))
                  CALL LOG_ERROR()
               endif

               if(IC_PSD_STDEV(ICV,M) == UNDEFINED) then
                  WRITE(ERR_MSG,1000) trim(iVar('IC_PSD_STDEV',ICV,M))
                  CALL LOG_ERROR()
               endif

! Convert mean and std dev into log-normal parameters mu and sigma               
               IC_PSD_MU(ICV,M) = LOG(IC_PSD_MEAN_DP(ICV,M)**2/SQRT(IC_PSD_MEAN_DP(ICV,M)**2+IC_PSD_STDEV(ICV,M)**2))
               IC_PSD_SIGMA(ICV,M) = SQRT(LOG(1.0D0 + (IC_PSD_STDEV(ICV,M)/IC_PSD_MEAN_DP(ICV,M))**2))

! Median               
               mu_star = exp(IC_PSD_MU(ICV,M))
! Scatter               
               sigma_star = exp(IC_PSD_SIGMA(ICV,M))

               mode = exp(IC_PSD_MU(ICV,M) - IC_PSD_SIGMA(ICV,M)**2) 
               variance = (exp(IC_PSD_SIGMA(ICV,M)**2) - 1.0D0)*exp(2.0D0*IC_PSD_MU(ICV,M) + IC_PSD_SIGMA(ICV,M)**2 )
               skewness = (exp(IC_PSD_SIGMA(ICV,M)**2) + 2.0D0)*DSQRT((exp(IC_PSD_SIGMA(ICV,M)**2) - 1.0D0))

               if(IC_PSD_MIN_DP(ICV,M) == UNDEFINED) &
                  IC_PSD_MIN_DP(ICV,M) = mu_star/sigma_star**3 

               if(IC_PSD_MAX_DP(ICV,M) == UNDEFINED) &
                  IC_PSD_MAX_DP(ICV,M) = mu_star*sigma_star**3 

               if(IC_PSD_MIN_DP(ICV, M).le.0.D0) then
                 WRITE(ERR_MSG,1500) ICV, M
                 CALL LOG_ERROR()
               ENDIF

               if(IC_PSD_MAX_DP(ICV, M).le.0.D0 .or. IC_PSD_MAX_DP(ICV, M)<IC_PSD_MIN_DP(ICV, M)) then
                 WRITE(ERR_MSG,1600) ICV, M
                 CALL LOG_ERROR()
               ENDIF

               WRITE(ERR_MSG,1200) ICV,M,IC_PSD_TYPE(ICV,M),IC_PSD_MEAN_DP(ICV,M),IC_PSD_STDEV(ICV,M), &
                     IC_PSD_MIN_DP(ICV,M),IC_PSD_MAX_DP(ICV,M), IC_PSD_MU(ICV,M), IC_PSD_SIGMA(ICV,M), &
                     mu_star, mode, variance, skewness,&
                     mu_star/sigma_star, mu_star*sigma_star,&
                     mu_star/sigma_star**2, mu_star*sigma_star**2,&
                     mu_star/sigma_star**3, mu_star*sigma_star**3
               CALL LOG_INFO()

 1200 FORMAT( 'Info: DEM Particle size distribution within initial condition for ICV=',I3, ', M=',I3, &
             /'Type                       = ',A, &
             /'Mean                       = ',F14.8, &
             /'Standard deviation         = ',F14.8,&
             /'Minimum (clipped) diameter = ',F14.8,&
             /'Maximum (clipped) diameter = ',F14.8,&
             /'Converted mu value         = ',F14.8,&
             /'Converted sigma value      = ',F14.8,&
             /'Theoretical statistical values:',&
             /'Median diameter            = ',F14.8,&
             /'Mode diameter              = ',F14.8,&
             /'Variance                   = ',F14.8,&
             /'Skewness                   = ',F14.8,&
             /'68.0% distribution range   = [',F14.8,' ;',F14.8,']',&
             /'95.0% distribution range   = [',F14.8,' ;',F14.8,']',&
             /'99.7% distribution range   = [',F14.8,' ;',F14.8,']'&
             )


            endif

            if(IC_PSD_MIN_DP(ICV, M).le.0.D0) then
              WRITE(ERR_MSG,1500) ICV, M
              CALL LOG_ERROR()
            ENDIF

            if(IC_PSD_MAX_DP(ICV, M).le.0.D0 .or. IC_PSD_MAX_DP(ICV, M)<IC_PSD_MIN_DP(ICV, M)) then
              WRITE(ERR_MSG,1600) ICV, M
              CALL LOG_ERROR()
            ENDIF

 1000 FORMAT('Error 1000: Required input not specified: ',A)
 1500 FORMAT('Error 1500: Enter a valid IC_PSD_MIN_DP for ICV :',I2, &
         ' and phase M :',I2)
 1600 FORMAT('Error 1600: Enter a valid IC_PSD_MAX_DP for ICV :',I2, &
         ' and phase M :',I2)

!@JFD: For custom distribution, IC_PSD_MIN_DP and the distribution's actual minimum
! could be different. Should we adjust min_radius based on the actual minimum radius of 
! particles generated ?!?
            if(IC_PSD_MIN_DP(ICV, M)/=UNDEFINED) MIN_RADIUS = MIN(MIN_RADIUS, 0.5d0*IC_PSD_MIN_DP(ICV, M))
            if(IC_PSD_MAX_DP(ICV, M)/=UNDEFINED) MAX_RADIUS = MAX(MAX_RADIUS, 0.5d0*IC_PSD_MAX_DP(ICV, M))

         ENDDO
      ENDDO IC_LOOP

! Set close_packed to true to prevent possible issues stemming from the
! pressure correction equation.  Specifically, if closed_packed is false
! then a mixture pressure correction equation is invoked and this is not
! correctly setup for DEM.  To do so would require ensuring that
! 1) the solids phase continuum quantities used in these equations are
!    correctly set based on their DEM counterparts and
! 2) the pressure correction coefficients for such solids phases are
!    also calculated (currently these calculations are turned off
!    when using DEM)
      CLOSE_PACKED((MMAX+1):DIM_M) = .TRUE.


! Turn off the 'continuum' equations for discrete solids if the user
! specified them.  We could make use of these flags.
      MOMENTUM_X_EQ((MMAX+1):DIM_M) = .FALSE.
      MOMENTUM_Y_EQ((MMAX+1):DIM_M) = .FALSE.
      MOMENTUM_Z_EQ((MMAX+1):DIM_M) = .FALSE.

! Derive periodicity from cyclic boundary flags.
      DES_PERIODIC_WALLS_X = CYCLIC_X .OR. CYCLIC_X_PD
      DES_PERIODIC_WALLS_Y = CYCLIC_Y .OR. CYCLIC_Y_PD
      DES_PERIODIC_WALLS_Z = CYCLIC_Z .OR. CYCLIC_Z_PD

      DES_PERIODIC_WALLS = (DES_PERIODIC_WALLS_X .OR.                  &
        DES_PERIODIC_WALLS_Y .OR. DES_PERIODIC_WALLS_Z)


! Overwrite for restart cases.
      IF(TRIM(RUN_TYPE) .NE. 'NEW') GENER_PART_CONFIG = .FALSE.

! Check for valid neighbor search option.
      SELECT CASE(DES_NEIGHBOR_SEARCH)
      CASE (1) ! N-Square
      CASE (2)
         WRITE(ERR_MSG,2001) 2, 'QUADTREE'
         CALL LOG_ERROR()
      CASE (3)
         WRITE(ERR_MSG,2001) 3, 'OCTREE'
         CALL LOG_ERROR()
      CASE (4) ! Grid based
      CASE DEFAULT
         WRITE(ERR_MSG,2001) DES_NEIGHBOR_SEARCH,'UNKNOWN'
         CALL LOG_ERROR()

 2001 FORMAT('Error 2001:Invalid DES_NEIGHBOR_SEARCH method: ',I2,1X,  &
         A,/'Please correct the project settings.')

      END SELECT


! Check the output file format
      IF(DES_OUTPUT_TYPE == UNDEFINED_C) DES_OUTPUT_TYPE = 'PARAVIEW'
      SELECT CASE(trim(DES_OUTPUT_TYPE))
      CASE ('PARAVIEW')
      CASE ('TECPLOT')
      CASE DEFAULT
         WRITE(ERR_MSG,2010) trim(DES_OUTPUT_TYPE)
         CALL LOG_ERROR()

 2010 FORMAT('Error 2010:Invalid DES_OUTPUT_TYPE: ',A,/'Please ',       &
         'correct the project settings.')

      END SELECT


! Check for valid integration method
      SELECT CASE(trim(DES_INTG_METHOD))
      CASE ('EULER')
         INTG_EULER = .TRUE.
         INTG_ADAMS_BASHFORTH = .FALSE.
         !DES_INTG_METHOD_ENUM = 1
      CASE ('ADAMS_BASHFORTH')
         INTG_EULER = .FALSE.
         INTG_ADAMS_BASHFORTH = .TRUE.
         !DES_INTG_METHOD_ENUM = 2
      CASE DEFAULT
         WRITE(ERR_MSG,2020) trim(DES_INTG_METHOD)
         CALL LOG_ERROR()

 2020 FORMAT('Error 2020:Invalid DES_INGT_METHOD: ',A,/'Please ',      &
         'correct the project settings.')

      END SELECT

      DO_OLD = INTG_ADAMS_BASHFORTH .OR. MPPIC

! Check interpolation input.
      CALL CHECK_SOLIDS_COMMON_DISCRETE_INTERP

! Set flags for energy equations
      IF(ENERGY_EQ) CALL CHECK_SOLIDS_COMMON_DISCRETE_ENERGY

! Check thermodynamic properties of discrete solids.
      IF(ANY_SPECIES_EQ) &
         CALL CHECK_SOLIDS_COMMON_DISCRETE_THERMO

! Check geometry constrains.
      CALL CHECK_SOLIDS_COMMON_DISCRETE_GEOMETRY

      RETURN

      END SUBROUTINE CHECK_SOLIDS_COMMON_DISCRETE


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE CHECK_SOLIDS_COMMON_DISCRETE_ENERGY                      !
!  Author: J.Musser                                   Date: 02-FEB-14  !
!                                                                      !
!  Purpose: Check input parameters for solving discrete solids phase   !
!  energy equations.  Only DEM simulations (neither hybrid nor MPPIC)  !
!  can invoke particle-particle heat transfer. Therefore, checks for   !
!  those functions are reserved for later.                              !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_SOLIDS_COMMON_DISCRETE_ENERGY

! Modules
!---------------------------------------------------------------------//
      use param1, only: ZERO, UNDEFINED
      use des_thermo, only: DES_CONV_CORR
      use des_thermo, only: DES_CONV_CORR_ENUM
      use des_thermo, only: RANZ_1952, WAKAO, GUNN, Tavassoli
      use des_thermo, only: CALC_CONV_DES ! Convection
      use des_thermo, only: CALC_COND_DES ! Conduction
      use des_thermo, only: CALC_RADT_DES ! Radiation
! Flag to explicitly couple source terms and DES
      use discretelement, only: DES_MMAX
      use discretelement, only: DES_CONTINUUM_COUPLED
! User input for DES interpolation scheme.
      use particle_filter, only: DES_INTERP_SCHEME
! Enumerated interpolation scheme for faster access
      use particle_filter, only: DES_INTERP_SCHEME_ENUM
      use particle_filter, only: DES_INTERP_NONE

      use physprop, only: MMAX
      use run, only: ks_model
      use mfix_pic, only: MPPIC

      IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//
! Loop counter
      INTEGER :: M
      INTEGER :: zero_ks0, undef_ks0

! Set runtime flags for which modes of heat transfer to calculate.
      CALC_CONV_DES = DES_CONTINUUM_COUPLED

! Disable flags for radiation for MPPIC
      IF (MPPIC) THEN
         CALC_RADT_DES = .FALSE.
      ENDIF

! Disable flags for conduction for MPPIC
      IF(MPPIC) THEN
         CALC_COND_DES = .FALSE.
      ELSE
! Check KS_MODEL
! Set flags for conduction in dem specific routine
         DO M = MMAX+1, MMAX+DES_MMAX
            IF (KS_MODEL(M) == UNDEFINED_C) THEN
               WRITE(ERR_MSG, 1001) trim(iVar('Ks_model',M)), &
                  trim(Ks_model(M))
                  CALL LOG_ERROR()
            ENDIF
         ENDDO
      ENDIF


! Gas/Solids convection:
!'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
! Verify the selected convective heat transfer coefficient model
      SELECT CASE(TRIM(DES_CONV_CORR))
! Ranz, W.E. and Marshall, W.R., "Friction and transfer coefficients
! for single particles and packed beds,"  Chemical Engineering Science,
! Vol. 48, No. 5, pp 247-253, 1952.
      CASE ('RANZ_1952')
         DES_CONV_CORR_ENUM = RANZ_1952
      CASE ('WAKAO')
         DES_CONV_CORR_ENUM = WAKAO
      CASE ('GUNN')
         DES_CONV_CORR_ENUM = GUNN
      CASE ('Tavassoli')
         DES_CONV_CORR_ENUM = Tavassoli
! If the heat transfer coefficient correlation provided by the user does
! not match one of the models outlined above, flag the error and exit.
      CASE DEFAULT
         WRITE(ERR_MSG,1001)'DES_CONV_CORR', trim(DES_CONV_CORR)
         CALL LOG_ERROR()
      END SELECT

! Notify that interpolation is not support for thermo variables
      SELECT CASE(DES_INTERP_SCHEME_ENUM)
      CASE(DES_INTERP_NONE)
      CASE DEFAULT
         WRITE(ERR_MSG,2000) trim(adjustl(DES_INTERP_SCHEME))
         CALL LOG_INFO()
      END SELECT

 2000 FORMAT('WARNING 2000: The selected interpolation scheme (',A,    &
         ') is not',/'supported by the DES energy equation implemen',  &
         'tation. All energy',/'equation variables will use the ',     &
         'centroid method for interphase',/'data exchange.')

      IF(DES_EXPLICITLY_COUPLED)THEN
!llu, comment out to allow interplotion
    !     WRITE(ERR_MSG, 2100)
    !     CALL LOG_ERROR()
      ENDIF

 2100 FORMAT('Error 2100: The DES Energy equation implementation ',    &
         'does not',/'currently support explicit coupling (DES_',      &
         'EXPLICITLY_COUPLED).',/'Please correct the project settings.')

      RETURN

 1000 FORMAT('Error 1000: Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

 1001 FORMAT('Error 1001: Illegal or unknown input: ',A,' = ',A,/   &
         'Please correct the project settings.')

      END SUBROUTINE CHECK_SOLIDS_COMMON_DISCRETE_ENERGY


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_SOLIDS_COMMON_DISCRETE_THERMO                     !
!  Author: J.Musser                                   Date: 17-Jun-10  !
!                                                                      !
!  Purpose:                                                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_SOLIDS_COMMON_DISCRETE_THERMO

! Modules
!---------------------------------------------------------------------//
      use discretelement, only: DES_EXPLICITLY_COUPLED
! User input for DES interpolation scheme.
      use particle_filter, only: DES_INTERP_SCHEME
! Enumerated interpolation scheme for faster access
      use particle_filter, only: DES_INTERP_SCHEME_ENUM
      use particle_filter, only: DES_INTERP_NONE
      use stiff_chem, only: STIFF_CHEMISTRY
      IMPLICIT NONE

! Stiff chemistry solver was originally only for TFM reaction model
! but some extension has been done for a limited scenario in the DES
! model.
      IF(STIFF_CHEMISTRY) THEN
         IF (.NOT.DES_EXPLICITLY_COUPLED) THEN
            WRITE(ERR_MSG,2200)
            CALL LOG_ERROR()
         ENDIF
      ENDIF
 2200 FORMAT('Error 2200: The DES implementation of the stiff ',&
         'chemistry solver',/,'only supports explicit coupling ',&
         '(DES_EXPLICITLY_COUPLED).',/,'Please correct the input file.')


! Notify that interpolation is not support for thermo variables
      SELECT CASE(DES_INTERP_SCHEME_ENUM)
      CASE(DES_INTERP_NONE)
      CASE DEFAULT
         WRITE(ERR_MSG,2000) trim(adjustl(DES_INTERP_SCHEME))
         !CALL LOG_ERROR()
      END SELECT

 2000 FORMAT('WARNING 2000: The selected interpolation scheme (',A,    &
         ') is not',/'supported by the DES Species equation implemen', &
         'tation. All energy',/'equation variables will use the ',     &
         'centroid method for interphase',/'data exchange.')

      RETURN
      END SUBROUTINE CHECK_SOLIDS_COMMON_DISCRETE_THERMO


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_SOLIDS_COMMON_DISCRETE_GEOMETRY                   !
!  Author: J.Musser                                   Date: 11-DEC-13  !
!                                                                      !
!  Purpose: Check user input data                                      !
!                                                                      !
!  Comments: Geometry checks were moved here from CHECK_DES_DATA.      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_SOLIDS_COMMON_DISCRETE_GEOMETRY

! Modules
!---------------------------------------------------------------------//
! Flag: Use Cartesian grid cut-cell implementation
      USE cutcell, only: CARTESIAN_GRID
! Flag: Use STL representation in CG
      USE cutcell, only: USE_STL
! Flag: Use DES E-L model
      USE discretelement, only: DES_CONTINUUM_COUPLED
      USE discretelement, only: MAX_RADIUS
      USE geometry, only: COORDINATES
      USE geometry, only: NO_I, NO_J
      USE geometry, only: ZLENGTH
      IMPLICIT NONE

! Local Variables
!---------------------------------------------------------------------//
      DOUBLE PRECISION :: MIN_DEPTH

! DEM/MPPIC is restricted to CARTESIAN coordinates.
      IF(COORDINATES == 'CYLINDRICAL') THEN
         WRITE (ERR_MSG, 1100)
         CALL LOG_ERROR()
      ENDIF

 1100 FORMAT('Error: 1100: DES and MPPIC models only support ',        &
         'CARTESIAN coordinates.')


! Check dimension. This is redundant with check_data_03.
      IF(NO_I .OR. NO_J) THEN
         WRITE(ERR_MSG, 1200)
         CALL LOG_ERROR()
      ENDIF

 1200 FORMAT('Error 1200: Illegal geometry for DEM/MPPIC. 2D ',        &
         'simulations are',/'restricted to the XY plane. Please ',     &
         'correct the project settings.')


      IF(DES_CONTINUUM_COUPLED)THEN
! Check that the depth of the simulation exceeds the largest particle
! to ensure correct calculation of volume fraction. This is important
! for coupled simulations.
         MIN_DEPTH = 2.0d0*MAX_RADIUS
         IF(ZLENGTH < MIN_DEPTH)THEN
            WRITE(ERR_MSG, 1300)
            CALL LOG_WARNING()
         ENDIF
      ENDIF

 1300 FORMAT('Error 1300: The maximum particle diameter exceeds the ', &
         'simulation',/'depth (ZLENGTH). Please correct the project settings.')

      IF(CARTESIAN_GRID .AND. .NOT.USE_STL) THEN
         WRITE(ERR_MSG,1400)
         CALL LOG_ERROR()
      ENDIF

 1400 FORMAT('Error 1400: Cartesian grid and discrete models (DEM or ',&
         'PIC) only',/'support STL wall representations. Quadrics ',   &
         'and polygons are not',/'supported.')

      RETURN

      END SUBROUTINE CHECK_SOLIDS_COMMON_DISCRETE_GEOMETRY

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_SOLIDS_COMMON_DISCRETE_INTERP                     !
!  Author: J.Musser                                   Date: 25-Nov-14  !
!                                                                      !
!  Purpose:                                                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_SOLIDS_COMMON_DISCRETE_INTERP

! Modules
!---------------------------------------------------------------------//
! Runtime Flag: Invoke gas/solids coupled simulation.
      use discretelement, only: DES_CONTINUUM_COUPLED
! Runtime FLag: 3D simulation
      use geometry, only: DO_K
! Runtime Flag: Invoke MPPIC model.
      USE mfix_pic, only: MPPIC
      use param1, only: UNDEFINED
! User input for DES interpolation scheme.
      use particle_filter, only: DES_INTERP_SCHEME
! Enumerated interpolation scheme for faster access
      use particle_filter, only: DES_INTERP_SCHEME_ENUM
      use particle_filter, only: DES_INTERP_NONE
      use particle_filter, only: DES_INTERP_GARG
      use particle_filter, only: DES_INTERP_DPVM
      use particle_filter, only: DES_INTERP_GAUSS
      use particle_filter, only: DES_INTERP_LHAT
! User specified filter width
      use particle_filter, only: DES_INTERP_WIDTH
! Flag: Diffuse DES field variables.
      use particle_filter, only: DES_DIFFUSE_MEAN_FIELDS
! Diffusion filter width
      use particle_filter, only: DES_DIFFUSE_WIDTH
! Flag: Interpolate continuum fields
      use particle_filter, only: DES_INTERP_MEAN_FIELDS
! Flag: Interplate variables for drag calculation.
      use particle_filter, only: DES_INTERP_ON
! Size of interpolation filter
      use particle_filter, only: FILTER_SIZE
      IMPLICIT NONE

! Set the runtime flag for diffusing mean fields
      DES_DIFFUSE_MEAN_FIELDS = (DES_DIFFUSE_WIDTH /= UNDEFINED)

! Set the interpolation ENUM value.
      SELECT CASE(trim(adjustl(DES_INTERP_SCHEME)))
      CASE ('NONE')
         DES_INTERP_SCHEME_ENUM = DES_INTERP_NONE
! Cannot use interpolation when no scheme is selected.
         IF(DES_INTERP_ON)THEN
            WRITE(ERR_MSG,2001) 'DES_INTERP_ON'
            CALL LOG_ERROR()
         ELSEIF(DES_INTERP_MEAN_FIELDS)THEN
            WRITE(ERR_MSG,2001) 'DES_INTERP_MEAN_FIELDS'
            CALL LOG_ERROR()

         ELSEIF(DES_CONTINUUM_COUPLED) THEN
            IF(MPPIC) THEN
               WRITE(ERR_MSG,2002) 'MPPIC solids'
               CALL LOG_WARNING()
            ELSEIF(MPPIC) THEN
               WRITE(ERR_MSG,2002) 'Cartesian grid cut-cells'
               CALL LOG_ERROR()
            ENDIF
         ENDIF

      CASE ('GARG_2012')
         DES_INTERP_SCHEME_ENUM = DES_INTERP_GARG

      CASE ('SQUARE_DPVM')
         DES_INTERP_SCHEME_ENUM = DES_INTERP_DPVM

      CASE ('GAUSS_DPVM')
         DES_INTERP_SCHEME_ENUM = DES_INTERP_GAUSS

      CASE ('LINEAR_HAT')
         DES_INTERP_SCHEME_ENUM = DES_INTERP_LHAT

      CASE DEFAULT
         WRITE(ERR_MSG,2000) trim(adjustl(DES_INTERP_SCHEME))
         CALL LOG_ERROR()
      END SELECT

 2000 FORMAT('Error 2000: Invalid DES_INTERP_SCHEME: ',A,/'Please ',   &
         'correct the project settings.')

 2001 FORMAT('Error 2001: No interpolation scheme specified when ',A,/ &
         'is enabled. Please correct the project settings.')

 2002 FORMAT('Error 2002: DES simulations utilizing ',A,' require',/   &
         'interpolation (DES_INTERP_ON and DES_INTERP_MEANFIELDS). ',/ &
         'Please correct the project settings.')

      SELECT CASE(DES_INTERP_SCHEME_ENUM)

      CASE(DES_INTERP_NONE)

         IF(DES_INTERP_WIDTH /= UNDEFINED) THEN
            WRITE(ERR_MSG,2100) trim(adjustl(DES_INTERP_SCHEME))
            CALL LOG_ERROR()
         ENDIF

 2100 FORMAT('Error 2100: The selected interpolation scheme (',A,') ', &
         'does',/'not support an adjustable interpolation width.',/    &
         'Please correct the input file.')


      CASE(DES_INTERP_GARG)
         DES_INTERP_MEAN_FIELDS= .TRUE.

         IF(DES_INTERP_WIDTH /= UNDEFINED) THEN
            WRITE(ERR_MSG,2100) trim(adjustl(DES_INTERP_SCHEME))
            CALL LOG_ERROR()
         ENDIF

         IF(DES_DIFFUSE_MEAN_FIELDS) THEN
            WRITE(ERR_MSG,2110) trim(adjustl(DES_INTERP_SCHEME))
            CALL LOG_ERROR()
         ENDIF

 2110 FORMAT('Error 2110: The selected interpolation scheme (',A,') ', &
         'does not',/'support diffusive filtering of mean field ',     &
          'quantities. Please correct',/'the input file.')

      CASE(DES_INTERP_DPVM, DES_INTERP_GAUSS)

! Set the size of the interpolation filter.
         FILTER_SIZE = merge(27, 9, DO_K)

         IF(DES_INTERP_WIDTH == UNDEFINED) THEN
            WRITE(ERR_MSG,2120) trim(adjustl(DES_INTERP_SCHEME))
            CALL LOG_ERROR()
         ENDIF

 2120 FORMAT('Error 2120: The selected interpolation scheme (',A,') ', &
         'requires',/'a DES_INTERP_WIDTH. Please correct the ',        &
         'input file.')


      CASE(DES_INTERP_LHAT)

! Set the size of the interpolation filter.
         FILTER_SIZE = merge(27, 9, DO_K)

         IF(DES_INTERP_WIDTH /= UNDEFINED) THEN
            WRITE(ERR_MSG,2100) trim(adjustl(DES_INTERP_SCHEME))
            CALL LOG_ERROR()
         ENDIF

      END SELECT

      RETURN

   END SUBROUTINE CHECK_SOLIDS_COMMON_DISCRETE_INTERP

END MODULE CHECK_SOLIDS_COMMON_DISCRETE_MOD
