#include "error.inc"

module check_bc_dem_mod

   use error_manager

contains

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
! minimum amount of geometry data.                                     !
!                                                                      !
! Subroutine: CHECK_BC_DEM                                             !
! Author: J.Musser                                    Date: 01-Mar-14  !
!                                                                      !
! Purpose: Determine if BCs are "DEFINED" and that they contain the    !
! minimum amount of geometry data.                                     !
!                                                                      !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
      SUBROUTINE CHECK_BC_DEM(M_TOT)

! Global Variables:
!---------------------------------------------------------------------//
! User specified BC
      use bc, only: MASS_INFLOW, MASS_OUTFLOW, P_INFLOW, P_OUTFLOW, CG_MI, CG_PO, OUTFLOW, BC_TYPE_ENUM
! User specified: BC geometry
      use bc, only: BC_EP_G, BC_EP_s
! Use specified flag for ignoring PO BC for discrete solids
      USE bc, only: BC_PO_APPLY_TO_DES
! Use specified flag for ignoring PO BC for discrete solids
      USE bc, only: BC_MI_APPLY_TO_DES
! Particle size distribution
      USE bc, only: BC_PSD_TYPE
      USE bc, only: BC_PSD_MEAN_DP
      USE bc, only: BC_PSD_STDEV
      USE bc, only: BC_PSD_MAX_DP
      USE bc, only: BC_PSD_MIN_DP
      USE bc, only: BC_PSD_MU
      USE bc, only: BC_PSD_SIGMA
! Number of TFM solids phases
      USE physprop, only: MMAX
! Solids phase identifier
      use run, only: SOLIDS_MODEL
! Number of DEM inlet/outlet BCs detected.
      use des_bc, only: DEM_MIO, DEM_BCMI, DEM_BCMO
!
      use des_bc, only: DEM_BCMI_MAP
      use des_bc, only: DEM_BCMO_MAP
! Max/Min particle radii
      USE discretelement, only: MAX_RADIUS, MIN_RADIUS
! Number of DEM solids phases.
      USE discretelement, only: DES_MMAX
! Coarse Grain DEM
      use discretelement, only: CGDEM,CGP_STAT_WT,CGP_SCALING_METHOD
! MPPIC
      use mfix_pic, only: DES_STAT_WT, MPPIC
! Global Parameters:
!---------------------------------------------------------------------//
! The max number of BCs.
      use param, only: DIMENSION_BC
! Parameter constants
      use param1, only: ZERO, ONE, UNDEFINED

      IMPLICIT NONE

! Passed Arguments:
!---------------------------------------------------------------------//
! Total number of solids phases.
      INTEGER, INTENT(in) :: M_TOT

! Local Variables:
!---------------------------------------------------------------------//
! loop/variable indices
      INTEGER :: BCV, M, BCMI
      DOUBLE PRECISION :: mu_star, sigma_star, mode,variance, skewness
!......................................................................!

! Initialize
      DEM_BCMI = 0
      DEM_BCMO = 0

! Loop over all BCs looking for DEM solids inlets/outlets
      DO BCV = 1, DIMENSION_BC

         SELECT CASE (BC_TYPE_ENUM(BCV))

! Determine the number of mass inlets that contain DEM solids.
         CASE (MASS_INFLOW)
            M_LP: DO M=1,M_TOT
               IF((SOLIDS_MODEL(M)=='DEM'.OR.SOLIDS_MODEL(M)=='CGP') .AND.  &
                  BC_EP_s(BCV,M) > ZERO) THEN
                  DEM_BCMI = DEM_BCMI + 1
                  DEM_BCMI_MAP(DEM_BCMI) = BCV
                  EXIT M_LP
               ENDIF
            ENDDO M_LP
! Allow DEM particles to exit along a Mass inflow BC
! If only gas is injected. The anticipated purpose of
! this is to have a simple mechanism to drain a fixed
! bed from the bottom even if there is gas inlet at
! the bottom
            IF(BC_EP_G(BCV) == ONE .AND.                         &
               BC_PO_APPLY_TO_DES(BCV)) THEN
               DEM_BCMO = DEM_BCMO + 1
               DEM_BCMO_MAP(DEM_BCMO) = BCV
            ENDIF

! Count the number of pressure outflows.
         CASE (P_OUTFLOW,MASS_OUTFLOW)
            IF(BC_PO_APPLY_TO_DES(BCV)) then
               DEM_BCMO = DEM_BCMO + 1
               DEM_BCMO_MAP(DEM_BCMO) = BCV
            ELSEIF(BC_MI_APPLY_TO_DES(BCV)) then
               DEM_BCMI = DEM_BCMI + 1
               DEM_BCMI_MAP(DEM_BCMI) = BCV
            ENDIF

! Flag CG_MI as an error if DEM solids are present.
         CASE (CG_MI)
            DO M=1,M_TOT
               IF(SOLIDS_MODEL(M)=='DEM'.OR.SOLIDS_MODEL(M)=='CGP') THEN
                  IF(BC_EP_s(BCV,M) /= UNDEFINED .AND.                 &
                     BC_EP_s(BCV,M) > ZERO) THEN
                     WRITE(ERR_MSG,900) trim(iVar('BC_TYPE',BCV)),    &
                        'CG_MI'
                     CALL LOG_ERROR()
                  ENDIF
               ENDIF
            ENDDO

         CASE (CG_PO)
            WRITE(ERR_MSG,900) trim(iVar('BC_TYPE',BCV)), 'CG_PO'
            CALL LOG_ERROR()

         CASE (OUTFLOW, P_INFLOW)
            WRITE(ERR_MSG,900) trim(iVar('BC_TYPE',BCV)),             &
               BC_TYPE_ENUM(BCV)
            CALL LOG_ERROR()

         END SELECT

      ENDDO

! Set the flag that one or more DEM MI/MO exists.
      DEM_MIO = (DEM_BCMI /= 0 .OR. DEM_BCMO /= 0)


! PSD data check
      BC_LOOP: DO BCMI=1, DEM_BCMI

         BCV = DEM_BCMI_MAP(DEM_BCMI)

         DO M = MMAX+1,DES_MMAX+MMAX

! Resising the PSD for CGDEM
            IF(CGDEM .and. .not. MPPIC.and.TRIM(BC_PSD_TYPE(BCV,M))/='MONO') THEN
               IF(CGP_SCALING_METHOD(M)==1) THEN
                  IF(BC_PSD_MEAN_DP(BCV,M) /= UNDEFINED) BC_PSD_MEAN_DP(BCV,M) = BC_PSD_MEAN_DP(BCV,M) * CGP_STAT_WT(M)**(1.d0/3.d0)
                  IF(BC_PSD_STDEV(BCV,M) /= UNDEFINED) BC_PSD_STDEV(BCV,M) = BC_PSD_STDEV(BCV,M) * CGP_STAT_WT(M)**(1.d0/3.d0)
                  IF(BC_PSD_MIN_DP(BCV,M) /= UNDEFINED) BC_PSD_MIN_DP(BCV,M) = BC_PSD_MIN_DP(BCV,M) * CGP_STAT_WT(M)**(1.d0/3.d0)
                  IF(BC_PSD_MAX_DP(BCV,M) /= UNDEFINED) BC_PSD_MAX_DP(BCV,M) = BC_PSD_MAX_DP(BCV,M) * CGP_STAT_WT(M)**(1.d0/3.d0)
                  WRITE(ERR_MSG,400)BCV,M,CGP_STAT_WT(M)
                  CALL LOG_INFO()
               ENDIF
            ENDIF

400 FORMAT('Info: Using Coarse Grain DEM, PSD is scaled up for BCV= ',I3, ', solids phase: ',I2/, &
           ' and a statistical weight of:', G9.2)

! Normal distribution
            if(BC_PSD_TYPE(BCV,M) == 'NORMAL') then

               if(BC_PSD_MEAN_DP(BCV,M) == UNDEFINED) then
                  WRITE(ERR_MSG,1000) trim(iVar('BC_PSD_MEAN_DP',BCV,M))
                  CALL LOG_ERROR()
               endif

               if(BC_PSD_STDEV(BCV,M) == UNDEFINED) then
                  WRITE(ERR_MSG,1000) trim(iVar('BC_PSD_STDEV',BCV,M))
                  CALL LOG_ERROR()
               endif

! For normal distribution without min/max bounds, assign 6-Sigma
               if(BC_PSD_MIN_DP(BCV,M) == UNDEFINED) &
                  BC_PSD_MIN_DP(BCV,M) = BC_PSD_MEAN_DP(BCV,M) - 3.D0*BC_PSD_STDEV(BCV,M)

               if(BC_PSD_MAX_DP(BCV,M) == UNDEFINED) &
                  BC_PSD_MAX_DP(BCV,M) = BC_PSD_MEAN_DP(BCV,M) + 3.D0*BC_PSD_STDEV(BCV,M)

               if(BC_PSD_MIN_DP(BCV, M).le.0.D0) then
                 WRITE(ERR_MSG,1500) BCV, M
                 CALL LOG_ERROR()
               ENDIF

               if(BC_PSD_MAX_DP(BCV, M).le.0.D0 .or. BC_PSD_MAX_DP(BCV, M)<BC_PSD_MIN_DP(BCV, M)) then
                 WRITE(ERR_MSG,1600) BCV, M
                 CALL LOG_ERROR()
               ENDIF

               WRITE(ERR_MSG,1100) BCV,M,BC_PSD_TYPE(BCV,M),BC_PSD_MEAN_DP(BCV,M),BC_PSD_STDEV(BCV,M),BC_PSD_MIN_DP(BCV,M),BC_PSD_MAX_DP(BCV,M), &
                     BC_PSD_MEAN_DP(BCV,M)-BC_PSD_STDEV(BCV,M),BC_PSD_MEAN_DP(BCV,M)+BC_PSD_STDEV(BCV,M),&            
                     BC_PSD_MEAN_DP(BCV,M)-2.0D0*BC_PSD_STDEV(BCV,M),BC_PSD_MEAN_DP(BCV,M)+2.0D0*BC_PSD_STDEV(BCV,M),&            
                     BC_PSD_MEAN_DP(BCV,M)-3.0D0*BC_PSD_STDEV(BCV,M),BC_PSD_MEAN_DP(BCV,M)+3.0D0*BC_PSD_STDEV(BCV,M)
               CALL LOG_INFO()

 1100 FORMAT( 'Info: DEM Particle size distribution along mass inlet for BCV=',I3, ' M=',I3, &
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
            if(BC_PSD_TYPE(BCV,M) == 'LOG_NORMAL') then

               if(BC_PSD_MEAN_DP(BCV,M) == UNDEFINED) then
                  WRITE(ERR_MSG,1000) trim(iVar('BC_PSD_MEAN_DP',BCV,M))
                  CALL LOG_ERROR()
               endif

               if(BC_PSD_STDEV(BCV,M) == UNDEFINED) then
                  WRITE(ERR_MSG,1000) trim(iVar('BC_PSD_STDEV',BCV,M))
                  CALL LOG_ERROR()
               endif

! Convert mean and std dev into log-normal parameters mu and sigma               
               BC_PSD_MU(BCV,M) = LOG(BC_PSD_MEAN_DP(BCV,M)**2/SQRT(BC_PSD_MEAN_DP(BCV,M)**2+BC_PSD_STDEV(BCV,M)**2))
               BC_PSD_SIGMA(BCV,M) = SQRT(LOG(1.0D0 + (BC_PSD_STDEV(BCV,M)/BC_PSD_MEAN_DP(BCV,M))**2))

! Median
               mu_star = exp(BC_PSD_MU(BCV,M))
! Scatter
               sigma_star = exp(BC_PSD_SIGMA(BCV,M))

               mode = exp(BC_PSD_MU(BCV,M) - BC_PSD_SIGMA(BCV,M)**2)
               variance = (exp(BC_PSD_SIGMA(BCV,M)**2) - 1.0D0)*exp(2.0D0*BC_PSD_MU(BCV,M) + BC_PSD_SIGMA(BCV,M)**2 )
               skewness = (exp(BC_PSD_SIGMA(BCV,M)**2) + 2.0D0)*DSQRT((exp(BC_PSD_SIGMA(BCV,M)**2) - 1.0D0))

               if(BC_PSD_MIN_DP(BCV,M) == UNDEFINED) &
                  BC_PSD_MIN_DP(BCV,M) = mu_star/sigma_star**3

               if(BC_PSD_MAX_DP(BCV,M) == UNDEFINED) &
                  BC_PSD_MAX_DP(BCV,M) = mu_star*sigma_star**3

               if(BC_PSD_MIN_DP(BCV, M).le.0.D0) then
                 WRITE(ERR_MSG,1500) BCV, M
                 CALL LOG_ERROR()
               ENDIF

               if(BC_PSD_MAX_DP(BCV, M).le.0.D0 .or. BC_PSD_MAX_DP(BCV, M)<BC_PSD_MIN_DP(BCV, M)) then
                 WRITE(ERR_MSG,1600) BCV, M
                 CALL LOG_ERROR()
               ENDIF

               WRITE(ERR_MSG,1200) BCV,M,BC_PSD_TYPE(BCV,M),BC_PSD_MEAN_DP(BCV,M),BC_PSD_STDEV(BCV,M), &
                     BC_PSD_MIN_DP(BCV,M),BC_PSD_MAX_DP(BCV,M), BC_PSD_MU(BCV,M), BC_PSD_SIGMA(BCV,M), &
                     mu_star, mode, variance, skewness,&
                     mu_star/sigma_star, mu_star*sigma_star,&
                     mu_star/sigma_star**2, mu_star*sigma_star**2,&
                     mu_star/sigma_star**3, mu_star*sigma_star**3
               CALL LOG_INFO()

 1200 FORMAT( 'Info: DEM Particle size distribution along mass inlet for BCV=',I3, ', M=',I3, &
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

            if(BC_PSD_MIN_DP(BCV, M).le.0.D0) then
              WRITE(ERR_MSG,1500) BCV, M
              CALL LOG_ERROR()
            ENDIF

            if(BC_PSD_MAX_DP(BCV, M).le.0.D0 .or. BC_PSD_MAX_DP(BCV, M)<BC_PSD_MIN_DP(BCV, M)) then
              WRITE(ERR_MSG,1600) BCV, M
              CALL LOG_ERROR()
            ENDIF

 1000 FORMAT('Error 1000: Required input not specified: ',A)
 1500 FORMAT('Error 1500: Enter a valid BC_PSD_MIN_DP for BCV :',I2, &
         ' and phase M :',I2)
 1600 FORMAT('Error 1600: Enter a valid BC_PSD_MAX_DP for BCV :',I2, &
         ' and phase M :',I2)

!@JFD: For custom distribution, BC_PSD_MIN_DP and the distribution's actual minimum
! could be different. Should we adjust min_radius based on the actual minimum radius of 
! particles generated ?!?
            if(BC_PSD_MIN_DP(BCV, M)/=UNDEFINED) MIN_RADIUS = MIN(MIN_RADIUS, 0.5d0*BC_PSD_MIN_DP(BCV, M))
            if(BC_PSD_MAX_DP(BCV, M)/=UNDEFINED) MAX_RADIUS = MAX(MAX_RADIUS, 0.5d0*BC_PSD_MAX_DP(BCV, M))

         ENDDO
      ENDDO BC_LOOP



      RETURN

 900 FORMAT('Error 900: Unsupported boundary condition specified ',  &
         'with',/'DEM simulation: ',A,' = ',A,/'Please correct the ',&
         'project settings.')

      END SUBROUTINE CHECK_BC_DEM

end module check_bc_dem_mod
