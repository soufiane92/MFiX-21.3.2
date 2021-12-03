#include "error.inc"

MODULE CHECK_OUTPUT_CONTROL_MOD

   use error_manager

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Subroutine: CHECK_OUTPUT_CONTROL                                    !
!  Purpose: Check the output control namelist section                  !
!                                                                      !
!  Author: P. Nicoletti                               Date: 27-NOV-91  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_OUTPUT_CONTROL

! Global Variables:
!---------------------------------------------------------------------//
! Time intervalue between updating the RES and SPx files.
      use output, only: RES_DT
! Time-step intervalue between updating the .LOG file.
      use output, only: NLOG
! VTK
      use vtk

! Global Parameters:
!---------------------------------------------------------------------//
! Number aliases
      use param1, only: UNDEFINED, ZERO

! Skip data check when doing pre-processing only
      USE run, only:ppo

      IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//

!......................................................................!

      IF(PPO) RETURN

! Check the values specified for the RES file.
      IF (RES_DT==UNDEFINED)THEN
         WRITE(ERR_MSG,1000) 'RES_DT'
         CALL LOG_ERROR()
      ELSEIF(RES_DT <= ZERO) THEN
         WRITE(ERR_MSG,1002) 'RES_DT', RES_DT
         CALL LOG_ERROR()
      ENDIF

!     Verify that the LOG frequency is valid.
      IF(NLOG <= 0) THEN
         WRITE(ERR_MSG,1003) 'NLOG', NLOG
         CALL LOG_ERROR()
      ENDIF


      CALL CHECK_OUTPUT_SPX()
      CALL CHECK_OUTPUT_VTK()
      CALL CHECK_OUTPUT_MONITOR()

      RETURN

 1000 FORMAT('Error 1000: Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

 1002 FORMAT('Error 1002: Illegal or unknown input: ',A,' = ',E14.6,/  &
         'Please correct the project settings.')

 1003 FORMAT('Error 1003: Illegal or unknown input: ',A,' = ',I4,/     &
         'Please correct the project settings.')

      END SUBROUTINE CHECK_OUTPUT_CONTROL


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Subroutine: CHECK_OUTPUT_CONTROL                                    !
!  Purpose: Check the output control namelist section                  !
!                                                                      !
!  Author: P. Nicoletti                               Date: 27-NOV-91  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_OUTPUT_SPX

! Global Variables:
!---------------------------------------------------------------------//
! Time intervalue between updating the RES and SPx files.
! Flag: Use the K-Epsilon model
      use turb, only: K_EPSILON
      use output, only: SPX_DT
! Number of arrays to store in SPA
      use rxns, only: nRR

! Global Parameters:
!---------------------------------------------------------------------//
! Number aliases
      use param1, only: UNDEFINED, UNDEFINED_I, ZERO, LARGE_NUMBER
! Number of SPx files.
      USE param1, only: N_SPX

      IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//
! Loop counter
      INTEGER :: LC

! Check the SPx Files
      SPx_LP: DO LC = 1, N_SPX

! Disable writing the .SPA file if nRR is unspecified.
         IF(LC == 10) THEN
            IF(nRR == 0) THEN
               IF (SPX_DT(LC) == UNDEFINED) SPX_DT(LC) = LARGE_NUMBER
               CYCLE SPx_LP
            ENDIF

! Disable writing the .SPB file if K-Epsilon is unspecified.
         ELSEIF(LC == 11) THEN
            IF(.NOT.K_Epsilon) THEN
               IF (SPX_DT(LC)==UNDEFINED)SPX_DT(LC) = LARGE_NUMBER
               CYCLE SPx_LP
            ENDIF

! Verify the remaining SPx files.
         ELSE
            IF(SPX_DT(LC) <= ZERO) THEN
               WRITE(ERR_MSG,1001) iVar('SPX_DT',LC), SPX_DT(LC)
               CALL LOG_ERROR()
            ENDIF
         ENDIF
      ENDDO SPx_LP

      RETURN

 1001 FORMAT('Error 1001: Illegal or unknown input: ',A,' = ',A,/      &
         'Please correct the project settings.')

      END SUBROUTINE CHECK_OUTPUT_SPX

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Subroutine: CHECK_OUTPUT_VTK                                        !
!  Purpose: Check the output control namelist section                  !
!                                                                      !
!  Author: P. Nicoletti                               Date: 27-NOV-91  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_OUTPUT_VTK

! Global Variables:
!---------------------------------------------------------------------//
! Flag: Use the K-Epsilon model
      use turb, only: K_EPSILON
! Number of arrays to store in SPA
      use rxns, only: nRR
! VTK
      use vtk
      USE run, only: RUN_NAME, REINITIALIZING
      USE physprop, only: MMAX
      USE scalars, only :NSCALAR
      USE DISCRETELEMENT, only:DISCRETE_ELEMENT
      USE DISCRETELEMENT, only: PARTICLE_ORIENTATION
      USE DISCRETELEMENT, only: DES_USR_VAR_SIZE
      USE DISCRETELEMENT, ONLY: DES_MMAX
      USE DISCRETELEMENT, ONLY: WRITE_FORCE_CHAIN
      USE cutcell, only: USE_STL
      use des_rxns, only: NO_OF_DES_RXNS
      use rxns, only: NO_OF_RXNS
      use param, only: dimension_3

! Global Parameters:
!---------------------------------------------------------------------//
! Number aliases
      use param1, only: UNDEFINED, UNDEFINED_I, ZERO, LARGE_NUMBER
! Number of SPx files.
      USE param1, only: N_SPX
! Geometry bounds
      use geometry, only: X_MIN, X_MAX, Y_MIN, Y_MAX, Z_MIN, Z_MAX

      IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//

      INTEGER :: L,M,N,LV,N_VTK_REGIONS,R, M_IN_VTK

!......................................................................!

! Check VTK regions

      IF(FRAME(1)<-1) THEN
         WRITE(ERR_MSG, 2000) trim(iVAL(FRAME(1)))
         CALL LOG_ERROR()
      ENDIF

 2000 FORMAT('Error 2000: Invalid value for FRAME = ',A, '. Acceptable ',&
         'values',/'are integers >= -1. Please correct the&
         & project settings and',/'try again.')

      IF(VTK_DT(1)<ZERO) THEN
          WRITE(ERR_MSG,2001) trim(iVal(VTK_DT(1)))
         CALL LOG_ERROR()
      ENDIF

 2001 FORMAT('Error 2001: Invalid value for VTK_DT = ',A, '. Acceptable',&
         ' values',/'are positive numbers (e.g., 0.1).  Please ',      &
         'correct the project settings and',/'try again.')

      N_VTK_REGIONS = 0

      SAVE_FLUID_RRATES = .FALSE.
      SAVE_DES_RRATES = .FALSE.
      SAVE_PART_RRATES = .FALSE.

      DO L = 1, DIMENSION_VTK
         VTK_DEFINED(L) = .FALSE.
         IF (VTK_X_W(L) /= -UNDEFINED)   VTK_DEFINED(L) = .TRUE.
         IF (VTK_X_E(L) /=  UNDEFINED)   VTK_DEFINED(L) = .TRUE.
         IF (VTK_Y_S(L) /= -UNDEFINED)   VTK_DEFINED(L) = .TRUE.
         IF (VTK_Y_N(L) /=  UNDEFINED)   VTK_DEFINED(L) = .TRUE.
         IF (VTK_Z_B(L) /= -UNDEFINED)   VTK_DEFINED(L) = .TRUE.
         IF (VTK_Z_T(L) /=  UNDEFINED)   VTK_DEFINED(L) = .TRUE.
         
         IF (VTK_DATA(L) == 'G')   VTK_DEFINED(L) = .TRUE.

         IF (VTK_DOMAIN_DECOMPOSITION(L)) VTK_DEFINED(L) = .TRUE.

         IF(VTK_DEFINED(L)) THEN
            N_VTK_REGIONS =  N_VTK_REGIONS + 1
         ELSE IF(REINITIALIZING) THEN
            VTK_TIME(L) = UNDEFINED
         ENDIF
      ENDDO   ! end loop over (l = 1,dimension_vtk)

! There must be at least one VTK region defined
! If this is not the case, define the entire domain as default region
      IF(WRITE_VTK_FILES.AND.N_VTK_REGIONS==0) THEN
         VTK_DEFINED(1) = .TRUE.
         VTK_X_W(1) = X_MIN
         VTK_X_E(1) = X_MAX
         VTK_Y_S(1) = Y_MIN
         VTK_Y_N(1) = Y_MAX
         VTK_Z_B(1) = Z_MIN
         VTK_Z_T(1) = Z_MAX
         VTK_FILEBASE(1) = RUN_NAME
      ENDIF

! If VTK_VAR is defined, fill-up the variable list
! for the vtk subdomains
      DO L = 1, DIM_VTK_VAR
         IF(VTK_VAR(L)/=UNDEFINED_I) VTK_VARLIST(:,L) = VTK_VAR(L)
      ENDDO

      WRITE_FORCE_CHAIN = .FALSE.

      DO L = 1, DIMENSION_VTK

         IF(.NOT.VTK_DEFINED(L)) CYCLE

         DO LV = 1, DIM_VTK_VAR

            SELECT CASE (VTK_VARLIST(L,LV))

               CASE (1)
                  VTK_EP_g(L) = .TRUE.

               CASE (2)
                  VTK_P_g(L)        = .TRUE.
                  VTK_P_s(L,1:MMAX) = .TRUE.

               CASE (3)
                  VTK_VEL_G(L) = .TRUE.

               CASE (4)
                  DO M = 1,MMAX
                     VTK_VEL_S(L,M) = .TRUE.
                  END DO

               CASE (5)
                  DO M = 1,MMAX
                     VTK_ROP_s(L,M) = .TRUE.
                  END DO

               CASE (6)
                  VTK_T_g(L) = .TRUE.
                  DO M = 1,MMAX
                     VTK_T_s(L,M) = .TRUE.
                  END DO

               CASE (7)
                 !DO N = 1,NMAX(0)
                    VTK_X_g(L,:) = .TRUE.
                 !END DO

                  DO M = 1, MMAX
                    !DO N = 1,NMAX(M)
                        VTK_X_s(L,M,:) = .TRUE.
                    !END DO
                  END DO

               CASE (8)
                  DO M = 1,MMAX
                     VTK_Theta_m(L,M) = .TRUE.
                  END DO

               CASE (9)
                  DO N = 1,NSCALAR
                     VTK_Scalar(L,N) =.TRUE.
                  END DO

               CASE (10)
                  DO R = 1,nRR
                     VTK_RRate(L,R) = .TRUE.
                  END DO

               CASE (11)
                  IF(K_EPSILON) THEN
                     VTK_K_Turb_G(L) = .TRUE.
                     VTK_E_Turb_G(L) = .TRUE.
                  ENDIF

               CASE (12)
                  VTK_VORTICITY(L) = .TRUE.
                  VTK_LAMBDA_2(L)  = .TRUE.

               CASE (100)
                  VTK_PARTITION(L) = .TRUE.

               CASE (101)
                  VTK_BC_ID(L) = .TRUE.

               CASE (102)
                  VTK_DWALL(L) = .TRUE.

               CASE (103)
                  IF(DISCRETE_ELEMENT.AND.USE_STL) THEN
                     VTK_FACET_COUNT_DES(L) = .TRUE.
                  ENDIF

               CASE (104)
                  IF(DISCRETE_ELEMENT.AND.USE_STL) THEN
                     VTK_NB_FACET_DES(L) = .TRUE.
                  ENDIF

               CASE(999)
                  VTK_IJK(L) = .TRUE.

               CASE(1000)
                  VTK_NORMAL(L) = .TRUE.

               CASE (1001)
                  VTK_DEBUG(L,1) = .TRUE.

               CASE (1002)
                  VTK_DEBUG(L,2) = .TRUE.

               CASE (1003)
                  VTK_DEBUG(L,3) = .TRUE.

               CASE (1004)
                  VTK_DEBUG(L,4) = .TRUE.

               CASE (1005)
                  VTK_DEBUG(L,5) = .TRUE.

               CASE (1006)
                  VTK_DEBUG(L,6) = .TRUE.

               CASE (1007)
                  VTK_DEBUG(L,7) = .TRUE.

               CASE (1008)
                  VTK_DEBUG(L,8) = .TRUE.

               CASE (1009)
                  VTK_DEBUG(L,9) = .TRUE.

               CASE (1010)
                  VTK_DEBUG(L,10) = .TRUE.

               CASE (1011)
                  VTK_DEBUG(L,11) = .TRUE.

               CASE (1012)
                  VTK_DEBUG(L,12) = .TRUE.

               CASE (1013)
                  VTK_DEBUG(L,13) = .TRUE.

               CASE (1014)
                  VTK_DEBUG(L,14) = .TRUE.

               CASE (1015)
                  VTK_DEBUG(L,15) = .TRUE.


               CASE (0) ! do nothing

               CASE (UNDEFINED_I) ! do nothing

               CASE DEFAULT
                  WRITE(ERR_MSG,2100) trim(iVal(L)),                   &
                     trim(iVal(VTK_VAR(L)))
                  CALL LOG_ERROR()
            END SELECT

 2100 FORMAT(' Error 2100: Unknown VTK variable flag ',A,':',A,       /&
         'Available flags are:',                                      /&
         '  1 : Void fraction (EP_g)',                                /&
         '  2 : Gas pressure, solids pressure (P_g, P_star)',         /&
         '  3 : Gas velocity (U_g, V_g, W_g)',                        /&
         '  4 : Solids velocity (U_s, V_s, W_s)',                     /&
         '  5 : Solids density (ROP_s)',                              /&
         '  6 : Gas and solids temperature (T_g, T_s1, T_s2)',        /&
         '  7 : Gas and solids mass fractions (X_g, X-s)',            /&
         '  8 : Granular temperature (G)',                            /&
         '  9 : User defined scalars',                                /&
         ' 10 : Reaction Rates',                                      /&
         ' 11 : Turbulence quantities (k and ε)',                     /&
         ' 12 : Gas Vorticity magn and Lambda_2(VORTICITY,LAMBDA_2)', /&
         '100: Processor assigned to scalar cell (Partition)',        /&
         '101: Boundary condition flag for scalar cell (BC_ID)',      /&
         'Please correct the project settings.')


         ENDDO

! Activate particle orientation calculation if one vtk region needs it.
         IF(VTK_PART_ORIENTATION(L)) PARTICLE_ORIENTATION = .TRUE.


! If there is only one DES solids phase, use species aliases when
! saving DES_X_S in the vtp file
! Set VTK_PHASE_FOR_DES_X_S(VTK_REGION) equal to the DES phase ID
! if there is only one des phase in the vtk region, set it to zero
! otherwise

         M_IN_VTK=0
         VTK_PHASE_FOR_DES_X_S(L) = 0
         DO M = MMAX+1,MMAX+DES_MMAX
            IF(VTK_PART_PHASE(L,M)) THEN
               M_IN_VTK = M_IN_VTK + 1
               VTK_PHASE_FOR_DES_X_S(L) = M
            ENDIF
         ENDDO

         IF(M_IN_VTK/=1) VTK_PHASE_FOR_DES_X_S(L) = 0

! Force chain

         IF(VTK_DATA(L)=='F') WRITE_FORCE_CHAIN = .TRUE.


! Set up flags to save reaction rates in vtu/vtp files
! Cell data (vtu files): Fluid (gas phase or gas and solids phase for TFM) rates
!                        and particle rates, accumulated at the cell level.
! Particle data (DEM, CGDEM, PIC): each particle store its own rates.
         IF(VTK_DATA(L)=='C') THEN
            DO R = 1,NO_OF_RXNS
               IF(VTK_FLUID_RRate(L,R)) SAVE_FLUID_RRATES = .TRUE.
            END DO
            DO R = 1,NO_OF_DES_RXNS
               IF(VTK_DES_RRate(L,R)) SAVE_DES_RRATES = .TRUE.
            END DO
         ELSEIF(VTK_DATA(L)=='P') THEN
            DO R = 1,NO_OF_DES_RXNS
               IF(VTK_PART_RRate(L,R)) SAVE_PART_RRATES = .TRUE.
            END DO
         ENDIF


      ENDDO   ! end loop over (l = 1,dimension_vtk)


      IF(DES_USR_VAR_SIZE>VTK_PART_USRmax) THEN
         WRITE(ERR_MSG,1005) DES_USR_VAR_SIZE,VTK_PART_USRmax
         CALL LOG_ERROR()
      ENDIF

 1005 FORMAT('Error 1005: Illegal input: DES_USR_VAR_SIZE>&
      &VTK_PART_USRmax:',/ 'DES_USR_VAR_SIZE = ',I6,/ &
      'VTK_PART_USRmax  = ',I6,/ &
      'Please correct the project settings or increase ', / &
      'VTK_PART_USRmax in model/cartesian_grid/vtk_mod.f')

      RETURN

      END SUBROUTINE CHECK_OUTPUT_VTK


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Subroutine: CHECK_OUTPUT_MONITOR                                    !
!  Purpose: Check the output control namelist section                  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_OUTPUT_MONITOR

! Global Variables:
!---------------------------------------------------------------------//
      use monitor

! Global Parameters:
!---------------------------------------------------------------------//
! Number aliases
      use param1, only: UNDEFINED, ZERO
      USE physprop, only: mmax, nmax
      use rxns, only: nrr, nrrmax
      use scalars, only : nscalar
      use discretelement, only: des_usr_var_size

      use run, only: reinitializing, time
      use compar, only: mype,pe_io

      use monitor_functions, only: monitor_open_file
      use monitor_functions, only: monitor_write_header
      use vtk
      use param, only: dimension_3

      IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//
! Loop counter
      integer :: lc1, lc2, lc3, EEcnt
      integer :: wcnt, funit

!......................................................................!

      do lc1 = 1, dimension_monitor

         monitor_defined(lc1) = .false.
         if (monitor_dt(lc1)  /=  undefined) monitor_defined(lc1) = .true.
         if (monitor_x_w(lc1) /= -undefined) monitor_defined(lc1) = .true.
         if (monitor_x_e(lc1) /=  undefined) monitor_defined(lc1) = .true.
         if (monitor_y_s(lc1) /= -undefined) monitor_defined(lc1) = .true.
         if (monitor_y_n(lc1) /=  undefined) monitor_defined(lc1) = .true.
         if (monitor_z_b(lc1) /= -undefined) monitor_defined(lc1) = .true.
         if (monitor_z_t(lc1) /=  undefined) monitor_defined(lc1) = .true.

         if(monitor_defined(lc1)) then
            if(monitor_dt(lc1)<zero) then
               write(err_msg,2001) trim(ival(monitor_dt(lc1)))
               call log_error()
            endif
         else if(reinitializing) then
            monitor_time(lc1) = undefined
         endif

 2001 FORMAT('Error 2001: Invalid value for MONITOR_DT = ',A, '. &
         &Acceptable values',/'are positive numbers (e.g., 0.1).  &
         &Please correct the project settings and',/'try again.')

         if(monitor_defined(lc1)) then

            wcnt = 0
            if(monitor_ep_g(lc1)) wcnt = wcnt + 1
            if(monitor_ro_g(lc1)) wcnt = wcnt + 1
            if(monitor_p_g (lc1)) wcnt = wcnt + 1
            if(monitor_u_g (lc1)) wcnt = wcnt + 1
            if(monitor_v_g (lc1)) wcnt = wcnt + 1
            if(monitor_w_g (lc1)) wcnt = wcnt + 1
            if(monitor_t_g (lc1)) wcnt = wcnt + 1
            if(monitor_mw_mix_g (lc1)) wcnt = wcnt + 1
            do lc2 = 1, nmax(0)
               if(monitor_x_g(lc1,lc2)) wcnt = wcnt + 1
               if(monitor_y_g(lc1,lc2)) wcnt = wcnt + 1
            enddo

            if(monitor_k_turb_g(lc1)) wcnt = wcnt + 1
            if(monitor_e_turb_g(lc1)) wcnt = wcnt + 1

            if(monitor_p_star (lc1)) wcnt = wcnt + 1
            do lc2 = 1, mmax
               if(monitor_ep_s   (lc1,lc2)) wcnt = wcnt + 1
               if(monitor_rop_s  (lc1,lc2)) wcnt = wcnt + 1
               if(monitor_ro_s   (lc1,lc2)) wcnt = wcnt + 1
               if(monitor_u_s    (lc1,lc2)) wcnt = wcnt + 1
               if(monitor_v_s    (lc1,lc2)) wcnt = wcnt + 1
               if(monitor_w_s    (lc1,lc2)) wcnt = wcnt + 1
               if(monitor_p_s    (lc1,lc2)) wcnt = wcnt + 1
               if(monitor_t_s    (lc1,lc2)) wcnt = wcnt + 1
               if(monitor_theta_m(lc1,lc2)) wcnt = wcnt + 1
               do lc3 = 1, nmax(lc2)
                  if(monitor_x_s(lc1,lc2,lc3)) wcnt = wcnt + 1
               enddo
            enddo

            do lc2 = 1, nscalar
               if(monitor_scalar(lc1,lc2)) wcnt = wcnt + 1
            enddo

            do lc2 = 1, min(nrrmax,nrr)
               if(monitor_rrate(lc1,lc2)) wcnt = wcnt + 1
            enddo

            do lc2 = 1, NO_OF_RXNS
               if(monitor_fluid_rrate(lc1,lc2)) then
                  wcnt = wcnt + 1
                  SAVE_FLUID_RRATES = .TRUE.
               endif
            enddo

            do lc2 = 1, NO_OF_DES_RXNS
               if(monitor_des_rrate(lc1,lc2)) then
                  wcnt = wcnt + 1
                  SAVE_FLUID_RRATES = .TRUE.
               endif
            enddo

            ! Save the number of Eulerian monitors
            EEcnt = wcnt

            if(monitor_radius(lc1)) wcnt = wcnt + 1
            if(monitor_pmass(lc1)) wcnt = wcnt + 1
            if(monitor_pvol(lc1)) wcnt = wcnt + 1
            if(monitor_ro_p(lc1)) wcnt = wcnt + 1

            if(monitor_vel_x(lc1)) wcnt = wcnt + 1
            if(monitor_vel_y(lc1)) wcnt = wcnt + 1
            if(monitor_vel_z(lc1)) wcnt = wcnt + 1

            if(monitor_rot_x(lc1)) wcnt = wcnt + 1
            if(monitor_rot_y(lc1)) wcnt = wcnt + 1
            if(monitor_rot_z(lc1)) wcnt = wcnt + 1

            if(monitor_t_p(lc1)) wcnt = wcnt + 1

            do lc2 = 1, dimension_n_s
               if(monitor_x_p(lc1, lc2)) wcnt = wcnt + 1
            end do

            do lc2 = 1, des_usr_var_size
               if(monitor_des_usr_var(lc1, lc2)) wcnt = wcnt + 1
            end do

            do lc2 = 1, NO_OF_DES_RXNS
               if(monitor_part_rrate(lc1,lc2)) then
                  wcnt = wcnt + 1
                  SAVE_PART_RRATES = .TRUE.
               endif
            enddo

            if(monitor_part_residence_time(lc1)) wcnt = wcnt + 1

            if(wcnt == 0) then
               monitor_defined(lc1) = .false.
               write(err_msg, 4455) lc1
               call log_warning()

            else if( EEcnt /= 0 .and. EEcnt /= wcnt) then
               write(err_msg, 4460) lc1
               call log_error()

            else
               monitor_var_count(lc1) = wcnt
            endif

            if(reinitializing .and. monitor_time(lc1) == undefined) &
               monitor_time(lc1) = time

            if(mype == pe_io) then
               call monitor_open_file(lc1, funit)
               call monitor_write_header(lc1, funit)
               close(funit)
            endif

         endif
      enddo


4455  format('Warning 4455: Monitor region ',i3,&
           ' has no variables selected.')

4460  format('Monitors cannot collect both Eulerian and Lagrangian data.',&
           &/'Correct the case setup file.')


      RETURN

      END SUBROUTINE CHECK_OUTPUT_MONITOR

END MODULE CHECK_OUTPUT_CONTROL_MOD
