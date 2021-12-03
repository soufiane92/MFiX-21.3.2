#include "error.inc"

MODULE FLOW_TO_VEL_MOD

   use bc, only: mass_inflow, mass_outflow, bc_area, bc_plane
   use bc, only: BC_MASSFLOW_G, BC_P_g, BC_VOLFLOW_G, BC_X_g
   use bc, only: BC_MASSFLOW_S, BC_T_g, BC_VOLFLOW_S, BC_X_s
   use bc, only: bc_massflow_g, bc_massflow_s
   use bc, only: bc_t_g, bc_t_s
   use bc, only: bc_type_enum
   use bc, only: bc_ep_g, bc_ep_s
   use bc, only: bc_u_g, bc_v_g, bc_w_g
   use bc, only: bc_u_s, bc_v_s, bc_w_s
   use bc, only: bc_vol
   use bc, only: cg_mi_converted_to_ps
   use compar, only: ijkstart3, ijkend3, numpes, mype, mpierr
   use cutcell, only: bc_id, interior_cell_at, normal_s
   use eos, only: EOSG, EOSS
   use error_manager, only: err_msg, loglevel_error, loglevel_warning, log_message, ivar
   use funits, only: dmp_log, unit_log
   use geometry, only: NO_I, NO_J, NO_K, vol
   use indices, only: i_of, j_of, k_of
   use mpi_utility, only: bcast
   use param, only: DIMENSION_BC, DIM_M, dimension_ps
   use param1, only: ONE, UNDEFINED, ZERO
   use physprop, only: CALC_MW, INERT_SPECIES
   use physprop, only: MW_AVG, MW_g
   use physprop, only: RO_g0, RO_s0, X_s0
   use physprop, only: nmax, mmax
   use ps, only: ps_defined, point_source
   use ps, only: ps_i_e, ps_i_w, ps_j_n, ps_j_s, ps_k_t, ps_k_b
   use ps, only: ps_massflow_g, ps_massflow_s, ps_volume
   use ps, only: ps_u_g, ps_v_g, ps_w_g, ps_t_g, ps_x_g
   use ps, only: ps_u_s, ps_v_s, ps_w_s, ps_t_s, ps_x_s
   use run, only: REINITIALIZING
   use scales, only: P_REF
   use toleranc, only: compare
   use usr_prop, only: usr_rog, usr_ros

#ifdef MPI
   USE mpi, only: MPI_COMM_WORLD
#endif

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: FLOW_TO_VEL                                             !
!  Author: M. Syamlal                                 Date: 28-JUL-92  !
!                                                                      !
!  Purpose: Convert volumetric and mass flow rates to velocities       !
!     A specified mass flow rate is first converted to volumetric      !
!     flow rate. The volumetric flow rate is then converted to a       !
!     velocity.                                                        !
!                                                                      !
!    When both flow rates and velocities are specified, a consistency  !
!    check is done. The first time flow_to_vel is called in by setting !
!    the logical DO_VEL_CHECK to .TRUE.. If cut-cells are not used,    !
!    flow_to_vel is only called once.  When cut-cells are used,        !
!    flow_to_vel is called another time after the cut-cell pre-        !
!    processing stage. During, the second call, the velocity check     !
!    should not be performed, because the velocity assigned suring the !
!    first call will not match the flow rate. Therefore, when called   !
!    from cut_cell_preprocessing.f DO_VEL_CHECK is set to .FALSE..     !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE FLOW_TO_VEL(DO_VEL_CHECK, M_TOT, SKIP, BCV)

      IMPLICIT NONE
!-----------------------------------------------
! Local variables
!-----------------------------------------------
      LOGICAL, intent(in) :: DO_VEL_CHECK

! loop/variable indices
      INTEGER, intent(in) :: M_TOT
      LOGICAL, intent(in) :: SKIP(DIM_M)

! loop/variable indices
      INTEGER, intent(in) :: BCV

! Whether any volumetric flow conversion was done
      LOGICAL :: CONVERTED = .FALSE.

! Loop index
      INTEGER :: M

! Mass flows rates are converted to volumetric flow rates.
      IF(BC_MASSFLOW_G(BCV) /= UNDEFINED) &
         CALL GAS_MASSFLOW_TO_VOLFLOW(BCV)

      DO M=1,M_TOT
         IF(BC_MASSFLOW_S(BCV,M) /= UNDEFINED) &
            CALL SOLIDS_MASSFLOW_TO_VOLFLOW(BCV,M,SKIP(M))
      ENDDO

! Volumetric flow rates are converted to velocities.
      IF(BC_VOLFLOW_G(BCV) /= UNDEFINED) THEN
         CALL GAS_VOLFLOW_TO_VELOCITY(DO_VEL_CHECK, BCV)
! Set the conversion flag.
         CONVERTED = .TRUE.
      ENDIF

      DO M=1,M_TOT
         IF(BC_VOLFLOW_S(BCV,M) /= UNDEFINED) THEN
            CALL SOLIDS_VOLFLOW_TO_VELOCITY(DO_VEL_CHECK,BCV,M,SKIP(M))
! Set the conversion flag.
            CONVERTED = .TRUE.
         ENDIF
      ENDDO

      IF(CONVERTED .AND. .NOT.REINITIALIZING .AND. &
         (NO_I.OR.NO_J.OR.NO_K)) THEN
         WRITE(ERR_MSG, 1100)
         CALL LOG_WARNING()
      ENDIF

      RETURN

 1100 FORMAT('Warning 1100: Some volumetric or mass flow rates have ', &
         'been converted',/'velocity. Ensure that the third (unused) ',&
         'dimension in 2D simulations',/'is correctly specified (e.g.',&
         ', in axisymmetric cylindrical coordinates',/'ZLENGTH = 2*Pi)')

      END SUBROUTINE FLOW_TO_VEL


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: GAS_MASSFLOW_TO_VOLFLOW                                 !
!  Author: M. Syamlal                                 Date: 28-JUL-92  !
!                                                                      !
!  Purpose: Convert a gas phase BC input from a mass flow rate to      !
!  a volumetric flow rate.                                             !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE GAS_MASSFLOW_TO_VOLFLOW(BCV)

      IMPLICIT NONE

      INTEGER, INTENT(in) :: BCV

! Volumetric flow rate computed from mass flow rate
      DOUBLE PRECISION :: VOLFLOW
! Average molecular weight
      DOUBLE PRECISION :: MW

! No need to convert if the mass flow is zero.
      IF(COMPARE(BC_MASSFLOW_G(BCV),ZERO)) THEN
         VOLFLOW = ZERO

! Incompressible gas BC.
      ELSEIF(RO_G0 /= UNDEFINED) THEN
         VOLFLOW = BC_MASSFLOW_G(BCV)/RO_G0

      ELSEIF(USR_ROg) THEN
! prior error checks should catch this. a massflow rate cannot be
! specified in the case of a udf for density at this time. now that
! field variables have been set it may be possible to evoke the ijk
! value as opposed to the bc region...

! Well-defined compresible gas BC.
      ELSEIF(BC_P_G(BCV)/=UNDEFINED .AND. BC_T_G(BCV)/=UNDEFINED) THEN
         IF(MW_AVG == UNDEFINED) THEN
            MW = CALC_MW(BC_X_G,DIMENSION_BC,BCV,NMAX(0),MW_G)
         ELSE
            MW = MW_AVG
         ENDIF
         VOLFLOW = BC_MASSFLOW_G(BCV) / &
            EOSG(MW,(BC_P_G(BCV)-P_REF),BC_T_G(BCV))

! Fails. This shouldn't happen as previous checks should catch any
! errors leading to this routine.
      ELSE
         WRITE(ERR_MSG, 1100) BCV
         CALL LOG_ERROR()

 1100 FORMAT('Error 1100: Boundary condition ',I3,' failed sanity check.')

      ENDIF

! Check that a specified volumetric flow matches the calculated value.
      IF(BC_VOLFLOW_G(BCV) /= UNDEFINED) THEN
         IF(.NOT.COMPARE(VOLFLOW,BC_VOLFLOW_G(BCV))) THEN
            WRITE(ERR_MSG,1101) trim(iVar('BC_MASSFLOW_g',BCV)), BCV,  &
               VOLFLOW, BC_VOLFLOW_g(BCV)
            CALL LOG_ERROR()
         ENDIF
      ELSE

! Store the calculated volumetric flow rate.
         BC_VOLFLOW_G(BCV) = VOLFLOW
      ENDIF

 1101 FORMAT('Error 1101: Volumetric flow rate calculated from ',A,/   &
         'does NOT equal the specified volumetric flow rate for BC',I3,&
         /3x,'>>> Calculated: ',G14.7,/3x,'>>> Specified:  ',G14.7)

      RETURN

      END SUBROUTINE GAS_MASSFLOW_TO_VOLFLOW


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: SOLIDS_MASSFLOW_TO_VOLFLOW                              !
!  Author: M. Syamlal                                 Date: 28-JUL-92  !
!                                                                      !
!  Purpose: Convert solids phase BC input from a mass flow rate to     !
!  a volumetric flow rate.                                             !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE SOLIDS_MASSFLOW_TO_VOLFLOW(BCV,M, SKIP_M)

      IMPLICIT NONE

! loop/variable indices
      INTEGER, INTENT(in) :: BCV, M
      LOGICAL, INTENT(in) :: SKIP_M

! Volumetric flow rate computed from mass flow rate
      DOUBLE PRECISION :: VOLFLOW
! Index of inert species
      INTEGER :: INERT

      IF(SKIP_M) THEN
         WRITE(ERR_MSG,1100) M, BCV, trim(iVar("BC_MASSFLOW_S",BCV,M))
         CALL LOG_ERROR()
      ENDIF

 1100 FORMAT('Error 1100: Solids phase ',I2,' has a specified mass ',  &
         'flow rate',/'at BC ',I3,', ',A,'. But, both BC_ROP_s and ',&
         'BC_EP_s are zero or undefined.')

      IF(COMPARE(BC_MASSFLOW_S(BCV,M),ZERO)) THEN
         VOLFLOW = ZERO
! Constant solids density.
      ELSEIF(RO_S0(M) /= UNDEFINED) THEN
         VOLFLOW = BC_MASSFLOW_S(BCV,M)/RO_S0(M)
      ELSEIF(USR_ROs(M)) THEN
! prior error checks should catch this. a massflow rate cannot be
! specified in the case of a udf for density at this time
      ELSE
! Set an alias for the inert species.
         INERT = INERT_SPECIES(M)
! Variable solids density.
         VOLFLOW = BC_MASSFLOW_S(BCV,M)/EOSS(RO_s0(M),              &
            X_s0(M,INERT), BC_X_S(BCV,M,INERT))
      ENDIF

! If volumetric flow is also specified compare both
      IF(BC_VOLFLOW_S(BCV,M) /= UNDEFINED) THEN
         IF(.NOT.COMPARE(VOLFLOW,BC_VOLFLOW_S(BCV,M))) THEN
            WRITE(ERR_MSG,1101) trim(iVar('BC_MASSFLOW_S',BCV,M)), BCV, &
               VOLFLOW, BC_VOLFLOW_S(BCV,M)
            CALL LOG_ERROR()
         ENDIF
      ELSE
         BC_VOLFLOW_S(BCV,M) = VOLFLOW
      ENDIF

      RETURN

 1101 FORMAT('Error 1101: Volumetric flow rate calculated from ',A,/   &
         'does NOT equal the specified volumetric flow rate for BC',I3,&
         /3x,'>>> Calculated: ',G14.7,/3x,'>>> Specified:  ',G14.7)

      END SUBROUTINE SOLIDS_MASSFLOW_TO_VOLFLOW


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: GAS_VOLFLOW_TO_VELOCITY                                 !
!  Author: M. Syamlal                                 Date: 28-JUL-92  !
!                                                                      !
!  Purpose: Convert gas phase volumetric rate to a velocity.           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE GAS_VOLFLOW_TO_VELOCITY(DO_VEL_CHECK, BCV)

      IMPLICIT NONE
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! loop/variable indices
      INTEGER, INTENT(in) :: BCV

! Whether any volumetric flow conversion was done
      LOGICAL, INTENT(in) :: DO_VEL_CHECK


      DOUBLE PRECISION :: SGN, OFF

! Velocity computed from volumetric flow rate
      DOUBLE PRECISION :: VEL
!-----------------------------------------------

      SELECT CASE (BC_TYPE_ENUM(BCV))
      CASE (MASS_INFLOW);  SGN =  ONE; OFF = ZERO
      CASE (MASS_OUTFLOW); SGN = -ONE; OFF = ONE
      CASE DEFAULT
        write(ERR_MSG,*) 'error in GAS_VOLFLOW_TO_VELOCITY'
        call log_error()
      END SELECT

      SELECT CASE (BC_PLANE(BCV))
      CASE ('W'); SGN = -SGN
      CASE ('S'); SGN = -SGN
      CASE ('B'); SGN = -SGN
      END SELECT

! Calculate the velocity based on the volumetric flow rate,
! BC area and BC volume fraction.
      VEL = SGN*BC_VOLFLOW_G(BCV)/(BC_AREA(BCV)*BC_EP_G(BCV))

! if the user also defined the boundary velocity through the plane, then
! check that the calculated value agrees with the specified value. if
! the user did not define the boundary velocity through the plane, then
! if mass_inflow set the value of the boundary velocity to the
! calculated value. otherwise do nothing.
      IF(BC_PLANE(BCV) == 'W' .OR. BC_PLANE(BCV)== 'E') THEN

         IF(BC_U_G(BCV) /= UNDEFINED .AND. DO_VEL_CHECK) THEN
            IF(.NOT.COMPARE(VEL,BC_U_G(BCV))) THEN
               WRITE(ERR_MSG,1100) BCV, VEL, 'BC_U_g', BC_U_G(BCV)
               CALL LOG_ERROR()
            ENDIF
         ELSE
            BC_U_G(BCV) = VEL
            BC_V_G(BCV) = OFF * BC_V_G(BCV)
            BC_W_G(BCV) = OFF * BC_W_G(BCV)
         ENDIF

      ELSEIF(BC_PLANE(BCV) == 'S' .OR. BC_PLANE(BCV)== 'N') THEN
         IF(BC_V_G(BCV) /= UNDEFINED .AND. DO_VEL_CHECK) THEN
            IF(.NOT.COMPARE(VEL,BC_V_G(BCV))) THEN
               WRITE(ERR_MSG, 1100) BCV, VEL, 'BC_V_g', BC_V_G(BCV)
               CALL LOG_ERROR()
            ENDIF
         ELSE
            BC_V_G(BCV) = VEL
            BC_U_G(BCV) = OFF * BC_U_G(BCV)
            BC_W_G(BCV) = OFF * BC_W_G(BCV)
         ENDIF

      ELSEIF(BC_PLANE(BCV) == 'B' .OR. BC_PLANE(BCV)== 'T') THEN
         IF(BC_W_G(BCV) /= UNDEFINED .AND. DO_VEL_CHECK) THEN
            IF(.NOT.COMPARE(VEL, BC_W_G(BCV))) THEN
               WRITE(ERR_MSG, 1100) BCV, VEL, 'BC_W_g', BC_W_G(BCV)
               CALL LOG_ERROR()
            ENDIF
         ELSE
            BC_W_G(BCV) = VEL
            BC_U_G(BCV) = OFF * BC_U_G(BCV)
            BC_V_G(BCV) = OFF * BC_V_G(BCV)
         ENDIF

      ENDIF

      RETURN

 1100 FORMAT(/1X,//' From: FLOW_TO_VEL',/' Message: BC No:',I2,/,&
         ' Computed velocity is not equal to specified value',/,&
         ' Value computed from vol. or mass flow  = ',G14.7,/,&
         ' Specified value (',A,') = ',G14.7,/1X,/)

      END SUBROUTINE GAS_VOLFLOW_TO_VELOCITY

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: SOLIDS_VOLFLOW_TO_VELOCITY                              !
!  Author: M. Syamlal                                 Date: 28-JUL-92  !
!                                                                      !
!  Purpose: Convert volumetric and mass flow rates to velocities       !
!     A specified mass flow rate is first converted to volumetric      !
!     flow rate. The volumetric flow rate is then converted to a       !
!     velocity.                                                        !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE SOLIDS_VOLFLOW_TO_VELOCITY(DO_VEL_CHECK, BCV, M, SKIP_M)

      IMPLICIT NONE
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! loop/variable indices
      INTEGER, INTENT(in) :: BCV, M
! Logical to perform velocity check.
      LOGICAL, INTENT(in) :: DO_VEL_CHECK, SKIP_M

! Velocity computed from volumetric flow rate
      DOUBLE PRECISION :: VEL

      DOUBLE PRECISION :: SGN, OFF

!-----------------------------------------------

      IF(SKIP_M) THEN
         WRITE(ERR_MSG,1100) M, BCV, trim(iVar("BC_VOLFLOW_S",BCV,M))
         CALL LOG_ERROR()
      ENDIF

 1100 FORMAT('Error 1100: Solids phase ',I2,' has a specified ',       &
         'volumetric flow rate',/'at BC ',I3,', ',A,'. But, both ',&
         'BC_ROP_s and BC_EP_s are zero or undefined.')

      SELECT CASE (BC_TYPE_ENUM(BCV))
      CASE (MASS_INFLOW);  SGN =  ONE; OFF = ZERO
      CASE (MASS_OUTFLOW); SGN = -ONE; OFF = ONE
      CASE DEFAULT
        write(ERR_MSG,*) 'error in SOLIDS_VOLFLOW_TO_VELOCITY'
        call log_error()
      END SELECT

      SELECT CASE (BC_PLANE(BCV))
      CASE ('W'); SGN = -SGN
      CASE ('S'); SGN = -SGN
      CASE ('B'); SGN = -SGN
      END SELECT

      IF(BC_EP_S(BCV,M) /= ZERO) THEN
         VEL = SGN * BC_VOLFLOW_S(BCV,M)/(BC_AREA(BCV)*BC_EP_S(BCV,M))
      ELSE
         IF(BC_VOLFLOW_S(BCV,M) == ZERO) THEN
            VEL = ZERO
         ELSE
            IF(DMP_LOG)WRITE (UNIT_LOG, 1101) BCV, M
            CALL LOG_ERROR()
         ENDIF
      ENDIF

 1101 FORMAT('Error 1101: BC No:',I2,' Non-zero vol. or mass flow ',&
         'specified with BC_ROP_s', I1,' = 0.')

      IF(BC_PLANE(BCV) == 'W' .OR. BC_PLANE(BCV)== 'E') THEN
         IF(BC_U_S(BCV,M) /= UNDEFINED .AND. DO_VEL_CHECK) THEN
            IF(.NOT.COMPARE(VEL, BC_U_S(BCV,M))) THEN
              WRITE(ERR_MSG, 1300) BCV, (-VEL), 'BC_U_s', M, BC_U_S(BCV,M)
              CALL LOG_ERROR()
            ENDIF
         ELSE
            BC_U_S(BCV,M) = VEL
            BC_V_S(BCV,M) = OFF * BC_V_S(BCV,M)
            BC_W_S(BCV,M) = OFF * BC_W_S(BCV,M)
         ENDIF

      ELSEIF(BC_PLANE(BCV) == 'S' .OR. BC_PLANE(BCV)== 'N') THEN
         IF(BC_V_S(BCV,M) /= UNDEFINED .AND. DO_VEL_CHECK) THEN
            IF(.NOT.COMPARE(VEL,BC_V_S(BCV,M))) THEN
               WRITE(ERR_MSG,1300) BCV, VEL, 'BC_V_s', M, BC_V_S(BCV,M)
               CALL LOG_ERROR()
            ENDIF
         ELSE
            BC_V_S(BCV,M) = VEL
            BC_U_S(BCV,M) = OFF * BC_U_S(BCV,M)
            BC_W_S(BCV,M) = OFF * BC_W_S(BCV,M)
         ENDIF

      ELSEIF(BC_PLANE(BCV) == 'B' .OR. BC_PLANE(BCV)== 'T') THEN
         IF(BC_W_S(BCV,M) /= UNDEFINED .AND. DO_VEL_CHECK) THEN
            IF(.NOT.COMPARE(VEL,BC_W_S(BCV,M))) THEN
               WRITE(ERR_MSG, 1300) BCV, VEL, 'BC_W_s', M, BC_W_S(BCV,M)
               CALL LOG_ERROR()
            ENDIF
         ELSE
            BC_W_S(BCV,M) = VEL
            BC_U_S(BCV,M) = OFF * BC_U_S(BCV,M)
            BC_V_S(BCV,M) = OFF * BC_V_S(BCV,M)
         ENDIF
      ENDIF

      RETURN

 1300 FORMAT(/1X,//' From: FLOW_TO_VEL',/' Message: BC No:',I2,/,&
         ' Computed velocity is not equal to specified value',/,&
         ' Value computed from vol. or mass flow  = ',G14.7,/,&
         ' Specified value (',A,I1,') = ',G14.7,/1X,/)

   END SUBROUTINE SOLIDS_VOLFLOW_TO_VELOCITY


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: CONVERT_CG_MI_TO_PS                                    !
!  Purpose: Convert CG_MI BCs to Point sources                         !
!                                                                      !
!  Author: Jeff Dietiker                              Date: 06-Jan-14  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE CONVERT_CG_MI_TO_PS

      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------
! loop/variable indices
      INTEGER :: IJK, M, NN, BCV
!
      INTEGER :: iproc
      INTEGER :: NPS,PSV
!-----------------------------------------------

!      print*,'Entering test',MyPE
#ifdef MPI
      CALL MPI_BARRIER(MPI_COMM_WORLD,mpierr)
#endif

! Each processor waits for its turn to find cells where to add a point source and updates the list of point sources
      do iproc = 0,NumPEs-1
         if (MyPE==iproc) Then

! First, find how many point sources are already defined. This could be regular PS or new ones
! coming from the conversion of CG_MI to PS
               NPS = 0

               PS_LP: do PSV = 1, DIMENSION_PS
                  if(.NOT.PS_DEFINED(PSV)) cycle PS_LP
                  NPS = PSV
               enddo PS_LP

!               print *,'Last PS=',NPS

! Next loop through all cells, and when a cut-cell with CG_MI is found, add a point source in this cell
            DO IJK = ijkstart3, ijkend3
               BCV = BC_ID(IJK)
               IF(BCV>0) THEN
                  IF(CG_MI_CONVERTED_TO_PS(BCV).AND.INTERIOR_CELL_AT(IJK).AND.VOL(IJK)>ZERO) THEN

                     NPS = NPS + 1

!                     print*,MyPE,NPS

                     PS_DEFINED(NPS) = .TRUE.

                     POINT_SOURCE = .TRUE.

                     PS_I_w(NPS) = I_OF(IJK)
                     PS_I_e(NPS) = I_OF(IJK)
                     PS_J_s(NPS) = J_OF(IJK)
                     PS_J_n(NPS) = J_OF(IJK)
                     PS_K_b(NPS) = K_OF(IJK)
                     PS_K_t(NPS) = K_OF(IJK)

                     PS_VOLUME(NPS) = VOL(IJK)

                     PS_MASSFLOW_g(NPS) = BC_MASSFLOW_g(BCV) * VOL(IJK) / BC_VOL(BCV)

                     PS_T_g(NPS)    = BC_T_g(BCV)

! from the logic present Jan 2019 then in noway would bc_u_g become defined by
! default and no logic checks would be conducted on its value... so going forward
! this should simply not ever be set
                     IF(BC_U_g(BCV)==UNDEFINED) THEN
                        PS_U_g(NPS)    = Normal_S(IJK,1)
                     ELSE
                        PS_U_g(NPS)    = BC_U_g(BCV)
                     ENDIF

                     IF(BC_V_g(BCV)==UNDEFINED) THEN
                        PS_V_g(NPS)    = Normal_S(IJK,2)
                     ELSE
                        PS_V_g(NPS)    = BC_V_g(BCV)
                     ENDIF

                     IF(BC_W_g(BCV)==UNDEFINED) THEN
                        PS_W_g(NPS)    = Normal_S(IJK,3)
                     ELSE
                        PS_W_g(NPS)    = BC_W_g(BCV)
                     ENDIF

                     DO NN=1,NMAX(0)
                        PS_X_g(NPS,NN)    = BC_X_g(BCV,NN)
                     ENDDO

                     DO M=1, MMAX
                        PS_MASSFLOW_s(NPS,M) = BC_MASSFLOW_s(BCV,M) * VOL(IJK) / BC_VOL(BCV)

                        PS_T_s(NPS,1)  = BC_T_s(BCV,M)

                        IF(BC_U_s(BCV,M)==UNDEFINED) THEN
                           PS_U_s(NPS,M)    = Normal_S(IJK,1)
                        ELSE
                           PS_U_s(NPS,M)    = BC_U_s(BCV,M)
                        ENDIF

                        IF(BC_V_s(BCV,M)==UNDEFINED) THEN
                           PS_V_s(NPS,M)    = Normal_S(IJK,2)
                        ELSE
                           PS_V_s(NPS,M)    = BC_V_s(BCV,M)
                        ENDIF

                        IF(BC_W_s(BCV,M)==UNDEFINED) THEN
                           PS_W_s(NPS,M)    = Normal_S(IJK,3)
                        ELSE
                           PS_W_s(NPS,M)    = BC_W_s(BCV,M)
                        ENDIF


                        DO NN=1,NMAX(M)
                           PS_X_s(NPS,M,NN)    = BC_X_s(BCV,M,NN)
                        ENDDO

                     ENDDO

!                     print*,'PS created:',NPS,PS_MASSFLOW_g(NPS),PS_VOLUME(NPS),BC_VOL(BCV)
                  ENDIF
               ENDIF

            ENDDO  ! IJK Loop

         endif  ! Work done by each processor in same order as rank

#ifdef MPI
         CALL MPI_BARRIER(MPI_COMM_WORLD,mpierr)
#endif
         call bcast(POINT_SOURCE,iproc)
         call bcast(PS_DEFINED,iproc)
         call bcast(PS_I_w,iproc)
         call bcast(PS_I_e,iproc)
         call bcast(PS_J_s,iproc)
         call bcast(PS_J_n,iproc)
         call bcast(PS_K_b,iproc)
         call bcast(PS_K_t,iproc)
         call bcast(PS_MASSFLOW_g,iproc)
         call bcast(PS_U_g,iproc)
         call bcast(PS_V_g,iproc)
         call bcast(PS_W_g,iproc)
         call bcast(PS_X_g,iproc)
         call bcast(PS_T_g,iproc)
         call bcast(PS_MASSFLOW_s,iproc)
         call bcast(PS_U_s,iproc)
         call bcast(PS_V_s,iproc)
         call bcast(PS_W_s,iproc)
         call bcast(PS_X_s,iproc)
         call bcast(PS_T_s,iproc)
         call bcast(PS_VOLUME,iproc)

      enddo

#ifdef MPI
      CALL MPI_BARRIER(MPI_COMM_WORLD,mpierr)
#endif

      RETURN

      END SUBROUTINE CONVERT_CG_MI_TO_PS

END MODULE FLOW_TO_VEL_MOD
