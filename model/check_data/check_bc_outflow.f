#include "error.inc"

MODULE CHECK_BC_OUTFLOW_MOD

   use error_manager

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
! Subroutine: CHECK_BC_OUTFLOW                                         !
! Author: J.Musser                                    Date: 01-Mar-14  !
!                                                                      !
! Purpose: Provided basic checks as possible and return any error      !
! message concerning specification of bc_ep_g + bc_rop_s at an         !
! outflow boundary type or pressure inflow boundary.  Note that a      !
! more rigorous check on bc_ep_g and bc_rop_s for MI/MO cells is       !
! made in separate routines. It is also worth noting that in general   !
! bc_ep_g and bc_rop_s should probably not really be set in outflow    !
! boundaries...but instead reflect their upstream neighbor. MFIX       !
! allows this but it may be worth just eliminating...                  !
!                                                                      !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
      SUBROUTINE CHECK_BC_OUTFLOW(M_TOT, BCV)

! Modules
! --------------------------------------------------------------------//
      use bc, only: bc_ep_g, bc_rop_s
      use param1, only: one, undefined, zero
      use physprop, only: ro_s0
      use run, only: solids_model
      use usr_prop, only: usr_ros
      use toleranc, only: compare

      IMPLICIT NONE

! Dummy arguments
! --------------------------------------------------------------------//
! loop/variable indices
      INTEGER, INTENT(in) :: BCV
      INTEGER, INTENT(in) :: M_TOT

! Local variables
! --------------------------------------------------------------------//
      INTEGER :: M
      DOUBLE PRECISION :: SUM_EP
      LOGICAL :: FLAG_WARNING

      FLAG_WARNING = .TRUE.

! A user may have set bc_rop_s in a PO, PI or O boundary (recall
! the checks for a MO/MI boundary are separate). Here we are stopping
! a user from setting bc_rop_s when using usr_ros for these boundaries..
! 1) for MO or O such a value is not used by the solver anyway...
! 2) for PO/PI such a value could be set at the boundary, however,
!    if left unspecified it would take the neighbor fluid value
      DO M = 1, M_TOT
! for usr_ros we cannot have bc_rop_s specified since we don't have
! ro_s in the boundary...
         IF (USR_ROS(M)) THEN
            IF (BC_ROP_S(BCV,M) /= UNDEFINED) THEN
               write(err_msg, 1104) trim(iVar('BC_ROP_S',BCV,M)), &
               trim(iVar('USR_ROS',M))
               CALL LOG_ERROR()
            ENDIF
         ENDIF
      ENDDO
 1104 FORMAT('Error 1104:',A,' cannot be specified when using ',&
             A,/,'Please correct project settings.')

! if bc_ep_g is defined at the outflow boundary, then the sum of ep_g
! and ep_s at the boundary may not equal one given the code in the
! subroutine set_outflow (see code for details).
! therefore if bc_ep_g and/or bc_rop_s are defined, perform possible
! data consistency checks and, when appropriate, provide the user with
! a warning/error about their chosen settings.

      IF (BC_EP_G(BCV) /= UNDEFINED) THEN

         SUM_EP = BC_EP_G(BCV)
         DO M = 1, M_TOT
            IF(SOLIDS_MODEL(M) /= 'TFM' .AND. FLAG_WARNING) THEN
               WRITE(ERR_MSG, 1101) trim(iVar('BC_EP_g',BCV))
               CALL LOG_WARNING()
               FLAG_WARNING = .FALSE.
            ENDIF

            IF(BC_ROP_S(BCV,M) == UNDEFINED) THEN
               IF(BC_EP_G(BCV) == ONE) THEN
! What does it mean to set solids bulk density to zero at the boundary?
! The flux at that plane involves an average ROP of that set in the
! boundary cell and that of the fluid cell... so it does enter the
! solution?
                  BC_ROP_S(BCV,M) = ZERO
               ELSEIF(M_TOT == 1) THEN
! ro_s0 may not be defined for variable density cases, in which case
! bc_rop_s remains undefined
                  IF (RO_s0(M) /= UNDEFINED) THEN
                     BC_ROP_S(BCV,M) = (ONE - BC_EP_G(BCV))*RO_S0(M)
                  ELSE
                     WRITE(ERR_MSG, 1102) trim(iVar('BC_EP_g',BCV))
                     CALL LOG_ERROR()
                  ENDIF
               ELSE
! bc_ep_g is defined but some bc_rop_s(m) are undefined; in this case,
! according to set_outflow ep_p in the outflow boundary will be based
! on the user defined value of bc_ep_g, while rop_s would become based
! on the value in the adjacent fluid cell. consequently, no check ensures
! the result is consistent with a requirement for ep_g+ep_s=1. So here
! we simply force the user to modify their BC setting
                  WRITE(ERR_MSG, 1102) trim(iVar('BC_EP_g',BCV))
                  CALL LOG_ERROR()
               ENDIF
            ENDIF  ! end if(bc_rop_s(bcv,m) == undefined)
 1102 FORMAT('Error: Volume fraction may not sum to one when ',/&
         A,' is defined.')

! by this point bc_rop_s should either be defined or mfix exited
! therefore we can check that sum of void fraction and solids volume
! fractions
            SUM_EP = SUM_EP + BC_ROP_S(BCV,M)/RO_S0(M)
        ENDDO

! now verify that the volume fractions sum to one.
        IF(.NOT.COMPARE(SUM_EP,ONE)) THEN
           WRITE(ERR_MSG,1103) BCV, trim(iVal(SUM_EP))
           CALL LOG_ERROR()
       ENDIF

! bc_ep_g is not defined but check if any bc_rop_s are defined
      ELSE

         SUM_EP = ZERO
         DO M = 1, M_TOT
            IF(BC_ROP_S(BCV,M) /= UNDEFINED) THEN
               IF(SOLIDS_MODEL(M) /= 'TFM') THEN
                  WRITE(ERR_MSG, 1101) trim(iVar('BC_ROP_s',BCV,M))
                  CALL LOG_WARNING()
               ENDIF
               IF (RO_S0(M) /= UNDEFINED) THEN
                  SUM_EP = SUM_EP + BC_ROP_S(BCV,M)/RO_S0(M)
               ENDIF
            ENDIF
         ENDDO

! verify that the sum of any specified volume fractions is not greater
! than one
         IF(SUM_EP > ONE) THEN
            WRITE(ERR_MSG,1103) BCV, trim(iVal(SUM_EP))
            CALL LOG_ERROR()
         ENDIF

      ENDIF

      RETURN

 1101 FORMAT('Warning 1101: ',A,' should not be specified for ', &
         'outflow BCs',/'with DEM/PIC runs except for a mass outflow ',&
         'boundary with specified ',/ 'flow rate(s). In this case ',&
         'volume fraction data is used for ',/ 'conversion to ',&
         'velocity(s). However, the solids volume fraction data ',/&
         'is effectively disregarded and it is the solids velocity ',&
         'that is ',/'used to direct any solids at the boundary.')

 1103 FORMAT('Error 1103: Illegal boundary condition region: ',I3,'. ',&
         'Sum of volume',/'fractions does NOT equal ONE. (SUM = ',A,   &
         ')',/'Please correct the project settings.')

      END SUBROUTINE CHECK_BC_OUTFLOW


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
! Subroutine: CHECK_BC_PRESSURE_FLOW                                   !
! Author: J.Musser                                    Date: 01-Mar-14  !
!                                                                      !
! Purpose: Provided a detailed error message on bc                     !
!                                                                      !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
      SUBROUTINE CHECK_BC_PRESSURE_FLOW(M_TOT, BCV)

! Modules
! --------------------------------------------------------------------
      USE param1, only: UNDEFINED
      USE param1, only: ZERO
      use physprop, only: RO_g0
      use bc, only: BC_P_g

      IMPLICIT NONE

! Dummy arguments
! --------------------------------------------------------------------
! loop/variable indices
      INTEGER, INTENT(in) :: BCV
      INTEGER, INTENT(in) :: M_TOT
! --------------------------------------------------------------------

      IF (BC_P_G(BCV) == UNDEFINED.AND.RO_G0>ZERO) THEN
         WRITE(ERR_MSG,1000) trim(iVar('BC_P_g',BCV))
         CALL LOG_ERROR()

      ELSEIF (BC_P_G(BCV)<=ZERO .AND. RO_G0==UNDEFINED) THEN
         WRITE(ERR_MSG, 1100) BCV, trim(iVal(BC_P_G(BCV)))
         CALL LOG_ERROR()
      ENDIF

 1100 FORMAT('Error 1100: Pressure must be greater than zero for ',    &
         'compressible flow',/3x,'BC_P_g(',I3,') = ',A,/'Please ',     &
         'correct the project settings.')

      RETURN

 1000 FORMAT('Error 1000: Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

      END SUBROUTINE CHECK_BC_PRESSURE_FLOW


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
! Subroutine: CHECK_BC_MASS_OUTFLOWA                                   !
! Author: J.Musser                                    Date: 01-Mar-14  !
!                                                                      !
! Comments:                                                            !
!     The velocities at the outflow face are fixed and the momentum    !
!     equations are not solved in the outflow cells. Since the flow    !
!     is out of the domain none of the other scalars should need to    !
!     be specified (e.g., mass fractions, void fraction, etc.,).       !
!     Such values will become defined according to their adjacent      !
!     fluid cell. Checks are made on massflow/volflow/velocity in      !
!     a separate routine.                                              !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_BC_MASS_OUTFLOWA(M_TOT, BCV)

! Modules
! --------------------------------------------------------------------

      use bc, only: bc_plane
      use bc, only: bc_dt_0, bc_massflow_g
      use bc, only: bc_massflow_s, bc_volflow_s
      use bc, only: bc_p_g, bc_t_g
      use bc, only: bc_u_g, bc_v_g, bc_w_g
      use physprop, only: ro_g0
      use param1, only: undefined, zero

      IMPLICIT NONE

! Dummy arguments
! --------------------------------------------------------------------
! loop/variable indices
      INTEGER, intent(in) :: BCV
      INTEGER, intent(in) :: M_TOT
! Local variables
! --------------------------------------------------------------------
      INTEGER :: M

      IF(BC_DT_0(BCV) == UNDEFINED) THEN
         WRITE(ERR_MSG, 1000) trim(iVar('BC_DT_0',BCV))
         CALL LOG_ERROR()
      ENDIF

      DO M = 1, M_TOT
         IF(BC_MASSFLOW_S(BCV,M) /= UNDEFINED .OR. &
            BC_VOLFLOW_S(BCV,M) /= UNDEFINED) THEN
            WRITE(ERR_MSG,1102) trim(iVar('BC_MASSFLOW_S',BCV,M)), &
               trim(iVar('BC_VOLFLOW_S',BCV,M))
            CALL LOG_WARNING()
         ENDIF
 1102 FORMAT('Warning 1102: ', A,' and/or ', A,' have been defined',/&
         'at a mass outflow boundary. A specified solids flow ',&
         'rate may not be ',/'physically achievable depending on the ',&
         'system and simulation ',/'setup.')
      ENDDO


! Until we can set ro_g in the BC cell before call to set_bc_flow this
! check is requisite...ideally p_g and t_g would come from the flow
! domain not as a bc...; however flow_to_vel would also need to be
! restricted to use a field ro_g and not a 'bc' ro_g
      IF(RO_G0 == UNDEFINED .AND. (BC_P_G(BCV) == UNDEFINED .OR.       &
         BC_T_G(BCV) == UNDEFINED) .AND.BC_MASSFLOW_G(BCV) /= ZERO) THEN

         IF(BC_PLANE(BCV)=='W' .OR. BC_PLANE(BCV)=='E') THEN
            IF(BC_U_G(BCV) /= ZERO) THEN
               WRITE(ERR_MSG, 1100) BCV, 'BC_U_g'
               CALL LOG_ERROR()
            ENDIF
         ELSEIF(BC_PLANE(BCV)=='N' .OR. BC_PLANE(BCV)=='S') THEN
            IF(BC_V_G(BCV) /= ZERO) THEN
               WRITE(ERR_MSG, 1100) BCV, 'BC_V_g'
               CALL LOG_ERROR()
            ENDIF
         ELSEIF (BC_PLANE(BCV)=='T' .OR. BC_PLANE(BCV)=='B') THEN
            IF(BC_W_G(BCV) /= ZERO) THEN
               WRITE(ERR_MSG, 1100)  BCV, 'BC_W_g'
               CALL LOG_ERROR()
            ENDIF
         ENDIF
      ENDIF   ! end if/else (ro_g0 /=undefined)

 1100 FORMAT('Error 1100: Invalid mass outflow boundary condition: ',  &
         I3,/'RO_g0, BC_P_g, and BC_T_g are UNDEFINED and ',A,' is ',  &
         'non-zero',/'Please correct the project settings.')

      RETURN

 1000 FORMAT('Error 1000: Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

   END SUBROUTINE CHECK_BC_MASS_OUTFLOWA


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
! Subroutine: CHECK_BC_MASS_OUTFLOWB                                   !
! Author: J.Musser                                    Date: 01-Mar-14  !
!                                                                      !
! Purpose: Provided a detailed error message on BC                     !
!                                                                      !
! Comments:                                                            !
!     The velocities at the outflow face are fixed and the momentum    !
!     equations are not solved in the MO cells. Depending on user      !
!     specification (mass flow or volume flow or velocity)             !
!     different checks need to be made. Consistently on the sum of      !
!     the volume fractions at the boundary are made as needed.         !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_BC_MASS_OUTFLOWB(M_TOT, SKIP, BCV)

! Modules
!---------------------------------------------------------------------
      use bc, only: bc_ep_g, bc_p_g
      use bc, only: bc_rop_s, bc_ep_s, bc_x_s
      use bc, only: bc_massflow_g, bc_massflow_s
      use bc, only: bc_volflow_g, bc_volflow_s
      use eos, only: eoss
      use param, only: dim_m
      use param1, only: undefined, one, zero
      use physprop, only: ro_g0, ro_s0
      use physprop, only: inert_species, x_s0
      use run, only: solve_ros
      use toleranc, only: compare
      use usr_prop, only: usr_ros, usr_rog
      use check_bc_inflow_mod, only: check_bc_usr_ro

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------
      INTEGER, INTENT(in) :: M_TOT
      LOGICAL, INTENT(in) :: SKIP(DIM_M)
      INTEGER, INTENT(in) :: BCV

! Local variables
!---------------------------------------------------------------------
! loop/variable indices
      INTEGER :: M
      DOUBLE PRECISION :: SUM_EP
! Solids phase density in BC region.
      DOUBLE PRECISION :: BC_ROs(DIM_M)
! Index of inert species
      INTEGER :: INERT
! count of number of phases skipped from boundary volume fraction
! checks due to a user variable density model
      INTEGER :: lskip

!---------------------------------------------------------------------

! The check on velocities, among other checks, is made slightly later
! in set_bc0_flow -> check_bc_vel_inflow. Massflow or volflow needs
! to be converted to velocity using known density/volume fraction.   
! So such checks are delayed until the calculations can be done.


! Check gas phase volume fraction.
! BC_EP_G is needed to convert massflow and volflow to a velocity
! Should this come from the upstream value however... 
      IF(BC_EP_G(BCV) == UNDEFINED) THEN
         IF(BC_MASSFLOW_G(BCV) /= UNDEFINED .AND. &
            BC_MASSFLOW_G(BCV) /= ZERO) THEN
            WRITE(ERR_MSG, 1096) trim(iVar('BC_EP_G',BCV)), &
            trim(iVar('BC_MASSFLOW_G',BCV))
            CALL LOG_ERROR()
         ENDIF
         IF(BC_VOLFLOW_G(BCV) /= UNDEFINED .AND. &
            BC_VOLFLOW_G(BCV) /= ZERO) THEN
            WRITE(ERR_MSG, 1096) trim(iVar('BC_EP_G',BCV)), &
            trim(iVar('BC_MASSFLOW_G',BCV))
            CALL LOG_ERROR()
         ENDIF

 1096 FORMAT('Error 1096: ',A,' must be defined when requesting',/, &
         'a non-zero ', A,'. Please correct the project settings.')
      ENDIF

! Verify compressible boundary condition variables.
! TODO: It may be possible to update this check so that bc_p_g is
! not needed for usr_rog
      IF(RO_G0 == UNDEFINED) THEN
         IF(BC_P_G(BCV) == UNDEFINED) THEN
            IF(BC_MASSFLOW_G(BCV) /= UNDEFINED .AND.                   &
               BC_MASSFLOW_G(BCV) /= ZERO) THEN
               WRITE(ERR_MSG, 1100) trim(iVar('BC_P_g',BCV))
               CALL LOG_ERROR()
            ENDIF
 1100 FORMAT('Error 1100: ',A,' must be specified for compressible ',  &
         'flows',/'when specifying BC_MASSFLOW_g to make the ',        &
         'conversion to velocity.',/'Please correct the project '&
         'settings.')

         ELSEIF(BC_P_G(BCV) <= ZERO) THEN
            WRITE(ERR_MSG, 1101) BCV, trim(iVal(BC_P_G(BCV)))
            CALL LOG_ERROR()
         ENDIF
 1101 FORMAT('Error 1101: Pressure must be greater than zero for ',    &
         'compressible flow',/' >>>  BC_P_g(',I3,') = ',A,/'Please ',  &
         'correct the project settings.')
      ENDIF

      CALL CHECK_BC_USR_RO(M_TOT, BCV)

! Check solids phase volume fraction; the following is a complicated
! series of checks depending on user configuration. Technically,
! bc_ep_s/bc_rop_s are only needed if bc_massflow_s or bc_volflow_s
! is specified. However, the code checks for consistency of the
! definitions whenever they are set (needed or not).

! Calculate the solids volume fraction from the gas phase if there is
! only one solids phase. Do this check regardless of whether it needs
! to be set or not.
      IF(M_TOT == 1 .AND. BC_EP_S(BCV,1) == UNDEFINED) THEN
         IF (BC_EP_G(BCV) /= UNDEFINED) BC_EP_S(BCV,1) = ONE - BC_EP_g(BCV)
      ENDIF

      DO M = 1, M_TOT

! Checking if massflow or volflow has been specified for phase m
         IF((BC_MASSFLOW_S(BCV,M) /= UNDEFINED .AND. &
             BC_MASSFLOW_S(BCV,M) /= ZERO) .OR.      &
            (BC_VOLFLOW_S(BCV,M) /= UNDEFINED .AND.  &
             BC_VOLFLOW_S(BCV,M) /= ZERO)) THEN
! If so, then check if rop_s or ep_s are also be specified or can be
! calculated based on ep_g
             IF (BC_ROP_S(BCV,M) == UNDEFINED .AND. &
                 BC_EP_S(BCV,M) == UNDEFINED) THEN
! For M>1, rop_s/ep_s can only be calculated if ep_g=1; If M=1, then
! ep_s will have already been set if undefined (see above).
                IF (BC_EP_G(BCV) /= UNDEFINED) THEN
                   IF (M_TOT > 1 .AND. .NOT. COMPARE(BC_EP_G(BCV),ONE)) THEN
                     WRITE(ERR_MSG, 1201) M, BCV, 'BC_ROP_s or BC_EP_s'
                     CALL LOG_ERROR()
                   ENDIF
                ELSE
! bc_ep_g is undefined and bc_rop_s/bc_ep_s is undefined while volflow_s
! and/or massflow_s are defined; error not enough information at boundary
                   WRITE(ERR_MSG, 1201) M, BCV, 'BC_ROP_s or BC_EP_s'
                   CALL LOG_ERROR()
                ENDIF
             ENDIF
         ELSE
! In this case velocities should be specified. Ultimately the check on
! velocities, among other checks, is made slightly later in set_bc_flow
! by check_bc_vel_inflow.
         ENDIF
      ENDDO
 1201 FORMAT('Error 1201: Insufficient solids phase ',I2,' data ',     &
         'for BC',I3,'. ',/A,' not specified.',/'Please correct the ', &
         'project settings.')

! At this point a check has been made to ensure that bc_rop_s or bc_ep_s
! have been set if they are needed. Similarly for bc_ep_g. However, if
! they are not needed then they may not necessarily be defined.

! If the user has provided bc_ep_g but not defined bc_rop_s or bc_ep_s
! (and they cannot be readily calculated based on bc_ep_g), then the
! boundary may evolve inconsistent values for these quantities (simply
! due to setting such outflow bc cells based on neighbor fluid cells
! when undefined). Even though this information is not used by any
! calculation, request the user redefine their bc more appropriately.
      IF (BC_EP_G(BCV) /= UNDEFINED) THEN
! This is a very similar check as above but covers the case when
! massflow or volflow were not specified
         IF(M_TOT > 1 .AND. .NOT.COMPARE(BC_EP_g(BCV),ONE)) THEN
! Bulk density or solids volume fraction should be explicitly defined
! if there are more than one solids phase and bc_ep_g is defined but
! not 1.
            DO M = 1, M_TOT
               IF(BC_ROP_S(BCV,M) == UNDEFINED .AND. &
                  BC_EP_S(BCV,M) == UNDEFINED) THEN
                  WRITE(ERR_MSG, 1205) BCV, M, 'BC_ROP_s or BC_EP_s'
                  CALL LOG_ERROR()
               ENDIF
            ENDDO
         ENDIF
 1205 FORMAT('Error 1205: Incomplete specification for BC', I3,'.',/&
         'BC_EP_G is defined but insufficient corresponding solids ',&
         'phase ',I2,' data:',/,A,' not specified. Please correct ',&
         'the project settings.')
! If bc_ep_g is 1, then we can set/clear any undefined bc_ep_s and
! bc_rop_s to zero for a consistent bc definition.
         DO M = 1, M_TOT
            IF(SKIP(M)) THEN
               BC_EP_S(BCV,M)  = ZERO
               BC_ROP_S(BCV,M) = ZERO
               CYCLE
            ENDIF
         ENDDO
      ENDIF

! At this point, if bc_ep_g was defined then bc_rop_s and/or bc_ep_s must
! also be fully specified for all phases. However, bc_ep_g and bc_rop_s/
! bc_ep_s still may not necessarily be defined if they are not needed. So
! now check if any bc_ep_s or bc_rop_s are defined. If any one phase is
! specified, then all should be set otherwise return an error. If they
! are all set, we can also calculate bc_ep_g (if undefined).
! Initialize the sum of the total volume fraction.
      SUM_EP = ZERO
      lskip = 0
      DO M = 1, M_TOT
         IF(BC_ROP_S(BCV,M) == UNDEFINED .AND. &
            BC_EP_S(BCV,M) == UNDEFINED) THEN
            lskip = lskip + 1
         ENDIF
      ENDDO

      IF (lskip /= M_TOT .AND. lskip /= ZERO) then
! Some phases are set and some are not. Return an error; do not attempt
! to guess user intention.
         WRITE(ERR_MSG, 1206) BCV, 'BC_ROP_s or BC_EP_s', M
         CALL LOG_ERROR()
 1206 FORMAT('Error 1206: Incomplete specification for BC', I3,'.',/&
         A,' is not specified for solids phase ',I2,' but is given ',/&
         'other solids phases. Please correct the project settings.')
      ELSEIF (lskip == M_TOT) THEN
! No phase specification has been made at the boundary, that is, all
! bc_rop_s/bc_ep_s are undefined as is bc_ep_g. This is ok. Do nothing.
! Set mass inflow routine will set bc values to neighbor fluid value.
         RETURN
      ELSEIF (lskip == zero) THEN
         IF (BC_EP_G(BCV) /= UNDEFINED) THEN
! perform checks (below) for consistency of boundary specification
! Initialize the sum of the total volume fraction.
            SUM_EP = BC_EP_G(BCV)
         ELSEIF (BC_EP_G(BCV) == UNDEFINED) THEN
! perform checks (below) for consistency, and define bc_ep_g according
! to sum of bc_rops/bc_ep_s
            SUM_EP = ZERO
         ENDIF
      ENDIF

! At this point we have made a number of checks to ensure one of the
! following:
! 1) either all bc_ep_g, bc_rop_s/bc_ep_s are defined or
! 2) all bc_rop_s/bc_ep_s are defined such that we might back out
!    bc_ep_g
! So now conduct checks to ensure any user settings of volume/void
! fraction are self-consistent (whether needed or not)
      DO M = 1, M_TOT

! Set the solids density for the BC region.
         IF(SOLVE_ROs(M)) THEN
! presence of non-zero inert species is checked by bc_inflow
            INERT = INERT_SPECIES(M)
            BC_ROs(M) = EOSS(RO_s0(M), X_s0(M,INERT),&
               BC_X_S(BCV,M,INERT))
         ELSEIF (USR_ROs(M)) THEN
! cannot readily set solids density in bc when using udf for density
! specification of rop_s will have been caught above.
         ELSE
            BC_ROs(M) = RO_s0(M)
         ENDIF

! If both input parameters are defined. Make sure they are equivalent.
! (Not possible when udf for density is requested).
         IF(BC_ROP_S(BCV,M) /= UNDEFINED .AND.                         &
            BC_EP_S(BCV,M) /= UNDEFINED) THEN

            IF(.NOT.COMPARE(BC_EP_S(BCV,M)*BC_ROs(M),                  &
               BC_ROP_S(BCV,M))) THEN
               WRITE(ERR_MSG,1214) BCV
               CALL LOG_ERROR()
            ENDIF
 1214 FORMAT('Error 1214: Illegal initial condition region : ',I3,/    &
         'BC_EP_s and BC_ROP_s are inconsistent. Please correct the ',/&
         'project settings.')

! Compute BC_EP_s from BC_ROP_s
         ELSEIF(BC_EP_S(BCV,M) == UNDEFINED .AND. .NOT. USR_ROS(M)) THEN
! at this point bc_ep_s should have to be defined in the case of a udf
! for density if volflow_s was defined as bc_rop_s will be rejected.
            BC_EP_S(BCV,M) = BC_ROP_S(BCV,M) / BC_ROs(M)

! Compute BC_ROP_s from BC_EP_s and BC_ROs
         ELSEIF(BC_ROP_S(BCV,M) == UNDEFINED .AND. .NOT. USR_ROS(M)) THEN
! bc_ros it not defined when allowing a udf for density; so bc_rop_s will
! still be undefined here..
            BC_ROP_S(BCV,M) = BC_EP_S(BCV,M) * BC_ROs(M)

         ENDIF
! Add this phase to the total volume fraction.
         SUM_EP = SUM_EP + BC_EP_S(BCV,M)
      ENDDO

! Verify that the volume fractions sum to one.
      IF(BC_EP_G(BCV) /= UNDEFINED) THEN
         IF(.NOT.COMPARE(SUM_EP,ONE)) THEN
            WRITE(ERR_MSG,1215) BCV, trim(iVal(SUM_EP))
            CALL LOG_ERROR()
         ENDIF
      ELSE
        BC_EP_G(BCV) = ONE - SUM_EP
         IF( BC_EP_G(BCV) <= ZERO) THEN
            WRITE(ERR_MSG,1216) BCV, trim(iVal(SUM_EP))
            CALL LOG_ERROR()
         ENDIF
      ENDIF
 1215 FORMAT('Error 1215: Illegal boundary condition region: ',I3,'. ',&
         'Sum of volume',/'fractions does NOT equal ONE. (SUM = ',A,   &
         ')',/'Please correct the project settings.')
 1216 FORMAT('Error 1216: Illegal boundary condition region: ',I3,'. ',&
         'Sum of solids ',/,'volume fractions exceeds ONE. (SUM = ',A, &
         ')',/'Please correct the project settings.')

      RETURN
      END SUBROUTINE CHECK_BC_MASS_OUTFLOWB

END MODULE CHECK_BC_OUTFLOW_MOD
