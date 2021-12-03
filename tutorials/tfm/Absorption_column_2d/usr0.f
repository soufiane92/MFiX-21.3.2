#include "error.inc"

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR0                                                   C
!  Purpose: This routine is called before the time loop starts and is  C
!           user-definable.  The user may insert code in this routine  C
!           or call appropriate user defined subroutines.  This        C
!           can be used for setting constants and checking errors in   C
!           data.  This routine is not called from an IJK loop, hence  C
!           all indices are undefined.                                 C
!                                                                      C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced:                                               C
!  Variables modified:                                                 C
!                                                                      C
!  Local variables:                                                    C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE USR0

! Global Variables
!---------------------------------------------------------------------//

! Global parameters
!---------------------------------------------------------------------//

! Modules procedures
!---------------------------------------------------------------------//
      use error_manager

      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------//

! Include statement functions
!---------------------------------------------------------------------//

      CALL CHECK_USR_INPUT
      CALL ALLOCATE_USR_ARRAYS
      CALL SET_INIT_USR_PROPERTIES

      CALL SETUP_DATAEXTRACT
      CALL WRITE_USR0p1

      RETURN
      END SUBROUTINE USR0


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: check_usr_input                                         C
!  Purpose: Check the user input that is possible for the solvent      C
!  absorption model.                                                   C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CHECK_USR_INPUT

! Global variables/parameters
!---------------------------------------------------------------------//
      use constant, only: c

      use usr, only: solvent_absorption
      use usr, only: index_sol, index_liq
      use usr, only: mech_dispersion, spread_factor
      use usr, only: enhancement_factor
      use usr, only: sa_pack, d_pack, omega_pack
      use usr, only: omega_l0
      use usr, only: lam_mu_s0, lam_mu_g0

      use usr, only: CAP_PRESS_TYPE, CAP_PRESS_TYPE_ENUM
      use usr, only: UNDEFINED_CAP_PRESS_TYPE
      use usr, only: GROSSER_1988

      use usr, only: USR_DRAG_TYPE, USR_DRAG_TYPE_ENUM
      use usr, only: UNDEFINED_USR_DRAG_TYPE
      use usr, only: ATTOU_99, ATTOU_99_MOD, SOLOMENKO_15
      use usr, only: LAPPALAINEN_09, LAPPALAINEN_09_MOD

      use usr, only: wetarea_type, wetarea_type_enum
      use usr, only: UNDEFINED_WETAREA_TYPE
      use usr, only: LAPPALAINEN_08, ONDA_68, BILLET_95, CONSTANT_WAF
      use usr, only: wetareafrac0, apply_waf, ch_pack

      use usr, only: absorption_chem_type, absorption_chem_type_enum
      use usr, only: undefined_absorption_chem_type
      use usr, only: single_step, equilibrium_segregated
      use usr, only: equilibrium_coupled

      use usr, only: mass_transfer_type, mass_transfer_type_enum
      use usr, only: undefined_mass_transfer_type
      use usr, only: onda_1968_mtc, constant_mtc, psfirst_lmtc

      use geometry, only: ylength, no_k
      use run, only: UNITS
      use run, only: drag_type, drag_type_enum, user_drag
      use rxns, only : NO_OF_RXNS
      use param1, only: undefined, undefined_c, zero, one
      use param1, only: large_number
      use physprop, only: d_p0, mu_s0, mu_g0
      use error_manager
      use usr_src, only: call_usr_source
      use usr_prop, only: usr_fss
      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------//
! local index
      INTEGER :: L, Ll
!---------------------------------------------------------------------//

      IF (.NOT.SOLVENT_ABSORPTION) THEN
         CAP_PRESS_TYPE_ENUM = UNDEFINED_CAP_PRESS_TYPE
         USR_DRAG_TYPE_ENUM = UNDEFINED_USR_DRAG_TYPE
         ABSORPTION_CHEM_TYPE_ENUM = UNDEFINED_ABSORPTION_CHEM_TYPE
         RETURN
      ENDIF

! check usr_drag_type is set to an available option
!-------------------------------------------------//
      IF (DRAG_TYPE_ENUM /= USER_DRAG) THEN
         write(err_msg, 1140)
 1140 format('Error 1140: DRAG_TYPE must be set to USER_DRAG when ',/,&
         'invoking solvent_absorption model.')
         CALL LOG_ERROR()
      ENDIF

      IF (USR_DRAG_TYPE /= UNDEFINED_C) THEN
         SELECT CASE(trim(Adjustl(USR_DRAG_TYPE)))
         CASE ('ATTOU_99')
            USR_DRAG_TYPE_ENUM = ATTOU_99
         CASE ('ATTOU_99_MOD')
            USR_DRAG_TYPE_ENUM = ATTOU_99_MOD
         CASE ('SOLOMENKO_15')
            USR_DRAG_TYPE_ENUM = SOLOMENKO_15
            IF (.NOT.APPLY_WAF) THEN
               WRITE(ERR_MSG,1149)
               CALL LOG_WARNING()
            ENDIF
         CASE ('LAPPALAINEN_09')
            USR_DRAG_TYPE_ENUM = LAPPALAINEN_09
            IF (.NOT.CALL_USR_SOURCE(3) .OR. .NOT.CALL_USR_SOURCE(4) .OR. &
               (.NOT.CALL_USR_SOURCE(5) .AND. .NOT.NO_K)) THEN
               WRITE(ERR_MSG,1147)
               CALL LOG_ERROR()
            ENDIF
            IF (.NOT.APPLY_WAF) THEN
               WRITE(ERR_MSG,1149)
               CALL LOG_WARNING()
            ENDIF
         CASE ('LAPPALAINEN_09_MOD')
            USR_DRAG_TYPE_ENUM = LAPPALAINEN_09_MOD
            IF (.NOT.CALL_USR_SOURCE(3) .OR. .NOT.CALL_USR_SOURCE(4) .OR. &
               (.NOT.CALL_USR_SOURCE(5) .AND. .NOT.NO_K)) THEN
               WRITE(ERR_MSG,1147)
               CALL LOG_ERROR()
            ENDIF
            IF (.NOT.APPLY_WAF) THEN
               WRITE(ERR_MSG,1149)
               CALL LOG_WARNING()
            ENDIF
         CASE DEFAULT
            USR_DRAG_TYPE_ENUM = UNDEFINED_USR_DRAG_TYPE
         END SELECT

         IF (USR_DRAG_TYPE_ENUM == UNDEFINED_USR_DRAG_TYPE) THEN
            WRITE(ERR_MSG,1106) 'USR_DRAG_TYPE', &
                trim(adjustl(USR_DRAG_TYPE))
            CALL LOG_ERROR()
         ENDIF

         IF (.NOT.USR_FSS(1)) THEN
            WRITE(ERR_MSG,1148)
            CALL LOG_ERROR()
         ENDIF
      ELSE
         USR_DRAG_TYPE_ENUM = UNDEFINED_USR_DRAG_TYPE
      ENDIF
 1147 format("Error 1147: the selected drag model results in ",&
        "additional",/"source terms that require CALL_USR_SOURCE,"&
        "be set to true for all momentum equations.")
 1148 format("Error 1148: USR_FSS(1) must be true when invoking ",&
        "this module",/"so that the liquid-solid drag is calculated ",&
        "appropriately.")
 1149 format("Warning 1149: APPLY_WAF should be .TRUE. to remain ",&
       "consistent",/"with the published form of the selected drag ",&
       "model.")

! check settings for 'calibration factors' for momentum interaction f
! forces
      if (c(5) == undefined) then
         write(err_msg, 1141)
         call log_warning()
 1141 format("Warning 1141: setting the calibration factor, c(5), to ",&
        "1 for the",/"viscous term in gas-liquid interaction model")
            c(5) = one
      elseif (c(5) <= 0) then
         WRITE(err_msg, 1106) 'C(5)', ival(C(5))
         CALL LOG_ERROR()
      endif
      if (c(6) == undefined) then
         write(err_msg, 1142)
         call log_warning()
 1142 format("Warning 1142: setting the calibration factor, c(6), to ",&
        "1 for the",/"inertial term in gas-liquid interaction model")
            c(6) = one
      elseif (c(6) <= 0) then
         WRITE(err_msg, 1106) 'C(6)', ival(C(6))
         CALL LOG_ERROR()
      endif
      if (c(7) == undefined) then
         write(err_msg, 1143)
         call log_warning()
 1143 format("Warning 1143: setting the calibration factor, c(7), to ",&
        "1 for the",/"viscous term in gas-solids interaction model")
            c(7) = one
      elseif (c(7) <= 0) then
         WRITE(err_msg, 1106) 'C(7)', ival(C(7))
         CALL LOG_ERROR()
      endif
      if (c(8) == undefined) then
         write(err_msg, 1144)
         call log_warning()
 1144 format("Warning 1144: setting the calibration factor, c(8), to ",&
        "1 for the",/"inertial term in gas-solids interaction model")
            c(8) = one
      elseif (c(8) <= 0) then
         WRITE(err_msg, 1106) 'C(8)', ival(C(8))
         CALL LOG_ERROR()
      endif
      if (c(9) == undefined) then
         write(err_msg, 1145)
         call log_warning()
 1145 format("Warning 1145: setting the calibration factor, c(9), to ",&
        "1 for the",/"viscous term in liquid-solids interaction model")
            c(9) = one
      elseif (c(9) <= 0) then
         WRITE(err_msg, 1106) 'C(9)', ival(C(9))
         CALL LOG_ERROR()
      endif
      if (c(10) == undefined) then
         write(err_msg, 1146)
         call log_warning()
 1146 format("Warning 1146: setting the calibration factor, c(10), to ",&
        "1 for the",/"inertial term in liquid-solids interaction model")
            c(10) = one
      elseif (c(10) <= 0) then
         WRITE(err_msg, 1106) 'C(10)', ival(C(10))
         CALL LOG_ERROR()
      endif


! check settings for mechanical dispersion
!-------------------------------------------------//
      IF (MECH_DISPERSION ) THEN
! currently cannot invoke mechanical dispersion unless specific user
! drag model is also invoked
         IF (USR_DRAG_TYPE_ENUM == UNDEFINED_USR_DRAG_TYPE) THEN
            write(err_msg, 1150)
 1150 format('Error 1150: If MECH_DISPERSION = .TRUE. then ',&
         'USR_DRAG_TYPE',/,'must be defined')
            CALL LOG_ERROR()
         ELSEIF (USR_DRAG_TYPE_ENUM /= ATTOU_99 .OR. &
                 USR_DRAG_TYPE_ENUM /= ATTOU_99_MOD .OR. &
                 USR_DRAG_TYPE_ENUM /= SOLOMENKO_15 .OR. &
                 USR_DRAG_TYPE_ENUM /= LAPPALAINEN_09_MOD .OR. &
                 USR_DRAG_TYPE_ENUM /= LAPPALAINEN_09) THEN
           WRITE(ERR_MSG,1151)
 1151 format('Error 1151: Selected USR_DRAG_TYPE is not valid ',&
         'when',/'MECH_DISPERSION = .TRUE.')
            CALL LOG_ERROR()
         ENDIF
! check value of spread_factor
         IF (spread_factor == undefined) THEN
            IF (d_pack == undefined) THEN
                write(err_msg, 1152) index_sol, ival(d_p0(index_sol))
                call log_warning()
                d_pack = d_p0(index_sol)
            ELSE
               IF(d_pack <= 0) THEN
                  WRITE(err_msg, 1106) 'd_pack', ival(d_pack)
                  CALL LOG_ERROR()
               ENDIF
            ENDIF

            IF (UNITS == 'SI') THEN
               spread_factor = 0.015*sqrt(d_pack)
            ELSE
               spread_factor = 0.15*sqrt(d_pack)
            ENDIF
         ELSE
            IF (spread_factor < 0) THEN
               WRITE(err_msg, 1106) 'spread_factor', ival(spread_factor)
               CALL LOG_ERROR()
            ENDIF
         ENDIF
         IF (.NOT.CALL_USR_SOURCE(3) .OR. .NOT.CALL_USR_SOURCE(4) .OR. &
            (.NOT.CALL_USR_SOURCE(5) .AND. .NOT.NO_K)) THEN
            WRITE(ERR_MSG,1153)
            CALL LOG_ERROR()
         ENDIF
      ENDIF
 1152 format('Warning 1152: setting the nominal size of the packing ',&
            'to the ',/,'diameter of solids phase: d_pack = d_p0(',i2,&
            ') = ',A)
 1153 format("Error 1153: invoking a mechanical dispersion model ",&
        "requires",/"that CALL_USR_SOURCE be set to true for all,"&
        "momentum equations.")


! check surface tension
!-------------------------------------------------//
      IF (omega_l0 == undefined) THEN
         write(err_msg, 1160)
         call log_warning()
 1160 format('Warning 1160: The value of surface tension will be ',&
         'calculated ',/'based on the provided model.')
      ELSE
         IF (omega_l0 < 0) THEN
            WRITE(err_msg, 1106) 'omega_l0', ival(omega_l0)
            CALL LOG_ERROR()
         ENDIF
      ENDIF


! check wetted area fraction is set to available option
!-------------------------------------------------//
      IF (wetarea_type /= undefined_c) THEN
         SELECT CASE(trim(Adjustl(WETAREA_TYPE)))
         CASE ('ONDA_68')
            WETAREA_TYPE_ENUM = ONDA_68
            IF (sa_pack == undefined .OR. sa_pack <=0) THEN
               WRITE(err_msg, 1106) 'sa_pack', ival(sa_pack)
               CALL LOG_ERROR()
            ENDIF
         CASE ('LAPPALAINEN_08')
            WETAREA_TYPE_ENUM = LAPPALAINEN_08
         CASE ('BILLET_95')
            WETAREA_TYPE_ENUM = BILLET_95
! Must specify ch_pack if using BILLET_95 wetted area model
            IF (ch_pack == undefined .OR. ch_pack < 0) THEN
               WRITE(err_msg, 1106) 'ch_pack', ival(ch_pack)
               CALL LOG_ERROR()
            ENDIF
            IF (sa_pack == undefined .OR. sa_pack <=0) THEN
               WRITE(err_msg, 1106) 'sa_pack', ival(sa_pack)
               CALL LOG_ERROR()
            ENDIF

         CASE ('CONSTANT')
            WETAREA_TYPE_ENUM = CONSTANT_WAF
            if (wetareafrac0 == undefined) then
               write(err_msg, 1170)
               CALL LOG_ERROR()
 1170 format("Error 1170: Constant fraction wetted area is requested",&
        "This",/"requires defining wetareafrac0.")
            elseif (wetareafrac0 > 1 .or. wetareafrac0<0) then
               WRITE(err_msg, 1106) 'wetareafrac0', ival(wetareafrac0)
               CALL LOG_ERROR()
            endif
         CASE DEFAULT
            WRITE(ERR_MSG,1106) 'WETAREA_TYPE', &
                trim(adjustl(WETAREA_TYPE))
            CALL LOG_ERROR()
         END SELECT
      ELSE
         WETAREA_TYPE_ENUM = UNDEFINED_WETAREA_TYPE
         write(err_msg, 1171)
 1171 format('Warning 1171: Undefined wetarea_type. No fractional ',&
         'wetted area ',/, 'factor will be used in any calculation.')
         call log_warning()
         wetareafrac0 = ONE
         IF (apply_WAF) THEN
            write(err_msg,1172)
 1172 format('Error 1172: Undefined wetarea_type but requesting ',&
         'apply_waf.',/, 'Clarify intent with appropriate settings.')
            CALL LOG_ERROR()
         ENDIF
         APPLY_WAF = .FALSE.
      ENDIF

! if the user chooses to apply a fractional area then they must
! select a model
      IF (apply_waf) THEN
         IF(wetarea_type_enum == undefined_wetarea_type) THEN
            write(err_msg, 1173)
 1173 format("Error 1173: Fractional wetted area is requested for ",&
        "interaction",/"terms. This requires defining WETAREA_TYPE.")
            CALL LOG_ERROR()
         ENDIF
      ENDIF


! check capillary pressure model is set to one available
!-------------------------------------------------//
      IF (CAP_PRESS_TYPE /= UNDEFINED_C) THEN
         SELECT CASE(trim(Adjustl(CAP_PRESS_TYPE)))
         CASE ('GROSSER_1988')
            CAP_PRESS_TYPE_ENUM = GROSSER_1988
         CASE DEFAULT
            CAP_PRESS_TYPE_ENUM = UNDEFINED_CAP_PRESS_TYPE
         END SELECT

         IF (CAP_PRESS_TYPE_ENUM == UNDEFINED_CAP_PRESS_TYPE) THEN
            WRITE(ERR_MSG,1106) 'CAP_PRESS_TYPE', &
                trim(adjustl(CAP_PRESS_TYPE))
            CALL LOG_ERROR()
         ENDIF

         IF (.NOT.CALL_USR_SOURCE(3) .OR. .NOT.CALL_USR_SOURCE(4) .OR. &
            (.NOT.CALL_USR_SOURCE(5) .AND. .NOT.NO_K)) THEN
            WRITE(ERR_MSG,1191)
            CALL LOG_ERROR()
         ENDIF
      ELSE
         CAP_PRESS_TYPE_ENUM = UNDEFINED_CAP_PRESS_TYPE
      ENDIF
 1191 format("Error 1191: invoking a capillary pressure model ",&
        "requires",/"that CALL_USR_SOURCE be set to true for all,"&
        "momentum equations.")

! check/set effective liquid viscosity
!-------------------------------------------------//
      IF (lam_mu_s0 == undefined) THEN
         IF (mu_s0(index_liq) == undefined) THEN
            write(err_msg, 1111)
            call log_warning()
 1111 format('Warning 1111: The viscosity of the liquid used in the ',&
         'liquid',/,'solids drag correlation is set to the calculated ',&
         'viscosity',/,'of the liquid phase: lambda_mu_s0 = mu_s = ',&
         'calculated.')
         ELSE
            write(err_msg, 1112) index_liq, trim(ival(mu_s0(index_liq)))
            call log_warning()
 1112 format('Warning 1112: The viscosity of the liquid used in the ',&
         'liquid',/,'solids drag correlation is set to the viscosity ',&
         'of the liquid',/,'phase: lam_mu_s0 = mu_s0(',i2,') = ',A)
         ENDIF
      ELSE
         IF (lam_mu_s0 < 0) THEN
            WRITE(err_msg, 1106) 'lam_mu_s0', ival(lam_mu_s0)
            CALL LOG_ERROR()
         ENDIF
         IF (MU_s0(index_liq) /= undefined) THEN
            write(err_msg, 1113) trim(ival(lam_mu_s0)), index_liq, &
               trim(ival(mu_s0(index_liq)))
            call log_warning()
 1113 format('Warning 1113: The viscosity of the liquid phase used ',&
         'in the drag ',/,'correlation (lam_mu_s0= ',A,') is set ',&
         'independent of the ',/,'viscosity used in the stress ',&
         'calculation (mu_s0(',i2,')= ', A,').')
         ELSE
            write(err_msg, 1114) trim(ival(lam_mu_s0))
            call log_warning()
 1114 format('Warning 1114: The viscosity of the liquid phase used ',&
         'in the drag ',/,'correlation (lam_mu_s0= ',A,') is set ',&
         'independent of the ',/,'viscosity used in the stress ',&
         'calculation (mu_s= calculated).')
         ENDIF
      ENDIF


! check/set effective gas viscosity
!-------------------------------------------------//
      IF (lam_mu_g0 == UNDEFINED) THEN
         IF (MU_G0 == UNDEFINED) THEN
            write(err_msg, 1115)
            call log_warning()
 1115 format('Warning 1115: The viscosity of the gas used in the gas-',&
         'solids',/,'drag correlation is set to the calculated visco',&
         'sity of the',/, 'gas phase: lam_mu_g0 = mu_g = calculated.')
         ELSE
            write(err_msg, 1116) trim(ival(mu_g0))
            call log_warning()
 1116 format('Warning 1116: The viscosity of the gas used in the gas-',&
         'solids',/,'drag correlation is set to the viscosity of the ',&
         'gas phase: ',/,'lam_mu_g0 = mu_g0 = ',A)
            lam_Mu_g0 = mu_g0
         ENDIF
      ELSE
         IF (lam_mu_g0 < 0) THEN
            WRITE(err_msg, 1106) 'lam_mu_g0', ival(lam_mu_g0)
            CALL LOG_ERROR()
         ENDIF
         IF (mu_g0 /= undefined) THEN
            write(err_msg, 1117) trim(ival(lam_mu_g0)), &
               trim(ival(mu_g0))
            call log_warning()
 1117 format('Warning 1117: The viscosity of the gas phase used ',&
         'in the drag ',/,'correlation (lam_mu_g0= ',A,') is set ',&
         'independent of the ',/,'viscosity used in the stress ',&
         'calculation (mu_g0= ', A,').')
         ELSE
            write(err_msg, 1118) trim(ival(lam_mu_g0))
            call log_warning()
 1118 format('Warning 1118: The viscosity of the gas phase used ',&
         'in the drag ',/,'correlation (lam_mu_g0= ',A,') is set ',&
         'independent of the ',/,'viscosity used in the stress ',&
         'calculation (mu_g= calculated).')
         ENDIF
      ENDIF



! check reaction model specific quantities
!-------------------------------------------------//
      IF (NO_OF_RXNS > 0) THEN

! check absorption_chem_type is set to available option
!-------------------------------------------------//
         IF (ABSORPTION_CHEM_TYPE /= UNDEFINED_C) THEN
            SELECT CASE(trim(Adjustl(ABSORPTION_CHEM_TYPE)))
            CASE ('SINGLE_STEP')
               ABSORPTION_CHEM_TYPE_ENUM = SINGLE_STEP
               IF (ENHANCEMENT_FACTOR /= UNDEFINED) THEN
                  IF (ENHANCEMENT_FACTOR /= ONE) THEN
                    write(err_msg, 1188) 'ENHANCEMENT_FACTOR',&
                       ival(ENHANCEMENT_FACTOR)
 1188 format("Error 1188: ",A,"=",A,". For consistency with",/&
             "ABSORPTION_CHEM_TYPE=""SINGLE_STEP"", ENHANCEMENT_",&
             "FACTOR should be ",/,"set to 1. Please correct the ",&
             "input file.")
                     CALL LOG_ERROR()
                  ENDIF
               ELSE
                  write(err_msg, 1189)
 1189 format("Warning 1189: For consistency with ABSORPTION_CHEM_",&
             "TYPE=",/,"""SINGLE_STEP"", ENHANCEMENT_FACTOR is set ",&
             "1.")
                  call log_warning()
                  ENHANCEMENT_FACTOR = ONE
               ENDIF
            CASE ('EQUILIBRIUM_SEGREGATED')
               ABSORPTION_CHEM_TYPE_ENUM = EQUILIBRIUM_SEGREGATED
            CASE ('EQUILIBRIUM_COUPLED')
               ABSORPTION_CHEM_TYPE_ENUM = EQUILIBRIUM_COUPLED
            CASE DEFAULT
               ABSORPTION_CHEM_TYPE_ENUM=UNDEFINED_ABSORPTION_CHEM_TYPE
            END SELECT
         ELSE
            ABSORPTION_CHEM_TYPE_ENUM=UNDEFINED_ABSORPTION_CHEM_TYPE
         ENDIF
         IF (ABSORPTION_CHEM_TYPE_ENUM == &
            UNDEFINED_ABSORPTION_CHEM_TYPE) THEN
            WRITE(ERR_MSG,1106) 'ABSORPTION_CHEM_TYPE', &
                trim(adjustl(ABSORPTION_CHEM_TYPE))
            CALL LOG_ERROR()
         ENDIF

! check mass_transfer_type is set to available option or explicitly
! set it to onda (default)
!-------------------------------------------------//
         IF (MASS_TRANSFER_TYPE /= UNDEFINED_C) THEN
            SELECT CASE(trim(Adjustl(MASS_TRANSFER_TYPE)))
            CASE ('ONDA_1968_MTC')
               MASS_TRANSFER_TYPE_ENUM = ONDA_1968_MTC
            CASE ('PSFIRST_LMTC')
               MASS_TRANSFER_TYPE_ENUM = PSFIRST_LMTC
! override any user supplied settings for the following keywords
! the enhancement factor is set to one to avoid calculations
               ENHANCEMENT_FACTOR = ONE
! gas side mass transfer resistance is assumed negligible
               C(2) = large_number
            CASE ('CONSTANT_MTC')
               MASS_TRANSFER_TYPE_ENUM = CONSTANT_MTC
               if (c(2) == undefined) then
                   write(err_msg, 1190)
                   CALL LOG_ERROR()
 1190 format('ERROR 1190: The factor c(2), the gas phase mass ',&
         'transfer coefficient,'/,'must be defined when specifying',&
         'MASS_TRANSFER_TYPE=""CONSTANT_MTC"".'/,&
         'Please correct the input file.')
               elseif (c(2) <= 0) then
                  WRITE(err_msg, 1106) 'C(2)', ival(C(2))
                  CALL LOG_ERROR()
               endif
               if (c(3) == undefined) then
                   write(err_msg, 1192)
                   call log_warning()
 1192 format('ERROR 1192: The factor c(3), the liquid phase mass ',&
         'transfer coefficient,'/,'must be defined when specifying',&
         'MASS_TRANSFER_TYPE=""CONSTANT_MTC"".'/,&
         'Please correct the input file.')
               elseif (c(3) <= 0) then
                  WRITE(err_msg, 1106) 'C(3)', ival(C(3))
                  CALL LOG_ERROR()
               endif

           CASE DEFAULT
               MASS_TRANSFER_TYPE_ENUM=UNDEFINED_MASS_TRANSFER_TYPE
           END SELECT
         ELSE
            MASS_TRANSFER_TYPE_ENUM=ONDA_1968_MTC
         ENDIF
         IF (MASS_TRANSFER_TYPE_ENUM == &
            UNDEFINED_MASS_TRANSFER_TYPE) THEN
            WRITE(ERR_MSG,1106) 'MASS_TRANSFER_TYPE', &
                trim(adjustl(MASS_TRANSFER_TYPE))
            CALL LOG_ERROR()
         ENDIF

         IF (ENHANCEMENT_FACTOR < ZERO) THEN
            WRITE(ERR_MSG,1106) 'ENHANCEMENT_FACTOR', &
               iVal(ENHANCEMENT_FACTOR)
            CALL LOG_ERROR()
         ENDIF

! set defaults for equilbrium chemistry if using the
! EQUILIBRIUM_SEGREGATED scheme
         IF(ABSORPTION_CHEM_TYPE_ENUM == EQUILIBRIUM_SEGREGATED) THEN
            if (c(11) == undefined) then
               write(err_msg, 1183)
               call log_warning()
 1183 format("Warning 1183: setting the forward reaction rate constant",&
           " C(11) for",/"the carabmate (RNHCOO-) reversion equilbrium ",&
           "reaction to 1.")
               c(11) = one
            elseif (c(11) <= 0) then
               WRITE(err_msg, 1106) 'C(11)', ival(C(11))
               CALL LOG_ERROR()
            endif

            if (c(12) == undefined) then
               write(err_msg, 1184)
               call log_warning()
 1184 format("Warning 1184: setting the forward reaction rate constant",&
           " C(12) for",/"the dissociation of dissolved CO2 equilbrium",&
           " reaction to 1.")
               c(12) = one
            elseif (c(12) <= 0) then
               WRITE(err_msg, 1106) 'C(12)', ival(C(12))
               CALL LOG_ERROR()
            endif

            if (c(13) == undefined) then
               write(err_msg, 1185)
               call log_warning()
 1185 format("Warning 1185: setting the forward reaction rate constant",&
           " C(13) for",/"the dissociation of bicarbonate (HCO3-)",&
           " equilbrium reaction to 1.")
               c(13) = one
            elseif (c(13) <= 0) then
               WRITE(err_msg, 1106) 'C(13)', ival(C(13))
               CALL LOG_ERROR()
            endif

            if (c(14) == undefined) then
               write(err_msg, 1186)
               call log_warning()
 1186 format("Warning 1186: setting the forward reaction rate constant",&
           " C(14) for",/"the dissociation of protonated MEA (RNH2)",&
           " equilbrium reaction to 1.")
               c(14) = one
            elseif (c(14) <= 0) then
               WRITE(err_msg, 1106) 'C(14)', ival(C(14))
               CALL LOG_ERROR()
            endif

            if (c(15) == undefined) then
               write(err_msg, 1187)
               call log_warning()
 1187 format("Warning 1187: setting the forward reaction rate constant",&
        " C(15) for",/"the ionization of water equilibrium reaction",&
        " to 1.")
               c(15) = one
            elseif (c(15) <= 0) then
               WRITE(err_msg, 1106) 'C(15)', ival(C(15))
               CALL LOG_ERROR()
            endif
         ENDIF   ! endif EQUILIBRIUM_SEGREGATED scheme


! check user settings for additional calibration parameters
!-------------------------------------------------
         if (c(1) == undefined) then
            write(err_msg, 1180)
            call log_warning()
 1180 format("Warning 1180: setting the calibration factor, c(1), to ",&
           "1 for Henry's",/"constant for CO2 in aequous MEA.")
               c(1) = one
            elseif (c(1) <= 0) then
               WRITE(err_msg, 1106) 'C(1)', ival(C(1))
               CALL LOG_ERROR()
         endif

         if (c(2) == undefined) then
             write(err_msg, 1181)
             call log_warning()
 1181 format('Warning 1181: setting the calibration factor, c(2), to ',&
         '1 for the ',/'gas phase mass transfer coefficient.')
             c(2) = one
         elseif (c(2) <= 0) then
            WRITE(err_msg, 1106) 'C(2)', ival(C(2))
            CALL LOG_ERROR()
         endif

         if (c(3) == undefined) then
             write(err_msg, 1182)
             call log_warning()
 1182 format('Warning 1182: setting the calibration factor, c(3), to ',&
         '1 for the ',/'liquid phase mass transfer coefficient.')
             c(3) = one
         elseif (c(3) <= 0) then
            WRITE(err_msg, 1106) 'C(3)', ival(C(3))
            CALL LOG_ERROR()
         endif

! must select a wetted area model
! prior checks have already been conducted on wetted area selection
! as needed for pure hydrodynamics; here are a few additional checks
! needed as chemistry is included
         IF(wetarea_type_enum == undefined_wetarea_type) THEN
            write(err_msg, 1108) 'WETAREA_TYPE'
            CALL LOG_ERROR()
         ENDIF

         IF (sa_pack == undefined) THEN
            WRITE(err_msg, 1108) 'sa_pack'
            CALL LOG_ERROR()
         ELSE
            IF(sa_pack <= 0) THEN
               WRITE(err_msg, 1106) 'sa_pack', ival(sa_pack)
               CALL LOG_ERROR()
            ENDIF
         ENDIF

         IF (d_pack == undefined) THEN
             write(err_msg, 1152) index_sol, ival(d_p0(index_sol))
             call log_warning()
             d_pack = d_p0(index_sol)
         ELSE
            IF(d_pack <= 0) THEN
               WRITE(err_msg, 1106) 'd_pack', ival(d_pack)
               CALL LOG_ERROR()
            ENDIF
         ENDIF

         IF (omega_pack == undefined) THEN
            WRITE(err_msg, 1108) 'omega_pack'
            CALL LOG_ERROR()
         ELSE
            IF(omega_pack < 0) THEN
               WRITE(err_msg, 1106) 'omega_pack', ival(omega_pack)
               CALL LOG_ERROR()
            ENDIF
         ENDIF

      ENDIF   ! end if no_of_rxns >0

 1106 FORMAT('Error 1106: Illegal or unknown input: ',A,' = ',A,/ &
         'Please correct the input file')

 1108 FORMAT('Error 1108: Required input not specified: ', A,/,&
         'Please correct the input file.')

      RETURN
      END SUBROUTINE CHECK_USR_INPUT


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: allocate_usr_arrays                                     C
!  Purpose: Allocate user arrays                                       C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE ALLOCATE_USR_ARRAYS

! Global variables/parameters
!---------------------------------------------------------------------
      use usr, only: Omega_L, fWetArea_Pack
      use param, only: dimension_3
      use error_manager
      IMPLICIT NONE
!---------------------------------------------------------------------

! Surface tension
      Allocate( Omega_L(DIMENSION_3) )
! Wetted area
      Allocate( fWetArea_Pack(DIMENSION_3) )

      RETURN
      END SUBROUTINE ALLOCATE_USR_ARRAYS


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: set_init_usr_propertie                                  C
!  Purpose: Set/Initialize certain user properties                     C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SET_INIT_USR_PROPERTIES

! Global variables/parameters
!---------------------------------------------------------------------
      use compar, only: ijkstart3, ijkend3
      use functions, only: fluid_at
      use param1, only: zero, undefined
      use usr, only: Omega_L, omega_l0
      use usr, only: fWetArea_Pack
      use error_manager

      use run, only: run_type, time
      use output, only: usr_dt
      use usr, only: usr_eqtime
      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------
! index
      INTEGER :: IJK
!---------------------------------------------------------------------

      omega_l = zero
      fWetArea_Pack = ZERO

      DO IJK = IJKSTART3,IJKEND3
         IF (FLUID_AT(IJK)) THEN
            IF (omega_l0 /= UNDEFINED) omega_l(IJK) = omega_l0
         ENDIF
      ENDDO

! Initialize USR_EQTIME
      USR_EQTIME = UNDEFINED
      IF (USR_DT(1) /= UNDEFINED) THEN
         IF (RUN_TYPE == 'NEW') THEN
            USR_EQTIME = TIME
         ELSE
            USR_EQTIME = USR_DT(1) * &
               (INT((TIME+0.1d0*USR_DT(1))/USR_DT(1))+1)
         ENDIF
      ENDIF

      RETURN
      END SUBROUTINE SET_INIT_USR_PROPERTIES


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: SETUP_DATAEXTRACT                                       C
!  Purpose: setup information needed for later processing of user      C
!   data                                                               C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SETUP_DATAEXTRACT

      use calc_cell_mod, only: calc_cell
      use geometry, only: do_k, kmin1
      use geometry, only: dx, dy, dz
      use geometry, only: imax, jmax, kmax
      use geometry, only: xmin
      use output, only: usr_dt
      use output, only: usr_x_w, usr_x_e, usr_i_w, usr_i_e
      use output, only: usr_y_s, usr_y_n, usr_j_s, usr_j_n
      use output, only: usr_z_b, usr_z_t, usr_k_b, usr_k_t
      use param, only: dimension_usr
      use param1, only: zero, one, undefined

      IMPLICIT NONE

! Local variables
!--------------------------------------------------------------------
! index of user defined boundary to use for extraction
      INTEGER :: L
!--------------------------------------------------------------------

! most of the current data processing routines need to check usr
! data first, which is done after this routine is called

      DO L = 1, DIMENSION_USR
         IF(USR_DT(L) == UNDEFINED) CYCLE
         IF (USR_X_W(L) == UNDEFINED) CYCLE
! Set the I/J/K values used to extract data from the simulation.
         CALL CALC_CELL (XMIN, USR_X_w(L), DX, IMAX, USR_I_w(L))
         CALL CALC_CELL (XMIN, USR_X_e(L), DX, IMAX, USR_I_e(L))
         CALL CALC_CELL (ZERO, USR_Y_s(L), DY, JMAX, USR_J_s(L))
         CALL CALC_CELL (ZERO, USR_Y_n(L), DY, JMAX, USR_J_n(L))
         IF(do_K) THEN
            CALL CALC_CELL (ZERO, USR_Z_b(L), DZ, KMAX, USR_K_b(L))
            CALL CALC_CELL (ZERO, USR_Z_t(L), DZ, KMAX, USR_K_t(L))
         ELSE
            USR_K_b(L) = KMIN1
            USR_K_t(L) = KMIN1
         ENDIF
      ENDDO

      RETURN
      END SUBROUTINE SETUP_DATAEXTRACT


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine:  write_usr0p1                                           C
!  Purpose: create a new/separate write_usr0 that will be called       C
!  later in the code then write_usr0 so that certain initialization    C
!  information is available to be written. write_usr0 cannot be        C
!  used for this as much of the file information isn't yet available   C
!  until after write_usr0 is called.                                   C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE WRITE_USR0p1

! Global variables
!--------------------------------------------------------------------
      use param, only: DIMENSION_USR
      use param1, only: undefined
      use output, only: usr_dt
      use output, only: usr_x_w, usr_x_e, usr_i_w, usr_i_e
      use output, only: usr_y_s, usr_y_n, usr_j_s, usr_j_n
      use output, only: usr_z_b, usr_z_t, usr_k_b, usr_k_t
      use geometry, only: do_k
      IMPLICIT NONE

! Local variables
!--------------------------------------------------------------------
      INTEGER :: L
!--------------------------------------------------------------------

      DO L = 1, DIMENSION_USR
         IF(USR_DT(L) == UNDEFINED) CYCLE
         IF (USR_X_W(L) == UNDEFINED) CYCLE

         SELECT CASE(L)
         CASE(1)
! reserved for equilibrium solver timestep
         CASE(2)
! create a custom file here:
!            CALL WRITE_CUSTOMDATA_HEADER('CustOutput',L)
!            CALL WRITE_CustomData1_HEADER('VolAvg',L)
         CASE(3)
         CASE(4)
         CASE DEFAULT
         END SELECT
      ENDDO

      RETURN
      CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Write the header info for examining some custom data       C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE write_customdata_header(FileN,L)

      use compar, only: myPE, PE_IO
      use error_manager
! error modules procedures
      use run, only: run_type

      IMPLICIT NONE

! Dummy arguments
!--------------------------------------------------------------------
! file name
      CHARACTER (len=*), INTENT(IN) :: FileN
! index of user defined boundary
      INTEGER, INTENT(IN) :: L

! Local variables
! -------------------------------------------------------------------
! file tag name
      CHARACTER(len=3) :: FTAG
      LOGICAL :: lExists
      INTEGER, PARAMETER :: lUnit=484
! local file name
      CHARACTER(LEN=30) :: lFileN
! -------------------------------------------------------------------

      IF (RUN_TYPE/='NEW' .AND. RUN_TYPE/='RESTART_2') RETURN
      IF(myPE /= PE_IO) RETURN

! set the file name
      write(fTag,"('_',I2.2)") L
      write(lFileN,"(A,'.log')") FileN//FTag

      inquire(file=trim(lFileN),exist=lExists)

      IF(lExists) then
         IF (RUN_TYPE=='NEW' .OR. RUN_TYPE=='RESTART_2') THEN
            write(err_msg, 1206) trim(lFileN)
 1206 FORMAT('Error 1206: File ',A,', already exists in run ', &
      'directory.',/'Please remove before proceeding.')
            CALL LOG_ERROR()
         ENDIF
         open(lUnit,file=trim(adjustl(lFileN)), &
              status='old', position='append')
         close(lUnit)
      ELSE
         open(lUnit,file=trim(adjustl(lFileN)), &
              status='new')
         write(lUnit,"(A,2X,A)") '#','Volume Averaged Data'

         write(lUnit,"(A,2X,A,I2)") &
            '#','Computed over user defined location at index = ', L
         write(lUnit,"(A,5X,A,1X,F7.3,1X,I4.2,5X,A,1X,F7.3,1X,I4.2)") &
            '#','xw & iw:', USR_X_w(L), USR_I_w(L),&
            'xe & ie:', USR_X_e(L), USR_I_e(L)
         write(lUnit,"(A,5X,A,1X,F7.3,1X,I4.2,5X,A,1X,F7.3,1X,I4.2)") &
            '#','ys & js:', USR_Y_s(L), USR_J_s(L),&
            'yn & jn:', USR_Y_n(L), USR_J_n(L)
         IF (DO_K) THEN
            write(lUnit,"(A,5X,A,1X,F7.3,1X,I4.2,5X,A,1X,F7.3,1X,I4.2)") &
               '#','zb & kb:', USR_Z_b(L), USR_K_b(L),&
               'zt & kt:', USR_Z_t(L), USR_K_t(L)
         ELSE
            write(lUnit,"(A,5X,A,1X,I4.2,1X,I4.2)") &
               '#','kb & kt:', USR_K_b(L), USR_K_t(L)
         ENDIF

! create a header; print out data in write_usr1.f
!         write(lUnit,'(/,5X,A,3(8X,A))') '# TIME'&

         close(lUnit)
      ENDIF

      RETURN
      END SUBROUTINE WRITE_customdata_HEADER


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Write the header info for examining vol averages           C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE WRITE_CustomData1_HEADER(FileN,L)

      use compar, only: myPE, PE_IO
      use error_manager
      use run, only: run_type

      IMPLICIT NONE

! Dummy arguments
!--------------------------------------------------------------------
! file name
      CHARACTER (len=*), INTENT(IN) :: FileN
! index of user defined boundary
      INTEGER, INTENT(IN) :: L

! Local variables
! -------------------------------------------------------------------
! file tag name
      CHARACTER(len=3) :: FTAG
      LOGICAL :: lExists
      INTEGER, PARAMETER :: lUnit=484
! local file name
      CHARACTER(LEN=30) :: lFileN
! -------------------------------------------------------------------

      IF (RUN_TYPE/='NEW' .AND. RUN_TYPE/='RESTART_2') RETURN
      IF(myPE /= PE_IO) RETURN

! set the file name
      write(fTag,"('_',I2.2)") L
      write(lFileN,"(A,'.log')") FileN//FTag

      inquire(file=trim(lFileN),exist=lExists)

      IF(lExists) then
         IF (RUN_TYPE=='NEW' .OR. RUN_TYPE=='RESTART_2') THEN
            write(err_msg, 1206) trim(lFileN)
 1206 FORMAT('Error 1206: File ',A,', already exists in run ', &
      'directory.',/'Please remove before proceeding.')
            CALL LOG_ERROR()
         ENDIF
         open(lUnit,file=trim(adjustl(lFileN)), &
              status='old', position='append')
         close(lUnit)
      ELSE
         open(lUnit,file=trim(adjustl(lFileN)), &
              status='new')
         write(lUnit,"(A,2X,A)") '#','Volume Averaged Data'

         write(lUnit,"(A,2X,A,I2)") &
            '#','Computed over user defined location at index = ', L
         write(lUnit,"(A,5X,A,1X,F7.3,1X,I4.2,5X,A,1X,F7.3,1X,I4.2)") &
            '#','xw & iw:', USR_X_w(L), USR_I_w(L),&
            'xe & ie:', USR_X_e(L), USR_I_e(L)
         write(lUnit,"(A,5X,A,1X,F7.3,1X,I4.2,5X,A,1X,F7.3,1X,I4.2)") &
            '#','ys & js:', USR_Y_s(L), USR_J_s(L),&
            'yn & jn:', USR_Y_n(L), USR_J_n(L)
         IF (DO_K) THEN
            write(lUnit,"(A,5X,A,1X,F7.3,1X,I4.2,5X,A,1X,F7.3,1X,I4.2)") &
               '#','zb & kb:', USR_Z_b(L), USR_K_b(L),&
               'zt & kt:', USR_Z_t(L), USR_K_t(L)
         ELSE
            write(lUnit,"(A,5X,A,1X,I4.2,1X,I4.2)") &
               '#','kb & kt:', USR_K_b(L), USR_K_t(L)
         ENDIF

! create a header; print out data in write_usr1.f
! pressure gradient, ffactor, l/g
         write(lUnit,'(/,A,3X,A,3X,3(A,6X))') '#', 'TIME',&
            'GradPG-Y', 'F-Factor', 'L/G     '
         write(lUnit,'(A,9X,A,2(2X,A))') '#',&
            ' [pa/m]     ',&
            ' [sqrt(pa)] ', &
            ' [L/m^3]    '

         close(lUnit)

      ENDIF
      RETURN
      END SUBROUTINE WRITE_CustomData1_HEADER

      END SUBROUTINE WRITE_USR0p1
