#include "error.inc"

MODULE CALC_COEFF_MOD

   USE COEFF

CONTAINS

!**********************************************************************!
!  SUBROUTINE: INIT_COEFF                                              !
!                                                                      !
!  Purpose: Initialize logical flags.                                  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE INIT_COEFF(MFIX_DAT, IER)

! Global Variables:
!-----------------------------------------------------------------------
! Run-time flag for invoking discrete element model
      use discretelement, only: DISCRETE_ELEMENT
! Run-time flag for gas/DEM coupling
      use discretelement, only: DES_CONTINUUM_COUPLED
! Run-time flag for invoking TFM/DEM hybrid model
      use discretelement, only: DES_CONTINUUM_HYBRID
! MMS flag
      use mms, only: USE_MMS
      use param, only: DIMENSION_M
      use param1, only: UNDEFINED
! Real number of solids phases (GHD theory)
      use physprop, only: SMAX
! Specified constant gas phase density (incompressible)
      use physprop, only: RO_G0
! Specified constant specific heat.
      use physprop, only: C_PG0, C_PS0
! Specified constant thermal conductivity.
      use physprop, only: K_G0, K_S0
! specified constant diffusivity
      use physprop, only: DIF_G0, DIF_S0
! Specified number of solids phases.
      use physprop, only: MMAX
! Run-time flag invoking QMOM theory
      use qmom_kinetic_equation, only: QMOMK
! Variable solids density flag.
      use run, only: SOLVE_ROs
! Run-time flag for to solve energy equations.
      use run, only: ENERGY_EQ
! Run-time flag for to solve species equations.
      use run, only: SPECIES_EQ
! Solids conductivity model.
      use run, only: ks_model_enum
      use run, only: ks_bauer, ks_usr
! Kinetic theory model.
      USE run, only: kt_type_enum
      USE run, only: gd_1999, gtsh_2012, ia_2005, ghd_2007
! Run-time flag for invoking DQMOM
      use run, only: CALL_DQMOM
! user defined flags
      use usr_prop, only: usr_rog, usr_cpg, usr_kg, usr_difg
      use usr_prop, only: usr_ros, usr_cps, usr_ks, usr_difs
      implicit none

! Dummy Arguments:
!-----------------------------------------------------------------------
! Error flag.
      INTEGER, intent(inout) :: IER

! Path to input file
      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

! Local Variables.
!-----------------------------------------------------------------------
! Invoke debug routine:
      LOGICAL, parameter :: dbg_coeffs = .FALSE.
! Loop counter for solids phases
      INTEGER :: M

! Allocate and initialize:
!```````````````````````````````````````````````````````````````````````
      IF(.NOT.allocated(DENSITY)) allocate( DENSITY(0:DIMENSION_M))
      IF(.NOT.allocated(SP_HEAT)) allocate( SP_HEAT(0:DIMENSION_M))
      IF(.NOT.allocated(PSIZE)) allocate( PSIZE(0:DIMENSION_M))
! Interphase heat transfer coefficient (GAMA)

      DENSITY = .FALSE.
      SP_HEAT = .FALSE.
      PSIZE   = .FALSE.

      IF(.NOT.allocated(VISC)) allocate( VISC(0:DIMENSION_M))
      IF(.NOT.allocated(COND)) allocate( COND(0:DIMENSION_M))
      IF(.NOT.allocated(DIFF)) allocate( DIFF(0:DIMENSION_M))
      IF(.NOT.allocated(GRAN_DISS)) allocate( GRAN_DISS(0:DIMENSION_M))

      VISC = .FALSE.
      COND = .FALSE.
      DIFF = .FALSE.
      GRAN_DISS = .FALSE.

      IF(.NOT.allocated(DRAGCOEF)) &
         allocate( DRAGCOEF(0:DIMENSION_M,0:DIMENSION_M))
      IF(.NOT.allocated(HEAT_TR)) &
         allocate( HEAT_TR(0:DIMENSION_M,0:DIMENSION_M))

      DRAGCOEF = .FALSE.
      HEAT_TR = .FALSE.

! Coefficients for gas phase parameters.
!```````````````````````````````````````````````````````````````````````
! Compressible flow.
      if(RO_G0 == UNDEFINED .OR. USR_ROg) DENSITY(0) = .TRUE.

! Gas viscosity:
! Calc_mu_g must be invoked every iteration even if constant viscosity
! (mu_g0 /= undefined) to incorporate ishii form of governing equations
! wherein the viscosity is multiplied by the phase volume fraction.
! Alternatively, we could invoke calc_mu_g only if energy, k_epsilon,
! mixing_length, or ishii (use recalc_visc_g)
      VISC(0) = .TRUE.

! Specific heat and thermal conductivity.
      if(ENERGY_EQ) then
         if(C_PG0 == UNDEFINED .or. usr_cpg) SP_HEAT(0) = .TRUE.
         if(K_G0  == UNDEFINED .or. usr_kg) COND(0) = .TRUE.
      endif

! Species diffusivity.
      if(SPECIES_EQ(0)) then
        if (dif_g0 == undefined .or. usr_difg) DIFF(0) = .TRUE.
      endif

! Interphase transfer terms.
!```````````````````````````````````````````````````````````````````````
! this needs to be mmax for ghd
       if(.NOT.QMOMK .AND. .NOT.USE_MMS) DRAGCOEF(0:MMAX,0:MMAX)=.TRUE.

! Interphase heat transfer coefficient (GAMA)
      IF (.NOT.DISCRETE_ELEMENT .OR. DES_CONTINUUM_HYBRID) THEN
         if(ENERGY_EQ .AND. .NOT.USE_MMS) HEAT_TR(0:SMAX,0:SMAX)=.TRUE.
      ENDIF

! Coefficients for solids phase parameters.
!```````````````````````````````````````````````````````````````````````
      IF (.NOT.DISCRETE_ELEMENT .OR. DES_CONTINUUM_HYBRID) THEN
         DO M=1,SMAX
! Variable solids density or user solids density
            if(SOLVE_ROs(M) .or. USR_ROs(M)) DENSITY(M) = .TRUE.
         ENDDO

! Solids viscosity.
! Calc_mu_s must be invoked every iteration even if constant viscosity
! (mu_s0 /= undefined) to incorporate ishii form of governing equations
! wherein the viscosity is multiplied by the phase volume fraction
         VISC(1:SMAX) = .TRUE.
! mu_s only needs to be called for ghd_2007 when m=mmax
         IF (KT_TYPE_ENUM == GHD_2007) THEN
            VISC(1:SMAX) = .FALSE.
            VISC(MMAX) = .TRUE.
         ENDIF

         do M=1,SMAX
! Specific heat and thermal conductivity.
            if(ENERGY_EQ) THEN
               if(C_PS0(M) == UNDEFINED .or. usr_cps(M)) SP_HEAT(M) = .TRUE.
               if(KS_MODEL_ENUM(M) == ks_bauer .or. &
                  KS_MODEL_ENUM(M) == ks_usr) COND(M) = .TRUE.
            endif
! Species diffusivity. Generally no need to invoke this routine since
! by default solids diffusivisty is zero, however, now it is invoked
! for user options
            IF(SPECIES_EQ(M)) THEN
               IF (DIF_S0(M) == UNDEFINED .or. usr_difs(M)) DIFF(M) = .TRUE.
            ENDIF
         enddo

! Particle-Particle Energy Dissipation
         IF (KT_TYPE_ENUM == IA_2005 .OR. &
             KT_TYPE_ENUM == GD_1999 .OR. &
             KT_TYPE_ENUM == GTSH_2012) THEN
            GRAN_DISS(:SMAX) = .TRUE.
         ENDIF

! Particle diameter.
         if(Call_DQMOM) PSIZE(1:SMAX)=.TRUE.

      ENDIF   ! end if (.not.discrete_element .or des_continuum_hybrid)

      if(dbg_coeffs) CALL DEBUG_COEFF

! Invoke calc_coeff.
      IF(.NOT.DISCRETE_ELEMENT .OR. DES_CONTINUUM_COUPLED) THEN
         CALL CALC_COEFF(MFIX_DAT, IER, 2)

! If gas viscosity is undefined and the flag for calculating gas
! viscosity is turned off: Turn it on and make the call to calc_coeff.
! Once viscosity values have been calculated (i.e., an initial value
! is calculated), turn the flag off again so it isn't recalculated.
!         IF(MU_g0 == UNDEFINED .AND. .NOT.VISC(0)) THEN
!            VISC(0) = .TRUE.; CALL CALC_COEFF(MFIX_DAT, IER, 2)
!            VISC(0) = .FALSE.
!         ELSE
!            CALL CALC_COEFF(MFIX_DAT, IER, 2)
!         ENDIF
      ENDIF

      END SUBROUTINE INIT_COEFF


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      !
!  Subroutine: CALC_COEFF_ALL                                          !
!  Purpose: This routine directs the calculation of all physical and   !
!           transport properties, exchange rates, and reaction rates.  !
!                                                                      !
!  Author: M. Syamlal                                 Date: 25-AUG-05  !
!  Reviewer:                                          Date:            !
!                                                                      !
!  Literature/Document References:                                     !
!                                                                      !
!  Variables referenced:                                               !
!  Variables modified:                                                 !
!  Local variables:                                                    !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE CALC_COEFF_ALL(MFIX_DAT, FLAG, IER)

! Global variables:
!-----------------------------------------------------------------------
! Double precision: 1.0d0
      use param1, only: ONE
! Underrelaxation factor for gas-solids drag coefficient
      use ur_facs, only: UR_F_gs
! Underrelaxation factor solid conductivity coefficient for IA theory
      use ur_facs, only: UR_kth_sml
! Flag for DES coupled simulation
      use discretelement, only: DES_CONTINUUM_COUPLED
! Flag for explicit coupling between the fluid and particles.
      use discretelement, only: DES_EXPLICITLY_COUPLED

      use calc_des_2fluid_mod, only: calc_des_2fluid, des_2fluid_rxns

      implicit none

! Path to input file
      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

! Dummy arguments
!-----------------------------------------------------------------------
! FLAG = 0, overwrite the coeff arrays, (e.g. start of a time step)
! FLAG = 1, do not overwrite
      INTEGER, intent(in) :: FLAG
! Error index
      INTEGER, intent(inout) :: IER

! Local variables
!-----------------------------------------------
! Underrelaxation factor for gas-solids drag coefficient
      DOUBLE PRECISION :: loc_UR_F_gs ! Local copy
! Underrelaxation factor solid conductivity coefficient for IA theory
      DOUBLE PRECISION :: loc_UR_kth_sml ! Local copy

!-----------------------------------------------------------------------

! 1) Backup user-defined coefficient relaxation factors.
! 2) Set user-defined coefficient relaxation factors to 1.
! Note that 'FLAG' is hard coded to 0 in time march and reset_new.
      IF(FLAG == 0) THEN
        loc_UR_F_gs = UR_F_gs;          UR_F_gs = ONE
        loc_UR_Kth_sml = UR_Kth_sml;    UR_Kth_sml = ONE
      ENDIF

! Calculate all physical properties, transport properties, and exchange
! rates.
      CALL CALC_COEFF(MFIX_DAT, IER, 2)

! DES interaction for explicitly coupled simulations
! call moved before calc_rrate as the call to rxns_gs_gas1 from
! calc_des_2fluid needs to come before the call to des_2fluid_rxns
! which comes from calc_rrate
      IF(DES_CONTINUUM_COUPLED .AND. DES_EXPLICITLY_COUPLED) &
         CALL CALC_DES_2FLUID

! Calculate reaction rates and interphase mass transfer.
      CALL CALC_RRATE(MFIX_DAT, IER)

! Restore all coefficient underrelaxation factors to original values.
      IF(FLAG == 0) THEN
        UR_F_gs = loc_UR_F_gs
        UR_Kth_sml = loc_UR_Kth_sml
      ENDIF


      RETURN
      END SUBROUTINE CALC_COEFF_ALL


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CALC_COEFF                                              !
!  Purpose: This routine directs the calculation of all physical and   !
!           transport properties, and exchange rates.                  !
!                                                                      !
!  Author: M. Syamlal                                 Date: 25-AUG-05  !
!  Reviewer:                                          Date:            !
!                                                                      !
!                                                                      !
!                                                                      !
!  Literature/Document References:                                     !
!                                                                      !
!  Variables referenced:                                               !
!  Variables modified:                                                 !
!  Local variables:                                                    !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CALC_COEFF(MFIX_DAT, IER, pLevel)

         use calc_des_2fluid_mod, only: calc_des_2fluid, des_2fluid_rxns
         use exchange_mod, only: exchange
         use transport_prop_mod, only: transport_prop

! Flag for DES coupled simulation
         use discretelement, only: DES_CONTINUUM_COUPLED
! Flag for explicit coupling between the fluid and particles.
         use discretelement, only: DES_EXPLICITLY_COUPLED

         implicit none

! Path to input file
      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

! Dummy arguments
!-----------------------------------------------------------------------
! Error index
      INTEGER, intent(inout) :: IER
! Level to calculate physical properties.
! 0) Only density
! 1) Everything but density
! 2) All physical properties
      INTEGER, intent(in) :: pLevel
!-----------------------------------------------------------------------

! Calculate physical properties: (density, specific heat, diameter)
      CALL PHYSICAL_PROP(MFIX_DAT, IER, pLevel)

! Calculate transport properties: (conductivity, diffusivity, etc)
      CALL TRANSPORT_PROP

! Calculate interphase coeffs: (momentum and energy)
      CALL EXCHANGE(IER)

! Calculate DES coupled quantities.
      IF(DES_CONTINUUM_COUPLED .AND. .NOT.DES_EXPLICITLY_COUPLED) &
         CALL CALC_DES_2FLUID

      RETURN
      END SUBROUTINE CALC_COEFF

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvc
!                                                                      c
!  subroutine: calc_rrate                                              c
!  purpose: if rrate then calculate reaction rates and interphase      c
!           mass transfer. if present, calculate discrete reactions    c
!                                                                      c
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^c
      subroutine calc_rrate(mfix_dat, ier)

         use calc_des_2fluid_mod, only: calc_des_2fluid, des_2fluid_rxns
         use discretelement, only : discrete_element
         use error_manager
         use run, only: any_species_eq
         use rxns,only : rrate, use_rrates

      implicit none
!-----------------------------------------------
! dummy arguments
!-----------------------------------------------
!-----------------------------------------------
! local variables
!-----------------------------------------------
! error index
      character(len=*), intent(in) :: mfix_dat
      integer, intent(inout) :: ier

!-----------------------------------------------

! calculate reaction rates and interphase mass transfer
      if(rrate) then
! legacy hook: calculate reactions from rrates.f.
         if(use_rrates) then
            call rrates (ier)
            if(ier .eq. 1) then
               write(err_msg, 1000) mfix_dat
               call log_error()
            endif
         else
            call rrates0
         endif

! des chemical reactions
         if(any_species_eq .and. discrete_element) &
            call des_2fluid_rxns
      endif

      return

 1000 format('species balance equations are being solved; but chemical',/, &
         ' reactions are not specified in ', a, ' or in rrates.f.',/,  &
         ' copy the file mfix/model/rrates.f into the run directory ',/, &
         ' and remove the initial section that returns ier=1.')

      end subroutine calc_rrate


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CALC_TRD_AND_TAU                                        !
!  Purpose: Calculate various terms in the gas and solids phase        !
!  stress tensor as indicated below                                    !
!                                                                      !
!  Author: M. Syamlal                                 Date: 25-AUG-05  !
!                                                                      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CALC_TRD_AND_TAU()

         use calc_tau_u_g_mod, only: calc_tau_u_g
         use calc_tau_u_s_mod, only: calc_tau_u_s
         use calc_tau_v_g_mod, only: calc_tau_v_g
         use calc_tau_v_s_mod, only: calc_tau_v_s
         use calc_tau_w_g_mod, only: calc_tau_w_g
         use calc_tau_w_s_mod, only: calc_tau_w_s
         use calc_trd_g_mod, only: calc_trd_g
         use calc_trd_s_mod, only: calc_trd_s

         USE run, only: jackson
! Stress tensor trace.
      USE visc_g, only : TRD_g
      USE visc_s, only : TRD_S
! Stress tensor cross terms.
      USE tau_g, only : TAU_U_G, TAU_V_G, TAU_W_G
      USE tau_g, only : cTAU_U_G, cTAU_V_G, cTAU_W_G
      USE tau_s, only : TAU_U_S, TAU_V_S, TAU_W_S
! Runtime flag for DEM model.
      USE discretelement, only: DISCRETE_ELEMENT
! Runtime flag for TFM-DEM hybrid model.
      USE discretelement, only: DES_CONTINUUM_HYBRID

      USE param1, only: zero
      implicit none

!-----------------------------------------------------------------------

! Calculate the trace of the stress tensor (gas phase; m=0)
      CALL CALC_TRD_G (TRD_G)

! Calculate the cross terms of the stress tensor (gas phase; m=0)
      CALL CALC_TAU_U_G (TAU_U_G, CTAU_U_G)
      CALL CALC_TAU_V_G (TAU_V_G, CTAU_V_G)
      CALL CALC_TAU_W_G (TAU_W_G, CTAU_W_G)

      IF (.NOT. JACKSON) THEN
         CTAU_U_G = ZERO
         CTAU_V_G = ZERO
         CTAU_W_G = ZERO
      ENDIF

! Bypass the following calculations if there are no TFM solids.
      IF (.NOT.DISCRETE_ELEMENT .OR. DES_CONTINUUM_HYBRID) THEN
! Calculate the cross terms of the stress tensor (solids phases; m>0)
         CALL CALC_TRD_S (TRD_S)
! Calculate the trace of the stress tensor (solids phases; m>0)
         CALL CALC_TAU_U_S (TAU_U_S)
         CALL CALC_TAU_V_S (TAU_V_S)
         CALL CALC_TAU_W_S (TAU_W_S)
      ENDIF

      RETURN

   END SUBROUTINE CALC_TRD_AND_TAU

END MODULE CALC_COEFF_MOD
