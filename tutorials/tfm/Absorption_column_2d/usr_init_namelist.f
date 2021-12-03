!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: usr_init_namelist                                       C
!  Purpose: initialize user_defined NAMELIST variables                 C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE USR_INIT_NAMELIST

! Global variables
!--------------------------------------------------------------------
      use usr, only: solvent_absorption, absorption_chem_type
      use usr, only: isothermal_eq
      use usr, only: mass_transfer_type
      use usr, only: mech_dispersion, spread_factor
      use usr, only: enhancement_factor
      use usr, only: cap_press_type, usr_drag_type
      USE usr, only: d_pack, omega_pack, sa_pack, ch_pack
      USE usr, only: wetarea_type, wetareafrac0, apply_waf
      USE usr, only: omega_l0
      use usr, only: LAM_mu_s0, LAM_mu_g0
      use param1, only: undefined, undefined_c, undefined_i

      IMPLICIT NONE
! Local variables
!--------------------------------------------------------------------
      INTEGER :: L

! Include statement functions
!--------------------------------------------------------------------
      INCLUDE 'usrnlst.inc'

!<keyword category="Run Control" required="true" dtype="LOGICAL">
!  <description>
!    Solvent absorption implies solids phase 1= liquid,
!    solids phase 2 = solid.
!  </description>
!  <valid value=".TRUE." note="Solve gas-liquid-solid system."/>
!  <valid value=".FALSE." note="Do not solve a gas-liquid-solid system."/>
      SOLVENT_ABSORPTION = .FALSE.
!</keyword>

!<keyword category="Run Control" required="true" dtype="CHARACTER">
!  <description>
!    Flag for coupled mass transfer/chemical reaction scheme
!  </description>
!  <valid value="SINGLE_STEP" note="Single step liquid phase reaction."/>
!  <valid value="EQUILIBRIUM_SEGREGATED" note="Segregated equilibrium"/>
!  <valid value="EQUILIBRIUM_COUPLED" note="Coupled equilibrium"/>
      ABSORPTION_CHEM_TYPE = UNDEFINED_C
!</keyword>

!<keyword category="Run Control" required="true" dtype="CHARACTER">
!  <description>
!    Flag for mass transfer scheme: gas & liquid mass transfer coefficients
!  </description>
!  <valid value="ONDA_1968_MTC" note="Onda et al. (1968). Gas and liquid mtc."/>
!  <valid value="PSFIRST_LMTC" note="Pseudo first order for liquid mtc"/>
!  <valid value="CONSTANT_MTC" note="user specified constant value via C(2) & C(3) for mtc"/>
      MASS_TRANSFER_TYPE = UNDEFINED_C
!</keyword>

!<keyword category="Run Control" required="false" dtype="LOGICAL">
!  <description>
!    Invoke mechanical dispersion.
!  </description>
!  <valid value=".TRUE." note="Include dispersion forces."/>
!  <valid value=".FALSE." note="Do not include dispersion force."/>
!  <dependent keyword="USR_DRAG_TYPE" value="ATTOU_99" />
      MECH_DISPERSION = .FALSE.
!</keyword>

!<keyword category="Run Control" required="false" dtype="REAL">
!  <description>
!    Specified value of spread factor. Units length.
!  </description>
!  <valid value="undefined." note="calculated constant value."/>
!  <valid value=">0" note="user specified constant value."/>
!  <dependent keyword="MECH_DISPERSION" value="TRUE" />
      SPREAD_FACTOR = UNDEFINED
!</keyword>

!<keyword category="Run Control" required="true" dtype="CHARACTER">
!  <description>
!    Interaction model for momentum transfer between phases.
!  </description>
!  <valid value="ATTOU_99" note="Attou et al. (1999), CES, 54"/>
!  <valid value="ATTOU_99_MOD" note="Attou et al. (1999), CES, 54 without tortuosity"/>
!  <valid value="SOLOMENKO" note="Solomenko et al. (2015), CES, 126"/>
!  <valid value="LAPPALAINEN_MOD" note="Lappalainen et al. (2009), CES, 64"/>
!  <valid value="WEN_YU_MOD" note="Adapated Wen & Yu (1966) by Benyahia (2011)"/>
      USR_DRAG_TYPE = UNDEFINED_C
!</keyword>

!<keyword category="Run Control" required="false" dtype="CHARACTER">
!  <description>
!    Capillary pressure models. No capillary pressure term when
!    undefined.
!  </description>
!  <valid value="undefined" note="No capillary pressure."/>
!  <valid value="GROSSER_1988" note="Grosser et al. (1988), AIChE J. 34(11)"/>
      CAP_PRESS_TYPE = UNDEFINED_C
!</keyword>

!<keyword category="Run Control" required="false" dtype="REAL">
!  <description>
!    Specified constant value of enhancement factor. A constant value
!    of 1 enables pure physical absorption to be modeled, however, the
!    user must still turn off subsequent reactions.
!  </description>
!  <valid value="undefined." note="user calculated value"/>
!  <valid value=">0" note="user specified constant value."/>
      ENHANCEMENT_FACTOR = UNDEFINED
!</keyword>

!<keyword category="Run Control" required="false" dtype="CHARACTER">
!  <description>
!    Wetted-area fraction model.
!  </description>
!  <valid value="undefined" note="No fractional wetted area factor is applied."/>
!  <valid value="ONDA" note="Onda et al. (1968), J. Che. Eng. Japan"/>
!  <valid value="BILLET_95" note="Billet. (1995), Packed Towers"/>
!  <valid value="LAPPALAINEN" note="Lappalainen et al. (2008), IECR, 47"/>
!  <valid value="CONSTANT" note="Specify a constant fraction via C(50)"/>
      WETAREA_TYPE = UNDEFINED_C
!</keyword>

!<keyword category="Run Control" required="false" dtype="LOGICAL">
!  <description>
!    Flag to indicate whether to consider the fractional wetted area as
!    a factor in the interphase interaction terms.
!  </description>
!  <valid value=".TRUE." note="Consider fractional wetted area"/>
!  <valid value=".FALSE." note="Do not consider fractional wetted area"/>
      APPLY_WAF = .FALSE.
!</keyword>

!<keyword category="Run Control" required="false" dtype="REAL">
!  <description>
!    Specified value of fractional wetted area. Dimensionless value
!    between 0 and 1.
!  </description>
!  <range min="+0" max="1"/>
!  <dependent keyword="WETAREA_TYPE" value="CONSTANT"/>
      WETAREAFRAC0 = UNDEFINED
!</keyword>

! packing bed characteristics

!<keyword category="Run Control" required="false" dtype="REAL">
!  <description>
!     Packing specific wetted area parameter, only used for
!     WETAREA_TYPE=BILLET_95
!  </description>
!  <range min="+0" max="1"/>
!  <dependent keyword="WETAREA_TYPE" value="BILLET_95"/>
      CH_PACK = UNDEFINED
!</keyword>

!<keyword category="Run Control" required="false" dtype="REAL">
!  <description>
!     Nominal size of packing (cm or m)
!     If undefined will be assigned d_p0(index_sol). The value of d_pack is
!     only used in evaluation of mass transfer coefficients.  Otherwise
!     d_p0(index_sol) is always used
!  </description>
!  <valid value="undefined." note="If undefined will be assigned
!  d_p0(index_sol)"/>
!  <valid value=">0" note="only used in evaluation of mass transfer coefficients."/>
      D_PACK = UNDEFINED
!</keyword>

!<keyword category="Run Control" required="false" dtype="REAL">
!  <description>
!     Critical surface tension of packing (surface energy) in (dyne/cm in
!     CGS or N/m in SI).
!  </description>
!  <valid value=">0" note="only used in evaluation of wetted area"/>
!  <dependent keyword="WETAREA_TYPE" value="ONDA"/>
      OMEGA_PACK = UNDEFINED
!</keyword>

!<keyword category="Run Control" required="false" dtype="REAL">
!  <description>
!     Specific area of the packing: surface area of packing per
!     unit volume of bed (cm^2/cm^3 in CGS or m^2/m^3 in SI).
!     Needed whenever reacting flow or if WETAREA_TYPE is set
!     to BILLET_95 or ONDA.
!  </description>
!  <valid value=">0" note=""/>
      SA_PACK = UNDEFINED
!</keyword>

! effective viscosities
      LAM_MU_S0 = UNDEFINED
      LAM_MU_G0 = UNDEFINED

! packing bed characteristics
      isothermal_eq = .false.

! surface tension of liquid
      OMEGA_L0 = UNDEFINED

      DUMMY_DPA = UNDEFINED

      RETURN
      END SUBROUTINE USR_INIT_NAMELIST
