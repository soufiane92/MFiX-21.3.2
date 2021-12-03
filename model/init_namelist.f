MODULE INIT_NAMELIST_MOD
CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: INIT_NAMELIST                                           !
!  Purpose: initialize the NAMELIST variables                          !
!                                                                      !
!  Author: P. Nicoletti                               Date: 26-NOV-91  !
!                                                                      !
!  Keyword Documentation Format:                                       !
!                                                                      !
!<keyword category="category name" required="true/false"               !
!                                  legacy="true/false">                !
!  <description></description>                                         !
!  <arg index="" id="" max="" min=""/>                                 !
!  <dependent keyword="" value="DEFINED"/>                             !
!  <conflict keyword="" value="DEFINED"/>                              !
!  <valid value="" note="" alias=""/>                                  !
!  <range min="" max="" />                                             !
!  MFIX_KEYWORD=INIT_VALUE                                             !
!</keyword>                                                            !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

   SUBROUTINE INIT_NAMELIST

      USE bc
      USE cdist
      USE cg_init_namelist, ONLY: CARTESIAN_GRID_INIT_NAMELIST
      USE compar
      USE constant
      USE des_init_namelist_mod, ONLY: des_init_namelist
      USE fldvar
      USE geometry
      USE ic
      USE indices
      USE is
      USE iterate, only: max_nit
      USE leqsol
      USE monitor
      USE output
      USE parallel
      USE param
      USE param1
      USE physprop
      USE ps
      USE qmomk_init_namelist_mod, only: qmomk_init_namelist
      USE residual
      USE residual_pub
      USE run
      USE rxns
      USE scalars
      USE scales
      USE stiff_chem
      USE toleranc
      USE ur_facs
      use turb, only: TURBULENCE_MODEL
      use visc_g, only: mu_gmax

! user defined flags
      use usr_prop, only: usr_gama, usr_fgs, usr_fss
      use usr_prop, only: usr_rog, usr_cpg, usr_kg, usr_mug, usr_difg
      use usr_prop, only: usr_ros, usr_cps, usr_ks, usr_mus, usr_difs
      use usr_src, only: call_usr_source

      IMPLICIT NONE
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! loop counters
      INTEGER :: LC

!#####################################################################!
!                             Run Control                             !
!#####################################################################!

!<keyword dtype="CHARACTER" category="Run Control" required="true" locked="true">
!  <description>
!    Name used to create output files. The name should
!    generate legal file names after appending extensions.
!    Ex: Given the input, RUN_NAME = "bub01", MFIX will generate
!    the output files: BUB01.LOG, BUB01.OUT, BUB01.RES, etc.
!  </description>
      RUN_NAME = UNDEFINED_C
!</keyword>

!<keyword dtype="CHARACTER" category="Run Control" required="false" locked="false">
!  <description>Problem description. Limited to 60 characters.</description>
      DESCRIPTION = UNDEFINED_C
!</keyword>

!<keyword dtype="CHARACTER" category="Run Control" required="false" locked="false">
!  <description>Project version. Limited to 80 characters.</description>
      PROJECT_VERSION = UNDEFINED_C
!</keyword>

!<keyword dtype="CHARACTER" category="Run Control" required="true" locked="true">
!  <description> Simulation input/output units.</description>
!  <valid value="cgs" note="All input and output in CGS units (g, cm, s, cal)."/>
!  <valid value="SI" note="All input and output in SI units (kg, m, s, J)."/>
      UNITS = UNDEFINED_C
!</keyword>

!<keyword dtype="CHARACTER" category="Run Control" required="true" locked="true">
!  <description>Type of run.</description>
!  <valid value="new" note="A new run. There should be no .RES, .SPx,
!    .OUT, or .LOG files in the run directory."/>
!  <valid value="RESTART_1" note="Traditional restart. The run continues
!    from the last time the .RES file was updated and new data is added
!    to the SPx files."/>
!  <valid value="RESTART_2"
!    note="Start a new run with initial conditions from a .RES file
!      created from another run. No other data files (SPx) should be
!      in the run directory."/>
      RUN_TYPE = UNDEFINED_C
!</keyword>

!<keyword dtype="REAL" category="Run Control" required="false" locked="true">
!  <description>
!    Simulation start time. This is typically zero.
!  </description>
!  <range min="0.0" max="+Inf" />
      TIME = ZERO
!</keyword>

!<keyword dtype="REAL" category="Run Control" required="false" locked="false">
!  <description>
!    Simulation stop time.
!  </description>
!  <range min="0.0" max="+Inf" />
      TSTOP = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Run Control" required="false" locked="false">
!  <description>
!    Use nrel-cu optimization routines?.
!  </description>
!  <range min="0" max="1" />
      optflag1 = 0
!</keyword>


!<keyword dtype="REAL" category="Run Control" required="false" locked="false">
!  <description>
!    Initial time step size. If left undefined, a steady-state
!    calculation is performed.
!  </description>
!  <dependent keyword="TIME" value="DEFINED"/>
!  <dependent keyword="TSTOP" value="DEFINED"/>
!  <range min="0.0" max="+Inf" />
      DT = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Run Control" required="false" locked="false">
!  <description>Maximum time step size.</description>
!  <dependent keyword="TIME" value="DEFINED"/>
!  <dependent keyword="TSTOP" value="DEFINED"/>
!  <range min="0.0" max="+Inf" />
      DT_MAX = ONE
!</keyword>

!<keyword dtype="REAL" category="Run Control" required="false" locked="false">
!  <description>Minimum time step size.</description>
!  <dependent keyword="TIME" value="DEFINED"/>
!  <dependent keyword="TSTOP" value="DEFINED"/>
!  <range min="0.0" max="+Inf" />
      DT_MIN = 1.0D-6
!</keyword>

!<keyword dtype="REAL" category="Run Control" required="false" locked="false">
!  <description>
!    Factor for adjusting time step.
!
!    - The value must be less than or equal to 1.0.
!    - A value of 1.0 keeps the time step constant which may help overcome
!      initial non-convergence.
!  </description>
!  <dependent keyword="TIME" value="DEFINED"/>
!  <dependent keyword="TSTOP" value="DEFINED"/>
!  <range min="0.0" max="1" />
      DT_FAC = 0.9D0
!</keyword>

!<keyword dtype="LOGICAL" category="Run Control" required="false" locked="false">
!  <description>
!    Force a forward time-step if the maximum number of iterations,
!    MAX_NIT, is reached. The forward time-step is only forced after
!    reaching the minimum time-step, DT_MIN, for adjustable time-step
!    simulations (DT_FAC /= 1). This option should be used with caution
!    as unconverged time-steps may lead to poor simulation results and/or
!    additional convergence issues.
!  </description>
!  <valid value=".TRUE." note="Force forward time-step when DT=DT_MIN and
!    the maximum number of iterations are reached."/>
!  <valid value=".FALSE." note="Abort run when DT &lt; DT_MIN."/>
      PERSISTENT_MODE = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Run Control" required="false" locked="false">
!  <description>
!    Flag to restart the code when DT &lt; DT_MIN.
!  </description>
      AUTO_RESTART = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Run Control" required="false" locked="false">
!  <description>
!    Flag to enable/disable solving the X-momentum equations.
!  </description>
!  <arg index="1" id="Phase" min="0" max="DIM_M"/>
!  <valid value=".TRUE." note="Solve X-momentum equations."/>
!  <valid value=".FALSE." note="The X velocity initial conditions
!   persist throughout the simulation."/>
      MOMENTUM_X_EQ(:DIM_M) = .TRUE.
!</keyword>

!<keyword dtype="LOGICAL" category="Run Control" required="false" locked="false">
!  <description>
!    Flag to enable/disable solving the Y-momentum equations.
! </description>
!  <arg index="1" id="Phase" min="0" max="DIM_M"/>
!  <valid value=".TRUE." note="Solve Y-momentum equations."/>
!  <valid value=".FALSE." note="The Y velocity initial conditions
!   persist throughout the simulation."/>
      MOMENTUM_Y_EQ(:DIM_M) = .TRUE.
!</keyword>

!<keyword dtype="LOGICAL" category="Run Control" required="false" locked="false">
!  <description>
!    Flag to enable/disable solving the Z-momentum equations.
!  </description>
!  <arg index="1" id="Phase" min="0" max="DIM_M"/>
!  <valid value=".TRUE." note="Solve Z-momentum equations."/>
!  <valid value=".FALSE." note="The Z velocity initial conditions
!   persist throughout the simulation."/>
      MOMENTUM_Z_EQ(:DIM_M) = .TRUE.
!</keyword>

!<keyword dtype="LOGICAL" category="Run Control" required="false" locked="true">
!  <description>
!    Flag to enable Jackson form of momentum equations.
!    See Anderson and Jackson, (1967), IECF, 6(4), p.527.
!  </description>
!  <valid value=".TRUE." note="Solve Jackson form of momentum equations."/>
!  <valid value=".FALSE." note="Default form."/>
      JACKSON = .FALSE.
!</keyword>
!<keyword dtype="LOGICAL" category="Run Control" required="false" locked="true">
!  <description>
!    Flag to enable Ishii form of momentum equations.
!    See Ishii, (1975), Thermo-fluid dynamic theory of two-phase flow.
!  </description>
!  <valid value=".TRUE." note="Solve Ishii form of momentum equations."/>
!  <valid value=".FALSE." note="Default form."/>
      ISHII = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Run Control" required="false" locked="true">
!  <description>Solve energy equations.</description>
!  <valid value=".TRUE." note="Solve energy equations."/>
!  <valid value=".FALSE." note="Do not solve energy equations."/>
      ENERGY_EQ = .TRUE.
!</keyword>

!<keyword dtype="LOGICAL" category="Run Control" required="false" locked="true">
!  <description>Solve species transport equations.</description>
!  <arg index="1" id="Phase" min="0" max="DIM_M"/>
!  <valid value=".TRUE." note="Solve species equations."/>
!  <valid value=".FALSE." note="Do not solve species equations."/>
      SPECIES_EQ(:DIM_M) = .TRUE.
!</keyword>

!<keyword dtype="CHARACTER" category="Run Control" required="false" locked="true">
!  <description>
!    Gas phase turbulence model. ["NONE"]
!
!    For K_EPSILON (K-epsilon turbulence model for single-phase flow):
!
!    - Numerical parameters (like underrelaxation) are the same as the
!      ones for SCALAR (index = 9).
!    - All walls must be defined (NSW, FSW or PSW) in order to use
!      standard wall functions. If a user does not specify a wall type,
!      the simulation will not contain the typical turbulent profile in
!      wall-bounded flows.
!  </description>
!  <dependent keyword="MU_GMAX" value="DEFINED"/>
!  <valid value="NONE"  note="No turbulence model."/>
!  <valid value="MIXING_LENGTH"  note="Turbulent length scale must
!    be specified for the full domain using keyword IC_L_SCALE."/>
!  <valid value="K_EPSILON"  note="K-epsilon turbulence model (for
!    single-phase flow) using standard wall functions."/>
      TURBULENCE_MODEL = "NONE"
!</keyword>

!<keyword dtype="REAL" category="Run Control" required="false" locked="false">
!  <description>
!    Maximum value of the turbulent viscosity of the fluid, which
!    must be defined if any turbulence model is used.
!    A value MU_GMAX =1.E+03 is recommended. (see calc_mu_g.f)
!  </description>
      MU_GMAX = UNDEFINED
!</keyword>

!<keyword dtype="CHARACTER" category="Run Control" required="false" locked="false">
!  <description>
!     Available gas-solids drag models.
!     Note: The extension _PCF following the specified drag model
!     indicates that the polydisperse correction factor is available.
!     This option is available for TFM solids only.
!     For PCF details see:
!
!     - Van der Hoef MA, Beetstra R, Kuipers JAM. (2005)
!       Journal of Fluid Mechanics.528:233-254.
!     - Beetstra, R., van der Hoef, M. A., Kuipers, J.A.M. (2007).
!       AIChE Journal, 53:489-501.
!     - Erratum (2007), AIChE Journal, Volume 53:3020
!  </description>
!
!  <valid value="SYAM_OBRIEN" note="Syamlal M, OBrien TJ (1988).
!   International Journal of Multiphase Flow 14:473-481.
!   Two additional parameters may be specified: DRAG_C1, DRAG_D1"/>
!
!  <valid value="GIDASPOW" note="Ding J, Gidaspow D (1990).
!   AIChE Journal 36:523-538"/>
!
!  <valid value="GIDASPOW_BLEND" note="Lathouwers D, Bellan J (2000).
!    Proceedings of the 2000 U.S. DOE
!        Hydrogen Program Review NREL/CP-570-28890."/>
!
!  <valid value="WEN_YU" note="Wen CY, Yu YH (1966).
!   Chemical Engineering Progress Symposium Series 62:100-111."/>
!
!  <valid value="KOCH_HILL" note="Hill RJ, Koch DL, Ladd JC (2001).
!   Journal of Fluid Mechanics, 448: 213-241. and 448:243-278."/>
!
!  <valid value="BVK" note="Beetstra, van der Hoef, Kuipers (2007).
!   Chemical Engineering Science 62:246-255"/>
!
!  <valid value="TPKKV" note="Tang, Peters, Kuipers, Kreibitzsch, & van der Hoef.
!     AIChE J., 61(2), pp.688-698 (2015)."/>
!
!  <valid value="HYS" note="Yin, X, Sundaresan, S. (2009).
!   AIChE Journal 55:1352-1368
!   This model has a lubrication cutoff distance, LAM_HYS, that can be
!   specified. (TFM only)."/>
!
!  <valid value="GAO" note="Gao, X., Li, T., Sarkar, A., Lu, L., Rogers,
!   W.A. Development and Validation of an Enhanced Filtered Drag Model
!   for Simulating Gas-Solid Fluidization of Geldart A Particles in All Flow Regimes.
!  Chemical Engineering Science, 184, 33-51, 2018."/>

!  <valid value="SARKAR" note="Sarkar, A., Milioli, F.E., Ozarkar, S.,
!  Li, T., Sun, X.,Sundaresan, S., 2016. Filtered sub-grid constitutive models for
!  fluidized gas-particle flows constructed from 3-D simulations.
!  Chem. Eng. Sci., 152, 443-456."/>

!  <valid value="RADL" note="Radl, S., Sundaresan, S., 2014. A drag model for filtered
!  Euler-Lagrange simulations of clustered gas-particle suspensions.
!   Chem. Eng. Sci., 117, 416-425."/>

!  <valid value="TGS" note="Tenneti, S., Garg, R., Subramaniam, S., 2011.
!  Drag law for monodisperse gas solid systems using particle-resolved direct
!  numerical simulation of flow past fixed assemblies of spheres.
!  Int. J. Multiph. Flow, 37(9), 1072-1092."/>

!  <valid value="DIFELICE" note="R. Di Felice, The voidage function for fluid-particle interaction
!  systems, Int. J. Multiph. Flow 20 (1994) 153-159."/>

!  <valid value="DIFELICE_GANSER" note="G.H. Ganser, A rational approach to drag
!  prediction of spherical and nonspherical particles, Powder Technol. 77 (1993) 143-152.
!  This model requires specification of a sphericity and a reference length
! (typically the bed diameter)"/>

!  <valid value="USER_DRAG" note="Invoke user-defined drag law. (usr_drag.f)"/>
!
!  <valid value="GIDASPOW_PCF" note="(TFM only). see GIDASPOW"/>
!  <valid value="GIDASPOW_BLEND_PCF" note="(TFM only). see GIDASPOW_BLEND"/>
!  <valid value="WEN_YU_PCF" note="(TFM only). see WEN_YU"/>
!  <valid value="KOCH_HILL_PCF" note="(TFM only). see KOCH_HILL"/>
!
      DRAG_TYPE = 'SYAM_OBRIEN'
!</keyword>

!<keyword dtype="REAL" category="Run Control" required="false" locked="false">
!  <description>
!    Quantity for calibrating Syamlal-O'Brien drag correlation using Umf
!    data.  This is determined using the Umf spreadsheet.
!  </description>
      DRAG_C1 = 0.8d0
!</keyword>

!<keyword dtype="REAL" category="Run Control" required="false" locked="false">
!  <description>
!    Quantity for calibrating Syamlal-O'Brien drag correlation using Umf
!    data.  This is determined using the Umf spreadsheet.
!  </description>
      DRAG_D1 = 2.65d0
!</keyword>

!<keyword dtype="REAL" category="Run Control" required="false" locked="false">
!  <description>
!    The lubrication cutoff distance for HYS drag model.  In practice
!    this number should be on the order of the mean free path of the
!    gas for smooth particles, or the RMS roughness of a particle if
!    they are rough (if particle roughness is larger than the mean
!    free path).
!  </description>
!  <dependent keyword="DRAG_TYPE" value="HYS"/>
      LAM_HYS = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Run Control" required="false" locked="false">
!  <description>
!    Particle sphericity (between zero and one) used in the DIFELICE_GANSER drag law.
!  </description>
!  <dependent keyword="DRAG_TYPE" value="DIFELICE_GANSER"/>
!  <range min="0" max="1" />
      SPHERICITY_DG = ONE
!</keyword>

!<keyword dtype="REAL" category="Run Control" required="false" locked="false">
!  <description>
!    Reference length (typically the bed diameter) used in the DIFELICE_GANSER drag law.
!  </description>
!  <dependent keyword="DRAG_TYPE" value="DIFELICE_GANSER"/>
!  <range min="0"/>
      REF_LENGTH_DG = ONE
!</keyword>

!<keyword dtype="CHARACTER" category="Run Control" required="false" tfm="true" locked="false">
!  <description>
!    Subgrid models.
!  </description>
!
!  <valid value="Igci" note="
!   Igci, Y., Pannala, S., Benyahia, S., and Sundaresan S. (2012).
!   Industrial &amp; Engineering Chemistry Research, 2012, 51(4):2094-2103"/>
!
!  <valid value="Milioli" note="
!   Milioli, C.C., Milioli, F. E., Holloway, W., Agrawal, K. and
!   Sundaresan, S. (2013). AIChE Journal, 59:3265-3275."/>
!
      SUBGRID_TYPE = UNDEFINED_C
!</keyword>

!<keyword dtype="REAL" category="Run Control" required="false" tfm="true" locked="false">
!  <description>
!    Ratio of filter size to computational cell size.
!  </description>
      FILTER_SIZE_RATIO = 2.0D0
!</keyword>

!<keyword dtype="LOGICAL" category="Run Control" required="false" tfm="true" locked="false">
!  <description>Flag for subgrid wall correction.</description>
!  <valid value=".FALSE." note="Do not include wall correction."/>
!  <valid value=".TRUE." note="Include subgrid wall correction."/>
      SUBGRID_Wall = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Run Control" required="false" locked="false">
!  <description>
!    Shared gas-pressure formulation. See Syamlal, M. and Pannala, S.
!    "Multiphase continuum formulation for gas-solids reacting flows,"
!    chapter in Computational Gas-Solids Flows and Reacting Systems:
!    Theory, Methods and Practice, S. Pannala, M. Syamlal and T.J.
!    O'Brien (editors), IGI Global, Hershey, PA, 2011.
!  </description>
!  <valid value=".FALSE." note="Use Model A. See J.X. Bouillard
!     and R.W. Lyczkowski (1991), Powder Tech, 68:31-51."/>
!  <valid value=".TRUE."  note="Use Model B. See Bouillard, J.X.,
!    Lyczkowski, R.W., Folga, S., Gidaspow, D., Berry, G.F. (1989).
!    Canadian Journal of Chemical Engineering 67:218-229."/>
      MODEL_B = .FALSE.
!</keyword>

!<keyword dtype="INTEGER" category="Run Control" required="false" locked="true">
!  <description> The number of user-defined scalar transport equations
!    to solve.
!  </description>
!  <range min="0" max="DIM_SCALAR" />
      NScalar = 0
!</keyword>

!<keyword dtype="INTEGER" category="Run Control" required="false" locked="true">
!  <description>
!    The phase convecting the indexed scalar transport equation.
!  </description>
!  <arg index="1" id="Scalar Equation" min="1" max="DIM_SCALAR"/>
!  <range min="0" max="DIM_M" />
      Phase4Scalar(:DIM_SCALAR) = UNDEFINED_I
!</keyword>

!#####################################################################!
!                           Physical Parameters                       !
!#####################################################################!


!<keyword dtype="REAL" category="Physical Parameters" required="false" locked="true">
!  <description>Reference pressure. [0.0]</description>
      P_REF = ZERO
!</keyword>

!<keyword dtype="REAL" category="Physical Parameters" required="false" locked="true">
!  <description>Scale factor for pressure. [1.0]</description>
      P_SCALE = ONE
!</keyword>

!<keyword dtype="REAL" category="Physical Parameters" required="false" locked="false">
!  <description>Gravitational acceleration. [9.807 m/s^2 in SI]</description>
      GRAVITY = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Physical Parameters" required="false" locked="false">
!  <description>
!    X-component of gravitational acceleration vector.
!  </description>
      GRAVITY_X = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Physical Parameters" required="false" locked="false">
!  <description>
!    Y-component of gravitational acceleration vector.
!  </description>
      GRAVITY_Y = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Physical Parameters" required="false" locked="false">
!  <description>
!    Z-component of gravitational acceleration vector.
!  </description>
      GRAVITY_Z = UNDEFINED
!</keyword>





!#####################################################################!
!                          Numerical Parameters                       !
!#####################################################################!



!<keyword dtype="INTEGER" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Maximum number of iterations [500].
!  </description>
!  <range min="1" />
      MAX_NIT = 500
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Factor to normalize the gas continuity equation residual. The
!    residual from the first iteration is used if NORM_G is left
!    undefined. NORM_G=0 invokes a normalization method based on the
!    dominant term in the continuity equation. This setting may speed up
!    calculations, especially near a steady state and for incompressible
!    fluids. But, the number of iterations for the gas phase pressure,
!    LEQ_IT(1), should be increased to ensure mass balance.
!  </description>
!  <range min="0.0"/>
      NORM_G = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Factor to normalize the solids continuity equation residual. The
!    residual from the first iteration is used if NORM_S is left
!    undefined. NORM_S = 0 invokes a normalization method based on the
!    dominant term in the continuity equation. This setting may speed up
!    calculations, especially near a steady state and incompressible
!    fluids. But, the number of iterations for the solids volume
!    fraction, LEQ_IT(2), should be increased to ensure mass balance.
!  </description>
!  <range min="0.0"/>
      NORM_S = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Denominator used to scale initial fluid pressure correction equation residual when Norm_g=0. 
!    Default value is 10. Increase to tighten tolerance, decrease to loosen.
!  </description>
!  <range min="0.0"/>
      ppg_den = 10.0D0
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Denominator used to scale initial solids volume fraction correction equation residual when Norm_g=0. 
!    Default value is 10. Increase to tighten tolerance, decrease to loosen.
!  </description>
!  <range min="0.0" />
      epp_den = 10.0D0
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Maximum residual at convergence (Continuity + Momentum) [1.0d-3].
!  </description>
      TOL_RESID = 1.0D-3
!</keyword>


!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Maximum residual at convergence (Energy) [1.0d-4].
!  </description>
      TOL_RESID_T = 1.0D-4
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Maximum residual at convergence (Species Balance) [1.0d-4].
!  </description>
      TOL_RESID_X = 1.0D-4
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Maximum residual at convergence (Granular Energy) [1.0d-4].
!  </description>
      TOL_RESID_Th = 1.0D-4
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Maximum residual at convergence (Scalar Equations) [1.0d-4].
!  </description>
      TOL_RESID_Scalar = 1.0D-4
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Maximum residual at convergence (K_Epsilon Model) [1.0d-4].
!  </description>
      TOL_RESID_K_Epsilon = 1.0D-4
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Minimum residual for declaring divergence [1.0d+4].
!    This parameter is useful for incompressible fluid simulations
!    because velocity residuals can take large values for the second
!    iteration (e.g., 1e+8) before dropping down to smaller values for
!    the third iteration.
!  </description>
      TOL_DIVERGE = 1.0D+4
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Maximum residual at convergence (sum of all residuals) [1.0d-10]. If sum of
!    all residuals is below this tolerance at the first inner iteration,
!    convergence is declared. This helps run faster when steady state is
!    reached.
!  </description>
      TOL_SUM_RESID_ABS = 1.0D-10
!</keyword>

!<keyword dtype="LOGICAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Reduce the time step if the residuals stop decreasing. Disabling this
!    feature may help overcome initial non-convergence.
!  </description>
!  <valid value=".FALSE." note="Continue iterating if residuals stall."/>
!  <valid value=".TRUE."  note="Reduce time step if residuals stall."/>
      DETECT_STALL = .TRUE.
!</keyword>


!<keyword dtype="INTEGER" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    LEQ Solver selection. BiCGSTAB is the default method for all
!    equation types.
!  </description>
!  <arg index="1" id="Equation ID Number" min="1" max="DIM_EQS"/>
!  <valid value="1" note="SOR - Successive over-relaxation"/>
!  <valid value="2" note="BiCGSTAB - Biconjugate gradient stabilized."/>
!  <valid value="3" note="GMRES - Generalized minimal residual method"/>
!  <valid value="5" note="CG - Conjugate gradient"/>
      LEQ_METHOD(:) = 2
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Linear Equation tolerance [1.0d-4].
!  </description>
!  <arg index="1" id="Equation ID Number" min="1" max="DIM_EQS"/>
!  <dependent keyword="LEQ_METHOD" value="2"/>
!  <dependent keyword="LEQ_METHOD" value="3"/>
      LEQ_TOL(:) = 1.0D-4
!</keyword>

!<keyword dtype="INTEGER" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Number of iterations in the linear equation solver.
!
!    Default value:
!    - 20 iterations for equation types 1-2
!    -  5 iterations for equation types 3-5,10
!    - 15 iterations for equation types 6-9
!  </description>
!  <arg index="1" id="Equation ID Number" min="1" max="DIM_EQS"/>
      LEQ_IT(1) =  20
      LEQ_IT(2) =  20
      LEQ_IT(3) =   5
      LEQ_IT(4) =   5
      LEQ_IT(5) =   5
      LEQ_IT(6) =  15
      LEQ_IT(7) =  15
      LEQ_IT(8) =  15
      LEQ_IT(9) =  15
      LEQ_IT(10) =  5
!</keyword>

!<keyword dtype="CHARACTER" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Linear equation sweep direction. This applies when using GMRES or
!    when using the LINE preconditioner with BiCGSTAB or CG methods.
!    'RSRS' is the default for all equation types.
!  </description>
!  <arg index="1" id="Equation ID Number" min="1" max="DIM_EQS"/>
!  <valid value="RSRS" note="(Red/Black Sweep, Send Receive) repeated twice"/>
!  <valid value="ISIS" note="(Sweep in I, Send Receive) repeated twice"/>
!  <valid value="JSJS" note="(Sweep in J, Send Receive) repeated twice"/>
!  <valid value="KSKS" note="(Sweep in K, Send Receive) repeated twice"/>
!  <valid value="ASAS" note="(All Sweep, Send Receive) repeated twice"/>
      LEQ_SWEEP(:) = 'RSRS'
!</keyword>

!<keyword dtype="CHARACTER" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Linear preconditioner used by the BiCGSTAB and CG LEQ solvers. 'LINE'
!    is the default for all equation types.
!  </description>
!  <arg index="1" id="Equation ID Number" min="1" max="DIM_EQS"/>
!  <valid value="NONE" note="No preconditioner"/>
!  <valid value="LINE" note="Line relaxation"/>
!  <valid value="DIAG" note="Diagonal Scaling"/>
      LEQ_PC(:) = 'LINE'
!</keyword>


!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Underrelaxation factors.
!    Default value:
!    - 0.8 for equation types 1,9
!    - 0.5 for equation types 2,3,4,5,8
!    - 1.0 for equation types 6,7,10
!  </description>
!  <arg index="1" id="Equation ID Number" min="1" max="DIM_EQS"/>
      UR_FAC(1)  = 0.8D0     ! pressure
      UR_FAC(2)  = 0.5D0     ! rho, ep
      UR_FAC(3)  = 0.5D0     ! U
      UR_FAC(4)  = 0.5D0     ! V
      UR_FAC(5)  = 0.5D0     ! W
      UR_FAC(6)  = 1.0D0     ! T
      UR_FAC(7)  = 1.0D0     ! X
      UR_FAC(8)  = 0.5D0     ! Th
      UR_FAC(9)  = 0.8D0     ! Scalar
      UR_FAC(10) = 1.0D0     ! DES Diffusion
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    The implicitness calculation of the gas-solids drag coefficient
!    may be underrelaxed by changing ur_f_gs, which takes values
!    between 0 to 1.
!
!    -  0 updates F_GS every time step
!    -  1 updates F_GS every iteration
!  </description>
!  <range min="0.0" max="1.0" />
      UR_F_gs = 1.0D0
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Underrelaxation factor for conductivity coefficient associated
!    with other solids phases for IA Theory [1.0].
!  </description>
!  <range min="0.0" max="1.0" />
      UR_Kth_sml = 1.0D0
!</keyword>

!<keyword dtype="INTEGER" category="Numerical Parameters" required="false" locked="false">
!  <description>Discretization scheme used to solve equations.</description>
!  <arg index="1" id="Equation ID Number" min="1" max="DIM_EQS"/>
!  <valid value="0" note="First-order upwinding."/>
!  <valid value="1" note="First-order upwinding (using down-wind factors)."/>
!  <valid value="3" note="Smart."/>
!  <valid value="2" note="Superbee (recommended method)."/>
!  <valid value="5" note="QUICKEST (does not work)."/>
!  <valid value="4" note="ULTRA-QUICK."/>
!  <valid value="7" note="van Leer."/>
!  <valid value="6" note="MUSCL."/>
!  <valid value="8" note="minmod."/>
!  <valid value="9" note="Central (often unstable; useful for testing)."/>
      DISCRETIZE(:) = 0
!</keyword>

!<keyword dtype="LOGICAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Use deferred correction method for implementing higher order
!    discretization.
!  </description>
!  <valid value=".FALSE." note="Use down-wind factor method (default)."/>
!  <valid value=".TRUE."  note="Use deferred correction method."/>
      DEF_COR  =  .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    This scheme guarantees that the set of differenced species mass
!    balance equations maintain the property that the sum of species
!    mass fractions is one. This property is not guaranteed when
!    a flux limiter is used with higher order spatial discretization
!    schemes. Note: The chi-scheme is implemented for SMART and MUSCL
!    discretization schemes.
!    Darwish, M.S., Moukalled, F. (2003). Computer Methods in Applied
!    Mech. Eng., 192(13):1711-1730.
!  </description>
!  <valid value=".FALSE." note="Do not use the chi-scheme."/>
!  <valid value=".TRUE."  note="Use the chi-scheme correction."/>
      Chi_scheme = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Numerical Parameters" required="false" locked="false">
!  <description>Temporal discretization scheme.</description>
!  <valid value=".FALSE."
!    note="Implicit Euler based temporal discretization scheme employed
!      (first-order accurate in time)."/>
!  <valid value=".TRUE."
!    note="Two-step implicit Runge-Kutta method based temporal
!      discretization scheme employed. This method should be second-
!      order accurate in time excluding pressure terms and restart
!      time step which are first-order accurate. However, as of 2015,
!      testing shows that second-order accuracy is not observed."/>
      CN_ON = .FALSE.
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    The code declares divergence if the velocity anywhere in the domain
!    exceeds a maximum value.  This maximum value is automatically
!    determined from the boundary values. The user may scale the maximum
!    value by adjusting this scale factor [1.0d0].
!  </description>
      MAX_INLET_VEL_FAC = ONE
!</keyword>

!<keyword dtype="LOGICAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Solve transpose of linear system. (BICGSTAB ONLY).
!  </description>
!  <dependent keyword="LEQ_METHOD" value="2"/>
      DO_TRANSPOSE = .FALSE.
!</keyword>

!<keyword dtype="INTEGER" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Frequency to check for convergence. (BICGSTAB ONLY)
!  </description>
!  <dependent keyword="LEQ_METHOD" value="2"/>
      icheck_bicgs = 1
!</keyword>

!<keyword dtype="LOGICAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Sets optimal LEQ flags for parallel runs.
!  </description>
      OPT_PARALLEL = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Use do-loop assignment over direct vector assignment.
!  </description>
      USE_DOLOOP = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Calculate dot-products more efficiently (Serial runs only.)
!  </description>
      IS_SERIAL = .TRUE.
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Dilute flow threshold. When the volume fraction of a certain phase
!    in a cell is smaller than this value the momentum equation for that
!    phase is not solved in the cell.
!    It is strongly encouraged to keep the default value of 1E-4.
!  </description>
!  <range min="0.0" max="1.0"/>
      DIL_EP_S = 1.0D-4
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Minimum value of solids volume fraction tracked
!    It is strongly encouraged to keep the default value of 1E-8.
!  </description>
!  <range min="0.0" max="1.0" />
      ZERO_EP_s = 1.0D-8
!</keyword>


!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>
!    Small value for species mass fraction for disregarding residual calculation
!    It is strongly encouraged to keep the default value of 1E-7.
!  </description>
!  <range min="0.0" max="1.0" />
      ZERO_X_gs = 1.0D-7
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>Minimum temperature allowed in the system (K).
!    It is strongly encouraged to keep the default value of 250.0.</description>
!  <range min="0.0" max="+Inf" />
      TMIN = 250.0D0
!</keyword>

!<keyword dtype="REAL" category="Numerical Parameters" required="false" locked="false">
!  <description>Maximum temperature allowed in the system (K).
!    It is strongly encouraged to keep the default value of 4000.0.</description>
!  <range min="0.0" max="+Inf" />
      TMAX = 4000.0D0
!</keyword>

!#####################################################################!
!                      Geometry and Discretization                    !
!#####################################################################!


!<keyword dtype="CHARACTER" category="Geometry and Discretization" required="false" locked="false">
!  <description>Coordinate system used in the simulation.</description>
!  <valid value="Cartesian" note="Cartesian coordinates."/>
!  <valid value="cylindrical" note="Cylindrical coordinates."/>
      COORDINATES = UNDEFINED_C
!</keyword>

!<keyword dtype="INTEGER" category="Geometry and Discretization" required="false" locked="true">
!  <description>Number of cells in the x (r) direction.</description>
      IMAX = UNDEFINED_I
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>
!    Cell sizes in the x (r) direction. Enter values from DX(0) to
!    DX(IMAX-1).
!
!    - Use uniform mesh size with higher-order discretization methods.
!    - DX should be kept uniform in cylindrical coordinates
!      for strict momentum conservation.
!  </description>
!  <arg index="1" id="Cell" min="0" max="DIM_I"/>
      DX(:DIM_I) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>
!    The inner radius in the simulation of an annular cylindrical region.
!  </description>
      XMIN = ZERO
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>Simulation domain length in the x (r) direction.</description>
      XLENGTH = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>Simulation domain lower bound in the x-direction.</description>
      X_MIN = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>Simulation domain upper bound in the x-direction.</description>
      X_MAX = UNDEFINED
!</keyword>

!<keyword dtype="INTEGER" category="Geometry and Discretization" required="false" locked="true">
!  <description>Number of cells in the y-direction.</description>
      JMAX = UNDEFINED_I
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>
!    Cell sizes in the y-direction. Enter values from DY(0) to
!    DY(JMAX-1). Use uniform mesh size with second-order
!    discretization methods.
!  </description>
!  <arg index="1" id="Cell" min="0" max="DIM_J"/>
      DY(:DIM_J) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>Simulation domain length in the y-direction.</description>
      YLENGTH = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>Simulation domain lower bound in the y-direction.</description>
      Y_MIN = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>Simulation domain upper bound in the y-direction.</description>
      Y_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Geometry and Discretization" required="false" locked="false">
!  <description>
!    Flag to disable the third dimension (i.e., 2D simulation).
!
!      - Z axis in Cartesian coordinate system
!      - Theta in Cylindrical coordinate system
!  </description>
!  <valid value=".FALSE." note="3D simulation."/>
!  <valid value=".TRUE."  note="2D simulation."/>
      NO_K = .FALSE.
!</keyword>

!<keyword dtype="INTEGER" category="Geometry and Discretization" required="false" locked="true">
!  <description>Number of cells in the z-direction.</description>
      KMAX = UNDEFINED_I
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>
!    Cell sizes in the z (theta) direction. Enter values from DZ(0) to
!    DZ(KMAX-1). Use uniform mesh size with second-order discretization
!    methods.
!  </description>
!  <arg index="1" id="Cell" min="0" max="DIM_K"/>
      DZ(:DIM_K) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>Simulation domain lower bound in the z-direction.</description>
      Z_MIN = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>Simulation domain upper bound in the z-direction.</description>
      Z_MAX = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>Simulation domain length in the z (theta) direction.</description>
      ZLENGTH = UNDEFINED
!</keyword>


!<keyword dtype="LOGICAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>
!    Flag for making the x-direction cyclic without pressure drop. No other
!    boundary conditions for the x-direction should be specified.
!</description>
!  <valid value=".FALSE." note="No cyclic condition at x-boundary."/>
!  <valid value=".TRUE." note="Cyclic condition at x-boundary."/>
      CYCLIC_X = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>
!    Flag for making the x-direction cyclic with pressure drop. If the
!    keyword FLUX_G is given a value, this becomes a cyclic boundary
!    condition with specified mass flux. No other boundary conditions
!    for the x-direction should be specified.
!  </description>
!  <valid value=".FALSE." note="No cyclic condition at x-boundary."/>
!  <valid value=".TRUE." note="Cyclic condition with pressure drop at x-boundary."/>
      CYCLIC_X_PD = .FALSE.
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="false">
!  <description>
!    Fluid pressure drop across XLENGTH when a cyclic boundary condition
!    with pressure drop is imposed in the x-direction.
!  </description>
      DELP_X = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>
!    Flag for making the y-direction cyclic without pressure drop. No
!    other boundary conditions for the y-direction should be specified.
!  </description>
!  <valid value=".FALSE." note="No cyclic condition at y-boundary."/>
!  <valid value=".TRUE." note="Cyclic condition at x-boundary."/>
      CYCLIC_Y = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>
!    Flag for making the y-direction cyclic with pressure drop. If the
!    keyword FLUX_G is given a value this becomes a cyclic boundary
!    condition with specified mass flux. No other boundary conditions
!    for the y-direction should be specified.
!  </description>
!  <valid value=".FALSE." note="No cyclic condition at y-boundary."/>
!  <valid value=".TRUE." note="Cyclic condition with pressure drop at y-boundary."/>
      CYCLIC_Y_PD = .FALSE.
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="false">
!  <description>
!    Fluid pressure drop across YLENGTH when a cyclic boundary condition
!    with pressure drop is imposed in the y-direction.
!  </description>
      DELP_Y = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>
!    Flag for making the z-direction cyclic without pressure drop. No
!    other boundary conditions for the z-direction should be specified.
!  </description>
!  <valid value=".FALSE." note="No cyclic condition at z-boundary."/>
!  <valid value=".TRUE." note="Cyclic condition at z-boundary."/>
      CYCLIC_Z = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>
!    Flag for making the z-direction cyclic with pressure drop. If the
!    keyword FLUX_G is given a value this becomes a cyclic boundary
!    condition with specified mass flux. No other boundary conditions
!    for the z-direction should be specified.
!  </description>
!  <valid value=".FALSE." note="No cyclic condition at z-boundary."/>
!  <valid value=".TRUE." note="Cyclic condition with pressure drop at
!    z-boundary."/>
      CYCLIC_Z_PD = .FALSE.
!</keyword>

!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="false">
!  <description>
!    Fluid pressure drop across ZLENGTH when a cyclic boundary condition
!    with pressure drop is imposed in the z-direction.
!  </description>
      DELP_Z = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>
!    Imposes a mean shear on the flow field as a linear function of the
!    x coordinate. This feature should only be used when CYCLIC_X is
!    .TRUE. and the keyword V_SH is set.
!  </description>
!  <dependent keyword="CYCLIC_X" value=".TRUE."/>
!  <dependent keyword="V_SH" value="DEFINED"/>
      SHEAR = .FALSE.
!</keyword>


!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>
!    Specifies the mean y velocity component at the eastern boundary
!    of the domain (V_SH), and the mean Y velocity (-V_SH) at the
!    western boundary of the domain.
!  </description>
      V_sh = 0.0d0
!</keyword>


!<keyword dtype="REAL" category="Geometry and Discretization" required="false" locked="false">
!  <description>
!    If a value is specified, then the domain-averaged gas flux is
!    held constant at that value in simulations over a periodic
!    domain.  A pair of boundaries specified as periodic with fixed
!    pressure drop is then treated as periodic with fixed mass flux.
!    Even for this case a pressure drop must also be specified, which
!    is used as the initial guess in the simulations.
!  </description>
      Flux_g = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Geometry and Discretization" required="false" locked="true">
!  <description>
!    Applies the 2.5D model for cylindrical column by combining 2D assumption
!    and axi-symmetric assumption.
!    Li et al. (2015). A 2.5D computational method to simulate
!    cylindrical fluidized beds, Chemical Engineering Science,
!    123:236-246.
!  </description>
      CYLINDRICAL_2D = .FALSE.
!</keyword>

!<keyword dtype="INTEGER" category="Geometry and Discretization" required="false" locked="true">
!  <description>
!    Parameter to control the plate half width and the wedge radius
!    in the 2.5D cylindrical model. This value should be less than
!    half the grid cells in the radial direction (IMAX/2).  [1]
!  </description>
!  <dependent keyword="CYLINDRICAL_2D" value=".TRUE."/>
      I_CYL_NUM = 1
!</keyword>

!<keyword dtype="INTEGER" category="Geometry and Discretization" required="false" locked="true">
!  <description>
!    Parameter to smooth the transition from cylindrical to 2D in
!    the 2.5D cylindrical model. [2]
!  </description>
!  <valid value="2" note="Two cell smoothing transition."/>
!  <valid value="1" note="One cell smoothing transition."/>
!  <valid value="0" note="No smoothing."/>
!  <dependent keyword="CYLINDRICAL_2D" value=".TRUE."/>
      I_CYL_TRANSITION = 2
!</keyword>

!#####################################################################!
!                               Gas Phase                             !
!#####################################################################!

!<keyword dtype="REAL" category="Gas Phase" required="false" locked="false">
!  <description>
!    Specified constant gas density [kg/m^3 in SI]. An equation of
!    state, the ideal gas law by default, is used to calculate the gas
!    density if this parameter is undefined. The value may be set to
!    zero to make the drag zero and to simulate granular flow in a
!    vacuum. For this case, users may turn off solving for gas momentum
!    equations to accelerate convergence.
!  </description>
      RO_G0 = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Gas Phase" required="false" locked="false">
!  <description>
!    Specified constant gas viscosity [kg/(m.s) in SI].
!  </description>
      MU_G0 = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Gas Phase" required="false" locked="false">
!  <description>
!    Specified constant gas conductivity [J/(s.m.K) in SI].
!  </description>
      K_G0 = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Gas Phase" required="false" locked="false">
!  <description>
!    Specified constant gas specific heat [J/(kg.K) in SI].
!  </description>
      C_PG0 = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Gas Phase" required="false" locked="false">
!  <description>
!    Specified constant gas diffusivity [m^2/s in SI].
!  </description>
      DIF_G0 = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Gas Phase" required="false" locked="false">
!  <description>
!    Average molecular weight of gas [kg/kmol in SI]. Used in
!    calculating the gas density for non-reacting flows when the gas
!    composition is not defined.
!  </description>
      MW_AVG = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Gas Phase" required="false" locked="false">
!  <description>
!    Molecular weight of gas species [kg/kmol in SI].
!  </description>
!  <arg index="1" id="Species" min="1" max="DIM_N_G"/>
      MW_G(:DIM_N_G) = UNDEFINED
!</keyword>

!<keyword dtype="INTEGER" category="Gas Phase" required="false" locked="true">
!  <description>Number of species comprising the gas phase.</description>
      NMAX_g = UNDEFINED_I
!</keyword>

!<keyword dtype="CHARACTER" category="Gas Phase" required="false" locked="true">
!  <description>
!    Name of gas phase species as it appears in the materials database.
!  </description>
!  <arg index="1" id="Species" min="1" max="DIM_N_G"/>
      SPECIES_g(1:DIM_N_G) = UNDEFINED_C
!</keyword>

!<keyword dtype="CHARACTER" category="Gas Phase" required="false" locked="true">
!  <description>
!    User defined name for gas phase species. Aliases are used in
!    specifying chemical equations and must be unique.
!  </description>
!  <arg index="1" id="Species" min="1" max="DIM_N_G"/>
      SPECIES_ALIAS_g(1:DIM_N_G) = UNDEFINED_C
!</keyword>



!#####################################################################!
!                            Solids Phase                             !
!#####################################################################!

!<keyword dtype="CHARACTER" category="Solids Phase" required="false" locked="false">
!  <description>
!    Defines the model used for the solids phase.
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
!  <valid value='TFM' note='Two-fluid model (continuum)'/>
!  <valid value='DEM' note='Discrete element model'/>
!  <valid value='CGP' note='Coarse-grained particle'/>
!  <valid value='PIC' note='Multiphase particle-in-cell'/>
      SOLIDS_MODEL(:DIM_M) = 'TFM'
!</keyword>

!<keyword dtype="INTEGER" category="Solids Phase" required="false"
!  tfm="true" dem="true" pic="true" locked="true">
!  <description>Number of solids phases.</description>
      MMAX = 1
!</keyword>

!<keyword dtype="REAL" category="Solids Phase" required="false"
!  tfm="true" dem="true" pic="true" locked="false">
!  <description>
!    Initial particle diameters [m in SI].
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      D_P0(:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Solids Phase" required="false"
!  tfm="true" dem="true" pic="true" locked="false">
!  <description>
!    Specified constant solids density [kg/m^3 in SI]. Reacting flows
!    may use variable solids density by leaving this parameter
!    undefined and specifying X_S0 and RO_XS0 as well as the index
!    of the inert species.
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      RO_S0(:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Solids Phase" required="false" tfm="true" dem="true" locked="false">
!  <description>
!    Baseline species mass fraction. Specifically, the mass fraction
!    of an unreacted sample (e.g., proximate analysis).
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
!  <arg index="2" id="Species" min="1" max="DIM_N_s"/>
!  <dependent keyword="SPECIES_EQ" value=".TRUE."/>
!  <dependent keyword="RO_Xs0" value="DEFINED"/>
!  <dependent keyword="INERT_SPECIES" value="DEFINED"/>
!  <conflict keyword="RO_s0" value="DEFINED"/>
      X_s0(:DIM_M,:DIM_N_s) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Solids Phase" required="false" tfm="true" dem="true" locked="false">
!  <description>
!    Specified constant solids species density [kg/m^3 in SI].
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
!  <arg index="2" id="Species" min="1" max="DIM_N_s"/>
!  <dependent keyword="SPECIES_EQ" value=".TRUE."/>
!  <dependent keyword="X_s0" value="DEFINED"/>
!  <dependent keyword="INERT_SPECIES" value="DEFINED"/>
!  <conflict keyword="RO_s0" value="DEFINED"/>
      RO_Xs0(:DIM_M,:DIM_N_s) = UNDEFINED
!</keyword>

!<keyword dtype="INTEGER" category="Solids Phase" required="false" tfm="true" dem="true" locked="false">
!  <description>
!    Index of inert solids phase species. This species should not be a
!    product or reactant of any chemical reaction.
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
!  <dependent keyword="SPECIES_EQ" value=".TRUE."/>
!  <dependent keyword="X_s0" value="DEFINED"/>
!  <dependent keyword="RO_Xs0" value="DEFINED"/>
!  <conflict keyword="RO_s0" value="DEFINED"/>
      INERT_SPECIES(:DIM_M) = UNDEFINED_I
!</keyword>

!<keyword dtype="REAL" category="Solids Phase" required="false" tfm="true" dem="true" locked="false">
!  <description>
!    Mass fraction of inert solids phase species in the dilute region.
!    In dilute region (see DIL_FACTOR_VSD), the solids density is computed based
!    on this inert species mass fraction, rather than the current inert species mass fraction.
!    This may help convergence when the Variable Solids Density model is invoked.
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
!  <dependent keyword="SPECIES_EQ" value=".TRUE."/>
!  <dependent keyword="X_s0" value="DEFINED"/>
!  <dependent keyword="RO_Xs0" value="DEFINED"/>
!  <conflict keyword="RO_s0" value="DEFINED"/>
      DIL_INERT_X_VSD(:DIM_M) = ONE
!</keyword>

!<keyword dtype="REAL" category="Solids Phase" required="false" tfm="true" dem="true" locked="false">
!  <description>
!    Factor to define the dilute region where the solids density is set using DIL_INERT_X_VSD.
!    Cells where the solids volume fraction is between DIL_EP_S and DIL_EP_S x DIL_FACTOR_VSD
!    will automatically set the solids density using DIL_INERT_X_VSD instead of the current
!    inerts species mass fraction. Set this factor to zero to always use the current inert
!    species mass fraction.
!  </description>
!  <dependent keyword="SPECIES_EQ" value=".TRUE."/>
!  <dependent keyword="X_s0" value="DEFINED"/>
!  <dependent keyword="RO_Xs0" value="DEFINED"/>
!  <conflict keyword="RO_s0" value="DEFINED"/>
      DIL_FACTOR_VSD = 10.0D0
!</keyword>

!<keyword dtype="REAL" category="Solids Phase" required="false" tfm="true" dem="true" locked="false">
!  <description>
!    Specified solids or material conductivity [J/(s.m.K) in SI].
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
!  <dependent keyword="KS_MODEL" value="DEFINED"/>
      K_S0(:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="CHARACTER" category="Solids Phase" required="false" tfm="true" dem="true" locked="false">
!  <description>
!    Solids (TFM) or particle (DEM) thermal conductivity model for each phase.
!  </description>
!  <valid value="NONE"
!    note="(TFM or DEM). K_s0 is set to 0 (no conduction). In DEM, calculations are skipped."/>
!  <valid value="CONST_EFF"
!    note="(TFM only). K_s0 specifies a constant effective solids phase conductivity."/>
!  <valid value="USR"
!    note="(TFM only). K_s0 is set based on user defined function."/>
!  <valid value="Bauer"
!    note="(TFM only). K_s0 specifies the material conductivity.
!         See Bauer, R. and Schlünder, E.U. (1978). Int. Chem. Eng. 18(2), 181."/>
!  <valid value="Musser"
!   note="(DEM only). K_s0 specifies the material conductivity.
!         See Musser, J. (2011), PhD Dissertation, WVU. Chapter 3 (3.6)."/>
!  <dependent keyword="ENERGY_EQ" value="TRUE"/>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      KS_MODEL(:DIM_M) =  UNDEFINED_C
!</keyword>

!<keyword dtype="REAL" category="Solids Phase" required="false" tfm="true" dem="true" locked="false">
!  <description>
!    Specified constant solids specific heat [J/(kg.K) in SI].
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      C_PS0(:DIM_M) = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Solids Phase" required="false" tfm="true" dem="true" locked="false">
!  <description>
!    Molecular weight of solids phase species [kg/kmol in SI].
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
!  <arg index="2" id="Species" min="1" max="DIM_N_s"/>
      MW_S(:DIM_M,:DIM_N_s) = UNDEFINED
!</keyword>

!<keyword dtype="INTEGER" category="Solids Phase" required="false" tfm="true" dem="true" locked="true">
!  <description>
!    Number of species comprising the solids phase.
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      NMAX_s(:DIM_M) = UNDEFINED_I
!</keyword>

!<keyword dtype="CHARACTER" category="Solids Phase" required="false" tfm="true" dem="true" locked="true">
!  <description>
!    Name of solids phase M, species N as it appears in the materials
!    database.
!</description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
!  <arg index="2" id="Species" min="1" max="DIM_N_s"/>
      SPECIES_s(:DIM_M,:DIM_N_s) = UNDEFINED_C
!</keyword>

!<keyword dtype="CHARACTER" category="Solids Phase" required="false" tfm="true" dem="true" locked="true">
!  <description>
!    User defined name for solids phase species. Aliases are used in
!    specifying chemical equations and must be unique.
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
!  <arg index="2" id="Species" min="1" max="DIM_N_s"/>
      SPECIES_ALIAS_s(:DIM_M,:DIM_N_s) = UNDEFINED_C
!</keyword>

!#####################################################################!
!                           Two Fluid Model                           !
!#####################################################################!


!<keyword dtype="CHARACTER" category="Two Fluid Model" required="false" tfm="true" locked="true">
!  <description>
!    Solids phase stress model [Algebraic].
!  </description>
!  <valid value="ALGEBRAIC"
!    note="Granular energy algebraic formulation."/>
!  <valid value="AHMADI"
!    note="Cao and Ahmadi (1995). Int. J. Multiphase Flow 21(6), 1203."/>
!  <valid value="GD_99"
!     note="Garzo and Dufty (1999). Phys. Rev. E 59(5), 5895."/>
!  <valid value="GHD"
!    note="Garzo, Hrenya and Dufty (2007). Phys. Rev. E 76(3), 31304"/>
!  <valid value="GTSH"
!    note="Garzo, Tenneti, Subramaniam, Hrenya (2012). J.Fluid Mech. 712, 129."/>
!  <valid value="IA_NONEP"
!     note="Iddir &amp; Arastoopour (2005). AIChE J. 51(6), 1620"/>
!  <valid value="LUN_1984"
!    note="Lun et al (1984). J. Fluid Mech., 140, 223."/>
!  <valid value="SIMONIN"
!    note="Simonin (1996). VKI Lecture Series, 1996-2"/>
      KT_TYPE = "ALGEBRAIC"
!</keyword>

!<keyword dtype="CHARACTER" category="Two Fluid Model" required="false" tfm="true" locked="true">
!  <description>
!    Solids stress friction model selection.
!  </description>
!  <valid value="NONE" note="Only solids pressure"/>
!  <valid value="SCHAEFFER" note="Schaeffer friction model.
!     1987, Journal of Differential Equations 66, 19–50."/>
!  <valid value="SRIVASTAVA"  note="Srivastava and Sundaresan friction model.
!     2003, Powder Technology 129(1-3):72-85"/>
!  <valid value="GUO_BOYCE"  note="Guo-Boyce friction model, 2021.
!    Proceedings of the National Academy of Sciences, 118 (35)."/>
      FRICTION_MODEL = 'SCHAEFFER'
!</keyword>

!<keyword dtype="CHARACTER" category="Two Fluid Model" required="false" tfm="true" locked="true">
!  <description>
!    Blend the Schaeffer stresses with the stresses resulting from
!    algebraic kinetic theory around the value of EP_STAR. [NONE]
!  </description>
!  <valid value="NONE" note="No blending"/>
!  <valid value="TANH_BLEND" note="Hyperbolic tangent function"/>
!  <valid value="SIGM_BLEND" note="Scaled sigmodial function"/>
!  <dependent keyword="FRICTION_MODEL" value="SCHAEFFER"/>
      BLENDING_FUNCTION = "NONE"
!</keyword>

!<keyword dtype="LOGICAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Use Yu-Standish correlation to compute maximum packing for polydisperse systems.
!
!    A.B. Yu and N. Standish. Powder Tech, 52 (1987) 233-241
!  </description>
!  <valid value=".TRUE."
!    note="Use the Yu-Standish correlation."/>
!  <valid value=".FALSE."
!    note="Do not use the Yu-Standish correlation."/>
      YU_STANDISH = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Use Fedors-Landel correlation to compute maximum packing for binary (only) mixtures of powders.
!
!    R.F. Fedors and R.F. Landel. Powder Tech, 23 (1979) 225-231
!  </description>
!  <valid value=".TRUE."
!    note="Use the Fedors-Landel correlation."/>
!  <valid value=".FALSE."
!    note="Do not use the Fedors-Landel correlation."/>
      FEDORS_LANDEL = .FALSE.
!</keyword>

!<keyword dtype="CHARACTER" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Radial distribution function (RDF) at contact.
!  </description>
!
!  <valid value="CARNAHAN_STARLING" note="
!    Carnahan, N.F. and Starling K.E., (1969).
!    The Journal of Chemical Physics, Vol. 51(2):635-636.
!    Only applies to monodisperse cases."/>
!
!  <valid value="MA_AHMADI" note="
!    Ma, D. and Ahmadi, G., (1986).
!    The Journal of Chemical Physics, 84(6):3449.
!    Only applies to monodisperse cases."/>
!
!  <valid value="LEBOWITZ" note="Lebowitz, J.L. (1964)
!    The Physical Review, A133, 895-899.
!    Only applies to polydisperse cases."/>
!
!  <valid value="MODIFIED_LEBOWITZ" note="
!    Iddir, H. Y., Modeling of the multiphase mixture of particles
!    using the kinetic theory approach. Doctoral Dissertation,
!    Illinois Institute of Technology, Chicago, Illinois, 2004,
!    (chapter 2, equations 2-49 through 2-52.)
!    Only applies to polydisperse cases."/>
!
!  <valid value="MANSOORI" note="
!   Mansoori, GA, Carnahan N.F., Starling, K.E. Leland, T.W. (1971).
!     The Journal of Chemical Physics, Vol. 54:1523-1525.
!     Only applies to polydisperse cases."/>
!
!  <valid value="MODIFIED_MANSOORI" note="van Wachem, B.G.M., Schouten, J.C.,
!    van den Bleek, C.M., Krishna, R. and Sinclair, J. L. (2001)
!     AIChE Journal 47:1035–1051.
!     Only applies to polydisperse cases."/>
      RDF_TYPE = UNDEFINED_C
!</keyword>

!<keyword dtype="LOGICAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Flag to include the added (or virtual) mass force. This force
!    acts to increase the inertia of the dispersed phase, which
!    tends to stabilize simulations of bubbly gas-liquid flows.
!  </description>
!  <dependent keyword="M_AM" value="DEFINED"/>
      Added_Mass = .FALSE.
!</keyword>

!<keyword dtype="INTEGER" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    The disperse phase number to which the added mass is applied.
!  </description>
      M_AM = UNDEFINED_I
!</keyword>

!<keyword dtype="REAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Coefficient of restitution for particle-particle collisions.
!  </description>
!  <range min="0.0" max="1.0" />
      C_E = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Two Fluid Model" required="false" locked="false">
!  <description>
!    Coefficient of restitution for particle-particle collisions specific
!    to GHD theory implementation.
!  </description>
!  <arg index="1" id="Phase" min="1" max="2"/>
!  <arg index="2" id="Phase" min="1" max="2"/>
      r_p(:DIM_M, :DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Two Fluid Model" required="false" locked="false">
!  <description>
!    Coefficient of restitution for particle-wall collisions when using
!    Johnson and Jackson partial slip BC (BC_JJ_PS).</description>
!  <range min="0.0" max="1.0" />
      E_W = 1.D0
!</keyword>

!<keyword dtype="REAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Specularity coefficient associated with particle-wall collisions
!    when using Johnson and Jackson partial slip BC (BC_JJ_PS). If
!    Jenkins small frictional BC are invoked (JENKINS) then PHIP is
!    not used.
!  </description>
!  <range min="0.0" max="1.0" />
      PHIP = 0.6D0
!</keyword>

!<keyword dtype="REAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Specify the value of specularity coefficient when the normalized
!    slip velocity goes to zero when BC_JJ_M is .TRUE.. This variable
!    is calculated internally in MFiX. Do not modify unless an
!    accurate number is known.
!  </description>
!  <dependent keyword="BC_JJ_M" value=".TRUE."/>
      PHIP0 = undefined
!</keyword>

!<keyword dtype="REAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Coefficient of friction between the particles of two solids phases.
!  </description>
      C_F = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!     Angle of internal friction (in degrees). Set this value
!     to zero to turn off plastic regime stress calculations.
!  </description>
      PHI = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Angle of internal friction (in degrees) at walls. Set this
!    value to non-zero (PHI_W = 11.31 means TAN_PHI_W = MU = 0.2)
!    when using Johnson and Jackson partial slip BC (BC_JJ_PS) with
!    Friction model or Jenkins small frictional boundary condition.
!  </description>
      PHI_W = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Minimum solids fraction above which friction sets in. [0.5]
!  </description>
      EPS_F_MIN = 0.5D0
!</keyword>

!<keyword dtype="REAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Maximum solids volume fraction at packing for polydisperse
!    systems (more than one solids phase used). The value of
!    EP_STAR may change during the computation if solids phases
!    with different particle diameters are specified and
!    Yu_Standish or Fedors_Landel correlations are used.
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
!  <range min="0" max="1-EP_STAR" />
      EP_S_MAX(:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Used in calculating the initial slope of segregation: see
!    Gera et al. (2004) - recommended value 0.3. Increasing this
!    coefficient results in decrease in segregation of particles
!    in binary mixtures.
!  </description>
      SEGREGATION_SLOPE_COEFFICIENT=0.D0
!</keyword>


!<keyword dtype="REAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>Excluded volume in Boyle-Massoudi stress.</description>
!  <valid value="0.0" note="Boyle-Massoudi stress is turned off."/>
      V_EX = ZERO
!</keyword>

!<keyword dtype="REAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Specified constant viscosity. If any value is specified then:
!
!    - kinetic theory calculations (granular_energy) are off, which
!       means zero granular pressure contribution (P_S = 0)
!    - frictional/plastic calculations are off, which means zero
!       frictional viscosity contributions, however, a plastic pressure
!       term is still invoked (P_STAR)
!    - LAMBDA_S = -2/3 MU_S0
!
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      MU_S0(:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Specified constant solids diffusivity [m^2/s in SI].
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      DIF_S0(:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Packed bed void fraction. Used to calculate plastic stresses (for
!    contribution to viscosity) and when to implement plastic pressure,
!    P_STAR. Specifically, if EP_G &lt; EP_STAR, then plastic pressure is
!    employed in the momentum equations.
!  </description>
!  <range min="0.0" max="1.0" />
      EP_STAR = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Two Fluid Model" required="false" tfm="true" locked="false">
!  <description>
!    Flag to enable/disable a phase from forming a packed bed.
!    Effectively removes plastic pressure term from the solids phase
!    momentum equation.
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
!  <valid value=".TRUE." note="The phase forms a packed bed with void
!    fraction EP_STAR."/>
!  <valid value=".FALSE." note="The phase can exceed close pack conditions
!    so that it maybe behave like a liquid."/>
      CLOSE_PACKED(:DIM_M) = .TRUE.
!</keyword>


!#####################################################################!
!                   Initial Conditions Section                        !
!#####################################################################!


      DO LC = 1, DIMENSION_IC

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>X coordinate of the west face.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_X_W(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>X coordinate of the east face.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_X_E(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Y coordinate of the south face.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_Y_S(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Y coordinate of the north face.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_Y_N(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Z coordinate of the bottom face.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_Z_B(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Z coordinate of the top face.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_Z_T(LC) = UNDEFINED
!</keyword>

!<keyword dtype="INTEGER" category="Initial Condition" required="false" locked="false">
!  <description>I index of the west-most wall.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_I_W(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Initial Condition" required="false" locked="false">
!  <description>I index of the east-most wall.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_I_E(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Initial Condition" required="false" locked="false">
!  <description>J index of the south-most wall.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_J_S(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Initial Condition" required="false" locked="false">
!  <description>J index of the north-most wall.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_J_N(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Initial Condition" required="false" locked="false">
!  <description>K index of the bottom-most wall.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_K_B(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Initial Condition" required="false" locked="false">
!  <description>K index of the top-most wall.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_K_T(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="CHARACTER" category="Initial Condition" required="false" locked="false">
!  <description>
!    Type of initial condition. Mainly used in restart runs to overwrite
!    values read from the .RES file by specifying it as _PATCH_. The
!    user needs to be careful when using the _PATCH_ option, since the
!    values from the .RES file are overwritten and no error checking is
!    done for the patched values.
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_TYPE(LC) = UNDEFINED_C
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Initial void fraction in the IC region.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_EP_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>
!    Initial gas pressure in the IC region. If this quantity is not
!    specified, MFiX will set up a hydrostatic pressure profile,
!    which varies only in the y-direction.
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_P_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>
!    Initial solids pressure in the IC region. Usually, this value is
!    specified as zero.
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_P_STAR(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Turbulence length scale in the IC region.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_L_SCALE(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>
!    Initial bulk density (rop_s = ro_s x ep_s) of solids phase in the
!    IC region. Users need to specify this IC only for polydisperse flow
!    (MMAX &gt; 1). Users must make sure that summation of ( IC_ROP_s(ic,m)
!    / RO_s(m) ) over all solids phases is equal to ( 1.0 - IC_EP_g(ic)).
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         IC_ROP_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>
!    Initial solids volume fraction of solids phase in the IC region.
!    This may be specified in place of IC_ROP_s.
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         IC_EP_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="CHARACTER" category="Initial Condition" required="false" locked="false">
!  <description>
!    Initial particle size distribution type of solids phase in the IC region.
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <valid value='MONO'
!     note='Uniform particle size.'/>
!  <valid value='NORMAL'
!     note='Normal (Gaussian) particle size distribution.'/>
!  <valid value='LOG_NORMAL'
!     note='Log-normal particle size distribution.'/>
!  <valid value='CUSTOM'
!     note='Particle size distribution specified in external file.'/>
         IC_PSD_TYPE(LC,:DIM_M) = "MONO"
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>
!    Initial mean value of particle size distribution of solids phase in the
!    IC region.
!    For a log-normal psd, The mean diameter (µ) and standard deviation (𝜎) will be used
!    to compute the log-normal parameters µ_ln and 𝜎_ln :
!    µ_ln = ln(µ²/√(µ² + 𝜎²)) and 𝜎_ln = √(ln(1 + (𝜎/µ)²)))
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         IC_PSD_MEAN_DP(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>
!    Initial standard deviation of particle size distribution of solids phase
!    in the IC region.
!    For a log-normal psd, The mean diameter (µ) and standard deviation (𝜎) will be used
!    to compute the log-normal parameters µ_ln and 𝜎_ln :
!    µ_ln = ln(µ²/√(µ² + 𝜎²)) and 𝜎_ln = √(ln(1 + (𝜎/µ)²)))
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         IC_PSD_STDEV(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>
!    Initial maximum particle size of particle size distribution of solids phase
!    in the IC region.
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         IC_PSD_MAX_DP(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>
!    Initial minimum particle size of particle size distribution of solids phase
!    in the IC region.
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         IC_PSD_MIN_DP(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Initial gas phase temperature in the IC region.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_T_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Initial solids phase temperature in the IC region.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         IC_T_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Initial solids phase granular temperature in the IC region.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         IC_THETA_M(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>
!    Gas phase radiation coefficient in the IC region. Modify file
!    rdtn2.inc to change the source term.
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_GAMA_RG(LC) = ZERO
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Gas phase radiation temperature in the IC region.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_T_RG(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>
!    Solids phase radiation coefficient in the IC region. Modify file
!    energy_mod.f to change the source term.
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         IC_GAMA_RS(LC,:DIM_M) = ZERO
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Solids phase radiation temperature in the IC region.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         IC_T_RS(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Initial x-component of gas velocity in the IC region.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_U_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Initial x-component of solids-phase velocity in the IC region.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         IC_U_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Initial y-component of gas velocity in the IC region.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_V_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Initial y-component of solids-phase velocity in the IC region.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         IC_V_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Initial z-component of gas velocity in the IC region.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
         IC_W_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Initial z-component of solids-phase velocity in the IC region.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         IC_W_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Initial mass fraction of gas species.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Species" min="1" max="DIM_N_G"/>
         IC_X_G(LC,:DIM_N_G) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Initial mass fraction of solids species.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <arg index="3" id="Species" min="1" max="DIM_N_S"/>
         IC_X_S(LC,:DIM_M,:DIM_N_S) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Initial value of Scalar n, assigned to scalar equation.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Scalar Eq." min="1" max="DIM_SCALAR"/>
        IC_SCALAR(LC,:DIM_SCALAR) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Initial value of K in K-Epsilon equation.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <range min="0" max="" />
         IC_K_Turb_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="false">
!  <description>Initial value of Epsilon in K-Epsilon equation.</description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <range min="0" max="" />
         IC_E_Turb_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Initial Condition" required="false" locked="true">
!  <description>Flag for inflating initial lattice distribution
! to the entire IC region. </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
          IC_DES_FIT_TO_REGION(LC) = .FALSE.
!</keyword>

!<keyword dtype="CHARACTER" category="Initial Condition" required="false" locked="true">
!  <description>Type of initial lattice distribution (simple cubic or hexagonal)
! to the entire IC region. </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <valid value='HEXA'
!     note='Particles are distributed on a hexagonal close-packed lattice.'/>
!  <valid value='CUBIC'
!    note='Particles are distributed on a cubical lattice.'/>
          IC_DES_LATTICE(LC,:DIM_M) = 'HEXA'
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="true">
!  <description>Spacing between particle within initial lattice distribution
! (expressed as a fraction of particle diameter)
! to the entire IC region. </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <range min="0" max="" />
          IC_DES_SPACING(LC,:DIM_M) = 0.05
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="true">
!  <description>Spacing factor in x-direction (multiplier of IC_DES_SPACING).
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <range min="0" max="1" />
          IC_DES_SPACE_FACTOR_X(LC,:DIM_M) = 1.0
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="true">
!  <description>Spacing factor in y-direction (multiplier of IC_DES_SPACING).
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <range min="0" max="1" />
          IC_DES_SPACE_FACTOR_Y(LC,:DIM_M) = 1.0
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="true">
!  <description>Spacing factor in z-direction (multiplier of IC_DES_SPACING).
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <range min="0" max="1" />
          IC_DES_SPACE_FACTOR_Z(LC,:DIM_M) = 1.0
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="true">
!  <description>Random factor applied to particle positions  within the lattice distribution.
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <range min="0" max="1" />
          IC_DES_RAND(LC,:DIM_M) = 0.0
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="true">
!  <description>Random factor in x-direction (multiplier of IC_DES_RAND)
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <range min="0" max="1" />
          IC_DES_RAND_FACTOR_X(LC,:DIM_M) = 1.0
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="true">
!  <description>Random factor in y-direction (multiplier of IC_DES_RAND)
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <range min="0" max="1" />
          IC_DES_RAND_FACTOR_Y(LC,:DIM_M) = 1.0
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="true">
!  <description>Random factor in z-direction (multiplier of IC_DES_RAND)
!  </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <range min="0" max="1" />
          IC_DES_RAND_FACTOR_Z(LC,:DIM_M) = 1.0
!</keyword>

!<keyword dtype="REAL" category="Initial Condition" required="false" locked="true">
!  <description>Initial solids mass within the entire IC region. </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
          IC_DES_SM(LC,: DIM_M) = ZERO
!</keyword>

!<keyword dtype="INTEGER" category="Initial Condition" required="false" locked="true">
!  <description>Initial particle count within the entire IC region. </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
          IC_DES_NP(LC,: DIM_M) = 0
!</keyword>


!<keyword dtype="LOGICAL" category="Initial Condition" required="false" locked="true">
!  <description>Check for particle overlap with SL geometry
! in the entire IC region. </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
          IC_DES_CHECK_STL_OVERLAP(LC,:DIM_M) = .TRUE.
!</keyword>


!<keyword dtype="REAL" category="Initial Condition" required="false" locked="true">
!  <description>Flag to specify the initial constant statistical
! weight for computational particles/parcels. Actual number of
! parcels will be automatically computed. </description>
!  <arg index="1" id="IC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <dependent keyword="SOLIDS_MODEL" value="PIC"/>
          IC_PIC_CONST_STATWT(LC, :DIM_M) = ZERO
!</keyword>
      ENDDO




!#####################################################################!
!                        Boundary Conditions                          !
!#####################################################################!
      DO LC = 1, DIMENSION_BC


!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="true">
!  <description>X coordinate of the west face or edge.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_X_W(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="true">
!  <description>X coordinate of the east face or edge.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_X_E(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="true">
!  <description>Y coordinate of the south face or edge.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_Y_S(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="true">
!  <description>Y coordinate of the north face or edge.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_Y_N(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="true">
!  <description>Z coordinate of the bottom face or edge.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_Z_B(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="true">
!  <description>Z coordinate of the top face or edge.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_Z_T(LC) = UNDEFINED
!</keyword>

!<keyword dtype="INTEGER" category="Boundary Condition" required="false" locked="true">
!  <description>I index of the west-most cell.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_I_W(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Boundary Condition" required="false" locked="true">
!  <description>I index of the east-most cell.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_I_E(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Boundary Condition" required="false" locked="true">
!  <description>J index of the south-most cell.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_J_S(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Boundary Condition" required="false" locked="true">
!  <description>J index of the north-most cell.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_J_N(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Boundary Condition" required="false" locked="true">
!  <description>K index of the bottom-most cell.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_K_B(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Boundary Condition" required="false" locked="true">
!  <description>K index of the top-most cell.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_K_T(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="CHARACTER" category="Boundary Condition" required="false" locked="true">
!  <description>Type of boundary.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!
!  <valid value='DUMMY'
!    note='The specified boundary condition is ignored. This is
!      useful for turning off some boundary conditions without having
!      to delete them from the file.' />
!
!  <valid value='MASS_INFLOW' alias='MI'
!    note='Mass inflow rates for gas and solids phases are
!      specified at the boundary.'/>
!
!  <valid value='MASS_OUTFLOW' alias='MO'
!    note='The specified values of gas and solids mass outflow
!      rates at the boundary are maintained, approximately. This
!      condition should be used sparingly for minor outflows, when
!      the bulk of the outflow is occurring through other constant
!      pressure outflow boundaries.' />
!
!  <valid value='P_INFLOW' alias='PI'
!    note='Inflow from a boundary at a specified constant
!      pressure. To specify as the west, south, or bottom end of
!      the computational region, add a layer of wall cells to the
!      west, south, or bottom of the PI cells. Users need to specify
!      all scalar quantities and velocity components. The specified
!      values of fluid and solids velocities are only used initially
!      as MFIX computes these values at this inlet boundary.' />
!
!  <valid value='P_OUTFLOW' alias='PO'
!    note='Outflow to a boundary at a specified constant pressure.
!      To specify as the west, south, or bottom end of the computational
!      region, add a layer of wall cells to the west, south, or bottom of
!      the PO cells.' />
!
!  <valid value='FREE_SLIP_WALL' alias='FSW'
!    note='Velocity gradients at the wall vanish. If BC_JJ_PS is
!      equal to 1, the Johnson-Jackson boundary condition is used for
!      solids.  A FSW is equivalent to using a PSW with Hw=0.' />
!
!  <valid value='NO_SLIP_WALL' alias='NSW'
!    note='All components of the velocity vanish at the wall. If
!      BC_JJ_PS is equal to 1, the Johnson-Jackson boundary condition is
!      used for solids.  A NSW is equivalent to using a PSW with vw=0
!      and Hw undefined.' />
!
!  <valid value='PAR_SLIP_WALL' alias='PSW'
!    note='Partial slip at the wall implemented as the boundary condition:
!      dv/dn + Hw (v - vw) = 0, where n is the normal pointing from the
!      fluid into the wall. The coefficients Hw and vw should be
!      specified. For free-slip set Hw = 0. For no-slip leave Hw
!      undefined (Hw=+inf) and set vw = 0. To set Hw = +inf, leave it
!      unspecified. If BC_JJ_PS is equal to 1, the Johnson-Jackson
!      boundary condition is used for solids.' />
         BC_TYPE(LC) = UNDEFINED_C
!</keyword>

!<keyword dtype="CHARACTER" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Particle size distribution type of solids phase at BC region.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <valid value='MONO'
!     note='Uniform particle size.'/>
!  <valid value='NORMAL'
!     note='Normal (Gaussian) particle size distribution.'/>
!  <valid value='LOG_NORMAL'
!     note='Log-normal particle size distribution.'/>
!  <valid value='CUSTOM'
!     note='Particle size distribution specified in external file.'/>
         BC_PSD_TYPE(LC,:DIM_M) = "MONO"
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Mean value of particle size distribution of solids phase in the
!    BC region.
!    For a log-normal psd, The mean diameter (µ) and standard deviation (𝜎) will be used
!    to compute the log-normal parameters µ_ln and 𝜎_ln :
!    µ_ln = ln(µ²/√(µ² + 𝜎²)) and 𝜎_ln = √(ln(1 + (𝜎/µ)²)))
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_PSD_MEAN_DP(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Standard deviation of particle size distribution of solids phase
!    in the BC region.
!    For a log-normal psd, The mean diameter (µ) and standard deviation (𝜎) will be used
!    to compute the log-normal parameters µ_ln and 𝜎_ln :
!    µ_ln = ln(µ²/√(µ² + 𝜎²)) and 𝜎_ln = √(ln(1 + (𝜎/µ)²)))
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_PSD_STDEV(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Maximum particle size of particle size distribution of solids phase
!    in the BC region.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_PSD_MAX_DP(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Minimum particle size of particle size distribution of solids phase
!    in the BC region.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_PSD_MIN_DP(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Mass inlet starting time (sec.).
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_MI_START_TIME(LC) = ZERO
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Mass inlet ending time (sec.).
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_MI_END_TIME(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Gas phase Hw for partial slip boundary.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_HW_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Solids phase Hw for partial slip boundary.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_HW_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Gas phase Uw for partial slip boundary.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_UW_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Solids phase Uw for partial slip boundary.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_UW_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Gas phase Vw for partial slip boundary.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_VW_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Solids phase Vw for partial slip boundary.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_VW_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Gas phase Ww for partial slip boundary.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_WW_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Solids phase Ww for partial slip boundary.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_WW_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="INTEGER" category="Boundary Condition" required="false" locked="false">
!  <description>
!   Johnson and Jackson partial slip BC.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <valid value='0'
!    note='Do not use Johnson and Jackson partial slip bc. Default
!      if granular energy transport equation is not solved.'/>
!  <valid value='1'
!    note='Use Johnson and Jackson partial slip bc. Default if
!      granular energy transport equation is solved.'/>
         BC_JJ_PS(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="LOGICAL" category="Boundary Condition" required="false" locked="false">
!  <description>Use a modified version of Johnson and Jackson
!   partial slip BC (BC_JJ_PS BC) with a variable specularity
!   coefficient.
!  </description>
!  <dependent keyword="E_w" value="DEFINED"/>
!  <dependent keyword="PHI_w" value="DEFINED"/>
!  <conflict keyword="JENKINS" value=".TRUE."/>
         BC_JJ_M = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Two Fluid Model" required="false" locked="false">
!  <description>
!    This flag affects how the momentum and granular energy boundary
!    conditions are implemented when using BC_JJ_PS BC.
!  </description>
!  <dependent keyword="PHI_w" value="DEFINED"/>
!  <dependent keyword="E_w" value="DEFINED"/>
!  <conflict keyword="BC_JJ_M" value=".TRUE."/>
!  <valid value=".FALSE." note="Use standard boundary conditions."/>
!  <valid value=".TRUE."
!    note="Use Jenkins small frictional boundary condition."/>
         JENKINS = .FALSE.
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Specified wall value, THETAw_M, in diffusion boundary condition:
!    d(THETA_M)/dn + Hw (THETA_M - THETAw_M) = C, where n is the fluid-to-wall normal.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_THETAW_M(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Transfer coefficient, Hw, in diffusion boundary condition:
!    d(THETA_M)/dn + Hw (THETA_M - THETAw_M) = C, where n is the fluid-to-wall normal.
!  </description>
!  <description>Hw for granular energy bc.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_HW_THETA_M(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Specified constant flux, C, in diffusion boundary condition:
!    d(THETA_M)/dn + Hw (THETA_M - THETAw_M) = C, where n is the fluid-to-wall normal.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_C_THETA_M(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Gas phase heat transfer coefficient, Hw, in diffusion boundary condition:
!    d(T_g)/dn + Hw (T_g - Tw_g) = C, where n is the fluid-to-wall normal.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_HW_T_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Specified gas phase wall temperature, Tw_g, in diffusion boundary condition:
!    d(T_g)/dn + Hw (T_g - Tw_g) = C, where n is the fluid-to-wall normal.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_TW_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Specified constant gas phase heat flux, C, in diffusion boundary condition:
!    d(T_g)/dn + Hw (T_g - Tw_g) = C, where n is the fluid-to-wall normal.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_C_T_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Solids phase heat transfer coefficient, Hw, in diffusion boundary condition:
!    d(T_s)/dn + Hw (T_s - Tw_s) = C, where n is the fluid-to-wall normal.
!  </description>
!  <description>Solids phase Hw for heat transfer.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_HW_T_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Specified solids phase wall temperature, Tw_s, in diffusion boundary condition:
!    d(T_s)/dn + Hw (T_s - Tw_s) = C, where n is the fluid-to-wall normal.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_TW_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Specified constant solids phase heat flux, C, in diffusion boundary condition:
!    d(T_s)/dn + Hw (T_s - Tw_s) = C, where n is the fluid-to-wall normal.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_C_T_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Gas phase species mass transfer coefficient, Hw, in diffusion boundary condition:
!    d(X_g)/dn + Hw (X_g - Xw_g) = C, where n is the fluid-to-wall normal.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Species" min="1" max="DIM_N_G"/>
         BC_HW_X_G(LC,:DIM_N_G) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Specified wall gas species mass fraction, Xw, in diffusion boundary condition:
!    d(X_g)/dn + Hw (X_g - Xw_g) = C, where n is the fluid-to-wall normal.
!  </description>
!  <description>Gas phase Xw for mass transfer.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Species" min="1" max="DIM_N_G"/>
         BC_XW_G(LC,:DIM_N_G) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Specified constant gas species mass flux, C, in diffusion boundary condition:
!    d(X_g)/dn + Hw (X_g - Xw_g) = C, where n is the fluid-to-wall normal.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Species" min="1" max="DIM_N_G"/>
         BC_C_X_G(LC,:DIM_N_G) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Solid phase species mass transfer coefficient, Hw, in diffusion boundary condition:
!    d(X_s)/dn + Hw (X_s - Xw_s) = C, where n is the fluid-to-wall normal.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <arg index="3" id="Species" min="1" max="DIM_N_S"/>
         BC_HW_X_S(LC,:DIM_M,:DIM_N_S) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Specified solids species mass fraction at the wall, Xw_s, in diffusion boundary condition:
!    d(X_s)/dn + Hw (X_s - Xw_s) = C, where n is the fluid-to-wall normal.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <arg index="3" id="Species" min="1" max="DIM_N_S"/>
         BC_XW_S(LC,:DIM_M,:DIM_N_S) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Specified constant solids species mass flux, C, in diffusion boundary condition:
!    d(X_s)/dn + Hw (X_s - Xw_s) = C, where n is the fluid-to-wall normal.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <arg index="3" id="Species" min="1" max="DIM_N_S"/>
         BC_C_X_S(LC,:DIM_M,:DIM_N_S) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Scalar transfer coefficient, Hw, in diffusion boundary condition:
!    d(Scalar)/dn + Hw (Scalar - ScalarW) = C, where n is the fluid-to-wall normal.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Scalar Eq." min="1" max="DIM_SCALAR"/>
         BC_HW_Scalar(LC,:DIM_SCALAR) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Specified scalar value at the wall, ScalarW, in diffusion boundary condition:
!    d(Scalar)/dn + Hw (Scalar - ScalarW) = C, where n is the fluid-to-wall normal.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Scalar Eq." min="1" max="DIM_SCALAR"/>
         BC_ScalarW(LC,:DIM_SCALAR) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>
!    Specified constant scalar flux, C, in diffusion boundary condition:
!    d(Scalar)/dn + Hw (Scalar - ScalarW) = C, where n is the fluid-to-wall normal.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Scalar Eq." min="1" max="DIM_SCALAR"/>
         BC_C_Scalar(LC,:DIM_SCALAR) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Void fraction at the BC plane.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_EP_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Gas pressure at the BC plane.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_P_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Bulk density of solids phase at the BC plane.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_ROP_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Solids volume fraction at the BC plane.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_EP_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Gas phase temperature at the BC plane.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_T_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Solids phase temperature at the BC plane.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_T_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Solids phase granular temperature at the BC plane.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_THETA_M(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Mass fraction of gas species at the BC plane.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Species" min="1" max="DIM_N_G"/>
         BC_X_G(LC,:DIM_N_G) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Mass fraction of solids species at the BC plane.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <arg index="3" id="Species" min="1" max="DIM_N_S"/>
         BC_X_S(LC,:DIM_M,:DIM_N_S) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>X-component of gas velocity at the BC plane.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_U_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>X-component of solids-phase velocity at the BC plane.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_U_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Y-component of gas velocity at the BC plane.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_V_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Y-component of solids-phase velocity at the BC plane.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_V_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Z-component of gas velocity at the BC plane.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_W_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Z-component of solids-phase velocity at the BC plane.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_W_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Gas volumetric flow rate through the boundary.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_VOLFLOW_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Solids volumetric flow rate through the boundary.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_VOLFLOW_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Gas mass flow rate through the boundary.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_MASSFLOW_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Solids mass flow rate through the boundary.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         BC_MASSFLOW_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>The interval at the beginning when the normal
!    velocity at the boundary is equal to BC_Jet_g0. When restarting
!    run, this value and BC_Jet_g0 should be specified such that the
!    transient jet continues correctly. MFIX does not store the jet
!    conditions. For MASS_OUTFLOW boundary conditions, BC_DT_0 is
!    the time period to average and print the outflow rates. The
!    adjustment of velocities to get a specified mass or volumetric
!    flow rate is based on the average outflow rate.
!  </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_DT_0(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Value of normal velocity during the initial time interval BC_DT_0.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_JET_G0(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>The time interval when normal velocity is equal to BC_Jet_gh.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_DT_H(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Value of normal velocity during the interval BC_DT_h.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_JET_GH(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>The interval when normal velocity is equal to BC_JET_gL.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_DT_L(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Value of normal velocity during the interval BC_DT_L.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
         BC_JET_GL(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Boundary value for user-defined scalar equation.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <arg index="2" id="Scalar Eq." min="1" max="DIM_SCALAR"/>
         BC_Scalar(LC,:DIM_SCALAR) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Boundary value of K for K-Epsilon Equation.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <range min="0" max="" />
         BC_K_Turb_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="false">
!  <description>Boundary value of Epsilon for K-Epsilon Equation.</description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_BC"/>
!  <range min="0" max="" />
         BC_E_Turb_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Boundary Condition" required="false" locked="true">
!  <description>Flag to specify the constant statistical
! weight for inflowing computational particles/parcels. Actual number of
! parcels will be automatically computed. </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_IC"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
          BC_PIC_MI_CONST_STATWT(LC, :DIM_M) = ZERO
!</keyword>

!<keyword dtype="LOGICAL" category="Boundary Condition" required="false" locked="true">
!  <description>Flag to make the PO BC invisible to discrete solids.
! Set this flag to .FALSE. to remove this BC for discrete solids. </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_IC"/>
         BC_PO_APPLY_TO_DES(LC) = .TRUE.
!</keyword>

!<keyword dtype="LOGICAL" category="Boundary Condition" required="false" locked="true">
!  <description>Flag to make the PO BC invisible to discrete solids.
! Set this flag to .FALSE. to remove this BC for discrete solids. </description>
!  <arg index="1" id="BC" min="1" max="DIMENSION_IC"/>
         BC_MI_APPLY_TO_DES(LC) = .TRUE.
!</keyword>

         BC_ROP_G(LC) = UNDEFINED
      ENDDO




!#####################################################################!
!                         Internal Surfaces                           !
!#####################################################################!
      DO LC = 1, DIMENSION_IS


!<keyword dtype="REAL" category="Internal Surface" required="false" locked="true">
!  <description>X coordinate of the west face or edge.</description>
!  <arg index="1" id="IS" min="1" max="DIMENSION_IS"/>
         IS_X_W(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Internal Surface" required="false" locked="true">
!  <description>X coordinate of the east face or edge.</description>
!  <arg index="1" id="IS" min="1" max="DIMENSION_IS"/>
         IS_X_E(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Internal Surface" required="false" locked="true">
!  <description>Y coordinate of the south face or edge.</description>
!  <arg index="1" id="IS" min="1" max="DIMENSION_IS"/>
         IS_Y_S(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Internal Surface" required="false" locked="true">
!  <description>Y coordinate of the north face or edge.</description>
!  <arg index="1" id="IS" min="1" max="DIMENSION_IS"/>
         IS_Y_N(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Internal Surface" required="false" locked="true">
!  <description>Z coordinate of the bottom face or edge.</description>
!  <arg index="1" id="IS" min="1" max="DIMENSION_IS"/>
         IS_Z_B(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Internal Surface" required="false" locked="true">
!  <description>Z coordinate of the top face or edge.</description>
!  <arg index="1" id="IS" min="1" max="DIMENSION_IS"/>
         IS_Z_T(LC) = UNDEFINED
!</keyword>

!<keyword dtype="INTEGER" category="Internal Surface" required="false" locked="true">
!  <description>I index of the west-most cell.</description>
!  <arg index="1" id="IS" min="1" max="DIMENSION_IS"/>
         IS_I_W(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Internal Surface" required="false" locked="true">
!  <description>I index of the east-most cell</description>
!  <arg index="1" id="IS" min="1" max="DIMENSION_IS"/>
         IS_I_E(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Internal Surface" required="false" locked="true">
!  <description>J index of the south-most cell</description>
!  <arg index="1" id="IS" min="1" max="DIMENSION_IS"/>
         IS_J_S(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Internal Surface" required="false" locked="true">
!  <description>J index of the north-most cell</description>
!  <arg index="1" id="IS" min="1" max="DIMENSION_IS"/>
         IS_J_N(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Internal Surface" required="false" locked="true">
!  <description>K index of the bottom-most cell</description>
!  <arg index="1" id="IS" min="1" max="DIMENSION_IS"/>
         IS_K_B(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Internal Surface" required="false" locked="true">
!  <description>K index of the top-most cell</description>
!  <arg index="1" id="IS" min="1" max="DIMENSION_IS"/>
         IS_K_T(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="CHARACTER" category="Internal Surface" required="false" locked="true">
!  <description>Type of internal surface</description>
!  <arg index="1" id="IS" min="1" max="DIMENSION_IS"/>
!  <valid value="IMPERMEABLE"
!    note="No gas or solids flow through the surface." alias="IP"/>
!  <valid value="SEMIPERMEABLE" alias='SP'
!    note="Gas flows through the surface with an additional resistance.
!      Solids velocity through the surface is set to zero or to a user-
!      specified fixed value (i.e., solids momentum equation for this
!      direction is not solved)." />
!  <valid value="X_IMPERMEABLE"
!    note="No gas or solids flow in the X direction." alias="IP"/>
!  <valid value="X_SEMIPERMEABLE" alias='SP'
!    note="Gas flows in the X direction with additional resistance.
!      Solids velocity in the X direction is set to zero or to a user-
!      specified fixed value (i.e., solids momentum equation for this
!      direction is not solved)." />
!  <valid value="Y_IMPERMEABLE"
!    note="No gas or solids flow in the Y direction." alias="IP"/>
!  <valid value="Y_SEMIPERMEABLE" alias='SP'
!    note="Gas flows in the Y direction with additional resistance.
!      Solids velocity in the Y direction is set to zero or to a user-
!      specified fixed value (i.e., solids momentum equation for this
!      direction is not solved)." />
!  <valid value="Z_IMPERMEABLE"
!    note="No gas or solids flow in the Z direction." alias="IP"/>
!  <valid value="X_SEMIPERMEABLE" alias='SP'
!    note="Gas flows in the Z direction with additional resistance.
!      Solids velocity in the Z direction is set to zero or to a user-
!      specified fixed value (i.e., solids momentum equation for this
!      direction is not solved)." />
!  <valid value="STL"
!    note="Arbitrary shaped internal surface defined by an STL file.
!      Only visible to DEM particles or PIC parcels. The fluid phase
!      doesn't feel this stl internal surface." alias="IP"/>
         IS_TYPE(LC) = UNDEFINED_C
!</keyword>

!<keyword dtype="REAL" category="Internal Surface" required="false" locked="false">
!  <description>
!    Parameters defining the internal surface. These values need to be
!    specified for semipermeable surfaces only. The thickness used for
!    pressure drop computation is that of the momentum cell (DX_e,
!    DY_n, or DZ_t). To turn off the resistance, use a large value
!    for permeability.
!
!    - IDX=1: Permeability [1.0E32]
!    - IDX=2: Inertial resistance coefficient [0.0]
!  </description>
!  <arg index="1" id="IS" min="1" max="DIMENSION_IS"/>
!  <arg index="2" id="IDX" min="1" max="2"/>
         IS_PC(LC,1) = UNDEFINED
         IS_PC(LC,2) = ZERO
!</keyword>

!<keyword dtype="REAL" category="Internal Surface" required="false" locked="false">
!  <description>Value of fixed solids velocity through semipermeable surfaces.</description>
!  <arg index="1" id="IS" min="1" max="DIMENSION_IS"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         IS_VEL_S(LC,:DIM_M) = ZERO
!</keyword>
      ENDDO


!#####################################################################!
!                     Point Source Mass Inlets                        !
!#####################################################################!
      DO LC = 1, DIMENSION_PS

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>X coordinate of the west face or edge.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_X_W(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>X coordinate of the east face or edge.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_X_E(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>Y coordinate of the south face or edge.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_Y_S(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>Y coordinate of the north face or edge.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_Y_N(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>Z coordinate of the bottom face or edge.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_Z_B(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>Z coordinate of the top face or edge.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_Z_T(LC) = UNDEFINED
!</keyword>

!<keyword dtype="INTEGER" category="Point Source" required="false" locked="false">
!  <description>I index of the west-most cell.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_I_W(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Point Source" required="false" locked="false">
!  <description>I index of the east-most cell.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_I_E(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Point Source" required="false" locked="false">
!  <description>J index of the south-most cell.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_J_S(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Point Source" required="false" locked="false">
!  <description>J index of the north-most cell.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_J_N(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Point Source" required="false" locked="false">
!  <description>K index of the bottom-most cell.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_K_B(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Point Source" required="false" locked="false">
!  <description>K index of the top-most cell.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_K_T(LC) = UNDEFINED_I
!</keyword>


!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>X-component of incoming gas velocity.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_U_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>Y-component of incoming gas velocity.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_V_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>Z-component of incoming gas velocity.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_W_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>Gas mass flow rate through the point source.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_MASSFLOW_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>Temperature of incoming gas.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
         PS_T_G(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>Gas phase incoming species n mass fraction.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
!  <arg index="2" id="Species" min="1" max="DIM_N_G"/>
         PS_X_G(LC,:DIM_N_g) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>X-component of incoming solids velocity.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         PS_U_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>Y-component of incoming solids velocity.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         PS_V_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>Z-component of incoming solids velocity.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         PS_W_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>Solids mass flow rate through the point source.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         PS_MASSFLOW_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>Temperature of incoming solids.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
         PS_T_S(LC,:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Point Source" required="false" locked="false">
!  <description>Solids phase incoming species n mass fraction.</description>
!  <arg index="1" id="PS" min="1" max="DIMENSION_PS"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <arg index="3" id="Species" min="1" max="DIM_N_S"/>
         PS_X_S(LC,:DIM_M,:DIM_N_S) = UNDEFINED
!</keyword>

      ENDDO


!#####################################################################!
!                          Output Control                             !
!#####################################################################!

!<keyword dtype="REAL" category="Output Control" required="true" locked="false">
!  <description>
!    Time interval at which restart (.res) file is updated.
!  </description>
      RES_DT = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" locked="false">
!  <description>
!    Time interval at which a backup copy of the restart file is created.
!  </description>
      RES_BACKUP_DT = UNDEFINED
!</keyword>

!<keyword dtype="INTEGER" category="Output Control" required="false" locked="true">
!  <description>
!    The number of backup restart files to retain.
!  </description>
      RES_BACKUPS = UNDEFINED_I
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" locked="true">
!  <description>
!    Time interval at which .SPX files are updated.
!
!    - SP1: void fraction (EP_G)
!    - SP2: Gas pressure (P_G) and Solids pressure (P_star)
!    - SP3: Gas velocity (U_G, V_G, W_G)
!    - SP4: Solids velocity (U_S, V_S, W_S)
!    - SP5: Solids bulk density (ROP_s)
!    - SP6: Gas and solids temperature (T_G, T_S)
!    - SP7: Gas and solids mass fractions (X_G, X_S)
!    - SP8: Granular temperature (THETA_M)
!    - SP9: User defined scalars (SCALAR)
!    - SPA: Reaction Rates (ReactionRates)
!    - SPB: Turbulence quantities (K_TURB_G, E_TURB_G)
!  </description>
!  <arg index="1" id="SP Value" min="1" max="N_SPX"/>
      SPX_DT(:N_SPX) = UNDEFINED
!</keyword>

!<keyword dtype="INTEGER" category="Output Control" required="false" locked="true">
!  <description>
!    The number of user defined chemical reactions stored
!    in the *.SPA and monitor files.
!  </description>
      nRR = 0
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" locked="false">
!  <description> Time interval at which standard output (.OUT) file is updated.
!    Only run configuration information is written if left undefined. Otherwise
!    all field variables for the entire domain are written in ASCII
!    format to the .OUT file at OUT_DT intervals.
!  </description>
      OUT_DT = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="true" locked="false">
!  <description>
!    Option to report solid inventory.
!  </description>
      REPORT_SOLID_INVENTORY = .FALSE.
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="true" locked="false">
!  <description>
!    Interval at which solid inventory is reported (seconds).
!  </description>
!  <dependent keyword="REPORT_SOLID_INVENTORY" value=".TRUE."/>
      REPORT_SOLID_INVENTORY_DT = 0.1
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="true" locked="false">
!  <description>
!    Option to breakdown solid inventory by phase (if more than one solids phase).
!  </description>
!  <dependent keyword="REPORT_SOLID_INVENTORY" value=".TRUE."/>
      BREAKDOWN_SOLID_INVENTORY_BY_PHASE = .FALSE.
!</keyword>

!<keyword dtype="INTEGER" category="Output Control" required="false" locked="false">
!  <description>Number of time steps between .LOG file updates.</description>
      NLOG = 25
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description> Display the residuals on the screen and provide
!    messages about convergence on the screen and in the .LOG file.
!  </description>
      FULL_LOG = .FALSE.
!</keyword>

!<keyword dtype="CHARACTER" category="Output Control" required="false" locked="false">
!  <description>Specifies the residuals to display. </description>
!  <arg index="1" id="Residual Index" max="8" min="1"/>
!  <valid value="P0" note="Gas pressure"/>
!  <valid value="PM" note="Solids phase M pressure"/>
!  <valid value="R0" note="Gas density"/>
!  <valid value="RM" note="Solids phase M density"/>
!  <valid value="U0" note="Gas phase U-velocity"/>
!  <valid value="V0" note="Gas phase V-velocity"/>
!  <valid value="W0" note="Gas phase W-velocity"/>
!  <valid value="UM" note="Solids phase M U-velocity"/>
!  <valid value="VM" note="Solids phase M V-velocity"/>
!  <valid value="WM" note="Solids phase M W-velocity"/>
!  <valid value="T0" note="Gas temperature"/>
!  <valid value="TM" note="Solids phase M temperature"/>
!  <valid value="GM" note="Solids phase M granular temperature"/>
!  <valid value="X0NN" note="Gas phase species NN mass fraction"/>
!  <valid value="XMNN" note="Solids phase M species NN mass fraction"/>
!  <valid value="K0" note="K-Epsilon model residuals"/>
      RESID_STRING(:8) = UNDEFINED_C
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Display residuals by equation.  </description>
      GROUP_RESID = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>
!    Provide detailed logging of negative density errors.
!  </description>
!  <valid value=".FALSE." note="Do not log negative density errors."/>
!  <valid value=".TRUE." note="Log negative density errors."/>
      REPORT_NEG_DENSITY = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>
!    Provide detailed logging of zero or negative specific heat errors.
!  </description>
!  <valid value=".FALSE." note="Do not log zero or negative specific heat errors."/>
!  <valid value=".TRUE." note="Log zero or negative specific heat errors."/>
      REPORT_NEG_SPECIFICHEAT = .FALSE.
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" locked="false">
!  <description>
!    Frequency to perform an overall species mass balance. Leaving
!    undefined suppresses the mass balance calculations which can
!    slightly extend run time.
!  </description>
      REPORT_MASS_BALANCE_DT = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="true">
!  <description>
!    Output the variable specularity coefficient when BC_JJ_M is
!    .TRUE.. The specularity coefficient will be stored in ReactionRates
!    array for post-processing by post_mfix. User needs to set NRR to 1
!    for this purpose. Be careful with this setting when reacting flow
!    is simulated.
!  </description>
      PHIP_OUT_JJ=.FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="true">
!  <description>
!    Use distributed IO: Each MPI process generates RES/SPx/VTU/VTP files.
!  </description>
      bDist_IO = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="true">
!  <description>
!    Restart a serial IO run (only one RES file was created) with
!    distributed IO.
!  </description>
!  <dependent keyword="RUN_TYPE" value="RESTART_2"/>
!  <dependent keyword="bDist_IO" value=".TRUE."/>
      bStart_with_one_RES = .FALSE.
!</keyword>


!<keyword  dtype="REAL" category="Output Control" required="false" locked="false">
!  <description>X coordinate of the monitor region west face.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_X_W(:) = -UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Output Control" required="false" locked="false">
!  <description>X coordinate of the monitor region east face.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_X_E(:) = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Output Control" required="false" locked="false">
!  <description>Y coordinate of the monitor region south face.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_Y_S(:) = -UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Output Control" required="false" locked="false">
!  <description>Y coordinate of the monitor region north face.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_Y_N(:) = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Output Control" required="false" locked="false">
!  <description>Z coordinate of the monitor region bottom face.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_Z_B(:) = -UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Output Control" required="false" locked="false">
!  <description>Z coordinate of the monitor region top face.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_Z_T(:) = UNDEFINED
!</keyword>


!<keyword dtype="CHARACTER" category="Output Control" required="false" locked="false">
!  <description>Monitor region output file name base.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_NAME(:) = UNDEFINED_C
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" locked="false">
!  <description>
!     Interval to collect monitor data.
!  </description>
!     <arg index="1" id="Monitor" max="DIMENSION_MONITOR" min="1"/>
      MONITOR_DT(:) = UNDEFINED
!</keyword>

!<keyword dtype="INTEGER" category="Output Control" required="false" locked="false">
!  <description>Data monitor type. </description>
!  <arg index="1" id="Monitor" max="DIMENSION_MONITOR" min="1"/>
!  <valid value="0"  note="Point value"/>
!  <valid value="1"  note="Sum over region"/>
!  <valid value="2"  note="Minimum over region"/>
!  <valid value="3"  note="Maximum over region"/>
!  <valid value="4"  note="Arithmetic average over region"/>
!  <valid value="5"  note="Standard deviation over region"/>
!  <valid value="6"  note="Area-weighted average over surface"/>
!  <valid value="7"  note="Flow rate across surface"/>
!  <valid value="8"  note="Mass flow rate across surface"/>
!  <valid value="9"  note="Mass-weighted average over surface"/>
!  <valid value="10" note="Volumetric flow rate over surface"/>
!  <valid value="11" note="Volume integral"/>
!  <valid value="12" note="Volume-weighted average"/>
!  <valid value="13" note="Mass-weighted volume integral"/>
!  <valid value="14" note="Mass-weighted volume average"/>
!  <valid value="101" note="Sum over region"/>
!  <valid value="102" note="Minimum over region"/>
!  <valid value="103" note="Maximum over region"/>
!  <valid value="104" note="Arithmetic average over region"/>
!  <valid value="105" note="Standard deviation over region"/>
!  <valid value="106" note="Mass-weighted average over region"/>
!  <valid value="107" note="Volume-weighted average over region"/>
!  <valid value="108" note="Flow rate across surface"/>
!  <valid value="109" note="Mass-weighted flow rate across surface"/>
!  <valid value="110" note="Volume-weighted flow rate across surface"/>
      MONITOR_TYPE(:) = UNDEFINED_I
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write the void fraction in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_EP_g(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write the gas density in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_RO_g(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write the gas pressure in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_P_g(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>
!    Write x-component of gas velocity vector in region.
!  </description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_U_g(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>
!    Write y-component of gas velocity vector in region.
!  </description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_V_g(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>
!    Write z-component of gas velocity vector in region.
!  </description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_W_g(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write gas temperature in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_T_g(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write gas mixture molecular weight in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_MW_MIX_g(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write gas phase species mass fraction in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="Species" min="1" max="DIM_N_G"/>
      MONITOR_X_g(:,:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write gas phase species molar fraction in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="Species" min="1" max="DIM_N_G"/>
      MONITOR_Y_g(:,:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write turbulent kinetic energy in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_K_Turb_G(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write turbulent dissipation rate in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_E_Turb_G(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>
!     Write x-component of solids velocity vector in region.
!  </description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
      MONITOR_U_s(:,:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>
!    Write y-component of solids velocity vector in region.
!  </description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
      MONITOR_V_s(:,:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>
!    Write z-component of solids velocity vector in region.
!  </description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
      MONITOR_W_s(:,:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write solids bulk density in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
      MONITOR_ROP_s(:,:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write solids material density in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
      MONITOR_RO_s(:,:) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write the solids pressure preventing overpacking in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_P_star(:) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write the solids pressure as a result of granular motion in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
      MONITOR_P_s(:,DIM_M) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write solids temperature in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
      MONITOR_T_s(:,:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write solids phase species mass fraction in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
!  <arg index="3" id="Species" min="1" max="DIM_N_S"/>
      MONITOR_X_s(:,:,:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write granular temperature in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
      MONITOR_Theta_m(:,:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write scalar in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="Scalar Eq." min="1" max="DIM_SCALAR"/>
      MONITOR_Scalar(:,:) =.FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write reaction rate in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="RATE" min="1" max="NRRMAX"/>
      MONITOR_RRate(:,:) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write fluid reaction rates in region. This keyword replaces
!  MONITOR_RRate and does not require users to write code to save data in
!  ReactionRates(:,:) array.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="RATE" min="1" max="NRRMAX"/>
      MONITOR_fluid_RRate(:,:) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write particle/parcel reaction rates in region. Particle/parcel data
!  is converted to cell data.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="RATE" min="1" max="NRRMAX"/>
      MONITOR_des_RRate(:,:) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>
!      Option to save or not save particles belonging to solids phases in region.
!  </description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="Phase" min="1" max="DIM_M"/>
      MONITOR_part_phase(:,:) = .TRUE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write particle radius in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_radius(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write particle mass in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_pmass(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write particle volume in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_pvol(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write particle density in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_ro_p(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write particle x-axis translational velocity in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_vel_x(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write particle y-axis translational velocity in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_vel_y(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write particle z-axis translational velocity in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_vel_z(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write particle x-axis rotational velocity in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_rot_x(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write particle y-axis rotational velocity in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_rot_y(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write particle z-axis rotational velocity in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_rot_z(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write particle temperature in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_t_p(:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write particle species mass fraction in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="Species" min="1" max="DIM_N_S"/>
      MONITOR_x_p(:,:) = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write des_usr_var in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="USR_VAR" min="1" max="100"/>
      MONITOR_des_usr_var(:,:) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write particle/parcel reaction rates in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
!  <arg index="2" id="rate" min="1" max="100"/>
      MONITOR_part_rrate(:,:) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" locked="false">
!  <description>Write particle/parcel residence time in region.</description>
!  <arg index="1" id="Monitor" min="1" max="DIMENSION_MONITOR"/>
      MONITOR_part_residence_time(:) = .FALSE.
!</keyword>

!#####################################################################!
!                           UDF  Control                              !
!#####################################################################!

!<keyword dtype="LOGICAL" category="UDF Control" required="false" locked="false">
!  <description>
!    Flag to enable user-defined subroutines: USR0, USR1, USR2, USR3,
!    USR0_DES, USR1_DES, USR2_DES, USR3_DES, USR4_DES.
!  </description>
!  <valid value=".TRUE." note="Call user-defined subroutines."/>
!  <valid value=".FALSE." note="Do NOT call user-defined subroutines."/>
      CALL_USR = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="UDF Control" required="false" locked="false">
!  <description>
!    Flag to enable user_defined subroutine, usr_source, for
!    calculating source terms in the indicated equation.
!  </description>
!  <arg index="1" id="Equation ID Number" min="1" max="DIM_EQS"/>
!  <valid value=".TRUE." note="Call user-defined source."/>
!  <valid value=".FALSE." note="MFIX default: No additional source."/>
      CALL_USR_SOURCE(:) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="UDF Control" required="false" locked="false">
!  <description>
!    Flag to use the User Defined Function, USR_PROP_ROg,
!    in model/usr_prop.f for calculating the gas phase
!    density, RO_g.
!  </description>
!  <valid value=".TRUE." note="Call user-defined function."/>
!  <valid value=".FALSE." note="Use MFIX default calculation."/>
      USR_ROg = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="UDF Control" required="false" locked="false">
!  <description>
!    Flag to use the User Defined Function, USR_PROP_CPg,
!    in model/usr_prop.f for calculating the gas phase
!    constant pressure specific heat, C_pg.
!  </description>
!  <valid value=".TRUE." note="Call user-defined function."/>
!  <valid value=".FALSE." note="Use MFIX default calculation."/>
      USR_CPg = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="UDF Control" required="false" locked="false">
!  <description>
!    Flag to use the User Defined Function, USR_PROP_Kg,
!    in model/usr_prop.f for calculating the gas phase
!    conductivity, K_g.
!  </description>
!  <valid value=".TRUE." note="Call user-defined function."/>
!  <valid value=".FALSE." note="Use MFIX default calculation."/>
      USR_Kg = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="UDF Control" required="false" locked="false">
!  <description>
!    Flag to use the User Defined Function, USR_PROP_Difg,
!    in model/usr_prop.f for calculating the gas phase
!    diffusivity.
!  </description>
!  <valid value=".TRUE." note="Call user-defined function."/>
!  <valid value=".FALSE." note="Use MFIX default calculation."/>
      USR_Difg = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="UDF Control" required="false" locked="false">
!  <description>
!    Flag to use the User Defined Function, USR_PROP_Mug,
!    in model/usr_prop.f for calculating the gas phase
!    viscosity, Mu_g.
!  </description>
!  <valid value=".TRUE." note="Call user-defined function."/>
!  <valid value=".FALSE." note="Use MFIX default calculation."/>
      USR_Mug = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="UDF Control" required="false" tfm="true" locked="false">
!  <description>
!    Flag to use the User Defined Function, USR_PROP_ROs,
!    in model/usr_prop.f for calculating the solids phase
!    density, RO_s.
!  </description>
!  <valid value=".TRUE." note="Call user-defined function."/>
!  <valid value=".FALSE." note="Use MFIX default calculation."/>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      USR_ROs(:DIM_M) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="UDF Control" required="false" tfm="true" locked="false">
!  <description>
!    Flag to use the User Defined Function, USR_PROP_CPs,
!    in model/usr_prop.f for calculating the solids phase
!    constant pressure specific heat, C_ps.
!  </description>
!  <valid value=".TRUE." note="Call user-defined function."/>
!  <valid value=".FALSE." note="Use MFIX default calculation."/>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      USR_CPs(:DIM_M) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="UDF Control" required="false" tfm="true" locked="false">
!  <description>
!    Flag to use the User Defined Function, USR_PROP_Ks,
!    in model/usr_prop.f for calculating the solids phase
!    conductivity, K_s.
!  </description>
!  <valid value=".TRUE." note="Call user-defined function."/>
!  <valid value=".FALSE." note="Use MFIX default calculation."/>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      USR_Ks(:DIM_M) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="UDF Control" required="false" tfm="true" locked="false">
!  <description>
!    Flag to use the User Defined Function, USR_PROP_Difs,
!    in model/usr_prop.f for calculating the solids phase
!    diffusivity, Dif_s.
!  </description>
!  <valid value=".TRUE." note="Call user-defined function."/>
!  <valid value=".FALSE." note="Use MFIX default calculation."/>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      USR_Difs(:DIM_M) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="UDF Control" required="false" tfm="true" locked="false">
!  <description>
!    Flag to use the User Defined Function, USR_PROP_Mus,
!    in model/usr_prop.f for calculating the solids phase
!    viscosity, Mu_s; second viscosity, lambda_s; and pressure,
!    P_s.
!  </description>
!  <valid value=".TRUE." note="Call user-defined function."/>
!  <valid value=".FALSE." note="Use MFIX default calculation."/>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      USR_Mus(:DIM_M) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="UDF Control" required="false" tfm="true" locked="false">
!  <description>
!    Flag to use the User Defined Function, USR_PROP_Gama,
!    in model/usr_prop.f for calculating the gas-solids phase
!    heat transfer coefficient, Gama_gs.
!  </description>
!  <valid value=".TRUE." note="Call user-defined function."/>
!  <valid value=".FALSE." note="Use MFIX default calculation."/>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      USR_Gama(:DIM_M) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="UDF Control" required="false" tfm="true" locked="false">
!  <description>
!    Flag to use the User Defined Function, USR_PROP_Fgs, in
!    model/usr_prop.f for calculating the gas-solids phase drag
!    coefficient due to relative velocity differences, F_gs.
!    Currently unavailable.
!  </description>
!  <valid value=".TRUE." note="Call user-defined function."/>
!  <valid value=".FALSE." note="Use MFIX default calculation."/>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      USR_Fgs(:DIM_M) = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="UDF Control" required="false" tfm="true" locked="false">
!  <description>
!    Flag to use the User Defined Function, USR_PROP_Fss, in
!    model/usr_prop.f for calculating the solids-solids phase
!    drag coefficient due to relative velocity differences, F_ss.
!    Currently unavailable.
!  </description>
!  <valid value=".TRUE." note="Call user-defined function."/>
!  <valid value=".FALSE." note="Use MFIX default calculation."/>
!  <arg index="1" id="Phase" min="1" max="DIM_LM"/>
      USR_Fss( :((DIM_M*(DIM_M-1)/2)+1) ) = .FALSE.
!</keyword>

!<keyword dtype="REAL" category="UDF Control" required="false" locked="false">
!  <description>User defined constants.</description>
!  <arg index="1" id="User-defined ID" min="1" max="DIMENSION_C"/>
      C(1:DIMENSION_C) = UNDEFINED
!</keyword>

!<keyword dtype="CHARACTER" category="UDF Control" required="false" locked="false">
!  <description>Name of user-defined constant. (20 character max)</description>
!  <arg index="1" id="User-defined ID" min="1" max="DIMENSION_C"/>
      C_NAME(1:DIMENSION_C) = '....................'
!</keyword>

      DO LC=1, DIMENSION_USR
!<keyword dtype="REAL" category="UDF Control" required="false" locked="false">
!  <description>
!    Interval at which subroutine write_usr1 is called.
!  </description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_DT(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="UDF Control" required="false" locked="false">
!  <description>UDF hook: x coordinate of the west face or edge.</description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_X_W(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="UDF Control" required="false" locked="false">
!  <description>UDF hook: x coordinate of the east face or edge.</description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_X_E(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="UDF Control" required="false" locked="false">
!  <description>UDF hook: y coordinate of the south face or edge.</description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_Y_S(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="UDF Control" required="false" locked="false">
!  <description>UDF hook: y coordinate of the north face or edge.</description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_Y_N(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="UDF Control" required="false" locked="false">
!  <description>UDF hook: z coordinate of the bottom face or edge.</description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_Z_B(LC) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="UDF Control" required="false" locked="false">
!  <description>UDF hook: z coordinate of the top face or edge.</description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_Z_T(LC) = UNDEFINED
!</keyword>

!<keyword dtype="INTEGER" category="UDF Control" required="false" locked="false">
!  <description>UDF hook: i index of the west-most cell.</description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_I_W(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="UDF Control" required="false" locked="false">
!  <description>UDF hook: i index of the east-most cell.</description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_I_E(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="UDF Control" required="false" locked="false">
!  <description>UDF hook: j index of the south-most cell.</description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_J_S(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="UDF Control" required="false" locked="false">
!  <description>UDF hook: j index of the north-most cell.</description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_J_N(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="UDF Control" required="false" locked="false">
!  <description>UDF hook: k index of the bottom-most cell.</description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_K_B(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="UDF Control" required="false" locked="false">
!  <description>UDF hook: k index of the top-most cell.</description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_K_T(LC) = UNDEFINED_I
!</keyword>

!<keyword dtype="CHARACTER" category="UDF Control" required="false" locked="false">
!  <description>UDF hook: Type of user-defined output: Binary or ASCII.</description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_TYPE(LC) = UNDEFINED_C
!</keyword>

!<keyword dtype="CHARACTER" category="UDF Control" required="false" locked="false">
!  <description>UDF hook:
!    Variable to be written in the user-defined output files.
!  </description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_VAR(LC) = UNDEFINED_C
!</keyword>

!<keyword dtype="CHARACTER" category="UDF Control" required="false" locked="false">
!  <description>UDF hook:
!    Format for writing user-defined (ASCII) output file.
!  </description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_FORMAT(LC) = UNDEFINED_C
!</keyword>

!<keyword dtype="CHARACTER" category="UDF Control" required="false" locked="false">
!  <description>UDF hook: File extension for the user-defined output.</description>
!  <arg index="1" id="USR" max="DIMENSION_USR" min="1"/>
         USR_EXT(LC) = UNDEFINED_C
!</keyword>
      ENDDO


!#####################################################################!
!                        Chemical Reactions                           !
!#####################################################################!


!<keyword dtype="LOGICAL" category="Chemical Reactions" required="false" locked="false">
!  <description>Flag to use stiff chemistry solver (Direct Integration).</description>
!  <conflict keyword="USE_RRATES" value=".TRUE."/>
      STIFF_CHEMISTRY = .FALSE.
!</keyword>

!<keyword dtype="INTEGER" category="Chemical Reactions" required="false" locked="false">
!  <description>
!    Maximum number of internal steps ODEPACK may use to integrate
!    over the time interval. Leaving this value unspecified defaults
!    to 500,000 steps. The stiff solver reports the
!    number of cells that exceed the number of steps as 'incomplete'.
!  </description>
!  <dependent keyword="STIFF_CHEMISTRY" value=".TRUE."/>
!  <conflict keyword="USE_RRATES" value=".TRUE."/>
      STIFF_CHEM_MAX_STEPS = 500000
!</keyword>

!<keyword dtype="LOGICAL" category="Chemical Reactions" required="false" locked="true">
!  <description>Flag to use legacy chemical reaction UDFs.</description>
      USE_RRATES = .FALSE.
!</keyword>

!<keyword dtype="CHARACTER" category="Chemical Reactions" required="false" legacy="true" locked="true">
!  <description>
!    Names of gas and solids phase species as it appears in the
!    materials database. The first NMAX(0) are the names of gas
!    species. The next NMAX(1) are the names of solids phase-1
!    species, etc.
!     </description>
!  <arg index="1" id="Phase" min="0" max="DIM_M"/>
!  <dependent keyword="USE_RRATES" value=".TRUE."/>
      SPECIES_NAME(:DIM_N_ALL) = UNDEFINED_C
!</keyword>

!<keyword dtype="INTEGER" category="Chemical Reactions" required="false" locked="true">
!  <description>
!    Number of species in phase m. Note that the gas phase is indicated
!    as m=0.
!     </description>
!  <arg index="1" id="Phase" min="0" max="DIM_M"/>
!  <dependent keyword="USE_RRATES" value=".TRUE."/>
      NMAX(0:DIM_M) = UNDEFINED_I
!</keyword>


!#####################################################################!
!                    Parallelization Control                          !
!#####################################################################!


!<keyword dtype="INTEGER" category="Parallelization Control" required="false" locked="true">
!  <description>Number of grid blocks in x-direction.</description>
      NODESI = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Parallelization Control" required="false" locked="true">
!  <description>Number of grid blocks in y-direction.</description>
      NODESJ = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Parallelization Control" required="false" locked="true">
!  <description>Number of grid blocks in z-direction.</description>
      NODESK = UNDEFINED_I
!</keyword>

! Dynamic load balance list of partitions to test

!<keyword dtype="INTEGER" category="Parallelization Control" required="false" locked="true">
!  <description>
!     List of grid blocks in x-direction used in Dynamic Load balance (DLB).
!     The DLB will test each partition layout defined by DLB_NODESI, DLB_NODESJ,
!     DLB_NODESK, and choose the one providing the best load balance.
!     For each layout, the product DLB_NODESIxDLB_NODESJxDLB_NODESK must match
!     the number of cores used in the DMP run.
!
!     Example: To test two 80-cores layouts with 4x5x4 and 2x20x2 partitions,
!     define DLB_NODESI(1)=4, DLB_NODESJ(1)=5, DLB_NODESK(1)=4, and
!     DLB_NODESI(2)=2, DLB_NODESJ(2)=20, DLB_NODESK(2)=2.
!  </description>
!  <arg index="1" id="Layout" min="1" max="100"/>
      DLB_NODESI(:) = 0
!</keyword>

!<keyword dtype="INTEGER" category="Parallelization Control" required="false" locked="true">
!  <description>
!     List of grid blocks in y-direction used in Dynamic Load balance (DLB).
!     The DLB will test each partition layout defined by DLB_NODESI, DLB_NODESJ,
!     DLB_NODESK, and choose the one providing the best load balance.
!     For each layout, the product DLB_NODESIxDLB_NODESJxDLB_NODESK must match
!     the number of cores used in the DMP run.
!
!     Example: To test two 80-cores layouts with 4x5x4 and 2x20x2 partitions,
!     define DLB_NODESI(1)=4, DLB_NODESJ(1)=5, DLB_NODESK(1)=4, and
!     DLB_NODESI(2)=2, DLB_NODESJ(2)=20, DLB_NODESK(2)=2.
!  </description>
!  <arg index="1" id="Layout" min="1" max="100"/>
      DLB_NODESJ(:) = 0
!</keyword>

!<keyword dtype="INTEGER" category="Parallelization Control" required="false" locked="true">
!  <description>
!     List of grid blocks in z-direction used in Dynamic Load balance (DLB).
!     The DLB will test each partition layout defined by DLB_NODESI, DLB_NODESJ,
!     DLB_NODESK, and choose the one providing the best load balance.
!     For each layout, the product DLB_NODESIxDLB_NODESJxDLB_NODESK must match
!     the number of cores used in the DMP run.
!
!     Example: To test two 80-cores layouts with 4x5x4 and 2x20x2 partitions,
!     define DLB_NODESI(1)=4, DLB_NODESJ(1)=5, DLB_NODESK(1)=4, and
!     DLB_NODESI(2)=2, DLB_NODESJ(2)=20, DLB_NODESK(2)=2.
!  </description>
!  <arg index="1" id="Layout" min="1" max="100"/>
      DLB_NODESK(:) = 0
!</keyword>


!<keyword dtype="LOGICAL" category="Parallelization Control" required="false" locked="false">
!  <description>Print out additional statistics for parallel runs</description>
      solver_statistics = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Parallelization Control" required="false" locked="false">
!  <description>Group residuals to reduce global collectives.</description>
      DEBUG_RESID = .TRUE.
!</keyword>

!<keyword dtype="LOGICAL" category="Parallelization Control" required="false" locked="true">
!  <description>All ranks write error messages.</description>
      ENABLE_DMP_LOG = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Parallelization Control" required="false" locked="true">
!  <description>Print the index layout for debugging.</description>
      DBGPRN_LAYOUT = .FALSE.
!</keyword>


!#####################################################################!
!                       Batch Queue Environment                       !
!#####################################################################!


!<keyword dtype="LOGICAL" category="Batch Queue Environment" required="false" locked="false">
!  <description>
!    Enables controlled termination feature when running under batch
!    queue system to force MFiX to cleanly terminate before the end
!    of wall clock allocated in the batch session.
!  </description>
      CHK_BATCHQ_END = .FALSE.
!</keyword>

!<keyword dtype="REAL" category="Batch Queue Environment" required="false" locked="false">
!  <description>Total wall-clock duration of the job, in seconds.</description>
      BATCH_WALLCLOCK = 172800.0    ! set to 2.0 days
!</keyword>

!<keyword dtype="REAL" category="Batch Queue Environment" required="false" locked="false">
!  <description>
!    Buffer time specified to allow MFiX to write out the files and
!    cleanly terminate before queue wall clock time limit is reached
!    such that (BATCH_WALLCLOCK-TERM_BUFFER) is less than then batch
!    queue wall clock time limit, in seconds.
!  </description>
      TERM_BUFFER = 180.0         ! set to 3 minutes prior to end of job
!</keyword>



!#####################################################################!
!          Direct Quadrature Method of Moments (DQMOM)                !
!#####################################################################!


!<keyword dtype="LOGICAL" category="Direct Quadrature Method of Moments (DQMOM)" required="false" locked="true">
!  <description>Variable to decide if the population balance equations are solved.</description>
      Call_DQMOM = .FALSE.
!</keyword>

!<keyword dtype="REAL" category="Direct Quadrature Method of Moments (DQMOM)" required="false" locked="true">
!  <description>Success-factor for aggregation.</description>
      AGGREGATION_EFF=0.D0
!</keyword>

!<keyword dtype="REAL" category="Direct Quadrature Method of Moments (DQMOM)" required="false" locked="true">
!  <description>Success-factor for breakage.</description>
      BREAKAGE_EFF=0.D0
!</keyword>








! ---------------------------------- questionable namelist entries below








!<keyword dtype="LOGICAL" category="questionable" required="false" locked="false">
!  <description>Variable which triggers an automatic restart.</description>
      AUTOMATIC_RESTART = .FALSE.
!</keyword>

!<keyword dtype="INTEGER" category="questionable" required="false" locked="false">
!  <description>AUTO_RESTART counter.</description>
      ITER_RESTART = 1
!</keyword>


!<keyword dtype="LOGICAL" category="Run Control" required="false" locked="true">
!  <description>
!    Flag to run the pre-processing (mesh generation) only and exit.
!  </description>
      PPO = .FALSE.
!</keyword>

! NO_OF_RXNS is not a keyword. However, it is initialized here so that
! if there are no reactions, this value is assigned.
      NO_OF_RXNS = UNDEFINED_I

      U_G0 = UNDEFINED
      V_G0 = UNDEFINED
      W_G0 = UNDEFINED
      U_S0(:DIM_M) = UNDEFINED
      V_S0(:DIM_M) = UNDEFINED
      W_S0(:DIM_M) = UNDEFINED

      PHIP_OUT_ITER=0

      CALL DES_INIT_NAMELIST

      CALL QMOMK_INIT_NAMELIST

      CALL USR_INIT_NAMELIST

      CALL CARTESIAN_GRID_INIT_NAMELIST

      RETURN

   END SUBROUTINE INIT_NAMELIST

END MODULE INIT_NAMELIST_MOD
