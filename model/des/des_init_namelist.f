MODULE DES_INIT_NAMELIST_MOD
CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                         C
!     Module name: DES_INIT_NAMELIST                                      C
!     Purpose: DES - initialize the des-namelist                          C
!                                                                         C
!     Reviewer: Rahul Garg                               Date: 01-Aug-07  C
!     Comments: Added some interpolation based inputs                     C
!                                                                         C
!  Keyword Documentation Format:                                          C
!<keyword category="category name" required="true/false"                  C
!                                    legacy="true/false">                 C
!  <description></description>                                            C
!  <arg index="" id="" max="" min=""/>                                    C
!  <dependent keyword="" value="DEFINED"/>                                C
!  <conflict keyword="" value="DEFINED"/>                                 C
!  <valid value="" note="" alias=""/>                                     C
!  <range min="" max="" />                                                C
!  MFIX_KEYWORD=INIT_VALUE                                                C
!</keyword>                                                               C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

   SUBROUTINE DES_INIT_NAMELIST

      USE des_bc, only: des_bc_uw_s, des_bc_vw_s, des_bc_ww_s, force_ord_bc
      USE des_thermo, only: des_em, des_conv_corr, flpc, des_min_cond_dist
      USE discretelement
      USE mfix_pic, only: mppic_coeff_en_wall, mppic_coeff_en1, mppic_coeff_et_wall, mppic_velfac_coeff, fric_exp_pic, fric_non_sing_fac, psfac_fric_pic, pic_cfl, pic_cfl_parcel_fraction, pic_cfl_control, pic_collision_damping
      USE param1, only: undefined, undefined_c, undefined_i, zero
      USE particle_filter, only: des_diffuse_width, des_interp_mean_fields, des_interp_on, des_interp_scheme, des_interp_width, des_report_mass_interp

      IMPLICIT NONE

      INCLUDE 'desnamelist.inc'


!#####################################################################!
!                             Run Control                             !
!#####################################################################!




!#####################################################################!
!                           Physical Parameters                       !
!#####################################################################!



!#####################################################################!
!                          Numerical Parameters                       !
!#####################################################################!



!#####################################################################!
!                          Output Control                             !
!#####################################################################!

!<keyword dtype="LOGICAL" category="Output Control" required="false"
!  dem="true" pic="true" locked="true">
!  <description>
!    Reports mass based on Lagrangian particles and continuum
!    representation. Useful to ensure mass conservation between
!    Lagrangian and continuum representations. Recommended use for
!    debugging purposes.
!  </description>
!  <dependent keyword="DES_INTERP_MEAN_FIELDS" value=".TRUE."/>
      DES_REPORT_MASS_INTERP = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false"
!  dem="true" pic="true" locked="true">
!  <description>
!    Allows writing of discrete particle data to output files. Relevant
!    to both granular and coupled simulations.
!  </description>
      PRINT_DES_DATA = .FALSE.
!</keyword>

!<keyword dtype="CHARACTER" category="Output Control" required="false" locked="true">
!  <description>
!    Directory where particle vtp files are stored. The files are
!    written in the run directory by default.
!  </description>
!  <dependent keyword="CARTESIAN_GRID" value=".TRUE."/>
      VTP_DIR = '.'
!</keyword>

!<keyword dtype="CHARACTER" category="Output Control" required="false"
!   dem="true" pic="true" locked="true">
!  <description> Output file format for DES data.</description>
!  <valid value="PARAVIEW" note="ParaView formatted files (.vtp)"/>
!  <valid value="TECPLOT" note="Tecplot formatted files (.dat)"/>
      DES_OUTPUT_TYPE = "PARAVIEW"
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false"
!  dem="true" pic="true" locked="true">
!  <description>
!    Runtime flag to generate debugging information. Additional data for
!    FOCUS_PARTICLE is saved.
!  </description>
      DEBUG_DES = .FALSE.
!</keyword>

!<keyword dtype="INTEGER" category="Output Control" required="false" dem="true" pic="true" locked="true">
!  <description>
!    Specify particle number for particle level debugging details.
!  </description>
!  <dependent keyword="DEBUG_DES" value=".TRUE."/>
      FOCUS_PARTICLE = 0
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false"
!  dem="true" pic="false" locked="true">
!  <description>
!    Option to write a particle_output.dat file at the end of a simulation.
!    This file has the same format as the particle_input.dat file (version 2.0
!    and up). After the simulation, the file can be renamed as
!    particle_input.dat and used as initial condition for another simulation.
!    For example, a first granular DEM simulation can be run fairly quickly
!    to settle a bed of particles and the data saved in this file can be used
!    to start a new coupled simulation.
!    Default value is .False. (do not write the file).
!  </description>
      WRITE_PART_OUT = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false"
!  dem="true" pic="false" locked="true">
!  <description>
!    Option to reset the velocity to zero in the particle_output.dat file.
!    Default value is .False. (do not reset the velocity to zero).
!  </description>
      PART_OUT_ZERO_VEL = .FALSE.
!</keyword>



!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    X-coordinate minimum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_X_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    X-coordinate maximum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_X_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    X-coordinate filter selection mode applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select only particles with data is outside the [min;max] range.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_X_EXCLUDE = .FALSE.
!</keyword>


!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Y-coordinate minimum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_Y_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Y-coordinate maximum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_Y_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Y-coordinate filter selection mode applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select only particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_Y_EXCLUDE = .FALSE.
!</keyword>


!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Z-coordinate minimum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_Z_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Z-coordinate maximum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_Z_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Z-coordinate filter selection mode applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select only particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_Z_EXCLUDE = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Solids phase filter selection mode applied to particles written in particle_output.dat.
!    Change to .FALSE. to exclude particles belonging to this phase.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      PART_OUT_PHASE(1:DIM_M) = .TRUE.
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Diameter minimum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_DIAMETER_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Diameter maximum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_DIAMETER_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Diameter filter selection mode applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select only particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_DIAMETER_EXCLUDE = .FALSE.
!</keyword>




!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Density minimum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_DENSITY_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Density maximum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_DENSITY_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Density filter selection mode applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select only particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_DENSITY_EXCLUDE = .FALSE.
!</keyword>




!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    X-velocity minimum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_U_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    X-velocity maximum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_U_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    X-velocity filter selection mode applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRYE. to select only particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_U_EXCLUDE = .FALSE.
!</keyword>




!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Y-velocity minimum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_V_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Y-velocity maximum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_V_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Y-velocity filter selection mode applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select only particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_V_EXCLUDE = .FALSE.
!</keyword>




!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Z-velocity minimum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_W_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Z-velocity maximum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_W_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Z-velocity filter selection mode applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_W_EXCLUDE = .FALSE.
!</keyword>




!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Temperature minimum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_TEMP_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Temperature maximum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_TEMP_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Temperature filter selection mode applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
      PART_OUT_TEMP_EXCLUDE = .FALSE.
!</keyword>




!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Species minimum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
!  <arg index="1" id="Index" min="1" max="100"/>
      PART_OUT_X_S_MIN(:) = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Species maximum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
!  <arg index="1" id="Index" min="1" max="100"/>
      PART_OUT_X_S_MAX(:) = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Species filter selection mode applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
!  <arg index="1" id="Index" min="1" max="100"/>
      PART_OUT_X_S_EXCLUDE(:) = .FALSE.
!</keyword>




!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    User-defined variable minimum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
!  <arg index="1" id="Index" min="1" max="100"/>
      PART_OUT_USR_VAR_MIN(:) = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    User-defined variable maximum value of the filter applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_OUT_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
!  <arg index="1" id="Index" min="1" max="100"/>
      PART_OUT_USR_VAR_MAX(:) = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    User-defined variable filter selection mode applied to particles written in particle_output.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="WRITE_PART_OUT" value="TRUE"/>
!  <arg index="1" id="Index" min="1" max="100"/>
      PART_OUT_USR_VAR_EXCLUDE(:) = .FALSE.
!</keyword>







!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    X-coordinate minimum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_X_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    X-coordinate maximum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_X_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    X-coordinate filter selection mode applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_X_EXCLUDE = .FALSE.
!</keyword>


!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Y-coordinate minimum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_Y_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Y-coordinate maximum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_Y_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Y-coordinate filter selection mode applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_Y_EXCLUDE = .FALSE.
!</keyword>


!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Z-coordinate minimum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_Z_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Z-coordinate maximum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_Z_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Z-coordinate filter selection mode applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_Z_EXCLUDE = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Solids phase filter applied to particles read from particle_input.dat.
!    Change to .FALSE. to exclude particles belonging to this phase.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
      PART_IN_PHASE(1:DIM_M) = .TRUE.
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Diameter minimum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_DIAMETER_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Diameter maximum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_DIAMETER_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Diameter filter selection mode applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_DIAMETER_EXCLUDE = .FALSE.
!</keyword>




!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Density minimum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_DENSITY_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Density maximum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_DENSITY_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Density filter selection mode applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_DENSITY_EXCLUDE = .FALSE.
!</keyword>




!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    X-velocity minimum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_U_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    X-velocity maximum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_U_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    X-velocity filter selection mode applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_U_EXCLUDE = .FALSE.
!</keyword>




!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Y-velocity minimum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_V_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Y-velocity maximum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_V_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Y-velocity filter selection mode applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_V_EXCLUDE = .FALSE.
!</keyword>




!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Z-velocity minimum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_W_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Z-velocity maximum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_W_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Z-velocity filter selection mode applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_W_EXCLUDE = .FALSE.
!</keyword>




!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Temperature minimum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_TEMP_MIN = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Temperature maximum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_TEMP_MAX = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Temperature filter selection mode applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
      PART_IN_TEMP_EXCLUDE = .FALSE.
!</keyword>




!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Species minimum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
!  <arg index="1" id="Index" min="1" max="100"/>
      PART_IN_X_S_MIN(:) = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Species maximum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
!  <arg index="1" id="Index" min="1" max="100"/>
      PART_IN_X_S_MAX(:) = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    Species filter selection mode applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
!  <arg index="1" id="Index" min="1" max="100"/>
      PART_IN_X_S_EXCLUDE(:) = .FALSE.
!</keyword>




!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    User-defined variable minimum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
!  <arg index="1" id="Index" min="1" max="100"/>
      PART_IN_USR_VAR_MIN(:) = -UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    User-defined variable maximum value of the filter applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are written
!    out, unless corresponding PART_IN_[DATA]_EXCLUDE is set to .TRUE.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
!  <arg index="1" id="Index" min="1" max="100"/>
      PART_IN_USR_VAR_MAX(:) = UNDEFINED
!</keyword>

!<keyword dtype="LOGICAL" category="Output Control" required="false" dem="true" locked="true">
!  <description>
!    User-defined variable filter selection mode applied to particles read from particle_input.dat.
!    By default, particles with data between min and max value are selected and written
!    out. Change to .TRUE. to select particles with data outside the [min;max] range.
!  </description>
!  <dependent keyword="GENER_PART_CONFIG" value="FALSE"/>
!  <arg index="1" id="Index" min="1" max="100"/>
      PART_IN_USR_VAR_EXCLUDE(:) = .FALSE.
!</keyword>




!#####################################################################!
! DEM/PIC COMMON:      Discrete Element Simulation                    !
!#####################################################################!


!<keyword dtype="INTEGER" category="Discrete Element Simulation" required="false"
!  dem="true" pic="true" locked="true">
!  <description>
!    Number of particles to be read in from the particle_input.dat file.
!    This value is overwritten when using automatic particle generation.
!    A simulation with a mass inflow BC can start without solids by
!    setting PARTICLES = 0.
!  </description>
!  <range min="0" max="+Inf" />
      PARTICLES = 1 !UNDEFINED_I
!</keyword>

!<keyword dtype="LOGICAL" category="Discrete Element Simulation" required="false"
!  dem="true" pic="true" locked="true">
!  <description>
!    Automatically generate the initial particle position and velocity
!    data based on the parameters specified for each initial condition
!    (IC) region.
!  </description>
!  <valid value=".TRUE." note="Generate particle configuration based
!    on the initial condition parameters. Data provided in the
!    particle_input.dat file, if present, is ignored. "/>
!  <valid value=".FALSE." note="Particle position and velocity data are
!    provided in the particle_input.dat file. A runtime error occurs if
!    this file is not provided."/>
      GENER_PART_CONFIG = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Discrete Element Simulation" required="false"
!  dem="true" pic="true" locked="true">
!  <description>Run one-way coupled simulations [default .FALSE.].
!   If set, the fluid does not see the particles in terms of drag force.
!   The effect of particle volume is still felt by the fluid through
!   non-unity voidage values.
!  </description>
!  <valid value=".FALSE."
!    note="Two-way particle-fluid coupling."/>
!  <valid value=".TRUE."
!    note="One-way particle-fluid coupling (fluid does not see particle drag)."/>
      DES_ONEWAY_COUPLED = .FALSE.
!</keyword>

!<keyword dtype="CHARACTER" category="Discrete Element Simulation" required="false" dem="true" locked="true">
!  <description>
!    Time stepping scheme.
!  </description>
!  <valid value="EULER"
!    note="First-order Euler scheme."/>
!  <valid value="ADAMS_BASHFORTH"
!    note="Second-order ADAMS_BASHFORTH scheme (DEM only)"/>
      DES_INTG_METHOD = 'EULER'
!</keyword>

!<keyword dtype="INTEGER" category="UDF Control" required="false" dem="true" locked="true">
!  <description>
!    Defines the size of the particle-based user variable:
!    DES_USR_VAR(SIZE, PARTICLES). Information in this array follows
!    the particle throughout a simulation.
!  </description>
      DES_USR_VAR_SIZE = 0
!</keyword>

!<keyword dtype="INTEGER" category="Discrete Element Simulation" required="false"
!  dem="true" pic="true" locked="true">
!  <description>
!    Number of des grid cells in the I-direction. If left undefined,
!    then it is set by MFiX such that its size equals three times the
!    maximum particle diameter with a minimum of 1 cell.
!  </description>
      DESGRIDSEARCH_IMAX = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Discrete Element Simulation" required="false"
!  dem="true" pic="true" locked="true">
!  <description>
!    Number of des grid cells in the J-direction. If left undefined,
!    then it is set by MFiX such that its size equals three times
!    the maximum particle diameter with a minimum of 1 cell.
!  </description>
      DESGRIDSEARCH_JMAX = UNDEFINED_I
!</keyword>

!<keyword dtype="INTEGER" category="Discrete Element Simulation" required="false"
!  dem="true" pic="true" locked="true">
!  <description>
!    Number of des grid cells in the K-direction. If left undefined,
!    then it is set by MFiX such that its size equals three times
!    the maximum particle diameter with a minimum of 1 cell.
!  </description>
      DESGRIDSEARCH_KMAX = UNDEFINED_I
!</keyword>

!<keyword dtype="CHARACTER" category="Discrete Element Simulation" required="false"
!  dem="true" pic="true" locked="true">
!  <description>
!    Specify the scheme used to map data to/from a particle's position
!    and the Eulerian grid. This keyword is required when
!    DES_INTERP_MEAN_FIELDS and/or DES_INTERP_ON are specified.
!  </description>
!  <valid value="NONE" note="Do not use interpolation."/>
!  <valid value="GARG_2012" note="Interpolate to/from a particle's
!    position using the corners (nodes) of the fluid cells. This was
!    the default behavior prior to the 2015-1 Release.
!    See Garg et al. (2012) Documentation of the open-source MFiX-DEM
!    software for gas-solids flows."/>
!  <valid value="SQUARE_DPVM" note="Divided Particle Volume Method:
!    Information is interpolated to/from a particles position using
!    a square filter of size DES_INTERP_WIDTH."/>
!  <valid value="LINEAR_HAT" note="Linear interpolation: Hat functions
!    are used to distribute particle information."/>
      DES_INTERP_SCHEME = 'NONE'
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Simulation" required="false" dem="true" locked="true">
!  <description>
!    Length used in interpolating data to/from a particle's position
!    and the Eulerian grid. The interpolation width is only applicable
!    to the DPVM_SQUARE and DPVM_GAUSS interpolation schemes as the
!    GARG_2012 scheme's interpolation width is determined by the
!    Eulerian grid dimensions.
!
!    - The interpolation half-width cannot exceed the minimum cell
!      dimension because interpolation is restricted to the 27-cell
!      neighborhood surrounding a particle (9-cell neighborhood in 2D).
!    - It is recommended that the DES_INTERP_WIDTH be set equal to the
!      maximum particle diameter when using STL defined boundaries.
!      Field data can be smoothed by specifying DES_DIFFUSE_WIDTH.
!
!  </description>
      DES_INTERP_WIDTH = UNDEFINED
!</keyword>


!<keyword dtype="LOGICAL" category="Discrete Element Simulation" required="false"
!  dem="true" pic="true" locked="true">
!  <description>
!    Enable/disable interpolation of field quantities to a particle's
!    position. This is used in calculating gas-particle interactions,
!    such as the drag force.
!  </description>
!  <valid value=".FALSE." note="Use fluid values from the cell containing
!    the particle's center."/>
!  <valid value=".TRUE." note="Interpolate fluid values from the 27-cell
!    neighborhood to a particle's position."/>
      DES_INTERP_ON = .FALSE.
!</keyword>

!<keyword dtype="LOGICAL" category="Discrete Element Simulation" required="false"
!  dem="true" pic="true" locked="true">
!  <description>
!    Enable/disable interpolation of particle data (e.g., solids
!    volume and drag force) from a particle's position to the
!    Eulerian grid.
!  </description>
!  <valid value=".FALSE." note="Assign particle data to the fluid
!    grid cell containing the particle's center."/>
!  <valid value=".TRUE." note="Interpolate particle data from the
!    particle's position to the 27-cell neighborhood surrounding
!    the particle."/>
      DES_INTERP_MEAN_FIELDS = .FALSE.
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Simulation" required="false" dem="true" locked="true">
!  <description>
!    The length scale used to smooth dispersed phase averaged fields by
!    solving a diffusion equation. This approach is typically used when
!    particle sizes near or exceed the size of the Eulerian grid cell sizes.
!
!    -  Mean field diffusion is disabled if DES_DIFFUSE_WIDTH is not specified.
!    -  Mean field diffusion cannot be used with the GARG_2012
!       interpolation scheme.
!    -  It is recommended that mean field diffusion be used in conjunction
!       with DES_EXPLICITLY_COUPLED to minimize the computational cost of
!       diffusing field data.
!    -  The DES diffusion equation is listed as equation type 10 in the
!       Numerical Parameters section.
!
!  </description>
      DES_DIFFUSE_WIDTH = UNDEFINED
!</keyword>


!<keyword dtype="LOGICAL" category="Discrete Element Simulation" required="false" dem="true" locked="true">
!  <description>
!    Enable/Disable explicit coupling of DEM solids and the fluid. This
!    algorithm is presently limited to hydrodynamic simulations.
!  </description>
!  <valid value=".FALSE." note="The fluid and particles calculate
!    interphase forces at their respective time scales. The fluid phase
!    calculates the interphase coupling forces once per fluid time step.
!    Similarly, DEM particles calculate the interface coupling forces at
!    each solids time-step. The DEM must also bin particles to the fluid
!    grid and recalculate the fluid volume fraction every time-step."/>
!  <valid value=".TRUE." note="Interphase forces are calculated during
!    the fluid time step and stored for each particle. The interphase
!    forces are then distributed among the solids time-steps. This
!    approach can substantially reduce the computational overhead for
!    coupled simulations."/>
      DES_EXPLICITLY_COUPLED = .FALSE.
!</keyword>


!#####################################################################!
! DEM ONLY:            Discrete Element Model                         !
!#####################################################################!

!<keyword dtype="INTEGER" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    The number of iterations of a pure granular simulation to let
!    the initial particle configuration settle before a coupled
!    gas-solid calculation is started.
!  </description>
!  <range min="0" max="+Inf" />
      NFACTOR = 0
!</keyword>

!<keyword dtype="INTEGER" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Maximum number of steps through a DEM loop before a neighbor
!    search will be performed. The search may be called earlier
!    based on other logic.
!  </description>
!  <range min="0.0" max="+Inf" />
      NEIGHBOR_SEARCH_N = 25
!</keyword>

!<keyword dtype="INTEGER" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Flag to set the neighbor search algorithm.
!  </description>
!  <valid value="1" note="N-Square search algorithm (most expensive)"/>
!  <valid value="4" note="Grid-Based Neighbor Search (Recommended)"/>
      DES_NEIGHBOR_SEARCH = 4
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Ratio of the distance (imaginary sphere radius) to particle radius
!    that is allowed before a neighbor search is performed. This works
!    in conjunction with the logic imposed by NEIGHBOR_SEARCH_N in
!    deciding calls to the neighbor search algorithm.
!  </description>
      NEIGHBOR_SEARCH_RAD_RATIO = 1.0D0
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Effectively increases the radius of a particle (multiple of the sum
!    of particle radii) during the building of particle neighbor list.
!  </description>
      FACTOR_RLM = 1.2
!</keyword>

!<keyword dtype="LOGICAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Flag to use van der Hoef et al. (2006) model for adjusting the
!    rotation of the contact plane. See the MFiX-DEM documentation.
!  </description>
      USE_VDH_DEM_MODEL = .FALSE.
!</keyword>


!<keyword dtype="CHARACTER" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Collision model for the soft-sphere approach used in DEM model.
!    All models require specifying the following parameters: DES_EN_INPUT,
!    DES_EN_WALL_INPUT, MEW, and MEW_W.
!  </description>
!  <valid value="LSD" note="The linear spring-dashpot model.
!    Requires: KN, KN_W, KT_FAC, KT_W_FAC, DES_ETAT_FAC, DES_ETAT_W_FAC."/>
!  <valid value="HERTZIAN" note="The Hertzian model.
!    Requires: DES_ET_INPUT, DES_ET_WALL_INPUT, E_YOUNG, EW_YOUNG
!    V_POISSON, VW_POISSON."/>
      DES_COLL_MODEL = 'LSD'
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" dem="true" locked="true">
!  <description>
!    Normal spring constant [N/m in SI] for inter-particle collisions.
!    Required when using the linear spring-dashpot collision model.
!  </description>
      KN = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" dem="true" locked="true">
!  <description>
!    Ratio of the tangential spring constant to normal spring constant
!    for inter-particle collisions. Use it to specify the tangential
!    spring constant for particle-particle collisions as KT_FAC*KN.
!    Required when using the linear spring-dashpot collision model.
!  </description>
!  <dependent keyword="DES_COLL_MODEL" value="LSD"/>
!  <range min="0.0" max="1.0" />
      KT_FAC = 0.28571428571429
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" dem="true" locked="true">
!  <description>
!    Normal spring constant [N/m in SI] for particle-wall collisions.
!    Required when using the linear spring-dashpot collision model.
!  </description>
      KN_W = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" dem="true" locked="true">
!  <description>
!    Ratio of the tangential spring constant to normal spring constant
!    for particle-wall collisions. Use it to specify the tangential
!    spring constant for particle-wall collisions as KT_W_FAC*KN_W.
!    Required when using the linear spring-dashpot collision model.
!  </description>
!  <dependent keyword="DES_COLL_MODEL" value="LSD"/>
!  <range min="0.0" max="1.0" />
      KT_W_FAC = 0.28571428571429
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Model" required="false" dem="true" locked="true">
!  <description>
!    Inter-particle Coulomb friction coefficient.
!  </description>
! <range min="0.0" max="1.0" />
      MEW = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Particle-wall Coulomb friction coefficient.
!  </description>
! <range min="0.0" max="1.0" />
      MEW_W = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Model" required="false" dem="true" locked="true">
!  <description>
!    Inter-particle rolling friction coefficient.
!  </description>
! <range min="0.0" max="1.0" />
      MEW_R = 0.0D0
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Particle-wall rolling friction coefficient.
!  </description>
! <range min="0.0" max="1.0" />
      MEW_RW = 0.0D0
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Model" required="false" dem="true" locked="true">
!  <description>
!    Normal restitution coefficient for inter-particle collisions
!    used to determine the inter-particle normal damping factor.
!
!    Values should be defined for a single dimensional array. For
!    example, a simulation with three solids phases (MMAX=3) needs
!    six values: en11, en12, en13; en22, en23; en33.
!  </description>
!  <range min="0.0" max="1.0" />
!  <arg index="1" id="Index" min="1" max="MMAX*(MMAX-1)/2"/>
      DES_EN_INPUT(:) = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" dem="true" locked="true">
!  <description>
!    Normal restitution coefficient for particle-wall collisions
!    used to determine the particle-wall normal damping factor.
!
!    Values should be defined in a single dimensional array. For
!    example, a simulation with three solids phases (MMAX=3) needs
!    three values: enw1, enw2, enw3.
!  </description>
!  <range min="0.0" max="1.0" />
!  <arg index="1" id="Index" min="1" max="MMAX"/>
      DES_EN_WALL_INPUT(:) = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" dem="true" locked="true">
!  <description>
!    Tangential restitution coefficient for inter-particle collisions.
!    Values are defined in a one dimensional array. This is required
!    input when using the Hertzian collision model.
! </description>
! <dependent keyword="DES_COLL_MODEL" value="HERTZIAN"/>
! <range min="0.0" max="1.0" />
! <arg index="1" id="Index" min="1" max="MMAX*(MMAX-1)/2"/>
      DES_ET_INPUT(:) = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" dem="true" locked="true">
!  <description>
!    Tangential restitution coefficient for particle wall collisions.
!    Values are defined in a one dimensional array. This is required
!    input when using the Hertzian collision model.
!  </description>
! <range min="0.0" max="1.0" />
! <dependent keyword="DES_COLL_MODEL" value="HERTZIAN"/>
! <arg index="1" id="Index" min="1" max="MMAX"/>
      DES_ET_WALL_INPUT(:) = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" dem="true" locked="true">
!  <description>
!    Ratio of the tangential damping factor to the normal damping factor
!    for inter-particle collisions.  Required for the linear spring-dashpot
!    collision model.
!  </description>
!  <dependent keyword="DES_COLL_MODEL" value="LSD"/>
!  <range min="0.0" max="1.0" />
!  <valid value="UNDEFINED" note="For LSD model, if left undefined, MFiX
!   reverts to default value of 0.5." />
      DES_ETAT_FAC = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
! <description>
!    Ratio of the tangential damping factor to the normal damping
!    factor for particle-wall collisions. Required for the linear
!    spring-dashpot model for soft-spring collision modelling under
!    DEM. For the Hertzian model, the tangential damping coefficients
!    have to be explicitly specified and specification of this
!    variable is not required.
! </description>
! <dependent keyword="DES_COLL_MODEL" value="LSD"/>
! <range min="0.0" max="1.0" />
! <valid value="UNDEFINED" note="For LSD model, if left undefined, MFiX
! will revert to default value of 0.5" />
      DES_ETAT_W_FAC = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Young's modulus for the wall [Pa in SI]. Required when using the
!    Hertzian collision model.
!  </description>
!  <dependent keyword="DES_COLL_MODEL" value="HERTZIAN"/>
      EW_YOUNG = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Poisson ratio for the wall. Required when using the Hertzian
!    collision model.
!  </description>
!  <dependent keyword="DES_COLL_MODEL" value="HERTZIAN"/>
      VW_POISSON = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Young's modulus for the particle [Pa in SI]. Required when using
!    the Hertzian collision model.
!  </description>
!  <arg index="1" id="Phase" min="1" max="DES_MMAX"/>
!  <dependent keyword="DES_COLL_MODEL" value="HERTZIAN"/>
      E_YOUNG(:DIM_M) = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Poisson's ratio for the particle. Required when using the Hertzian
!    collision model.
!  </description>
!  <arg index="1" id="Phase" min="1" max="DES_MMAX"/>
!  <dependent keyword="DES_COLL_MODEL" value="HERTZIAN"/>
      V_POISSON(:DIM_M) = UNDEFINED
!</keyword>



!<keyword dtype="LOGICAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Flag to enable/disable cohesion model.
!  </description>
      USE_COHESION = .FALSE.
!</keyword>


!<keyword dtype="LOGICAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Flag to turn on the Hamaker van der Waals forces.
!  </description>
!  <dependent keyword="USE_COHESION" value=".TRUE."/>
      VAN_DER_WAALS = .FALSE.
!</keyword>


! for cohesion: van der waals
!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Hamaker constant used in particle-particle cohesive interactions.
!  </description>
!  <dependent keyword="USE_COHESION" value=".TRUE."/>
      HAMAKER_CONSTANT = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Hamaker constant used in particle-wall cohesive interactions.
!  </description>
!  <dependent keyword="USE_COHESION" value=".TRUE."/>
      WALL_HAMAKER_CONSTANT = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Maximum separation distance above which van der Waals forces are
!    not implemented.
!  </description>
!  <dependent keyword="USE_COHESION" value=".TRUE."/>
      VDW_OUTER_CUTOFF = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Minimum separation distance below which van der Waals forces are
!    calculated using a surface adhesion model.
!  </description>
!  <dependent keyword="USE_COHESION" value=".TRUE."/>
      VDW_INNER_CUTOFF = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Maximum separation distance above which van der Waals forces are
!    not implemented (particle-wall interactions).
!  </description>
!  <dependent keyword="USE_COHESION" value=".TRUE."/>
      WALL_VDW_OUTER_CUTOFF = ZERO
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Minimum separation distance below which van der Waals forces are
!    calculated using a surface adhesion model (particle-wall
!    interactions).
!  </description>
!  <dependent keyword="USE_COHESION" value=".TRUE."/>
      WALL_VDW_INNER_CUTOFF = UNDEFINED
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Mean radius of surface asperities that influence the cohesive force.
!    See H. Rumpf, Particle Technology, Chapman &amp; Hall,
!    London/New York, 1990.
!  </description>
!  <dependent keyword="USE_COHESION" value=".TRUE."/>
      Asperities = ZERO
!</keyword>


!<keyword dtype="CHARACTER" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Specify the Nusselt number correlation used for particle-gas
!    convection.
!  </description>
!  <valid value="RANZ_1952" note="Ranz, W.E. and Marshall, W.R. (1952).
!    Chemical Engineering Progress, 48: 141-146 and 173-180"/>
!  <valid value="GUNN" note="Gunn D. Transfer of heat or mass to particles in fixed and fluidised beds.
!   International Journal of Heat and Mass Transfer. 1978;21(4):467-476."/>
!  <valid value="WAKAO" note="Wakao N, Kaguei S, Funazkri T. Effect of fluid dispersion coefficients
!  on particle-to-fluid heat transfer coefficients in packed beds: correlation of Nusselt numbers.
!  Chemical engineering science. 1979;34(3):325-336."/>
!  <valid value="TAVASSOLI" note="Tavassoli H, Peters E, Kuipers J. Direct numerical simulation
!  of fluid–particle heat transfer in fixed random arrays of non-spherical particles.
!  Chemical Engineering Science.  2015;129:42-48."/>
      DES_CONV_CORR = 'RANZ_1952'
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Minimum separation distance between the surfaces of two contacting
!    particles.
!  </description>
      DES_MIN_COND_DIST = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Fluid lens proportionality constant used to calculate the radius of
!    the fluid lens that surrounds a particle. This parameter is used in
!    the particle-fluid-particle conduction model.
!  </description>
      FLPC = 0.2d0
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>Emissivity of solids phase.</description>
!  <arg index="1" id="Phase" min="1" max="DES_MMAX"/>
      DES_Em(:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Actual Young's modulus for the particle [Pa in SI]. Used for
!    computing correction terms for DEM conduction.
!  </description>
!  <arg index="1" id="Phase" min="1" max="DES_MMAX"/>
      E_YOUNG_ACTUAL(:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Actual Young's modulus for the walls [Pa in SI]. Used for
!    computing correction terms for DEM conduction.
!  </description>
!  <dependent keyword="DES_COLL_MODEL" value="HERTZIAN"/>
      EW_YOUNG_ACTUAL = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Poisson's ratio for the particle. Used for
!    computing correction terms for DEM conduction.
!  </description>
!  <arg index="1" id="Phase" min="1" max="DES_MMAX"/>
      V_POISSON_ACTUAL(:DIM_M) = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Poisson's ratio for the wall. Used for
!    computing correction terms for DEM conduction.
!  </description>
      VW_POISSON_ACTUAL = UNDEFINED
!</keyword>



!<keyword dtype="LOGICAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Flag to turn on/off optimizing the list of facets at each des grid cell.
!  </description>
!  <dependent keyword="USE_STL" value=".TRUE."/>
      MINIMIZE_DES_FACET_LIST =.TRUE.
!</keyword>

!<keyword dtype="LOGICAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Flag to remove rogue particles. A rogue particle is defined as a particle
!    that goes out of the MFiX box outside of a defined pressure or mass outlet
!    BC.
!  </description>
      REMOVE_ROGUE_PARTICLES =.TRUE.
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Ratio between the collision time and the DEM time step (default value is
!    50). Minimum value is 10.0. It is highly recommended to keep the default value.
!  </description>
!  <range min="10.0" max="+Inf" />
!  <dependent keyword="DES_COLL_MODEL" value="HERTZIAN"/>
      DTSOLID_FACTOR = 50.0
!</keyword>


!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Time interval at which the DEM solids time step is updated.
!    Useful when particle's size or mass change over time.
!    Only applicable to coupled gas/solids flows (this setting is ignored for
!    pure granular flows).
!  </description>
!  <range min="0.0" max="+Inf" />
!  <dependent keyword="DES_COLL_MODEL" value="HERTZIAN"/>
      DTSOLID_UPDATE_DT = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Growth factor when resizing send/recv buffers (Default is 0.5)
!    Increase this value if the resizing fails. This could occur with initial
!    packed bed, and small partition blocks (around 5 cells).
!  </description>
!  <range min="0.0" max="+Inf" />
      DES_BUFF_RESIZE_FACTOR = 0.5
!</keyword>

!#####################################################################!
!                          Particle In Cell                           !
!#####################################################################!


!<keyword dtype="REAL" category="Particle In Cell" required="false"
!     pic="true" locked="false">
!  <description>
!    Volume fraction exponential scale factor in frictional stress model.
!  </description>
!  <range min="1.0" max="5.0" />
      FRIC_EXP_PIC = 3.0d0
!</keyword>


!<keyword dtype="REAL" category="Particle In Cell" required="false"
!     pic="true" locked="false">
!  <description>
!    Pressure linear scale factor in frictional stress model.
!  </description>
!  <range min="1.0" max="10000.0" />
      PSFAC_FRIC_PIC = 100.0d0
!</keyword>


!<keyword dtype="REAL" category="Particle In Cell" required="false"
!     pic="true" locked="false">
!  <description>
!    An empirical dampening factor for the frictional stress model.
!  </description>
!  <range min="0.0" max="1.0" />
      MPPIC_COEFF_EN1 = 0.85d0
!</keyword>


!<keyword dtype="REAL" category="Particle In Cell" required="false" locked="false">
!  <description>
!    Non-singularity term in frictional stress model.
!  </description>
!  <range min="1.0e-15" max="1.0e-4" />
      FRIC_NON_SING_FAC = 1.0e-07
!</keyword>


!<keyword dtype="REAL" category="Particle In Cell" required="false"
!     pic="true" locked="false">
!  <description>
!    Normal coefficient of restitution for parcel-wall collisions.
!  </description>
!  <range min="0.0" max="1.0" />
      MPPIC_COEFF_EN_WALL = 0.85d0
!</keyword>


!<keyword dtype="REAL" category="Particle In Cell" required="false"
!     pic="true" locked="false">
!  <description>
!    Tangential coefficient of restitution for parcel-wall collisions.
!  </description>
!  <range min="0.0" max="1.0" />
      MPPIC_COEFF_ET_WALL = 1.0d0
!</keyword>


!<keyword dtype="REAL" category="Particle In Cell" required="false"
!     pic="true" locked="false">
!  <description>
!    Solids slip velocity scale factor. This term can be used to scale the
!    bulk solids velocity when calculating parcel/bulk solids slip velocity.
!    Scaling is uniform in all three directions.
!  </description>
!  <range min="0.0" max="1.0" />
      MPPIC_VELFAC_COEFF = 1.0d0
!</keyword>

!<keyword dtype="REAL" category="Particle In Cell" required="false"
!     pic="true" locked="false">
!  <description>
!     Solids CFL (Courant-Friedrichs-Lewy) value.
!     This term can be used to arrest solids time step through a local examination of parcel velocity.  Leave undefined (blank) to disable time step control.
!  </description>
!  <dependent keyword="PIC_CFL_PARCEL_FRACTION" />
!  <range min="0.0" max="1.0" />
      PIC_CFL = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Particle In Cell" required="false"
!     pic="true" locked="false">
!  <description>
!    Parcel fraction threshold to invoke a change in solids time step
!    based on solids CFL value.
!  </description>
!  <range min="0.0" max="1.0" />
      PIC_CFL_PARCEL_FRACTION = 0.01
!</keyword>

!<keyword dtype="CHARACTER" category="Particle In Cell" required="false"
!     pic="true" locked="false">
!  <description>
!    Control variable for managing solids CFL value.  Solids time step
!    will be based on MAX or AVG calculated solids CFL.
!  </description>
      PIC_CFL_CONTROL = 'MAX'
!</keyword>

!<keyword dtype="LOGICAL" category="Particle In Cell" required="false"
!     pic="true" locked="false">
!  <description>
!    Flag to turn on/off PIC collision damping model.
!  </description>
      PIC_COLLISION_DAMPING =.FALSE.
!</keyword>

!#####################################################################!
!                      Coarse-grain DEM                               !
!#####################################################################!


!<keyword dtype="REAL" category="Solids Phase" required="false"
!  tfm="false" dem="true" pic="false" locked="false">
!  <description>
!    Coarse-grain particle statistical weight.
!    The coarse graining is achieved by specifying either the statistical
!    weight, or the coarse-grain particle size. It is not allowed to specify
!    both the statistical weight and Coarse Grain particle size at the same
!    time.
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
!  <range min="1.0" max=""/>
      CGP_STAT_WT(:DIM_M) = ZERO
!</keyword>

!<keyword dtype="REAL" category="Solids Phase" required="false"
!  tfm="false" dem="true" pic="false" locked="false">
!  <description>
!    Coarse-grain particle size.
!    The coarse graining is achieved by specifying either the statistical
!    weight, or the coarse-grain particle size (it is not allowed to specify
!    both the statistical weight and coarse-grain particle size at the same
!    time).
!  </description>
!  <arg index="1" id="Phase" min="1" max="DIM_M"/>
!  <range min="0.0" max=""/>
      CGP_D_P0(:DIM_M) = ZERO
!</keyword>
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                            Load balance Keywords                     !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Time interval at which Dynamic Load Balance (DLB) is performed (sec)
!  </description>
!  <valid value="UNDEFINED" note=": UNDEFINED value turns Dynamic Load Balance off" alias=""/>
!  <valid value=">0" note=": Any positive value turns Dynamic Load Balance on" alias=""/>
!  <range min="+0.0"/>
      DLB_DT = UNDEFINED
!</keyword>

!<keyword dtype="REAL" category="Discrete Element Model" required="false" locked="true">
!  <description>
!    Eulerian Grid Weight use in Dynamic Load Balance.
!    A value of zero means the balancing only considers particles.
!    A very large value means the partition will balance only the fluid mesh.
!    Finding the optimal value will require trial and error.
!  </description>
!  <valid value=">=0" note="" alias=""/>
!  <range min="0.0"/>
      DLB_EGW = 0.0
!</keyword>

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                            UNSUPPORTED KEYWORDS                      !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

! Logical to force the inlet to operate with an ordered boundary
! condition. This may be useful during long simulations or if the
! inlet appears to be taking a long time to randomly place particles.
      FORCE_ORD_BC = .FALSE.

! Lees-Edwards boundary condition to simulate homogeneous shear
! problem with periodic boundary conditions. Not supported in this
! version.
      DES_LE_BC = .FALSE.

! Relative velocity needed for Lees-Edwards BC.
! Not supported in this version.
      DES_LE_REL_VEL = UNDEFINED

! Direction of shear for Lees-Edwards BC.
! Not supported in this version. </description>
      DES_LE_SHEAR_DIR = UNDEFINED_C

! des wall boundaries: wall velocities. I think they probably
! defined for the Lees-Edwards BC's
      DES_BC_Uw_s(:,:) = ZERO
      DES_BC_Vw_s(:,:) = ZERO
      DES_BC_Ww_s(:,:) = ZERO


! These need to be initialized to 0, but they are not part of the namelist
      VTP_FINDEX = 0
      TECPLOT_FINDEX = 0

! not a well supported feature and not generic either. So removing
! from namelists
      DES_CALC_BEDHEIGHT = .FALSE.


      RETURN

   END SUBROUTINE DES_INIT_NAMELIST

END MODULE DES_INIT_NAMELIST_MOD
