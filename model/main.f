#include "error.inc"

! -*- f90 -*-
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  MODULE: MAIN                                                        !
!                                                                      !
!  Purpose: Main module for top level mfix subroutines.                !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

      MODULE MAIN

      use allocate_dummy_cut_cell_arrays_mod, only: allocate_dummy_cut_cell_arrays
      use cal_d_mod, only: cal_d
      use cg_set_bc0_mod, only: cg_set_bc0
      use check_dmp_prereqs_mod, only: check_dmp_prereqs
      use des_init_arrays_mod, only: des_init_arrays
      use des_psd, only: prepare_custom_dist
      use error_manager
      use exit, only: mfix_exit, check_exit_flag, exit_flag
      use funits, only: file_size
      use get_bc_area_mod, only: get_bc_area
      use init_fvars_mod, only: init_fvars
      use init_namelist_mod, only: init_namelist
      use iso_c_binding, only: c_bool, c_int, c_char, c_ptr
      use make_arrays_des_mod, only: make_arrays_des
      use qmomk_allocate_arrays_mod, only: qmomk_allocate_arrays
      use qmomk_make_arrays_mod, only: qmomk_make_arrays
      use read_res1_mod, only: read_res1
      use set_bc_dem_mod, only: set_bc_dem
      use set_flags_mod, only: set_flags, set_flags1
      use set_fluidbed_p_mod, only: set_fluidbed_p
      use set_geometry1_mod, only: set_geometry1
      use set_geometry_des_mod, only: set_geometry_des
      use set_geometry_mod, only: set_geometry
      use set_ic_dem_mod, only: set_ic_dem
      use set_icbc_flag_mod, only: set_icbc_flag
      use set_increments_mod, only: set_increments, re_index_arrays
      use set_max2_mod, only: set_max2
      use set_parameters_mod, only: set_parameters
      use undef_2_0_mod, only: undef_2_0
      use update_dashboard_mod, only: update_dashboard
      use vtk_out_mod
      use write_header_mod, only: write_header
      use write_out3_mod, only: write_out3
      use write_res1_mod, only: write_res1
      use write_spx0_mod, only: write_spx0
      use write_spx1_mod, only: write_spx1

! Module variables
!---------------------------------------------------------------------
! Final value of CPU time.
      DOUBLE PRECISION :: CPU1
! time used for computations.
      DOUBLE PRECISION :: CPUTIME_USED, WALLTIME_USED
! DISTIO variable for specifying the mfix version (should this be 01.8?)
      CHARACTER(LEN=512) :: version = 'RES = 01.6'

! Number of iterations
      INTEGER :: NIT_TOTAL
! used for activating check_data_30
      INTEGER :: NCHECK, DNCHECK

      CHARACTER(LEN=80), DIMENSION(100) :: CMD_LINE_ARGS
      INTEGER :: CMD_LINE_ARGS_COUNT = 0

   CONTAINS

      subroutine run_mfix0(mfix_dat_filename, cmdline_loglevel)

!f2py threadsafe
         USE ERROR_MANAGER, ONLY: LOGLEVEL
         USE EXIT, ONLY: MFIX_EXIT, CHECK_EXIT_FLAG
         USE MPI_UTILITY

         CHARACTER(LEN=1000), INTENT(IN) :: MFIX_DAT_FILENAME
         INTEGER(KIND=c_int), INTENT(IN) :: CMDLINE_LOGLEVEL

         loglevel = cmdline_loglevel
         CALL RUN_MFIX(MFIX_DAT_FILENAME, FILE_SIZE(MFIX_DAT_FILENAME))
         EXIT_FLAG = .TRUE.
      end subroutine run_mfix0

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE INITIALIZE(MFIX_DAT)

! Modules
!---------------------------------------------------------------------

#ifdef MPI
         USE mpi, only: mpi_comm_world, mpi_barrier
         USE compar, only: mpierr
#endif

      USE allocate_arrays_mod, only: allocate_arrays
      USE bc_pic_mi, only: set_bc_pic_mi
      USE calc_coeff_mod, only: init_coeff
      USE cdist, only: bstart_with_one_res, bdist_io
      USE check, only: check_mass_balance
      USE check_data_20_mod, only: check_data_20
      USE check_data_cg, only: set_CG_bc_flags, report_best_processor_size
      USE compar, only: mype, pe_io
      USE cont, only: do_cont
      USE corner, only: get_corner_cells
      USE cutcell, only: cartesian_grid, re_indexing, set_corner_cells
      USE dashboard, ONLY: dtmax, dtmin, init_time, n_dashboard, nit_max, nit_min, smmax, smmin
      USE dbg, only: debug_write_layout, write_parallel_info
      USE des_allocate, only: des_allocate_arrays
      USE discretelement, only: discrete_element
      USE drag, only: f_gs
      USE fldvar, only: rop_g, rop_s
      USE funits, only: dmp_log, unit_log, unit_res
      USE iterate, ONLY: max_nit
      USE machine, only: start_log, end_log
      USE mark_phase_4_cor_mod, only: mark_phase_4_cor
      USE output, only: dbgprn_layout
      USE output_man, only: init_output_vars, output_manager
      USE param1, only: n_spx, undefined, zero, large_number
      USE parse_resid_string_mod, only: parse_resid_string
      USE pgcor, only: d_e, d_n, d_t, phase_4_p_g, switch_4_p_g
      USE physprop, only: mmax
      USE pscor, only: e_e, e_n, e_t, do_p_s, phase_4_p_s, mcp, switch_4_p_s
      USE qmom_kinetic_equation, only: qmomk
      USE rrates_init_mod, only: rrates_init
      USE run, only: call_usr, dem_solids, dt_max, dt_min
      USE run, only: ier
      USE run, only: nstep, pic_solids, run_type, dt, shear, time, v_sh
      USE run, only: time
      USE run, only:ppo
      USE set_bc0_mod, only: set_bc0
      USE set_bc0_flow_mod, only: set_bc0_flow
      USE set_bc1_mod, only: set_bc1
      USE set_constprop_mod, only: set_constprop
      USE set_ic_mod, only: set_ic
      USE set_mw_mix_g_mod, only: set_mw_mix_g
      USE set_ps_mod, only: set_ps
      USE set_ro_g_mod, only: set_ro_g
      USE set_ro_s_mod, only: set_ro_s, bc_ros_correction
      USE time_cpu, only: CPU00, wall0, wall_time
      USE time_cpu, only: cpu_io, cpu_nlog, cpu0, cpuos, time_nlog
      USE vtk, only: write_vtk_files
      USE write_out0_mod, only: write_out0, write_flags
      USE write_res0_mod, only: write_res0
      USE zero_norm_vel_mod, only: zero_norm_vel

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

      ! Temporary storage for DT
     DOUBLE PRECISION :: DT_tmp
      ! Save TIME in input file for RESTART_2
     DOUBLE PRECISION :: TIME_SAVE

     INTEGER :: LL, MM

     IF(PPO) RETURN

! ARRAY ALLOCATION
!---------------------------------------------------------------------
! Allocate array storage.
     CALL ALLOCATE_ARRAYS
     IF(DISCRETE_ELEMENT) CALL DES_ALLOCATE_ARRAYS
     IF(QMOMK) CALL QMOMK_ALLOCATE_ARRAYS

! Initialize arrays.
     CALL INIT_FVARS
     IF(DISCRETE_ELEMENT) CALL DES_INIT_ARRAYS


! Data initialization for Dashboard
!---------------------------------------------------------------------
     INIT_TIME = TIME
     SMMIN =  LARGE_NUMBER
     SMMAX = -LARGE_NUMBER

     DTMIN =  LARGE_NUMBER
     DTMAX = -LARGE_NUMBER

     NIT_MIN = MAX_NIT
     NIT_MAX = 0

     N_DASHBOARD = 0

! stop trigger mechanism to terminate MFIX normally before batch
! queue terminates. timestep at the beginning of execution
      CALL CPU_TIME (CPU00)
      WALL0 = WALL_TIME()

! Write the initial part of the standard output file
      CALL WRITE_OUT0
      IF(.NOT.CARTESIAN_GRID)  CALL WRITE_FLAGS

! Write the initial part of the special output file(s)
      CALL WRITE_USR0

      DT_TMP = DT
      SELECT CASE (TRIM(RUN_TYPE))

      CASE ('NEW')
! Write the initial part of the restart files
         CALL WRITE_RES0
         DO LL = 1, N_SPX
            CALL WRITE_SPX0 (LL, 0)
         ENDDO

      CASE ('RESTART_1')
! Read the time-dependent part of the restart file
         CALL READ_RES1
         WRITE(ERR_MSG, 1010) TIME, NSTEP
         CALL LOG_INFO()

      CASE ('RESTART_2')
         TIME_SAVE = TIME
! DISTIO
         if (myPE .ne. PE_IO .and. bDist_IO .and. bStart_with_one_res) then
            write (unit_res,rec=1) version
            write (unit_res,rec=2) 4
            write (unit_res,rec=3) 4
         endif

         CALL READ_RES1
         TIME = TIME_SAVE

1010     FORMAT('Message 1010: Read in data from .RES file for TIME = ',&
              G12.5,/'Time step number (NSTEP) =',I7)

         WRITE(ERR_MSG, 1010) TIME, NSTEP
         CALL LOG_INFO()

         CALL WRITE_RES0

! Writing the RES1 and SPX1 can only be done here when re-indexing is turned off
! This will be done after the cell re-indexing is done later in this file.
! This allows restarting independently of the re-indexing setting between
! the previous and current run.
         IF(.NOT.RE_INDEXING) THEN
            CALL WRITE_RES1
            DO LL = 1, N_SPX
               CALL WRITE_SPX0 (LL, 0)
               CALL WRITE_SPX1 (LL, 0)
            END DO
         ENDIF

      CASE DEFAULT
         CALL START_LOG
         IF(DMP_LOG)WRITE (UNIT_LOG, *) &
              " MFIX: Do not know how to process"
         IF(DMP_LOG)WRITE (UNIT_LOG, *) ' RUN_TYPE in data file'
         CALL END_LOG
         call mfix_exit(myPE)

      END SELECT

#ifdef MPI
      call MPI_Barrier(MPI_COMM_WORLD,mpierr)
#endif

      IF (DT_TMP /= UNDEFINED) THEN
         DT = MAX(DT_MIN,MIN(DT_MAX,DT))
      ELSE
         DT = DT_TMP
      ENDIF

! Set arrays for computing indices. A secondary call is made
! after cut cell-preprocessing to update array indices.
      IF(CARTESIAN_GRID) THEN
         CALL SET_INCREMENTS
      ENDIF

!      IF(.NOT.RE_INDEXING) CALL WRITE_IJK_VALUES

! Set the flags for wall surfaces impermeable and identify flow
! boundaries using FLAG_E, FLAG_N, and FLAG_T
      CALL SET_FLAGS1

!  Update flags for Cartesian_GRID.
      IF(CARTESIAN_GRID) CALL SET_CG_BC_FLAGS

! Calculate cell volumes and face areas
      IF(.NOT.CARTESIAN_GRID) CALL SET_GEOMETRY1

! Find corner cells and set their face areas to zero
      IF(.NOT.CARTESIAN_GRID)  THEN
         CALL GET_CORNER_CELLS()
      ELSE
         IF (SET_CORNER_CELLS)  CALL GET_CORNER_CELLS ()
      ENDIF

! Set constant physical properties
      CALL SET_CONSTPROP

! Set initial conditions
      CALL SET_IC

! Set wall boundary conditions
      CALL ZERO_NORM_VEL
! Set scalar flow boundary conditions
      CALL SET_BC0
! Cartesian grid implementation
      IF(CARTESIAN_GRID) CALL CG_SET_BC0

! Set gas mixture molecular weight
      CALL SET_MW_MIX_G

! Set the pressure field for a fluidized bed
      IF (RUN_TYPE == 'NEW') CALL SET_FLUIDBED_P

! Initialize densities.
      IF (RUN_TYPE == 'NEW') CALL SET_RO_G
      IF (RUN_TYPE == 'NEW') THEN
         CALL SET_RO_S
      ELSE
         CALL BC_ROS_CORRECTION
      ENDIF

! Convert (mass, volume) flows to velocities and
! set velocity flow boundary conditions
      CALL SET_BC0_FLOW; IF(REINIT_ERROR()) RETURN

! Set point sources.
      CALL SET_PS(MFIX_DAT)

! Initialize time dependent boundary conditions
      CALL SET_BC1


! Check the field variable data and report errors.
      CALL CHECK_DATA_20

      IF(CARTESIAN_GRID.AND.RE_INDEXING) THEN

         IF(myPE == PE_IO) THEN
            WRITE(*,"(72('='))")
            WRITE(*,*)' RE-INDEXING CELLS FOR CARTESIAN GRID...'
         ENDIF
         CALL RE_INDEX_ARRAYS


         !IF(myPE == PE_IO)print*,'Calling REPORT_BEST_IJK_SIZE:'
         !CALL REPORT_BEST_IJK_SIZE
         CALL REPORT_BEST_PROCESSOR_SIZE
         !IF(myPE == PE_IO)print*,'Exiting MFIX after REPORT_BEST_IJK_SIZE.'

         IF(myPE == PE_IO) WRITE(*,"(72('='))")

! In case of a RESTART_2, write the RES1 and SPX1 files here
! This was commented out earlier in this file.
         IF(RUN_TYPE == 'RESTART_2') THEN
            CALL WRITE_RES1
            DO LL = 1, N_SPX
               CALL WRITE_SPX0 (LL, 0)
               CALL WRITE_SPX1 (LL, 0)
            END DO
         ENDIF
      ENDIF

! Setup VTK data for regular (no cut cells) grid
      IF(.NOT.CARTESIAN_GRID.AND.WRITE_VTK_FILES) CALL SETUP_VTK_NO_CUTCELL

! Read custom particle size distributions for IC and BC, if any defined
      IF(DISCRETE_ELEMENT) THEN
        CALL PREPARE_CUSTOM_DIST(1)   ! 1 implies IC
        CALL PREPARE_CUSTOM_DIST(2)   ! 2 implies BC
      END IF

      IF(DISCRETE_ELEMENT) CALL MAKE_ARRAYS_DES
      IF(QMOMK) CALL QMOMK_MAKE_ARRAYS

! Set the inflow/outflow BCs for DEM solids
      IF(DEM_SOLIDS) CALL SET_BC_DEM
! Set the inflow/outflow BC for PIC solids
      IF(PIC_SOLIDS) CALL SET_BC_PIC_MI

! Set the initial properties of each particle.
      IF(DEM_SOLIDS) CALL SET_IC_DEM

! debug prints
      if (DBGPRN_LAYOUT .or. bdist_io) then
         !write (*,*) myPE , ' E.4 ... version = ' , version(1:33)
         call debug_write_layout()
         call write_parallel_info()
      endif

! Initializations for CPU time calculations in iterate
      CPUOS = 0.
      CALL CPU_TIME (CPU1)
      CPU_NLOG = CPU1
      TIME_NLOG = TIME - DT

! Get the initial value of CPU time
      CALL CPU_TIME (CPU0)

! Find the solution of the equations from TIME to TSTOP at
! intervals of DT
!---------------------------------------------------------------------

      NCHECK  = NSTEP
      DNCHECK = 1
      CPU_IO  = ZERO
      NIT_TOTAL = 0

      CALL INIT_OUTPUT_VARS

! Parse residual strings
      CALL PARSE_RESID_STRING ()

! Call user-defined subroutine to set constants, check data, etc.
      IF (CALL_USR) CALL USR0

      CALL RRATES_INIT()

! Calculate all the coefficients once before entering the time loop
      CALL INIT_COEFF(MFIX_DAT, IER)

      DO MM=1, MMAX
         F_gs(:,MM) = ZERO
      ENDDO

! Remove undefined values at wall cells for scalars
      CALL UNDEF_2_0 (ROP_G)
      DO MM = 1, MMAX
         CALL UNDEF_2_0 (ROP_S(:,MM))
      ENDDO

! Initialize d's and e's to zero
      DO MM = 0, MMAX
         D_E(:,MM) = ZERO
         D_N(:,MM) = ZERO
         D_T(:,MM) = ZERO
      ENDDO
      E_E(:) = ZERO
      E_N(:) = ZERO
      E_T(:) = ZERO

! calculate shear velocities if periodic shear BCs are used
      IF(SHEAR) CALL CAL_D(V_sh)

! Initialize check_mass_balance.  This routine is not active by default.
! Specify a reporting interval (hard-wired in the routine) to activate
! the routine.
      Call check_mass_balance (0)

! sof modification: now it's only needed to do this once before time-loop
! Mark the phase whose continuity will be solved and used to correct
! void/volume fraction in calc_vol_fr (see subroutine for details)
      CALL MARK_PHASE_4_COR (PHASE_4_P_G, PHASE_4_P_S, DO_CONT, MCP,&
           DO_P_S, SWITCH_4_P_G, SWITCH_4_P_S)

      END SUBROUTINE INITIALIZE

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  SUBROUTINE: GET_DATA                                                C
!  Purpose: read and verify input data, open files                     C
!                                                                      C
!  Author: P. Nicoletti                               Date: 04-DEC-91  C
!  Reviewer: M.SYAMLAL, W.ROGERS, P.NICOLETTI         Date: 24-JAN-92  C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE GET_DATA(MFIX_DAT)

! Modules
!---------------------------------------------------------------------

      USE compar, only: adjust_partition, mype, nodesi, nodesj, nodesk, pe_io
      USE cut_cell_preproc, only: write_mesh_ppo
      USE debug, only: good_config
      USE mpi_utility, only: bcast
      USE open_files_mod, only: open_files, open_mesh_file
      USE param1, only: n_spx
      USE run, only: run_type, run_name, ppo, generate_mesh
      use read_namelist_mod, only: read_namelist, read_keyword_namelist, enum_run_name, enum_enable_dmp_log, enum_full_log
      USE get_stl_data_mod, only: get_stl_data

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

      LOGICAL :: PRESENT

      LOGICAL, SAVE :: FIRST_PASS = .TRUE.

      GOOD_CONFIG = .TRUE.


      IF (FIRST_PASS) THEN
! Initialize the error manager. This call occurs after the MFIX_DAT
! is read so that message verbosity can be set and the .LOG file
! can be opened.
         CALL READ_KEYWORD_NAMELIST(MFIX_DAT, ENUM_RUN_NAME)
         CALL READ_KEYWORD_NAMELIST(MFIX_DAT, ENUM_ENABLE_DMP_LOG)
         CALL READ_KEYWORD_NAMELIST(MFIX_DAT, ENUM_FULL_LOG)
         CALL INIT_ERROR_MANAGER(RUN_NAME)
      ENDIF

! This module call routines to initialize the namelist variables.
      CALL INIT_NAMELIST
! Read in the namelist variables from the ascii input file.
      CALL READ_NAMELIST(0, MFIX_DAT, CMD_LINE_ARGS, CMD_LINE_ARGS_COUNT)
      IF(REINIT_ERROR()) RETURN

! Set RUN_TYPE to RESTART_1 when adjusting partition
! and read partition layout in gridmap.dat if it exists
      IF(ADJUST_PARTITION) THEN
         RUN_TYPE = 'RESTART_1'

         INQUIRE(FILE='gridmap.dat',EXIST=PRESENT)
         IF(PRESENT) THEN
            IF(MyPE == PE_IO) THEN
               WRITE(*,*)'Reading partition layout from gridmap.dat...'
               OPEN(UNIT=777, FILE='gridmap.dat', STATUS='OLD')

                READ (777, *) NODESI,NODESJ,NODESK

                CLOSE(777)
            ENDIF

            CALL BCAST(NODESI)
            CALL BCAST(NODESJ)
            CALL BCAST(NODESK)
         ENDIF

      ENDIF


! Turn off bDist_IO for serial runs
      if (bDist_IO.AND.numPEs==1) then
         bDist_IO = .false.
         WRITE(ERR_MSG, "(A)") "This is a serial run. BDIST_IO was turned off."
         CALL LOG_INFO()
      endif

! If anything is wrong, bail out
      IF (CHECK_EXIT_FLAG()) RETURN

      IF (FIRST_PASS) THEN

! Write header in the .LOG file and to screen.
! Not sure if the values are correct or useful
         CALL WRITE_HEADER

! Open files
         CALL OPEN_FILES(RUN_NAME, RUN_TYPE)
         FIRST_PASS = .FALSE.
      ENDIF

      CALL OPEN_MESH_FILE(RUN_NAME)

! Read stl geometry, and convert CG_* BCs to regular BCs
! if geometry is flush with the MFiX domain box
      call get_stl_data

! Check data, do computations for IC and BC locations
! and flows, and set geometry parameters such as X, X_E, DToDX, etc.
      CALL CHECK_DATA(MFIX_DAT)
! If anything is wrong, bail out
      IF (CHECK_EXIT_FLAG()) RETURN

      IF(GENERATE_MESH) THEN
         call WRITE_MESH_PPO
      ENDIF

      RETURN

    END SUBROUTINE GET_DATA


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  SUBROUTINE: CHECK_DATA                                              C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CHECK_DATA(MFIX_DAT)

! Modules
!---------------------------------------------------------------------

         USE check_boundary_conditions_mod, only: check_boundary_conditions
         USE check_chemical_rxns_mod, only: check_chemical_rxns
         USE check_data_cg, only: adjust_ijk_size, check_data_cartesian
         USE check_data_cg, only: check_data_cg_bc
         USE check_gas_phase_mod, only: check_gas_phase
         USE check_geometry_mod, only: check_geometry, check_geometry_dem, check_geometry_pic
         USE check_geometry_prereqs_mod, only: check_geometry_prereqs
         USE check_initial_conditions_mod, only: check_initial_conditions
         USE check_internal_surfaces_mod, only: check_internal_surfaces
         USE check_numerics_mod, only: check_numerics
         USE check_odepack_stiff_chem_mod, only: check_odepack_stiff_chem
         USE check_output_control_mod, only: check_output_control
         USE check_point_sources_mod, only: check_point_sources
         USE check_run_control_mod, only: check_run_control
         USE check_solids_model_prereqs_mod, only: check_solids_model_prereqs
         USE check_solids_phases_mod, only: check_solids_phases
         USE constant, only: set_constants
         USE cut_cell_preproc, only: cut_cell_preprocessing
         USE cutcell, ONLY: cartesian_grid
         USE desgrid, only: DESGRID_INIT
         USE discretelement, ONLY: discrete_element
         USE error_manager
         USE gridmap
         USE mpi_init_des, only: DESMPI_INIT
         USE run, ONLY: run_name, run_type, time
         USE set_bc0_flow_mod, only: set_bc0_flow
         USE set_ic_mod, only: set_ic
         USE stl_preproc_des, only: DES_STL_PREPROCESSING

         IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------
      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

! Local variables
!---------------------------------------------------------------------
! shift DX, DY and DZ values
      LOGICAL, PARAMETER :: SHIFT = .TRUE.
!---------------------------------------------------------------------

! These checks verify that sufficient information was provided
! to setup the domain indices and DMP gridmap.
      CALL CHECK_GEOMETRY_PREREQS; IF(REINIT_ERROR()) RETURN
      CALL CHECK_DMP_PREREQS; IF(REINIT_ERROR()) RETURN

! Set up the physical domain indices (cell index max/min values).
      CALL SET_MAX2; IF(REINIT_ERROR()) RETURN

! Set constants
      CALL SET_CONSTANTS; IF(REINIT_ERROR()) RETURN

! Adjust partition for better load balance (done when RE_INDEXING is .TRUE.)
      CALL ADJUST_IJK_SIZE; IF(REINIT_ERROR()) RETURN

! Partition the domain and set indices
      CALL GRIDMAP_INIT(postmfix=.false.); IF(REINIT_ERROR()) RETURN

! Check the minimum solids phase requirements.
      CALL CHECK_SOLIDS_MODEL_PREREQS; IF(REINIT_ERROR()) RETURN

      CALL CHECK_RUN_CONTROL; IF(REINIT_ERROR()) RETURN
      CALL CHECK_NUMERICS; IF(REINIT_ERROR()) RETURN

      CALL CHECK_GAS_PHASE(MFIX_DAT); IF(REINIT_ERROR()) RETURN
      CALL CHECK_SOLIDS_PHASES(MFIX_DAT); IF(REINIT_ERROR()) RETURN
      CALL SET_PARAMETERS; IF(REINIT_ERROR()) RETURN

! Basic geometry checks.
      CALL CHECK_GEOMETRY(SHIFT); IF(REINIT_ERROR()) RETURN
      IF(DEM_SOLIDS) CALL CHECK_GEOMETRY_DEM; IF(REINIT_ERROR()) RETURN
      IF(PIC_SOLIDS) CALL CHECK_GEOMETRY_PIC; IF(REINIT_ERROR()) RETURN

! Set grid spacing variables.
      CALL SET_GEOMETRY; IF(REINIT_ERROR()) RETURN
      IF(DISCRETE_ELEMENT) CALL SET_GEOMETRY_DES; IF(REINIT_ERROR()) RETURN

      CALL CHECK_INITIAL_CONDITIONS; IF(REINIT_ERROR()) RETURN
      CALL CHECK_BOUNDARY_CONDITIONS; IF(REINIT_ERROR()) RETURN
      CALL CHECK_INTERNAL_SURFACES; IF(REINIT_ERROR()) RETURN
      CALL CHECK_POINT_SOURCES; IF(REINIT_ERROR()) RETURN

      CALL CHECK_CHEMICAL_RXNS(MFIX_DAT); IF(REINIT_ERROR()) RETURN
      CALL CHECK_ODEPACK_STIFF_CHEM; IF(REINIT_ERROR()) RETURN

! Output control - Must be called after geometry check
      CALL CHECK_OUTPUT_CONTROL; IF(REINIT_ERROR()) RETURN


! DOMAIN SPECIFIC CHECKS
!--------------------------------------------------------------------!
! This call needs to occur before any of the IC/BC checks.
      CALL SET_ICBC_FLAG; IF(REINIT_ERROR()) RETURN

! Compute area of boundary surfaces.
      CALL GET_BC_AREA; IF(REINIT_ERROR()) RETURN

! Set the flags for identifying computational cells
      CALL SET_FLAGS; IF(REINIT_ERROR()) RETURN
! Set arrays for computing indices
      CALL SET_INCREMENTS; IF(REINIT_ERROR()) RETURN

! check_data_cartesian needs (?) to be after set_flags or it will fail
      CALL CHECK_DATA_CARTESIAN; IF(REINIT_ERROR()) RETURN
      IF(CARTESIAN_GRID) THEN
         CALL CHECK_DATA_CG_BC; IF(REINIT_ERROR()) RETURN
         CALL CUT_CELL_PREPROCESSING; IF(REINIT_ERROR()) RETURN
      ELSE
         CALL ALLOCATE_DUMMY_CUT_CELL_ARRAYS; IF(REINIT_ERROR()) RETURN
      ENDIF

      IF(DISCRETE_ELEMENT) THEN
         CALL DESGRID_INIT; IF(REINIT_ERROR()) RETURN
         CALL DESMPI_INIT; IF(REINIT_ERROR()) RETURN
         CALL DES_STL_PREPROCESSING; IF(REINIT_ERROR()) RETURN
      ENDIF

      END SUBROUTINE CHECK_DATA

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  SUBROUTINE: FINALIZE                                                C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE FINALIZE

      USE allocate_arrays_mod, only: deallocate_arrays
      USE compar, only:ADJUST_PARTITION
      USE cut_cell_preproc, only: close_cut_cell_files
      USE cutcell, only: cartesian_grid
      USE dashboard
      USE output_man
      USE parallel_mpi, only: parallel_fin
      USE run, only: dt, call_usr, dt_min, get_tunit, tunit,time,tstop
      USE run, only:ppo
      USE time_cpu
      use read_par_input_mod

      IMPLICIT NONE

! Skip finalization when doing pre-processing only
      IF(PPO) RETURN

! Call user-defined subroutine after time-loop.
      IF (CALL_USR) CALL USR3

      IF(DISCRETE_ELEMENT) CALL WRITE_PART_OUTPUT_V2P0

! Get the final value of CPU time.  The difference gives the
! CPU time used for the computations.
      CALL CPU_TIME (CPU1)

! Compute the CPU time and write it out in the .OUT file.
      CPUTIME_USED = CPU1 - CPU0 - CPU_IO
      WALLTIME_USED = WALL_TIME() - WALL0
      CALL WRITE_OUT3 (CPUTIME_USED, WALLTIME_USED, CPU_IO)

! JFD: cartesian grid implementation
      IF(WRITE_DASHBOARD) THEN
         IF(DT>=DT_MIN) THEN
            RUN_STATUS = 'Complete.'
         ELSE
            RUN_STATUS = 'DT < DT_MIN.  Recovery not possible!'
         ENDIF
         CALL GET_TUNIT(CPUTIME_USED,TUNIT)
         CALL UPDATE_DASHBOARD(0,CPUTIME_USED,TUNIT)
      ENDIF
!      IF(CARTESIAN_GRID)  CALL CLOSE_CUT_CELL_FILES

! JFD: Dynamic load balance
      IF(TIME+0.1d0*DT>=TSTOP)  ADJUST_PARTITION=.FALSE.
      IF(ADJUST_PARTITION) THEN
         CALL OUTPUT_MANAGER(.TRUE., .FALSE.)
         CALL DEALLOCATE_ARRAYS
      ELSE
! Finalize and terminate MPI
         call parallel_fin
      ENDIF

      END SUBROUTINE FINALIZE


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Subroutine: ADD_COMMAND_LINE_KEYWORD                                !
!  Author: M.Meredith                                 Date: 03-FEB-16  !
!                                                                      !
!  Purpose: Save command line arguments in CMD_LINE_ARGS array.        !
!   Passing keywords on command line is deprecated, but is used by the !
!   test suite
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE ADD_COMMAND_LINE_KEYWORD(ARG)
      implicit none
      CHARACTER(LEN=*), INTENT(IN) :: ARG

      CMD_LINE_ARGS_COUNT = CMD_LINE_ARGS_COUNT + 1

      IF (CMD_LINE_ARGS_COUNT > SIZE(CMD_LINE_ARGS)) THEN
         print *, "Too many command line arguments: ", COMMAND_ARGUMENT_COUNT()
         print *, "Only ",SIZE( CMD_LINE_ARGS )," or fewer command line arguments are supported."
         STOP
      ENDIF

      CMD_LINE_ARGS(CMD_LINE_ARGS_COUNT) = arg

   END SUBROUTINE ADD_COMMAND_LINE_KEYWORD

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Subroutine: PRINT_FLAGS                                             !
!  Author: M.Meredith                                 Date: 27-APR-16  !
!                                                                      !
!  Purpose: Print the configure flags MFIX was built with.             !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE PRINT_FLAGS
      implicit none

#ifdef MPI
      write(*,"(A)",advance="no") "dmp "
#endif

#ifdef MKL
      write(*,"(A)",advance="no") "mkl "
#endif

#ifdef _OPENMP
      write(*,"(A)",advance="no") "smp "
#endif

#ifdef MFIX_INTERACTIVE
      write(*,"(A)",advance="no") "python "
#endif

      write(*,"(A)",advance="yes") ""

   END SUBROUTINE PRINT_FLAGS

END MODULE MAIN
