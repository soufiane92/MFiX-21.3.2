#include "error.inc"
#include "version.inc"

! -*- f90 -*-
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: MFIX                                                    !
!  Author: M. Syamlal                                 Date: 29-JAN-92  !
!                                                                      !
!  Purpose: The main module in the MFIX program                        !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
!
!> \mainpage Multiphase Flow with Interphase eXchanges
!!
!! MFIX is a general-purpose computer code developed at the National
!! Energy Technology Laboratory, NETL, for describing the hydrodynamics,
!! heat transfer, and chemical reactions in fluid-solid systems.
!!
!! It has been used for describing bubbling and circulating fluidized
!! beds and spouted beds. MFiX calculations give transient data on the
!! three-dimensional distribution of pressure, velocity, temperature,
!! and species mass fractions. MFiX code is based on a generally
!! accepted set of multiphase flow equations. The code is used as a
!! "test-stand" for testing and developing multiphase flow constitutive
!!  equations.
!!
!! \section Notice
!! Neither the United States Government nor any agency thereof, nor any
!! of their employees, makes any warranty, expressed or implied, or
!! assumes any legal liability or responsibility for the accuracy,
!! completeness, or usefulness of any information, apparatus, product,
!! or process disclosed or represents that its use would not infringe
!! privately owned rights.
!!
!! * MFIX is provided without any user support for applications in the
!!   user's immediate organization. It should not be redistributed in
!!   whole or in part.
!!
!! * The use of MFIX is to be acknowledged in any published paper based
!!   on computations using this software by citing the MFIX theory
!!   manual. Some of the submodels are being developed by researchers
!!   outside of NETL. The use of such submodels is to be acknowledged
!!   by citing the appropriate papers of the developers of the submodels.
!!
!! * The authors would appreciate receiving any reports of bugs or other
!!   difficulties with the software, enhancements to the software, and
!!   accounts of practical applications of this software.
!!
!! \section Disclaimer
!! This report was prepared as an account of work sponsored by an agency
!! of the United States Government. Neither the United States Government
!! nor any agency thereof, nor any of their employees, makes any
!! warranty, express or implied, or assumes any legal liability or
!! responsibility for the accuracy, completeness, or usefulness of any
!! information, apparatus, product, or process disclosed, or represents
!! that its use would not infringe privately owned rights. Reference
!! herein to any specific commercial product, process, or service by
!! trade name, trademark, manufacturer, or otherwise does not
!! necessarily constitute or imply its endorsement, recommendation, or
!! favoring by the United States Government or any agency thereof. The
!! views and opinions of authors expressed herein do not necessarily
!! state or reflect those of the United States Government or any
!! agency thereof.



SUBROUTINE RUN_MFIX(MFIX_DAT_FILENAME, MFIX_DAT_SIZE)
!f2py threadsafe

      USE CHECK_BATCH_QUEUE_END_MOD, ONLY: CHECK_BATCH_QUEUE_END
      USE COMPAR, only:ADJUST_PARTITION, MYPE, PE_IO
      USE DES_TIME_MARCH, ONLY: DES_TIME_INIT, DES_TIME_STEP, DES_TIME_END, FACTOR, EXIT_LOOP
      USE DISCRETELEMENT, ONLY: DES_CONTINUUM_COUPLED, DISCRETE_ELEMENT
      USE ERROR_MANAGER, ONLY: INIT_ERROR_MANAGER
      USE EXIT, ONLY: EXIT_FLAG, CHECK_EXIT_FLAG
      USE FUNITS, ONLY: UNIT_DAT
      USE ISO_C_BINDING;
      USE ITERATE, ONLY: CONVERGED, DIVERGED, ADJUSTDT
      USE ITERATE, ONLY: ITERATE_INIT, DO_ITERATION, POST_ITERATE
      USE ITERATE, ONLY: LOG_CONVERGED, LOG_DIVERGED, NIT, MAX_NIT
      USE MAIN, ONLY: GET_DATA, INITIALIZE, FINALIZE
      USE MAIN, ONLY: PRINT_FLAGS
      USE PAUSE, only: WAIT_WHILE_PAUSED
      USE PIC_TIME_MARCH_MOD, ONLY: PIC_TIME_INIT, PIC_TIME_STEP, PIC_TIME_END
      USE PIC_TIME_MARCH_MOD, ONLY: S_TIME, TEND_PIC_LOOP
      USE RUN, ONLY:  DT, DEM_SOLIDS, PIC_SOLIDS, STEADY_STATE, TIME, TSTOP
      USE STEP, ONLY: CHECK_LOW_DT, CHEM_MASS, TIME_STEP_INIT, TIME_STEP_END
      USE TIME_CPU, ONLY: WALL_TIME

      IMPLICIT NONE
! Path to input file
      CHARACTER(LEN=1000), INTENT(IN) :: MFIX_DAT_FILENAME
      INTEGER, INTENT(IN) :: MFIX_DAT_SIZE

!-----------------------------------------------
! Local variables
!-----------------------------------------------

      DOUBLE PRECISION :: STARTTIME
      INTEGER :: II

      CHARACTER(MFIX_DAT_SIZE) :: MFIX_DAT

      INTEGER :: IOS

! Dynamic load balance loop
      DO

     ! Open input deck file. Report errors if the file is not located or
     ! there is difficulties opening it.

      OPEN(UNIT=UNIT_DAT, FILE=mfix_dat_filename, STATUS='OLD', IOSTAT=IOS, &
           FORM="UNFORMATTED", ACCESS="STREAM", ACTION="READ")
      IF(IOS /= 0) THEN
         IF(myPE == PE_IO) WRITE (*,1001) mfix_dat_FILENAME
         ERROR_STOP
1001     FORMAT(2/,1X,70('*')/' From: RUN_MFIX',/' Error 1001: ',    &
              'Unable to open input data file: ',A/,'Aborting.',/1x,70('*'),2/)
      ENDIF

      READ(UNIT_DAT) MFIX_DAT
      CLOSE(UNIT_DAT)

! Read input data, check data, do computations for IC and BC locations
! and flows, and set geometry parameters such as X, X_E, DToDX, etc.
      CALL GET_DATA(MFIX_DAT)

      IF (CHECK_EXIT_FLAG()) RETURN

! Initialize the simulation
      CALL INITIALIZE(MFIX_DAT)

      IF (CHECK_EXIT_FLAG()) RETURN

! Time march loop.
      dt_loop: DO WHILE (TIME + 0.1*DT < TSTOP)

         CALL WAIT_WHILE_PAUSED

         IF(DES_CONTINUUM_COUPLED .OR. .NOT.DISCRETE_ELEMENT) THEN
            call run_fluid
         ENDIF

         IF (DEM_SOLIDS) THEN
            call run_dem
            IF(.NOT.DES_CONTINUUM_COUPLED) EXIT
            IF (CHECK_EXIT_FLAG()) THEN
               CALL TIME_STEP_END
               EXIT
            ENDIF
         ENDIF

         IF (PIC_SOLIDS) THEN
            call run_pic
            IF(.NOT.DES_CONTINUUM_COUPLED) EXIT
            IF (CHECK_EXIT_FLAG()) THEN
               CALL TIME_STEP_END
               EXIT
            ENDIF
         ENDIF

! Terminate MFIX normally before batch queue terminates.
         CALL CHECK_BATCH_QUEUE_END(EXIT_FLAG)

         CALL TIME_STEP_END

! Transient or steady state simulation
         IF (STEADY_STATE .OR. ADJUST_PARTITION) EXIT

         IF (CHECK_EXIT_FLAG()) EXIT

      ENDDO dt_loop

      CALL FINALIZE
      IF(.NOT.ADJUST_PARTITION) EXIT
      ENDDO

   CONTAINS

      subroutine run_fluid
         STARTTIME = WALL_TIME()
         CALL TIME_STEP_INIT(MFIX_DAT)
         DO
            CALL ITERATE_INIT
            DO WHILE (NIT<MAX_NIT .AND. .NOT.(CONVERGED.OR.DIVERGED))
               NIT = NIT + 1
               CALL DO_ITERATION(MFIX_DAT)
               IF (CHECK_EXIT_FLAG()) RETURN
               CALL WAIT_WHILE_PAUSED
            ENDDO
            CALL POST_ITERATE
            IF (STEADY_STATE) EXIT
            IF (.NOT.ADJUSTDT(MFIX_DAT)) EXIT
         ENDDO

! Exit if DT < DT_MIN
         CALL CHECK_LOW_DT
         IF (CHECK_EXIT_FLAG()) RETURN

! Stiff Chemistry Solver.
         CALL CHEM_MASS
         IF (CHECK_EXIT_FLAG()) RETURN
         CALL PRINT_WALLTIME("Timestep walltime, fluid solver:", STARTTIME)
      end subroutine run_fluid

      subroutine run_dem
         STARTTIME = WALL_TIME()
         CALL DES_TIME_INIT
         DO II = 1, FACTOR
            CALL DES_TIME_STEP(II)
            IF (MOD(factor-II, 10) == 0) THEN
               CALL CHECK_BATCH_QUEUE_END(EXIT_FLAG)
               EXIT_LOOP = EXIT_LOOP .OR. EXIT_FLAG
            ENDIF
            IF ( EXIT_LOOP ) EXIT
            CALL WAIT_WHILE_PAUSED
         ENDDO
         CALL DES_TIME_END
         CALL PRINT_WALLTIME("Timestep walltime, DEM solver:", STARTTIME)
      end subroutine run_dem

      subroutine run_pic
! number of PIC time steps
         Integer :: PIC_ITERS

         STARTTIME = WALL_TIME()
         CALL PIC_TIME_INIT

! If the current time in the discrete loop exceeds the current time in
! the continuum simulation, exit the Lagrangian loop
         PIC_ITERS = 0
         DO WHILE(S_TIME.LT.TEND_PIC_LOOP)
            PIC_ITERS  = PIC_ITERS + 1
            CALL PIC_TIME_STEP(PIC_ITERS)
            IF (MOD(PIC_ITERS, 10) == 0) THEN
               CALL CHECK_BATCH_QUEUE_END(EXIT_FLAG)
               EXIT_LOOP = EXIT_LOOP .OR. EXIT_FLAG
            ENDIF
            IF ( EXIT_LOOP ) EXIT
            CALL WAIT_WHILE_PAUSED
         ENDDO
         CALL PIC_TIME_END(PIC_ITERS)
         CALL PRINT_WALLTIME("Timestep walltime, PIC solver:", STARTTIME)
      end subroutine run_pic

    END SUBROUTINE RUN_MFIX

SUBROUTINE PRINT_WALLTIME(LABEL, STARTTIME)
   USE ERROR_MANAGER, ONLY: ERR_MSG, LOG_MESSAGE, LOGLEVEL_INFO
   USE TIME_CPU, ONLY: WALL_TIME
   USE OUTPUT, ONLY: FULL_LOG

   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN) :: LABEL
   DOUBLE PRECISION, INTENT(IN) :: STARTTIME
   DOUBLE PRECISION :: ELAPSED_WALLTIME

   IF (FULL_LOG) THEN
      ELAPSED_WALLTIME = WALL_TIME() - STARTTIME
      WRITE(ERR_MSG, "(A, f9.3, A)" ) LABEL, ELAPSED_WALLTIME, " s"
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
   ENDIF
END SUBROUTINE PRINT_WALLTIME


PROGRAM MFIX

      USE compar, only: mype, pe_io
      USE error_manager, only: loglevel, loglevel_error, loglevel_info, loglevel_status, loglevel_warning
      USE exit, only: mfix_exit
      USE fs_util, only: file_exists
      USE funits, only: file_size
      USE iso_c_binding, only: c_bool, c_char
      USE parallel_mpi, only: parallel_init

      IMPLICIT none
      CHARACTER(LEN=1000) :: MFIX_DAT_FILENAME = "mfix.dat"
      INTEGER :: MFIX_DAT_SIZE
      INTEGER :: II

      CALL PARSE_COMMAND_LINE_ARGS

      IF (.NOT. FILE_EXISTS(MFIX_DAT_FILENAME)) THEN
         IF(myPE == PE_IO) WRITE(*, 1000) trim(mfix_dat_filename)
1000     FORMAT(2/,1X,70('*')/' From: mfix.f',/' Error 1000: ',    &
              'Input data file does not exist: ',A/,'Aborting.',/1x,   &
              70('*'),2/)

         CALL MFIX_EXIT(myPE, .TRUE.)
      ENDIF

      MFIX_DAT_SIZE = FILE_SIZE(MFIX_DAT_FILENAME)

      ! Invoke MPI/SMP initialization and get rank info.
      CALL PARALLEL_INIT

      CALL RUN_MFIX(MFIX_DAT_FILENAME, MFIX_DAT_SIZE)

   CONTAINS

   SUBROUTINE PARSE_COMMAND_LINE_ARGS

      USE main, only: add_command_line_keyword, print_flags
      USE run, only: id_version

      IMPLICIT NONE

      LOGICAL :: reading_mfix_dat = .false.
      LOGICAL :: reading_log_level = .false.
      character(len=80) :: mfix_log_level = "INFO"
      CHARACTER(LEN=1000) :: tmp

      DO II=1, COMMAND_ARGUMENT_COUNT()

         IF (reading_mfix_dat) THEN
            call get_command_argument(ii, mfix_dat_filename)
            reading_mfix_dat = .false.
            CYCLE

         ELSE IF (reading_log_level) THEN
            call get_command_argument(ii, mfix_log_level)
            reading_log_level = .false.
            CYCLE

         ELSE
            CALL GET_COMMAND_ARGUMENT(II,tmp)
         ENDIF

         IF (tmp=='-h'.or.tmp=='--help') THEN
            CALL PRINT_USAGE
            STOP
         ELSE IF (tmp=='-v'.or.tmp=='--version') THEN
            print *, ID_VERSION
            STOP
         ELSE IF (tmp=='-p'.or.tmp=='--print-flags') THEN
            CALL PRINT_FLAGS
            STOP
         ELSE IF (tmp=='-f'.or.tmp=='--file') THEN
            reading_mfix_dat = .true.
            CYCLE
         ELSE IF (tmp=='-l'.or.tmp=='--log') THEN
            reading_log_level = .true.
            CYCLE
         ELSE IF (INDEX(tmp, '-')==1 .or. INDEX(tmp, '=')==0) THEN
            ! argument does not start with - or contain =
            print *, "Unknown option: ", tmp
            CALL PRINT_USAGE
            ERROR_STOP
         ELSE
            CALL ADD_COMMAND_LINE_KEYWORD(tmp)
         ENDIF
      ENDDO

      if (mfix_log_level == "ERROR") THEN
         loglevel = LOGLEVEL_ERROR
      else if (mfix_log_level == "WARNING") THEN
         loglevel = LOGLEVEL_WARNING
      else if (mfix_log_level == "STATUS") THEN
         loglevel = LOGLEVEL_STATUS
      else if (mfix_log_level == "INFO") THEN
         loglevel = LOGLEVEL_INFO
      else
         print *, "Invalid log level: ", mfix_log_level
         print *, "Must be one of: ERROR, WARNING, STATUS, INFO"
         stop
      end if

   END SUBROUTINE PARSE_COMMAND_LINE_ARGS

   SUBROUTINE PRINT_USAGE

      CHARACTER(LEN=1000) :: solver_executable

      call get_command_argument(0, solver_executable)

      print *, "Usage: ", trim(solver_executable), " [-h,--help] [-p,--print-flags] &
         &[-f,--file <filename>] [<KEYWORD>=<VALUE> ...]"


      print *, "       -h,--help: display this help message"
      print *, "       -p,--print-flags: print flags solver was &
         &built with, such as: dmp mkl python smp"
      print *, "       -f,--file <filename>: &
         &specify filename of input file (Default: mfix.dat)"
      print *, "       <KEYWORD>=<VALUE>: specify keyword on &
         &command line, overrides values in input desk (mfix.dat or <RUN_NAME>.mfx)"
      print *, "       -l,--log <loglevel>: log message level, one of {ERROR, WARNING, STATUS, INFO}, default INFO"
      print *, "       -v,--version: print version info"
   END SUBROUTINE PRINT_USAGE

END PROGRAM MFIX
