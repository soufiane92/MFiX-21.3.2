! -*- f90 -*-
!----------------------------------------------------------------------!
! Module: ERROR_MANAGER                                                !
!                                                                      !
! Purpose: Unify error message handling.                               !
!                                                                      !
!----------------------------------------------------------------------!
MODULE ERROR_MANAGER

   use compar, only: mype, pe_io, numPEs
   use compar, only: ADJUST_PARTITION
   use debug, only: good_config
   use exit, only: mfix_exit, exit_flag
   use funits, only: dmp_log, unit_log
   use mpi_utility, only: GLOBAL_ALL_SUM
   use open_file_mod, only: open_file
   use output, only: ENABLE_DMP_LOG, FULL_LOG
   use param1, only: UNDEFINED_C
   use, intrinsic :: ISO_C_BINDING

   implicit none

! Interface
!---------------------------------------------------------------------//
   interface iVal
      module procedure iVal_int
      module procedure iVal_dbl
      module procedure iVal_log
   end interface iVal

! Maximum number of lines a message can have before a flush is needed.
   INTEGER, PARAMETER :: LINE_COUNT = 32
! Maximum size of ERR_MSG buffer
   INTEGER, PARAMETER :: LINE_LENGTH = 8192

! Character string for storing the error message.
   CHARACTER(LEN=LINE_LENGTH), DIMENSION(LINE_COUNT) :: ERR_MSG

! Depth that the current call tree can go.
   INTEGER, PARAMETER, PRIVATE :: MAX_CALL_DEPTH = 16
! Current call depth.
   INTEGER, PRIVATE :: CALL_DEPTH

! The name of the calling routine. Set by calling: INIT_ERR_MSG
   CHARACTER(LEN=128), DIMENSION(MAX_CALL_DEPTH), PRIVATE :: CALLERS

! Error Flag.
   INTEGER :: IER_EM

   ENUM, BIND(C)
      ENUMERATOR :: LOGLEVEL_ERROR=1, LOGLEVEL_WARNING=2, LOGLEVEL_STATUS=3, LOGLEVEL_INFO=4
   END ENUM
   INTEGER(kind=c_int) :: LOGLEVEL

contains

!``````````````````````````````````````````````````````````````````````!
! Subroutine: INIT_ERROR_MANAGER                                       !
!                                                                      !
! Purpose: Initialize the error manager. This routine also opens the   !
! .LOG file(s) based on user input settings.                           !
!......................................................................!
   SUBROUTINE INIT_ERROR_MANAGER(RUN_NAME)

      implicit none

      CHARACTER(len=*), intent(in) :: RUN_NAME

! Local Variables:
!---------------------------------------------------------------------//
! Log file name.
      CHARACTER(len=255) :: LOGFILE
      CHARACTER(len=255) :: FILE_NAME
! First non-blank character in run_name.
      INTEGER :: NB
! Integer error flag
      INTEGER :: IER(0:numPEs - 1)
      CHARACTER(len=256) :: IMSG(0:numPEs - 1)

! Initialize the error flags.
      IER = 0
      IER_EM = 0
! Initialize the call tree depth.
      CALL_DEPTH = 0
! Clear the error message storage container.
      ERR_MSG = ''
! Clear the caller routine information.
      CALLERS = ''

! This turns on error messaging from all processes.
      DMP_LOG = (myPE == PE_IO) .OR. ENABLE_DMP_LOG

! Verify the length of user-provided name.
      LOGFILE = ''
      NB = INDEX(RUN_NAME, ' ')
! RUN_NAME length too short.
      IF (RUN_NAME == UNDEFINED_C .OR. NB <= 1) THEN
         IF (myPE == PE_IO) WRITE (*, 1000) 'short'
         CALL MFIX_EXIT(myPE)
! RUN_NAME length too long.
      ELSEIF (NB + 10 > LEN(LOGFILE)) THEN
         IF (myPE == PE_IO) WRITE (*, 1000) 'long'
         CALL MFIX_EXIT(myPE)
! RUN_NAME length just right.
      ELSE
! Specify the .LOG file name based on MPI Rank extension.
         IF (myPE == 0 .OR. .NOT. ENABLE_DMP_LOG) THEN
            WRITE (LOGFILE, "(A)") RUN_NAME(1:(NB - 1))
         ELSEIF (numPEs < 10) THEN
            WRITE (LOGFILE, "(A,'_',I1.1)") RUN_NAME(1:(NB - 1)), myPE
         ELSEIF (numPEs < 100) THEN
            WRITE (LOGFILE, "(A,'_',I2.2)") RUN_NAME(1:(NB - 1)), myPE
         ELSEIF (numPEs < 1000) THEN
            WRITE (LOGFILE, "(A,'_',I3.3)") RUN_NAME(1:(NB - 1)), myPE
         ELSEIF (numPEs < 10000) THEN
            WRITE (LOGFILE, "(A,'_',I4.4)") RUN_NAME(1:(NB - 1)), myPE
         ELSE
            WRITE (LOGFILE, "(A,'_',I8.8)") RUN_NAME(1:(NB - 1)), myPE
         ENDIF
      ENDIF

! Open the .LOG file. From here forward, all routines should store
! error messages (at a minimum) in the .LOG file.
      IF (DMP_LOG) THEN
         CALL OPEN_FILE(LOGFILE, UNIT_LOG, '.LOG', FILE_NAME, &
                        'APPEND', 'SEQUENTIAL', 'FORMATTED', 132, IER(myPE), IMSG(myPE))
      ENDIF

! Verify that the .LOG file was successfully opened. Otherwise, flag the
! error and abort. Currently skipped when adjusting partition.
      CALL GLOBAL_ALL_SUM(IER)
      IF (sum(IER) /= 0 .AND. .NOT. ADJUST_PARTITION) THEN
         IF (myPE == PE_IO) WRITE (*, 1001) trim(FILE_NAME)
         CALL MFIX_EXIT(myPE)
      ENDIF

      RETURN

1000  FORMAT(2/, 1X, 70('*')/' From: INIT_ERROR_MANAGER', / &
             ' Error 1000: RUN_NAME too ', A, '. ', /1x, 70('*'), 2/)

1001  FORMAT(2/, 1X, 70('*')/' From: INIT_ERROR_MANAGER', / &
             ' Error 1001: Failed to open log file: ', A, /' Aborting run.'/, &
             1x, 70('*'), 2/)

   END SUBROUTINE INIT_ERROR_MANAGER

!``````````````````````````````````````````````````````````````````````!
!                                                                      !
!......................................................................!
   CHARACTER(len=32) FUNCTION iVar(VAR, i1, i2, i3)

      CHARACTER(len=*), intent(in) :: VAR

      INTEGER, intent(in) :: i1
      INTEGER, OPTIONAL, intent(in) :: i2
      INTEGER, OPTIONAL, intent(in) :: i3

      CHARACTER(len=16) :: iASc
      CHARACTER(len=64) :: tVAR

      iASc = ''; WRITE (iASc, *) i1
      tVar = ''; WRITE (tVar, "(A,'(',A)") &
 trim(adjustl(VAR)), trim(adjustl(iASc))

      IF (PRESENT(i2)) THEN
         iASc = ''; WRITE (iASc, *) i2
         WRITE (tVar, "(A,',',A)") trim(tVar), trim(adjustl(iASc))
      ENDIF

      IF (PRESENT(i3)) THEN
         iASc = ''; WRITE (iASc, *) i3
         WRITE (tVar, "(A,',',A)") trim(tVar), trim(adjustl(iASc))
      ENDIF

      WRITE (tVar, "(A,')')") trim(tVar)

      iVar = trim(adjustl(tVar))

      RETURN
   END FUNCTION iVar

!``````````````````````````````````````````````````````````````````````!
!                                                                      !
!......................................................................!
   CHARACTER(len=32) FUNCTION iVal_int(VAL)
      INTEGER, intent(in) :: VAL

      CHARACTER(len=32) :: iASc

      WRITE (iASc, *) VAL
      iVal_int = trim(adjustl(iASc))

   END FUNCTION iVal_int

!``````````````````````````````````````````````````````````````````````!
!                                                                      !
!......................................................................!
   CHARACTER(len=32) FUNCTION iVal_dbl(VAL)
      DOUBLE PRECISION, intent(in) :: VAL

      CHARACTER(len=32) :: dASc

      IF (abs(VAL) < 1.0d-2 .AND. abs(VAL) < 1.0d2) THEN
         WRITE (dASc, "(F18.4)") VAL
      ELSE
         WRITE (dASc, "(G18.4)") VAL
      ENDIF

      iVal_dbl = trim(adjustl(dASc))

   END FUNCTION iVal_dbl

!``````````````````````````````````````````````````````````````````````!
!                                                                      !
!......................................................................!
   CHARACTER(len=32) FUNCTION iVal_log(VAL)
      LOGICAL, intent(in) :: VAL

      IF (VAL) THEN
         iVal_log = ".TRUE."
      ELSE
         iVal_log = ".FALSE."
      ENDIF

      RETURN
   END FUNCTION iVal_log

!``````````````````````````````````````````````````````````````````````!
! Function: Reports TRUE if one or more processes set an ABORT flag.   !
!......................................................................!
   LOGICAL FUNCTION REINIT_ERROR()

      CALL GLOBAL_ALL_SUM(IER_EM)
      REINIT_ERROR = (IER_EM /= 0) .OR. EXIT_FLAG
      GOOD_CONFIG = .NOT. REINIT_ERROR
      IER_EM = 0
      RETURN
   END FUNCTION REINIT_ERROR

!``````````````````````````````````````````````````````````````````````!
! Subroutine: LOG_MESSAGE                                              !
!                                                                      !
! Purpose: Log an error, warning, or informational message.            !
!......................................................................!
   subroutine log_message(filename, lineno, msg_loglevel, &
                          HEADER, FOOTER, NO_NEW_LINE, ABORT, LOG, SCR_LOG)

      character(len=*), intent(in) :: filename
      integer, intent(in) :: lineno
      integer(kind=c_int), intent(in) :: msg_loglevel
      logical, intent(in), optional :: header, footer, no_new_line, abort, log, scr_log
      character(len=1000) :: err_buf

      if (loglevel < msg_loglevel) then
         ERR_MSG = ''
         return
      end if
#ifdef MFIX_INTERACTIVE
      call write_header
#endif
      call write_msg
#ifdef MFIX_INTERACTIVE
      call write_footer
#endif
      ERR_MSG = ''

! Abort the run if specified.
      IF (present(abort)) THEN
         IF (abort) THEN
#ifdef MFIX_INTERACTIVE
            IER_EM = 1
#endif
            CALL MFIX_EXIT(myPE)
         ENDIF
      ENDIF

   contains

!``````````````````````````````````````````````````````````````````````!
! Subroutine: WRITE_LOCATION                                           !
!                                                                      !
! Purpose: Write log level and source code file/line of message        !
!......................................................................!
      subroutine write_location
         character(len=10) :: msg_type
         integer :: src_path_offset, status, idx
         character(4096) :: curdir
         character(:), ALLOCATABLE :: short_name
         ! Strip off mfix source path to keep message from being too long
         ! (Is this a good idea?)
         src_path_offset = len(__FILE__) - len("error_manager_mod.f")

         short_name = filename
         if (filename(:src_path_offset) .EQ. __FILE__(:src_path_offset)) then
            short_name = filename(src_path_offset+1:)
         else
            status = getcwd(curdir) ! Strip off project directory, eg UDF files
            if (status .eq. 0) then
               src_path_offset = len(trim(curdir))
               idx = scan(curdir, '/\\', back=.TRUE.)
               if (idx .ge. 0) then
                  src_path_offset = idx - 1
               end if
               if (filename(:src_path_offset) .EQ. curdir(:src_path_offset)) then
                  short_name = trim(filename(src_path_offset+2:))
                  end if
               end if
         end if

         if (msg_loglevel == LOGLEVEL_ERROR) then
            msg_type = "ERROR"
         else if (msg_loglevel == LOGLEVEL_WARNING) then
            msg_type = "WARNING"
         else if (msg_loglevel == LOGLEVEL_STATUS) then
            msg_type = "STATUS"
         else if (msg_loglevel == LOGLEVEL_INFO) then
            msg_type = "MESSAGE"
         end if
         write (err_buf, '(A,A,A,A,I0)') trim(msg_type), " from ", &
            short_name, ":", int(lineno)
         call write_l(trim(err_buf), log)
      end subroutine write_location

!``````````````````````````````````````````````````````````````````````!
! Subroutine: WRITE_MSG                                                !
!                                                                      !
! Purpose: Write ERR_MSG                                               !
!......................................................................!
      subroutine write_msg
         integer :: nn, ii
         do ii = 1, LINE_COUNT
            nn = len(trim(err_msg(ii)))
            if (nn .ne. 0 .and. nn .ne. 256) call write_l(trim(err_msg(ii)), log)
         enddo
      end subroutine write_msg

!``````````````````````````````````````````````````````````````````````!
! Subroutine: WRITE_HEADER                                             !
!                                                                      !
! Purpose: Write leading    >>>>--- >>>> === or >>>> #### delimiters   !
!......................................................................!
      subroutine write_header
         if (present(header)) then
            if (.not. header) return
         endif
         call write_l(NEW_LINE(''), log)
         if (msg_loglevel .eq. LOGLEVEL_ERROR) write (err_buf, 1111)
         if (msg_loglevel .eq. LOGLEVEL_WARNING) write (err_buf, 2222)
         if (msg_loglevel .eq. LOGLEVEL_INFO) write (err_buf, 3333)
         if (msg_loglevel .eq. LOGLEVEL_STATUS) return
1111     format(1x, 5('>'), 65('#'))
2222     format(1x, 5('>'), 65('='))
3333     format(1x, 5('>'), 65('-'))
         call write_l(trim(err_buf), log)

         call write_location

      end subroutine write_header

!``````````````````````````````````````````````````````````````````````!
! Subroutine: WRITE_FOOTER                                             !
!                                                                      !
! Purpose: Write trailing   <<<<--- <<<< === or <<<< #### delimiters   !
!......................................................................!
      subroutine write_footer
         if (present(footer)) then
            if (.not. footer) return
         endif
         if (msg_loglevel .eq. LOGLEVEL_ERROR) write (err_buf, 1110)
         if (msg_loglevel .eq. LOGLEVEL_WARNING) write (err_buf, 2220)
         if (msg_loglevel .eq. LOGLEVEL_INFO) write (err_buf, 3330)
         if (msg_loglevel .eq. LOGLEVEL_STATUS) return

1110     format(1x, 5('<'), 65('#'))
2220     format(1x, 5('<'), 65('='))
3330     format(1x, 5('<'), 65('-'))
         call write_l(trim(err_buf), log)
      end subroutine write_footer

!``````````````````````````````````````````````````````````````````````!
! Subroutine: WRITE_L                                                  !
!                                                                      !
! Purpose: Write a string to standard output and .LOG file.            !
!......................................................................!
      subroutine write_l(str, log, scr_log)

         character(len=*), intent(in) :: str
         logical, intent(in), optional :: log, scr_log
         logical :: do_unit_log, do_scr_log

         do_unit_log = dmp_log
         if (present(log)) then
            do_unit_log = do_unit_log .and. log
         endif

         do_scr_log = (myPE == PE_IO)

         if (present(scr_log)) then
            do_scr_log = do_scr_log .and. scr_log
         endif

         if (present(no_new_line)) then
            if (no_new_line) then
               if (do_unit_log) write (unit_log, '(a)', advance="no") str
               if (do_scr_log) write (*, '(a)', advance="no") str
               return
            endif
         endif
         if (do_unit_log) write (unit_log, '(a)') str
         if (do_scr_log) write (*, '(a)') str

      end subroutine write_l

   end subroutine log_message

END MODULE ERROR_MANAGER
