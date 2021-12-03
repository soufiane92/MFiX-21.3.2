#include "error.inc"

MODULE OPEN_FILES_MOD

   USE cdist, only: bdist_io, bstart_with_one_res
   USE compar, only: myPE, pe_io, numpes, adjust_partition
   USE error_manager
   USE funits, only: DMP_LOG, LOG_WAS_CLOSED
   USE funits, only: UNIT_LOG, UNIT_OUT, UNIT_RES, UNIT_SPX
   USE funits, only: UNIT_MSH
   USE machine, only: open_n1
   USE mpi_utility, only: GLOBAL_ALL_SUM
   use mpi_utility, only: BCAST
   USE open_file_mod, only: OPEN_FILE_NEW_WITH_EXISTING, OPEN_FILE_OLD_WITH_MISSING
   USE param1, only: n_spx
   USE run, only: RUN_NAME, RUN_TYPE
   USE run, only: PPO, MESH_FILE_PRESENT, GENERATE_MESH

! Generic SPx end characters in order.
   CHARACTER(len=35), parameter :: EXT_END = '123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: OPEN_FILES                                             !
!  Author: P. Nicoletti                               Date: 12-DEC-91  !
!                                                                      !
!  Purpose: open all the files for this run                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE OPEN_FILES(RUN_NAME, RUN_TYPE)

      IMPLICIT NONE

! Error index: 0 - no error, 1 could not open file
      INTEGER :: IER(0:numPEs - 1)
      CHARACTER(LEN=256) :: IMSG(0:numPEs - 1)
! RUN_NAME (as specified in input file)
      CHARACTER(LEN=*) :: RUN_NAME
! Run_type (as specified in input file)
      CHARACTER(LEN=*) :: RUN_TYPE
! run_name + extension
      CHARACTER(len=255) :: FILE_NAME
! Index to first blank character in FILENAME
      INTEGER :: NB
!-----------------------------------------------

! Initialize the error flag array.
      IER = 0

! Only PE_IO opens the RUN_NAME.OUT file.
      IF (myPE == PE_IO.AND.(.NOT.PPO)) THEN
         CALL OPEN_FILE(RUN_NAME, UNIT_OUT, '.OUT', FILE_NAME, 'UNKNOWN', 'SEQUENTIAL', 'FORMATTED', 132, IER(myPE), IMSG(myPE))
      END IF

! Check if there was an error opening the file.
      IF (ERROR_OPENING(IER)) THEN
         WRITE (ERR_MSG, 3000)
         CALL LOG_ERROR()
      ENDIF

! Open the RES and SPx files. By default, only PE_IO opens these files,
! but all ranks open a rank-specific copy for distributed IO runs.
      SELECT CASE (TRIM(RUN_TYPE))

! Open the RES and SPx files for a new run.
!......................................................................
      CASE ('NEW')

         IF ((myPE == PE_IO .OR. bDist_IO).AND.(.NOT.PPO)) THEN
            call check_open_res("NEW")
            call check_open_spx("NEW")
         ENDIF

! Open the RES and SPx files for a typical restart run.
!......................................................................
      CASE ('RESTART_1')

! Open the RES file.
         IF (myPE == PE_IO .or. bDist_IO) THEN
            call check_open_res("OLD")
            call check_open_spx("OLD")
         ENDIF

! Open the RES and SPx files for a typical restart run.
!......................................................................
      CASE ('RESTART_2')
! Open the RES file.
         IF (myPE == PE_IO .OR. bDist_IO) THEN
            IF (bStart_with_one_res) THEN
               call check_open_res('UNKNOWN')
               call check_open_spx('UNKNOWN')
            else
               call check_open_res('OLD')
               call check_open_spx('NEW')
            endif
         ENDIF

      CASE DEFAULT
         WRITE (ERR_MSG, 3000)
3000     FORMAT('Error 3000: Invalid run type: ', A)
         CALL LOG_WARNING()

      END SELECT

! If an error was detected, abort the run.
      IF (ERROR_OPENING(IER)) call log_error()


      RETURN

   CONTAINS

      SUBROUTINE CHECK_OPEN_RES(CSTATUS)
         IMPLICIT NONE
         character(len=*), intent(in) :: cstatus

         CALL OPEN_FILE_AND_CHECK_ERROR(CSTATUS, UNIT_RES, '.RES')
! .RES is only opened to check for errors and ensure it exists, and is now
! closed again. It is reopened/closed in each call to WRITE_RES0 and WRITE_RES1
         CLOSE (UNIT_RES)

      END SUBROUTINE CHECK_OPEN_RES

      SUBROUTINE CHECK_OPEN_SPX(CSTATUS)
         IMPLICIT NONE
         character(len=*), intent(in) :: cstatus

         CALL OPEN_SPX(CSTATUS)
! .SPX are only opened to check for errors and ensure it exists, and is now
! closed again. It is reopened/closed in each call to WRITE_SPX0 and WRITE_SPX1
         CALL CLOSE_SPX
      END SUBROUTINE CHECK_OPEN_SPX

!``````````````````````````````````````````````````````````````````````!
! FUNCTION: ERROR_OPENING                                              !
! Purpose: Collect the error flags from all processes and sum them.    !
! RESULT: .TRUE.  :: Sum of IER over all processes is non-zero.        !
!         .FALSE. :: GLOBAL_ALL_SUM is zero.                           !
!                                                                      !
!......................................................................!
      LOGICAL FUNCTION ERROR_OPENING(IER_l)

         IMPLICIT NONE

! Array containing error flags from all ranks.
         INTEGER, INTENT(IN) :: IER_L(0:numPEs - 1)
! Initialize error flags.
         ERROR_OPENING = .FALSE.
! Globally collect flags.
         CALL GLOBAL_ALL_SUM(IER)
! Report errors.
         IF (sum(IER_l) /= 0 .AND. .NOT. ADJUST_PARTITION) ERROR_OPENING = .TRUE.

         RETURN
      END FUNCTION ERROR_OPENING

   END SUBROUTINE OPEN_FILES

   SUBROUTINE OPEN_SPX(CSTATUS)
      IMPLICIT NONE
      character(len=*), intent(in) :: cstatus
      character(len=4) :: EXT
      INTEGER :: LC

! Initialize the generic SPx extension.
      EXT = '.SPx'

      DO LC = 1, N_SPX
         EXT(4:4) = EXT_END(LC:LC)
         CALL OPEN_FILE_AND_CHECK_ERROR(CSTATUS, UNIT_SPX + LC, EXT)
      ENDDO
   END SUBROUTINE OPEN_SPX

   SUBROUTINE CLOSE_SPX()
      IMPLICIT NONE
      INTEGER :: LC

      DO LC = 1, N_SPX
         CLOSE (UNIT_SPX + LC)
      ENDDO
   END SUBROUTINE CLOSE_SPX

   SUBROUTINE OPEN_FILE_AND_CHECK_ERROR(CSTATUS, UNIT_NUMBER, EXTENSION)

      IMPLICIT NONE

      character(len=*), intent(in) :: cstatus, extension
      integer, intent(in) :: unit_number

! Error index: 0 - no error, 1 could not open file
      INTEGER :: IER(0:numPEs - 1)
      CHARACTER(len=256) :: IMSG(0:numPEs - 1)
! Character error code.
      CHARACTER(len=32) :: CER
! run_name + extension
      CHARACTER(len=255) :: FILE_NAME

      IER = 0

      CALL OPEN_FILE(RUN_NAME, unit_number, EXTENSION, FILE_NAME, cstatus, 'DIRECT', 'UNFORMATTED', OPEN_N1, IER(myPE), IMSG(myPE))
      IF (IER(myPE) /= 0) THEN
         IF (IER(myPE) == OPEN_FILE_NEW_WITH_EXISTING) THEN
            WRITE (ERR_MSG, 1000) RUN_TYPE, trim(FILE_NAME)
            CALL LOG_ERROR()
1000        FORMAT('File exists but RUN_TYPE=', A/, 'Cannot create file: ', A)
         ELSEIF (IER(myPE) == OPEN_FILE_OLD_WITH_MISSING) THEN
            WRITE (ERR_MSG, 1001) RUN_TYPE, trim(FILE_NAME)
            CALL LOG_ERROR()
1001        FORMAT('File missing but RUN_TYPE=', A/, 'Cannot open file: ', A)
         ELSE
            CER = ''; WRITE (CER, *)
            WRITE (ERR_MSG, 2000) trim(FILE_NAME), trim(IMSG(myPE))
            CALL LOG_ERROR()
2000        FORMAT('Unknown error opening file ', A/, 'Error message: ', A)
         ENDIF
         RETURN
      ENDIF
   END SUBROUTINE OPEN_FILE_AND_CHECK_ERROR

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: OPEN_PE_LOG                                            !
!  Author: P. Nicoletti                               Date: 12-DEC-91  !
!                                                                      !
!  Purpose: Every rank open a .LOG file for domain specific errors.    !
!  This routine should only be invoked before writing to the log and   !
!  exiting.                                                            !
!                                                                      !
!  This routine only opens files when the following are met:           !
!    (1) MFIX is run in DMP parallel (MPI)                             !
!    (2) ENABLE_DMP_LOG keyword is not set.                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE OPEN_PE_LOG(IER)

      use, intrinsic :: iso_fortran_env, only: output_unit

      IMPLICIT NONE

! Dummy Arguments:
!---------------------------------------------------------------------//
! Error index.
      INTEGER, INTENT(out) :: IER

! Local Variables:
!---------------------------------------------------------------------//
! Log file name.
      CHARACTER(len=255) :: LOGFILE
      CHARACTER(len=255) :: FILE_NAME
! Flag for LOG files that are already open.
      LOGICAL :: DO_NOTHING
! Index of first blank character in RUN_NAME
      INTEGER :: NB
! Error message.
      CHARACTER(len=256) :: IMSG
!......................................................................!

! Enable output from this rank.
      DMP_LOG = .TRUE.

! Return to the caller if this rank is already connect to a log file.
      INQUIRE (UNIT=UNIT_LOG, OPENED=DO_NOTHING)
      IF (DO_NOTHING) RETURN

! Flag that the log had to be opened.
      LOG_WAS_CLOSED = .TRUE.

! Verify the length of user-provided name.
      LOGFILE = ''
      NB = INDEX(RUN_NAME, ' ')

! Specify the .LOG file name based on MPI Rank extension.
      IF (myPE == 0) THEN
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

! Open the .LOG file. From here forward, all routines should store
! error messages (at a minimum) in the .LOG file.
      NB = len_trim(LOGFILE) + 1
      CALL OPEN_FILE(LOGFILE, UNIT_LOG, '.LOG', FILE_NAME, 'APPEND', 'SEQUENTIAL', 'FORMATTED', 132, IER, IMSG)

      RETURN
   END SUBROUTINE OPEN_PE_LOG

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: CLOSE_PE_LOG                                           !
!  Author: P. Nicoletti                               Date: 12-DEC-91  !
!                                                                      !
!  Purpose: Every rank open a .LOG file for domain specific errors.    !
!  This routine should only be invoked before writing to the log and   !
!  exiting.                                                            !
!                                                                      !
!  This routine only opens files when the following are met:           !
!    (1) MFIX is run in DMP parallel (MPI)                             !
!    (2) ENABLE_DMP_LOG keyword is not set.                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE CLOSE_PE_LOG

      IMPLICIT NONE

! The log had to be opened for global error.
      IF (LOG_WAS_CLOSED) THEN
! Reset the flag.
         LOG_WAS_CLOSED = .FALSE.
! Disable output from this rank and close connection to LOG.
         DMP_LOG = .FALSE.
! Return to the caller if this rank is already connect to a log file.
         CLOSE (UNIT_LOG)
      ENDIF

      RETURN
   END SUBROUTINE CLOSE_PE_LOG

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: OPEN_MESH_FILE                                         !
!  Author: Jeff Dietiker                              Date: 18-DEC-20  !
!                                                                      !
!  Purpose: open the mesh file                                         !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE OPEN_MESH_FILE(RUN_NAME)

      IMPLICIT NONE

! Error index: 0 - no error, 1 could not open file
      INTEGER :: IER(0:numPEs - 1)
      CHARACTER(LEN=256) :: IMSG(0:numPEs - 1)
! RUN_NAME (as specified in input file)
      CHARACTER(LEN=*) :: RUN_NAME
! run_name + extension
      CHARACTER(len=255) :: FILE_NAME
! Index to first blank character in FILENAME
      INTEGER :: NB
!-----------------------------------------------

! Initialize the error flag array.
      IER = 0


! Open the mesh file
      IF(myPE==PE_IO) THEN

! Find first blank character of RUN_NAME
         NB = INDEX(RUN_NAME,' ')

         WRITE(FILE_NAME,4000) RUN_NAME(1:NB-1), '.msh'
         INQUIRE(FILE=FILE_NAME,EXIST=MESH_FILE_PRESENT)
         IF(MESH_FILE_PRESENT) THEN
            IF(.NOT.bDist_IO) THEN
               WRITE(ERR_MSG, 5000) trim(FILE_NAME)
               CALL LOG_INFO()
            ELSE
               MESH_FILE_PRESENT = .FALSE.
               WRITE(ERR_MSG, 5005) trim(FILE_NAME)
               CALL LOG_INFO()
            ENDIF
         ELSE
            WRITE(ERR_MSG, 5010) trim(FILE_NAME)
            CALL LOG_INFO()
         ENDIF

         IF(PPO.OR.(.NOT.MESH_FILE_PRESENT)) THEN
            GENERATE_MESH = .TRUE.
         ELSE
            GENERATE_MESH = .FALSE.
         ENDIF

         CALL OPEN_FILE (RUN_NAME, UNIT_MSH, '.msh', FILE_NAME, &
               'UNKNOWN', 'DIRECT', 'UNFORMATTED', OPEN_N1, IER(myPE),IMSG(myPE))
! Report errors.
            IF (IER(myPE) /= 0) THEN
               WRITE(ERR_MSG, 5020) trim(FILE_NAME),IER(myPE)
               CALL LOG_ERROR()
            else
               WRITE(ERR_MSG, 5030) trim(FILE_NAME)
               CALL LOG_STATUS()
            ENDIF

      ENDIF

      CALL BCAST(GENERATE_MESH)
      CALL BCAST(MESH_FILE_PRESENT)

      RETURN

 4000 FORMAT(A,A4)

 5000 FORMAT('The following mesh file was found: ',A, /                &
             'The mesh file will be used instead of mesh generation.')

 5005 FORMAT('The following mesh file was found: ',A, /                &
             'However reading the mesh file with distributed I/O is not suupported.', / &
             'Mesh generation will be performed (no mesh file will be written).')


 5010 FORMAT('The following mesh file was not found: ',A, /                &
             'Mesh generation will be performed and a mesh file will be saved.')

 5020 FORMAT('Error 4000: Unknown error while opening mesh file: ',A,' IER=',I5)

 5030 FORMAT('The following mesh file was successfully opened: ',A)


   END SUBROUTINE OPEN_MESH_FILE
END MODULE OPEN_FILES_MOD
