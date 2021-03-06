MODULE OPEN_FILE_MOD

   use cdist, only: bdist_io, bstart_with_one_res
   use compar, only: adjust_partition, mype, pe_io

! NEW run with existing files in directory
   INTEGER, PARAMETER :: OPEN_FILE_NEW_WITH_EXISTING = 12345

! OLD run missing RES and/or SPx files
   INTEGER, PARAMETER :: OPEN_FILE_OLD_WITH_MISSING = 54321
! File exists but OPEN_STAT was not REPLACE or APPEND
   INTEGER, PARAMETER :: OPEN_FILE_INVALID_ARGS = 32415

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: OPEN_FILE                                              C
!  Purpose: open a file                                                C
!                                                                      C
!  Author: P. Nicoletti                               Date: 12-DEC-91  C
!  Reviewer: P. Nicoletti, W. Rogers, M. Syamlal      Date: 24-JAN-92  C
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
   SUBROUTINE OPEN_FILE(FILENAME, IUNIT, EXT, FULL_NAME,        &
      OPEN_STAT, OPEN_ACCESS, OPEN_FORM, IRECL, IER, IMSG)

      IMPLICIT NONE

! Dummy Arguments
!---------------------------------------------------------------------//
! FILENAME (without extension)
      CHARACTER(LEN=*), INTENT(IN) :: FILENAME
! File extension.
      CHARACTER(LEN=*), INTENT(IN) :: EXT
! FILENAME + EXTENSION
      CHARACTER(LEN=*), INTENT(OUT) :: FULL_NAME
! File status (NEW, OLD, UNKNOWN)
      CHARACTER(LEN=*), INTENT(IN) :: OPEN_STAT
! File access method ('SEQUENTIAL', 'DIRECT')
      CHARACTER(LEN=*), INTENT(IN) :: OPEN_ACCESS
! Open form ('FORMATTED' or 'UNFORMATTED')
      CHARACTER(LEN=*)   OPEN_FORM
! Unit number to open
      INTEGER, INTENT(IN) :: IUNIT
! Record length
      INTEGER, INTENT(IN) :: IRECL

! Error message
      INTEGER, INTENT(OUT) :: IER
      CHARACTER(LEN=*), INTENT(OUT) :: IMSG

! Local Variables
!---------------------------------------------------------------------//
! Logical used to store result of file INQUIRE
      LOGICAL :: FILE_EXISTS

! Logicals that determine if files should be index.
      LOGICAL :: RES_IDX  ! Index RES files
      LOGICAL :: SPX_IDX  ! Index SPx files
      LOGICAL :: USE_IDX  ! Use the IDX value

! Index to first blank character in FILENAME
      INTEGER :: NB

! Initialize the error flag.
      IER = 0

! Get the length of RUN_NAME. Note that len_trim would allow the
! name to still contain spaces. The following approach truncates
! RUN_NAME at the first blank character.
      NB = INDEX(FILENAME,' ')

! Conditions for indexing the RES files for distributed IO.
      RES_IDX = (myPE .NE. PE_IO) .OR. (.NOT.bStart_with_one_RES)
! Conditions for indexing the SPX files for distributed IO.
      SPX_IDX = .TRUE.

! Flag for indexing files.
      USE_IDX = bDist_IO .AND. (                                       &
         (SPX_IDX .AND. (EXT(2:3) .EQ. 'SP')) .OR.                     &
         (RES_IDX .AND. (EXT(2:4) .EQ. 'RES')))

! Construct the file name.
      FULL_NAME = ''
      IF(USE_IDX)THEN
         WRITE(FULL_NAME,1000) FILENAME(1:NB-1), myPE, EXT(1:4)
      ELSE
         WRITE(FULL_NAME,1001) FILENAME(1:NB-1), EXT(1:4)
      ENDIF

! Check to see if the file already exists in the run directory.
      INQUIRE(FILE=trim(FULL_NAME),EXIST=FILE_EXISTS)

! NEW files should not be in the run directory.
      IF(FILE_EXISTS .AND. (OPEN_STAT == 'NEW')) THEN
         IER = OPEN_FILE_NEW_WITH_EXISTING
         IF(.NOT.ADJUST_PARTITION) THEN
            RETURN
         ENDIF
! OLD files must be in the run directory.
      ELSEIF(.NOT. FILE_EXISTS .AND. OPEN_STAT .EQ. 'OLD') THEN
         IER = OPEN_FILE_OLD_WITH_MISSING
         RETURN
      ENDIF

! Open direct access files.
      IF (OPEN_ACCESS == 'DIRECT') THEN
         OPEN(UNIT=IUNIT, FILE=trim(FULL_NAME), STATUS=OPEN_STAT,     &
            RECL=IRECL, ACCESS=OPEN_ACCESS, FORM=OPEN_FORM, IOSTAT=IER, &
            IOMSG=IMSG, CONVERT='BIG_ENDIAN')
      ELSE
! No matter the status passed to the routine, the file is created as
! NEW if it doesn't exist in the run directory.
         IF(.NOT.FILE_EXISTS) THEN
            OPEN(UNIT=IUNIT, FILE=trim(FULL_NAME), STATUS='NEW',       &
               ACCESS=OPEN_ACCESS, FORM=OPEN_FORM, IOSTAT=IER, IOMSG=IMSG)
         ELSEIF(OPEN_STAT == 'REPLACE') THEN
            OPEN(UNIT=IUNIT, FILE=trim(FULL_NAME), STATUS=OPEN_STAT,   &
               ACCESS=OPEN_ACCESS, FORM=OPEN_FORM, IOSTAT=IER, IOMSG=IMSG)
         ELSEIF(OPEN_STAT == 'APPEND' .OR. OPEN_STAT == 'UNKNOWN') THEN
            OPEN(UNIT=IUNIT, FILE=trim(FULL_NAME), STATUS='UNKNOWN',   &
               ACCESS=OPEN_ACCESS, FORM=OPEN_FORM, POSITION='APPEND',  &
               IOSTAT=IER, IOMSG=IMSG)
         ELSE
            IER = OPEN_FILE_INVALID_ARGS
         ENDIF
      ENDIF

      RETURN

 1000 FORMAT(A,'_',I5.5,A4)
 1001 FORMAT(A,A4)

   END SUBROUTINE OPEN_FILE

END MODULE OPEN_FILE_MOD
