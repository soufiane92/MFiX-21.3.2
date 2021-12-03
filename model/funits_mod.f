#include "version.inc"

      MODULE FUNITS

! Whether this processor should write the log file in DMP mode.
! Usually this flag is true only for PE_IO.  All the PEs may be forced
! to write a log file by setting ENABLE_DMP_LOG to .true. in output_mod.f.
      LOGICAL :: DMP_LOG

! Flag indicated that the log was opened globally.
      LOGICAL :: LOG_WAS_CLOSED = .FALSE.

! RRATES debug file unit number
      INTEGER, PARAMETER :: UNIT_RRATES = 43

! input deck file unit number
      INTEGER, PARAMETER :: UNIT_DAT = 51

! RUN_NAME.OUT file unit number
      INTEGER, PARAMETER :: UNIT_OUT = 52

! RUN_NAME.LOG file unit number. (DEFAULT/Serial 53)
      INTEGER :: UNIT_LOG = 53

! Temporary (scratch) file unit number
      INTEGER, PARAMETER :: UNIT_TMP = 54

! RUN_NAME.RES file unit number
      INTEGER, PARAMETER :: UNIT_RES = 55

! RUN_NAME.SPx file unit offset number
      INTEGER, PARAMETER :: UNIT_SPX = 60

! Used by COPY_FILE in fs_util
      INTEGER, PARAMETER :: UNIT_COPY_SRC = 65
      INTEGER, PARAMETER :: UNIT_COPY_DEST = 70

! RUN_NAME.MSH file unit number      
      INTEGER, PARAMETER :: UNIT_MSH = 77 
      INTEGER, PARAMETER :: UNIT_MSH_STATS = 78 

      INTEGER, PARAMETER :: UNIT_VTP = 80
      INTEGER, PARAMETER :: UNIT_VTU = 82
      INTEGER, PARAMETER :: UNIT_VTR = 83
! file unit for ParaView *.pvd data
      INTEGER, PARAMETER :: UNIT_PVD = 84
      INTEGER, PARAMETER :: UNIT_PVTP = 85
      INTEGER, PARAMETER :: UNIT_PVTU = 88
      INTEGER, PARAMETER :: UNIT_VTU_FRAME = 90

      CONTAINS


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: NEWUNIT                                                !
!  Author: A. Choudhary                               Date: 01/21/2015 !
!                                                                      !
!  Purpose: Finds an open i/o unit number; Usage:                      !
!   integer myunit                                                     !
!   open(unit=newunit(myunit),file='filename')                         !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      INTEGER FUNCTION newunit(unit)
      IMPLICIT NONE

! optional variable to hold unit number
      INTEGER, INTENT(OUT), OPTIONAL  :: unit
! lower and upper limits to search for available units
      INTEGER, PARAMETER  :: lun_min = 100, lun_max= 999
! check to see if the unit is open
      LOGICAL :: is_open
! looping variable
      INTEGER :: lun

      newunit = -1

      DO lun = lun_min, lun_max
        INQUIRE(UNIT=lun, OPENED=is_open)
        IF(.NOT.is_open) THEN
          newunit = lun
          EXIT
        END IF
      END DO

      IF(present(unit)) unit=newunit

      RETURN
      END FUNCTION NEWUNIT

      INTEGER FUNCTION FILE_SIZE(FILENAME)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: FILENAME
         INTEGER :: SIZE

         OPEN(UNIT=UNIT_DAT, FILE=FILENAME, FORM="UNFORMATTED", ACCESS="STREAM", ACTION="READ")
         INQUIRE(UNIT=UNIT_DAT, SIZE=SIZE)
         CLOSE(UNIT_DAT)
         FILE_SIZE = SIZE

      END FUNCTION FILE_SIZE

      END MODULE FUNITS
