#include "error.inc"

MODULE PVD_MOD

   USE cdist, only: bdist_io
   USE compar, only: myPE, PE_IO, NODESI,NODESJ,NODESK
   USE error_manager
   USE param1, only: zero
   USE run, only: time, run_type, ppo
   USE vtk
   use funits, only: unit_pvd, unit_vtu_frame

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: OPEN_PVD_FILE                                          C
!  Purpose: Open a PVD file and writes the header                      C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE OPEN_PVD_FILE

      IMPLICIT NONE
      LOGICAL :: PVD_EXISTS

      IF (myPE /= PE_IO) RETURN

      PVD_FILENAME = TRIM(VTK_FILEBASE(VTK_REGION)) // '.pvd'

! First, check if the file already exists.

      INQUIRE(FILE=PVD_FILENAME,EXIST=PVD_EXISTS)

! The first time this subroutine is executed, properly initialize the pvd file

      IF(.NOT.PVD_FILE_INITIALIZED(VTK_REGION)) THEN

         IF(RUN_TYPE == 'NEW'.OR.RUN_TYPE=='RESTART_2')THEN
! For a new or RESTART_2 run, the pvd file should not exist, and is created with appropriate header
            IF (.NOT.PVD_EXISTS) THEN
               OPEN(UNIT = UNIT_PVD, FILE = TRIM(PVD_FILENAME))
               WRITE(UNIT_PVD,100) '<?xml version="1.0"?>'
               WRITE(UNIT_PVD,100) '<VTKFile type="Collection" version="0.1" byte_order="BigEndian">' 
               WRITE(UNIT_PVD,100) '<Collection>'
               PVD_FILE_INITIALIZED(VTK_REGION)=.TRUE.
            ELSE ! If the pvd file exists, print error message and exits
               WRITE(ERR_MSG,*) TRIM(PVD_FILENAME), " is present at start of run."
               call log_error()
            ENDIF
         ELSE
! For a restart_1 run, if the pvd file does not exist, create it
! (It must be a new vtk region)
            IF (.NOT.PVD_EXISTS) THEN
               OPEN(UNIT = UNIT_PVD, FILE = TRIM(PVD_FILENAME))
               WRITE(UNIT_PVD,100) '<?xml version="1.0"?>'
               WRITE(UNIT_PVD,100) '<VTKFile type="Collection" version="0.1" byte_order="BigEndian">'
               WRITE(UNIT_PVD,100) '<Collection>'
               PVD_FILE_INITIALIZED(VTK_REGION)=.TRUE.
            ELSE
! If it already exists, go to the bottom of the file and prepare to append data (remove last two lines)
               OPEN(UNIT=UNIT_PVD,FILE = TRIM(PVD_FILENAME),POSITION="APPEND",STATUS='OLD')
               BACKSPACE(UNIT_PVD)
               BACKSPACE(UNIT_PVD)
               PVD_FILE_INITIALIZED(VTK_REGION)=.TRUE.
            ENDIF
         ENDIF
      ELSE
! When properly initialized, open the file and go to the
! bottom of the file and prepare to append data (remove last two lines)
         OPEN(UNIT=UNIT_PVD,FILE = TRIM(PVD_FILENAME),POSITION="APPEND",STATUS='OLD')
         BACKSPACE(UNIT_PVD)
         BACKSPACE(UNIT_PVD)
      ENDIF


100   FORMAT(A)

      RETURN

   END SUBROUTINE OPEN_PVD_FILE



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: UPDATE_AND_CLOSE_PVD_FILE                              C
!  Purpose: Updates and close a pvd file                               C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE UPDATE_AND_CLOSE_PVD_FILE

      IMPLICIT NONE

      CHARACTER(:), allocatable :: FILENAME

      IF (myPE /= PE_IO) RETURN

      IF (.NOT. BDIST_IO) THEN
         IF (VTK_DATA(VTK_REGION) == "C") THEN
            FILENAME = FIND_VTK_BASENAME(0) // ".vtu"
         ELSEIF (VTK_DATA(VTK_REGION) == "G") THEN
            FILENAME = FIND_VTK_BASENAME(0) // ".vtu"
         ELSEIF(VTK_DATA(VTK_REGION) == "P") THEN
            FILENAME = FIND_VTK_BASENAME(0) // ".vtp"
         ELSEIF(VTK_DATA(VTK_REGION) == "F") THEN
            FILENAME = FIND_VTK_BASENAME(0) // ".vtp"
         ELSE
            ERR_MSG = "Invalid call to UPDATE_AND_CLOSE_PVD_FILE"
            CALL LOG_ERROR()
         ENDIF
      ELSE
         IF (VTK_DATA(VTK_REGION) == "C") THEN
            FILENAME = FIND_VTK_BASENAME(0) // ".pvtu"
         ELSEIF(VTK_DATA(VTK_REGION) == "P") THEN
            FILENAME = FIND_VTK_BASENAME(0) // ".pvtp"
         ELSEIF(VTK_DATA(VTK_REGION) == "F") THEN
            FILENAME = FIND_VTK_BASENAME(0) // ".pvtp"
         ELSE
            ERR_MSG = "Invalid call to UPDATE_AND_CLOSE_PVD_FILE"
            CALL LOG_ERROR()
         ENDIF
      ENDIF

! Write the data to the file
      WRITE(UNIT_PVD, "(6X,A,E14.7,5A)") &
         '<DataSet timestep="',TIME,'" ', & ! simulation time
         'group="" part="0" ', & ! necessary file data
         'file="', TRIM(FILENAME),'"/>'

! Write the closing tags
      WRITE(UNIT_PVD, *)'</Collection>'
      WRITE(UNIT_PVD, *)'</VTKFile>'

      CLOSE(UNIT_PVD)

      RETURN

   END SUBROUTINE UPDATE_AND_CLOSE_PVD_FILE
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_PVD_FRAMES                                        C
!  Purpose: Writes PVD frame indices                                    C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 09-Sep-17  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE WRITE_PVD_FRAMES

      IMPLICIT NONE
      INTEGER :: L1
      INTEGER :: N_PVD_FILES

      IF(TIME_DEPENDENT_FILENAME.and.(.NOT.PPO)) THEN
         OPEN(UNIT = UNIT_VTU_FRAME, FILE = TRIM(VTU_FRAME_FILENAME))
         N_PVD_FILES=0
         DO L1 = 1, DIMENSION_VTK
            IF(VTK_DEFINED(L1)) N_PVD_FILES = N_PVD_FILES + 1
         ENDDO
         WRITE(UNIT_VTU_FRAME,*)N_PVD_FILES
         OPEN(UNIT = UNIT_VTU_FRAME, FILE = TRIM(VTU_FRAME_FILENAME))
         N_PVD_FILES=0
         DO L1 = 1, DIMENSION_VTK
            IF(VTK_DEFINED(L1)) THEN
               N_PVD_FILES = N_PVD_FILES + 1
               PVD_FILENAME = TRIM(VTK_FILEBASE(L1)) // '.pvd'
               WRITE(UNIT_VTU_FRAME,*)N_PVD_FILES,L1,FRAME(L1),TRIM(PVD_FILENAME)
            ENDIF
         ENDDO

         CLOSE(UNIT_VTU_FRAME)
      ENDIF

      RETURN

   END SUBROUTINE WRITE_PVD_FRAMES

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: READ_PVD_FRAMES                                        C
!  Purpose: Reads PVD frame indices                                    C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 09-Sep-17  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE READ_PVD_FRAMES

      IMPLICIT NONE
      LOGICAL :: VTU_FRAME_FILE_EXISTS,PVD_FILE_MATCH
      INTEGER :: BUFF1,BUFF2,BUFF3,L,L1,L2
      INTEGER :: N_PVD_FILES,PVD_INDEX(DIMENSION_VTK),PVD_FRAME(DIMENSION_VTK)
      CHARACTER (LEN=255) :: BUFF_CHAR, PVD_NAME(DIMENSION_VTK)


      INQUIRE(FILE=VTU_FRAME_FILENAME,EXIST=VTU_FRAME_FILE_EXISTS)
      IF(VTU_FRAME_FILE_EXISTS) THEN
         OPEN(UNIT = UNIT_VTU_FRAME, FILE = TRIM(VTU_FRAME_FILENAME))
         READ(UNIT_VTU_FRAME,*)N_PVD_FILES
         DO L = 1, N_PVD_FILES
            READ(UNIT_VTU_FRAME,*)BUFF1,BUFF2,BUFF3,BUFF_CHAR
            PVD_INDEX(L) = BUFF2
            PVD_NAME(L) = BUFF_CHAR
            PVD_FRAME(L) = BUFF3
         ENDDO

         PVD_FILE_MATCH = .FALSE.
         DO L1 = 1, DIMENSION_VTK
            IF(VTK_DEFINED(L1)) THEN
               PVD_FILENAME = TRIM(VTK_FILEBASE(L1)) // '.pvd'
               DO L2 = 1, N_PVD_FILES
                  IF(TRIM(PVD_FILENAME)==TRIM(PVD_NAME(L2))) then
                     PVD_FILE_MATCH = .TRUE.
                     FRAME(PVD_INDEX(L2)) = PVD_FRAME(L2)
                  endif
               ENDDO
               IF(.NOT.PVD_FILE_MATCH) PVD_FILE_INITIALIZED(L1) = .FALSE.
            ENDIF
         ENDDO
         CLOSE(UNIT_VTU_FRAME)
      ENDIF

      RETURN

   END SUBROUTINE READ_PVD_FRAMES

END MODULE PVD_MOD
