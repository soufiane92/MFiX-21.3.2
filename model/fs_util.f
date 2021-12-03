#include "error.inc"

! Module for filesystem tasks: make directory, copy file, move file
! The caller of CREATE_DIR, MOVE_FILE, COPY_FILE is responsible for checking: myPE /= PE_IO

module fs_util

   use error_manager

contains

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: CREATE_DIR                                             !
!  Author: J.Musser                                      Date: 06/2015 !
!                                                                      !
!  Purpose: Create the directory pDIR in the run directory.            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE CREATE_DIR(PDIR)

      use iso_c_binding

      IMPLICIT NONE

      interface
         function mkdir(path, mode) bind(c,name="mkdir")
            use iso_c_binding
            integer(c_int) :: mkdir
            character(kind=c_char,len=1) :: path(*)
            integer(c_int16_t), value :: mode
         end function mkdir
      end interface

      CHARACTER(LEN=*), INTENT(IN) :: pDIR

      CHARACTER(LEN=1024) :: pDIR_c
      INTEGER :: IOS

      pDIR_c = trim(adjustl(pDIR))//C_NULL_CHAR

      ios = mkdir(pDIR_c, int(o'772', c_int16_t))

      RETURN
   END SUBROUTINE CREATE_DIR

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: FILE_EXISTS                                            !
!  Author: Mark Meredith                                 Date: 11/2018 !
!                                                                      !
!  Purpose: Check whether a file exists.                               !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   FUNCTION FILE_EXISTS(filename)
      IMPLICIT NONE
      LOGICAL ::FILE_EXISTS
      CHARACTER(LEN=*) :: filename
      INQUIRE(FILE=filename, EXIST=FILE_EXISTS)
   END FUNCTION FILE_EXISTS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: MOVE_FILE                                              !
!  Author: J.Musser                                      Date: 06/2015 !
!                                                                      !
!  Purpose: Move a file, using C rename() function,                    !
!           instead of GNU Fortran extension RENAME()                  !
!           or shell with mv command.                                  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE MOVE_FILE(src, dest)

      use iso_c_binding

      IMPLICIT NONE

      interface
         function rename_c(src, dest) bind(c,name="rename")
            use iso_c_binding
            integer(c_int) :: rename_c
            character(kind=c_char,len=1) :: src(*)
            character(kind=c_char,len=1) :: dest(*)
         end function rename_c
      end interface

      CHARACTER(LEN=*), INTENT(IN) :: src
      CHARACTER(LEN=*), INTENT(IN) :: dest

      CHARACTER(LEN=1024) :: src_c
      CHARACTER(LEN=1024) :: dest_c
      INTEGER :: IOS

      src_c = trim(adjustl(src))//C_NULL_CHAR
      dest_c = trim(adjustl(dest))//C_NULL_CHAR

      if (.not.file_exists(src)) then
         return
      endif

      ios = rename_c(src_c, dest_c)

      if (ios /= 0) then
         WRITE(ERR_MSG, "(A, A, A, A)" ) "Error renaming file ", trim(src), " to ", trim(dest)
         CALL LOG_WARNING()
      else
         WRITE (ERR_MSG, '(A,A)') 'Moved ', src, " to ", dest
         CALL LOG_INFO()
      end if

      RETURN

   END SUBROUTINE MOVE_FILE

!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
!                                                                      !
!  Module name: COPY_FILE                                              !
!  Author: J.Musser                                      Date: 06/2015 !
!                                                                      !
!  Purpose: Copy a file, by closing file unit (if open),               !
!           copying to destination file, then reopening source         !
!           file unit (which stays open for .RES files).               !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE COPY_FILE(src, dest)

      use iso_c_binding
      use iso_fortran_env
      use funits, only: UNIT_COPY_SRC, UNIT_COPY_DEST

      IMPLICIT NONE
      integer :: out, in
      integer :: ios
      character :: cc

      logical :: lopened
      CHARACTER(LEN=*), INTENT(IN) :: src
      CHARACTER(LEN=*), INTENT(IN) :: dest

      CHARACTER(len=1024) :: lmsg
      CHARACTER(len=16) :: laccess, lform
      integer :: lrec

      inquire(file=src, opened=lopened, number=in, access=laccess, &
           recl=lrec, form=lform)

      if(.not.lopened) then
         in = UNIT_COPY_SRC
         open(in, file=src, status="old", action="read", access="stream", iostat=ios)
         if (ios /= 0) then
            WRITE (ERR_MSG, '(A)') "Unable to read file ", src
            CALL LOG_WARNING()
         end if

      else
         close(in)
         open(in, file=src, status="old", action="read", access="stream", iostat=ios)
         if (ios /= 0) then
            WRITE (ERR_MSG, '(A)') "Unable to read file ", src
            CALL LOG_WARNING()
         end if
         rewind(in)

      endif

      out = UNIT_COPY_DEST
      open(out, file=dest, status="new", action="write", access="stream", iostat=ios)

      if (ios /= 0) then
         WRITE (ERR_MSG, '(A)') "Unable to read file ", dest
         CALL LOG_WARNING()
      end if

      ios = 0
      do while (ios == 0)
         read(unit=in, iostat=ios, iomsg=lmsg) cc

         if (ios == iostat_end) exit ! EOF

         if (ios /= 0) then
            WRITE (ERR_MSG, '(A)') "Error reading from ", src, " : ", trim(lmsg)
            CALL LOG_WARNING()
            exit
         end if

         write(UNIT_COPY_DEST) cc
      end do

      close(UNIT_COPY_DEST)
      close(UNIT_COPY_SRC)

      if(lopened) then
         if(trim(laccess)=='DIRECT') then
            open(unit=in, file=src, status='old', access='DIRECT', recl=lrec, &
                 form=trim(lform), iostat=ios, convert='BIG_ENDIAN')
         else
            open(unit=in, file=src, status='old', access=trim(laccess),&
                 form=trim(lform), iostat=ios, position='APPEND')
         endif
      endif

      WRITE (ERR_MSG, '(A,A)') 'Copied ', src, " to ", dest
      CALL LOG_INFO()

      RETURN

   END SUBROUTINE COPY_FILE
end module fs_util
