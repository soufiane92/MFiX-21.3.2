MODULE machine

   use funits, only: unit_log, dmp_log
   use, intrinsic  :: iso_c_binding, only: c_char, c_int, c_size_t, c_null_char

! Record length used in open statement for unformatted, direct access
! file, with 512 bytes per record
   integer, parameter :: OPEN_N1 = 512

! Number of DOUBLE PRECISION words in 512 bytes
   integer, parameter :: NWORDS_DP = 64

! Number of REAL words in 512 bytes
   integer, parameter  :: NWORDS_R = 128

! Number of INTEGER words in 512 bytes
   integer, parameter :: NWORDS_I = 128

! Number of LOGICAL words in 512 bytes
   integer, parameter :: NWORDS_L = 128

! computer node name/id
   CHARACTER(LEN=64) :: ID_NODE

! RUN ID info
   INTEGER :: ID_MONTH
   INTEGER :: ID_DAY
   INTEGER :: ID_YEAR
   INTEGER :: ID_HOUR
   INTEGER :: ID_MINUTE
   INTEGER :: ID_SECOND

   interface !to function: int gethostname(char *name, size_t namelen);
      integer(c_int) function gethostname(name, namelen) bind(c)
         use, intrinsic  :: iso_c_binding, only: c_char, c_int, c_size_t
         integer(c_size_t), value, intent(in) :: namelen
         character(len=1, kind=c_char), dimension(namelen), intent(inout) ::  name
      end function gethostname
   end interface

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: GET_RUN_ID                                             C
!  Purpose: get the run id for this run                                C
!                                                                      C
!  Author: P. Nicoletti                               Date: 16-DEC-91  C
!  Reviewer: P. Nicoletti, W. Rogers, M. Syamlal      Date:            C
!                                                                      C
!  Revision Number: 1                                                  C
!  Purpose: add ndoe name                                              C
!  Author: P.Nicoletti                                Date: 07-FEB-92  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced: None                                          C
!  Variables modified: ID_MONTH, ID_DAY, ID_YEAR, ID_HOUR, ID_MINUTE   C
!                      ID_SECOND, ID_NODE                              C
!                                                                      C
!  Local variables: TIME_ARRAY                                         C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

   SUBROUTINE GET_RUN_ID

      IMPLICIT NONE
      INTEGER DAT(8)
      CHARACTER(LEN=10) DATE, TIME, ZONE

      integer(c_int) :: status
      integer, parameter :: HOST_NAME_MAX = 255
      character(kind=c_char, len=1), dimension(HOST_NAME_MAX) :: cstr_hostname
      integer(c_size_t) :: lenstr
      character(len=HOST_NAME_MAX) :: env_hostname
      character(len=:), allocatable :: hostname

      CALL DATE_AND_TIME(DATE, TIME, ZONE, DAT)
      ID_YEAR = DAT(1)
      ID_MONTH = DAT(2)
      ID_DAY = DAT(3)
      ID_HOUR = DAT(5)
      ID_MINUTE = DAT(6)
      ID_SECOND = DAT(7)

#ifdef MINGW
      call get_environment_variable("COMPUTERNAME", env_hostname)
      ID_NODE = env_hostname
#else
      lenstr = HOST_NAME_MAX
      status = gethostname(cstr_hostname, lenstr)
      hostname = c_to_f_string(cstr_hostname)
      ID_NODE = hostname
#endif

      RETURN

   contains
! convert c_string to f_string
      pure function c_to_f_string(c_string) result(f_string)
         character(kind=c_char, len=1), intent(in) :: c_string(:)
         character(len=:), allocatable :: f_string
         integer i, n
         i = 1
         do while (c_string(i) /= c_null_char)
            i = i + 1
         end do
         n = i - 1  ! exclude c_null_char
         allocate (character(len=n) :: f_string)
         f_string = transfer(c_string(1:n), f_string)
      end function c_to_f_string

   END SUBROUTINE GET_RUN_ID
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: START_LOG                                              C
!  Purpose: does nothing ... for VAX/VMS compatibility (SGI ONLY)      C
!                                                                      C
!  Author: P. Nicoletti                               Date: 28-JAN-92  C
!  Reviewer: P. Nicoletti, W. Rogers, M. Syamlal      Date:            C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced: None                                          C
!  Variables modified: None                                            C
!                                                                      C
!  Local variables: None                                               C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
   SUBROUTINE START_LOG
      IMPLICIT NONE
      RETURN
   END SUBROUTINE START_LOG
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: END_LOG                                                C
!  Purpose: flushes the log file                                       C
!                                                                      C
!  Author: P. Nicoletti                               Date: 28-JAN-92  C
!  Reviewer: P. Nicoletti, W. Rogers, M. Syamlal      Date:            C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced: None                                          C
!  Variables modified: None                                            C
!                                                                      C
!  Local variables: None                                               C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
   SUBROUTINE END_LOG
      IMPLICIT NONE
      IF (DMP_LOG) FLUSH(UNIT_LOG)
      RETURN
   END SUBROUTINE END_LOG

END MODULE machine
