MODULE WRITE_ERROR_MOD

   USE funits, only: dmp_log, unit_log
   USE machine, only: start_log, end_log

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: Write_error(Name, Line, L)                             C                     C
!  Purpose: Write an error message                                     C
!                                                                      C
!                                                                      C
!  Author: M. Syamlal                                 Date: 16-MAY-96  C
!  Reviewer:                                          Date:            C
!                                                                      C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced:                                               C
!  Variables modified:                                                 C
!                                                                      C
!  Local variables:                                                    C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
   SUBROUTINE WRITE_ERROR(NAME, LINE, LMAX)

      IMPLICIT NONE

!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
!
!                      Subroutine name
      CHARACTER(LEN=*)    Name
!
!                      Message
      CHARACTER(LEN=*)    LINE(*)
!
!                      Dimension of message array
      INTEGER          LMAX
!
!                      Index
      INTEGER          L
!
!-----------------------------------------------
!

#ifndef MFIX_INTERACTIVE
      CALL START_LOG
#endif
      IF(DMP_LOG)WRITE (UNIT_LOG, 1000) NAME
      DO L = 1, LMAX
         IF(DMP_LOG)WRITE (UNIT_LOG, 1010) LINE(L)
      END DO
#ifndef MFIX_INTERACTIVE
      IF(DMP_LOG)WRITE (UNIT_LOG, 1020)
#endif
      CALL END_LOG
      RETURN
 1000 FORMAT(1X,70('*'),/,/,1X,'From : ',A)
 1010 FORMAT(1X,A)
 1020 FORMAT(/,/,1X,70('*'))

   END SUBROUTINE WRITE_ERROR

END MODULE WRITE_ERROR_MOD
