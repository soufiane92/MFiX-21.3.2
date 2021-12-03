MODULE OUT_BIN_R_MOD

   use out_bin_512r_mod, only: out_bin_512r

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: OUT_BIN_R                                              C
!  Purpose: write out a time-dependent restart variable (REAL)         C
!                                                                      C
!  Author: P. Nicoletti                               Date: 13-DEC-91  C
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
!  Local variables: LC, ARRAY_REAL                                     C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
   SUBROUTINE OUT_BIN_R(IUNIT, ARRAY, IJKMAX2, NEXT_REC)

      USE param
      IMPLICIT NONE

!                      double precision array to write out
      DOUBLE PRECISION ARRAY(*)
!
!                      unit number to write to
      INTEGER          IUNIT
!
!                      record pointer into file IUNIT
      INTEGER          NEXT_REC
!
!                      number of indices in ARRAY to write out
      INTEGER          IJKMAX2
!
! local variables
!
!                      single precision version of ARRAY
!//     REAL             ARRAY_REAL(DIMENSION_3)
!
      real, allocatable :: array_real(:)
!                      loop counter
      INTEGER          LC
!-----------------------------------------------
!
      allocate (array_real(ijkmax2))

      LC = 1
      IF (IJKMAX2 > 0) THEN
         ARRAY_REAL(:IJKMAX2) = SNGL(ARRAY(:IJKMAX2))
         LC = IJKMAX2 + 1
      ENDIF
      CALL OUT_BIN_512R (IUNIT, ARRAY_REAL, IJKMAX2, NEXT_REC)

      deallocate (array_real)

      RETURN

   END SUBROUTINE OUT_BIN_R

END MODULE OUT_BIN_R_MOD
