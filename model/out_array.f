MODULE OUT_ARRAY_MOD

   USE compar
   USE fldvar
   USE functions
   USE funits
   USE geometry
   USE in_binary_512
   USE indices
   USE out_array_k_mod, only: out_array_k
   USE param
   USE param1
   USE physprop

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: OUT_ARRAY (ARRAY, MESSAGE)                             C
!  Purpose: print out a 3D array to standard output                    C
!                                                                      C
!  Author: P.Nicoletti                                Date: 02-DEC-91  C
!  Reviewer: W. Rogers, M. Syamlal, S. Venkatesan     Date: 31-JAN-92  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced: KMAX2                                         C
!  Variables modified: None                                            C
!                                                                      C
!  Local variables: IJK, K                                        C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE OUT_ARRAY(ARRAY, MESSAGE)

      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
!
!                       array to print out
      DOUBLE PRECISION  ARRAY(*)
!
!                       message to print out
      CHARACTER(LEN=*) :: MESSAGE
!
! local variables
!
!                       pointer into array (points to start of a k-plane)
      INTEGER           IJK
!
!                       loop counter
      INTEGER           K

      double precision,  allocatable :: array1(:)
!
!-----------------------------------------------
!
!//d      call lock_tmp_array

      allocate (array1(ijkmax2))

      call convert_to_io_dp(array,array1,ijkmax2)

!!/SP
    IF(CYCLIC_Z) then
      DO K = 2, KMAX1
         IJK = FUNIJK_IO(1,1,K)
         WRITE (UNIT_OUT, 1100) MESSAGE, K
         CALL OUT_ARRAY_K (ARRAY1(IJK))
      END DO
    ELSE
      DO K = 1, KMAX2
         IJK = FUNIJK_IO(1,1,K)
         WRITE (UNIT_OUT, 1100) MESSAGE, K
         CALL OUT_ARRAY_K (ARRAY1(IJK))
      END DO
    ENDIF
 1100 FORMAT(/,1X,A,' at K = ',I4,/)


      deallocate (array1)

!//d      call unlock_tmp_array
!
      RETURN

   END SUBROUTINE OUT_ARRAY

END MODULE OUT_ARRAY_MOD
