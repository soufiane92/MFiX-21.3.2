MODULE UNDEF_2_0_MOD

   USE compar, only: ijkend3
   USE param, only: dimension_3
   USE param1, only: zero, undefined

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: UNDEF_2_0 (Var, IER)                                   C
!  Purpose: change undefined values to zero.  Otherwise linear equationC
!           solver does not work                                       C
!                                                                      C
!  Author: M. Syamlal                                 Date: 18-JUL-96  C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
   SUBROUTINE UNDEF_2_0(VARDUM)

      IMPLICIT NONE

      DOUBLE PRECISION, DIMENSION(DIMENSION_3) :: VARDUM

         WHERE (VARDUM(1:IJKEND3) == UNDEFINED) VARDUM(1:IJKEND3) = ZERO

      RETURN
   END SUBROUTINE UNDEF_2_0

END MODULE UNDEF_2_0_MOD
