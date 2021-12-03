MODULE INIT_AB_M_MOD

   USE param, only: bottom, top, east, west, north, south
   USE param, only: dimension_3, dimension_m
   USE param1, only: one, zero

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: Init_Ab_m(A_m, b_m, IJKMAX2, M, IER)                   C                     C
!  Purpose:Initialiize the sparse matrix coefficients and the          C
!           source vector.                                             C
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
   SUBROUTINE INIT_AB_M(A_M, B_M, IJKMAX2A, M)

      IMPLICIT NONE

!                      Phase index
      INTEGER          M
!
!                      Maximum dimension
      INTEGER          IJKMAX2A
!
!                      Septadiagonal matrix A_m
      DOUBLE PRECISION A_m(DIMENSION_3, -3:3, 0:DIMENSION_M)
!
!                      Source vector
      DOUBLE PRECISION b_m(DIMENSION_3, 0:DIMENSION_M)
!
!-----------------------------------------------
!
!      IJK = 1
      IF (IJKMAX2A > 0) THEN
!$omp parallel
!$omp sections
         A_M(:,bottom,M) = ZERO
!$omp section
         A_M(:,south,M) = ZERO
!$omp section
         A_M(:,west,M) = ZERO
!$omp section
         A_M(:,0,M) = -ONE
!$omp section
         A_M(:,east,M) = ZERO
!$omp section
         A_M(:,north,M) = ZERO
!$omp section
         A_M(:,top,M) = ZERO
!$omp section
         B_M(:,M) = ZERO
!$omp end sections
!$omp end parallel
      ENDIF
      RETURN

   END SUBROUTINE INIT_AB_M

END MODULE INIT_AB_M_MOD
