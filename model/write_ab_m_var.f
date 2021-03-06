MODULE WRITE_AB_M_VAR_MOD
CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: Write_Ab_m_var(A_m, b_m, var, IER)                     C
!  Purpose: Write the sparse matrix coefficients and the               C
!           source vector.                                             C
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
   SUBROUTINE WRITE_AB_M_VAR(A_M, B_M, VAR)

      USE MACHINE
      USE compar
      USE functions
      USE geometry
      USE indices
      USE mpi_utility
      USE param
      USE param1

      IMPLICIT NONE
!-----------------------------------------------
!
!                      Local index
      INTEGER          L
!
!                      cell index
      INTEGER          IJK
!
!                      Septadiagonal matrix A_m
      DOUBLE PRECISION A_m(DIMENSION_3, -3:3)
!
!                      Source vector
      DOUBLE PRECISION b_m(DIMENSION_3)

!                      Source vector
      DOUBLE PRECISION var(DIMENSION_3)

      double precision, allocatable :: array1(:) , array2(:)   !//
      double precision, allocatable :: am(:,:)                !//
!
!-----------------------------------------------
!
      integer i, j, k

      if (myPE == PE_IO) then
         allocate (array1(ijkmax3))
         allocate (array2(ijkmax3))
         allocate (am(ijkmax3,-3:3))
      else
         allocate (array1(1))
         allocate (array2(1))
         allocate (am(1,-3:3))
      end if

      if (myPE == PE_IO) then
         CALL START_LOG
         IF(DMP_LOG)WRITE (UNIT_LOG,*) ' Note : write_am_m is VERY inefficient '
         WRITE (*,*) ' Note : write_am_m is VERY inefficient '
         IF(DMP_LOG)WRITE (UNIT_LOG,*) '  '
         IF(DMP_LOG)WRITE (UNIT_LOG,*) ' A_m and B_m arrays below are in the '
         IF(DMP_LOG)WRITE (UNIT_LOG,*) ' mfix INTERNAL order'
         IF(DMP_LOG)WRITE (UNIT_LOG,*) ' '
         IF(DMP_LOG)WRITE (UNIT_LOG, '(A,A)') &
           '  IJK  I  J  K   b         s         w         p         e       ', &
           '  n         t        Source     Variable'
      end if


      do L = -3,3

      call gather(a_m(:,L),array1,root)

      DO K = Kmin2, Kmax2
      DO I = Imin2, Imax2
      DO J = Jmin2, Jmax2

      IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells

      IJK = FUNIJK_GL(IMAP_C(I),JMAP_C(J),KMAP_C(K))
!     IJK = FUNIJK_GL(I,J,K)

      if (myPE == PE_IO) am(ijk,l) = array1(ijk)


      END DO
      END DO
      END DO

      end do

      call gather(var(:),array1,root)
      call gather(b_m(:),array2,root)

      DO K = Kmin2, Kmax2
      DO I = Imin2, Imax2
      DO J = Jmin2, Jmax2

      IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells

!     IJK = FUNIJK_GL(I,J,K)
      IJK = FUNIJK_GL(IMAP_C(I),JMAP_C(J),KMAP_C(K))

        if (myPE == PE_IO .AND. DMP_LOG)WRITE (UNIT_LOG, '(I5, 3(I3), 9(1X,G9.2))') FUNIJK_IO(I,J,K), I, J, K,&
                                    (AM(ijk,L),L=-3,3), array2(IJK), array1(IJK)

      END DO
      END DO
      END DO

      if (myPE == PE_IO) CALL END_LOG


      deallocate (array1)    !//
      deallocate (array2)    !//

      RETURN
   END SUBROUTINE WRITE_AB_M_VAR

END MODULE WRITE_AB_M_VAR_MOD
