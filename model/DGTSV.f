#include "error.inc"

MODULE DGTSV_MOD

   USE compar
   USE error_manager

CONTAINS

      SUBROUTINE XERBLA ( SRNAME, INFO )

!     ..    Scalar Arguments ..
      INTEGER            INFO
      CHARACTER(LEN=6)        SRNAME
!     ..
!
!  Purpose
!  =======
!
!  XERBLA  is an error handler for the Level 2 BLAS routines.
!
!  It is called by the Level 2 BLAS routines if an input parameter is
!  invalid.
!
!  Installers should consider modifying the STOP statement in order to
!  call system-specific exception-handling facilities.
!  Parameters
!  ==========
!
!  SRNAME - CHARACTER*6.
!           On entry, SRNAME specifies the name of the routine which
!           called XERBLA.
!
!  INFO   - INTEGER.
!           On entry, INFO specifies the position of the invalid
!           parameter in the parameter-list of the calling routine.
!
!
!  Auxiliary routine for Level 2 Blas.
!
!  Written on 20-July-1986.
!
!     .. Executable Statements ..
!
      WRITE (err_msg,99999) myPE,SRNAME, INFO

!     STOP
      call log_error()
99999 FORMAT ( '(PE ',I6,'): ** On entry to ', A6, ' parameter number ', I2, &
         ' had an illegal value' )

      END SUBROUTINE XERBLA


!-----------------------------------------------
      SUBROUTINE DGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
!-----------------------------------------------
!
!  -- LAPACK routine (version 3.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     October 31, 1999
!
!     .. Scalar Arguments ..
      INTEGER            INFO, LDB, N, NRHS
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   B( LDB, * ), D( * ), DL( * ), DU( * )
!     ..
!
!  Purpose
!  =======
!
!  DGTSV  solves the equation
!
!     A*X = B,
!
!  where A is an n by n tridiagonal matrix, by Gaussian elimination with
!  partial pivoting.
!
!  Note that the equation  A'*X = B  may be solved by interchanging the
!  order of the arguments DU and DL.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  NRHS    (input) INTEGER
!          The number of right hand sides, i.e., the number of columns
!          of the matrix B.  NRHS >= 0.
!
!  DL      (input/output) DOUBLE PRECISION array, dimension (N-1)
!          On entry, DL must contain the (n-1) sub-diagonal elements of
!          A.
!
!          On exit, DL is overwritten by the (n-2) elements of the
!          second super-diagonal of the upper triangular matrix U from
!          the LU factorization of A, in DL(1), ..., DL(n-2), unless
!          the alternate Thomas algorithm is being used, see comments below
!
!  D       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, D must contain the diagonal elements of A.
!
!          On exit, D is overwritten by the n diagonal elements of U.
!
!  DU      (input/output) DOUBLE PRECISION array, dimension (N-1)
!          On entry, DU must contain the (n-1) super-diagonal elements
!          of A.
!
!          On exit, DU is overwritten by the (n-1) elements of the first
!          super-diagonal of U.  See comments at THOMAS below
!
!  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
!          On entry, the N by NRHS matrix of right hand side matrix B.
!          On exit, if INFO = 0, the N by NRHS solution matrix X.
!
!  LDB     (input) INTEGER
!          The leading dimension of the array B.  LDB >= max(1,N).
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument had an illegal value
!          > 0: if INFO = i, U(i,i) is exactly zero, and the solution
!               has not been computed.  The factorization has not been
!               completed unless i = N.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   FACT, TEMP

!     .. Temporary arrays
      DOUBLE PRECISION GG(N)
      DOUBLE PRECISION YY(N)
      DOUBLE PRECISION TMP(N-1)
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
!     ..
!     .. Executable Statements ..
!
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGTSV ', -INFO )
         RETURN
      END IF
!
      IF( N.EQ.0 ) RETURN
!

!     cgw 2021-02-02
!     Modified Thomas algorithm for tridiagonal systems.
!     This avoids some divions and allows for some vectorization
!      and SIMD optimizations
!
!     See Quarterioni, Sacco and Saleri, "Numerical Mathematics",
!      Springer Verlag, p93
!
!     See also: Wang et al "A Parallel Solver for Incompressible Fluid Flows"
!     Procedia Computer Science V18, 2013, p439-448
!      available as   https://doi.org/10.1016/j.procs.2013.05.207
!
!     Implementation notes:
!      We only implement the case NRHS=1 because that seems
!      to be all that is required by MFiX.
!
!     We do not return the elements of the LU factorization, because
!      we are not computing LU factorization, and MFiX is not using
!      this data anyway
!
      if (nrhs .eq. 1) then
         GG(1) = 1.0 / D(1)
         TMP = DU(1:n-1)*DL(1:n-1)
         do 1 I = 2, N
!           GG(i) = 1.0 / (D(i) - DL(i-1)*GG(i-1)*DU(i-1))
            GG(i) = 1.0 / (D(i) - GG(i-1)*TMP(i-1))
 1       continue

         YY(1) = GG(1) * B(1,1)
         do 2 I = 2, N
            YY(i) = GG(i) * (B(i,1) - DL(i-1)*YY(i-1))
 2       continue

         B(n,1) = YY(n)
         TMP = GG(1:n-1)*DU(1:n-1)
         do 3 I = n-1, 1, -1
!           B(i,1) = YY(i) - GG(i)*DU(i)*B(i+1, 1)
            B(i,1) = YY(i) - TMP(i)*B(i+1, 1)
 3       continue
         return
      end if

! Although the code below should not be executed any more
! with the new Thomas algorithm above (NRHS=1), it is
! kept in case we need to revert to it in the future.

      IF( NRHS.EQ.1 ) THEN
         DO 10 I = 1, N - 2
            IF( ABS( D( I ) ).GE.ABS( DL( I ) ) ) THEN
!
!              No row interchange required
!
               IF( D( I ).NE.ZERO ) THEN
                  FACT = DL( I ) / D( I )
                  D( I+1 ) = D( I+1 ) - FACT*DU( I )
                  B( I+1, 1 ) = B( I+1, 1 ) - FACT*B( I, 1 )
               ELSE
                  INFO = I
                  RETURN
               END IF
               DL( I ) = ZERO
            ELSE
!
!              Interchange rows I and I+1
!
               FACT = D( I ) / DL( I )
               D( I ) = DL( I )
               TEMP = D( I+1 )
               D( I+1 ) = DU( I ) - FACT*TEMP
               DL( I ) = DU( I+1 )
               DU( I+1 ) = -FACT*DL( I )
               DU( I ) = TEMP
               TEMP = B( I, 1 )
               B( I, 1 ) = B( I+1, 1 )
               B( I+1, 1 ) = TEMP - FACT*B( I+1, 1 )
            END IF
   10    CONTINUE
         IF( N.GT.1 ) THEN
            I = N - 1
            IF( ABS( D( I ) ).GE.ABS( DL( I ) ) ) THEN
               IF( D( I ).NE.ZERO ) THEN
                  FACT = DL( I ) / D( I )
                  D( I+1 ) = D( I+1 ) - FACT*DU( I )
                  B( I+1, 1 ) = B( I+1, 1 ) - FACT*B( I, 1 )
               ELSE
                  INFO = I
                  RETURN
               END IF
            ELSE
               FACT = D( I ) / DL( I )
               D( I ) = DL( I )
               TEMP = D( I+1 )
               D( I+1 ) = DU( I ) - FACT*TEMP
               DU( I ) = TEMP
               TEMP = B( I, 1 )
               B( I, 1 ) = B( I+1, 1 )
               B( I+1, 1 ) = TEMP - FACT*B( I+1, 1 )
            END IF
         END IF
         IF( D( N ).EQ.ZERO ) THEN
            INFO = N
            RETURN
         END IF
      ELSE
         DO 40 I = 1, N - 2
            IF( ABS( D( I ) ).GE.ABS( DL( I ) ) ) THEN
!
!              No row interchange required
!
               IF( D( I ).NE.ZERO ) THEN
                  FACT = DL( I ) / D( I )
                  D( I+1 ) = D( I+1 ) - FACT*DU( I )
                  DO 20 J = 1, NRHS
                     B( I+1, J ) = B( I+1, J ) - FACT*B( I, J )
   20             CONTINUE
               ELSE
                  INFO = I
                  RETURN
               END IF
               DL( I ) = ZERO
            ELSE
!
!              Interchange rows I and I+1
!
               FACT = D( I ) / DL( I )
               D( I ) = DL( I )
               TEMP = D( I+1 )
               D( I+1 ) = DU( I ) - FACT*TEMP
               DL( I ) = DU( I+1 )
               DU( I+1 ) = -FACT*DL( I )
               DU( I ) = TEMP
               DO 30 J = 1, NRHS
                  TEMP = B( I, J )
                  B( I, J ) = B( I+1, J )
                  B( I+1, J ) = TEMP - FACT*B( I+1, J )
   30          CONTINUE
            END IF
   40    CONTINUE
         IF( N.GT.1 ) THEN
            I = N - 1
            IF( ABS( D( I ) ).GE.ABS( DL( I ) ) ) THEN
               IF( D( I ).NE.ZERO ) THEN
                  FACT = DL( I ) / D( I )
                  D( I+1 ) = D( I+1 ) - FACT*DU( I )
                  DO 50 J = 1, NRHS
                     B( I+1, J ) = B( I+1, J ) - FACT*B( I, J )
   50             CONTINUE
               ELSE
                  INFO = I
                  RETURN
               END IF
            ELSE
               FACT = D( I ) / DL( I )
               D( I ) = DL( I )
               TEMP = D( I+1 )
               D( I+1 ) = DU( I ) - FACT*TEMP
               DU( I ) = TEMP
               DO 60 J = 1, NRHS
                  TEMP = B( I, J )
                  B( I, J ) = B( I+1, J )
                  B( I+1, J ) = TEMP - FACT*B( I+1, J )
   60          CONTINUE
            END IF
         END IF
         IF( D( N ).EQ.ZERO ) THEN
            INFO = N
            RETURN
         END IF
      END IF
!
!     Back solve with the matrix U from the factorization.
!
      IF( NRHS.LE.2 ) THEN
         J = 1
   70    CONTINUE
         B( N, J ) = B( N, J ) / D( N )
         IF( N.GT.1 ) &
            B( N-1, J ) = ( B( N-1, J )-DU( N-1 )*B( N, J ) ) / D( N-1 )
         DO 80 I = N - 2, 1, -1
            B( I, J ) = ( B( I, J )-DU( I )*B( I+1, J )-DL( I )* &
                        B( I+2, J ) ) / D( I )
   80    CONTINUE
         IF( J.LT.NRHS ) THEN
            J = J + 1
            GO TO 70
         END IF
      ELSE
         DO 100 J = 1, NRHS
            B( N, J ) = B( N, J ) / D( N )
            IF( N.GT.1 ) &
               B( N-1, J ) = ( B( N-1, J )-DU( N-1 )*B( N, J ) ) / &
                             D( N-1 )
            DO 90 I = N - 2, 1, -1
               B( I, J ) = ( B( I, J )-DU( I )*B( I+1, J )-DL( I )*&
                           B( I+2, J ) ) / D( I )
   90       CONTINUE
  100    CONTINUE
      END IF
!
      RETURN
!
!     End of DGTSV
!
      END SUBROUTINE DGTSV

END MODULE
