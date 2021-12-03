!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: NEWTON_RAPHSON                                         C
!  Purpose: Solve N non-linear equations for N unknowns.               C
!                                                                      C
!  Author: J.Finn                                     Date: 03-02-17   C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      MODULE NEWTON_RAPHSON_M

      IMPLICIT NONE
      !-----

! Machine Precision
      DOUBLE PRECISION, PARAMETER,PRIVATE:: EPS = 10e-16

! Tolerance for line search
! If the newton iterations diverge, try decreasing this value
      DOUBLE PRECISION,PARAMETER,PRIVATE:: TOL_LS = 0.1


! Errors corrsponding to error codes returned in SUCCESS var
      CHARACTER(LEN=50), PARAMETER:: NEWTON_RAPHSON_ERRORS(6) = (/ &
      'ZERO JACOBIAN, FALSE CONVERGENCE                 ', &
      'NEWTON-RAPHSON STEP IS ZERO                      ', &
      'MAX ITERATIONS EXCEEDED                          ', &
      'FOUND NaN DURING SOLVE                           ', &
      'SINGULAR MATRIX (IN LUDCMP)                      ',&
      'UNPHYSICAL SOLUTION                              '&
      /)

! Interface for the function to solve.
      ABSTRACT INTERFACE
         FUNCTION F_SOLVE(N,R)
            INTEGER, INTENT(IN):: N
            DOUBLE PRECISION, INTENT(IN):: R(N)
            DOUBLE PRECISION,DIMENSION(N) :: F_SOLVE
         END FUNCTION F_SOLVE
      END INTERFACE

      contains

! ----------------------------------------------------------------------
! SUBROUTINE NEWTON_RAPHSON_SOLVE(N,X,ITER_MAX,TOL,F_PTR)
!
! PURPOSE
! Solve a system of N nonlinear equations for N unknowns.
!
! INPUT
! N:  The number of unknowns/equations
! X:  Initial guess (input) and solution (output) for N unknowns
! ITER_MAX: Max number of iterations to perform
! TOL: Convergence tolerance
! F_PTR: Pointer to the function of N equations to solve
! XMIN: Minimum bound on solution (Optional)
! XMAX: Maximum bound on solution (Optional)
!
! OUTPUT
! X: Solution to the problem.
! SUCCESS:  Flag indicating successful/unsuccessful exit
! ----------------------------------------------------------------------
      SUBROUTINE NEWTON_RAPHSON_SOLVE(N,X,ITER_MAX,TOL,F_PTR,VERBOSE, &
         SUCCESS,XMIN, XMAX)
      INTEGER, INTENT(IN):: N
      DOUBLE PRECISION, INTENT(INOUT):: X(N)
      INTEGER, INTENT(IN):: ITER_MAX
      DOUBLE PRECISION, INTENT(IN):: TOL
      PROCEDURE(F_SOLVE), POINTER, INTENT(IN) :: F_PTR
      LOGICAL,INTENT(IN):: VERBOSE
      INTEGER,INTENT(OUT):: SUCCESS
      DOUBLE PRECISION, INTENT(IN),OPTIONAL:: XMIN(N)
      DOUBLE PRECISION, INTENT(IN),OPTIONAL:: XMAX(N)
      !-----
! Iteration counter
      INTEGER:: ITER
! Function evaluations
      DOUBLE PRECISION:: F(N), F_XPS(N)
! Jacobian of the function
      DOUBLE PRECISION:: DF(N,N)
! Scale factor for newton step
      DOUBLE PRECISION:: LAMBDA
! The newton step
      DOUBLE PRECISION:: S(N)
! LU Decomposition variables
      integer :: indx(N)
      DOUBLE PRECISION:: d
      DOUBLE PRECISION:: LMAX,L1,L2,CONV
      !-----

      if(VERBOSE) then
      write(*,1003)'Iteration','|',' L1 ERROR ','|',' L2 Error ','|',&
      'LMAX Error','|',' Conv Rate ','|',' Lambda ','|'
      write(*,'(a)') repeat('-',72)
      endif

      ITER = 0
      F = F_PTR(N,X)
      DO
         ITER = ITER + 1

! Evaluate Jacobian of the function at X (and check for false conv.)
         DF = JACOBIAN(N,X,F_PTR)
         if(maxval(abs(DF)) < EPS) then
            SUCCESS = 1
            return
         endif

! Solve DF(R_n)*S = -F(R_n) for the full Newton step, S
! using LU decomposition / back substitution. Check for NaNs.
         S = -F
         CALL LUDCMP(DF, N, N, indx, d, 'NEWTON_RAPHSON_SOLVE',SUCCESS)
         if(SUCCESS==5) then
            return
         endif
         CALL LUBKSB(DF, N, N, indx, S)
         if(any(isnan(S))) then
            SUCCESS = 4
            return
         endif
         if(maxval(abs(S)) == 0.0) then
            SUCCESS = 2
            return
         endif

! Use a golden section search to find the step size (LAMBDA*S)
! that minimizes the error at the next step.  For the first few
! iterations, we can use a lower line search tolerance, to be more
! cautious about overshooting the solution.
!         if(ITER <= 5) then
!            call GOLDEN_MIN(N,LAMBDA,X,S,0.1d0*TOL_LS,F_PTR)
!         else
            call GOLDEN_MIN(N,LAMBDA,X,S,TOL_LS,F_PTR)
!         endif

! Perform the Newton Step
         X = X + LAMBDA*S

! Impose bounds on solution:
         if(PRESENT(XMIN)) then
            X = max(X,XMIN)
         endif
         if(PRESENT(XMAX)) then
            X = min(X,XMAX)
         endif

!Convergence rate and Error norms
         F = F_PTR(N,X)
         if(iter>1) then
            CONV= sqrt(dot_product(F,F))/L2
         else
            CONV=0.0
         endif
         LMAX = maxval(abs(F))
         L1 = sum(abs(F))
         L2 = sqrt(dot_product(F,F))

! Optional verbose output of every iteration:
         if(VERBOSE)then
            write(*,1002) ITER,L1,L2,LMAX,CONV,LAMBDA
         endif

! Check convergence
         IF (LMAX < TOL .AND. ITER > 2 .AND. CONV < 1.0d0) then
            SUCCESS = 0
            return
         endif

! Check if we have performed maximum iterations.
         if(ITER == ITER_MAX) then
            SUCCESS = 3
            return
         endif
      ENDDO

1002 format (i11,11(es12.3))
1003 format (6(a11,a1))
      END SUBROUTINE NEWTON_RAPHSON_SOLVE

! -----
! Jacobian matrix of the N-Dimension function WRT N unknowns X
! Use central differencing with step size X*eps**(1/3), which is
! recommended by numerical recipes.
! -----
      FUNCTION JACOBIAN(N,X,F_PTR)
      INTEGER, INTENT(IN):: N
      DOUBLE PRECISION,INTENT(IN):: X(N)
      PROCEDURE(F_SOLVE), POINTER, INTENT(IN) :: F_PTR
      DOUBLE PRECISION:: JACOBIAN(N,N)
      !-----
      DOUBLE PRECISION:: Xa(N),Xb(N)
      INTEGER:: i
      DOUBLE PRECISION:: DELTA
      DOUBLE PRECISION, PARAMETER:: EPS_13 = EPS**(1.0/3.0)
      !-----
      do i = 1, N
         Xa = X; Xb=X;
         DELTA = max(abs(X(i))*EPS_13,EPS_13)
         Xa(i) = X(i) - DELTA
         Xb(i) = X(i) + DELTA
         JACOBIAN(1:N,i)=(F_PTR(N,Xb)-F_PTR(N,Xa))/(Xb(i)-Xa(i))
      enddo
      END FUNCTION JACOBIAN

!----------------------------------------------------------------------!
! Subroutine:  GOLDEN_MIN(N,LAMBDA,R,S,K,C,SF,TOL)                     !
! Find the value of Lambda where the function f(R,K,C) is minimized on !
! the interval [R, R+Lambda*S], where 0 < Lambda < 1.  This is the     !
! multidimensional version of the GOLDEN_MIN search above.             !
!----------------------------------------------------------------------!
      SUBROUTINE GOLDEN_MIN(N,LAMBDA,R,S,TOL,F_PTR)
      IMPLICIT NONE
      INTEGER, INTENT(IN):: N
      DOUBLE PRECISION, INTENT(OUT):: LAMBDA
      DOUBLE PRECISION, INTENT(IN):: R(N)
      DOUBLE PRECISION, INTENT(IN):: S(N)
      DOUBLE PRECISION, INTENT(IN):: TOL
      PROCEDURE(F_SOLVE), POINTER, INTENT(IN) :: F_PTR
      !-----
      DOUBLE PRECISION,PARAMETER:: G = 0.618033988 ! 1.0 - Golden ratio
      INTEGER,PARAMETER:: MAX_ITER = 1000
      DOUBLE PRECISION:: a(N), b(N), x1(N), x2(N)
      DOUBLE PRECISION:: f1,f2
      DOUBLE PRECISION:: resid,delx(N),smag
      integer:: i
      !-----

! Initial bounds
      a = R
      b = R+S

! Magnitude of the S Vector
      smag = sqrt(dot_product(S,S))

! Bracket the minimum function value:
      DO i = 1,MAX_ITER
         x1 = a + G*(b-a)
         x2 = b - G*(b-a)

         f1=dot_product(F_PTR(N,x1),F_PTR(N,x1))
         f2=dot_product(F_PTR(N,x2),F_PTR(N,x2))

         IF ( f1 < f2) THEN
            a = x2 ; x2 = x1 ; f2 = f1
            x1 = a + G*(b-a) ; f1=dot_product(F_PTR(N,x1),F_PTR(N,x1))
         ELSE
            b = x1 ; x1 = x2 ; f1 = f2
            x2 = b - G*(b-a) ; f2=dot_product(F_PTR(N,x2),F_PTR(N,x2))
         ENDIF

! Check convergence:
         delx = x1-x2
         resid = sqrt(dot_product(delx,delx))/smag
         if(resid < TOL) then
            exit
         endif

      ENDDO

! Compute Lambda as middle of interval...
      delx = 0.5*(x1+x2)-R
      LAMBDA = sqrt(dot_product(delx,delx))/smag

! Ensure that Lambda is not too small or large
      LAMBDA = max(LAMBDA,TOL)
      LAMBDA = min(LAMBDA,1.0d0)

      END SUBROUTINE GOLDEN_MIN


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!
!  subroutine name: ludcmp(a,n,np,indx,d, calledFrom)
!  Purpose: Replaces matrix a (n,n) by the LU decomposition of a rowwise
!           permutation of itself. Used in combination with lubksb.
!
!  Returns SUCCESS = 0, unless an error is found.
!
!  Literature/Document References:
!     Numerical Recipies in Fortran 77, page 38-39
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      subroutine ludcmp(a,n,np,indx,d,calledFrom,success)
      implicit none
!-----------------------------------------------
! Dummy arguments
!-----------------------------------------------
      integer, intent(in) :: n
      DOUBLE PRECISION, intent(inout) :: a(n,n)
      integer :: np
      integer, intent(out) :: indx(n)
      DOUBLE PRECISION, intent(out) :: d
      CHARACTER(len=*), intent(in) :: calledFrom
      integer, intent(out):: success
!-----------------------------------------------
! Local parameters
!-----------------------------------------------
      integer :: nmax
      DOUBLE PRECISION :: TINY
      parameter (NMAX=500, TINY=1.0D-20)
!-----------------------------------------------
! Local variables
!-----------------------------------------------
      integer :: i, j, k, imax
      DOUBLE PRECISION :: vv(NMAX)
      DOUBLE PRECISION :: aamax, sum, dum
!-----------------------------------------------

      success = 0

      d = 1.0d0
      do i = 1,n
         aamax=0.0d0
         do j = 1,n
            if (abs(a(i,j)).gt.aamax) aamax = abs(a(i,j))
         enddo
         if (aamax.eq.0.0d0) then
            !SINGULAR MATRIX, return an error:
            success = 5
            return
         endif
         vv(i) = 1.d0/aamax
      enddo
      do j = 1,n
         do i = 1,j-1
            sum = a(i,j)
            do k = 1,i-1
               sum = sum-a(i,k)*a(k,j)
            enddo
            a(i,j) = sum
         enddo
         aamax = 0.0d0
         do i = j,n
            sum = a(i,j)
            do k = 1,j-1
               sum = sum-a(i,k)*a(k,j)
            enddo
            a(i,j) = sum
            dum = vv(i)*abs(sum)
            if (dum.ge.aamax) then
               imax = i
               aamax = dum
            endif
         enddo
         if (j.ne.imax) then
            do k = 1,n
               dum = a(imax,k)
               a(imax,k) = a(j,k)
               a(j,k) = dum
            enddo
            d = -d
            vv(imax) = vv(j)
         endif
         indx(j) = imax
         if (a(j,j).eq.0.0d0) a(j,j) = TINY
         if (j.ne.n) then
            dum = 1.0d0/a(j,j)
            do i = j+1,n
               a(i,j) = a(i,j)*dum
            enddo
         endif
      enddo

      return
      end subroutine ludcmp

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!
!  subroutine name: lubksb (a,n,np,indx,b)
!  Purpose: solves the set of n linear equations A(n,n).X(n) = B(n).
!
!
!  Literature/Document References:
!     Numerical Recipies in Fortran 77, page 39.
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      subroutine lubksb (a,n,np,indx,b)

      implicit none
!-----------------------------------------------
! Dummy arguments
!-----------------------------------------------
      integer, intent(in) :: n
      DOUBLE PRECISION, intent(in) :: a(n,n)
      integer :: np
      integer, intent(in) :: indx(n)
      DOUBLE PRECISION, intent(inout) :: b(n)
!-----------------------------------------------
! Local variables
!-----------------------------------------------
      integer :: i, ii, j, ll
      DOUBLE PRECISION :: sum
!-----------------------------------------------

      ii=0
      do i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0) then
          do j=ii,i-1
            sum=sum-a(i,j)*b(j)
          enddo
         elseif (sum.ne.0.d0) then
           ii=i
         endif
         b(i)=sum
      enddo
       do i=n,1,-1
         sum=b(i)
         if (i.lt.n) then
           do j=i+1,n
             sum=sum-a(i,j)*b(j)
           enddo
         endif
         b(i)=sum/a(i,i)
       enddo
       return
       end subroutine lubksb

      END MODULE NEWTON_RAPHSON_M
