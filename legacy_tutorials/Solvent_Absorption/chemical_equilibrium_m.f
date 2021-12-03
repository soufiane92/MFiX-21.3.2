!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: CHEMICAL_EQUILIBRIUM                                   C
!  Purpose: Determine the equilibrium chemical composition of a liquid C
!  solution governed by several equilibrium reactions.                 C
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
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      MODULE CHEMICAL_EQUILIBRIUM_M
      implicit none

      contains

!----------------------------------------------------------------------!
! Function: SOLVE_CHEM_EQ                                              !
!                                                                      !
! Purpose:  Given an initial set of mass fractions (X) for NS species  !
! in liquid solution, this routine solves for the mass fractions that  !
! satisfy NR equilibrium reaction constraints.  These constraints      !
! are solution specific, and are defined in the REACTIONS_M module.    !
!                                                                      !  
! Arguments:                                                           !
!  X:  The initial (input) and equilibrium (output) mass fractions     !
!  T: The Solution Temperature in degrees Kelvin                    ! 
!  TOL:  Solution tolerance                                            !          
!  ITER_MAX:  Maximum number of iterations to perform in solver        !
!  VERBOSE: Logical flag to turn on/off debugging and convergence info !
!  SUCCESS:  Logical flag indicating whether the routine succeeded      !
!      = 0 if successful                                               ! 
!     /= 0 if unsuccessful                                             !
!                                                                      !
!                                                                      !
! References:  See the working document on MECS Chemistry.             !
!----------------------------------------------------------------------!
      SUBROUTINE SOLVE_CHEM_EQ(X,T,CP_HAT,ISOTHERMAL &
         ,TOL,ITER_MAX,VERBOSE,SUCCESS)
      USE REACTIONS_M
      USE NEWTON_RAPHSON_M
      IMPLICIT NONE 
! Liquid solution mass fractions [-]
      DOUBLE PRECISION, INTENT(INOUT):: X(NS) 
! Particle Temperature [K]
      DOUBLE PRECISION, INTENT(INOUT):: T 
! Specific heat divided by gas constant, Cp/R [mol/kg]
      DOUBLE PRECISION, INTENT(IN):: CP_HAT
! Flag for isothermal calculation
      LOGICAL, INTENT(IN):: ISOTHERMAL
! Numerical solution tolerance 
      DOUBLE PRECISION, INTENT(IN):: TOL 
! Maximum number of iterations
      INTEGER, INTENT(IN):: ITER_MAX 
! Flag to use/suppress verbose output
      LOGICAL,INTENT(IN):: VERBOSE 
! Flag for error tracking
      INTEGER, INTENT(OUT):: SUCCESS 
      !-----
! Change in moles per capsule mass for each reaction: 
! moles/kg (si) or moles/g (cgs)
      DOUBLE PRECISION:: R(NR) 
! Equilibrium constants
      DOUBLE PRECISION:: K(NR) 
! Reaction quotients 
      DOUBLE PRECISION:: Q(NR) 
! Residual of solution
      DOUBLE PRECISION:: RES(NR)  
! Molecular weights
      DOUBLE PRECISION:: MW(NS) 
! Stoichiometric matrix
      DOUBLE PRECISION:: STOICH_MX(NR,NS)
! Pointer to the system of equations
      PROCEDURE(F_SOLVE), POINTER :: F_PTR => null()
! Indices
      INTEGER:: is,ir,iter, isolve
! Number of "outer" solver iterations.  Increasing this may help
! convergence for difficult initial conditions.
      INTEGER, PARAMETER:: NSOLVE = 1
      !-----

! Check that you have enough H2O
      if(X(lh2o)<MIN_H2O_MASS_FRACTION) then
         SUCCESS=0
         return 
      endif

! Store the Cp/R value for access in the reactions module.
! This MUST have units of mol/kg here.
      if(ISOTHERMAL) then
         CP_OVER_GASCONST = 0.0d0
      else
         CP_OVER_GASCONST = CP_HAT
      endif

! The stoichiometric matrix      
      STOICH_MX = SMX()

! Grab the Molecular weights [kg/mol]:
      MW = MOLECULAR_WEIGHT()

! Point to the function containing the equilibrium constraints.
      F_PTR => EQ_CONSTRAINT

! Main "outer" solve loop.
      do isolve=1,NSOLVE
         if(VERBOSE) then
            write(*,'(a,i1)') 'SOLVE CHEMICAL EQUILIBRIUM:',isolve
         endif
   
! Initial temperature (before equilibrium)   
         T_0 = T  

! Initial mass fractions (before equilibrium)
         X_0 = X 

! Guess and solve for R, the molar shift of each reaction.  R has units
! of [moles / mass of capsule]
! R=0 is probably the best initial guess.
! However, on the first timestep if we just start with H2O and solvent,
! or even pure H2O, we will need at least one non zero guess for R, in 
! order to avoid a singular matrix when solving the equations.  There are
! two logical guesses here:
! 1.  We have some solvent, but no Na+.  All solvent gets ionized: 
! R(4) = X(solvent)/MW(solvent).
! 2.  Capsule is pure H2O.  No solvent, ions:  Guess a small number for
! all rates:  R = SMALL
         R = 0.0d0
         if(X(SOLUTE)==0.0d0 .and. X(SOLVENT)>0.0d0) then
            R(4) = X(SOLVENT)/MW(SOLVENT)  
         elseif(x(SOLUTE)==0.0d0.AND.X(SOLVENT)==0.0d0.and.X(WATER)>0.0d0) then
            R=1e-8
         endif

         CALL NEWTON_RAPHSON_SOLVE(NR,R,ITER_MAX,TOL,F_PTR,VERBOSE,SUCCESS)
         RES = EQ_CONSTRAINT(NR,R)

! Update mass fractions, X
         do is=1,NS
            X(is) = X_0(is)+ MW(is)*sum(STOICH_MX(1:NR,is)*R(1:NR))
         enddo

! Check for any (strongly) negative mass fractions.  
! Set slightly -ve values to zero.
         if (any(X < -1000.0*TOL)) then
            write(*,*) 'ERROR: -ve mass fractions:',X
            SUCCESS = 6
            return
         else
            X = max(X,0.0d0)
         endif

! Ensure that all capsule mass fractions sum to one.
! This should help with any MFIX tolerance issues when restarting using
! an equilibrium chemistry solution
         X = X/sum(X)

!Update the temperature, T
         T = TEMP_UPDATE(T_0,R)

! Some optional output info if called with VERBOSE = .TRUE.
         if(VERBOSE) then
            K(1:NR) = KEQ(T,RETURN_HEATS=.FALSE.)
            Q(1:NR) = REACTION_QUOTIENT(X)
            write(*,'(a)') repeat('-',84)
            write(*,1002)'|','EQ. REACTION','|','RESIDUAL','|',&
            'RATE','|','Q','|','K','|','Q/K','|'
            do ir=1,NR
               write(*,1001)'|',REACTION_NAME(ir),'|',RES(ir),'|'&
               ,R(ir),'|',Q(ir),'|',K(ir),'|',Q(ir)/K(ir),'|'
            enddo
            write(*,'(a1,a22,a,f7.2,a,d10.4)') '|','TEMPERATURE [K]',&
               ' = ' , T , ', Delta T [K] = ', T-T_0
            write(*,'(a)') repeat('-',84)
         endif

! If the solver converged successfully, we are done.         
         if(SUCCESS==0) return 
      enddo

1001 format(a1,a22,a1,5(ES11.2,a1))
1002 format (a1,a22,a1,5(a11,a1))         
1003 format(a1,a22,a1,a11,a1,4(ES11.2,a1))
      END SUBROUTINE SOLVE_CHEM_EQ

      END MODULE CHEMICAL_EQUILIBRIUM_M
