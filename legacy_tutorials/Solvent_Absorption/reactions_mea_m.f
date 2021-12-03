!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: REACTIONS_M (File = reactions_mea_m.f90)               C
!                                                                      C    
!  Purpose: This module contains the equilibrium description for       C  
!  aequeous monoethanolamine (MEA) solutions loaded with CO2. The      C   
!  equilibrium composition of 9 species in solution is govrned by 5    C
!  reactions:                                                          C 
!  1.  MEACOO- + H20  <--> MEA + HCO3- (Reversion of Carbamate)        C
!  2.  CO2 + 2H2O <--> HCO3- + H3O+    (Dissociation of CO2)           C 
!  3.  HCO3- + H2O --> CO3-- + H3O+    (Dissociation of Bicarbonate)   C
!  4.  MEAH+ + H2O <--> MEA + H3O+      (Dissociation of MEAH+)        C
!  5.  2H2O <--> H3O+ + OH-            (Ionization of Water)           C  
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
      MODULE REACTIONS_M
      IMPLICIT NONE
      
      !Include the MFIX Species file for species names.
      include 'species.inc'
      
      !Number of Species in equilibrium
      INTEGER,PARAMETER:: NS = 9 

      !Number of equilibrium reactions
      INTEGER,PARAMETER:: NR = 5 
      
      !Name of reactions 
      CHARACTER(len=22),PARAMETER:: REACTION_NAME(NR) = (/ &
         '  Carbamate Reversion',&
         '     Dissociation CO2',&
         '    Dissociation HCO3',&
         '    Dissociation MEAH',&
         '       Ionization H2O' &
         /)
         
      !Index of solvent species:
      INTEGER,PARAMETER:: SOLVENT = lrnh2

      !Index of solute species
      INTEGER, PARAMETER:: SOLUTE = lco2

      !Index of H2O:
      INTEGER,PARAMETER:: WATER = lh2o
         
      !Initial Temperature [deg Kelvin]: Set in solve_chem_eq
      DOUBLE PRECISION,SAVE:: T_0 
      
      !Initial mass fraction:  Set in solve_chem_eq
      DOUBLE PRECISION,SAVE:: X_0(NS)
      
      !Ratio of specific heat to gas constant, Cp/R
      DOUBLE PRECISION, SAVE:: CP_OVER_GASCONST

      ! Minimum H2O mass fraction in capsule.  This is used to prevent the
      ! capsule from completely drying out.  Set to something small, but
      ! nozero.
      DOUBLE PRECISION, PARAMETER:: MIN_H2O_MASS_FRACTION = 0.001 


      contains

!---------------------------------------------------------------------
! The system of Equilibrium constraints for this system.  There should
! be one equation for each equilibrium reaction.
!---------------------------------------------------------------------
      FUNCTION EQ_CONSTRAINT(N,R)
         !The number of Equations (must equal NR)
         INTEGER, INTENT(IN):: N
         !The molar shift of each reaction [mol/kg of capsule]
         DOUBLE PRECISION, INTENT(IN):: R(N)
         !Error associated with each equilibrium constraint.
         DOUBLE PRECISION,DIMENSION(N) :: EQ_CONSTRAINT
         !-----
         !Equilibrium constants [mol/kg H2O]
         DOUBLE PRECISION,DIMENSION(N):: K
         !Equilibrium molalities
         DOUBLE PRECISION:: B_EQ(NS)
         !Temperature at Eq. [deg Kelvin] 
         DOUBLE PRECISION,SAVE:: T_EQ 
         !-----

         !Compute the temperature at equilibrium based on current guess
         !for rates
         !T_EQ = TEMP_UPDATE(T_0,R) 
         !Or... Just assume it is constant and update later
         T_EQ =T_0

         !Compute equilibrium concentrations based on current rates
         !and initial mass fractions.
         B_EQ = MOLALITY_UPDATE(X_0,R)

         !Determine the equilibrium constants at T_EQ.
         K = KEQ(T_EQ,RETURN_HEATS=.FALSE.)
         
         !Equilibrium constants: R1-R5
         EQ_CONSTRAINT(1) = K(1)*B_EQ(rnhco2m)-B_EQ(hco3m)*B_EQ(lrnh2)
         EQ_CONSTRAINT(2) = K(2)*B_EQ(lco2)-B_EQ(hco3m)*B_EQ(h3op)
         EQ_CONSTRAINT(3) = K(3)*B_EQ(hco3m)-B_EQ(co3m2)*B_EQ(h3op)
         EQ_CONSTRAINT(4) = K(4)*B_EQ(rnh3p)-B_EQ(lrnh2)*B_EQ(h3op)
         EQ_CONSTRAINT(5) = K(5)-B_EQ(ohm)*B_EQ(h3op)

         !Normalize each equation by the equilibrium constant.
         !This way the error in each equation relects the relative error
         !in K and is dimensionless
         EQ_CONSTRAINT = EQ_CONSTRAINT/K
         
         return
      END FUNCTION EQ_CONSTRAINT
      
!---------------------------------------------------------------------
! Return the value of the reaction quotient for each eq. constraint.
! These are used to check convergence of the solution.
!---------------------------------------------------------------------
      FUNCTION REACTION_QUOTIENT(X)
         ! Mass Fractions
         DOUBLE PRECISION, DIMENSION(NS):: X
         DOUBLE PRECISION, DIMENSION(NR):: REACTION_QUOTIENT
         ! Dummy rates (assumed zero here)
         DOUBLE PRECISION, DIMENSION(NR):: R0=0.0d0
         ! Molalities
         DOUBLE PRECISION, DIMENSION(NS):: B
         
         !Compute molalities
         B= MOLALITY_UPDATE(X,R0)  
        
         ! Define Individual reaction quotients here
         REACTION_QUOTIENT(1) = B(lrnh2)*B(hco3m)/B(rnhco2m)
         REACTION_QUOTIENT(2) = B(hco3m)*B(h3op)/B(lco2)
         REACTION_QUOTIENT(3) = B(co3m2)*B(h3op)/B(hco3m)
         REACTION_QUOTIENT(4) = B(lrnh2)*B(h3op)/B(rnh3p)
         REACTION_QUOTIENT(5) = B(ohm)*B(h3op)
         
      END FUNCTION REACTION_QUOTIENT

!---------------------------------------------------------------------
! Temperature dependent equilibrium constants, K
! These are computed using the coeffients suggested by Aboudheir
! et al [CES Vol 58, pp 5195-5210, 2003]. The values of K can be used 
! with concentrations computed on a molality basis [mol/kg H2O]
!
! The form of ln(KEQ) is assumed to be of the form:
! ln(KEQ) = A + B/T + C*ln(T) + D*T + E*T^2
!
!
! This function can be called in two modes:
! 1.  We return the value of the temperature dependent Eq. Constants,
! KEQ
! 2.  We return the value of ddT(ln(KEQ)) for use in van't Hoff's law:
! \Delta H = ddT(ln(KEQ))*RT^2
!---------------------------------------------------------------------
      FUNCTION KEQ(T,RETURN_HEATS)
         !Temperature [Deg. Kelvin]
         DOUBLE PRECISION, INTENT(IN):: T
         !Flag to return heats of reaction vs KEQ
         LOGICAL, INTENT(IN):: RETURN_HEATS
         !Log of the equilibrium constant, and temperature derivative
         DOUBLE PRECISION, DIMENSION(NR):: LN_KEQ, DDT_LN_KEQ
         !Return values of the equilibrium const or H.o.R. 
         DOUBLE PRECISION, DIMENSION(NR):: KEQ
         !Coefficients for the equilibrium constants:
         !ln(K_EQ) = A + B/T + C*ln(T) + D*T + E*T^2
         DOUBLE PRECISION, PARAMETER,DIMENSION(NR):: &
         A=(/ 6.69425,  235.482  , 220.067,-3.3636 , 140.932   /), &
         B=(/-3090.83, -12092.1  ,-12431.7,-5851.11,-13455.9   /), &
         C=(/ 0.0    , -36.7816  ,-35.4819,  0.0   ,-22.4773   /), &
         D=(/ 0.0    ,  0.0      , 0.0    ,  0.0   , 0.0       /), &
         E=(/ 0.0    ,  0.0      , 0.0    ,  0.0   , 0.0       /)
         
         
         if(RETURN_HEATS) then
            !Derivative of ln(KEQ) wrt. T:  [1/K]
            DDT_LN_KEQ = -B/T**2 + C/T + D + 2.0*E*T
            KEQ = DDT_LN_KEQ
         else
            !Just compute and return KEQ
            LN_KEQ = A + B/T + C*log(T) + D*T + E*T**2
            KEQ = exp(LN_KEQ)  
         endif

      END FUNCTION KEQ

!---------------------------------------------------------------------
! This function sets the stoichiometric matrix for the equilibrium
! reactions.  We use the MFIX indices passed in at runtime.
!---------------------------------------------------------------------
      FUNCTION SMX()
      DOUBLE PRECISION, DIMENSION (NR,NS):: SMX
         SMX(:,lco2)    =  (/  0.0,-1.0, 0.0, 0.0, 0.0   /) !CO2
         SMX(:,lrnh2)   =  (/  1.0, 0.0, 0.0, 1.0, 0.0   /) !MEA
         SMX(:,rnh3p)   =  (/  0.0, 0.0, 0.0,-1.0, 0.0   /) !MEAH
         SMX(:,hco3m)   =  (/  1.0, 1.0,-1.0, 0.0, 0.0   /) !HCO3
         SMX(:,ohm)     =  (/  0.0, 0.0, 0.0, 0.0, 1.0   /) !OH
         SMX(:,co3m2)   =  (/  0.0, 0.0, 1.0, 0.0, 0.0   /) !CO3
         SMX(:,h3op)    =  (/  0.0, 1.0, 1.0, 1.0, 1.0   /) !H3O
         SMX(:,rnhco2m) =  (/ -1.0, 0.0, 0.0, 0.0, 0.0   /) !MEACOO
         SMX(:,lh2o)    =  (/ -1.0,-2.0,-1.0,-1.0,-2.0   /) !H2O
      END FUNCTION SMX

!---------------------------------------------------------------------
! This function returns molecular weights [kg/mol]
!---------------------------------------------------------------------
      FUNCTION MOLECULAR_WEIGHT()
         DOUBLE PRECISION, DIMENSION(NS):: MOLECULAR_WEIGHT
         MOLECULAR_WEIGHT(lco2)     =  0.044
         MOLECULAR_WEIGHT(lrnh2)    =  0.061
         MOLECULAR_WEIGHT(rnh3p)    =  0.062
         MOLECULAR_WEIGHT(hco3m)    =  0.061
         MOLECULAR_WEIGHT(ohm)      =  0.017
         MOLECULAR_WEIGHT(co3m2)    =  0.060
         MOLECULAR_WEIGHT(h3op)     =  0.019
         MOLECULAR_WEIGHT(rnhco2m)  =  0.104
         MOLECULAR_WEIGHT(lh2o)     =  0.018
      END FUNCTION MOLECULAR_WEIGHT

!---------------------------------------------------------------------
! This function updates the molality of each species [mol/kg h2o].
! This should not be dependent of the solvent, but we keep it here so
! it is easily used bu the EQ_CONSTRAINT function.
!---------------------------------------------------------------------
      FUNCTION MOLALITY_UPDATE(X,R)
         !Mass fractions
         DOUBLE PRECISION, INTENT(IN):: X(NS)
         !Molar shift of equilibrium reactions [mol/kg of capsule]
         DOUBLE PRECISION, INTENT(IN):: R(NR)  
         !The molality at equilibrium
         DOUBLE PRECISION, DIMENSION(NS):: MOLALITY_UPDATE ! mol/kg h2o
         !Stoichiometric matrix
         DOUBLE PRECISION:: STOICH_MX(NR,NS)
         !Molecular Weights in kg/mol
         DOUBLE PRECISION:: MW(NS)
         !Species index
         INTEGER:: is
         !-----
        
         ! The stoichiometric matrix
         STOICH_MX = SMX()
         
         ! Molecular wts [kg/mol]
         MW = MOLECULAR_WEIGHT()
         
         ! Molality: [mol/kg H2O]
         do is = 1, NS
            MOLALITY_UPDATE(is)=&
            (X(is)/MW(is)+sum(STOICH_MX(1:NR,is)*R(1:NR)))&
            /(X(WATER)+ MW(WATER)*sum(STOICH_MX(1:NR,WATER)*R(1:NR)))
         enddo

      END FUNCTION MOLALITY_UPDATE



!---------------------------------------------------------------------
! This function updates the temperature of the particle based on the
! change in moles associated with each Eq Reaction (per kg capsule).
!
! Van't Hoff's Law for enthalpy change associated with Eq. RXN:
!     Delta H = d/dT(ln KEQ)*GasConst*T**2 [J]
!
! Energy Produced by all Eq RXNs (divided by gas const):
!     Q/GasConst = sum(R_i*d/dT(ln KEQ_i))*T**2 [mol-K/kg]
!
! Change in temp:  Delta T = (Delta H/GasConst) / (Cp/GasConst) [K]
!
! Update:  T_new = T + Delta T
!---------------------------------------------------------------------
      FUNCTION TEMP_UPDATE(T_0,R)
         !Initial temperature [K]
         DOUBLE PRECISION:: T_0
         !Change in moles associated with each EQ Rxn [moles/kg capsule]
         DOUBLE PRECISION, INTENT(IN):: R(NR)
         !New temperature
         DOUBLE PRECISION:: TEMP_UPDATE
         !Enthalpy change (divided by gas const)
         DOUBLE PRECISION:: Q_OVER_GASCONST
         !-----

         !If Cp = 0, we just assume isothermal
         if (CP_OVER_GASCONST <= 0.0d0) then
            TEMP_UPDATE = T_0
            return
         endif
         
         !Use van't hoff's law to get net energy produced/consumed
         Q_OVER_GASCONST =  sum(R*KEQ(T_0,RETURN_HEATS=.TRUE.))*T_0**2
      
         !The new temperature [K]:
         TEMP_UPDATE = T_0 - Q_OVER_GASCONST / CP_OVER_GASCONST

      END FUNCTION TEMP_UPDATE

      END MODULE REACTIONS_M
