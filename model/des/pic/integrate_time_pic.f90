!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: CFNEWVALUES                                            !
!                                                                      !
!  Purpose: DES - Calculate the new values of particle velocity,       !
!           position, angular velocity etc                             !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
SUBROUTINE INTEGRATE_TIME_PIC(PIC_ITERS)

  ! Defines general problem dimensions, basically the variables involving DIM
  USE param
  ! Pre-defines word numbers like ONE, HALF, SMALL_NUMBER
  USE param1
  ! Small definitions file for parallel processing; don't think I need this yet
  USE parallel
  ! Physical property data like RO_Xs0, MU_g, etc
  USE physprop
  ! Simple constants like EP_star and model specific constants
  USE constant
  ! All the calculable field variables like EP_g, RO_g, U_g, etc
  USE fldvar, only: ep_g, u_s, v_s, w_s
  ! Defines simple particle variables like MAX_PIP, DT_SOLID, logicals for DEM
  USE discretelement
  ! DEM routine that manages boundary conditions; not sure I need this overhead
  USE des_bc
  ! Sets up mpi; don't think I need this yet
  USE mpi_utility
  ! Lots of cartesian grid management; more than just cutcell needs
  USE cutcell
  ! MP_PIC specific variables
  USE mfix_pic
  ! Random number generator variables and functions
  USE randomno
  ! Includes XLENGTH, DX, IMAX, etc.  The DO_K are logical for 3rd dim
  USE geometry, only: DO_K, NO_K
  ! Solids bulk density
  use fldvar, only: rop_s
  ! Inline function definitions for various AVG; not sure I need this overhead
  USE fun_avg
  ! Tons of functions like IP_OF, FLOW_AT_E, etc
  ! Plus more functions that give back IJK as single value & tests for ghosts
  USE functions
  use geometry, only: x_min,x_max,y_min,y_max,z_min,z_max

  USE run, only: DT,DT_PREV



  IMPLICIT NONE

   INTEGER, intent(in)::PIC_ITERS

  ! Setting up local variables

  ! NP = number of particles, LC = loop count, PC = particle count
  INTEGER:: NP, LC
  ! DP_BAR = drag coefficient / parcel mass (D sub p, Snider)
  DOUBLE PRECISION:: DP_BAR
  ! ENp1 = restitution coefficient plus 1 (MP-PIC specific)
  ! n + 1
  DOUBLE PRECISION:: ENp1
  ! VEL() = Particle velocity (integrated) without particle normal stress
  DOUBLE PRECISION:: VEL(3)
  ! DELUP() = estimate of parcel velocity using particle normal stress
  ! Del u sub p (Snider)
  DOUBLE PRECISION:: DELUP(3)
  ! UPRIMETAU() = actual parcel velocity incorporating particle normal stress
  ! u prime sub tau (Snider)
  DOUBLE PRECISION:: UPRIMETAU(3)
  ! SLIPVEL() = slip velocity between parcels and bulk
  DOUBLE PRECISION:: SLIPVEL(3)
  ! DIST() = distance travelled by a parcel based on velocity
  DOUBLE PRECISION:: DIST(3)
  ! DIST_MAG = total magnitude of distance
  DOUBLE PRECISION:: DIST_MAG
  ! MAX_VEL = maximum velocity of all of the parcels
  DOUBLE PRECISION:: MAX_VEL
  ! l3SQT2 = 3 on sqrt(2)
  DOUBLE PRECISION:: l3SQT2
  ! EPg_MFP = Gas volume fraction based on mean free path to limit parcel
  ! movement in close pack
  DOUBLE PRECISION:: EPg_MFP
  ! MFP = mean free path of parcels based on current EPg
  DOUBLE PRECISION:: MFP_VEL
  ! EPs = solids volume fraction
  DOUBLE PRECISION:: EPs
  ! LC_BND = loop count bound; IJK = max (local) IJK location
  INTEGER:: LC_BND, IJK
  ! VEL_FAC = empirical velocity scaling factor; programmer's control
  ! for testing only
  DOUBLE PRECISION :: VEL_FAC, MFP
  DOUBLE PRECISION, parameter :: minROPs = epsilon(0.0)

!NEW NEW NEW NEW NEW
!variables needed for collision damping
! mass-averaged velocity
  DOUBLE PRECISION :: AVG_MASS_VEL(DIMENSION_3,3)
! time frequency for collision damping model
  DOUBLE PRECISION :: ONE_OVER_TAU_D(DIMENSION_3,3)
! adjustment to velocity from collision damping effect
  DOUBLE PRECISION :: ADJ_VEL(3)
! time ratio for collision damping effect
  DOUBLE PRECISION :: TIME_RATIO(3)
! distance moved from collision affect
  DOUBLE PRECISION :: COLL_DIST(3)
! local parccel density variables (sum and average)
  DOUBLE PRECISION :: SUM_LOCAL_PARCEL_DENS(DIMENSION_3)
  DOUBLE PRECISION :: AVG_LOCAL_PARCEL_DENS(DIMENSION_3)
! parcel count per cell
  INTEGER :: PARCEL_COUNT(DIMENSION_3)


  IF(PIC_CFL/=UNDEFINED) THEN
! CFL Control (if last DT the same, DTSOLID is held)
     IF ((DT.EQ.DT_PREV).AND.(PIC_ITERS.EQ.1)) THEN
!  use the last DTSOLID_PIC_CFL for DTSOLID
!    query condition for first pass and force call to MANAGE_PIC_CFL
        IF ((DTSOLID_PIC_CFL.EQ.ZERO).OR.(DT.EQ.DTSOLID_PIC_CFL)) THEN
           CALL MANAGE_PIC_CFL
        ELSE
           DTSOLID = DTSOLID_PIC_CFL
           CALL bcast(DTSOLID,PE_IO)
        END IF
!   else update DTSOLIDS if DT changes from last time step
     ELSE IF((DT.NE.DT_PREV).AND.(PIC_ITERS.EQ.1)) THEN
!  find a new DTSOLID_PIC_CFL through MANAGE_PIC_CFL
        CALL MANAGE_PIC_CFL
     END IF
  ENDIF


  ! VELFAC is an empirical parameter that damps the slip velocity
  VEL_FAC = MPPIC_VELFAC_COEFF

  ! Begin MP-PIC management

  ! Set limiting volume fraction (slightly less than close pack)
  EPg_MFP = 0.98d0 * (ONE - EP_star)
  ! Initialize maximum allowable parcel velocity to program defined minimum
  MAX_VEL = -UNDEFINED
  ! Create the MP-PIC restitution coefficient used in Snider papers
  ! This will be used to create eta = (1 + e_subp)/2
  ENp1 = MPPIC_COEFF_EN1 + 1.0d0
  ! Create 3 on the square root of 2 for general calculations
  l3SQT2 = 3.0d0 * sqrt(2.0d0)
  ! Create maximum loop count bound based on problem dimension (2D v 3D)
  LC_BND = merge(2,3,NO_K)

  
  ! Calculate a collision time frequency before any looping begins
  IF(PIC_COLLISION_DAMPING) CALL MAKE_TAU_D(AVG_MASS_VEL, ONE_OVER_TAU_D)


  ! Initialize force on individual parcels
  DO NP = 1, MAX_PIP
     IF (IS_NONEXISTENT(NP)) CYCLE !if particle has left domain
     ! Contains fluid pressure grad force
     FC(NP,:) = FC(NP,:) / PMASS(NP) + GRAV(:)

     ! Musser showed drag correlation to MFIX whereby
     ! D_p = Beta/(EP_S*RHOP)  (Snider) <-need
     ! F_gp = Beta*PVOL/EP_S   (MFIX in fgs.f)  <-already have this
     ! D_p = F_gp/(PVOL*RHOP) = F_gp/PMASS  <-so use this for correlation

     IF (DES_CONTINUUM_COUPLED) THEN
        DP_BAR = F_gp(NP) / PMASS(NP)
     ELSE
        DP_BAR = 0.0d0
     ENDIF

     IJK = PIJK(NP,4)

     ! Calculate new solids fraction; threshold eliminiates poss div0
     EPs = MAX(ONE - EP_G(IJK), 1.0d-4)  !threshold is 1e-4

     ! (new)
     MFP = DES_RADIUS(NP)/(l3SQT2 * EPS)   !MFP for comparison later

     ! Calculate velocity devoid of particle normal stress (Snider)
     ! u-tilda sub p = [u sub p bar at n + delta t * Dp * u sub p bar at n+1
     ! - delta t / rho sub p * grad P at n+1 + delta t * g] / [1 + delta t * Dp]
     VEL(:) = (DES_VEL_NEW(NP,:) + FC(NP,:) * DTSOLID) /  &
          (1.0d0 + DP_BAR * DTSOLID)

     ! Estimate a discrete particle velocity from the continuum using
     ! a normal stress gradient (Snider)
     ! del u sub p-tau = - (delta t * grad tau sub p) / (rho sub p
     ! * solid frac * (1 + delta t * Dp)
     ! note that grad tau sub p is calculated in CALC_PS_PIC_SNIDER
     ! and called PS_GRAD below
     IF(ROP_S(IJK,1) > minROPs) then
        DELUP(:) = - (DTSOLID * PS_GRAD(:,NP)) / &
             (ROP_S(IJK,1) * (1.0d0 + DP_BAR * DTSOLID))
     ELSE
        DELUP(:) = 0.0d0
     ENDIF

     ! Calculate the mean free path for the current parcel if the solids
     ! fraction exceeds the threshold value set against close pack state
     MFP_VEL = DES_RADIUS(NP)/(l3SQT2 * EPs)/DTSOLID

     ! Note:  Musser modified SLIPVEL with an empirical velocity factor on the
     ! bulk, i.e. "VEL_FAC *" but should not be necessary
     ! This appears to act as a damping factor for decision tree, sort of like
     ! in Barracuda manual where the "fraction of average velocity" is accounted


     ! Calculate the velocity differential between bulk solid vel and parcel
     ! u bar sub p - u tilda sub p
     slipvel(1) = VEL_FAC*U_s(IJK,1) - VEL(1)
     slipvel(2) = VEL_FAC*V_s(IJK,1) - VEL(2)
     slipvel(3) = VEL_FAC*W_s(IJK,1) - VEL(3)

     ! apply winding scheme to slipvel
     ! Calculate particle velocity from the particle normal stress
     DO LC = 1, LC_BND
        IF (PS_GRAD(LC,NP).LE.ZERO) THEN
           ! Choose between Snider (39) and (40) in 2001 paper
           UPRIMETAU(LC) = min(DELUP(LC), ENp1*SLIPVEL(LC))
           ! Keep value positive or 0
           UPRIMETAU(LC) = max(UPRIMETAU(LC), ZERO)
        ELSE
           ! Choose between Snider (39) and (41) in 2001 paper
           UPRIMETAU(LC) = max(DELUP(LC), ENp1*SLIPVEL(LC))
           ! Keep value negative or 0
           UPRIMETAU(LC) = min(UPRIMETAU(LC), ZERO)
        ENDIF
        ! Update velocity at new time step
        DES_VEL_NEW(NP,LC) = VEL(LC) + UPRIMETAU(LC)

        ! Calculate distance parcel traveled
        DIST(LC) = DES_VEL_NEW(NP,LC) * DTSOLID

!if distance travelled less than local mean free path, particle not moving much
!using this distance catch as a way to capture parcel stray
        IF (DIST(LC).LT.MFP) THEN
           !only look at particles that are in dilute regions
           !close pack is 1-EP_STAR
           !if cell particle fraction is less than 1% of close-pack, we are generally dilute
           IF (EPS.LT.(0.01d0 * (1.0d0 - EP_STAR))) THEN
               !parcel is being pushed toward +x,+y,+z generally
               IF ((PS_GRAD(LC,NP).GE.0.0d0).AND.(GRAV(LC).GT.0.0d0)) THEN
                   !give the parcel an extra push
                   !(1st try) increase effect of solids model when UPRIMETAU non-zero, otherwise use grav
                   IF (UPRIMETAU(LC).NE.0.0d0) THEN
                      DES_VEL_NEW(NP,LC) = VEL(LC) + 2.0d0 * UPRIMETAU(LC)
                   ELSE
                   !(2nd try) give an extra 10% from gravity
                      DES_VEL_NEW(NP,LC) = DES_VEL_NEW(NP,LC) + 0.10d0 * &
                             (GRAV(LC)*DTSOLID/(1.0d0+DP_BAR*DTSOLID))
                   END IF
                   !update distance travelled
                   DIST(LC) = DES_VEL_NEW(NP,LC) * DTSOLID
               !parcel is being pushed toward -x,-y,-z generally
               ELSE IF ((PS_GRAD(LC,NP).LE.0.0d0).AND.(GRAV(LC).LT.0.0d0)) THEN
                   !give the parcel an extra push
                   !(1st try) increase effect of solids model when UPRIMETAU non-zero, otherwise use grav
                   IF (UPRIMETAU(LC).NE.0.0d0) THEN
                      DES_VEL_NEW(NP,LC) = VEL(LC) + 2.0d0 * UPRIMETAU(LC)
                   !(2nd try) give an extra 10% from gravity
                   ELSE
                      DES_VEL_NEW(NP,LC) = DES_VEL_NEW(NP,LC) + 0.10d0 * &
                             (GRAV(LC)*DTSOLID/(1.0d0+DP_BAR*DTSOLID))
                   END IF
                   !update distance travelled
                   DIST(LC) = DES_VEL_NEW(NP,LC) * DTSOLID
               END IF ! end extra push
               !write(*,*) "exiting,  DIST = ", DIST(LC), "DES_VEL_NEW = ", DES_VEL_NEW(NP,LC)
           END IF  !close pack comparison - dilute case


        END IF  !DIST_MAG.LT.MFP

        ! Update parcel position
        DES_POS_NEW(NP,LC) = DES_POS_NEW(NP,LC) + DIST(LC)

     END DO  !LC_BND

! PULL in the effect of collision damping (fully separated)
! This is a direct calculation of equation 13 from Snider
! (current vel+(dt/2taud)*mass avg vel)/(1+(dt/2taud))
     IF (PIC_COLLISION_DAMPING) THEN
        DO LC = 1, LC_BND
           if(ONE_OVER_TAU_D(IJK,LC)/=ZERO) then
            TIME_RATIO(LC) = DTSOLID * 0.5d0 * ONE_OVER_TAU_D(IJK,LC)
            ADJ_VEL(LC) = (DES_VEL_NEW(NP,LC) + &
                          (TIME_RATIO(LC) * AVG_MASS_VEL(IJK,LC))) / &
                          (ONE + TIME_RATIO(LC))

            COLL_DIST(LC) = ADJ_VEL(LC) * DTSOLID

          ! update parcel position
          ! Roll back original position based on first velocity prediction
          ! then correct position based on adjusted velocity from collision 
          ! damping
            DES_POS_NEW(NP,LC) = DES_POS_NEW(NP,LC) - &
                                 DES_VEL_NEW(NP,LC) * DTSOLID + COLL_DIST(LC)
          ! update velocity
            DES_VEL_NEW(NP,LC) = ADJ_VEL(LC)  

           endif
        END DO
     END IF !COLL_DAMP


     ! Clear force calculation
     FC(NP,:) = ZERO

! This is something Garg was doing to calculate a local CFL
! but then he abandoned it
! keeping for now in case I need for something else
! Calculate max distance traveled by any parcel
     DIST_MAG = dot_product(DES_VEL_NEW(NP,:), DES_VEL_NEW(NP,:))
     MAX_VEL = max(MAX_VEL, DIST_MAG)
! I can see how checking distance traveled against a maximum mean
! free path may be useful to keep particles from blowing through a bed

! Residence time
     RESIDENCE_TIME(NP) = RESIDENCE_TIME(NP) + DTSOLID

  END DO  !MAX_PIP


!  if(max_vel == -undefined) then
!     dtsolid_cfl = 1.0
!  else
!     dtsolid_cfl = dxyz_min / max(small_number, abs(max_vel))
!  endif

  return

END SUBROUTINE INTEGRATE_TIME_PIC


!------------------------------------------------------------------------
! subroutine       : des_dbgpic
! Author           : Pradeep G.
! Purpose          : For debugging the pic values
! Parameters       : pstart - start indices of the particle
!                    pend - end indices of the particle
!                    pfreq - optional frequency (when the local count matches the
!                    frequency the filw will be written)
!                    if not send then it prints the file
!------------------------------------------------------------------------

      subroutine des_dbgpic (pstart,pend,pfreq)

!-----------------------------------------------
! Modules
!-----------------------------------------------
      use discretelement
      USE fldvar
      use physprop, only: mmax
      use functions
      implicit none
!-----------------------------------------------
! Dummy arguments
!-----------------------------------------------
      INTEGER, INTENT(IN) :: pstart,pend
      INTEGER, INTENT(IN), OPTIONAL :: pfreq
!-----------------------------------------------
! Local variables
!-----------------------------------------------
      integer lp,lijk
      integer, save :: lfcount = 0 ,lfreq =0
      character(255) :: filename
!-----------------------------------------------
      if (present(pfreq)) then
         lfreq = lfreq+1
         if (lfreq .ne. pfreq) return
         lfreq =0
      end if
      lfcount = lfcount + 1
      write(filename,'("debug",I3.3)') lfcount
      open (unit=100,file=filename)
      do lp = pstart,pend
         if (is_normal(lp) .or. is_entering(lp) .or. is_exiting(lp)) then
            lijk = pijk(lp,4)
            write(100,*)"position =",lijk,pijk(lp,1),pijk(lp,2), &
               pijk(lp,3),ep_g(lijk),U_s(lijk,MMAX+1)
            write(100,*)"forces =", FC(lp,2),tow(lp,1)
         end if
      end do
      close (100)

      RETURN
      END SUBROUTINE des_dbgpic


!------------------------------------------------------------------------
! subroutine       : des_dbgtecplot
! Author           : Pradeep G.
! Purpose          : prints the tecplot file for particle location
! Parameters       : pstart - start indices of the particle
!                    pend - end indices of the particle
!                    pfreq - optional frequency (when the local count matches the
!                    frequency the filw will be written)
!                    if not send then it prints the file
!------------------------------------------------------------------------

      subroutine des_dbgtecplot (pstart,pend,pfreq)

!-----------------------------------------------
! Modules
!-----------------------------------------------
      use discretelement
      USE fldvar
      use functions
      implicit none
!-----------------------------------------------
! Dummy arguments
!-----------------------------------------------
      INTEGER, INTENT(IN) :: pstart,pend
      INTEGER, INTENT(IN), OPTIONAL :: pfreq
!-----------------------------------------------
! Local variables
!-----------------------------------------------
      integer lp,lijk
      integer, save :: lfcount = 0 ,lfreq =0
      character(255) :: filename
!-----------------------------------------------

      if (present(pfreq)) then
         lfreq = lfreq+1
         if (lfreq .ne. pfreq) return
         lfreq =0
      end if
      lfcount = lfcount + 1
      write(filename,'("new_tec",I3.3,".dat")') lfcount
      open (unit=100,file=filename)
      write(100,'(9(A,3X),A)') &
         'VARIABLES = ', '"ijk"', '"x"', '"y"', '"vx"', '"vy"', &
         '"ep_g"', '"FCX"' ,'"FCY"', '"TOW"'
      write(100,'(A,F14.7,A)') 'zone T = "' , s_time , '"'
      do lp = pstart,pend
         if (.not.is_nonexistent(lp)) then
            lijk = pijk(lp,4)
            write(100,*)lijk,des_pos_new(lp,1),des_pos_new(lp,2), &
               des_vel_new(lp,1),des_vel_new(lp,2),ep_g(lijk),&
               fc(lp,1),fc(lp,2),tow(1,lp)
         endif
      enddo
      close (100)
      RETURN
      END SUBROUTINE DES_DBGTECPLOT

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: MANAGE_PIC_CFL                                         !
!                                                                      !
!  Purpose: PIC - Adjust PIC time step based on PIC CFL number         !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE MANAGE_PIC_CFL
  
!     modules
      USE discretelement
      USE error_manager, only: err_msg, log_message, loglevel_info
      USE fldvar
      USE functions
      USE geometry, only:dx,dy,dz
      USE mfix_pic

!     additions for parallel implementation
      USE mpi_utility, only:global_sum,global_max,bcast
      USE compar, only:MYPE,PE_IO
      USE param1, only:zero
      USE run, only: dt

!     move these to the input deck
!      CHARACTER(3)::PIC_CFL_CONTROL='MAX' !other choice is 'AVG'
!     mac: leaving code associated with direction in place but 
!     specifying ALL as only choice to user (no addition to input deck)
      CHARACTER(1)::PIC_CFL_DIRECTION='A' !other choices are 
!                                          'X', 'Z', 'A' (all)
!      DOUBLE PRECISION::PIC_CFL=0.1d0    !user assigned
!      DOUBLE PRECISION::PIC_CFL_PARCEL_FRACTION = 0.01d0 !user assigned

!     global variables used for data reduction (parallel implementation)
      INTEGER::GLOBAL_HIGH_CFL_X_COUNT, GLOBAL_HIGH_CFL_Y_COUNT
      INTEGER::GLOBAL_HIGH_CFL_Z_COUNT, GLOBAL_HIGH_CFL_XYZ_COUNT
      INTEGER::GLOBAL_PARCEL_COUNT
      INTEGER::IERR

      DOUBLE PRECISION::GLOBAL_MAX_PIC_CFL_X, GLOBAL_MAX_PIC_CFL_Y
      DOUBLE PRECISION::GLOBAL_MAX_PIC_CFL_Z, GLOBAL_MAX_PIC_CFL_XYZ
      DOUBLE PRECISION::GLOBAL_SUM_PIC_CFL_X, GLOBAL_SUM_PIC_CFL_Y
      DOUBLE PRECISION::GLOBAL_SUM_PIC_CFL_Z, GLOBAL_SUM_PIC_CFL_XYZ

!     local variables
      INTEGER::NP   !counter over the parcels
      INTEGER::PARCEL_COUNT
      INTEGER::HIGH_CFL_X_COUNT, HIGH_CFL_Y_COUNT, HIGH_CFL_Z_COUNT
      INTEGER::HIGH_CFL_XYZ_COUNT 

!     to easily consolidate routine, many of these variables could
!     be pulled into an array and managed universally.  leaving 
!     independent for easy read during development
      DOUBLE PRECISION::PIC_CFL_X, PIC_CFL_Y, PIC_CFL_Z, PIC_CFL_XYZ
      DOUBLE PRECISION::MAX_PIC_CFL_X, MAX_PIC_CFL_Y, MAX_PIC_CFL_Z
      DOUBLE PRECISION::MAX_PIC_CFL_XYZ
 
      DOUBLE PRECISION::SUM_PIC_CFL_X, SUM_PIC_CFL_Y, SUM_PIC_CFL_Z 
      DOUBLE PRECISION::SUM_PIC_CFL_XYZ

      DOUBLE PRECISION::AVG_PIC_CFL_X, AVG_PIC_CFL_Y, AVG_PIC_CFL_Z
      DOUBLE PRECISION::AVG_PIC_CFL_XYZ

      DOUBLE PRECISION::OUT_OF_SPEC_X, OUT_OF_SPEC_Y, OUT_OF_SPEC_Z
      DOUBLE PRECISION::OUT_OF_SPEC_XYZ

!     -------------------------------------------------------  

!     clear local variables
      MAX_PIC_CFL_X   = ZERO
      MAX_PIC_CFL_Y   = ZERO
      MAX_PIC_CFL_Z   = ZERO
      MAX_PIC_CFL_XYZ = ZERO

      SUM_PIC_CFL_X   = ZERO
      SUM_PIC_CFL_Y   = ZERO
      SUM_PIC_CFL_Z   = ZERO
      SUM_PIC_CFL_XYZ = ZERO

      AVG_PIC_CFL_X   = ZERO
      AVG_PIC_CFL_Y   = ZERO
      AVG_PIC_CFL_Z   = ZERO
      AVG_PIC_CFL_XYZ = ZERO

      OUT_OF_SPEC_X   = ZERO
      OUT_OF_SPEC_Y   = ZERO
      OUT_OF_SPEC_Z   = ZERO
      OUT_OF_SPEC_XYZ = ZERO

      PARCEL_COUNT       = 0
      HIGH_CFL_X_COUNT   = 0
      HIGH_CFL_Y_COUNT   = 0
      HIGH_CFL_Z_COUNT   = 0
      HIGH_CFL_XYZ_COUNT = 0

! JFD:
! When enterin this subroutine, DTSOLID is set to the fluid DT
! and DTSOLID_PIC_CFL is set to the previous time step used for PIC
! (regular PIC sub iterations, not the last one which  uses a truncated
! time step).
! Here the PIC CFL is computed based on DTSOLID_PIC_CFL, so that once the
! PIC time step has been adjusted once, the computed CFL should not
! significantly vary.  

      IF(DTSOLID_PIC_CFL==ZERO) DTSOLID_PIC_CFL = DTSOLID


!     loop over parcel count
      DO NP = 1, MAX_PIP

         IF (IS_NONEXISTENT(NP)) CYCLE
!        clear last instance of loop variables
         PIC_CFL_X   = ZERO
         PIC_CFL_Y   = ZERO
         PIC_CFL_Z   = ZERO
         PIC_CFL_XYZ = ZERO
         
!        calculate CFL by abs(vel) for each parcel by direction
!        Using current DTSOLID_PIC_CFL time step
         PIC_CFL_X = ABS(DES_VEL_NEW(NP,1))*DTSOLID_PIC_CFL/DX(PIJK(NP,1))
         PIC_CFL_Y = ABS(DES_VEL_NEW(NP,2))*DTSOLID_PIC_CFL/DY(PIJK(NP,2))
         PIC_CFL_Z = ABS(DES_VEL_NEW(NP,3))*DTSOLID_PIC_CFL/DZ(PIJK(NP,3))
         PIC_CFL_XYZ = MAX(PIC_CFL_X,PIC_CFL_Y,PIC_CFL_Z)
 
         MAX_PIC_CFL_X = MAX(MAX_PIC_CFL_X,PIC_CFL_X)
         MAX_PIC_CFL_Y = MAX(MAX_PIC_CFL_Y,PIC_CFL_Y)
         MAX_PIC_CFL_Z = MAX(MAX_PIC_CFL_Z,PIC_CFL_Z)
         MAX_PIC_CFL_XYZ = MAX(MAX_PIC_CFL_XYZ,PIC_CFL_XYZ)

            IF(PIC_CFL_X.GT.PIC_CFL)THEN
                 HIGH_CFL_X_COUNT = HIGH_CFL_X_COUNT + 1
            END IF

            IF(PIC_CFL_Y.GT.PIC_CFL)THEN
                 HIGH_CFL_Y_COUNT = HIGH_CFL_Y_COUNT + 1
            END IF

            IF(PIC_CFL_Z.GT.PIC_CFL)THEN
                 HIGH_CFL_Z_COUNT = HIGH_CFL_Z_COUNT + 1
            END IF

         HIGH_CFL_XYZ_COUNT = HIGH_CFL_X_COUNT + HIGH_CFL_Y_COUNT + &
                              HIGH_CFL_Z_COUNT
         
!        create component for an average CFL by direction and total vel
         SUM_PIC_CFL_X = SUM_PIC_CFL_X + PIC_CFL_X
         SUM_PIC_CFL_Y = SUM_PIC_CFL_Y + PIC_CFL_Y
         SUM_PIC_CFL_Z = SUM_PIC_CFL_Z + PIC_CFL_Z
         SUM_PIC_CFL_XYZ = SUM_PIC_CFL_XYZ + PIC_CFL_XYZ
      
         PARCEL_COUNT = PARCEL_COUNT+1  

      END DO !NP

! To grab the true max, run through the global call
      CALL GLOBAL_MAX(MAX_PIC_CFL_X,GLOBAL_MAX_PIC_CFL_X,PE_IO,IERR)
      CALL GLOBAL_MAX(MAX_PIC_CFL_Y,GLOBAL_MAX_PIC_CFL_Y,PE_IO,IERR)
      CALL GLOBAL_MAX(MAX_PIC_CFL_Z,GLOBAL_MAX_PIC_CFL_Z,PE_IO,IERR)

! to grab the true sum, run through the global call
      CALL GLOBAL_SUM(HIGH_CFL_X_COUNT,GLOBAL_HIGH_CFL_X_COUNT,PE_IO,IERR)
      CALL GLOBAL_SUM(HIGH_CFL_Y_COUNT,GLOBAL_HIGH_CFL_Y_COUNT,PE_IO,IERR)
      CALL GLOBAL_SUM(HIGH_CFL_Z_COUNT,GLOBAL_HIGH_CFL_Z_COUNT,PE_IO,IERR)

      CALL GLOBAL_SUM(SUM_PIC_CFL_X,GLOBAL_SUM_PIC_CFL_X,PE_IO,IERR)
      CALL GLOBAL_SUM(SUM_PIC_CFL_Y,GLOBAL_SUM_PIC_CFL_Y,PE_IO,IERR)
      CALL GLOBAL_SUM(SUM_PIC_CFL_Z,GLOBAL_SUM_PIC_CFL_Z,PE_IO,IERR)
      CALL GLOBAL_SUM(SUM_PIC_CFL_XYZ,GLOBAL_SUM_PIC_CFL_XYZ,PE_IO,IERR)

! create global parcel count
      CALL GLOBAL_SUM(PARCEL_COUNT,GLOBAL_PARCEL_COUNT,PE_IO,IERR)
      

      IF(MyPE.EQ.PE_IO) THEN

        GLOBAL_MAX_PIC_CFL_XYZ = MAX(GLOBAL_MAX_PIC_CFL_X, &
                   GLOBAL_MAX_PIC_CFL_Y, GLOBAL_MAX_PIC_CFL_Z)
 

        GLOBAL_HIGH_CFL_XYZ_COUNT = MIN(GLOBAL_HIGH_CFL_X_COUNT+ &
                   GLOBAL_HIGH_CFL_Y_COUNT + GLOBAL_HIGH_CFL_Z_COUNT, &
                   GLOBAL_PARCEL_COUNT)

        IF (GLOBAL_PARCEL_COUNT.GT.0)THEN
!       create average CFLs
           AVG_PIC_CFL_X = GLOBAL_SUM_PIC_CFL_X/GLOBAL_PARCEL_COUNT
           AVG_PIC_CFL_Y = GLOBAL_SUM_PIC_CFL_Y/GLOBAL_PARCEL_COUNT
           AVG_PIC_CFL_Z = GLOBAL_SUM_PIC_CFL_Z/GLOBAL_PARCEL_COUNT
           AVG_PIC_CFL_XYZ = GLOBAL_SUM_PIC_CFL_XYZ/GLOBAL_PARCEL_COUNT
      
!       calculate percent out of spec
           OUT_OF_SPEC_X = REAL(GLOBAL_HIGH_CFL_X_COUNT)/REAL(GLOBAL_PARCEL_COUNT)
           OUT_OF_SPEC_Y = REAL(GLOBAL_HIGH_CFL_Y_COUNT)/REAL(GLOBAL_PARCEL_COUNT)
           OUT_OF_SPEC_Z = REAL(GLOBAL_HIGH_CFL_Z_COUNT)/REAL(GLOBAL_PARCEL_COUNT)
           OUT_OF_SPEC_XYZ = REAL(GLOBAL_HIGH_CFL_XYZ_COUNT)/REAL(GLOBAL_PARCEL_COUNT)
        END IF
       
!     choose the CFL that will control the simulation time-step
!     and back calculate

      DTSOLID = DTSOLID_PIC_CFL

      SELECT CASE (PIC_CFL_CONTROL)

         CASE ('MAX')

            SELECT CASE (PIC_CFL_DIRECTION)

               CASE ('X')
                 IF(OUT_OF_SPEC_X.GT.PIC_CFL_PARCEL_FRACTION)THEN
                   IF(GLOBAL_MAX_PIC_CFL_X.GT.ZERO)THEN
                     DTSOLID_PIC_CFL=DTSOLID/GLOBAL_MAX_PIC_CFL_X*PIC_CFL
                   END IF
                 END IF
               CASE ('Y')
                 IF(OUT_OF_SPEC_Y.GT.PIC_CFL_PARCEL_FRACTION)THEN
                   IF(GLOBAL_MAX_PIC_CFL_Y.GT.ZERO)THEN
                     DTSOLID_PIC_CFL=DTSOLID/GLOBAL_MAX_PIC_CFL_Y*PIC_CFL
                   END IF      
                 END IF
               CASE ('Z')
                 IF(OUT_OF_SPEC_Z.GT.PIC_CFL_PARCEL_FRACTION)THEN
                   IF(GLOBAL_MAX_PIC_CFL_Z.GT.ZERO)THEN
                     DTSOLID_PIC_CFL=DTSOLID/GLOBAL_MAX_PIC_CFL_Z*PIC_CFL
                   END IF
                 END IF
               CASE ('A')
!      Allow DTSOLID_PIC_CFL to decrease or increase (up to fluid DT) to match
!      user-defined pic cfl.
                 IF(OUT_OF_SPEC_XYZ.GT.PIC_CFL_PARCEL_FRACTION.OR.OUT_OF_SPEC_XYZ==ZERO)THEN
                   IF(GLOBAL_MAX_PIC_CFL_XYZ.GT.ZERO)THEN
                     DTSOLID_PIC_CFL=DMIN1(DTSOLID/GLOBAL_MAX_PIC_CFL_XYZ*PIC_CFL,DT)
                   END IF
                 END IF
               CASE DEFAULT
                   DTSOLID_PIC_CFL = DTSOLID
            END SELECT

         CASE ('AVG')

            SELECT CASE (PIC_CFL_DIRECTION)
          
               CASE ('X')
                 IF(OUT_OF_SPEC_X.GT.PIC_CFL_PARCEL_FRACTION)THEN
                    DTSOLID_PIC_CFL=DTSOLID/AVG_PIC_CFL_X*PIC_CFL
                 END IF
               CASE ('Y')
                 IF(OUT_OF_SPEC_Y.GT.PIC_CFL_PARCEL_FRACTION)THEN
                    DTSOLID_PIC_CFL=DTSOLID/AVG_PIC_CFL_Y*PIC_CFL
                 END IF
               CASE ('Z')
                 IF(OUT_OF_SPEC_Z.GT.PIC_CFL_PARCEL_FRACTION)THEN
                    DTSOLID_PIC_CFL=DTSOLID/AVG_PIC_CFL_Z*PIC_CFL
                 END IF
               CASE ('A')
                 IF(OUT_OF_SPEC_XYZ.GT.PIC_CFL_PARCEL_FRACTION.OR.OUT_OF_SPEC_XYZ==ZERO)THEN
                   IF(AVG_PIC_CFL_XYZ.GT.ZERO)THEN
                     DTSOLID_PIC_CFL=DMIN1(DTSOLID/AVG_PIC_CFL_XYZ*PIC_CFL,DT)
                   END IF
                 END IF
               CASE DEFAULT
                   DTSOLID_PIC_CFL = DTSOLID
            END SELECT

         CASE DEFAULT
            DTSOLID_PIC_CFL = DTSOLID

      END SELECT
      
         DTSOLID = DTSOLID_PIC_CFL
      

      END IF ! MyPE.EQ.PE_IO

!     Broadcast new DTSOLID to all processors
      CALL bcast(DTSOLID,PE_IO)   
      CALL bcast(DTSOLID_PIC_CFL, PE_IO)

      WRITE(ERR_MSG, 1000) PIC_CFL, GLOBAL_MAX_PIC_CFL_XYZ , AVG_PIC_CFL_XYZ, &
      OUT_OF_SPEC_XYZ*100.0D0,DTSOLID_PIC_CFL
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)


1000  FORMAT(1x,'Adjusting PIC time step based on PIC CFL =',F8.3, &
      /1x,'Max CFL = ',F8.3,'; Avg CFL = ', F8.3, '; Out of specs =', F8.3, ' %', &
      /1x,'Adjusted PIC time step = ',E8.3, ' sec.' )
      RETURN
      END SUBROUTINE MANAGE_PIC_CFL

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: MAKE_TAU_D                                             !
!                                                                      !
!  M.A. Clarke;  Nov 2020                                              !
!                                                                      !
!  Purpose: Manage collision damping time scale factor (tau_d)         !
!  as described in:                                                    !
!  An improved collision damping time for MP-PIC calculations of dense !
!  particle flows with applications to polydisperse sedimenting bed    !
!  and colliding particle jets,                                        !
!  Chemical Engineering Science, Vol 65 (2010), pp. 6014-28            !
!                                                                      !
!  I want to make what is described as equation 48                     !
!  (1/tau_d) = (16/sqrt(3pi)) * (theta*sigma/r32) * (g0*eta*(1-eta))   !
!                                                                      !
!  theta = solids volume fraction                                      !
!  sigma = standard deviation of mass-averaged velocity                !
!  r32 = local Sauter radius (by cell)                                 !
!  g0 = close pack vol fraction                                        !
!       / (close pack vol fraction - solids vol fraction)              !
!  eta = (1 + coeff of restitution)/2                                  !
!                                                                      !
!  After I create 1/tau_d, I want to apply the term through a          !
!  correction value for velocity inside integrate_time_pic.            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
!        1         2         3         4         5         6         7         8

   SUBROUTINE MAKE_TAU_D(AVG_MASS_VEL,ONE_OVER_TAU_D)

!  Set up module use
! IJK bounds
   USE compar, only: ijkstart3, ijkend3
!  Defined general problem dimensions
   USE param
!  Pre defines word numbers like ONE, HALF, etc.
   USE param1
!  For parallel processing (once Jordan gets hold of this)
   USE parallel
!  Physical property data
   USE physprop
!  Constant valued model parameters
   USE constant
!  The field variables assigned to each cell
   USE fldvar, only: ep_g, u_s, v_s, w_s, rop_s
!  The values surrounding particle management (like max-pip)
   USE discretelement
!  MP-PIC variables
   USE mfix_pic
!  variables related to grid geometry
   USE geometry
!  more variables related to grid geometry
   USE functions

   IMPLICIT NONE

!  average mass velocity by cell
   DOUBLE PRECISION, intent(out) :: AVG_MASS_VEL(DIMENSION_3,3)
!  collision damping time frequency
   DOUBLE PRECISION, intent(out) :: ONE_OVER_TAU_D(DIMENSION_3,3)

!  all the local variables

!  carries the sum of the mass velocity of all parcels in each cell
   DOUBLE PRECISION:: SUM_MASS_VEL(DIMENSION_3,3)
!  carries the sum of the solids density of all parcels in each cell
   DOUBLE PRECISION:: SUM_LOCAL_PARCEL_DENS(DIMENSION_3)
!  carries the number of parcels in each cell
   INTEGER:: COUNT_PARCEL(DIMENSION_3)
!  carries the sum associated with the numerator of std dev for mass velocity by cell
   DOUBLE PRECISION:: SUM_STD_DEV(DIMENSION_3,3)
!  arithmetic factors used in 1/tau_d calculation
   DOUBLE PRECISION:: ARITHFAC, ARITHFAC2
!  std deviation (temporary)
   DOUBLE PRECISION:: SIGMA(3)
!  radial function
   DOUBLE PRECISION:: G_0
!  sum of cubes (volumes) to calculate Sauter radius
   DOUBLE PRECISION:: SUM_LOCAL_VOL(DIMENSION_3)
!  sum of squares (surface area) to calculate Sauter radius
   DOUBLE PRECISION:: SUM_LOCAL_SA(DIMENSION_3)
!  Sauter radius
   DOUBLE PRECISION:: SAUTER_RAD
!  local numbering
   INTEGER:: IJK, NP
! one third   
   DOUBLE PRECISION, PARAMETER  :: THIRD = (1.0d0/3.0d0)

!  initialize every time step change to prep for recalculation
   SUM_MASS_VEL(:,:)        = ZERO
   SUM_LOCAL_PARCEL_DENS(:) = ZERO
   COUNT_PARCEL(:)          = ZERO
   SUM_LOCAL_VOL(:)         = ZERO
   SUM_LOCAL_SA(:)          = ZERO
   SUM_STD_DEV(:,:)         = ZERO
   ONE_OVER_TAU_D(:,:)      = ZERO
   AVG_MASS_VEL(:,:)        = ZERO

!  if we want to really streamline code
!  this logic can be carefully inserted into existing NP loop in INTEGRATE routine
!  first, create pieces of parcel mass-averaged velocity for each cell
!  simultaneously create pieces of sauter mean radius by cell
   DO NP = 1, MAX_PIP
      IF (IS_NONEXISTENT(NP)) CYCLE !if particle has left domain
!  pull off the location of each parcel
      IJK = PIJK(NP,4)
!  Create pieces needed for mass-averaged velocity
!  allot the velocity components of that parcel to local sums around the grid
      SUM_MASS_VEL(IJK,:) = SUM_MASS_VEL(IJK,:) + &
                            PMASS(NP)/PVOL(NP)*DES_VEL_NEW(NP,:)
!  account sum of local parcel density
      SUM_LOCAL_PARCEL_DENS(IJK) = SUM_LOCAL_PARCEL_DENS(IJK) + &
                                   PMASS(NP)/PVOL(NP)

!  Create pieces needed for variable Sauter radius
!  do not account constants;  apply at end;  saves flops
!  account local volume of parcels (sum of cubes)
      SUM_LOCAL_VOL(IJK) = SUM_LOCAL_VOL(IJK) + DES_RADIUS(NP)**3
!  account local surface area of parcels (sum of squares)
      SUM_LOCAL_SA(IJK) = SUM_LOCAL_SA(IJK) + DES_RADIUS(NP) * DES_RADIUS(NP)

!  Create count needed in mean and standard deviation later on
!  count how many parcels are in each cell
      COUNT_PARCEL(IJK) = COUNT_PARCEL(IJK) + 1
   END DO !NP

!  create standard deviation of parcel velocity based on mass-averaged velocity
   DO IJK = IJKSTART3, IJKEND3
!  calculate mean parcel mass-averaged velocity for each cell
      IF(COUNT_PARCEL(IJK)>0) THEN
         AVG_MASS_VEL(IJK,:) = SUM_MASS_VEL(IJK,:) / SUM_LOCAL_PARCEL_DENS(IJK)
      END IF

   END DO !IJK

   DO NP = 1, MAX_PIP
      IF (IS_NONEXISTENT(NP)) CYCLE !if particle has left domain
      IJK = PIJK(NP,4)
!  create the sum needed for standard deviation based on mass-averaged velocity
!  for each cell in each direction

      SUM_STD_DEV(IJK,:) = SUM_STD_DEV(IJK,:) + &
                          (PMASS(NP)/PVOL(NP))/SUM_LOCAL_PARCEL_DENS(IJK) * &
                          (DES_VEL_NEW(NP,:)-AVG_MASS_VEL(IJK,:))**2 
   END DO !IJK

!  The standard deviation for each cell is now ready, in pieces as
!  SQRT(SUM_STD_DEV(IJK,:)/(COUNT_PARCEL(IJK)-1)
!  but I don't need to calculate it until I need it (will avoid full loop over IJK)
!  The sauter mean radius for each cell is also now ready, in pieces as
!  1/3 * (SUM_LOCAL_VOL(IJK)/SUM_LOCAL_SA(IJK))

!  It may be flop cheaper to create the radial function, std deviation and sauter
!  radius by cell, not by parcel.  But, we will have to populate temporary arrays
!  that carry the whole IJK.  

!  create tau_d arithmetic factor 16/sqrt(3pi)
   ARITHFAC = 16.0d0/sqrt(3.0d0 * PI)
!  create tau_d arithmetic factor eta(1-eta) eqn 33/48
   ARITHFAC2 = ((ONE+MPPIC_COEFF_EN1)*0.5)*(ONE-(ONE+MPPIC_COEFF_EN1)*0.5)

!  pull everything together to create collision damping time factor
   DO NP = 1, MAX_PIP
      IF (IS_NONEXISTENT(NP)) CYCLE !if particle has left domain
!  pull off the location of each parcel
      IJK = PIJK(NP,4)
!  clear radial function, standard deviation, sauter rad
      G_0 = ZERO
      SIGMA(:) = ZERO
      SAUTER_RAD = ZERO
!  piece together 1/tau_d
!  If there is only one parcel in the cell, ONE_OVER_TAU_D(IJK,:) is set to zero
      IF(COUNT_PARCEL(IJK)>1) THEN
!  create radial function for this parcel
!         solids fraction based (as eps->close pack, G_0->inf)
!         (as eps->0, G_0->1)
!         if we get a numerical overpack, G_0 goes negative (not good)
!         ONE is a reset to a fluid-full cell to reduce effect
          G_0 = max((ONE - EP_star) / ((ONE - EP_star) - (ONE - EP_g(IJK))),ONE)
!  create standard deviation function for this parcel in 3 directions
         SIGMA(:) = sqrt(SUM_STD_DEV(IJK,:))/(COUNT_PARCEL(IJK)-1)
!  create sauter radius for this parcel location
         SAUTER_RAD = THIRD * SUM_LOCAL_VOL(IJK)/SUM_LOCAL_SA(IJK)
!  create collision damping time factor for this parcel location
         ONE_OVER_TAU_D(IJK,:) = ARITHFAC *((ONE - EP_G(IJK))*SIGMA(:)/SAUTER_RAD) * &
                                 G_0 * ARITHFAC2
      ENDIF ! COUNT_PARCEL(IJK)>1

   END DO !NP

   RETURN

   END SUBROUTINE MAKE_TAU_D

