#include "error.inc"

MODULE SET_RO_s_MOD

   USE error_manager

CONTAINS



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: SET_RO_s                                                !
!                                                                      !
!  Author: J.Musser                                   Date: 09-Oct-13  !
!  Reviewer:                                                           !
!                                                                      !
!  Purpose: Initialize solids densities.                               !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE SET_RO_S

! Global Variables:
!---------------------------------------------------------------------
! Number of solids phases.
      use physprop, only: MMAX
      use discretelement, only: DES_MMAX
! Solids density field variable.
      use fldvar, only: RO_s, ROP_s
! Solid phase species mass fractions.
      use fldvar, only: X_s
! Initial mass fraction of inert species
      use physprop, only: X_S0
! Index of inert solids phase species.
      use physprop, only: INERT_SPECIES
! Inert solids phase species mass fraction in dilute region.
      use physprop, only: DIL_INERT_X_VSD
! Factor to define dilute region where DIL_INERT_X_VSD is used
      use physprop, only: DIL_FACTOR_VSD
! Run-time flag for variable soilds density
      use run, only: SOLVE_ROs
! Constant solids density.
      use physprop, only: RO_s0
! Minimum solids volume fraction
      use toleranc, only: DIL_EP_s

! Function for evaluating solids density.
      use eos, only: EOSS

      use usr_prop, only: usr_ros
! Modules needed to support function.inc
      use compar
      use geometry
      use indices
      use functions

      implicit none

! Local Variables
!---------------------------------------------------------------------
! Solids phase index
      INTEGER :: M
! Fluid cell index
      INTEGER :: IJK
! Index of the inert solids species.
      INTEGER :: IIS
! Flag for debugging.
      LOGICAL, parameter :: dbgMode = .FALSE.
      DOUBLE PRECISION :: minROPs
!---------------------------------------------------------------------

! Loop over all solids (I wonder if this shouldn't be smax)
      DO M=1,MMAX

! Variable solids density.
         IF (SOLVE_ROs(M)) THEN
! Set the index of the intert phase.
            IIS = INERT_SPECIES(M)
! Calculate the minimum solids density.
!            minROPs = RO_s0(M)*DIL_EP_s
            minROPs = RO_s0(M)*(DIL_FACTOR_VSD*DIL_EP_s)
! Debug/Development option.
            IF(dbgMode) CALL CHECK_SET_ROs()

! Calculate Ro_s in all fluid and flow boundary cells.
            DO IJK = ijkStart3, ijkEnd3
               IF(WALL_AT(IJK)) CYCLE
               IF(ROP_s(IJK,M) > minROPs) THEN
                  RO_S(IJK,M) = EOSS(RO_s0(M), X_s0(M,IIS),         &
                     X_s(IJK,M,IIS))
               ELSE
!                  RO_s(IJK,M) = RO_s0(M)
                  RO_S(IJK,M) = EOSS(RO_s0(M), X_s0(M,IIS),            &
                     DIL_INERT_X_VSD(M))
               ENDIF
            ENDDO
         ELSEIF (USR_ROs(M)) THEN
            DO IJK=IJKSTART3,IJKEND3
               IF (WALL_AT(IJK)) CYCLE
               CALL USR_PROP_ROS(IJK,M)
            ENDDO
         ELSE
! Constant solids density.
            DO IJK = ijkstart3, ijkend3
               IF (WALL_AT(IJK)) CYCLE
               RO_S(IJK,M) = RO_S0(M)
            ENDDO
         ENDIF
      ENDDO

      CALL BC_ROS_CORRECTION


! Set a phase density for each cell. The value isn't overly important,
! but it is needed to use the EP_s() function.
      DO M=MMAX+1, DES_MMAX+MMAX
         IF (SOLVE_ROs(M)) THEN
            DO IJK = ijkStart3, ijkEnd3
               IF(.NOT.WALL_AT(IJK)) RO_S(IJK,M) = RO_s0(M)
            ENDDO
         ENDIF
      ENDDO

      RETURN

      CONTAINS



!``````````````````````````````````````````````````````````````````````!
!  Subroutine: CHECK_SET_ROs                                           !
!  Author: J.Musser                                   Date:            !
!                                                                      !
!  Purpose: Verify that all the variable solids density information is !
!           present for solids phase M.                                !
!                                                                      !
!           Note: The check_data routines should have caught any       !
!           problematic IC/BC specifications. This is included mainly  !
!           for development efforts.                                   !
!``````````````````````````````````````````````````````````````````````!
      SUBROUTINE CHECK_SET_ROs()

! Global variables
!---------------------------------------------------------------------
      use fldvar, only: X_s
      use param1, only: zero
      use physprop, only: NMAX
      use physprop, only: INERT_SPECIES
      use toleranc
      implicit none

! Local variables
!---------------------------------------------------------------------
! Sum of solids phase mass fractions.
      DOUBLE PRECISION :: SUM_Xs
! Index of inert solids phase species.
      INTEGER :: INERT
! Integer Error Flag.
      INTEGER :: IER(2)
! Error file log.
      INTEGER, parameter :: lUnit = 8454
      LOGICAL :: lExists
      CHARACTER(LEN=64) :: lFName
!---------------------------------------------------------------------

! Initialize error flags.
      IER = 0

! Set the inert species index.
      INERT = INERT_SPECIES(M)

! Check all computational cells.
      DO IJK = ijkStart3, ijkEnd3
! Skip walls.
         IF (WALL_AT(IJK)) CYCLE
! Calculate the solids species mass fraction sum.
         SUM_Xs = sum(X_s(IJK,M,:NMAX(M)))
! Verify that the species mass fractions are specified and valid.
         IF(.NOT.compare(ONE,SUM_Xs)) IER(1) = IER(1)+1
! Verify that the inert species mass fraction is greater than zero.
         IF(X_s(IJK,M,INERT) <= ZERO) IER(2) = IER(2)+1

      ENDDO

! An error was detected. Open a log file.
      IF(sum(IER) /= 0) THEN
         lFName=''
         IF(numPEs == 1) THEN
            WRITE(lFName,"('setROs.log')")
         ELSE
            WRITE(lFName,"('setROs_',I6.6,'.log')") myPE
         ENDIF
         inquire(file=trim(lFName),exist=lExists)
         IF(lExists) THEN
            OPEN(unit=lUnit,file=trim(lFName),status='replace')
         ELSE
            OPEN(unit=lUnit,file=trim(lFName),status='new')
         ENDIF
      ENDIF


! An error was detected in species mass fraction sum.
      IF(IER(1) /= 0)THEN
         WRITE(lUnit,1100) myPE
! Skip walls.
         DO IJK = ijkStart3, ijkEnd3
            IF (WALL_AT(IJK)) CYCLE
! Calculate the solids species mass fraction sum.
            SUM_Xs = sum(X_s(IJK,M,:NMAX(M)))
! Verify that the species mass fractions are specified and valid.
            IF(.NOT.compare(ONE,SUM_Xs)) WRITE(lUnit,1101) IJK, SUM_Xs
         ENDDO
         WRITE(lUnit,9999)
      ENDIF

! An error was detected in inert species mass fraction.
      IF(IER(2) /= 0)THEN
         WRITE(lUnit,1200) myPE
         WRITE(lUnit,1201) M
         WRITE(lUnit,1202) INERT
! Skip walls.
         DO IJK = ijkStart3, ijkEnd3
            IF (WALL_AT(IJK)) CYCLE
! Calculate the solids species mass fraction sum.
! Verify that the species mass fractions are specified and valid.
            IF(X_s(IJK,M,INERT) <= ZERO) WRITE(lUnit,1203)             &
               IJK, X_s(IJK,M,INERT)
         ENDDO
         WRITE(lUnit,9999)
      ENDIF

! Close the file, cleanup, and exit.
      IF(sum(IER) /= 0) THEN
         CLOSE(lUnit)
         call log_error()
      ENDIF

      RETURN

 1100 FORMAT(//1X,/' From: CHECK_SET_ROs',/,' Error 1100:',     &
         ' One or more fluid cells contain invalid species mass',/     &
         ' fractions which do NOT sum to one.'/,'   > myPE = ',I6)

 1101 FORMAT('   > sum(X_s(',I6,')) = ',g12.5)

 1200 FORMAT(//1X,/' From: CHECK_SET_ROs',/,' Error 1200:',     &
         ' One or more fluid cells contain an invalid species mass',/  &
         ' fraction for the inert material.'/,'   > myPE = ',I6)

 1201 FORMAT('   > Solid Phase: ',I2)

 1202 FORMAT('   > Inert species index: ',I4)

 1203 FORMAT('   > X_s(',I6,',INERT) = ',g12.5)

 9999 FORMAT(1x,/)

      END SUBROUTINE CHECK_SET_ROs

   END SUBROUTINE SET_RO_S

!``````````````````````````````````````````````````````````````````````!
!  Subroutine: BC_ROS_CORRECTION                                       !
!  Author: J.CARNEY                                                    !
!                                                                      !
!  Master routine to correct any inconsistencies that may become       !
!  introduced at certain boundaries that do not require bc_rop_s (or   !
!  bc_ep_g) but are assigned a value based on their neighbor fluid     !
!  cell in order to prevent errors in later check routines. Issues     !
!  arise when the density at the boundary no longer matches the        !
!  interior.                                                           !
!                                                                      !
!``````````````````````````````````````````````````````````````````````!

      SUBROUTINE BC_ROS_CORRECTION
! Global Variables:
!---------------------------------------------------------------------
      use physprop, only: MMAX
      use run, only: SOLVE_ROs
      use usr_prop, only: usr_ros
      IMPLICIT NONE
! Local Variables
!---------------------------------------------------------------------
! Solids phase index
      INTEGER :: M
!---------------------------------------------------------------------

! Loop over all solids
      DO M=1,MMAX

! Variable solids density.
         IF (SOLVE_ROs(M)) THEN
! Correct volume fraction mismatch that is introduced in using the
! neighbor fluid cell value to initially assign any undefined bc_rop_s
! =>rop_s in the flow boundary; here ep_s becomes inconsistent with ep_g
! if the boundary ro_s does not also match the interior fluid ro_s.

         ELSEIF (USR_ROs(M)) THEN
! Correct volume fraction to user specification that was introduced in
! using an arbitrary ic_ros.
            CALL BC_USR_ROS_CORRECTION(M)
         ENDIF
      ENDDO
      RETURN
      END SUBROUTINE BC_ROS_CORRECTION


!``````````````````````````````````````````````````````````````````````!
!  Subroutine: BC_USR_ROS_CORRECTION                                   !
!  Author: J.CARNEY                                                    !
!                                                                      !
!  Purpose:  In the case of variable density, it is more rational      !
!  for the user to specify ep_s (ic_ep_s/bc_ep_s) as opposed to rop_s. !
!  since rop_s is itself derived from ep_s and ro_s and ro_s itself    !
!  is not yet calculated. Unfortunately, MFIX does not store ep_s      !
!  but makes it a function of rop_s at any time.                       !
!                                                                      !
!  As a result, imposing the user specified ic/bc_ep_s on the field    !
!  ep_s is not actually possible (we only store rop_s). So we must     !
!  instead assign a dummy rop_s through a dummy ro_s as ro_s itself    !
!  it not yet known. The call to usr_ro_s only becomes practical once  !
!  the scalar field variables are defined throughout the domain.       !
!                                                                      !
!  These subtleties come to head during the very fist call to usr_ros  !
!  (from set_ro_s). In particular, the routine calls upon ep_s         !
!  (therein rop_s) which as discussed above is not well defined during !
!  the initialization. So, to avoid the problem we allow rop_s to be   !
!  set throughout the domain based on the ic_ep_s and an arbitrary     !
!  ic_ros. The existing rop_s can now be overwritten based on given    !
!  ic_ep_s and the now updated ro_s.                                   !
!                                                                      !
!  A similar problem arises within certain BC cells; generally         !
!  outflow boundaries simply reflect their neighbor fluid cell value,  !
!  however, MFIX also allows user to specify bc_rop_s/bc_ep_s at the   !
!  boundary. So the flow boundaries are also addressed here.           !
!                                                                      !
!                                                                      !
!``````````````````````````````````````````````````````````````````````!
      SUBROUTINE BC_USR_ROS_CORRECTION(M)

! Global variables
!---------------------------------------------------------------------
      use ic, only: ic_defined, ic_ep_s
      use ic, only: ic_i_e, ic_i_w, ic_j_n, ic_j_s, ic_k_t, ic_k_b

      use bc, only: bc_defined, bc_ep_s
      use bc, only: bc_i_e, bc_i_w, bc_j_n, bc_j_s, bc_k_t, bc_k_b

      use fldvar, only: RO_s, ROP_s

      use param1, only: undefined
      use param, only: dimension_ic, dimension_bc

      use functions, only: is_on_mype_plus2layers
      use functions, only: funijk
      use compar, only: dead_cell_at

     IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------
! solids phase index
      INTEGER, INTENT(IN) :: M

! Local variables
!---------------------------------------------------------------------
! Loop counters
      INTEGER :: ICV, BCV
      INTEGER :: I, J, K, IJK
!......................................................................

      DO ICV = 1, DIMENSION_IC
         IF (IC_DEFINED(ICV)) THEN
            DO K = IC_K_B(ICV), IC_K_T(ICV)
            DO J = IC_J_S(ICV), IC_J_N(ICV)
            DO I = IC_I_W(ICV), IC_I_E(ICV)
               IF (.NOT.IS_ON_myPE_plus2layers(I,J,K)) CYCLE
               IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
               IJK = FUNIJK(I,J,K)

! This check is probably unnecessary as ic_ep_s must be specified
! when using usr_ros. The calculation corrects the arbitrary value of
! ic_ros, which together which ic_ep_s was used to define an ic_rop_s
               IF (IC_EP_S(ICV,M) /= UNDEFINED) THEN
                  ROP_S(IJK,M) = IC_EP_S(ICV,M)*RO_S(IJK,M)
               ENDIF
            ENDDO
            ENDDO
            ENDDO
         ENDIF
      ENDDO

      DO BCV = 1, DIMENSION_BC
         IF (BC_DEFINED(BCV)) THEN
            DO K = BC_K_B(BCV), BC_K_T(BCV)
            DO J = BC_J_S(BCV), BC_J_N(BCV)
            DO I = BC_I_W(BCV), BC_I_E(BCV)
               IF (.NOT.IS_ON_myPE_plus2layers(I,J,K)) CYCLE
               IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
               IJK = FUNIJK(I,J,K)

! This check is more necessary as in many outflow boundaries bc_ep_s is
! not required and therefore may not be set...
               IF (BC_EP_S(BCV,M) /= UNDEFINED) THEN
                  ROP_S(IJK,M) = BC_EP_S(BCV,M)*RO_S(IJK,M)
               ENDIF
            ENDDO
            ENDDO
            ENDDO
         ENDIF
      ENDDO

      RETURN

      END SUBROUTINE BC_USR_ROS_CORRECTION


END MODULE SET_RO_s_MOD
