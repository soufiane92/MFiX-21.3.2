#include "error.inc"

MODULE CHECK_IC_COMMON_DISCRETE_MOD

   use error_manager

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_IC_COMMON_DISCRETE                                !
!  Author:   R.Garg                                   Date: 11-Mar-14  !
!                                                                      !
!  Purpose: check the initial conditions input section common to both  !
!           DEM and MPPIC models                                       !
!     - ensure the first IC is defined over the entire domain with     !
!        ep_g = 1 when more than one IC has solids                     !
!     - ensure the ICs are non-overlapping                             !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_IC_COMMON_DISCRETE

! Modules
!---------------------------------------------------------------------//
! Runtime Flag: Generate initial particle configuration.
      USE discretelement, only : gener_part_config
! Simulation dimension (2D/3D)
      USE discretelement, only: DIMN
! Number of DEM solids phases.
      USE discretelement, only: DES_MMAX
! direction wise spans of the domain and grid spacing in each direction
      use geometry, only: X_MIN, X_MAX, Y_MIN, Y_MAX, Z_MIN, Z_MAX
! Flag indicating that the IC region is defined.
      USE ic, only: IC_DEFINED
! IC Region gas volume fraction.
      USE ic, only: IC_EP_G
! IC Region solid volume fraction.
      USE ic, only: IC_EP_S
! IC Region gas volume fraction.
      USE ic, only: IC_THETA_M, IC_DES_SM, IC_DES_NP
      USE ic, only: IC_WITH_ANY_DES_SOLID, IC_WITH_DES_SOLID_PHASE
!
      USE ic, only: IC_X_w, IC_X_e, IC_Y_s, IC_Y_n, IC_Z_b, IC_Z_t
! IC region DES settings
      USE ic, only: IC_DES_SM, IC_DES_NP
      USE ic, only: IC_DES_LATTICE, IC_DES_SPACING
      USE ic, only: IC_DES_SPACE_FACTOR_X,IC_DES_SPACE_FACTOR_Y
      USE ic, only: IC_DES_SPACE_FACTOR_Z
      USE ic, only: IC_DES_RAND, IC_DES_RAND_FACTOR_X
      USE ic, only: IC_DES_RAND_FACTOR_Y, IC_DES_RAND_FACTOR_Z
! Maximum number of IC regions
      USE param, only: DIMENSION_IC
!
      USE param1, only: UNDEFINED, UNDEFINED_I, ZERO, ONE
!
      use physprop, only: mmax
      use param, only: dim_m

      implicit none

! Local variables
!---------------------------------------------------------------------//
      INTEGER :: ICV, ICV2, M, IDIM
      INTEGER :: COUNT_IC, COUNT_IC_WITH_SOLS
      INTEGER :: FIRST_DEF_IC
      INTEGER :: N_IC_DES_INPUT(DIM_M)
      DOUBLE PRECISION :: IC_ORIG(3), IC_END(3), IC2_ORIG(3) , IC2_END(3)
      DOUBLE PRECISION :: IC_MIN, IC_MAX, IC2_MIN, IC2_MAX , TOL_IC_REG
      LOGICAL :: SEP_AXIS, first_ic_ok
!---------------------------------------------------------------------//

      IF (.NOT.GENER_PART_CONFIG) RETURN

! First check if multiple IC regions are defined for non-zero solids volume
! fraction, then check if the first IC is specified over the whole domain with IC_EP_g = 1

      !total count of defined ICs
      COUNT_IC           = 0
      !total count of defined IC's with solids
      COUNT_IC_WITH_SOLS = 0
      FIRST_DEF_IC = UNDEFINED_I
      IC_WITH_ANY_DES_SOLID(:) = .FALSE.
      IC_WITH_DES_SOLID_PHASE(:,:) = .FALSE.

      DO ICV = 1, DIMENSION_IC

         IF (IC_DEFINED(ICV)) THEN
            COUNT_IC = COUNT_IC + 1
            FIRST_DEF_IC = MIN(FIRST_DEF_IC, ICV)

            DO M = MMAX+1, DES_MMAX+MMAX
               IF(IC_EP_S(ICV,M)>ZERO.OR. &
                  IC_DES_SM(ICV,M)>ZERO.OR.&
                  IC_DES_NP(ICV,M)>0) THEN
                  IC_WITH_ANY_DES_SOLID(ICV) = .TRUE.
                  IC_WITH_DES_SOLID_PHASE(ICV,M) = .TRUE.
               ENDIF
            ENDDO
             
            N_IC_DES_INPUT(:) = 0
            DO M = MMAX+1, DES_MMAX+MMAX
               IF(IC_EP_S(ICV,M)>ZERO.AND.IC_EP_S(ICV,M)/=UNDEFINED) N_IC_DES_INPUT(M) = N_IC_DES_INPUT(M) + 1
               IF(IC_DES_SM(ICV,M)>ZERO) N_IC_DES_INPUT(M) = N_IC_DES_INPUT(M) + 1
               IF(IC_DES_NP(ICV,M)>0) N_IC_DES_INPUT(M) = N_IC_DES_INPUT(M) + 1
            ENDDO


            DO M = MMAX+1, DES_MMAX+MMAX
               SELECT CASE(N_IC_DES_INPUT(M))

                  CASE(0)
                     IC_WITH_DES_SOLID_PHASE(ICV,M) = .FALSE.

                  CASE(1)
                     IC_WITH_ANY_DES_SOLID(ICV)     = .TRUE.
                     IC_WITH_DES_SOLID_PHASE(ICV,M) = .TRUE.

                  CASE(2,3,4)
                     WRITE(ERR_MSG, 1005) ICV,M
                     CALL LOG_ERROR()

               END SELECT

 1005 FORMAT(' Error 1005: IC ',I3,', Solids phase ',I3,&
         /'Particle seeding with more than one method.', &
         /'Only one of the following can be specified: solids volume fraction,',&
         /'solid mass, number of particles.', &
         /'Please correct the project settings.')

            ENDDO
               

            IF(IC_EP_G(ICV).LT.ONE) COUNT_IC_WITH_SOLS &
            = COUNT_IC_WITH_SOLS  + 1

         ENDIF ! if(ic_defined(icv))
      end DO

      IF(COUNT_IC_WITH_SOLS >= 1 .AND. &
         COUNT_IC > COUNT_IC_WITH_SOLS+1) THEN

! If the number of IC's with solids is greater than one, make sure the
! first IC spans the entire domain with voidage of one. This ensures
! that the entire domain has valid ICs defined.
         ICV = FIRST_DEF_IC
         FIRST_IC_OK = .FALSE.
         IF(IC_EP_G(ICV).EQ.ONE &
           .AND.IC_X_W(ICV).LE.X_MIN.AND.IC_X_E(ICV).GE.X_MAX         &
           .AND.IC_Y_S(ICV).LE.Y_MIN.AND.IC_Y_N(ICV).GE.Y_MAX)        &
            FIRST_IC_OK = .TRUE.

         IF (FIRST_IC_OK .AND. IC_Z_B(ICV) <= Z_MIN .AND. &
            IC_Z_T(ICV) >= Z_MAX) FIRST_IC_OK = .TRUE.

         IF(.NOT.FIRST_IC_OK) THEN
            WRITE(ERR_MSG, 1003)
            CALL LOG_ERROR()
         ENDIF

 1003 FORMAT(' Error 1003: Particle seeding with more than one IC ',   &
         'region requires',/'that IC 1 span the entire domain and ',   &
         'have IC_EP_g(1) = 1.0.',/'Please correct the project settings.')

      ENDIF

! Check if the ICs are non-overlapping.
      TOL_IC_REG  = 1E-04
      ICVLOOP : DO ICV = 1, DIMENSION_IC

         IF(.NOT.IC_DEFINED(ICV)) CYCLE ICVLOOP
         IF(IC_EP_G(ICV) == 1.d0) CYCLE ICVLOOP
         IC_ORIG(1) = IC_X_W(ICV)
         IC_ORIG(2) = IC_Y_S(ICV)
         IC_ORIG(3) = IC_Z_B(ICV)
         IC_END(1)  = IC_X_E(ICV)
         IC_END(2)  = IC_Y_N(ICV)
         IC_END(3)  = IC_Z_T(ICV)
         ICVTWOLOOP : DO ICV2 = ICV+1, DIMENSION_IC

            IF(.NOT.IC_DEFINED(ICV2)) CYCLE ICVTWOLOOP
            IF(IC_EP_G(ICV2) == 1.0d0) CYCLE ICVTWOLOOP

            IC2_ORIG(1) = IC_X_W(ICV2)
            IC2_ORIG(2) = IC_Y_S(ICV2)
            IC2_ORIG(3) = IC_Z_B(ICV2)
            IC2_END(1)  = IC_X_E(ICV2)
            IC2_END(2)  = IC_Y_N(ICV2)
            IC2_END(3)  = IC_Z_T(ICV2)

            sep_axis  = .false.
            DO idim = 1, dimn

               ic_min = IC_ORIG(idim)
               ic_max = IC_END(idim)
               ic2_min = IC2_ORIG(idim)
               ic2_max = ic2_END(idim)

! Check for separating axis. If the separating axis exists, then the IC
! regions can't overlap generally equality implies lack of sep_axis,
! and thus, overlapping. However, doing so will flag all IC's as
! overlapping since IC's have to share common edges. So here the
! equality is considered as existence of a separating axis, and hence,
! no overlap equality is also considered as separating axis which is
               if ((ic_min .ge. ic2_max)  .or. (ic_max .le. ic2_min) ) then
                  sep_axis = .true.
                  exit
               endif
            end DO

! Implies the IC regions could not find a separating axis and are
! therefore overlapping.
            IF(.NOT.sep_axis) THEN
               WRITE(ERR_MSG, 1004) ICV, ICV2
               CALL LOG_ERROR()
            ENDIF

 1004 FORMAT('Error 1004: Overlapping IC regions with nonzero solids ',&
         'volume',/'fraction detected. This is not supported for ',    &
         'discrete solids.',2/'Overlapping ICs: ',2(2x,I4),2/,         &
         'Please correct the project settings.')

         end DO ICVTWOLOOP
      end DO ICVLOOP



! Check if IC_theta_M is specified for solids phases wherever IC_EP_g lt 1
      DO ICV = 1, DIMENSION_IC

         IF (IC_DEFINED(ICV).and.IC_EP_G(ICV).LT.ONE) THEN
            DO M = MMAX+1, DES_MMAX+MMAX
               IF(IC_THETA_M(ICV,M)==UNDEFINED) THEN
                  IF(IC_EP_S(ICV,M).gt.Zero) THEN
                     WRITE(ERR_MSG, 1000) trim(iVar('IC_THETA_M',ICV,M))
                     CALL LOG_ERROR()
                  ELSE
                     IC_Theta_M(ICV,M) = ZERO
                  ENDIF
               ENDIF


! Seed IC region with either volume fraction, solid mass
! or number of particles

               IF(IC_EP_S(ICV,M)<ZERO.OR.IC_EP_S(ICV,M)>0.7) THEN   
                  WRITE(ERR_MSG, 1012) ICV, M, IC_EP_S(ICV,M) 
                  CALL LOG_ERROR()
               ENDIF

 1012 FORMAT('Error 1012: IC ',I3,', Solids phase ',I3,&
             /'Invalid solids volume fraction (IC_EP_S).', &
             /'Value specified = ',F14.8,&
             /'Please correct the project settings.')

               IF(IC_DES_SM(ICV,M)<ZERO) THEN   
                  WRITE(ERR_MSG, 1013) ICV, M, IC_DES_SM(ICV,M) 
                  CALL LOG_ERROR()
               ENDIF

 1013 FORMAT('Error 1013: IC ',I3,', Solids phase ',I3,&
             /'Invalid solids mass (IC_DES_SM).', &
             /'Value specified = ',F14.8,&
             /'Please correct the project settings.')

               IF(IC_DES_NP(ICV,M)<0) THEN   
                  WRITE(ERR_MSG, 1014) ICV, M, IC_DES_NP(ICV,M) 
                  CALL LOG_ERROR()
               ENDIF

 1014 FORMAT('Error 1014: IC ',I3,', Solids phase ',I3,&
             /'Invalid number of particles (IC_DES_NP).', &
             /'Value specified = ',I10,&
             /'Please correct the project settings.')

! Throw error if IC_DES_LATTICE is not either CUBIC or HEXA
               IF(.not.((trim(IC_DES_LATTICE(ICV,M)).eq.'CUBIC').or.(trim(IC_DES_LATTICE(ICV,M)).eq.'HEXA'))) THEN   
                  WRITE(ERR_MSG, 1015) ICV, M, trim(IC_DES_LATTICE(ICV,M))
                  CALL LOG_ERROR()
               ENDIF

 1015 FORMAT('Error 1015: IC ',I3,', Solids phase ',I3,&
             /'Invalid Lattice (IC_DES_LATTICE).', &
             /'Value specified = ',A,&
             /'Please correct the project settings.')

               IF(IC_DES_SPACING(ICV,M)<ZERO) THEN   
                  WRITE(ERR_MSG, 1016) ICV, M, IC_DES_SPACING(ICV,M) 
                  CALL LOG_ERROR()
               ENDIF

 1016 FORMAT('Error 1016: IC ',I3,', Solids phase ',I3,&
             /'Invalid spacing (IC_DES_SPACING).', &
             /'Value specified = ',F14.8,&
             /'Please correct the project settings.')

               IF(IC_DES_SPACE_FACTOR_X(ICV,M)<ZERO) THEN   
                  WRITE(ERR_MSG, 1017) ICV, M, IC_DES_SPACE_FACTOR_X(ICV,M) 
                  CALL LOG_ERROR()
               ENDIF

 1017 FORMAT('Error 1017: IC ',I3,', Solids phase ',I3,&
             /'Invalid space factor in x-direction (IC_DES_SPACE_FACTOR_X).', &
             /'Value specified = ',F14.8,&
             /'Please correct the project settings.')

               IF(IC_DES_SPACE_FACTOR_Y(ICV,M)<ZERO) THEN   
                  WRITE(ERR_MSG, 1018) ICV, M, IC_DES_SPACE_FACTOR_Y(ICV,M) 
                  CALL LOG_ERROR()
               ENDIF

 1018 FORMAT('Error 1018: IC ',I3,', Solids phase ',I3,&
             /'Invalid space factor in y-direction (IC_DES_SPACE_FACTOR_Y).', &
             /'Value specified = ',F14.8,&
             /'Please correct the project settings.')

               IF(IC_DES_SPACE_FACTOR_Z(ICV,M)<ZERO) THEN   
                  WRITE(ERR_MSG, 1019) ICV, M, IC_DES_SPACE_FACTOR_Z(ICV,M) 
                  CALL LOG_ERROR()
               ENDIF

 1019 FORMAT('Error 1019: IC ',I3,', Solids phase ',I3,&
             /'Invalid space factor in z-direction (IC_DES_SPACE_FACTOR_Z).', &
             /'Value specified = ',F14.8,&
             /'Please correct the project settings.')

               IF(IC_DES_RAND(ICV,M)<ZERO.OR.IC_DES_RAND(ICV,M)>ONE) THEN   
                  WRITE(ERR_MSG, 1020) ICV, M, IC_DES_RAND(ICV,M) 
                  CALL LOG_ERROR()
               ENDIF

 1020 FORMAT('Error 1020: IC ',I3,', Solids phase ',I3,&
             /'Invalid random factor (IC_DES_RAND).', &
             /'Value specified = ',F14.8,&
             /'Please correct the project settings.')

               IF(IC_DES_RAND_FACTOR_X(ICV,M)<ZERO.OR.IC_DES_RAND_FACTOR_X(ICV,M)>ONE) THEN   
                  WRITE(ERR_MSG, 1021) ICV, M, IC_DES_RAND_FACTOR_X(ICV,M) 
                  CALL LOG_ERROR()
               ENDIF

 1021 FORMAT('Error 1021: IC ',I3,', Solids phase ',I3,&
             /'Invalid random factor in x-direction (IC_DES_RAND_FACTOR_X).', &
             /'Value specified = ',F14.8,&
             /'Please correct the project settings.')

               IF(IC_DES_RAND_FACTOR_Y(ICV,M)<ZERO.OR.IC_DES_RAND_FACTOR_Y(ICV,M)>ONE) THEN   
                  WRITE(ERR_MSG, 1022) ICV, M, IC_DES_RAND_FACTOR_Y(ICV,M) 
                  CALL LOG_ERROR()
               ENDIF

 1022 FORMAT('Error 1022: IC ',I3,', Solids phase ',I3,&
             /'Invalid random factor in y-direction (IC_DES_RAND_FACTOR_Y).', &
             /'Value specified = ',F14.8,&
             /'Please correct the project settings.')

               IF(IC_DES_RAND_FACTOR_Z(ICV,M)<ZERO.OR.IC_DES_RAND_FACTOR_Z(ICV,M)>ONE) THEN   
                  WRITE(ERR_MSG, 1023) ICV, M, IC_DES_RAND_FACTOR_Z(ICV,M) 
                  CALL LOG_ERROR()
               ENDIF

 1023 FORMAT('Error 1023: IC ',I3,', Solids phase ',I3,&
             /'Invalid random factor in z-direction (IC_DES_RAND_FACTOR_Z).', &
             /'Value specified = ',F14.8,&
             /'Please correct the project settings.')

            ENDDO
         ENDIF
      ENDDO

      RETURN

 1000 FORMAT('Error 1000: Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

   END SUBROUTINE CHECK_IC_COMMON_DISCRETE

END MODULE CHECK_IC_COMMON_DISCRETE_MOD
