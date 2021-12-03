MODULE CALC_K_s_MOD
CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: CALC_K_s                                                C
!  Purpose: Calculate the effective conductivity of solids phases      C
!                                                                      C
!                                                                      C
!  Comments:                                                           C
!  This routine will not be called if k_s0(M) is defined               C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE CALC_K_S(M)

! Modules
!---------------------------------------------------------------------//
      USE param1, only: undefined
      USE physprop, only: k_s0, k_s
      use run, only: ks_model_enum, ks_usr, ks_bauer
      USE sendrecv, only: send_recv
! invoke user defined quantity
      USE usr_prop, only: usr_ks, calc_usr_prop
      USE usr_prop, only: solids_conductivity
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! solids phase index
      INTEGER, INTENT(IN) :: M

!---------------------------------------------------------------------//


      IF (KS_MODEL_ENUM(M) == ks_usr) THEN
         CALL CALC_USR_PROP(Solids_Conductivity,lm=M)
      ELSEIF (KS_MODEL_ENUM(M) == ks_bauer) THEN
         CALL CALC_bauer_Ks(M)
      ENDIF

      CALL send_recv(K_S, 2)

      RETURN
   END SUBROUTINE CALC_K_S


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: CALC_bauer_Ks                                           C
!  Purpose: Compute the conductivity coefficient for indicated solids  C
!  wherein a solids phase is considered to be comprised of particles   C
!  of defined size                                                     C
!                                                                      C
!  Author:M. Syamlal                                  Date: 24-APR-96  C
!                                                                      C
!  Literature/Document References:                                     C
!  - R. Bauer and E. U. Schlunder, "Effective radial thermal           C
!    conductivity of packings in gas flow. Part II. Thermal            C
!    conductivity of the packing fraction without gas," International  C
!    Chemical Engineering, vol. 18, no. 2, pp. 189-204, 1978.          C
!  - A. Mills, Basic Heat & Mass Transfer, Upper Saddle River, N.J,    C
!    USA: Prentice Hall; 2 edition, 1998: Table A.3, dry-soil, p912.   C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE CALC_bauer_Ks(M)

! Modules
!---------------------------------------------------------------------//
      USE compar, only: ijkstart3, ijkend3
      USE functions, only: fluid_at
      USE fldvar, only: ep_g, ep_s
      USE param1, only: one, zero
      USE physprop, only: k_g, k_s, k_s0
      USE toleranc, only: dil_ep_s
      USE run, only: units
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! solids index
      INTEGER, INTENT(IN) :: M

! Local parameters
!---------------------------------------------------------------------//
! constant in conductivity equation (dimensionless)
      DOUBLE PRECISION :: PHI_k
      PARAMETER (PHI_k = 7.26D-3)

! Local variables
!---------------------------------------------------------------------//
! Indices
      INTEGER :: IJK
! Quantities in solids conductivity formula
      DOUBLE PRECISION :: BB, R_km, BoR, L_rm
! Local k_g(IJK) (gas phase material conductivity)
      DOUBLE PRECISION :: Kg_micro
! microscopic (material) conductivity of particle (not modified by
! the gas phase). See Mills, 1998, Table A.3 for dry-soil.
      DOUBLE PRECISION :: Ks_micro
!---------------------------------------------------------------------//

!!$omp parallel do private(IJK,B,R_km,BoR,L_rm,Kg_micro) &
!!$omp& schedule(dynamic,chunk_size)
      DO IJK = ijkstart3, ijkend3
         IF (FLUID_AT(IJK)) THEN

            kg_micro = K_G(IJK)

! the material conductivity
            ks_micro = K_S0(M)   ! J/(s.m.K) or cal/s.cm.K

            IF( EP_s(IJK,M) >  DIL_EP_s) THEN
               BB = 1.25D0 * ((ONE - EP_g(IJK))/EP_g(IJK))**(10.D0/9.D0)
               R_km = Ks_micro/Kg_micro
               BoR  = BB/R_km
               L_rm = -(2.d0/(ONE-BoR)) * &
                      ( ((R_km-ONE)/(ONE-BoR)**2)*BoR*LOG(BoR) + &
                        (BB-ONE)/(ONE-BoR) + (BB+ONE)/2.d0 )
! K_s is the macroscopic conductivity that has been modified by the
! presence of the gas phase (cal/s.cm.K)
               K_S(IJK,M) = (Phi_k*R_km + (ONE-Phi_k)*L_rm)*&
                             Kg_micro/SQRT(ONE - EP_g(IJK))
            ELSE
               K_S(IJK, M) = ZERO
            ENDIF

! An approximate average value for the solids conductivity is 2.5*K_g
!            K_S(IJK,M) = 2.5*Kg_micro            !in CGS system

         ELSE   ! else branch if(fluid_at(ijk))
            K_S(IJK,M) = ZERO
         ENDIF   ! end if/else (fluid_at(ijk))

      ENDDO   ! end do (ijk=ijkstart3,ijkend3)

      RETURN
   END SUBROUTINE CALC_bauer_KS

END MODULE CALC_K_s_MOD
