MODULE COMP_MEAN_FIELDS1_MOD
CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE COMP_MEAN_FIELDS1

!-----------------------------------------------
! Modules
!-----------------------------------------------
      USE param, only: dimension_3
      USE param1, only: zero, one
      USE fldvar, only: rop_s, ro_s
      USE geometry
      USE indices
      USE compar
      USE parallel
      USE sendrecv
      USE discretelement
      use desgrid
      use desmpi
      USE mfix_pic
      USE functions
      use particle_filter, only: DES_INTERP_ON
      use particle_filter, only: FILTER_WEIGHT
      use particle_filter, only: FILTER_CELL
      use particle_filter, only: FILTER_SIZE
      use sendrecvnode, only: DES_COLLECT_gDATA
      use physprop, only: mmax
      use param1, only: small_number

      IMPLICIT NONE
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! Loop counters: particles, filter cells, phases
      INTEGER NP, LC, M, mLB, mUB
! Fluid cell index
      INTEGER IJK
! Total Mth solids phase volume in IJK
      DOUBLE PRECISION :: SOLVOLINC(DIMENSION_3,MMAX+DES_MMAX)
! PVOL times statistical weight, and times filter weight
      DOUBLE PRECISION :: VOL_WT, VOLxWEIGHT

!-----------------------------------------------

      SOLVOLINC(:,:) = ZERO
      ROP_S(:,:)     = ZERO

      mLB = MMAX+1
      mUB = DES_MMAX+MMAX

! Calculate the gas phase forces acting on each particle.
!$omp parallel default(none) &
!$omp private(NP, VOL_WT, M, LC, IJK, VOLXWEIGHT) &
!$omp shared(MAX_PIP, PVOL, DES_STAT_WT, PIJK,  &
!$omp       FILTER_WEIGHT, SOLVOLINC, DO_K, &
!$omp       FILTER_CELL, FILTER_SIZE, DES_VEL_NEW)
!$omp do
      do NP=1,MAX_PIP
         IF(.NOT.IS_NORMAL(NP)) CYCLE

         VOL_WT = PVOL(NP)
! Particle phase for data binning.
         M = PIJK(NP,5)

         IF(FILTER_SIZE==0) THEN
            IJK = PIJK(NP,4)
            SOLVOLINC(IJK,M) = SOLVOLINC(IJK,M) + VOL_WT
         ENDIF

         DO LC=1,FILTER_SIZE
            IJK = FILTER_CELL(LC,NP)
! Particle volume times the weight for this cell.
            VOLxWEIGHT = VOL_WT*FILTER_WEIGHT(LC,NP)
! Accumulate total solids volume (by phase)
!$omp atomic
            SOLVOLINC(IJK,M) = SOLVOLINC(IJK,M) + VOLxWEIGHT
         ENDDO
      ENDDO
!$omp end do
!$omp end parallel


! Summ data interpolted into ghost cells into physical cells
!---------------------------------------------------------------------//
      IF(DES_INTERP_ON) THEN
         CALL DES_COLLECT_gDATA(SOLVOLINC(:,mLB:mUB))
      ENDIF


! Calculate the cell average solids velocity, the bulk density,
! and the void fraction.
!---------------------------------------------------------------------//
!$omp parallel do if(ijkend3 .ge. 2000) default(shared)                &
!$omp private(IJK,M)
      DO IJK = IJKSTART3, IJKEND3
         IF(.NOT.FLUID_AT(IJK)) CYCLE

! calculating the bulk density of solids phase
         ROP_S(IJK,mLB:mUB) = RO_S(IJK,mLB:mUB)*&
            SOLVOLINC(IJK,mLB:mUB)/VOL(IJK)
      ENDDO
!$omp end parallel do


! Halo exchange of solids volume fraction data.
      calL SEND_RECV(ROP_S,2)

   END SUBROUTINE COMP_MEAN_FIELDS1

END MODULE COMP_MEAN_FIELDS1_MOD
