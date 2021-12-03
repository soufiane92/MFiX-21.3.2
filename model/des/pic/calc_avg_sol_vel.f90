!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
SUBROUTINE CALC_AVG_SOL_VEL

!-----------------------------------------------
! Modules
!-----------------------------------------------
  USE param, only: dimension_3
  USE param1, only: zero
  USE fldvar, only: u_s, v_s, w_s
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

  ! use param
  ! use param1

  use bilinear
  use sendrecvnode, only: DES_COLLECT_gDATA

  IMPLICIT NONE
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! Loop counters: particles, filter cells, phases
  INTEGER NP
! Fluid cell index
  INTEGER IJK
! Total Mth solids phase volume in IJK
  DOUBLE PRECISION, allocatable :: uMass(:), vMass(:), wMass(:)

! PVOL times statistical weight, and times filter weight
  DOUBLE PRECISION :: MASS, MASSxWEIGHT
! Putting back in vol-wt calculation

  INTEGER :: lI, lJ, lK
  INTEGER :: IC, JC, KC

  DOUBLE PRECISION :: KWT

  DOUBLE PRECISION :: lOoDX, lOoDY, lOoDZ
  DOUBLE PRECISION :: Sx(0:1), Sy(0:1), Sz(0:1)
  DOUBLE PRECISION :: SxU(0:1), SyV(0:1), SzW(0:1)

!-----------------------------------------------

  allocate(uMass(dimension_3))
  allocate(vMass(dimension_3))
  allocate(wMass(dimension_3))

! Initialize arrays
  uMASS(:) = ZERO
  vMASS(:) = ZERO
  wMASS(:) = ZERO

  U_s(:,1) = ZERO
  V_s(:,1) = ZERO
  W_s(:,1) = ZERO

  KWT = merge(1.0d0, 0.0d0, DO_K)

! Calculate the gas phase forces acting on each particle.
! Applies bilinear interpolation to properly weight velocities
! to parcel location
  do NP=1,MAX_PIP
     IF(IS_NORMAL(NP)) THEN

        call bilinear_x(NP, lI, Sx, SxU, lOoDX)
        call bilinear_y(NP, lJ, Sy, SyV, lOoDY)
        call bilinear_z(NP, lK, Sz, SzW, lOoDZ)

        MASS = PMASS(NP)*DES_STAT_WT(NP)

        DO KC=0,1
           DO IC=0,1
              DO JC=0,1

                 IJK = FUNIJK_MAP_C(lI+IC,lJ+JC,lK+KC)

                 ! U-Momentum Cell
                 MASSxWEIGHT = MASS*SxU(IC)*Sy(JC)*Sz(KC)
                 uMASS(IJK) = uMASS(IJK) + MASSxWEIGHT
                 U_s(IJK,1) = U_s(IJK,1) + DES_VEL_NEW(NP,1)*MASSxWEIGHT

                 ! V-Momentum Cell
                 MASSxWEIGHT = MASS*Sx(IC)*SyV(JC)*Sz(KC)
                 vMASS(IJK) = vMASS(IJK) + MASSxWEIGHT
                 V_s(IJK,1) = V_s(IJK,1) + DES_VEL_NEW(NP,2)*MASSxWEIGHT

                 ! W-Momentum Cell
                 MASSxWEIGHT = MASS*Sx(IC)*Sy(JC)*SzW(KC)*KWT
                 wMASS(IJK) = wMASS(IJK) + MASSxWEIGHT
                 W_s(IJK,1) = W_s(IJK,1) + DES_VEL_NEW(NP,3)*MASSxWEIGHT

              enddo
           enddo
        enddo
     endif
  enddo

  ! Sum data interpolted into ghost cells into physical cells
  !---------------------------------------------------------------------//
  call DES_COLLECT_gDATA(U_s(:,1))
  call DES_COLLECT_gDATA(V_s(:,1))
  call DES_COLLECT_gDATA(W_s(:,1))

  call DES_COLLECT_gDATA(uMass)
  call DES_COLLECT_gDATA(vMass)
  call DES_COLLECT_gDATA(wMass)

  ! Calculate the cell average solids velocity
  !---------------------------------------------------------------------//
  DO IJK = IJKSTART3, IJKEND3

     IF(uMASS(IJK) > ZERO) THEN
        U_s(IJK,1) = U_s(IJK,1)/uMASS(IJK)
     else
        U_s(IJK,1) = 0.0d0
     ENDIF

     IF(vMASS(IJK) > ZERO) THEN
        V_s(IJK,1) = V_s(IJK,1)/vMASS(IJK)
     ELSE
        V_s(IJK,1) = 0.0d0
     ENDIF

     IF(wMASS(IJK) > ZERO) THEN
        W_s(IJK,1) = W_s(IJK,1)/wMASS(IJK)
     ELSE
        W_s(IJK,1) = 0.0d0
     ENDIF

  ENDDO

  if(allocated(uMass))deallocate(uMass)
  if(allocated(vMass))deallocate(vMass)
  if(allocated(wMass))deallocate(wMass)

end SUBROUTINE CALC_AVG_SOL_VEL
