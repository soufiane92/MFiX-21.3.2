      module bilinear
      contains

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Subroutine: INTERPOLATE_PIC2                                        !
!  Author: J.Musser                                                    !
!  Review: M. Clarke                                                   !
!                                                                      !
!  Purpose: Sets up bilinear interpolators using hat functions         !
!  First 3 subroutines account standard cells;  Last 3 subroutines     !
!  account when cell contains a wall.                                  !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE bilinear_x(pNP, pI, pSx, pSxU, pOoDX)

      use geometry, only: DX, oDX_E, oDX
 !     use discretelement, only: XE
      use param1, only: HALF, ONE

      use discretelement
      use functions
      use compar

      implicit none

! Dummy Arguments
!---------------------------------------------------------------------//
      INTEGER, INTENT(IN) :: pNP
      INTEGER, INTENT(OUT) :: pI
      DOUBLE PRECISION :: pOoDX
      DOUBLE PRECISION, INTENT(OUT) :: pSx(0:1)
      DOUBLE PRECISION, INTENT(OUT) :: pSxU(0:1)

! Local Variables
!---------------------------------------------------------------------//
      DOUBLE PRECISION :: XC
      INTEGER :: IJK, lI
!......................................................................!

      IJK = PIJK(pNP,4)
      lI = PIJK(pNP,1)
      pI = lI

! Skip parcels that are in a non-fluid cell.
     IF(FLUID_AT(IJK)) THEN

! Setting up x-direction
! Following the hat-function format of original PIC implementations
! from 1990s.  Not as accurate as other interpolative functions but
! very simple

! Tentative weights for I indices to the West and East.
         XC = XE(pI-1) + HALF*DX(pI)
         IF(DES_POS_NEW(pNP,1) < XC) THEN
            pI = pI-1
            pOoDX = oDX_E(pI)
            pSx(0) = (XC - DES_POS_NEW(pNP,1))*pOoDX
            pSx(1) = ONE - pSx(0)
         ELSE
            pOoDX = oDX_E(pI)
            pSx(1) = (DES_POS_NEW(pNP,1) - XC)*pOoDX
            pSx(0) = ONE - pSx(1)
         ENDIF

! Tentative weights for I+1/2 indices to the West and East.
          pSxU(0) = (XE(lI) - DES_POS_NEW(pNP,1))*oDX(lI)
          pSxU(1) = ONE - pSxU(0)

! The parcel is in a non-fluid grid cell
      ELSE

         IF(FLUID_AT(WEST_OF(IJK))) THEN
            pI = pI-1
            pOoDX = oDX_E(pI)
            pSx(0:1)=(/1.0d0,0.0d0/)
            pSxU(0:1)=(/1.0d0,0.0d0/)
         ELSEIF(FLUID_AT(EAST_OF(IJK))) THEN
            pOoDX = oDX_E(pI)
            pSx(0:1)=(/0.0d0,1.0d0/)
            pSxU(0:1)=(/0.0d0,1.0d0/)
         ELSE
            pOoDX = 0.0d0
            pSx(0:1)=(/1.0d0,1.0d0/)
            pSxU(0:1)=(/1.0d0,1.0d0/)
         ENDIF

      ENDIF

      RETURN
      END SUBROUTINE bilinear_x

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Subroutine: INTERPOLATE_PIC2                                        !
!  Author: J.Musser                                                    !
!                                                                      !
!  Purpose:                                                            !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE bilinear_y(pNP, pJ, pSy, pSyV, pOoDY)

!      use functions, only: FLUID_AT

      use geometry, only: DY, oDY
      use geometry, only: oDY_N
!      use discretelement, only: YN

      use param1, only: HALF, ONE

      use compar
      use functions
      use discretelement

      implicit none

! Dummy Arguments
!---------------------------------------------------------------------//
      INTEGER, INTENT(IN) :: pNP
      INTEGER, INTENT(OUT) :: pJ
      DOUBLE PRECISION :: pOoDY
      DOUBLE PRECISION, INTENT(OUT) :: pSy(0:1)
      DOUBLE PRECISION, INTENT(OUT) :: pSyV(0:1)

! Local Variables
!---------------------------------------------------------------------//
      INTEGER :: IJK, lJ
      DOUBLE PRECISION :: YC
!......................................................................!

      IJK = PIJK(pNP,4)
      lJ = PIJK(pNP,2)
      pJ = lJ

! Skip parcels that are in a non-fluid cell.
     IF(FLUID_AT(IJK)) THEN

! Setting up y-direction
! Following the hat function format of original PIC implementation

! Tentative weights for J indices to the South and North.
         YC = YN(pJ-1) + HALF*DY(pJ)
         IF(DES_POS_NEW(pNP,2) < YC) THEN
            pJ = pJ-1
            pOoDY = oDY_N(pJ)
            pSy(0) = (YC - DES_POS_NEW(pNP,2))*pOoDY
            pSy(1) = ONE - pSy(0)
         ELSE
            pOoDY = oDY_N(pJ)
            pSy(1) = (DES_POS_NEW(pNP,2) - YC)*pOoDY
            pSy(0) = ONE - pSy(1)
         ENDIF

! Tentative weights for J+1/2 indices to the South and North.
          pSyV(0) = (YN(lJ) - DES_POS_NEW(pNP,2))*oDY(lJ)
          pSyV(1) = ONE - pSyV(0)

! The parcel is in a non-fluid grid cell
      ELSE

         IF(FLUID_AT(SOUTH_OF(IJK))) THEN
            pJ = pJ-1
            pOoDY = oDY_N(pJ)
            pSy(0:1)=(/1.0d0,0.0d0/)
            pSyV(0:1)=(/1.0d0,0.0d0/)
         ELSEIF(FLUID_AT(NORTH_OF(IJK))) THEN
            pOoDY = oDY_N(pJ)
            pSy(0:1)=(/0.0d0,1.0d0/)
            pSyV(0:1)=(/0.0d0,1.0d0/)
         ELSE
            pOoDY = 0.0d0
            pSy(0:1)=(/1.0d0,1.0d0/)
            pSyV(0:1)=(/1.0d0,1.0d0/)
         ENDIF

      ENDIF

      RETURN
      END SUBROUTINE bilinear_y


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Subroutine: INTERPOLATE_PIC2                                        !
!  Author: J.Musser                                                    !
!                                                                      !
!  Purpose:                                                            !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE bilinear_z(pNP, pK, pSz, pSzW, pOoDZ)

!      use functions, only: FLUID_AT
      use geometry, only: DZ, oDZ
      use geometry, only: oDZ_T, NO_K
!      use discretelement, only: ZT
      use param1, only: HALF, ONE

      use compar
      use functions
      use discretelement

      implicit none

! Dummy Arguments
!---------------------------------------------------------------------//
      INTEGER, INTENT(IN) :: pNP
      INTEGER, INTENT(OUT) :: pK
      DOUBLE PRECISION :: pOoDZ
      DOUBLE PRECISION, INTENT(OUT) :: pSz(0:1)
      DOUBLE PRECISION, INTENT(OUT) :: pSzW(0:1)

! Local Variables
!---------------------------------------------------------------------//
      DOUBLE PRECISION :: ZC
      INTEGER :: IJK, lK
!......................................................................!

      IJK = PIJK(pNP,4)
      lK = PIJK(pNP,3)
      pK = lK

      IF(NO_K) THEN
         pOoDZ = 0.0d0
         pSz(:) = ONE
         pSzW(:) = ONE
         RETURN
      ENDIF

! Skip parcels that are in a non-fluid cell.
     IF(FLUID_AT(IJK)) THEN

! setting up z-direction
! Following the hat function format of the original PIC  method

! Tentative weights for K indices to the Top and Bottom.
         ZC = ZT(pK-1) + HALF*DZ(pK)
         IF(DES_POS_NEW(pNP,3) < ZC) THEN
            pK = pK-1
            pOoDZ = oDZ_T(pK)
            pSz(0) = (ZC - DES_POS_NEW(pNP,3))*pOoDZ
            pSz(1) = ONE - pSz(0)
         ELSE
            pOoDZ = oDZ_T(pK)
            pSz(1) = (DES_POS_NEW(pNP,3) - ZC)*pOoDZ
            pSz(0) = ONE - pSz(1)
         ENDIF

! Tentative weights for K+1/2 indices to the Top and Bottom.
         pSzW(0) = (ZT(lK) - DES_POS_NEW(pNP,3))*oDZ(lK)
         pSzW(1) = ONE - pSzW(0)

! The parcel is in a non-fluid grid cell
      ELSE

         IF(FLUID_AT(BOTTOM_OF(IJK))) THEN
            pK = pK-1
            pOoDZ = oDZ_T(pK)
            pSz(0:1)=(/1.0d0,0.0d0/)
            pSzW(0:1)=(/1.0d0,0.0d0/)
         ELSEIF(FLUID_AT(TOP_OF(IJK))) THEN
            pOoDZ = oDZ_T(pK)
            pSz(0:1)=(/0.0d0,1.0d0/)
            pSzW(0:1)=(/0.0d0,1.0d0/)
         ELSE
            pOoDZ = 0.0d0
            pSz(0:1)=(/1.0d0,1.0d0/)
            pSzW(0:1)=(/1.0d0,1.0d0/)
         ENDIF
      ENDIF

      RETURN
      END SUBROUTINE bilinear_z

      end module bilinear
