MODULE SET_GEOMETRY1_MOD
CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: SET_GEOMETRY1                                          C
!  Purpose: Calculate cell volumes and face areas                      C
!                                                                      C
!  Author: M. Syamlal                                 Date: 1-MAY-96   C
!  Reviewer:                                                           C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced: COORDINATES, IMAX2, DT, DX, JMAX2, DY, KMAX2, C
!                        DZ,                                           C
!                                                                      C
!  Variables modified:                                                 C
!  Local variables:                                                    C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
   SUBROUTINE SET_GEOMETRY1

!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE param
      USE param1
      USE parallel
      USE run
      USE geometry
      USE indices
      USE compar
      USE functions

      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!
!                      Indices
      INTEGER          I, J, K, IP, JP, KP, IJK
      INTEGER          i_cyl_min, i_cyl_max
!
!-----------------------------------------------
!
!                      Indices
!
!!!!$omp  parallel do private( I, J, K, IP, JP, KP, IJK)  &
!!!!$omp  schedule(dynamic,chunk_size)
!  For cylindrical_2D simulations
      IF(CYLINDRICAL_2D)THEN
         if(mod(imax,2).eq.1)then     ! odd
            i_cyl_min = (imax+1)/2 + 1 - i_cyl_num
            i_cyl_max = (imax+1)/2 + 1 + i_cyl_num
         else
            i_cyl_min = (imax)/2 + 2 - i_cyl_num
            i_cyl_max = (imax)/2 + 1 + i_cyl_num
         endif
      ENDIF

      DO IJK = ijkstart3, ijkend3

!
         I = I_OF(IJK)
         IP = IP1(I)
         J = J_OF(IJK)
         JP = JP1(J)
         K = K_OF(IJK)
         KP = KP1(K)

         IF(.NOT.IS_ON_myPE_plus2layers(I,J,K)) CYCLE
         IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells

!
         VOL(IJK) = DX(I)*DY(J)*(X(I)*DZ(K))
         VOL_U(IJK) = HALF*(DX(I)+DX(IP))*DY(J)*(HALF*(X(I)+X(IP))*DZ(K))
         VOL_V(IJK) = DX(I)*HALF*(DY(J)+DY(JP))*(X(I)*DZ(K))
         VOL_W(IJK) = DX(I)*DY(J)*(X(I)*HALF*(DZ(K)+DZ(KP)))
!
         AYZ(IJK) = DY(J)*(X_E(I)*DZ(K))
         AYZ_U(IJK) = DY(J)*(X(IP)*DZ(K))
         AYZ_V(IJK) = HALF*(DY(J)+DY(JP))*(X_E(I)*DZ(K))
         AYZ_W(IJK) = DY(J)*(X_E(I)*HALF*(DZ(K)+DZ(KP)))
!
         AXY(IJK) = DX(I)*DY(J)
         AXY_U(IJK) = HALF*(DX(I)+DX(IP))*DY(J)
         AXY_V(IJK) = DX(I)*HALF*(DY(J)+DY(JP))
         AXY_W(IJK) = AXY(IJK)
!
         AXZ(IJK) = DX(I)*(X(I)*DZ(K))
         AXZ_U(IJK) = HALF*(DX(I)+DX(IP))*(HALF*(X(I)+X(IP))*DZ(K))
         AXZ_V(IJK) = AXZ(IJK)
         AXZ_W(IJK) = DX(I)*(X(I)*HALF*(DZ(K)+DZ(KP)))

! Aspect Ratio         
         Aspect_Ratio(IJK)   = Get_Aspect_Ratio(DX(I),DY(J),DZ(K))
         Aspect_Ratio_U(IJK) = Get_Aspect_Ratio(HALF*(DX(I)+DX(IP)),DY(J),DZ(K))
         Aspect_Ratio_V(IJK) = Get_Aspect_Ratio(DX(I),HALF*(DY(J)+DY(JP)),DZ(K))
         Aspect_Ratio_W(IJK) = Get_Aspect_Ratio(DX(I),DY(J),HALF*(DZ(K)+DZ(KP)))




      if(cylindrical_2d)then
         VOL(IJK) = DX(I)*DY(J)*(cyl_X(I))
         VOL_U(IJK) = HALF*(DX(I)+DX(IP))*DY(J)*(HALF*(cyl_X(I)+cyl_X(IP)))
         VOL_V(IJK) = DX(I)*HALF*(DY(J)+DY(JP))*(cyl_X(I))
         VOL_W(IJK) = DX(I)*DY(J)*(cyl_X(I))
!
         AYZ(IJK) = DY(J)*(cyl_X_E(I))
         AYZ_U(IJK) = DY(J)*(cyl_X(IP))
         AYZ_V(IJK) = HALF*(DY(J)+DY(JP))*(cyl_X_E(I))
         AYZ_W(IJK) = DY(J)*(cyl_X_E(I))
!
         AXY(IJK) = DX(I)*DY(J)
         AXY_U(IJK) = HALF*(DX(I)+DX(IP))*DY(J)
         AXY_V(IJK) = DX(I)*HALF*(DY(J)+DY(JP))
         AXY_W(IJK) = AXY(IJK)
!
         AXZ(IJK) = DX(I)*(cyl_X(I))
         AXZ_U(IJK) = HALF*(DX(I)+DX(IP))*(HALF*(cyl_X(I)+cyl_X(IP)))
         AXZ_V(IJK) = AXZ(IJK)
         AXZ_W(IJK) = DX(I)*(cyl_X(I))

         if(i.ge.i_cyl_min -i_cyl_transition .and. i.le.i_cyl_max +i_cyl_transition)then
            vol_u(ijk) = half*dx(i)*dy(j)*half*(half*(cyl_x_e(i-1) + cyl_x_e(i))+cyl_x_e(i)) + &
               half*dx(i+1)*dy(j)*half*(half*(cyl_x_e(i) + cyl_x_e(i+1))+cyl_x_e(i))
            axz_u(IJK) = half*dx(i)*half*(half*(cyl_x_e(i-1) + cyl_x_e(i))+cyl_x_e(i)) + &
               half*dx(i+1)*half*(half*(cyl_x_e(i) + cyl_x_e(i+1))+cyl_x_e(i))
         endif
      endif

      END DO
      RETURN

   END SUBROUTINE SET_GEOMETRY1

!-----------------------------------------------------------------<<<
! Aspect ratio
!----------------------------------------------------------------->>>
      DOUBLE PRECISION FUNCTION Get_Aspect_Ratio(DXCELL,DYCELL,DZCELL)
      use param1, only: zero, undefined
      use geometry, only: no_k

      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN) :: DXCELL,DYCELL,DZCELL
      DOUBLE PRECISION :: SMALLEST_DIM, LARGEST_DIM

      IF(NO_K) THEN
         SMALLEST_DIM = DMIN1(DXCELL,DYCELL)
         LARGEST_DIM  = DMAX1(DXCELL,DYCELL)
      ELSE
         SMALLEST_DIM = DMIN1(DXCELL,DYCELL,DZCELL)
         LARGEST_DIM  = DMAX1(DXCELL,DYCELL,DZCELL)
      ENDIF

      IF(SMALLEST_DIM>ZERO) THEN
         GET_ASPECT_RATIO = LARGEST_DIM/SMALLEST_DIM
      ELSE
         GET_ASPECT_RATIO = UNDEFINED
      ENDIF

      RETURN
      END FUNCTION GET_ASPECT_RATIO
END MODULE SET_GEOMETRY1_MOD
