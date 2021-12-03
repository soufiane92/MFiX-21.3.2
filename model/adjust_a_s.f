!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module: ADJUST_A_s                                                  C
!  Purpose: Handle the special case of the center coefficient in       C
!               momentum eq. becoming zero.                            C
!                                                                      C
!  Author: M. Syamlal                                 Date:  2-AUG-96  C
!  Reviewer:                                          Date:            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

MODULE ADJUST_A_S

   USE compar, only: ijkstart3, ijkend3
   USE fldvar, only: rop_s
   USE fun_avg, only: avg_x_e, avg_y_n, avg_z_t
   USE functions, only: east_of, north_of, top_of, im_of, jm_of, km_of
   USE geometry, only: ayz_u, axz_v, axy_w
   USE indices, only: i_of, ip1
   USE param, only: dimension_3, dimension_m, bottom, east, north, south, top, west
   USE param1, only: one, small_number, zero
   USE physprop, only: mmax
   USE run, only: drag_type_enum, ghd_2007, momentum_x_eq, momentum_y_eq, momentum_z_eq

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: ADJUST_A_U_s                                            C
!  Purpose: Handle the special case of the center coefficient in       C
!           U_s momentum eq. becoming zero.                            C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

   SUBROUTINE ADJUST_A_U_S(A_M, B_M)

      IMPLICIT NONE
!-----------------------------------------------
! Dummy Arguments
!-----------------------------------------------
! Septadiagonal matrix A_m
      DOUBLE PRECISION, INTENT(INOUT) :: A_m(DIMENSION_3, -3:3, 0:DIMENSION_M)
! Vector b_m
      DOUBLE PRECISION, INTENT(INOUT) :: B_m(DIMENSION_3, 0:DIMENSION_M)
!-----------------------------------------------
! Local Variables
!-----------------------------------------------
! Indices
      INTEGER :: I, IP, IJK, IJKE, IMJK
! Phase index
      INTEGER :: M
!-----------------------------------------------

      DO M = 1, MMAX
         IF (DRAG_TYPE_ENUM == GHD_2007 .AND. M /= MMAX) CYCLE
         IF (MOMENTUM_X_EQ(M)) THEN

!!$omp     parallel do private(I, IP, IJK, IJKE, IMJK )

            DO IJK = ijkstart3, ijkend3
               IF (ABS(A_M(IJK,0,M)) < SMALL_NUMBER) THEN
                  A_M(IJK,east,M) = ZERO
                  A_M(IJK,west,M) = ZERO
                  A_M(IJK,north,M) = ZERO
                  A_M(IJK,south,M) = ZERO
                  A_M(IJK,top,M) = ZERO
                  A_M(IJK,bottom,M) = ZERO
                  A_M(IJK,0,M) = -ONE
                  IF (B_M(IJK,M) < ZERO) THEN
                     IJKE = EAST_OF(IJK)
                     IP = IP1(I_OF(IJK))
                     IF (ROP_S(IJKE,M)*AYZ_U(IJK) > SMALL_NUMBER) THEN
                        B_M(IJK,M) = SQRT((-B_M(IJK,M)/(ROP_S(IJKE,M)*&
                           AVG_X_E(ONE,ZERO,IP)*AYZ_U(IJK))))
                     ELSE
                        B_M(IJK,M) = ZERO
                     ENDIF
                  ELSEIF (B_M(IJK,M) > ZERO) THEN
                     I = I_OF(IJK)
                     IMJK = IM_OF(IJK)
                     IF (ROP_S(IJK,M)*AYZ_U(IMJK) > SMALL_NUMBER) THEN
                        B_M(IJK,M) = SQRT(B_M(IJK,M)/(ROP_S(IJK,M)*&
                           AVG_X_E(ZERO,ONE,I)*AYZ_U(IMJK)))
                     ELSE
                        B_M(IJK,M) = ZERO
                     ENDIF
                  ENDIF
               ENDIF    ! end if (abs(a_m(ijk,0,m))<small_number)
            ENDDO    ! end do loop (ijk=ijkstart3,ijkend3)

         ENDIF   ! end if (momentum_x_eq(m))
      ENDDO   ! end do loop (m=1,mmax)

      RETURN

   END SUBROUTINE ADJUST_A_U_S

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: ADJUST_A_V_s                                            C
!  Purpose: Handle the special case of the center coefficient in       C
!           V_s momentum eq. becoming zero.                            C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

   SUBROUTINE ADJUST_A_V_S(A_M, B_M)

      IMPLICIT NONE

!-----------------------------------------------
! Dummy Arguments
!-----------------------------------------------
! Septadiagonal matrix A_m
      DOUBLE PRECISION, INTENT(INOUT) :: A_m(DIMENSION_3, -3:3, 0:DIMENSION_M)
! Vector b_m
      DOUBLE PRECISION, INTENT(INOUT) :: B_m(DIMENSION_3, 0:DIMENSION_M)
!-----------------------------------------------
! Local Variables
!-----------------------------------------------
! Indices
      INTEGER :: IJK, IJKN, IJMK
! Phase index
      INTEGER :: M
!-----------------------------------------------

      DO M = 1, MMAX
         IF (DRAG_TYPE_ENUM == GHD_2007 .AND. M /= MMAX) CYCLE
         IF (MOMENTUM_Y_EQ(M)) THEN

!!$omp     parallel do private(IJK,IJKN,IJMK)
            DO IJK = ijkstart3, ijkend3
               IF (ABS(A_M(IJK,0,M)) < SMALL_NUMBER) THEN
                  A_M(IJK,east,M) = ZERO
                  A_M(IJK,west,M) = ZERO
                  A_M(IJK,north,M) = ZERO
                  A_M(IJK,south,M) = ZERO
                  A_M(IJK,top,M) = ZERO
                  A_M(IJK,bottom,M) = ZERO
                  A_M(IJK,0,M) = -ONE
                  IF (B_M(IJK,M) < ZERO) THEN
                     IJKN = NORTH_OF(IJK)
                     IF (ROP_S(IJKN,M)*AXZ_V(IJK) > SMALL_NUMBER) THEN
                        B_M(IJK,M) = SQRT((-B_M(IJK,M)/(ROP_S(IJKN,M)*&
                           AVG_Y_N(ONE,ZERO)*AXZ_V(IJK))))
                     ELSE
                        B_M(IJK,M) = ZERO
                     ENDIF
                  ELSEIF (B_M(IJK,M) > ZERO) THEN
                     IJMK = JM_OF(IJK)
                     IF (ROP_S(IJK,M)*AXZ_V(IJK) > SMALL_NUMBER) THEN
                        B_M(IJK,M) = SQRT(B_M(IJK,M)/(ROP_S(IJK,M)*&
                           AVG_Y_N(ZERO,ONE)*AXZ_V(IJK)))
                     ELSE
                        B_M(IJK,M) = ZERO
                     ENDIF
                  ENDIF
               ENDIF    ! end if (abs(a_m(ijk,0,m))<small_number)
            ENDDO    ! end do loop (ijk=ijkstart3,ijkend3)

         ENDIF   ! end if (momentum_y_eq(m))
      ENDDO   ! end do loop (m=1,mmax)

      RETURN

   END SUBROUTINE ADJUST_A_V_S

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: ADJUST_A_W_s                                            C
!  Purpose: Handle the special case of the center coefficient in       C
!           W_s momentum eq. becoming zero.                            C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

   SUBROUTINE ADJUST_A_W_S(A_M, B_M)

      IMPLICIT NONE
!-----------------------------------------------
! Dummy Arguments
!-----------------------------------------------
! Septadiagonal matrix A_m
      DOUBLE PRECISION, INTENT(INOUT) :: A_m(DIMENSION_3, -3:3, 0:DIMENSION_M)
! Vector b_m
      DOUBLE PRECISION, INTENT(INOUT) :: B_m(DIMENSION_3, 0:DIMENSION_M)
!-----------------------------------------------
! Local Variables
!-----------------------------------------------
! Indices
      INTEGER :: IJK, IJKT, IJKM
! Phase index
      INTEGER :: M
!-----------------------------------------------

      DO M = 1, MMAX
         IF (DRAG_TYPE_ENUM == GHD_2007 .AND. M /= MMAX) CYCLE
         IF (MOMENTUM_Z_EQ(M)) THEN

!!$omp  parallel do private(IJK,IJKT,IJKM)
            DO IJK = ijkstart3, ijkend3
               IF (ABS(A_M(IJK,0,M)) < SMALL_NUMBER) THEN
                  A_M(IJK,east,M) = ZERO
                  A_M(IJK,west,M) = ZERO
                  A_M(IJK,north,M) = ZERO
                  A_M(IJK,south,M) = ZERO
                  A_M(IJK,top,M) = ZERO
                  A_M(IJK,bottom,M) = ZERO
                  A_M(IJK,0,M) = -ONE
                  IF (B_M(IJK,M) < ZERO) THEN
                     IJKT = TOP_OF(IJK)
                     IF (ROP_S(IJKT,M)*AXY_W(IJK) > SMALL_NUMBER) THEN
                        B_M(IJK,M) = SQRT((-B_M(IJK,M)/(ROP_S(IJKT,M)*&
                           AVG_Z_T(ONE,ZERO)*AXY_W(IJK))))
                     ELSE
                        B_M(IJK,M) = ZERO
                     ENDIF
                  ELSE IF (B_M(IJK,M) > ZERO) THEN
                     IJKM = KM_OF(IJK)
                     IF (ROP_S(IJK,M)*AXY_W(IJKM) > SMALL_NUMBER) THEN
                        B_M(IJK,M) = SQRT(B_M(IJK,M)/(ROP_S(IJK,M)*&
                           AVG_Z_T(ZERO,ONE)*AXY_W(IJKM)))
                     ELSE
                        B_M(IJK,M) = ZERO
                     ENDIF
                  ENDIF
               ENDIF    ! end if (abs(a_m(ijk,0,m))<small_number)
            ENDDO    ! end do loop (ijk=ijkstart3,ijkend3)

         ENDIF   ! end if (momentum_z_eq(m))
      ENDDO   ! end do loop (m=1,mmax)

      RETURN

   END SUBROUTINE ADJUST_A_W_S

END MODULE ADJUST_A_S
