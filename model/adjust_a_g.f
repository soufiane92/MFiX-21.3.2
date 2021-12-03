!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: ADJUST_A_g                                             C
!  Purpose: Handle the special case of the center coefficient in       C
!      momentum eq. becoming zero.                                     C
!                                                                      C
!                                                                      C
!  Author: M. Syamlal                                 Date:  2-AUG-96  C
!  Reviewer:                                          Date:            C
!                                                                      C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced:                                               C
!  Variables modified:                                                 C
!                                                                      C
!  Local variables:                                                    C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

MODULE ADJUST_A_G

   USE compar, only: ijkstart3, ijkend3
   USE fldvar, only: rop_g
   USE fun_avg, only: avg_x_e, avg_y_n, avg_z_t
   USE functions, only: im_of, jm_of, km_of, east_of, north_of, top_of
   USE geometry, only: ayz_u, axz_v, axy_w
   USE indices, only: i_of, ip1
   USE param, only: dimension_3, dimension_m, bottom, east, north, south, top, west
   USE param1, only: one, small_number, zero
   USE run, only: momentum_x_eq, momentum_y_eq, momentum_z_eq, dt

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: ADJUST_A_g                                             C
!  Purpose: Handle the special case of the center coefficient in       C
!  U_g momentum eq. becoming zero.                                     C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

   SUBROUTINE ADJUST_A_U_G(A_M, B_M)

      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
!
!                      Indices
      INTEGER          I, IP, IJK, IJKE, IMJK
!
!                      Phase index
      INTEGER          M
!
!                      Septadiagonal matrix A_m
      DOUBLE PRECISION A_m(DIMENSION_3, -3:3, 0:DIMENSION_M)
!
!                      Vector b_m
      DOUBLE PRECISION B_m(DIMENSION_3, 0:DIMENSION_M)
!-----------------------------------------------

      M = 0
      IF (.NOT.MOMENTUM_X_EQ(0)) RETURN
!
!!!!$omp parallel do private(I,IP,IJK,IJKE,IMJK)
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
               IF (ROP_G(IJKE)*AYZ_U(IJK) > SMALL_NUMBER) THEN
                  B_M(IJK,M) = SQRT((-B_M(IJK,M)/(ROP_G(IJKE)*AVG_X_E(ONE,ZERO,&
                     IP)*AYZ_U(IJK))))
               ELSE
                  B_M(IJK,M) = ZERO
               ENDIF
            ELSE IF (B_M(IJK,M) > ZERO) THEN
               I = I_OF(IJK)
               IMJK = IM_OF(IJK)
               IF (ROP_G(IJK)*AYZ_U(IMJK) > SMALL_NUMBER) THEN
                  B_M(IJK,M) = SQRT(B_M(IJK,M)/(ROP_G(IJK)*AVG_X_E(ZERO,ONE,I)*&
                     AYZ_U(IMJK)))
               ELSE
                  B_M(IJK,M) = ZERO
               ENDIF
            ENDIF
         ENDIF
      END DO

      RETURN

   END SUBROUTINE ADJUST_A_U_G

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: ADJUST_A_V_g(A_m, B_m, IER)                            C
!  Purpose: Handle the special case of the center coefficient in       C
!  V_g momentum eq. becoming zero.                                     C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

   SUBROUTINE ADJUST_A_V_G(A_M, B_M)

      IMPLICIT NONE

!                      Indices
      INTEGER          IJK, IJKN, IJMK
!
!                      Phase index
      INTEGER          M
!
!                      Septadiagonal matrix A_m
      DOUBLE PRECISION A_m(DIMENSION_3, -3:3, 0:DIMENSION_M)
!
!                      Vector b_m
      DOUBLE PRECISION B_m(DIMENSION_3, 0:DIMENSION_M)
!-----------------------------------------------

      M = 0
      IF (.NOT.MOMENTUM_Y_EQ(0)) RETURN
!
!!!!$omp parallel do private(IJK,IJKN,IJMK)
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
               IF (ROP_G(IJKN)*AXZ_V(IJK) > SMALL_NUMBER) THEN
                  B_M(IJK,M) = SQRT((-B_M(IJK,M)/(ROP_G(IJKN)*AVG_Y_N(ONE,ZERO)&
                     *AXZ_V(IJK))))
               ELSE
                  B_M(IJK,M) = ZERO
               ENDIF
            ELSE IF (B_M(IJK,M) > ZERO) THEN
               IJMK = JM_OF(IJK)
               IF (ROP_G(IJK)*AXZ_V(IJK) > SMALL_NUMBER) THEN
                  B_M(IJK,M) = SQRT(B_M(IJK,M)/(ROP_G(IJK)*AVG_Y_N(ZERO,ONE)*&
                     AXZ_V(IJK)))
               ELSE
                  B_M(IJK,M) = ZERO
               ENDIF
            ENDIF
         ENDIF
      END DO
      RETURN

   END SUBROUTINE ADJUST_A_V_G

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: ADJUST_A_W_g(A_m, B_m, IER)                            C
!  Purpose: Handle the special case of the center coefficient in       C
!  W_g momentum eq. becoming zero.                                     C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

   SUBROUTINE ADJUST_A_W_G(A_M, B_M)

      IMPLICIT NONE
!
!                      Indices
      INTEGER          IJK, IJKT, IJKM
!
!                      Phase index
      INTEGER          M
!
!                      Septadiagonal matrix A_m
      DOUBLE PRECISION A_m(DIMENSION_3, -3:3, 0:DIMENSION_M)
!
!                      Vector b_m
      DOUBLE PRECISION B_m(DIMENSION_3, 0:DIMENSION_M)
!!-----------------------------------------------

      M = 0
      IF (.NOT.MOMENTUM_Z_EQ(0)) RETURN
!
!!!!$omp$ parallel do private(IJK,IJKT,IJKM)
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
               IF (ROP_G(IJKT)*AXY_W(IJK) > SMALL_NUMBER) THEN
                  B_M(IJK,M) = SQRT((-B_M(IJK,M)/(ROP_G(IJKT)*AVG_Z_T(ONE,ZERO)&
                     *AXY_W(IJK))))
               ELSE
                  B_M(IJK,M) = ZERO
               ENDIF
            ELSE IF (B_M(IJK,M) > ZERO) THEN
               IJKM = KM_OF(IJK)
               IF (ROP_G(IJK)*AXY_W(IJKM) > SMALL_NUMBER) THEN
                  B_M(IJK,M) = SQRT(B_M(IJK,M)/(ROP_G(IJK)*AVG_Z_T(ZERO,ONE)*&
                     AXY_W(IJKM)))
               ELSE
                  B_M(IJK,M) = ZERO
               ENDIF
            ENDIF
         ENDIF
      END DO
      RETURN

   END SUBROUTINE ADJUST_A_W_G

END MODULE ADJUST_A_G
