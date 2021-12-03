MODULE UNDER_RELAX_MOD
CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: UNDER_RELAX_S(Var, A_m, B_m, M, UR, IER)               C
!  Purpose: Underrelax scalar equations                               C
!                                                                      C
!                                                                      C
!  Author: M. Syamlal                                 Date: 24-MAY-96  C
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
!
   SUBROUTINE UNDER_RELAX_S(VAR, A_M, B_M, M, UR)

!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE param
      USE param1
      USE geometry
      USE indices
      USE compar
      USE sendrecv
      USE functions

      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
!
!                      phase index
      INTEGER          M
!
!                      index
      INTEGER          IJK
!
!                      Septadiagonal matrix A_m
      DOUBLE PRECISION A_m(DIMENSION_3, -3:3, 0:DIMENSION_M)
!
!                      Vector b_m
      DOUBLE PRECISION B_m(DIMENSION_3, 0:DIMENSION_M)
!
!                      Variable
      DOUBLE PRECISION Var(DIMENSION_3)
!
!                      Underrelaxation factor
      DOUBLE PRECISION UR
!
!                      Functions of underrelaxation factor
      DOUBLE PRECISION f1, f2
!
!                      center coefficient
      DOUBLE PRECISION ap

      F1 = ONE/UR
      F2 = F1 - ONE
!
!!!$omp    parallel do private(IJK,AP)
      DO IJK = ijkstart3, ijkend3
         IF (FLUID_AT(IJK)) THEN
            AP = A_M(IJK,0,M)
            IF (AP /= (-ONE)) THEN
               A_M(IJK,0,M) = AP*F1
               B_M(IJK,M) = B_M(IJK,M) + AP*VAR(IJK)*F2
            ENDIF
         ENDIF
      END DO
      RETURN
   END SUBROUTINE UNDER_RELAX_S
!
!
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: UNDER_RELAX_U(Var, A_m, B_m, M, UR, IER)               C
!  Purpose: Underrelax u-momentum equations                           C
!                                                                      C
!                                                                      C
!  Author: M. Syamlal                                 Date: 24-MAY-96  C
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
!
   SUBROUTINE UNDER_RELAX_U(VAR, A_M, B_M, M, UR)

!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE param
      USE param1
      USE geometry
      USE indices
      USE compar
      USE sendrecv
      USE functions

      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
!
!                      phase index
      INTEGER          M
!
!                      index
      INTEGER          IJK
!
!                      Septadiagonal matrix A_m
      DOUBLE PRECISION A_m(DIMENSION_3, -3:3, 0:DIMENSION_M)
!
!                      Vector b_m
      DOUBLE PRECISION B_m(DIMENSION_3, 0:DIMENSION_M)
!
!                      Variable
      DOUBLE PRECISION Var(DIMENSION_3)
!
!                      Underrelaxation factor
      DOUBLE PRECISION UR
!
!                      Functions of underrelaxation factor
      DOUBLE PRECISION f1, f2
!
!                      center coefficient
      DOUBLE PRECISION ap

      F1 = ONE/UR
      F2 = F1 - ONE
!
!!!$omp    parallel do private(IJK,AP)
      DO IJK = ijkstart3, ijkend3
         IF (FLOW_AT_E(IJK)) THEN
            AP = A_M(IJK,0,M)
            IF (AP /= (-ONE)) THEN
               A_M(IJK,0,M) = AP*F1
               B_M(IJK,M) = B_M(IJK,M) + AP*VAR(IJK)*F2
            ENDIF
         ENDIF
      END DO
      RETURN
   END SUBROUTINE UNDER_RELAX_U
!
!
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: UNDER_RELAX_V(Var, A_m, B_m, M, UR, IER)               C
!  Purpose: Underrelax v-momentum equations                           C
!                                                                      C
!                                                                      C
!  Author: M. Syamlal                                 Date: 24-MAY-96  C
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
!
   SUBROUTINE UNDER_RELAX_V(VAR, A_M, B_M, M, UR)

!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE param
      USE param1
      USE geometry
      USE indices
      USE compar
      USE sendrecv
      USE functions

      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

!                      phase index
      INTEGER          M
!
!                      index
      INTEGER          IJK
!
!                      Septadiagonal matrix A_m
      DOUBLE PRECISION A_m(DIMENSION_3, -3:3, 0:DIMENSION_M)
!
!                      Vector b_m
      DOUBLE PRECISION B_m(DIMENSION_3, 0:DIMENSION_M)
!
!                      Variable
      DOUBLE PRECISION Var(DIMENSION_3)
!
!                      Underrelaxation factor
      DOUBLE PRECISION UR
!
!                      Functions of underrelaxation factor
      DOUBLE PRECISION f1, f2
!
!                      center coefficient
      DOUBLE PRECISION ap

      F1 = ONE/UR
      F2 = F1 - ONE
!
!!!$omp    parallel do private(IJK,AP)
      DO IJK = ijkstart3, ijkend3
         IF (FLOW_AT_N(IJK)) THEN
            AP = A_M(IJK,0,M)
            IF (AP /= (-ONE)) THEN
               A_M(IJK,0,M) = AP*F1
               B_M(IJK,M) = B_M(IJK,M) + AP*VAR(IJK)*F2
            ENDIF
         ENDIF
      END DO
      RETURN
   END SUBROUTINE UNDER_RELAX_V
!
!
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: UNDER_RELAX_W(Var, A_m, B_m, M, UR, IER)               C
!  Purpose: Underrelax w-momentum equations                           C
!                                                                      C
!                                                                      C
!  Author: M. Syamlal                                 Date: 24-MAY-96  C
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
!
   SUBROUTINE UNDER_RELAX_W(VAR, A_M, B_M, M, UR)

!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE param
      USE param1
      USE geometry
      USE indices
      USE compar
      USE sendrecv
      USE functions

      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------

!                      phase index
      INTEGER          M
!
!                      index
      INTEGER          IJK
!
!                      Septadiagonal matrix A_m
      DOUBLE PRECISION A_m(DIMENSION_3, -3:3, 0:DIMENSION_M)
!
!                      Vector b_m
      DOUBLE PRECISION B_m(DIMENSION_3, 0:DIMENSION_M)
!
!                      Variable
      DOUBLE PRECISION Var(DIMENSION_3)
!
!                      Underrelaxation factor
      DOUBLE PRECISION UR
!
!                      Functions of underrelaxation factor
      DOUBLE PRECISION f1, f2
!
!                      center coefficient
      DOUBLE PRECISION ap

      F1 = ONE/UR
      F2 = F1 - ONE
!
!!!$omp    parallel do private(IJK,AP)
      DO IJK = ijkstart3, ijkend3
         IF (FLOW_AT_T(IJK)) THEN
            AP = A_M(IJK,0,M)
            IF (AP /= (-ONE)) THEN
               A_M(IJK,0,M) = AP*F1
               B_M(IJK,M) = B_M(IJK,M) + AP*VAR(IJK)*F2
            ENDIF
         ENDIF
      END DO
      RETURN
   END SUBROUTINE UNDER_RELAX_W

END MODULE UNDER_RELAX_MOD
