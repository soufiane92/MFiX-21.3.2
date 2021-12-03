#include "error.inc"

MODULE INTERSECT_MOD

   USE compar, ONLY: ijkstart3, ijkend3
   USE compar, ONLY: istart3, iend3
   USE compar, ONLY: jstart3, jend3
   USE compar, ONLY: kstart3, kend3
   USE cutcell
   USE dmp_cartesian, only: send_receive_1d_logical
   USE error_manager
   USE eval_f_mod, only: eval_f
   USE functions
   USE geometry
   USE get_connectivity_mod, only: get_cell_node_coordinates
   USE get_stl_data_mod, only: eval_stl_fct_at
   USE get_stl_data_mod, only: is_point_inside_facet, intersect_line_with_facet
   USE indices, ONLY: i_of, j_of, k_of
   USE param, only: dimension_3
   USE param1, only: zero, half, one, undefined
   USE polygon, ONLY: n_polygon
   USE quadric, only: tol_f, itermax_int
   USE resize, only: integer_grow2
   USE sendrecv, only: send_recv

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: intersect_line                                         C
!  Purpose: Finds the intersection between the quadric surface ,       C
!           and the line (xa,ya,za) and (xb,yb,zb).                    C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE INTERSECT_LINE(METHOD,xa,ya,za,xb,yb,zb,Q_ID,INTERSECT_FLAG,xc,yc,zc)

      IMPLICIT NONE

      DOUBLE PRECISION:: x1,y1,z1,x2,y2,z2,x3,y3,z3
      DOUBLE PRECISION:: xa,ya,za,xb,yb,zb,xc,yc,zc
      INTEGER :: Q_ID,niter
      DOUBLE PRECISION :: x_intersection
      DOUBLE PRECISION :: f1,f2,f3,fa,fb
      DOUBLE PRECISION :: t1,t2,t3
      LOGICAL :: CLIP_FLAG,CLIP_FLAG1,CLIP_FLAG2,CLIP_FLAG3,INTERSECT_FLAG
      CHARACTER (LEN=7) ::METHOD

      x1 = xa    ! Initial guesses
      y1 = ya
      z1 = za
      t1 = ZERO

      x2 = xb
      y2 = yb
      z2 = zb
      t2 = ONE

      CALL EVAL_F(METHOD,x1,y1,z1,Q_ID,f1,CLIP_FLAG1)
      CALL EVAL_F(METHOD,x2,y2,z2,Q_ID,f2,CLIP_FLAG2)

!======================================================================
!  The line from (x1,y1,z1) and (x2,y2,z2) is parametrized
!  from t1 = ZERO to t2 = ONE
!======================================================================

      niter = 1

      CLIP_FLAG = (CLIP_FLAG1).AND.(CLIP_FLAG2)

      if(DABS(f1)<TOL_F) then  ! ignore intersection at corner
         xc = UNDEFINED
         yc = UNDEFINED
         zc = UNDEFINED
         INTERSECT_FLAG = .FALSE.
      elseif(DABS(f2)<TOL_F) then ! ignore intersection at corner
         xc = UNDEFINED
         yc = UNDEFINED
         zc = UNDEFINED
         INTERSECT_FLAG = .FALSE.
      elseif(f1*f2 < ZERO) then
         niter = 0
         f3 = 2.0d0*TOL_F
         do while (   (abs(f3) > TOL_F)   .AND.   (niter<ITERMAX_INT)       )

            t3 = t1 - f1*(t2-t1)/(f2-f1)

            x3 = x1 + t3 * (x2 - x1)
            y3 = y1 + t3 * (y2 - y1)
            z3 = z1 + t3 * (z2 - z1)

            CALL EVAL_F(METHOD,x3,y3,z3,Q_ID,f3,CLIP_FLAG3)
            if(f1*f3<0) then
               t2 = t3
               f2 = f3
            else
               t1 = t3
               f1 = f3
            endif
            niter = niter + 1

         end do
         if (niter < ITERMAX_INT) then
            xc = x3
            yc = y3
            zc = z3
            INTERSECT_FLAG = .TRUE.
         else
            CALL EVAL_F(METHOD,xa,ya,za,Q_ID,fa,CLIP_FLAG1)
            CALL EVAL_F(METHOD,xb,yb,zb,Q_ID,fb,CLIP_FLAG1)
            WRITE(ERR_MSG,*)'   Subroutine intersect_line:'//&
               'Unable to find the intersection of quadric:',Q_ID,''//&
               'between (x1,y1,z1)= ', xa,ya,za,''//&
               '   and  (x2,y2,z2)= ', xb,yb,zb,''//&
               'f(x1,y1,z1) = ', fa,''//&
               'f(x2,y2,z2) = ', fb,''//&
               'Current Location (x3,y3,z3)= ', x3,y3,z3,''//&
               'Current value of abs(f) = ', DABS(f3),''//&
               'Tolerance = ', TOL_F,''//&
               'Maximum number of iterations = ', ITERMAX_INT,''//&
               'Please increase the intersection tolerance, '//&
               'or the maximum number of iterations, and try again.'//&
               'MFiX will exit now.'
            call log_error()
            x_intersection = UNDEFINED
            INTERSECT_FLAG = .FALSE.

         endif
      else
         xc = UNDEFINED
         yc = UNDEFINED
         zc = UNDEFINED
         INTERSECT_FLAG = .FALSE.
      endif

      RETURN

   END SUBROUTINE INTERSECT_LINE

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: INTERSECT                                              C
!  Purpose: Intersects quadric with grid                               C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE INTERSECT(IJK,TYPE_OF_CELL,Xi,Yi,Zi)

      IMPLICIT NONE

      CHARACTER (LEN=*) :: TYPE_OF_CELL
      INTEGER :: IJK,I,J,K,Q_ID,N_int_x,N_int_y,N_int_z,N_USR
      DOUBLE PRECISION :: xa,ya,za,xb,yb,zb,xc,yc,zc
      DOUBLE PRECISION :: Xi,Yi,Zi,Xc_backup,Yc_backup,Zc_backup
      LOGICAL :: INTERSECT_FLAG

      Xi = UNDEFINED
      Yi = UNDEFINED
      Zi = UNDEFINED

!======================================================================
!  Get coordinates of eight nodes
!======================================================================

      CALL GET_CELL_NODE_COORDINATES(IJK,TYPE_OF_CELL)

!======================================================================
!  Intersection with Edge 7 (node 7-8, Face North-Top):
!======================================================================
      xa = X_NODE(7)
      ya = Y_NODE(7)
      za = Z_NODE(7)

      xb = X_NODE(8)
      yb = Y_NODE(8)
      zb = Z_NODE(8)

      N_int_x = 0
      INTERSECT_X(IJK) = .FALSE.
      Q_ID = 1
      CALL INTERSECT_LINE('QUADRIC',xa,ya,za,xb,yb,zb,Q_ID,INTERSECT_FLAG,xc,yc,zc)
      IF ((INTERSECT_FLAG).AND.(xc/=Xi)) THEN
         N_int_x = N_int_x + 1
         INTERSECT_X(IJK) = .TRUE.
         xc_backup = Xi
         Xi = xc
      ENDIF

      IF(N_int_x /= 1) THEN
         Xi = UNDEFINED
         INTERSECT_X(IJK) = .FALSE.
      ENDIF

      DO Q_ID = 1, N_POLYGON
         CALL INTERSECT_LINE('POLYGON',xa,ya,za,xb,yb,zb,Q_ID,INTERSECT_X(IJK),xc,yc,zc)
         IF(INTERSECT_X(IJK)) Xi = xc
      ENDDO

      DO N_USR= 1, N_USR_DEF
         CALL INTERSECT_LINE('USR_DEF',xa,ya,za,xb,yb,zb,Q_ID,INTERSECT_X(IJK),xc,yc,zc)
         IF(INTERSECT_X(IJK)) Xi = xc
      ENDDO

!      IF(USE_STL) THEN
!         CALL INTERSECT_LINE_WITH_STL(xa,ya,za,xb,yb,zb,INTERSECT_X(IJK),xc,yc,zc)
!         IF(INTERSECT_X(IJK)) Xi = xc
!      ENDIF

      IF(TYPE_OF_CELL=='U_MOMENTUM') THEN
         IF(SNAP(IJK)) THEN
            INTERSECT_X(IJK) = .TRUE.
            I = I_OF(IJK)
            Xi = XG_E(I)
         ENDIF
      ENDIF

!======================================================================
!  Intersection with Edge 6 (node 6-8, Face East-Top):
!======================================================================
      xa = X_NODE(6)
      ya = Y_NODE(6)
      za = Z_NODE(6)

      N_int_y = 0
      INTERSECT_Y(IJK) = .FALSE.
      Q_ID = 1
      CALL INTERSECT_LINE('QUADRIC',xa,ya,za,xb,yb,zb,Q_ID,INTERSECT_FLAG,xc,yc,zc)
      IF ((INTERSECT_FLAG).AND.(yc/=Yi)) THEN
         N_int_y = N_int_y + 1
         INTERSECT_Y(IJK) = .TRUE.
         yc_backup = Yi
         Yi = yc
      ENDIF

      IF(N_int_y /= 1) THEN
         Yi = UNDEFINED
         INTERSECT_Y(IJK) = .FALSE.
      ENDIF

      DO Q_ID = 1, N_POLYGON
         CALL INTERSECT_LINE('POLYGON',xa,ya,za,xb,yb,zb,Q_ID,INTERSECT_Y(IJK),xc,yc,zc)
         IF(INTERSECT_Y(IJK)) Yi = yc
      ENDDO

      DO N_USR= 1, N_USR_DEF
         CALL INTERSECT_LINE('USR_DEF',xa,ya,za,xb,yb,zb,Q_ID,INTERSECT_Y(IJK),xc,yc,zc)
         IF(INTERSECT_Y(IJK)) Yi = yc
      ENDDO

!      IF(USE_STL) THEN
!         CALL INTERSECT_LINE_WITH_STL(xa,ya,za,xb,yb,zb,INTERSECT_Y(IJK),xc,yc,zc)
!         IF(INTERSECT_Y(IJK)) Yi = yc
!      ENDIF

      IF(TYPE_OF_CELL=='V_MOMENTUM') THEN
         IF(SNAP(IJK)) THEN
            INTERSECT_Y(IJK) = .TRUE.
            J = J_OF(IJK)
            Yi = YG_N(J)
         ENDIF
      ENDIF

      IF(DO_K) THEN
!======================================================================
!  Intersection with Edge 11 (node 4-8, Face East-North):
!======================================================================
         xa = X_NODE(4)
         ya = Y_NODE(4)
         za = Z_NODE(4)

         N_int_z = 0
         INTERSECT_Z(IJK) = .FALSE.
         Q_ID = 1
         CALL INTERSECT_LINE('QUADRIC',xa,ya,za,xb,yb,zb,Q_ID,INTERSECT_FLAG,xc,yc,zc)
         IF ((INTERSECT_FLAG).AND.(zc/=Zi)) THEN
            N_int_z = N_int_z + 1
            INTERSECT_Z(IJK) = .TRUE.
            zc_backup = Zi
            Zi = zc
         ENDIF

         IF(N_int_z /= 1) THEN
            Zi = UNDEFINED
            INTERSECT_Z(IJK) = .FALSE.
         ENDIF

         DO Q_ID = 1, N_POLYGON
            CALL INTERSECT_LINE('POLYGON',xa,ya,za,xb,yb,zb,Q_ID,INTERSECT_Z(IJK),xc,yc,zc)
            IF(INTERSECT_Z(IJK)) Zi = zc
         ENDDO

         DO N_USR= 1, N_USR_DEF
            CALL INTERSECT_LINE('USR_DEF',xa,ya,za,xb,yb,zb,Q_ID,INTERSECT_Z(IJK),xc,yc,zc)
            IF(INTERSECT_Z(IJK)) Zi = zc
         ENDDO

!      IF(USE_STL) THEN
!         CALL INTERSECT_LINE_WITH_STL(xa,ya,za,xb,yb,zb,INTERSECT_Z(IJK),xc,yc,zc)
!         IF(INTERSECT_Z(IJK)) Zi = zc
!      ENDIF

         IF(TYPE_OF_CELL=='W_MOMENTUM') THEN
            IF(SNAP(IJK)) THEN
               INTERSECT_Z(IJK) = .TRUE.
               K = K_OF(IJK)
               Zi = ZG_T(K)
            ENDIF
         ENDIF

      ENDIF

      IF(INTERSECT_X(IJK)) THEN
         POTENTIAL_CUT_CELL_AT(IJK) = .TRUE.
         POTENTIAL_CUT_CELL_AT(EAST_OF(IJK)) = .TRUE.
      ENDIF

      IF(INTERSECT_Y(IJK)) THEN
         POTENTIAL_CUT_CELL_AT(IJK) = .TRUE.
         POTENTIAL_CUT_CELL_AT(NORTH_OF(IJK)) = .TRUE.
      ENDIF

      IF(INTERSECT_Z(IJK)) THEN
         POTENTIAL_CUT_CELL_AT(IJK) = .TRUE.
         POTENTIAL_CUT_CELL_AT(TOP_OF(IJK)) = .TRUE.
      ENDIF

      RETURN

   END SUBROUTINE INTERSECT


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: CLEAN_INTERSECT                                        C
!  Purpose: Remove Intersection flags in preparation of small cell     C
!           removal                                                    C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE CLEAN_INTERSECT(IJK,TYPE_OF_CELL,Xi,Yi,Zi)

      IMPLICIT NONE

      CHARACTER (LEN=*) :: TYPE_OF_CELL
      INTEGER :: IJK,I,J,K,IM,JM,KM,IP,JP,KP
      INTEGER :: BCID
      INTEGER :: IMJK,IPJK,IJMK,IJPK,IJKM,IJKP,IMJPK,IMJKP,IPJMK,IJMKP,IPJKM,IJPKM
      DOUBLE PRECISION :: xa,ya,za,xb,yb,zb
      DOUBLE PRECISION :: Xi,Yi,Zi
      DOUBLE PRECISION :: DFC,DFC_MAX,F4,F6,F7,F8
      LOGICAL :: CLIP_FLAG,CAD,F_TEST

! When inputing geometry from CAD (STL or MSH file), the snapping procedure is
! dependent on the value of F at the cell corners
! For other geometry inputs (say quadrics), This is not needed, and the value
! of F_TEST is set to .TRUE. here
      CAD = USE_MSH.OR.USE_STL
      F_TEST = .TRUE.

!======================================================================
!  Get coordinates of eight nodes
!======================================================================

      CALL GET_CELL_NODE_COORDINATES(IJK,TYPE_OF_CELL)

      I = I_OF(IJK)
      J = J_OF(IJK)
      K = K_OF(IJK)

      IM = I - 1
      JM = J - 1
      KM = K - 1

      IP = I + 1
      JP = J + 1
      KP = K + 1

      IMJK = FUNIJK(IM,J,K)
      IPJK = FUNIJK(IP,J,K)
      IJMK = FUNIJK(I,JM,K)
      IJPK = FUNIJK(I,JP,K)
      IJKM = FUNIJK(I,J,KM)
      IJKP = FUNIJK(I,J,KP)

      IMJPK = FUNIJK(IM,JP,K)
      IMJKP = FUNIJK(IM,J,KP)

      IPJMK = FUNIJK(IP,JM,K)
      IJMKP = FUNIJK(I,JM,KP)

      IPJKM = FUNIJK(IP,J,KM)
      IJPKM = FUNIJK(I,JP,KM)

      IF(IMJK<1.OR.IMJK>DIMENSION_3) IMJK = IJK
      IF(IPJK<1.OR.IPJK>DIMENSION_3) IPJK = IJK
      IF(IJMK<1.OR.IJMK>DIMENSION_3) IJMK = IJK
      IF(IJPK<1.OR.IJPK>DIMENSION_3) IJPK = IJK
      IF(IJKM<1.OR.IJKM>DIMENSION_3) IJKM = IJK
      IF(IJKP<1.OR.IJKP>DIMENSION_3) IJKP = IJK

      IF(IMJPK<1.OR.IMJPK>DIMENSION_3) IMJPK = IJK
      IF(IMJKP<1.OR.IMJKP>DIMENSION_3) IMJKP = IJK

      IF(IPJMK<1.OR.IPJMK>DIMENSION_3) IPJMK = IJK
      IF(IJMKP<1.OR.IJMKP>DIMENSION_3) IJMKP = IJK

      IF(IPJKM<1.OR.IPJKM>DIMENSION_3) IPJKM = IJK
      IF(IJPKM<1.OR.IJPKM>DIMENSION_3) IJPKM = IJK

!======================================================================
!  Clean Intersection with Edge 7 (node 7-8, Face North-Top):
!======================================================================

      xa = X_NODE(7)
      ya = Y_NODE(7)
      za = Z_NODE(7)

      xb = X_NODE(8)
      yb = Y_NODE(8)
      zb = Z_NODE(8)

      DFC_MAX = TOL_SNAP(1) * DSQRT((xb-xa)**2+(yb-ya)**2+(zb-za)**2)  ! MAXIMUM DISTANCE FROM CORNER

      IF(INTERSECT_X(IJK)) THEN

         DFC = DABS(Xi-xa) ! DISTANCE FROM CORNER (NODE 7)

         IF(CAD) F_TEST = (F_AT(IMJK)/=ZERO)
         IF(DFC < DFC_MAX.AND.F_TEST) THEN
            IF(PRINT_WARNINGS) THEN
               WRITE(*,*)'MERGING X-INTERSECTION ALONG EDGE 7 ONTO NODE 7'
               WRITE(*,*)'AT IJK,I,J,K=',IJK,I,J,K
            ENDIF

            INTERSECT_X(IJK)  = .FALSE.
            IF(I>=IMIN1) THEN
               INTERSECT_X(IMJK) = .FALSE.
               INTERSECT_Y(IMJK)  = .FALSE.
               INTERSECT_Y(IMJPK) = .FALSE.
               IF(DO_K) INTERSECT_Z(IMJK)  = .FALSE.
               IF(DO_K) INTERSECT_Z(IMJKP) = .FALSE.

               SNAP(IMJK) = .TRUE.
            ENDIF

         ENDIF

         DFC = DABS(Xi-xb) ! DISTANCE FROM CORNER (NODE 8)

         IF(CAD) F_TEST = (F_AT(IJK)/=ZERO)
         IF(DFC < DFC_MAX.AND.F_TEST) THEN
            IF(PRINT_WARNINGS) THEN
               WRITE(*,*)'MERGING X-INTERSECTION ALONG EDGE 7 ONTO NODE 8'
               WRITE(*,*)'AT IJK,I,J,K=',IJK,I,J,K
            ENDIF

            INTERSECT_X(IJK)  = .FALSE.
            INTERSECT_Y(IJK)  = .FALSE.
            IF(DO_K) INTERSECT_Z(IJK)  = .FALSE.
            SNAP(IJK) = .TRUE.

            IF(I<=IMAX1) INTERSECT_X(IPJK) = .FALSE.
            IF(J<=JMAX1) INTERSECT_Y(IJPK) = .FALSE.
            IF(DO_K.AND.(K<=KMAX1)) INTERSECT_Z(IJKP) = .FALSE.

         ENDIF

         IF(USE_STL.OR.USE_MSH) THEN
            CALL EVAL_STL_FCT_AT(TYPE_OF_CELL,IJK,7,F7,CLIP_FLAG,BCID)
            CALL EVAL_STL_FCT_AT(TYPE_OF_CELL,IJK,8,F8,CLIP_FLAG,BCID)
            IF(F7*F8>TOL_STL**2) INTERSECT_X(IJK)  = .FALSE.
         ENDIF

      ENDIF

!======================================================================
!  Clean Intersection with Edge 6 (node 6-8, Face East-Top):
!======================================================================

      xa = X_NODE(6)
      ya = Y_NODE(6)
      za = Z_NODE(6)

      DFC_MAX = TOL_SNAP(2) * DSQRT((xb-xa)**2+(yb-ya)**2+(zb-za)**2)  ! MAXIMUM DISTANCE FROM CORNER


      IF(INTERSECT_Y(IJK)) THEN

         DFC = DABS(Yi-ya) ! DISTANCE FROM CORNER (NODE 6)

         IF(CAD) F_TEST = (F_AT(IJMK)/=ZERO)
         IF(DFC < DFC_MAX.AND.F_TEST) THEN
            IF(PRINT_WARNINGS) THEN
               WRITE(*,*)'MERGING Y-INTERSECTION ALONG EDGE 6 ONTO NODE 6'
               WRITE(*,*)'AT IJK,I,J,K=',IJK,I,J,K
            ENDIF

            INTERSECT_Y(IJK)  = .FALSE.
            IF(J>=JMIN1) THEN
               INTERSECT_X(IJMK)  = .FALSE.
               INTERSECT_X(IPJMK) = .FALSE.
               INTERSECT_Y(IJMK) = .FALSE.
               IF(DO_K) INTERSECT_Z(IJMK)  = .FALSE.
               IF(DO_K) INTERSECT_Z(IJMKP) = .FALSE.

               SNAP(IJMK) = .TRUE.
            ENDIF

         ENDIF

         DFC = DABS(Yi-yb) ! DISTANCE FROM CORNER (NODE 8)

         IF(CAD) F_TEST = (F_AT(IJK)/=ZERO)
         IF(DFC < DFC_MAX.AND.F_TEST) THEN
            IF(PRINT_WARNINGS) THEN
               WRITE(*,*)'MERGING Y-INTERSECTION ALONG EDGE 6 ONTO NODE 8'
               WRITE(*,*)'AT IJK,I,J,K=',IJK,I,J,K
            ENDIF

            INTERSECT_X(IJK)  = .FALSE.
            INTERSECT_Y(IJK)  = .FALSE.
            IF(DO_K) INTERSECT_Z(IJK)  = .FALSE.
            SNAP(IJK) = .TRUE.

            IF(I<=IMAX1) INTERSECT_X(IPJK) = .FALSE.
            IF(J<=JMAX1) INTERSECT_Y(IJPK) = .FALSE.
            IF(DO_K.AND.(K<=KMAX1)) INTERSECT_Z(IJKP) = .FALSE.


         ENDIF


         IF(USE_STL.OR.USE_MSH) THEN
            CALL EVAL_STL_FCT_AT(TYPE_OF_CELL,IJK,6,F6,CLIP_FLAG,BCID)
            CALL EVAL_STL_FCT_AT(TYPE_OF_CELL,IJK,8,F8,CLIP_FLAG,BCID)
            IF(F6*F8>TOL_STL**2) INTERSECT_Y(IJK)  = .FALSE.
         ENDIF

      ENDIF


      IF(DO_K) THEN
!======================================================================
!  Intersection with Edge 11 (node 4-8, Face East-North):
!======================================================================

         xa = X_NODE(4)
         ya = Y_NODE(4)
         za = Z_NODE(4)

         DFC_MAX = TOL_SNAP(3) * DSQRT((xb-xa)**2+(yb-ya)**2+(zb-za)**2)  ! MAXIMUM DISTANCE FROM CORNER

         IF(INTERSECT_Z(IJK)) THEN

            DFC = DABS(Zi-Za) ! DISTANCE FROM CORNER (NODE 4)

            IF(CAD) F_TEST = (F_AT(IJKM)/=ZERO)
            IF(DFC < DFC_MAX.AND.F_TEST) THEN
               IF(PRINT_WARNINGS) THEN
                  WRITE(*,*)'MERGING Z-INTERSECTION ALONG EDGE 11 ONTO NODE 4'
                  WRITE(*,*)'AT IJK,I,J,K=',IJK,I,J,K
               ENDIF

               INTERSECT_Z(IJK)  = .FALSE.

               IF(K>=KMIN1) THEN
                  INTERSECT_X(IJKM)  = .FALSE.
                  INTERSECT_X(IPJKM) = .FALSE.
                  INTERSECT_Y(IJKM)  = .FALSE.
                  INTERSECT_Y(IJPKM) = .FALSE.
                  INTERSECT_Z(IJKM) = .FALSE.

                  SNAP(IJKM) = .TRUE.
               ENDIF

            ENDIF

            DFC = DABS(Zi-Zb) ! DISTANCE FROM CORNER (NODE 8)

            IF(CAD) F_TEST = (F_AT(IJK)/=ZERO)
            IF(DFC < DFC_MAX.AND.F_TEST) THEN
               IF(PRINT_WARNINGS) THEN
                  WRITE(*,*)'MERGING Z-INTERSECTION ALONG EDGE 11 ONTO NODE 8'
                  WRITE(*,*)'AT IJK,I,J,K=',IJK,I,J,K
               ENDIF

               INTERSECT_X(IJK)  = .FALSE.
               INTERSECT_Y(IJK)  = .FALSE.
               INTERSECT_Z(IJK)  = .FALSE.
               SNAP(IJK) = .TRUE.
!            F_AT(IJKM) = UNDEFINED
!            IF(F_AT(IJK)/=ZERO) SNAP(IJK) = .TRUE.
!               F_AT(IJK) = ZERO

               IF(I<=IMAX1) INTERSECT_X(IPJK) = .FALSE.
               IF(J<=JMAX1) INTERSECT_Y(IJPK) = .FALSE.
               IF(K<=KMAX1) INTERSECT_Z(IJKP) = .FALSE.

            ENDIF

            IF(USE_STL.OR.USE_MSH) THEN
               CALL EVAL_STL_FCT_AT(TYPE_OF_CELL,IJK,4,F4,CLIP_FLAG,BCID)
               CALL EVAL_STL_FCT_AT(TYPE_OF_CELL,IJK,8,F8,CLIP_FLAG,BCID)
               IF(F4*F8>TOL_STL**2) INTERSECT_Z(IJK)  = .FALSE.
            ENDIF

         ENDIF

      ENDIF

      RETURN

   END SUBROUTINE CLEAN_INTERSECT


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: CLEAN_INTERSECT_SCALAR                                 C
!  Purpose: Remove Intersection flags in preparation of small cell     C
!           removal                                                    C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 04-Dec-14  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE CLEAN_INTERSECT_SCALAR

      IMPLICIT NONE

      INTEGER :: IJK

      call SEND_RECEIVE_1D_LOGICAL(INTERSECT_X,2)
      call SEND_RECEIVE_1D_LOGICAL(INTERSECT_Y,2)
      call SEND_RECEIVE_1D_LOGICAL(INTERSECT_Z,2)
      call send_recv(Xn_int,2)
      call send_recv(Ye_int,2)
      call send_recv(Zt_int,2)

      DO IJK = IJKSTART3, IJKEND3
         CALL SET_SNAP_FLAG(IJK,'SCALAR',Xn_int(IJK),Ye_int(IJK),Zt_int(IJK))
      END DO

      call SEND_RECEIVE_1D_LOGICAL(SNAP,2)
      call SEND_RECEIVE_1D_LOGICAL(INTERSECT_X,2)
      call SEND_RECEIVE_1D_LOGICAL(INTERSECT_Y,2)
      call SEND_RECEIVE_1D_LOGICAL(INTERSECT_Z,2)
      call send_recv(Xn_int,2)
      call send_recv(Ye_int,2)
      call send_recv(Zt_int,2)

      DO IJK = IJKSTART3, IJKEND3
         CALL REMOVE_INTERSECT_FLAG(IJK)
      END DO

      call SEND_RECEIVE_1D_LOGICAL(SNAP,2)
      call SEND_RECEIVE_1D_LOGICAL(INTERSECT_X,2)
      call SEND_RECEIVE_1D_LOGICAL(INTERSECT_Y,2)
      call SEND_RECEIVE_1D_LOGICAL(INTERSECT_Z,2)
      call send_recv(Xn_int,2)
      call send_recv(Ye_int,2)
      call send_recv(Zt_int,2)

      RETURN

   END SUBROUTINE CLEAN_INTERSECT_SCALAR


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: CAD_INTERSECT                                          C
!  Purpose: Intersects CAD (STL file or MSH file) geometry with grid   C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE CAD_INTERSECT(TYPE_OF_CELL,Xint,Yint,Zint)

      IMPLICIT NONE

      CHARACTER (LEN=*) :: TYPE_OF_CELL
      INTEGER :: IJK,I,J,K
      INTEGER :: IM,IP,JM,JP,KM,KP,IMJK,IPJK,IJMK,IJPK,IJKM,IJKP
      INTEGER :: IJPKP,IPJKP,IPJPK
      DOUBLE PRECISION :: xa,ya,za,xb,yb,zb,xc,yc,zc
      LOGICAL :: INTERSECT_FLAG,INSIDE_FACET_a,INSIDE_FACET_b

      DOUBLE PRECISION :: X1,X2,Y1,Y2,Z1,Z2

      DOUBLE PRECISION, DIMENSION(DIMENSION_3) :: Xint,Yint,Zint
      DOUBLE PRECISION, DIMENSION(DIMENSION_3) :: Xint_min,Yint_min,Zint_min
      DOUBLE PRECISION, DIMENSION(DIMENSION_3) :: Xint_max,Yint_max,Zint_max

      INTEGER, DIMENSION(DIMENSION_3) :: NXint,NYint,NZint

      INTEGER :: NN,I1,I2,J1,J2,K1,K2

      DOUBLE PRECISION :: X_OFFSET, Y_OFFSET, Z_OFFSET

      DOUBLE PRECISION, DIMENSION(3) :: N4,N6,N7,N8
      DOUBLE PRECISION :: CURRENT_F
      DOUBLE PRECISION :: TOL_F2


      INTEGER :: N_UNDEFINED, NTOTAL_UNDEFINED,N_PROP
      INTEGER, PARAMETER :: N_PROPMAX=1000
      LOGICAL:: F_FOUND


      TOL_F2 = TOL_F*TOL_F

!      CHARACTER (LEN=3) :: CAD_PROPAGATE_ORDER

      INTERSECT_X = .FALSE.
      INTERSECT_Y = .FALSE.
      INTERSECT_Z = .FALSE.

      Xint = UNDEFINED
      Yint = UNDEFINED
      Zint = UNDEFINED

      Xint_min = UNDEFINED
      Yint_min = UNDEFINED
      Zint_min = UNDEFINED

      Xint_max = -UNDEFINED
      Yint_max = -UNDEFINED
      Zint_max = -UNDEFINED

      NXint = 0
      NYint = 0
      NZint = 0

      F_AT = UNDEFINED

      SELECT CASE (TYPE_OF_CELL)
      CASE('SCALAR')


         X_OFFSET = ZERO
         Y_OFFSET = ZERO
         Z_OFFSET = ZERO

      CASE('U_MOMENTUM')

         X_OFFSET = HALF
         Y_OFFSET = ZERO
         Z_OFFSET = ZERO

      CASE('V_MOMENTUM')

         X_OFFSET = ZERO
         Y_OFFSET = HALF
         Z_OFFSET = ZERO

      CASE('W_MOMENTUM')

         X_OFFSET = ZERO
         Y_OFFSET = ZERO
         Z_OFFSET = HALF


      CASE DEFAULT
         WRITE(ERR_MSG,*)'SUBROUTINE: GET_CELL_NODE_COORDINATES'//&
            'UNKNOWN TYPE OF CELL:',TYPE_OF_CELL//&
            'ACCEPTABLE TYPES ARE:'//&
            'SCALAR'//&
            'U_MOMENTUM'//&
            'V_MOMENTUM'//&
            'W_MOMENTUM'
         call log_error()
      END SELECT


      DO NN = 1,N_FACETS

! Skip iontersection if the STL facet is flush with MFiX domain box. 
! This will provide better mesh
         IF (stl_facet_along_box(NN)) CYCLE

         X1 = MINVAL(VERTEX(1:3,1,NN))
         X2 = MAXVAL(VERTEX(1:3,1,NN))
         Y1 = MINVAL(VERTEX(1:3,2,NN))
         Y2 = MAXVAL(VERTEX(1:3,2,NN))
         Z1 = MINVAL(VERTEX(1:3,3,NN))
         Z2 = MAXVAL(VERTEX(1:3,3,NN))


         I1 = IEND3
         I2 = ISTART3

         IF(X2>=X_MIN-DX(ISTART3).AND.X1<=X_MAX+DX(IEND3)) THEN
            DO I = ISTART3, IEND3
               IP = I+1
               IF(XG_E(I)+X_OFFSET*DX(IP)>=X1-TOL_STL) THEN
                  I1=I
                  EXIT
               ENDIF
            ENDDO

            DO I = IEND3, ISTART3,-1
               IP = I+1
               IF(XG_E(I)-DX(I)+X_OFFSET*DX(IP)<=X2+TOL_STL) THEN
                  I2=I
                  EXIT
               ENDIF
            ENDDO
         ENDIF


         J1 = JEND3
         J2 = JSTART3

         IF(Y2>=Y_MIN-DY(JSTART3).AND.Y1<=Y_MAX+DY(JEND3)) THEN
            DO J = JSTART3, JEND3
               JP = J+1
               IF(YG_N(J)+Y_OFFSET*DY(JP)>=Y1-TOL_STL) THEN
                  J1=J
                  EXIT
               ENDIF
            ENDDO

            DO J = JEND3, JSTART3,-1
               JP=J+1
               IF(YG_N(J)-DY(J)+Y_OFFSET*DY(JP)<=Y2+TOL_STL) THEN
                  J2=J
                  EXIT
               ENDIF
            ENDDO
         ENDIF

         K1 = KEND3
         K2 = KSTART3

         IF(Z2>=Z_MIN-DZ(KSTART3).AND.Z1<=Z_MAX+DZ(KEND3)) THEN
            DO K = KSTART3, KEND3
               KP=K+1

               IF(ZG_T(K)+Z_OFFSET*DZ(KP)>=Z1-TOL_STL) THEN
                  K1=K
                  EXIT
               ENDIF
            ENDDO

            DO K = KEND3, KSTART3,-1
               KP = K+1
               IF(ZG_T(K)-DZ(K)+Z_OFFSET*DZ(KP)<=Z2+TOL_STL) THEN
                  K2=K
                  EXIT
               ENDIF
            ENDDO
         ENDIF




         DO K=K1,K2
            DO J=J1,J2
               DO I=I1,I2

                  IJK = FUNIJK(I,J,K)


                  IM = MAX0(I - 1 ,ISTART3)
                  JM = MAX0(J - 1 ,JSTART3)
                  KM = MAX0(K - 1 ,KSTART3)

                  IP = MIN0(I + 1 ,IEND3)
                  JP = MIN0(J + 1 ,JEND3)
                  KP = MIN0(K + 1 ,KEND3)


                  IMJK = FUNIJK(IM,J,K)
                  IPJK = FUNIJK(IP,J,K)
                  IJMK = FUNIJK(I,JM,K)
                  IJPK = FUNIJK(I,JP,K)
                  IJKM = FUNIJK(I,J,KM)
                  IJKP = FUNIJK(I,J,KP)

                  IJPKP = FUNIJK(I,JP,KP)
                  IPJKP = FUNIJK(IP,J,KP)
                  IPJPK = FUNIJK(IP,JP,K)


                  POTENTIAL_CUT_CELL_AT(IJK) = .TRUE.

!======================================================================
!  Get coordinates of eight nodes
!======================================================================

                  CALL GET_CELL_NODE_COORDINATES(IJK,TYPE_OF_CELL)

!======================================================================
!  Intersection with Edge 7 (node 7-8, Face North-Top):
!======================================================================
                  xa = X_NODE(7)
                  ya = Y_NODE(7)
                  za = Z_NODE(7)

                  xb = X_NODE(8)
                  yb = Y_NODE(8)
                  zb = Z_NODE(8)

! Check if intersection occurs at corners

                  INSIDE_FACET_a = .FALSE.
                  INSIDE_FACET_b = .FALSE.

                  CALL IS_POINT_INSIDE_FACET(xa,ya,za,NN,INSIDE_FACET_a)

                  IF(INSIDE_FACET_a) THEN   ! corner intersection at node 7

                     F_AT(IMJK) = ZERO

                     IF(TRIM(TYPE_OF_CELL).eq.'SCALAR')  CALL ADD_FACET_AND_SET_BC_ID(IJK,NN)

                  ENDIF


                  CALL IS_POINT_INSIDE_FACET(xb,yb,zb,NN,INSIDE_FACET_b)

                  IF(INSIDE_FACET_b) THEN   ! corner intersection at node 8

                     F_AT(IJK) = ZERO

                     IF(TRIM(TYPE_OF_CELL).eq.'SCALAR') CALL ADD_FACET_AND_SET_BC_ID(IJK,NN)

                  ENDIF



! Check intersection within line 7-8, excluding corners

                  INTERSECT_FLAG = .FALSE.

                  IF(.NOT.(INSIDE_FACET_a.OR.INSIDE_FACET_b)) THEN
                     CALL INTERSECT_LINE_WITH_FACET(xa,ya,za,xb,yb,zb,NN,INTERSECT_FLAG,xc,yc,zc)
                  ENDIF

                  IF(INTERSECT_FLAG) THEN
                     NXint(IJK) = NXint(IJK) + 1
                     IF(xc<Xint_min(IJK)-TOL_STL) THEN
                        Xint_min(IJK) = xc
                        IF(DABS(F_AT(IMJK))>TOL_F) THEN
                           N7(1) = xa-xc
                           N7(2) = ya-yc
                           N7(3) = za-zc
                           F_AT(IMJK) = -DOT_PRODUCT(N7,NORM_FACE(:,NN))
                        ENDIF
                     ENDIF

                     IF(xc>Xint_max(IJK)+TOL_STL) THEN
                        Xint_max(IJK) = xc
                        IF(DABS(F_AT(IJK))>TOL_F) THEN
                           N8(1) = xb-xc
                           N8(2) = yb-yc
                           N8(3) = zb-zc
                           F_AT(IJK) = -DOT_PRODUCT(N8,NORM_FACE(:,NN))
                        ENDIF
                     ENDIF


                     IF(TRIM(TYPE_OF_CELL).eq.'SCALAR') CALL ADD_FACET_AND_SET_BC_ID(IJK,NN)
                     IF(JP<=J2.AND.TRIM(TYPE_OF_CELL).eq.'SCALAR') CALL ADD_FACET_AND_SET_BC_ID(IJPK,NN)
                     IF(KP<=K2.AND.TRIM(TYPE_OF_CELL).eq.'SCALAR') CALL ADD_FACET_AND_SET_BC_ID(IJKP,NN)
                     IF(JP<=J2.AND.KP<=K2.AND.TRIM(TYPE_OF_CELL).eq.'SCALAR') CALL ADD_FACET_AND_SET_BC_ID(IJPKP,NN)
                  ENDIF

                  IF(TYPE_OF_CELL=='U_MOMENTUM') THEN
                     IF(SNAP(IJK)) THEN
                        INTERSECT_X(IJK) = .TRUE.
                        Xn_int(IJK) = XG_E(I)
                     ENDIF
                  ENDIF

!======================================================================
!  Intersection with Edge 6 (node 6-8, Face East-Top):
!======================================================================
                  xa = X_NODE(6)
                  ya = Y_NODE(6)
                  za = Z_NODE(6)

! Check if intersection occurs at corners
                  INSIDE_FACET_a = .FALSE.

                  CALL IS_POINT_INSIDE_FACET(xa,ya,za,NN,INSIDE_FACET_a)

                  IF(INSIDE_FACET_a) THEN   ! corner intersection at node 6

                     F_AT(IJMK) = ZERO

                     IF(TRIM(TYPE_OF_CELL).eq.'SCALAR') CALL ADD_FACET_AND_SET_BC_ID(IJK,NN)

                  ENDIF


! Check intersection within line 6-8, excluding corners

                  INTERSECT_FLAG = .FALSE.

                  IF(.NOT.(INSIDE_FACET_a.OR.INSIDE_FACET_b)) THEN
                     CALL INTERSECT_LINE_WITH_FACET(xa,ya,za,xb,yb,zb,NN,INTERSECT_FLAG,xc,yc,zc)
                  ENDIF


                  IF(INTERSECT_FLAG) THEN
                     NYint(IJK) = NYint(IJK) + 1
                     IF(yc<Yint_min(IJK)-TOL_STL) THEN
                        Yint_min(IJK) = yc
                        IF(DABS(F_AT(IJMK))>TOL_F) THEN
                           N6(1) = xa-xc
                           N6(2) = ya-yc
                           N6(3) = za-zc
                           F_AT(IJMK) = -DOT_PRODUCT(N6,NORM_FACE(:,NN))
                        ENDIF
                     ENDIF

                     IF(yc>Yint_max(IJK)+TOL_STL) THEN
                        Yint_max(IJK) = yc
                        IF(DABS(F_AT(IJK))>TOL_F) THEN
                           N8(1) = xb-xc
                           N8(2) = yb-yc
                           N8(3) = zb-zc
                           F_AT(IJK) = -DOT_PRODUCT(N8,NORM_FACE(:,NN))
                        ENDIF
                     ENDIF



                     IF(TRIM(TYPE_OF_CELL).eq.'SCALAR') CALL ADD_FACET_AND_SET_BC_ID(IJK,NN)
                     IF(IP<=I2.AND.TRIM(TYPE_OF_CELL).eq.'SCALAR') CALL ADD_FACET_AND_SET_BC_ID(IPJK,NN)
                     IF(KP<=K2.AND.TRIM(TYPE_OF_CELL).eq.'SCALAR') CALL ADD_FACET_AND_SET_BC_ID(IJKP,NN)
                     IF(IP<=I2.AND.KP<=K2.AND.TRIM(TYPE_OF_CELL).eq.'SCALAR') CALL ADD_FACET_AND_SET_BC_ID(IPJKP,NN)

                  ENDIF

                  IF(TYPE_OF_CELL=='V_MOMENTUM') THEN
                     IF(SNAP(IJK)) THEN
                        INTERSECT_Y(IJK) = .TRUE.
                        Ye_int(IJK) = YG_N(J)
                     ENDIF
                  ENDIF

                  IF(DO_K) THEN
!======================================================================
!  Intersection with Edge 11 (node 4-8, Face East-North):
!======================================================================
                     xa = X_NODE(4)
                     ya = Y_NODE(4)
                     za = Z_NODE(4)

! Check if intersection occurs at corners
                     INSIDE_FACET_a = .FALSE.

                     CALL IS_POINT_INSIDE_FACET(xa,ya,za,NN,INSIDE_FACET_a)

                     IF(INSIDE_FACET_a) THEN   ! corner intersection at node 4

                        F_AT(IJKM) = ZERO

                        IF(TRIM(TYPE_OF_CELL).eq.'SCALAR') CALL ADD_FACET_AND_SET_BC_ID(IJK,NN)

                     ENDIF

! Check intersection within line 4-8, excluding corners

                     INTERSECT_FLAG = .FALSE.

                     IF(.NOT.(INSIDE_FACET_a.OR.INSIDE_FACET_b)) THEN
                        CALL INTERSECT_LINE_WITH_FACET(xa,ya,za,xb,yb,zb,NN,INTERSECT_FLAG,xc,yc,zc)
                     ENDIF

                     IF(INTERSECT_FLAG) THEN
                        NZint(IJK) = NZint(IJK) + 1
                        IF(zc<Zint_min(IJK)-TOL_STL) THEN
                           Zint_min(IJK) = zc
                           IF(DABS(F_AT(IJKM))>TOL_F) THEN
                              N4(1) = xa-xc
                              N4(2) = ya-yc
                              N4(3) = za-zc
                              F_AT(IJKM) = -DOT_PRODUCT(N4,NORM_FACE(:,NN))
                           ENDIF
                        ENDIF

                        IF(zc>Zint_max(IJK)+TOL_STL) THEN
                           Zint_max(IJK) = zc
                           IF(DABS(F_AT(IJK))>TOL_F) THEN
                              N8(1) = xb-xc
                              N8(2) = yb-yc
                              N8(3) = zb-zc
                              F_AT(IJK) = -DOT_PRODUCT(N8,NORM_FACE(:,NN))
                           ENDIF
                        ENDIF


                        IF(TRIM(TYPE_OF_CELL).eq.'SCALAR') CALL ADD_FACET_AND_SET_BC_ID(IJK,NN)
                        IF(IP<=I2.AND.TRIM(TYPE_OF_CELL).eq.'SCALAR') CALL ADD_FACET_AND_SET_BC_ID(IPJK,NN)
                        IF(JP<=J2.AND.TRIM(TYPE_OF_CELL).eq.'SCALAR') CALL ADD_FACET_AND_SET_BC_ID(IJPK,NN)
                        IF(IP<=I2.AND.JP<=J2.AND.TRIM(TYPE_OF_CELL).eq.'SCALAR') CALL ADD_FACET_AND_SET_BC_ID(IPJPK,NN)


                     ENDIF


                     IF(TYPE_OF_CELL=='W_MOMENTUM') THEN
                        IF(SNAP(IJK)) THEN
                           INTERSECT_Z(IJK) = .TRUE.
                           Zt_int(IJK) = ZG_T(K)
                        ENDIF
                     ENDIF

                  ENDIF


               ENDDO  ! I loop
            ENDDO  ! J loop
         ENDDO  ! K loop


      ENDDO  ! Loop over facets

      CURRENT_F = UNDEFINED

! Overwrite small values to set them to zero

      DO IJK = IJKSTART3, IJKEND3
         IF(DABS(F_AT(IJK))<TOL_STL) THEN
            F_AT(IJK)=ZERO
         ENDIF
      END DO


! Reset intersection flags
      DO IJK = IJKSTART3, IJKEND3

         I = I_OF(IJK)
         J = J_OF(IJK)
         K = K_OF(IJK)

         IM = MAX0(I - 1 ,ISTART3)
         JM = MAX0(J - 1 ,JSTART3)
         KM = MAX0(K - 1 ,KSTART3)

         IMJK = FUNIJK(IM,J,K)
         IJMK = FUNIJK(I,JM,K)
         IJKM = FUNIJK(I,J,KM)


         IF(Xint_min(IJK)<UNDEFINED.AND.Xint_max(IJK)>-UNDEFINED) THEN
            IF(F_AT(IJK)*F_AT(IMJK)>TOL_F2) THEN
               INTERSECT_X(IJK) = .FALSE.
               Xint(IJK) = UNDEFINED
            ELSEIF(F_AT(IJK)*F_AT(IMJK)<-TOL_F2) THEN
               INTERSECT_X(IJK) = .TRUE.
               Xint(IJK) = HALF * (Xint_min(IJK)+Xint_max(IJK))
            ENDIF
         ENDIF

         IF(Yint_min(IJK)<UNDEFINED.AND.Yint_max(IJK)>-UNDEFINED) THEN
            IF(F_AT(IJK)*F_AT(IJMK)>TOL_F2) THEN
               INTERSECT_Y(IJK) = .FALSE.
               Yint(IJK) = UNDEFINED
            ELSEIF(F_AT(IJK)*F_AT(IJMK)<-TOL_F2) THEN
               INTERSECT_Y(IJK) = .TRUE.
               Yint(IJK) = HALF * (Yint_min(IJK)+Yint_max(IJK))
            ENDIF
         ENDIF

         IF(Zint_min(IJK)<UNDEFINED.AND.Zint_max(IJK)>-UNDEFINED) THEN
            IF(F_AT(IJK)*F_AT(IJKM)>TOL_F2) THEN
               INTERSECT_Z(IJK) = .FALSE.
               Zint(IJK) = UNDEFINED
            ELSEIF(F_AT(IJK)*F_AT(IJKM)<-TOL_F2) THEN
               INTERSECT_Z(IJK) = .TRUE.
               Zint(IJK) = HALF * (Zint_min(IJK)+Zint_max(IJK))
            ENDIF
         ENDIF
      END DO


! Propagates node values to all interior cells
! in order defined by CAD_PROPAGATE_ORDER (outer loop)


      call send_recv(F_AT,2)
      call send_recv(Xint,2)
      call send_recv(Yint,2)
      call send_recv(Zint,2)
      call SEND_RECEIVE_1D_LOGICAL(INTERSECT_X,2)
      call SEND_RECEIVE_1D_LOGICAL(INTERSECT_Y,2)
      call SEND_RECEIVE_1D_LOGICAL(INTERSECT_Z,2)
!======================================================================
!  Clean-up intersection flags in preparaton of small cells removal
!======================================================================

      DO IJK = IJKSTART3, IJKEND3

!        IF(INTERIOR_CELL_AT(IJK)) THEN

!            IF(POTENTIAL_CUT_CELL_AT(IJK))  CALL CLEAN_INTERSECT(IJK,'SCALAR',Xn_int(IJK),Ye_int(IJK),Zt_int(IJK))

         CALL CLEAN_INTERSECT(IJK,TYPE_OF_CELL,Xint(IJK),Yint(IJK),Zint(IJK))

!        ENDIF

      END DO

      call send_recv(F_AT,2)
      call SEND_RECEIVE_1D_LOGICAL(SNAP,2)
      call SEND_RECEIVE_1D_LOGICAL(INTERSECT_X,2)
      call SEND_RECEIVE_1D_LOGICAL(INTERSECT_Y,2)
      call SEND_RECEIVE_1D_LOGICAL(INTERSECT_Z,2)

      SELECT CASE (CAD_PROPAGATE_ORDER)

      CASE ('   ')

! After intersecting the edges of the background mesh with the STL facets,
! the end points (i.e., cell corners) are assigned a value, called F_AT, where:
! F_AT = zero if the corner point is on a facet (within some tolerance TOL_STL),
! F_AT < zero if the corner point is inside  the fluid region,
! F_AT > zero if the corner point is outside the fluid region.
! At this point F_AT is only defined across edges that intersect the STL facets,
! and it must be propagated to all cell corners to determine if uncut cells
! are inside or outside the fluid region.

! Only F_AT values that are defined and not zero are propagated to their direct
! neighbors, if it is not already defined. The propagation is repeated
! at most N_PROPMAX. The loop is exited when all F_AT values are defined.
! N_PROPMAX could be increased for very large domains.
! The propagation of F_AT will stop anytime a boundary is encountered since F_AT
! changes sign across a boundary.
!
         DO N_PROP=1,N_PROPMAX

            DO IJK = IJKSTART3, IJKEND3

! Aaron
               I = I_OF(IJK)
               J = J_OF(IJK)
               K = K_OF(IJK)
               IF(.NOT.IS_ON_myPE_plus1layer(I,J,K))cycle
! End aaron

               IF(F_AT(IJK)/=UNDEFINED.AND.F_AT(IJK)/=ZERO) THEN

                  IMJK = IM_OF(IJK)
                  IF(F_AT(IMJK)==UNDEFINED.AND.F_AT(IMJK)/=ZERO)  F_AT(IMJK)=F_AT(IJK)

                  IPJK = IP_OF(IJK)
                  IF(F_AT(IPJK)==UNDEFINED.AND.F_AT(IPJK)/=ZERO)  F_AT(IPJK)=F_AT(IJK)

                  IJMK = JM_OF(IJK)
                  IF(F_AT(IJMK)==UNDEFINED.AND.F_AT(IJMK)/=ZERO)  F_AT(IJMK)=F_AT(IJK)

                  IJPK = JP_OF(IJK)
                  IF(F_AT(IJPK)==UNDEFINED.AND.F_AT(IJPK)/=ZERO)  F_AT(IJPK)=F_AT(IJK)

                  IJKM = KM_OF(IJK)
                  IF(F_AT(IJKM)==UNDEFINED.AND.F_AT(IJKM)/=ZERO)  F_AT(IJKM)=F_AT(IJK)

                  IJKP = KP_OF(IJK)
                  IF(F_AT(IJKP)==UNDEFINED.AND.F_AT(IJKP)/=ZERO)  F_AT(IJKP)=F_AT(IJK)

               ENDIF

            ENDDO ! IJK Loop


! Communicate F_AT across processors for DMP runs
            call send_recv(F_AT,2)

! Count the number of undefined values of F_AT
! and exit loop if all values of F_AT have been propagated
            N_UNDEFINED = 0
            DO IJK = IJKSTART3, IJKEND3
               IF(INTERIOR_CELL_AT(IJK).AND.F_AT(IJK)==UNDEFINED) N_UNDEFINED = N_UNDEFINED + 1
            ENDDO

            call global_all_sum( N_UNDEFINED, NTOTAL_UNDEFINED )
            IF(NTOTAL_UNDEFINED==0) EXIT

         ENDDO ! N_PROP Loop


         call send_recv(F_AT,2)

! If a process still has undefined values of F_AT, this probably means
! that all cells belonging to that process are dead cells.
         IF(N_UNDEFINED>0) THEN
            WRITE(*,*)'WARNING: UNABLE TO PROPAGATE F_AT ARRAY FROM myPE=.', MyPE
            WRITE(*,*)'         THIS USUALLY INDICATE A RANK WITH NO ACTIVE CELL'
            WRITE(*,*)'         YOU MAY NEED TO ADJUST THE GRID PARTITIONNING'
            WRITE(*,*)'         TO GET BETTER LOAD_BALANCE.'
         ENDIF



      CASE ('IJK')

         DO I=ISTART3,IEND3
            DO K=KSTART3,KEND3
               F_FOUND = .FALSE.
               DO J=JSTART3,JEND3
                  IJK=FUNIJK(I,J,K)
                  IF(F_AT(IJK)/=UNDEFINED.AND.F_AT(IJK)/=ZERO) THEN
                     F_FOUND = .TRUE.
                     CURRENT_F = F_AT(IJK)
                     EXIT
                  ENDIF
               ENDDO
               IF(F_FOUND) THEN
                  DO J=JSTART3,JEND3
                     IJK=FUNIJK(I,J,K)
                     IF(F_AT(IJK)==UNDEFINED) THEN
                        F_AT(IJK)=CURRENT_F
                     ELSEIF(F_AT(IJK)/=ZERO) THEN
                        CURRENT_F = F_AT(IJK)
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO

         call send_recv(F_AT,2)

         DO J=JSTART3,JEND3
            DO I=ISTART3,IEND3
               F_FOUND = .FALSE.
               DO K=KSTART3,KEND3
                  IJK=FUNIJK(I,J,K)
                  IF(F_AT(IJK)/=UNDEFINED.AND.F_AT(IJK)/=ZERO) THEN
                     F_FOUND = .TRUE.
                     CURRENT_F = F_AT(IJK)
                     EXIT
                  ENDIF
               ENDDO
               IF(F_FOUND) THEN
                  DO K=KSTART3,KEND3
                     IJK=FUNIJK(I,J,K)
                     IF(F_AT(IJK)==UNDEFINED) THEN
                        F_AT(IJK)=CURRENT_F
                     ELSEIF(F_AT(IJK)/=ZERO) THEN
                        CURRENT_F = F_AT(IJK)
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO

         call send_recv(F_AT,2)

         DO K=KSTART3,KEND3
            DO J=JSTART3,JEND3
               F_FOUND = .FALSE.
               DO I=ISTART3,IEND3
                  IJK=FUNIJK(I,J,K)
                  IF(F_AT(IJK)/=UNDEFINED.AND.F_AT(IJK)/=ZERO) THEN
                     F_FOUND = .TRUE.
                     CURRENT_F = F_AT(IJK)
                     EXIT
                  ENDIF
               ENDDO
               IF(F_FOUND) THEN
                  DO I=ISTART3,IEND3
                     IJK=FUNIJK(I,J,K)
                     IF(F_AT(IJK)==UNDEFINED) THEN
                        F_AT(IJK)=CURRENT_F
                     ELSEIF(F_AT(IJK)/=ZERO) THEN
                        CURRENT_F = F_AT(IJK)
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO

         call send_recv(F_AT,2)

      CASE ('JKI')

         DO J=JSTART3,JEND3
            DO I=ISTART3,IEND3
               F_FOUND = .FALSE.
               DO K=KSTART3,KEND3
                  IJK=FUNIJK(I,J,K)
                  IF(F_AT(IJK)/=UNDEFINED.AND.F_AT(IJK)/=ZERO) THEN
                     F_FOUND = .TRUE.
                     CURRENT_F = F_AT(IJK)
                     EXIT
                  ENDIF
               ENDDO
               IF(F_FOUND) THEN
                  DO K=KSTART3,KEND3
                     IJK=FUNIJK(I,J,K)
                     IF(F_AT(IJK)==UNDEFINED) THEN
                        F_AT(IJK)=CURRENT_F
                     ELSEIF(F_AT(IJK)/=ZERO) THEN
                        CURRENT_F = F_AT(IJK)
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO

         call send_recv(F_AT,2)

         DO K=KSTART3,KEND3
            DO J=JSTART3,JEND3
               F_FOUND = .FALSE.
               DO I=ISTART3,IEND3
                  IJK=FUNIJK(I,J,K)
                  IF(F_AT(IJK)/=UNDEFINED.AND.F_AT(IJK)/=ZERO) THEN
                     F_FOUND = .TRUE.
                     CURRENT_F = F_AT(IJK)
                     EXIT
                  ENDIF
               ENDDO
               IF(F_FOUND) THEN
                  DO I=ISTART3,IEND3
                     IJK=FUNIJK(I,J,K)
                     IF(F_AT(IJK)==UNDEFINED) THEN
                        F_AT(IJK)=CURRENT_F
                     ELSEIF(F_AT(IJK)/=ZERO) THEN
                        CURRENT_F = F_AT(IJK)
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO

         call send_recv(F_AT,2)

         DO I=ISTART3,IEND3
            DO K=KSTART3,KEND3
               F_FOUND = .FALSE.
               DO J=JSTART3,JEND3
                  IJK=FUNIJK(I,J,K)
                  IF(F_AT(IJK)/=UNDEFINED.AND.F_AT(IJK)/=ZERO) THEN
                     F_FOUND = .TRUE.
                     CURRENT_F = F_AT(IJK)
                     EXIT
                  ENDIF
               ENDDO
               IF(F_FOUND) THEN
                  DO J=JSTART3,JEND3
                     IJK=FUNIJK(I,J,K)
                     IF(F_AT(IJK)==UNDEFINED) THEN
                        F_AT(IJK)=CURRENT_F
                     ELSEIF(F_AT(IJK)/=ZERO) THEN
                        CURRENT_F = F_AT(IJK)
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO

         call send_recv(F_AT,2)


      CASE ('KIJ')



         DO K=KSTART3,KEND3
            DO J=JSTART3,JEND3
               F_FOUND = .FALSE.
               DO I=ISTART3,IEND3
                  IJK=FUNIJK(I,J,K)
                  IF(F_AT(IJK)/=UNDEFINED.AND.F_AT(IJK)/=ZERO) THEN
                     F_FOUND = .TRUE.
                     CURRENT_F = F_AT(IJK)
                     EXIT
                  ENDIF
               ENDDO
               IF(F_FOUND) THEN
                  DO I=ISTART3,IEND3
                     IJK=FUNIJK(I,J,K)
                     IF(F_AT(IJK)==UNDEFINED) THEN
                        F_AT(IJK)=CURRENT_F
                     ELSEIF(F_AT(IJK)/=ZERO) THEN
                        CURRENT_F = F_AT(IJK)
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO

         call send_recv(F_AT,2)

         DO I=ISTART3,IEND3
            DO K=KSTART3,KEND3
               F_FOUND = .FALSE.
               DO J=JSTART3,JEND3
                  IJK=FUNIJK(I,J,K)
                  IF(F_AT(IJK)/=UNDEFINED.AND.F_AT(IJK)/=ZERO) THEN
                     F_FOUND = .TRUE.
                     CURRENT_F = F_AT(IJK)
                     EXIT
                  ENDIF
               ENDDO
               IF(F_FOUND) THEN
                  DO J=JSTART3,JEND3
                     IJK=FUNIJK(I,J,K)
                     IF(F_AT(IJK)==UNDEFINED) THEN
                        F_AT(IJK)=CURRENT_F
                     ELSEIF(F_AT(IJK)/=ZERO) THEN
                        CURRENT_F = F_AT(IJK)
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO


         call send_recv(F_AT,2)

         DO J=JSTART3,JEND3
            DO I=ISTART3,IEND3
               F_FOUND = .FALSE.
               DO K=KSTART3,KEND3
                  IJK=FUNIJK(I,J,K)
                  IF(F_AT(IJK)/=UNDEFINED.AND.F_AT(IJK)/=ZERO) THEN
                     F_FOUND = .TRUE.
                     CURRENT_F = F_AT(IJK)
                     EXIT
                  ENDIF
               ENDDO
               IF(F_FOUND) THEN
                  DO K=KSTART3,KEND3
                     IJK=FUNIJK(I,J,K)
                     IF(F_AT(IJK)==UNDEFINED) THEN
                        F_AT(IJK)=CURRENT_F
                     ELSEIF(F_AT(IJK)/=ZERO) THEN
                        CURRENT_F = F_AT(IJK)
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDDO


         call send_recv(F_AT,2)

      CASE DEFAULT
         IF(myPE == PE_IO) THEN
            WRITE(*,*)'CAD_INTERSECT.'
            WRITE(*,*)'UNKNOWN CAD_PROPAGATE_ORDER:',CAD_PROPAGATE_ORDER
            WRITE(*,*)'ACCEPTABLE VALUES:'
            WRITE(*,*)'IJK'
            WRITE(*,*)'JKI'
            WRITE(*,*)'KIJ'
         ENDIF
!            call log_error()

      END SELECT

      call send_recv(F_AT,2)

      RETURN

   END SUBROUTINE CAD_INTERSECT


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: SET_SNAP_FLAG                                          C
!  Purpose: Set SNAP flag in preparation of intersection flag removal  C
!           removal                                                    C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 04-Dec-14  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE SET_SNAP_FLAG(IJK,TYPE_OF_CELL,Xi,Yi,Zi)

      IMPLICIT NONE

      CHARACTER (LEN=*) :: TYPE_OF_CELL
      INTEGER :: IJK,I,J,K,IM,JM,KM
      INTEGER :: BCID
      INTEGER :: IMJK,IJMK,IJKM
      DOUBLE PRECISION :: xa,ya,za,xb,yb,zb
      DOUBLE PRECISION :: Xi,Yi,Zi
      DOUBLE PRECISION :: DFC,DFC_MAX,F4,F6,F7,F8
      LOGICAL :: CLIP_FLAG,CAD,F_TEST

! When inputing geometry from CAD (STL or MSH file), the snapping procedure is
! dependent on the value of F at the cell corners
! For other geometry inputs (say quadrics), This is not needed, and the value
! of F_TEST is set to .TRUE. here
      CAD = USE_MSH.OR.USE_STL
      F_TEST = .TRUE.

!======================================================================
!  Get coordinates of eight nodes
!======================================================================

      CALL GET_CELL_NODE_COORDINATES(IJK,TYPE_OF_CELL)

      I = I_OF(IJK)
      J = J_OF(IJK)
      K = K_OF(IJK)

      IM = I - 1
      JM = J - 1
      KM = K - 1

      IMJK = FUNIJK(IM,J,K)
      IJMK = FUNIJK(I,JM,K)
      IJKM = FUNIJK(I,J,KM)

      IF(IMJK<1.OR.IMJK>DIMENSION_3) IMJK = IJK
      IF(IJMK<1.OR.IJMK>DIMENSION_3) IJMK = IJK
      IF(IJKM<1.OR.IJKM>DIMENSION_3) IJKM = IJK

!======================================================================
!  Clean Intersection with Edge 7 (node 7-8, Face North-Top):
!======================================================================

      xa = X_NODE(7)
      ya = Y_NODE(7)
      za = Z_NODE(7)

      xb = X_NODE(8)
      yb = Y_NODE(8)
      zb = Z_NODE(8)


      DFC_MAX = TOL_SNAP(1) * DSQRT((xb-xa)**2+(yb-ya)**2+(zb-za)**2)  ! MAXIMUM DISTANCE FROM CORNER

      IF(INTERSECT_X(IJK)) THEN

         DFC = DABS(Xi-xa) ! DISTANCE FROM CORNER (NODE 7)

         IF(CAD) F_TEST = (F_AT(IMJK)/=ZERO)
         IF(DFC < DFC_MAX.AND.F_TEST) THEN
            IF(PRINT_WARNINGS) THEN
               WRITE(*,*)'MERGING X-INTERSECTION ALONG EDGE 7 ONTO NODE 7'
               WRITE(*,*)'AT IJK,I,J,K=',IJK,I,J,K
            ENDIF

            IF(I>=IMIN1) SNAP(IMJK) = .TRUE.

         ENDIF


         DFC = DABS(Xi-xb) ! DISTANCE FROM CORNER (NODE 8)

         IF(CAD) F_TEST = (F_AT(IJK)/=ZERO)
         IF(DFC < DFC_MAX.AND.F_TEST) THEN
            IF(PRINT_WARNINGS) THEN
               WRITE(*,*)'MERGING X-INTERSECTION ALONG EDGE 7 ONTO NODE 8'
               WRITE(*,*)'AT IJK,I,J,K=',IJK,I,J,K
            ENDIF

            SNAP(IJK) = .TRUE.

         ENDIF

         IF(USE_STL.OR.USE_MSH) THEN
            CALL EVAL_STL_FCT_AT(TYPE_OF_CELL,IJK,7,F7,CLIP_FLAG,BCID)
            CALL EVAL_STL_FCT_AT(TYPE_OF_CELL,IJK,8,F8,CLIP_FLAG,BCID)
            IF(F7*F8>TOL_STL**2) INTERSECT_X(IJK)  = .FALSE.
         ENDIF

      ENDIF


!======================================================================
!  Clean Intersection with Edge 6 (node 6-8, Face East-Top):
!======================================================================

      xa = X_NODE(6)
      ya = Y_NODE(6)
      za = Z_NODE(6)

      DFC_MAX = TOL_SNAP(2) * DSQRT((xb-xa)**2+(yb-ya)**2+(zb-za)**2)  ! MAXIMUM DISTANCE FROM CORNER


      IF(INTERSECT_Y(IJK)) THEN

         DFC = DABS(Yi-ya) ! DISTANCE FROM CORNER (NODE 6)

         IF(CAD) F_TEST = (F_AT(IJMK)/=ZERO)
         IF(DFC < DFC_MAX.AND.F_TEST) THEN
            IF(PRINT_WARNINGS) THEN
               WRITE(*,*)'MERGING Y-INTERSECTION ALONG EDGE 6 ONTO NODE 6'
               WRITE(*,*)'AT IJK,I,J,K=',IJK,I,J,K
            ENDIF

            IF(J>=JMIN1) SNAP(IJMK) = .TRUE.

         ENDIF


         DFC = DABS(Yi-yb) ! DISTANCE FROM CORNER (NODE 8)

         IF(CAD) F_TEST = (F_AT(IJK)/=ZERO)
         IF(DFC < DFC_MAX.AND.F_TEST) THEN
            IF(PRINT_WARNINGS) THEN
               WRITE(*,*)'MERGING Y-INTERSECTION ALONG EDGE 6 ONTO NODE 8'
               WRITE(*,*)'AT IJK,I,J,K=',IJK,I,J,K
            ENDIF

            SNAP(IJK) = .TRUE.


         ENDIF


         IF(USE_STL.OR.USE_MSH) THEN
            CALL EVAL_STL_FCT_AT(TYPE_OF_CELL,IJK,6,F6,CLIP_FLAG,BCID)
            CALL EVAL_STL_FCT_AT(TYPE_OF_CELL,IJK,8,F8,CLIP_FLAG,BCID)
            IF(F6*F8>TOL_STL**2) INTERSECT_Y(IJK)  = .FALSE.
         ENDIF

      ENDIF


      IF(DO_K) THEN
!======================================================================
!  Intersection with Edge 11 (node 4-8, Face East-North):
!======================================================================

         xa = X_NODE(4)
         ya = Y_NODE(4)
         za = Z_NODE(4)

         DFC_MAX = TOL_SNAP(3) * DSQRT((xb-xa)**2+(yb-ya)**2+(zb-za)**2)  ! MAXIMUM DISTANCE FROM CORNER

         IF(INTERSECT_Z(IJK)) THEN

            DFC = DABS(Zi-Za) ! DISTANCE FROM CORNER (NODE 4)

            IF(CAD) F_TEST = (F_AT(IJKM)/=ZERO)
            IF(DFC < DFC_MAX.AND.F_TEST) THEN
               IF(PRINT_WARNINGS) THEN
                  WRITE(*,*)'MERGING Z-INTERSECTION ALONG EDGE 11 ONTO NODE 4'
                  WRITE(*,*)'AT IJK,I,J,K=',IJK,I,J,K
               ENDIF

               IF(K>=KMIN1) SNAP(IJKM) = .TRUE.

            ENDIF

            DFC = DABS(Zi-Zb) ! DISTANCE FROM CORNER (NODE 8)

            IF(CAD) F_TEST = (F_AT(IJK)/=ZERO)
            IF(DFC < DFC_MAX.AND.F_TEST) THEN
               IF(PRINT_WARNINGS) THEN
                  WRITE(*,*)'MERGING Z-INTERSECTION ALONG EDGE 11 ONTO NODE 8'
                  WRITE(*,*)'AT IJK,I,J,K=',IJK,I,J,K
               ENDIF

               SNAP(IJK) = .TRUE.

            ENDIF


            IF(USE_STL.OR.USE_MSH) THEN
               CALL EVAL_STL_FCT_AT(TYPE_OF_CELL,IJK,4,F4,CLIP_FLAG,BCID)
               CALL EVAL_STL_FCT_AT(TYPE_OF_CELL,IJK,8,F8,CLIP_FLAG,BCID)
               IF(F4*F8>TOL_STL**2) INTERSECT_Z(IJK)  = .FALSE.
            ENDIF

         ENDIF

      ENDIF


      RETURN

   END SUBROUTINE SET_SNAP_FLAG

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: REMOVE_INTERSECT_FLAG                                  C
!  Purpose: Remove Intersection flags                                  C
!           removal                                                    C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 04-Dec-14  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE REMOVE_INTERSECT_FLAG(IJK)

      IMPLICIT NONE

      INTEGER :: IJK,I,J,K,IP,JP,KP
      INTEGER :: IPJK,IJPK,IJKP

      I = I_OF(IJK)
      J = J_OF(IJK)
      K = K_OF(IJK)

      IP = I + 1
      JP = J + 1
      KP = K + 1

      IPJK = FUNIJK(IP,J,K)
      IJPK = FUNIJK(I,JP,K)
      IJKP = FUNIJK(I,J,KP)

      IF(IPJK<1.OR.IPJK>DIMENSION_3) IPJK = IJK
      IF(IJPK<1.OR.IJPK>DIMENSION_3) IJPK = IJK
      IF(IJKP<1.OR.IJKP>DIMENSION_3) IJKP = IJK

      IF(SNAP(IJK)) THEN
         INTERSECT_X(IJK) = .FALSE.
         INTERSECT_Y(IJK) = .FALSE.
         INTERSECT_Z(IJK) = .FALSE.
         IF(I<=IMAX1) INTERSECT_X(IPJK) = .FALSE.
         IF(J<=JMAX1) INTERSECT_Y(IJPK) = .FALSE.
         IF(DO_K.AND.(K<=KMAX1)) INTERSECT_Z(IJKP) = .FALSE.
      ENDIF

      RETURN

   END SUBROUTINE REMOVE_INTERSECT_FLAG


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: ADD_FACET                                              C
!  Purpose: Add facet to list in IJK scalar cell                       C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 15-Oct-13  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE ADD_FACET_AND_SET_BC_ID(IJK,NN)

      IMPLICIT NONE

      INTEGER :: IJK,NN

      BC_ID(IJK) = BC_ID_STL_FACE(NN)             ! Set tentative BC_ID

      IF(N_FACET_AT(IJK)>=DIM_FACETS_PER_CELL) THEN
         DIM_FACETS_PER_CELL = DIM_FACETS_PER_CELL + 2
         CALL INTEGER_GROW2(LIST_FACET_AT,DIM_FACETS_PER_CELL)
      ENDIF

      N_FACET_AT(IJK) = N_FACET_AT(IJK) + 1
      LIST_FACET_AT(IJK,N_FACET_AT(IJK)) = NN

   END SUBROUTINE ADD_FACET_AND_SET_BC_ID

END MODULE INTERSECT_MOD
