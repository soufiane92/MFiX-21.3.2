MODULE CALC_S_DDOT_MOD

CONTAINS


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: CALC_S_DDOT_S                                           C
!                                                                      C
!  Purpose: At the boundary, calculate del.U (trace of D_s),           C
!           S:S and S_xx, S_yy or S_zz.                                C
!                                                                      C
!                                                                      C
!  Author: Anuj Srivastava, Princeton University      Date: 4-APR-98   C
!  Reviewer:                                          Date:            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE CALC_S_DDOT_S(IJK1, IJK2, FCELL, COM, M, &
                               DEL_DOT_U, S_DDOT_S, S_DD)

      USE param1, only: zero, half
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------
! IJK indices for wall cell and fluid cell
      INTEGER, INTENT(IN) :: IJK1, IJK2
! The location (e,w,n...) of fluid cell;
! normal direction from boundary plane into fluid
      CHARACTER, INTENT(IN) :: FCELL
! Velocity component (U, V, W)
      CHARACTER, INTENT(IN) :: COM
! Solids phase index
      INTEGER, INTENT(IN) :: M
! del.u = trace(grad(U))=trace(D)
      DOUBLE PRECISION, INTENT(OUT) :: DEL_DOT_U
! S:S
      DOUBLE PRECISION, INTENT(OUT) :: S_DDOT_S
! S_dd (dd is the relevant direction x,y or z)
      DOUBLE PRECISION, INTENT(OUT) :: S_dd

! Local variables
!---------------------------------------------------------------------
! Coordinates indicated here are relative; ultimately velocity components
! are returned at appropriate neighbor locations relevant to selected
! boundary
! U_s at the east (i+1/2, j, k) and the west (i-1/2, j, k)
      DOUBLE PRECISION :: Us_E, UsW
! U_s at the north (i, j+1/2, k) and the south (i, j-1/2, k)
      DOUBLE PRECISION :: UsN, UsS
! U_s at the top (i, j, k+1/2) and the bottom (i, j, k-1/2)
      DOUBLE PRECISION :: UsT, UsB
! U_s at the center (i, j, k)
! Calculated for Cylindrical coordinates only.
      DOUBLE PRECISION :: UscC

! V_s at the east (i+1/2, j, k) and the west (i-1/2, j, k)
      DOUBLE PRECISION :: VsE, VsW
! V_s at the north (i, j+1/2, k) and the south (i, j-1/2, k)
      DOUBLE PRECISION :: VsN, VsS
! V_s at the top (i, j, k+1/2) and the bottom (i, j, k-1/2)
      DOUBLE PRECISION :: VsT, VsB

! W_s at the east (i+1/2, j, k) and the west (i-1/2, j, k)
      DOUBLE PRECISION :: WsE, WsW
! W_s at the north (i, j+1/2, k) and the south (i, j-1/2, k)
      DOUBLE PRECISION :: WsN, WsS
! W_s at the top (i, j, k+1/2) and the bottom (i, j, k-1/2)
      DOUBLE PRECISION :: WsT, WsB
! W_s at the center (i, j, k).
! Calculated for Cylindrical coordinates only.
      DOUBLE PRECISION :: WscC

! 1/(distance between us_e and usw)
! 1/(distance between vsn and vss)
! 1/(distance between wst and wsb)
! 1/(position x)
      DOUBLE PRECISION :: O_DX, O_DY, O_DZ, O_X

! Strain rate tensor components for mth solids phase
       DOUBLE PRECISION :: D_s(3,3)
! trace of D
       DOUBLE PRECISION :: trd_sM
! trace of the square of D
       DOUBLE PRECISION :: trd_sM2
! Local indices
       INTEGER :: I1, I2

!-----------------------------------------------

      CALL GET_NEIGHBOR_VEL_AT_WALL(IJK1, IJK2, FCELL, COM, M, &
                  Us_E, UsW, UsN, UsS, UsT, UsB, UscC,&
                  VsE, VsW, VsN, VsS, VsT, VsB, &
                  WsE, WsW, WsN, WsS, WsT, WsB, WscC, &
                  o_DX, o_DY, o_DZ, o_x)

! Find components of Mth solids phase rate of strain tensor D_S at
! indicated wall position..
      D_S(1,1) = (Us_E-UsW)*O_DX
      D_S(1,2) = HALF*((UsN-UsS)*O_DY+&
                       (VsE-VsW)*O_DX)
      D_S(1,3) = HALF*((WsE-WsW)*O_DX+&
                       (UsT-UsB)*O_DZ*O_X-&
                 WscC*O_X)
      D_S(2,1) = D_S(1,2)
      D_S(2,2) = (VsN-VsS)*O_DY
      D_S(2,3) = HALF*((VsT-VsB)*O_X*O_DZ+&
                       (WsN-WsS)*O_DY)
      D_S(3,1) = D_S(1,3)
      D_S(3,2) = D_S(2,3)
      D_S(3,3) = (WsT-WsB)*O_X*O_DZ+&
                  UscC*O_X

! Calculate the trace of D_s
      trd_sM = D_S(1,1) + D_S(2,2) + D_S(3,3)

!  Calculate trace of the square of D_s
      trd_sM2 = zero
      DO I1 = 1, 3
         DO I2 = 1,3
            trd_sM2 = trd_sM2 + D_S(I1,I2)*D_S(I1,I2)
! tr(D^2)= DijDji= D11^2+D22^2+D33^2+2D12^2+2D13^2+2D23^3
         ENDDO
      ENDDO
      IF (trd_sM2 == zero) trd_sM = zero


      DEL_DOT_U = trd_sM

! tr(S^2)=SijSji = tr(D^2)-1/3(tr(D))^2 = -2 Im2D(Sij)
      S_DDOT_S = trd_sM2 - (trd_sM*trd_sM)/3.D0

      S_DDOT_S = DMAX1(1D-10,S_DDOT_S)

      IF (TRIM(FCELL)=='E' .OR. TRIM(FCELL)=='W') THEN
         S_DD = D_S(1,1) - trd_sM/3D0

      ELSE IF (TRIM(FCELL)=='N' .OR. TRIM(FCELL)=='S') THEN
         S_DD = D_S(2,2) - trd_sM/3D0

      ELSE
         S_DD = D_S(3,3) - trd_sM/3D0
      ENDIF



      RETURN
      END SUBROUTINE CALC_S_DDOT_S


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: GET_NEIGHBOR_VEL_AT_WALL                                C
!                                                                      C
!  Author: Anuj Srivastava, Princeton University      Date: 4-APR-98   C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

!  NOTES:
!  Get velocity components at all six neighbor points from a specific
!  position. The indicated position is the center of four cells. These
!  four cells consist of a fluid/wall cell combination and their
!  corresponding east/north/top neighbor depending on the momentum
!  equation (x/u=>east, y/v=>north, z/w>=top).

!  SO the 4 cells are dictated by the momentum equation (x/u, y/v, z/w)
!  and direction of fcell, which together specify the boundary plane
!  (xy, yz, xz) of interest and center point.

! For x-momentum planes xy and xz coordinates will be:
!    plane xz, fcell N/S: i+1/2, j+1/2, k from ijk1/ijk2
!    neighbor velocity positions are:
!       east  : i+1,   j+1/2, k
!       west  : i,     j+1/2, k
!       north : i+1/2, j+1,   k
!       south : i+1/2, j,     k
!       top   : i+1/2, j+1/2, k+1/2
!       bottom: i+1/2, j+1/2, k-1/2
!    plane xy, fcell T/B: i+1/2, j, k+1/2 from ijk1/ijk2
!    neighbor velocity positions are:
!       east:   i+1,   j,     k+1/2
!       west:   i,     j,     k+1/2
!       north:  i+1/2, j+1/2, k+1/2
!       south:  i+1/2, j-1/2, k+1/2
!       top:    i+1/2, j,     k+1
!       bottom: i+1/2, j,     k

! For y-momentum planes yz and xy coordinates will be:
!    plane yz, fcell E/W: i+1/2, j+1/2, k from ijk1/ijk2
!       east:   i+1,   j+1/2, k
!       west:   i,     j+1/2, k
!       north:  i+1/2, j+1,   k
!       south:  i+1/2, j,     k
!       top:    i+1/2, j+1/2, k+1/2
!       bottom: i+1/2, j+1/2, k-1/2
!    plane xy, fcell T/B: i, j+1/2, k+1/2 from ijk1/ijk2
!       east:   i+1/2, j+1/2, k+1/2
!       west:   i-1/2, j+1/2, k+1/2
!       north:  i,     j+1,   k+1/2
!       south:  i,     j,     k+1/2
!       top:    i,     j+1/2, k+1
!       bottom: i,     j+1/2, k

! For z-momentum planes yz and xz coordinates will be:
!    plane yz, fcell E/W: i+1/2, j, k+1/2 from ijk1/ijk2
!       east:   i+1,   j,     k+1/2
!       west:   i,     j,     k+1/2
!       north:  i+1/2, j+1/2, k+1/2
!       south:  i+1/2, j-1/2, k+1/2
!       top:    i+1/2, j,     k+1
!       bottom: i+1/2, j,     k

!    plane xz, fcell N/S: i, j+1/2, k+1/2 from ijk1/ijk2
!       east:   i+1/2, j+1/2, k+1/2
!       west:   i-1/2, j+1/2, k+1/2
!       north:  i,     j+1,   k+1/2
!       south:  i,     j,     k+1/2
!       top:    i,     j+1/2, k+1
!       bottom: i,     j+1/2, k


      SUBROUTINE GET_NEIGHBOR_VEL_AT_WALL(IJK1, IJK2, FCELL, COM, M, &
                        us_e, usw, usn, uss, ust, usb, uscc, &
                        vse, vsw, vsn, vss, vst, vsb, &
                        wse, wsw, wsn, wss, wst, wsb, wscc, &
                        o_dx, o_dy, o_dz, o_x)

      USE fldvar, only: u_s, v_s, w_s
      USE fun_avg, only: avg_x, avg_x_e
      USE fun_avg, only: avg_y, avg_y_n
      USE fun_avg, only: avg_z, avg_z_t
      USE functions, only: im_of, jm_of, km_of
      USE functions, only: ip_of, jp_of, kp_of
      USE geometry, only: cylindrical
      USE geometry, only: odx, ody, odz, ox
      USE geometry, only: odx_e, ody_n, odz_t, ox_e
      USE indices, only: i_of, j_of, k_of
      USE indices, only: im1, jm1, km1
      USE param1, only: zero
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------
! IJK indices for wall cell and fluid cell
      INTEGER, INTENT(IN) :: IJK1, IJK2
! The location (e,w,n...) of fluid cell
      CHARACTER, INTENT(IN) :: FCELL
! Velocity component (U, V, W)
      CHARACTER, INTENT(IN) :: COM
! Solids phase index
      INTEGER, INTENT(IN) :: M

! Coordinates indicated here are relative; ultimately velocity components
! are returned at locations indicated above
! U_s at the east (i+1/2, j, k) and the west (i-1/2, j, k)
      DOUBLE PRECISION, INTENT(OUT) :: Us_E, UsW
! U_s at the north (i, j+1/2, k) and the south (i, j-1/2, k)
      DOUBLE PRECISION, INTENT(OUT) :: UsN, UsS
! U_s at the top (i, j, k+1/2) and the bottom (i, j, k-1/2)
      DOUBLE PRECISION, INTENT(OUT) :: UsT, UsB
! U_s at the center (i, j, k)
! Calculated for Cylindrical coordinates only.
      DOUBLE PRECISION, INTENT(OUT) :: UscC

! V_s at the east (i+1/2, j, k) and the west (i-1/2, j, k)
      DOUBLE PRECISION, INTENT(OUT) :: VsE, VsW
! V_s at the north (i, j+1/2, k) and the south (i, j-1/2, k)
      DOUBLE PRECISION, INTENT(OUT) :: VsN, VsS
! V_s at the top (i, j, k+1/2) and the bottom (i, j, k-1/2)
      DOUBLE PRECISION, INTENT(OUT) :: VsT, VsB

! W_s at the east (i+1/2, j, k) and the west (i-1/2, j, k)
      DOUBLE PRECISION, INTENT(OUT) :: WsE, WsW
! W_s at the north (i, j+1/2, k) and the south (i, j-1/2, k)
      DOUBLE PRECISION, INTENT(OUT) :: WsN, WsS
! W_s at the top (i, j, k+1/2) and the bottom (i, j, k-1/2)
      DOUBLE PRECISION, INTENT(OUT) :: WsT, WsB
! W_s at the center (i, j, k).
! Calculated for Cylindrical coordinates only.
      DOUBLE PRECISION, INTENT(OUT) :: WscC

! 1/(distance between us_e and usw)
! 1/(distance between vsn and vss)
! 1/(distance between wst and wsb)
! 1/(position x)
      DOUBLE PRECISION, INTENT(OUT) :: O_DX, O_DY, O_DZ, O_X

! Local variables
!---------------------------------------------------------------------
! Other indices
      INTEGER :: IJKH, IJKL
      INTEGER :: IPJKH, IPJKL
      INTEGER :: IJPKH, IJPKL
      INTEGER :: IJKPH, IJKPL

      INTEGER :: IL, IH, JL, JH, KL, KH
      INTEGER :: IML, IMH, JML, JMH, KML, KMH
!---------------------------------------------------------------------

! initialize indices to avoid/flag possible undefined references
      IJKH = 0; IJKL = 0;
      IPJKH = 0; IPJKL = 0;
      IJPKH = 0; IJPKL = 0;
      IJKPH = 0; IJKPL = 0;
      IL=0; IH=0; JL=0; JH=0; KL=0; KH=0;
      IML=0; IMH=0; JML=0; JMH=0; KML=0; KMH=0;

      SELECT CASE (TRIM(COM))
      CASE ('U')
         SELECT CASE (TRIM(FCELL))
         CASE ('N')
! xz-boundary plane
! middle of 4 cells is at i+1/2, j+1/2, k from ijk1
            IJKL = IJK1               !i,   j,   k     (wall)
            IJKH = IJK2               !i,   j+1, k     (fluid)

! all indices are given relative to ijk1 (wall)
            IPJKH = IP_OF(IJKH)       !i+1, j+1, k
            IPJKL = JM_OF(IPJKH)      !i+1, j,   k;    IP_OF(IJKL)

! derived/evaluated from fluid cell
!            IMJKH = IM_OF(IJKH)       !i-1, j+1, k
!            IJKMH = KM_OF(IJKH)       !i,   j+1, k-1
!            IJKPH = KP_OF(IJKH)       !i,   j+1, k+1

!            IPJKMH = KM_OF(IPJKH)     !i+1, j+1, k-1

! derived from fluid cell position
!            IPJMKL = JM_OF(IPJKL)     !i+1, j-1, k
!            IPJKML = KM_OF(IPJKL)     !i+1, j,   k-1

! NOT derived from fluid cell position
!            IMJKL = IM_OF(IJKL)       !i-1, j,   k;    JM_OF(IMJKH)
!            IJMKL = JM_OF(IJKL)       !i,   j-1, k;
!            IJKPL = KP_OF(IJKL)       !i,   j,   k+1;  JM_OF(IJKPH)
!            IJKML = KM_OF(IJKL)       !i,   j,   k-1;  JM_OF(IJKMH)

! i/j/k
            IL = I_OF(IJKL)
            IH = I_OF(IJKH)  ! =IL?
            JL = J_OF(IJKL)
            KL = K_OF(IJKL)
            KH = K_OF(IJKH)  ! =KL?
            IML = IM1(IL)
            IMH = IM1(IH)    ! =IML?
            KML = KM1(KL)    ! K_OF(KM_OF(IJKL))
            KMH = KM1(KH)    ! K_OF(KM_OF(IJKH))

            O_DX = ODX_E(IL)
            O_DY = ODY_N(JL)
            O_DZ = ODZ(KL)
            O_X = OX_E(IL)

            Us_E = AVG_Y(AVG_X_E(U_S(IJKL,M),U_S(IPJKL,M),IL),&
                         AVG_X_E(U_S(IJKH,M),U_S(IPJKH,M),IL),JL)        !i+1,   j+1/2, k
            UsW = AVG_Y(AVG_X_E(U_S(IM_OF(IJKL),M),U_S(IJKL,M),IML),&
                        AVG_X_E(U_S(IM_OF(IJKH),M),U_S(IJKH,M),IMH),JL)  !i,     j+1/2, k
            UsN = U_S(IJKH,M)                                            !i+1/2, j+1,   k
            UsS = U_S(IJKL,M)                                            !i+1/2, j,     k
            UsT = AVG_Y(AVG_Z(U_S(IJKL,M),U_S(KP_OF(IJKL),M),KL),&
                        AVG_Z(U_S(IJKH,M),U_S(KP_OF(IJKH),M),KH),JL)     !i+1/2, j+1/2, k+1/2
            UsB = AVG_Y(AVG_Z(U_S(KM_OF(IJKL),M),U_S(IJKL,M),KML),&
                        AVG_Z(U_S(KM_OF(IJKH),M),U_S(IJKH,M),KMH),JL)    !i+1/2, j+1/2, k-1/2

            VsE = ZERO
            VsW = ZERO
! VsN is based on wall (impenetrable=0) & fluid values and their
!    IP neighbors
! VsS is based on wall (zero) value & JM cell value (likely wall=0)
!    and their IP neighbors
            VsN = AVG_X(AVG_Y_N(V_S(IJKL,M),V_S(IJKH,M)),&
                        AVG_Y_N(V_S(IPJKL,M),V_S(IPJKH,M)),IL)
            VsS = AVG_X(AVG_Y_N(V_S(JM_OF(IJKL),M),V_S(IJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IPJKL),M),V_S(IPJKL,M)),IL)
            VsT = ZERO
            VsB = ZERO

            WsE = AVG_Y(AVG_Z_T(W_S(KM_OF(IPJKL),M),W_S(IPJKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IPJKH),M),W_S(IPJKH,M)),JL)
            WsW = AVG_Y(AVG_Z_T(W_S(KM_OF(IJKL),M),W_S(IJKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IJKH),M),W_S(IJKH,M)),JL)
            WsN = AVG_X(AVG_Z_T(W_S(KM_OF(IJKH),M),W_S(IJKH,M)),&
                        AVG_Z_T(W_S(KM_OF(IPJKH),M),W_S(IPJKH,M)),IH)
            WsS = AVG_X(AVG_Z_T(W_S(KM_OF(IJKL),M),W_S(IJKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IPJKL),M),W_S(IPJKL,M)),IL)
            WsT = AVG_X(AVG_Y(W_S(IJKL,M),W_S(IJKH,M),JL),&
                        AVG_Y(W_S(IPJKL,M),W_S(IPJKH,M),JL),IL)
            WsB = AVG_X(AVG_Y(W_S(KM_OF(IJKL),M),W_S(KM_OF(IJKH),M),JL),&
                        AVG_Y(W_S(KM_OF(IPJKL),M),W_S(KM_OF(IPJKH),M),JL),IL)

            IF (CYLINDRICAL) THEN
               UscC = AVG_Y(U_S(IJKL,M),U_S(IJKH,M),JL)
               WscC = AVG_X(WsW,WsE,IL)
            ELSE
               UscC = ZERO
               WscC = ZERO
            ENDIF


         CASE ('S')
! xz-boundary plane
! middle of 4 cells is at i+1/2, j+1/2, k from ijk2
            IJKL = IJK2               !i,   j,   k     (fluid)
            IJKH = IJK1               !i,   j+1, k     (wall)

! all indices are given relative to ijk2 (fluid)
            IPJKL = IP_OF(IJKL)       !i+1, j,   k
            IPJKH = JP_OF(IPJKL)      !i+1, j+1, k;    IP_OF(IJKH)

! derived/evaluated from fluid cell
!            IMJKL = IM_OF(IJKL)       !i-1, j,   k
!            IJMKL = JM_OF(IJKL)       !i,   j-1, k
!            IJKPL = KP_OF(IJKL)       !i,   j,   k+1
!            IJKML = KM_OF(IJKL)       !i,   j,   k-1

!            IPJMKL = JM_OF(IPJKL)     !i+1, j-1, k
!            IPJKML = KM_OF(IPJKL)     !i+1, j,   k-1

! derived from fluid cell position
!            IPJKMH = KM_OF(IPJKH)     !i+1, j+1, k-1

! NOT derived from fluid cell position
!            IMJKH = IM_OF(IJKH)       !i-1, j+1, k  ;  JP_OF(IMJKL)
!            IJKMH = KM_OF(IJKH)       !i,   j+1, k-1;  JP_OF(IJKML)
!            IJKPH = KP_OF(IJKH)       !i,   j+1, k+1;  JP_OF(IJKPL)

! i/j/k
            IL = I_OF(IJKL)
            IH = I_OF(IJKH) ! =IL?
            JL = J_OF(IJKL)
            KL = K_OF(IJKL)
            KH = K_OF(IJKH) ! =KL?
            IML = IM1(IL)
            IMH = IM1(IH)
            KML = KM1(KL)
            KMH = KM1(KH)

            O_DX = ODX_E(IL)
            O_DY = ODY_N(JL)
            O_DZ = ODZ(KL)
            O_X = OX_E(IL)

            Us_E = AVG_Y(AVG_X_E(U_S(IJKL,M),U_S(IPJKL,M),IL),&
                         AVG_X_E(U_S(IJKH,M),U_S(IPJKH,M),IL),JL)        !i+1,   j+1/2, k
            UsW = AVG_Y(AVG_X_E(U_S(IM_OF(IJKL),M),U_S(IJKL,M),IML),&
                        AVG_X_E(U_S(IM_OF(IJKH),M),U_S(IJKH,M),IMH),JL)  !i,     j+1/2, k
            UsN = U_S(IJKH,M)                                            !i+1/2, j+1,   k
            UsS = U_S(IJKL,M)                                            !i+1/2, j,     k
            UsT = AVG_Y(AVG_Z(U_S(IJKL,M),U_S(KP_OF(IJKL),M),KL),&
                        AVG_Z(U_S(IJKH,M),U_S(KP_OF(IJKH),M),KH),JL)     !i+1/2, j+1/2, k+1/2
            UsB = AVG_Y(AVG_Z(U_S(KM_OF(IJKL),M),U_S(IJKL,M),KML),&
                        AVG_Z(U_S(KM_OF(IJKH),M),U_S(IJKH,M),KMH),JL)    !i+1/2, j+1/2, k-1/2

            VsE = ZERO
            VsW = ZERO

            VsE = ZERO
            VsW = ZERO
! VsN is based on wall & fluid (both impenetrable=0) values and
!    their IP neighbors
! VsS is based on fluid (impenetrable=0) value & JM cell value
!    (likely fluid?) and their IP neighbors
            VsN = AVG_X(AVG_Y_N(V_S(IJKL,M),V_S(IJKH,M)),&
                        AVG_Y_N(V_S(IPJKL,M),V_S(IPJKH,M)),IL)
            VsS = AVG_X(AVG_Y_N(V_S(JM_OF(IJKL),M),V_S(IJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IPJKL),M),V_S(IPJKL,M)),IL)
            VsT = ZERO
            VsB = ZERO

            WsE = AVG_Y(AVG_Z_T(W_S(KM_OF(IPJKL),M),W_S(IPJKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IPJKH),M),W_S(IPJKH,M)),JL)
            WsW = AVG_Y(AVG_Z_T(W_S(KM_OF(IJKL),M),W_S(IJKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IJKH),M),W_S(IJKH,M)),JL)
            WsN = AVG_X(AVG_Z_T(W_S(KM_OF(IJKH),M),W_S(IJKH,M)),&
                        AVG_Z_T(W_S(KM_OF(IPJKH),M),W_S(IPJKH,M)),IH)
            WsS = AVG_X(AVG_Z_T(W_S(KM_OF(IJKL),M),W_S(IJKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IPJKL),M),W_S(IPJKL,M)),IL)
            WsT = AVG_X(AVG_Y(W_S(IJKL,M),W_S(IJKH,M),JL),&
                        AVG_Y(W_S(IPJKL,M),W_S(IPJKH,M),JL),IL)
            WsB = AVG_X(AVG_Y(W_S(KM_OF(IJKL),M),W_S(KM_OF(IJKH),M),JL),&
                        AVG_Y(W_S(KM_OF(IPJKL),M),W_S(KM_OF(IPJKH),M),JL),IL)

            IF (CYLINDRICAL) THEN
               UscC = AVG_Y(U_S(IJKL,M),U_S(IJKH,M),JL)
               WscC = AVG_X(WsW,WsE,IL)
            ELSE
               UscC = ZERO
               WscC = ZERO
            ENDIF


         CASE ('T')
! xy-boundary plane
! middle of 4 cells is at i+1/2, j, k+1/2 from ijk1
! loc. is same as w-mom's yz-boundary plane
            IJKL = IJK1               !i,   j,   k     (wall)
            IJKH = IJK2               !i,   j,   k+1   (fluid)

! all indices are given relative to ijk1 (wall)
            IPJKH = IP_OF(IJKH)       !i+1, j,   k+1
            IPJKL = KM_OF(IPJKH)      !i+1, j,   k;    IP_OF(IJKL)

! derived/evaluated from fluid cell
!            IMJKH = IM_OF(IJKH)       !i-1, j,   k+1
!            IJPKH = JP_OF(IJKH)       !i,   j+1, k+1
!            IJMKH = JM_OF(IJKH)       !i,   j-1, k+1

!            IPJMKH = JM_OF(IPJKH)     !i+1, j-1, k+1

! derived from fluid cell position
!            IPJMKL = JM_OF(IPJKL)     !i+1, j-1, k;
!            IPJKML = KM_OF(IPJKL)     !i+1, j,   k-1

! NOT derived from fluid cell position
!            IJKML = KM_OF(IJKL)       !i,   j,   k-1;
!            IMJKL = IM_OF(IJKL)       !i-1, j,   k;    KM_OF(IMJKH)
!            IJPKL = JP_OF(IJKL)       !i,   j+1, k;    KM_OF(IJPKH)
!            IJMKL = JM_OF(IJKL)       !i,   j-1, k;    KM_OF(IJMKH)

! i/j/k
            IL = I_OF(IJKL)
            JL = J_OF(IJKL)
            KL = K_OF(IJKL)
            IH = I_OF(IJKH)  !=IL?
            JH = J_OF(IJKH)  !=JL?
            IML = IM1(IL)
            IMH = IM1(IH)    !=IML?
            JML = JM1(JL)
            JMH = JM1(JH)    !=JML?

            O_DX = ODX_E(IL)
            O_DY = ODY(JL)
            O_DZ = ODZ_T(KL)
            O_X = OX_E(IL)

            Us_E = AVG_Z(AVG_X_E(U_S(IJKL,M),U_S(IPJKL,M),IL),&
                         AVG_X_E(U_S(IJKH,M),U_S(IPJKH,M),IH),KL)        !i+1,   j,     k+1/2
            UsW = AVG_Z(AVG_X_E(U_S(IM_OF(IJKL),M),U_S(IJKL,M),IML),&
                        AVG_X_E(U_S(IM_OF(IJKH),M),U_S(IJKH,M),IMH),KL)  !i,     j,     k+1/2
            UsN = AVG_Z(AVG_Y(U_S(IJKL,M),U_S(JP_OF(IJKL),M),JL),&
                        AVG_Y(U_S(IJKH,M),U_S(JP_OF(IJKH),M),JH),KL)     !i+1/2, j+1/2, k+1/2
            UsS = AVG_Z(AVG_Y(U_S(JM_OF(IJKL),M),U_S(IJKL,M),JML),&
                        AVG_Y(U_S(JM_OF(IJKH),M),U_S(IJKH,M),JMH),KL)    !i+1/2, j-1/2, k+1/2
            UsT = U_S(IJKH,M)                                            !i+1/2, j,     k+1
            UsB = U_S(IJKL,M)                                            !i+1/2, j,     k

            VsE = AVG_Z(AVG_Y_N(V_S(JM_OF(IPJKL),M),V_S(IPJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IPJKH),M),V_S(IPJKH,M)),KL)
            VsW = AVG_Z(AVG_Y_N(V_S(JM_OF(IJKL),M),V_S(IJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IJKH),M),V_S(IJKH,M)),KL)
            VsN = AVG_Z(AVG_X(V_S(IJKL,M),V_S(IPJKL,M),IL),&
                        AVG_X(V_S(IJKH,M),V_S(IPJKH,M),IH),KL)
            VsS = AVG_Z(AVG_X(V_S(JM_OF(IJKL),M),V_S(JM_OF(IPJKL),M),IL),&
                        AVG_X(V_S(JM_OF(IJKH),M),V_S(JM_OF(IPJKH),M),IH),KL)
            VsT = AVG_X(AVG_Y_N(V_S(JM_OF(IJKH),M),V_S(IJKH,M)),&
                        AVG_Y_N(V_S(JM_OF(IPJKH),M),V_S(IPJKH,M)),IH)
            VsB = AVG_X(AVG_Y_N(V_S(JM_OF(IJKL),M),V_S(IJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IPJKL),M),V_S(IPJKL,M)),IL)

            WsE = ZERO
            WsW = ZERO
            WsN = ZERO
            WsS = ZERO
! WsT is based on wall (impenetrable=0) & fluid values and their
!    IP neighbors
! WsB is based on wall (zero) value & KM cell value (likely wall=0)
!    and their IP neighbor
            WsT = AVG_Z_T(AVG_X(W_S(IJKL,M),W_S(IPJKL,M),IL),&
                          AVG_X(W_S(IJKH,M),W_S(IPJKH,M),IH))
            WsB = AVG_Z_T(AVG_X(W_S(KM_OF(IJKL),M),W_S(KM_OF(IPJKL),M),IL),&
                          AVG_X(W_S(IJKL,M),W_S(IPJKL,M),IL))

            IF (CYLINDRICAL) THEN
               UscC = AVG_Z(U_S(IJKL,M),U_S(IJKH,M),KL)
               WscC = AVG_X(W_S(IJKL,M),W_S(IPJKL,M),IL)
            ELSE
               UscC = ZERO
               WscC = ZERO
            ENDIF


         CASE ('B')
! xy-boundary plane
! middle of 4 cells is at i+1/2, j, k+1/2 from ijk2
            IJKL = IJK2               !i,   j,   k     (fluid)
            IJKH = IJK1               !i,   j,   k+1   (wall)

! all indices are given relative to ijk2 (fluid)
            IPJKL = IP_OF(IJKL)       !i+1, j,   k;
            IPJKH = KP_OF(IPJKL)      !i+1, j,   k+1;  IP_OF(IJKL)

! derived/evaluated from fluid cell
!            IJKML = KM_OF(IJKL)       !i,   j,   k-1;
!            IMJKL = IM_OF(IJKL)       !i-1, j,   k;
!            IJPKL = JP_OF(IJKL)       !i,   j+1, k;
!            IJMKL = JM_OF(IJKL)       !i,   j-1, k;

!            IPJMKL = JM_OF(IPJKL)     !i+1, j-1, k;
!            IPJKML = KM_OF(IPJKL)     !i+1, j,   k-1

! derived from fluid cell position
!            IPJMKH = JM_OF(IPJKH)     !i+1, j-1, k+1

! NOT derived from fluid cell position
!            IJPKH = JP_OF(IJKH)       !i,   j+1, k+1;  KP_OF(IJPKL)
!            IMJKH = IM_OF(IJKH)       !i-1, j,   k+1;  KP_OF(IMJKL)
!            IJMKH = JM_OF(IJKH)       !i,   j-1, k+1;  KP_OF(IJMKL)

! i/j/k
            IL = I_OF(IJKL)
            JL = J_OF(IJKL)
            KL = K_OF(IJKL)
            IH = I_OF(IJKH)  !=IL?
            JH = J_OF(IJKH)   !=JL?
            IML = IM1(IL)
            IMH = IM1(IH)    !=IML?
            JML = JM1(JL)
            JMH = JM1(JH)    !=JML?

            O_DX = ODX_E(IL)
            O_DY = ODY(JL)
            O_DZ = ODZ_T(KL)
            O_X = OX_E(IL)

            Us_E = AVG_Z(AVG_X_E(U_S(IJKL,M),U_S(IPJKL,M),IL),&
                         AVG_X_E(U_S(IJKH,M),U_S(IPJKH,M),IH),KL)        !i+1,   j,     k+1/2
            UsW = AVG_Z(AVG_X_E(U_S(IM_OF(IJKL),M),U_S(IJKL,M),IML),&
                        AVG_X_E(U_S(IM_OF(IJKH),M),U_S(IJKH,M),IMH),KL)  !i,     j,     k+1/2
            UsN = AVG_Z(AVG_Y(U_S(IJKL,M),U_S(JP_OF(IJKL),M),JL),&
                        AVG_Y(U_S(IJKH,M),U_S(JP_OF(IJKH),M),JH),KL)     !i+1/2, j+1/2, k+1/2
            UsS = AVG_Z(AVG_Y(U_S(JM_OF(IJKL),M),U_S(IJKL,M),JML),&
                        AVG_Y(U_S(JM_OF(IJKH),M),U_S(IJKH,M),JMH),KL)    !i+1/2, j-1/2, k+1/2
            UsT = U_S(IJKH,M)                                            !i+1/2, j,     k+1
            UsB = U_S(IJKL,M)                                            !i+1/2, j,     k

            VsE = AVG_Z(AVG_Y_N(V_S(JM_OF(IPJKL),M),V_S(IPJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IPJKH),M),V_S(IPJKH,M)),KL)
            VsW = AVG_Z(AVG_Y_N(V_S(JM_OF(IJKL),M),V_S(IJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IJKH),M),V_S(IJKH,M)),KL)
            VsN = AVG_Z(AVG_X(V_S(IJKL,M),V_S(IPJKL,M),IL),&
                        AVG_X(V_S(IJKH,M),V_S(IPJKH,M),IH),KL)
            VsS = AVG_Z(AVG_X(V_S(JM_OF(IJKL),M),V_S(JM_OF(IPJKL),M),IL),&
                        AVG_X(V_S(JM_OF(IJKH),M),V_S(JM_OF(IPJKH),M),IH),KL)
            VsT = AVG_X(AVG_Y_N(V_S(JM_OF(IJKH),M),V_S(IJKH,M)),&
                        AVG_Y_N(V_S(JM_OF(IPJKH),M),V_S(IPJKH,M)),IH)
            VsB = AVG_X(AVG_Y_N(V_S(JM_OF(IJKL),M),V_S(IJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IPJKL),M),V_S(IPJKL,M)),IL)

            WsE = ZERO
            WsW = ZERO
            WsN = ZERO
            WsS = ZERO
! WsT is based on wall & fluid (both impenetrable=0) values and their
!    IP neighbors
! WsB is based on fluid (impementrable=0) value & KM cell value
!    (likely fluid?) and their IP neighbor
            WsT = AVG_Z_T(AVG_X(W_S(IJKL,M),W_S(IPJKL,M),IL),&
                          AVG_X(W_S(IJKH,M),W_S(IPJKH,M),IH))
            WsB = AVG_Z_T(AVG_X(W_S(KM_OF(IJKL),M),W_S(KM_OF(IPJKL),M),IL),&
                          AVG_X(W_S(IJKL,M),W_S(IPJKL,M),IL))

            IF (CYLINDRICAL) THEN
               UscC = AVG_Z(U_S(IJKL,M),U_S(IJKH,M),KL)
               WscC = AVG_X(W_S(IJKL,M),W_S(IPJKL,M),IL)
            ELSE
               UscC = ZERO
               WscC = ZERO
            ENDIF


         END SELECT

!--------------------------------------------------------------------
      CASE ('V')
         SELECT CASE (TRIM(FCELL))
         CASE ('T')
! xy-boundary plane
! middle of 4 cells is at i, j+1/2, k+1/2 from ijk1
! loc. is same as z-momentum's xz-boundary plane
            IJKL = IJK1               !i, j, k         (wall)
            IJKH = IJK2               !i, j, k+1       (fluid)

! all indices are given relative to ijk1 (wall)
            IJPKH = JP_OF(IJKH)       !i,   j+1, k+1
            IJPKL = KM_OF(IJPKH)      !i,   j+1, k;    JP_OF(IJKL)

! derived/evaluated from fluid cell
!            IMJKH = IM_OF(IJKH)       !i-1, j,   k+1
!            IPJKH = IP_OF(IJKH)       !i+1, j,   k+1
!            IJMKH = JM_OF(IJKH)       !i,   j-1, k+1

!            IMJPKH = IM_OF(IJPKH)     !i-1, j+1, k+1

! derived from fluid cell position
!            IMJPKL = IM_OF(IJPKL)     !i-1, j+1, k
!            IJPKML = KM_OF(IJPKL)     !i,   j+1, k-1

! NOT derived from fluid cell position
!            IMJKL = IM_OF(IJKL)       !i-1, j,   k;    KM_OF(IMJKH)
!            IJKML = KM_OF(IJKL)       !i,   j,   k-1;
!            IPJKL = IP_OF(IJKL)       !i+1, j,   k     KM_OF(IPJKH)
!            IJMKL = JM_OF(IJKL)       !i,   j-1, k;    KM_OF(IJMKH)

! i/j/k
            IL = I_OF(IJKL)
            IH = I_OF(IJKH)  ! =IL?
            JL = J_OF(IJKL)
            JH = J_OF(IJKH)  ! =JL?
            KL = K_OF(IJKL)
            IML = IM1(IL)
            IMH = IM1(IH)    ! =IML?

            O_DX = ODX(IL)
            O_DY = ODY_N(JL)
            O_DZ = ODZ_T(KL)
            O_X = OX(IL)

            Us_E = AVG_Z(AVG_Y(U_S(IJKL,M),U_S(IJPKL,M),JL),&
                         AVG_Y(U_S(IJKH,M),U_S(IJPKH,M),JH),KL)              ! i+1/2, j+1/2, k+1/2
            UsW = AVG_Z(AVG_Y(U_S(IM_OF(IJKL),M),U_S(IM_OF(IJPKL),M),JL),&
                        AVG_Y(U_S(IM_OF(IJKH),M),U_S(IM_OF(IJPKH),M),JH),KL)  !i-1/2, j+1/2, k+1/2
            UsN = AVG_Z(AVG_X_E(U_S(IM_OF(IJPKL),M),U_S(IJPKL,M),IML),&
                        AVG_X_E(U_S(IM_OF(IJPKH),M),U_S(IJPKH,M),IMH),KL)     !i,     j+1,   k+1/2
            UsS = AVG_Z(AVG_X_E(U_S(IM_OF(IJKL),M),U_S(IJKL,M),IML),&
                        AVG_X_E(U_S(IM_OF(IJKH),M),U_S(IJKH,M),IMH),KL)       !i,     j,     k+1/2
            UsT = AVG_Y(AVG_X_E(U_S(IM_OF(IJKH),M),U_S(IJKH,M),IH),&
                        AVG_X_E(U_S(IM_OF(IJPKH),M),U_S(IJPKH,M),IH),JH)      !i,     j+1/2, k+1
            UsB = AVG_Y(AVG_X_E(U_S(IM_OF(IJKL),M),U_S(IJKL,M),IL),&
                        AVG_X_E(U_S(IM_OF(IJPKL),M),U_S(IJPKL,M),IL),JL)      !i,     j+1/2, k

            VsE = AVG_Z(AVG_X(V_S(IJKL,M),V_S(IP_OF(IJKL),M),IL),&
                        AVG_X(V_S(IJKH,M),V_S(IP_OF(IJKH),M),IH),KL)
            VsW = AVG_Z(AVG_X(V_S(IM_OF(IJKL),M),V_S(IJKL,M),IML),&
                        AVG_X(V_S(IM_OF(IJKH),M),V_S(IJKH,M),IMH),KL)
            VsN = AVG_Z(AVG_Y_N(V_S(IJKL,M),V_S(IJPKL,M)),&
                        AVG_Y_N(V_S(IJKH,M),V_S(IJPKH,M)),KL)
            VsS = AVG_Z(AVG_Y_N(V_S(JM_OF(IJKL),M),V_S(IJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IJKH),M),V_S(IJKH,M)),KL)
            VsT = V_S(IJKH,M)
            VsB = V_S(IJKL,M)

            WsN = ZERO
            WsS = ZERO
            WsE = ZERO
            WsW = ZERO
            WsT = AVG_Z_T(AVG_Y(W_S(IJKL,M),W_S(IJPKL,M),JL),&
                          AVG_Y(W_S(IJKH,M),W_S(IJPKH,M),JH))
            WsB = AVG_Z_T(AVG_Y(W_S(KM_OF(IJKL),M),W_S(KM_OF(IJPKL),M),JL),&
                          AVG_Y(W_S(IJKL,M),W_S(IJPKL,M),JL))

            IF (CYLINDRICAL) THEN
               UscC = AVG_Z(UsB,UsT,KL)
               WscC = AVG_Y(W_S(IJKL,M),W_S(IJPKL,M),JL)
            ELSE
               UscC = ZERO
               WscC = ZERO
            ENDIF

         CASE ('B')
! xy-boundary plane
! middle of 4 cells is at i, j+1/2, k+1/2 from ijk2
! loc. is same as z-momentum's xz-boundary plane
            IJKL = IJK2               !i, j, k         (fluid)
            IJKH = IJK1               !i, j, k+1       (wall)

! all indices are given relative to ijk2 (fluid)
            IJPKL = JP_OF(IJKL)       !i,   j+1, k;
            IJPKH = KP_OF(IJPKL)      !i,   j+1, k+1;  JP_OF(IJKH)

! derived/evaluated from fluid cell
!            IMJKL = IM_OF(IJKL)       !i-1, j,   k
!            IJKML = KM_OF(IJKL)       !i,   j,   k-1
!            IPJKL = IP_OF(IJKL)       !i+1, j,   k
!            IJMKL = JM_OF(IJKL)       !i,   j-1, k

!            IMJPKL = IM_OF(IJPKL)     !i-1, j+1, k
!            IJPKML = KM_OF(IJPKL)     !i,   j+1, k-1

! derived from fluid cell position
!            IMJPKH = IM_OF(IJPKH)     !i-1, j+1, k+1

! NOT derived from fluid cell position
!            IMJKH = IM_OF(IJKH)       !i-1, j,   k+1   KP_OF(IMJKL)
!            IPJKH = IP_OF(IJKH)       !i+1, j,   k+1   KP_OF(IPJKL)
!            IJMKH = JM_OF(IJKH)       !i,   j-1, k+1   KP_OF(IJMKL)

! i/j/k
            IL = I_OF(IJKL)
            IH = I_OF(IJKH)  ! =IL?
            JL = J_OF(IJKL)
            JH = J_OF(IJKH)  ! =JL?
            KL = K_OF(IJKL)
            IML = IM1(IL)
            IMH = IM1(IH)    ! =IML?

            O_DX = ODX(IL)
            O_DY = ODY_N(JL)
            O_DZ = ODZ_T(KL)
            O_X = OX(IL)

            Us_E = AVG_Z(AVG_Y(U_S(IJKL,M),U_S(IJPKL,M),JL),&
                         AVG_Y(U_S(IJKH,M),U_S(IJPKH,M),JH),KL)              ! i+1/2, j+1/2, k+1/2
            UsW = AVG_Z(AVG_Y(U_S(IM_OF(IJKL),M),U_S(IM_OF(IJPKL),M),JL),&
                        AVG_Y(U_S(IM_OF(IJKH),M),U_S(IM_OF(IJPKH),M),JH),KL)  !i-1/2, j+1/2, k+1/2
            UsN = AVG_Z(AVG_X_E(U_S(IM_OF(IJPKL),M),U_S(IJPKL,M),IML),&
                        AVG_X_E(U_S(IM_OF(IJPKH),M),U_S(IJPKH,M),IMH),KL)     !i,     j+1,   k+1/2
            UsS = AVG_Z(AVG_X_E(U_S(IM_OF(IJKL),M),U_S(IJKL,M),IML),&
                        AVG_X_E(U_S(IM_OF(IJKH),M),U_S(IJKH,M),IMH),KL)       !i,     j,     k+1/2
            UsT = AVG_Y(AVG_X_E(U_S(IM_OF(IJKH),M),U_S(IJKH,M),IH),&
                        AVG_X_E(U_S(IM_OF(IJPKH),M),U_S(IJPKH,M),IH),JH)      !i,     j+1/2, k+1
            UsB = AVG_Y(AVG_X_E(U_S(IM_OF(IJKL),M),U_S(IJKL,M),IL),&
                        AVG_X_E(U_S(IM_OF(IJPKL),M),U_S(IJPKL,M),IL),JL)      !i,     j+1/2, k

            VsE = AVG_Z(AVG_X(V_S(IJKL,M),V_S(IP_OF(IJKL),M),IL),&
                        AVG_X(V_S(IJKH,M),V_S(IP_OF(IJKH),M),IH),KL)
            VsW = AVG_Z(AVG_X(V_S(IM_OF(IJKL),M),V_S(IJKL,M),IML),&
                        AVG_X(V_S(IM_OF(IJKH),M),V_S(IJKH,M),IMH),KL)
            VsN = AVG_Z(AVG_Y_N(V_S(IJKL,M),V_S(IJPKL,M)),&
                        AVG_Y_N(V_S(IJKH,M),V_S(IJPKH,M)),KL)
            VsS = AVG_Z(AVG_Y_N(V_S(JM_OF(IJKL),M),V_S(IJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IJKH),M),V_S(IJKH,M)),KL)
            VsT = V_S(IJKH,M)
            VsB = V_S(IJKL,M)

            WsN = ZERO
            WsS = ZERO
            WsE = ZERO
            WsW = ZERO
            WsT = AVG_Z_T(AVG_Y(W_S(IJKL,M),W_S(IJPKL,M),JL),&
                          AVG_Y(W_S(IJKH,M),W_S(IJPKH,M),JH))
            WsB = AVG_Z_T(AVG_Y(W_S(KM_OF(IJKL),M),W_S(KM_OF(IJPKL),M),JL),&
                          AVG_Y(W_S(IJKL,M),W_S(IJPKL,M),JL))

            IF (CYLINDRICAL) THEN
               UscC = AVG_Z(UsB,UsT,KL)
               WscC = AVG_Y(W_S(IJKL,M),W_S(IJPKL,M),JL)
            ELSE
               UscC = ZERO
               WscC = ZERO
            ENDIF

         CASE ('E')
! yz-boundary plane
! middle of 4 cells is at i+1/2, j+1/2, k from ijk1
! loc. is same as x-momentum's xz-plane
            IJKL = IJK1               !i,   j,   k       (wall)
            IJKH = IJK2               !i+1, j,   k       (fluid)

! all indices are given relative to ijk1 (wall)
            IJPKH = JP_OF(IJKH)       !i+1, j+1, k
            IJPKL = IM_OF(IJPKH)      !i,   j+1, k     JP_OF(IJKL)

! derived/evaluated from fluid cell
!            IJKPH = KP_OF(IJKH)       !i+1, j,   k+1
!            IJKMH = KM_OF(IJKH)       !i+1, j,   k-1
!            IJMKH = JM_OF(IJKH)       !i+1, j-1, k

!            IJPKMH = KM_OF(IJPKH)     !i+1, j+1, k-1

! derived from fluid cell position
!            IMJPKL = IM_OF(IJPKL)     !i-1, j+1, k
!            IJPKML = KM_OF(IJPKL)     !i,   j+1, k-1

! NOT derived from fluid cell position
!            IMJKL = IM_OF(IJKL)       !i-1, j,   k;
!            IJMKL = JM_OF(IJKL)       !i,   j-1, k;    IM_OF(IJMKH)
!            IJKPL = KP_OF(IJKL)       !i,   j,   k+1;  IM_OF(IJKPH)
!            IJKML = KM_OF(IJKL)       !i,   j,   k-1;  IM_OF(IJKMH)

! i/j/k
            IL = I_OF(IJKL)
            JL = J_OF(IJKL)
            JH = J_OF(IJKH)  ! =JL?
            KL = K_OF(IJKL)
            KH = K_OF(IJKH)  ! =KL?
            IML = IM1(IL)
            KML = KM1(KL)    ! K_OF(KM_OF(IJKL))
            KMH = KM1(KH)    ! K_OF(KM_OF(IJKH))

            O_DX = ODX_E(IL)
            O_DY = ODY_N(JL)
            O_DZ = ODZ(KL)
            O_X = OX_E(IL)

            Us_E = AVG_X_E(AVG_Y(U_S(IJKL,M),U_S(IJPKL,M),JL),&
                           AVG_Y(U_S(IJKH,M),U_S(IJPKH,M),JH),IL)         !i+1,   j+1/2, k
            UsW = AVG_X_E(AVG_Y(U_S(IM_OF(IJKL),M),U_S(IM_OF(IJPKL),M),JL),&
                          AVG_Y(U_S(IJKL,M),U_S(IJPKL,M),JL),IML)         !i,     j+1/2, k
            UsN = ZERO                                                    !i+1/2, j+1,   k
            UsS = ZERO                                                    !i+1/2, j,     k
            UsT = ZERO                                                    !i+1/2, j+1/2, k+1/2
            UsB = ZERO                                                    !i+1/2, j+1/2, k-1/2

            VsE = V_S(IJKH,M)
            VsW = V_S(IJKL,M)
            VsN = AVG_X(AVG_Y_N(V_S(IJKL,M),V_S(IJPKL,M)),&
                        AVG_Y_N(V_S(IJKH,M),V_S(IJPKH,M)),IL)
            VsS = AVG_X(AVG_Y_N(V_S(JM_OF(IJKL),M),V_S(IJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IJKH),M),V_S(IJKH,M)),IL)
            VsT = AVG_X(AVG_Z(V_S(IJKL,M),V_S(KP_OF(IJKL),M),KL),&
                        AVG_Z(V_S(IJKH,M),V_S(KP_OF(IJKH),M),KH),IL)
            VsB = AVG_X(AVG_Z(V_S(KM_OF(IJKL),M),V_S(IJKL,M),KML),&
                        AVG_Z(V_S(KM_OF(IJKH),M),V_S(IJKH,M),KMH),IL)

            WsE = AVG_Y(AVG_Z_T(W_S(KM_OF(IJKH),M),W_S(IJKH,M)),&
                        AVG_Z_T(W_S(KM_OF(IJPKH),M),W_S(IJPKH,M)),JH)
            WsW = AVG_Y(AVG_Z_T(W_S(KM_OF(IJKL),M),W_S(IJKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IJPKL),M),W_S(IJPKL,M)),JL)
            WsN = AVG_X(AVG_Z_T(W_S(KM_OF(IJPKL),M),W_S(IJPKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IJPKH),M),W_S(IJPKH,M)),IL)
            WsS = AVG_X(AVG_Z_T(W_S(KM_OF(IJKL),M),W_S(IJKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IJKH),M),W_S(IJKH,M)),IL)
            WsT = AVG_X(AVG_Y(W_S(IJKL,M),W_S(IJPKL,M),JL),&
                        AVG_Y(W_S(IJKH,M),W_S(IJPKH,M),JH),IL)
            WsB = AVG_X(AVG_Y(W_S(KM_OF(IJKL),M),W_S(KM_OF(IJPKL),M),JL),&
                        AVG_Y(W_S(KM_OF(IJKH),M),W_S(KM_OF(IJPKH),M),JH),IL)

            IF (CYLINDRICAL) THEN
               UscC = AVG_Y(U_S(IJKL,M),U_S(IJPKL,M),JL)
               WscC = AVG_X(WsW,WsE,IL)
            ELSE
               UscC = ZERO
               WscC = ZERO
            ENDIF


         CASE ('W')
! yz-boundary plane
! middle of 4 cells is at i+1/2, j, k+1/2 from ijk2
! loc. is same as x-momentum's xz-plane
            IJKL = IJK2               !i,   j,   k       (fluid)
            IJKH = IJK1               !i+1, j,   k       (wall)

! all indices are given relative to ijk2 (fluid)
            IJPKL = JP_OF(IJKL)       !i,   j+1, k
            IJPKH = IP_OF(IJPKL)      !i+1, j+1, k     JP_OF(IJKH)

! derived/evaluated from fluid cell
!            IMJKL = IM_OF(IJKL)       !i-1, j,   k
!            IJMKL = JM_OF(IJKL)       !i,   j-1, k
!            IJKPL = KP_OF(IJKL)       !i,   j,   k+1
!            IJKML = KM_OF(IJKL)       !i,   j,   k-1

!            IMJPKL = IM_OF(IJPKL)     !i-1, j+1, k
!            IJPKML = KM_OF(IJPKL)     !i,   j+1, k-1

! derived from fluid cell position
!            IJPKMH = KM_OF(IJPKH)     !i+1, j+1, k-1

! NOT derived from fluid cell position
!            IJKPH = KP_OF(IJKH)       !i+1, j,   k+1;  IP_OF(IJKPL)
!            IJKMH = KM_OF(IJKH)       !i+1, j,   k-1;  IP_OF(IJKML)
!            IJMKH = JM_OF(IJKH)       !i+1, j-1, k;    IP_OF(IJMKL)

! i/j/k
            IL = I_OF(IJKL)
            JL = J_OF(IJKL)
            JH = J_OF(IJKH)  ! =JL?
            KL = K_OF(IJKL)
            KH = K_OF(IJKH)  ! =KL?
            IML = IM1(IL)
            KML = KM1(KL)    ! K_OF(KM_OF(IJKL))
            KMH = KM1(KH)    ! K_OF(KM_OF(IJKH))

            O_DX = ODX_E(IL)
            O_DY = ODY_N(JL)
            O_DZ = ODZ(KL)
            O_X = OX_E(IL)

            Us_E = AVG_X_E(AVG_Y(U_S(IJKL,M),U_S(IJPKL,M),JL),&
                           AVG_Y(U_S(IJKH,M),U_S(IJPKH,M),JH),IL)         !i+1,   j+1/2, k
            UsW = AVG_X_E(AVG_Y(U_S(IM_OF(IJKL),M),U_S(IM_OF(IJPKL),M),JL),&
                          AVG_Y(U_S(IJKL,M),U_S(IJPKL,M),JL),IML)         !i,     j+1/2, k
            UsN = ZERO                                                    !i+1/2, j+1,   k
            UsS = ZERO                                                    !i+1/2, j,     k
            UsT = ZERO                                                    !i+1/2, j+1/2, k+1/2
            UsB = ZERO                                                    !i+1/2, j+1/2, k-1/2

            VsE = V_S(IJKH,M)
            VsW = V_S(IJKL,M)
            VsN = AVG_X(AVG_Y_N(V_S(IJKL,M),V_S(IJPKL,M)),&
                        AVG_Y_N(V_S(IJKH,M),V_S(IJPKH,M)),IL)
            VsS = AVG_X(AVG_Y_N(V_S(JM_OF(IJKL),M),V_S(IJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IJKH),M),V_S(IJKH,M)),IL)

            VsT = AVG_X(AVG_Z(V_S(IJKL,M),V_S(KP_OF(IJKL),M),KL),&
                        AVG_Z(V_S(IJKH,M),V_S(KP_OF(IJKH),M),KH),IL)
            VsB = AVG_X(AVG_Z(V_S(KM_OF(IJKL),M),V_S(IJKL,M),KML),&
                        AVG_Z(V_S(KM_OF(IJKH),M),V_S(IJKH,M),KMH),IL)

            WsE = AVG_Y(AVG_Z_T(W_S(KM_OF(IJKH),M),W_S(IJKH,M)),&
                        AVG_Z_T(W_S(KM_OF(IJPKH),M),W_S(IJPKH,M)),JH)
            WsW = AVG_Y(AVG_Z_T(W_S(KM_OF(IJKL),M),W_S(IJKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IJPKL),M),W_S(IJPKL,M)),JL)
            WsN = AVG_X(AVG_Z_T(W_S(KM_OF(IJPKL),M),W_S(IJPKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IJPKH),M),W_S(IJPKH,M)),IL)
            WsS = AVG_X(AVG_Z_T(W_S(KM_OF(IJKL),M),W_S(IJKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IJKH),M),W_S(IJKH,M)),IL)
            WsT = AVG_X(AVG_Y(W_S(IJKL,M),W_S(IJPKL,M),JL),&
                        AVG_Y(W_S(IJKH,M),W_S(IJPKH,M),JH),IL)
            WsB = AVG_X(AVG_Y(W_S(KM_OF(IJKL),M),W_S(KM_OF(IJPKL),M),JL),&
                        AVG_Y(W_S(KM_OF(IJKH),M),W_S(KM_OF(IJPKH),M),JH),IL)

            IF (CYLINDRICAL) THEN
               UscC = AVG_Y(U_S(IJKL,M),U_S(IJPKL,M),JL)
               WscC = AVG_X(WsW,WsE,IL)
            ELSE
               UscC = ZERO
               WscC = ZERO
            ENDIF

         END SELECT

!--------------------------------------------------------------------
      CASE ('W')
         SELECT CASE (TRIM(FCELL))
         CASE ('N')
! xz-boundary boundary
! middle of 4 cells is at i, j+1/2, k+1/2 from ijk1
! loc. is same as y-momentum's xy-plane
            IJKL = IJK1               !i,   j,   k (wall)
            IJKH = IJK2               !i,   j+1, k (fluid)

! all indices are given relative to ijk1 (wall)
            IJKPH = KP_OF(IJKH)       !i,   j+1, k+1
            IJKPL = JM_OF(IJKPH)      !i,   j,   k+1;  KP_OF(IJKL)

! derived/evaluated from fluid cell
!            IMJKH = IM_OF(IJKH)       !i-1, j+1, k
!            IPJKH = IP_OF(IJKH)       !i+1, j+1, k
!            IJKMH = KM_OF(IJKH)       !i,   j+1, k-1

!            IMJKPH = IM_OF(IJKPH)     !i-1, j+1, k+1

! derived from fluid cell position
!            IMJKPL = IM_OF(IJKPL)     !i-1, j,   k+1
!            IJMKPL = JM_OF(IJKPL)     !i,   j-1, k

! NOT derived from fluid cell position
!            IJMKL = JM_OF(IJKL)       !i,   j-1, k
!            IMJKL = IM_OF(IJKL)       !i-1, j,   k;    JM_OF(IMJKH)
!            IPJKL = IP_OF(IJKL)       !i+1, j,   k;    JM_OF(IPJKH)
!            IJKML = KM_OF(IJKL)       !i,   j,   k-1;  JM_OF(IJKMH)

! i/j/k
            IL = I_OF(IJKL)
            IH = I_OF(IJKH)  ! =IL?
            JL = J_OF(IJKL)
            KL = K_OF(IJKL)
            KH = K_OF(IJKH)  ! =KL?
            IML = IM1(IL)    ! I_OF(IM_OF(IJKL))
            IMH = IM1(IH)    ! I_OF(IM_OF(IJKH))

            O_DX = ODX(IL)
            O_DY = ODY_N(JL)
            O_DZ = ODZ_T(KL)
            O_X = OX(IL)

            Us_E = AVG_Y(AVG_Z(U_S(IJKL,M),U_S(IJKPL,M),KL),&
                         AVG_Z(U_S(IJKH,M),U_S(IJKPH,M),KH),JL)                 !i+1/2, j+1/2, k+1/2
            UsW = AVG_Y(AVG_Z(U_S(IM_OF(IJKL),M),U_S(IM_OF(IJKPL),M),KL),&
                        AVG_Z(U_S(IM_OF(IJKH),M),U_S(IM_OF(IJKPH),M),KH),JL)    !i-1/2, j+1/2, k+1/2
            UsN = AVG_X_E(AVG_Z(U_S(IM_OF(IJKH),M),U_S(IM_OF(IJKPH),M),KH),&
                          AVG_Z(U_S(IJKH,M),U_S(IJKPH,M),KH),IMH)               !i,     j+1,   k+1/2
            UsS = AVG_X_E(AVG_Z(U_S(IM_OF(IJKL),M),U_S(IM_OF(IJKPL),M),KL),&
                          AVG_Z(U_S(IJKL,M),U_S(IJKPL,M),KL),IML)               !i,     j,     k+1/2
            UsT = AVG_Y(AVG_X_E(U_S(IM_OF(IJKPL),M),U_S(IJKPL,M),IML),&
                        AVG_X_E(U_S(IM_OF(IJKPH),M),U_S(IJKPH,M),IMH),JL)       !i,     j+1/2, k+1
            UsB = AVG_Y(AVG_X_E(U_S(IM_OF(IJKL),M),U_S(IJKL,M),IML),&
                        AVG_X_E(U_S(IM_OF(IJKH),M),U_S(IJKH,M),IMH),JL)         !i,     j+1/2, k

            VsE = ZERO
            VsW = ZERO
            VsN = AVG_Y_N(AVG_Z(V_S(IJKL,M),V_S(IJKPL,M),KL),&
                          AVG_Z(V_S(IJKH,M),V_S(IJKPH,M),KH))
            VsS = AVG_Z(AVG_Y_N(V_S(JM_OF(IJKL),M),V_S(IJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IJKPL),M),V_S(IJKPL,M)),KL)
            VsT = ZERO
            VsB = ZERO

            WsE = AVG_Y(AVG_X(W_S(IJKL,M),W_S(IP_OF(IJKL),M),IL),&
                        AVG_X(W_S(IJKH,M),W_S(IP_OF(IJKH),M),IH),JL)
            WsW = AVG_Y(AVG_X(W_S(IM_OF(IJKL),M),W_S(IJKL,M),IML),&
                        AVG_X(W_S(IM_OF(IJKH),M),W_S(IJKH,M),IMH),JL)
            WsN = W_S(IJKH,M)
            WsS = W_S(IJKL,M)
            WsT = AVG_Y(AVG_Z_T(W_S(IJKL,M),W_S(IJKPL,M)),&
                        AVG_Z_T(W_S(IJKH,M),W_S(IJKPH,M)),JL)
            WsB = AVG_Y(AVG_Z_T(W_S(KM_OF(IJKL),M),W_S(IJKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IJKH),M),W_S(IJKH,M)),JL)

            IF (CYLINDRICAL) THEN
               UscC = AVG_Z(UsB,UsT,KL)
               WscC = AVG_Y(W_S(IJKL,M),W_S(IJKH,M),JL)
            ELSE
               UscC = ZERO
               WscC = ZERO
            ENDIF


         CASE ('S')
! xz-boundary boundary
! middle of 4 cells is at i, j+1/2, k+1/2 from ijk2
! loc. is same as y-momentum's xy-plane
            IJKL = IJK2               !i,   j,   k (fluid)
            IJKH = IJK1               !i,   j+1, k (wall)

! all indices are given relative to ijk2 (fluid)
            IJKPL = KP_OF(IJKL)       !i,   j+1, k+1
            IJKPH = JP_OF(IJKPL)      !i,   j,   k+1;  KP_OF(IJKH)

! derived/evaluated from fluid cell
!            IJMKL = JM_OF(IJKL)       !i,   j-1, k
!            IMJKL = IM_OF(IJKL)       !i-1, j,   k
!            IPJKL = IP_OF(IJKL)       !i+1, j,   k
!            IJKML = KM_OF(IJKL)       !i,   j,   k-1

!            IMJKPL = IM_OF(IJKPL)     !i-1, j,   k+1
!            IJMKPL = JM_OF(IJKPL)     !i,   j-1, k

! derived from fluid cell position
!            IMJKPH = IM_OF(IJKPH)     !i-1, j+1, k+1

! NOT derived from fluid cell position
!            IMJKH = IM_OF(IJKH)       !i-1, j+1, k;    JP_OF(IMJKL)
!            IPJKH = IP_OF(IJKH)       !i+1, j+1, k;    JP_OF(IPJKL)
!            IJKMH = KM_OF(IJKH)       !i,   j+1, k-1;  JP_OF(IJKML)

! i/j/k
            IL = I_OF(IJKL)
            IH = I_OF(IJKH)  ! =IL?
            JL = J_OF(IJKL)
            KL = K_OF(IJKL)
            KH = K_OF(IJKH)  ! =KL?
            IML = IM1(IL)    ! I_OF(IM_OF(IJKL))
            IMH = IM1(IH)    ! I_OF(IM_OF(IJKH))

            O_DX = ODX(IL)
            O_DY = ODY_N(JL)
            O_DZ = ODZ_T(KL)
            O_X = OX(IL)

            Us_E = AVG_Y(AVG_Z(U_S(IJKL,M),U_S(IJKPL,M),KL),&
                         AVG_Z(U_S(IJKH,M),U_S(IJKPH,M),KH),JL)                 !i+1/2, j+1/2, k+1/2
            UsW = AVG_Y(AVG_Z(U_S(IM_OF(IJKL),M),U_S(IM_OF(IJKPL),M),KL),&
                        AVG_Z(U_S(IM_OF(IJKH),M),U_S(IM_OF(IJKPH),M),KH),JL)    !i-1/2, j+1/2, k+1/2
            UsN = AVG_X_E(AVG_Z(U_S(IM_OF(IJKH),M),U_S(IM_OF(IJKPH),M),KH),&
                          AVG_Z(U_S(IJKH,M),U_S(IJKPH,M),KH),IMH)               !i,     j+1,   k+1/2
            UsS = AVG_X_E(AVG_Z(U_S(IM_OF(IJKL),M),U_S(IM_OF(IJKPL),M),KL),&
                          AVG_Z(U_S(IJKL,M),U_S(IJKPL,M),KL),IML)               !i,     j,     k+1/2
            UsT = AVG_Y(AVG_X_E(U_S(IM_OF(IJKPL),M),U_S(IJKPL,M),IML),&
                        AVG_X_E(U_S(IM_OF(IJKPH),M),U_S(IJKPH,M),IMH),JL)       !i,     j+1/2, k+1
            UsB = AVG_Y(AVG_X_E(U_S(IM_OF(IJKL),M),U_S(IJKL,M),IML),&
                        AVG_X_E(U_S(IM_OF(IJKH),M),U_S(IJKH,M),IMH),JL)         !i,     j+1/2, k

            VsE = ZERO
            VsW = ZERO
            VsN = AVG_Y_N(AVG_Z(V_S(IJKL,M),V_S(IJKPL,M),KL),&
                          AVG_Z(V_S(IJKH,M),V_S(IJKPH,M),KH))
            VsS = AVG_Z(AVG_Y_N(V_S(JM_OF(IJKL),M),V_S(IJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IJKPL),M),V_S(IJKPL,M)),KL)
            VsT = ZERO
            VsB = ZERO

            WsE = AVG_Y(AVG_X(W_S(IJKL,M),W_S(IP_OF(IJKL),M),IL),&
                        AVG_X(W_S(IJKH,M),W_S(IP_OF(IJKH),M),IH),JL)
            WsW = AVG_Y(AVG_X(W_S(IM_OF(IJKL),M),W_S(IJKL,M),IML),&
                        AVG_X(W_S(IM_OF(IJKH),M),W_S(IJKH,M),IMH),JL)
            WsN = W_S(IJKH,M)
            WsS = W_S(IJKL,M)
            WsT = AVG_Y(AVG_Z_T(W_S(IJKL,M),W_S(IJKPL,M)),&
                        AVG_Z_T(W_S(IJKH,M),W_S(IJKPH,M)),JL)
            WsB = AVG_Y(AVG_Z_T(W_S(KM_OF(IJKL),M),W_S(IJKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IJKH),M),W_S(IJKH,M)),JL)

            IF (CYLINDRICAL) THEN
               UscC = AVG_Z(UsB,UsT,KL)
               WscC = AVG_Y(W_S(IJKL,M),W_S(IJKH,M),JL)
            ELSE
               UscC = ZERO
               WscC = ZERO
            ENDIF


         CASE ('E')
! yz-boundary plane
! middle of 4 cells is at i+1/2, j, k+1/2 from ijk1
! loc. is same as u-mom's xy-plane
            IJKL = IJK1               !i,   j,   k (wall)
            IJKH = IJK2               !i+1, j,   k (fluid)

! all indices are given relative to ijk1 (wall)
            IJKPH = KP_OF(IJKH)       !i+1, j,   k+1
            IJKPL = IM_OF(IJKPH)      !i,   j,   k+1;  KP_OF(IJKL)

! derived/evaluated from fluid cell
!            IJMKH = JM_OF(IJKH)       !i+1, j-1, k
!            IJPKH = JP_OF(IJKH)       !i+1, j+1, k
!            IJKMH = KM_OF(IJKH)       !i+1, j,   k-1

!            IJMKPH = JM_OF(IJKPH)     !i+1, j-1, k+1

! derived from fluid cell position
!            IMJKPL = IM_OF(IJKPL)     !i-1, j,   k+1
!            IJMKPL = JM_OF(IJKPL)     !i,   j-1, k+1

! NOT derived from fluid cell position
!            IMJKL = IM_OF(IJKL)       !i-1, j,   k
!            IJMKL = JM_OF(IJKL)       !i,   j-1, k;    IM_OF(IJMKH)
!            IJPKL = JP_OF(IJKL)       !i,   j+1, k;    IM_OF(IJPKH)
!            IJKML = KM_OF(IJKL)       !i,   j,   k-1;  IM_OF(IJKMH)

! i/j/k
            IL = I_OF(IJKL)
            JL = J_OF(IJKL)
            JH = J_OF(IJKH)  ! =JL?
            KL = K_OF(IJKL)
            KH = K_OF(IJKH)  ! =KL?
            IML = IM1(IL)    ! I_OF(IM_OF(IJKL))
            JML = JM1(JL)    ! J_OF(JM_OF(IJKL))
            JMH = JM1(JH)    ! J_OF(JM_OF(IJKH))

            O_DX = ODX_E(IL)
            O_DY = ODY(JL)
            O_DZ = ODZ_T(KL)
            O_X = OX_E(IL)

            Us_E = AVG_X_E(AVG_Z(U_S(IJKL,M),U_S(IJKPL,M),KL),&
                           AVG_Z(U_S(IJKH,M),U_S(IJKPH,M),KH),IL)              !i+1,   j,     k+1/2
            UsW = AVG_X_E(AVG_Z(U_S(IM_OF(IJKL),M),U_S(IM_OF(IJKPL),M),KL),&
                          AVG_Z(U_S(IJKL,M),U_S(IJKPL,M),KL),IML)              !i,     j,     k+1/2
            UsN = ZERO                                                         !i+1/2, j+1/2, k+1/2
            UsS = ZERO                                                         !i+1/2, j-1/2, k+1/2
            UsT = ZERO                                                         !i+1/2, j,     k+1
            UsB = ZERO                                                         !i+1/2, j,     k

            VsE = AVG_Y_N(AVG_Z(V_S(JM_OF(IJKH),M),V_S(JM_OF(IJKPH),M),KH),&
                          AVG_Z(V_S(IJKH,M),V_S(IJKPH,M),KH))
            VsW = AVG_Y_N(AVG_Z(V_S(JM_OF(IJKL),M),V_S(JM_OF(IJKPL),M),KL),&
                          AVG_Z(V_S(IJKL,M),V_S(IJKPL,M),KL))
            VsN = AVG_X(AVG_Z(V_S(IJKL,M),V_S(IJKPL,M),KL),&
                        AVG_Z(V_S(IJKH,M),V_S(IJKPH,M),KH),IL)
            VsS = AVG_X(AVG_Z(V_S(JM_OF(IJKL),M),V_S(JM_OF(IJKPL),M),KL),&
                        AVG_Z(V_S(JM_OF(IJKH),M),V_S(JM_OF(IJKPH),M),KH),IL)
            VsT = AVG_X(AVG_Y_N(V_S(JM_OF(IJKPL),M),V_S(IJKPL,M)),&
                        AVG_Y_N(V_S(JM_OF(IJKPH),M),V_S(IJKPH,M)),IL)
            VsB = AVG_X(AVG_Y_N(V_S(JM_OF(IJKL),M),V_S(IJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IJKH),M),V_S(IJKH,M)),IL)

            WsE = W_S(IJKH,M)
            WsW = W_S(IJKL,M)
            WsN = AVG_X(AVG_Y(W_S(IJKL,M),W_S(JP_OF(IJKL),M),JL),&
                        AVG_Y(W_S(IJKH,M),W_S(JP_OF(IJKH),M),JH),IL)
            WsS = AVG_X(AVG_Y(W_S(JM_OF(IJKL),M),W_S(IJKL,M),JML),&
                        AVG_Y(W_S(JM_OF(IJKH),M),W_S(IJKH,M),JMH),IL)
            WsT = AVG_X(AVG_Z_T(W_S(IJKL,M),W_S(IJKPL,M)),&
                        AVG_Z_T(W_S(IJKH,M),W_S(IJKPH,M)),IL)
            WsB = AVG_X(AVG_Z_T(W_S(KM_OF(IJKL),M),W_S(IJKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IJKH),M),W_S(IJKH,M)),IL)

            IF (CYLINDRICAL) THEN
               UscC = AVG_Z(U_S(IJKL,M),U_S(IJKPL,M),KL)
               WscC = AVG_X(W_S(IJKL,M),W_S(IJKH,M),IL)
            ELSE
               UscC = ZERO
               WscC = ZERO
            ENDIF


         CASE ('W')
! yz-boundary plane
! middle of 4 celsl is at i+1/2, j, k+1/2
! loc. is same as u-mom's xy-plane
            IJKL = IJK2               !i,   j,   k (fluid)
            IJKH = IJK1               !i+1, j,   k (wall)

! all indices are given relative to ijk2 (fluid)
            IJKPL = KP_OF(IJKL)       !i+1, j,   k+1
            IJKPH = IP_OF(IJKPL)      !i,   j,   k+1;  KP_OF(IJKL)

! derived/evaluated from fluid cell
!            IMJKL = IM_OF(IJKL)       !i-1, j,   k
!            IJMKL = JM_OF(IJKL)       !i,   j-1, k
!            IJPKL = JP_OF(IJKL)       !i,   j+1, k
!            IJKML = KM_OF(IJKL)       !i,   j,   k-1

!            IMJKPL = IM_OF(IJKPL)     !i-1, j,   k+1
!            IJMKPL = JM_OF(IJKPL)     !i,   j-1, k+1

! derived from fluid cell position
!            IJMKPH = JM_OF(IJKPH)     !i+1, j-1, k+1

! NOT derived from fluid cell position
!            IJMKH = JM_OF(IJKH)       !i+1, j-1, k;    IP_OF(IJMKL)
!            IJPKH = JP_OF(IJKH)       !i+1, j+1, k;    IP_OF(IJPKL)
!            IJKMH = KM_OF(IJKH)       !i+1, j,   k-1;  IP_OF(IJKML)

! i/j/k
            IL = I_OF(IJKL)
            JL = J_OF(IJKL)
            JH = J_OF(IJKH)  ! =JL?
            KL = K_OF(IJKL)
            KH = K_OF(IJKH)  ! =KL?
            IML = IM1(IL)    ! I_OF(IM_OF(IJKL))
            JML = JM1(JL)    ! J_OF(JM_OF(IJKL))
            JMH = JM1(JH)    ! J_OF(JM_OF(IJKH))

            O_DX = ODX_E(IL)
            O_DY = ODY(JL)
            O_DZ = ODZ_T(KL)
            O_X = OX_E(IL)

            Us_E = AVG_X_E(AVG_Z(U_S(IJKL,M),U_S(IJKPL,M),KL),&
                           AVG_Z(U_S(IJKH,M),U_S(IJKPH,M),KH),IL)              !i+1,   j,     k+1/2
            UsW = AVG_X_E(AVG_Z(U_S(IM_OF(IJKL),M),U_S(IM_OF(IJKPL),M),KL),&
                          AVG_Z(U_S(IJKL,M),U_S(IJKPL,M),KL),IML)              !i,     j,     k+1/2
            UsN = ZERO                                                         !i+1/2, j+1/2, k+1/2
            UsS = ZERO                                                         !i+1/2, j-1/2, k+1/2
            UsT = ZERO                                                         !i+1/2, j,     k+1
            UsB = ZERO                                                         !i+1/2, j,     k

            VsE = AVG_Y_N(AVG_Z(V_S(JM_OF(IJKH),M),V_S(JM_OF(IJKPH),M),KH),&
                          AVG_Z(V_S(IJKH,M),V_S(IJKPH,M),KH))
            VsW = AVG_Y_N(AVG_Z(V_S(JM_OF(IJKL),M),V_S(JM_OF(IJKPL),M),KL),&
                          AVG_Z(V_S(IJKL,M),V_S(IJKPL,M),KL))

            VsN = AVG_X(AVG_Z(V_S(IJKL,M),V_S(IJKPL,M),KL),&
                        AVG_Z(V_S(IJKH,M),V_S(IJKPH,M),KH),IL)
            VsS = AVG_X(AVG_Z(V_S(JM_OF(IJKL),M),V_S(JM_OF(IJKPL),M),KL),&
                        AVG_Z(V_S(JM_OF(IJKH),M),V_S(JM_OF(IJKPH),M),KH),IL)
            VsT = AVG_X(AVG_Y_N(V_S(JM_OF(IJKPL),M),V_S(IJKPL,M)),&
                        AVG_Y_N(V_S(JM_OF(IJKPH),M),V_S(IJKPH,M)),IL)
            VsB = AVG_X(AVG_Y_N(V_S(JM_OF(IJKL),M),V_S(IJKL,M)),&
                        AVG_Y_N(V_S(JM_OF(IJKH),M),V_S(IJKH,M)),IL)

            WsE = W_S(IJKH,M)
            WsW = W_S(IJKL,M)
            WsN = AVG_X(AVG_Y(W_S(IJKL,M),W_S(JP_OF(IJKL),M),JL),&
                        AVG_Y(W_S(IJKH,M),W_S(JP_OF(IJKH),M),JH),IL)
            WsS = AVG_X(AVG_Y(W_S(JM_OF(IJKL),M),W_S(IJKL,M),JML),&
                        AVG_Y(W_S(JM_OF(IJKH),M),W_S(IJKH,M),JMH),IL)
            WsT = AVG_X(AVG_Z_T(W_S(IJKL,M),W_S(IJKPL,M)),&
                        AVG_Z_T(W_S(IJKH,M),W_S(IJKPH,M)),IL)
            WsB = AVG_X(AVG_Z_T(W_S(KM_OF(IJKL),M),W_S(IJKL,M)),&
                        AVG_Z_T(W_S(KM_OF(IJKH),M),W_S(IJKH,M)),IL)

            IF (CYLINDRICAL) THEN
               UscC = AVG_Z(U_S(IJKL,M),U_S(IJKPL,M),KL)
               WscC = AVG_X(W_S(IJKL,M),W_S(IJKH,M),IL)
            ELSE
               UscC = ZERO
               WscC = ZERO
            ENDIF

        END SELECT   ! end select FCELL (e/w, n/s, t/b)
      END SELECT     ! end select COM (u,v,w)



   RETURN
   END SUBROUTINE GET_NEIGHBOR_VEL_AT_WALL



END MODULE CALC_S_DDOT_MOD
