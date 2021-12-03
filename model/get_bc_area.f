MODULE GET_BC_AREA_MOD

   USE bc, only: bc_i_e, bc_i_w, bc_j_n, bc_j_s, bc_k_t, bc_k_b
   USE bc, only: bc_type_enum, bc_defined, dimension_bc, is_cg
   USE bc, only: mass_inflow, mass_outflow, bc_area, bc_plane, bc_vol
   USE compar, only: ijkstart3, ijkend3, dead_cell_at
   USE cutcell, only: bc_id, area_cut, cut_cell_at
   USE functions, only: im_of, jm_of, km_of, is_on_mype_owns, funijk
   USE geometry, only: axy, axz, ayz, vol, dx, dy, dz, x_e, x
   USE indices, only: i_of, j_of, k_of
   USE mpi_utility, only: global_all_sum
   USE param1, only: zero

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: GET_BC_AREA                                            C
!  Purpose: Compute area of boundary surfaces                          C
!                                                                      C
!  Author: M. Syamlal                                 Date: 29-JUL-92  C
!  Reviewer: W. Rogers                                Date: 11-DEC-92  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
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
   SUBROUTINE GET_BC_AREA

      IMPLICIT NONE

      INTEGER :: BCV
      INTEGER ::  I, J, K

!-----------------------------------------------

      DO BCV = 1, DIMENSION_BC
         IF (BC_DEFINED(BCV)) THEN
            BC_AREA(BCV) = ZERO
            IF (BC_PLANE(BCV) == 'W') THEN
               I = BC_I_W(BCV)
               DO K = BC_K_B(BCV), BC_K_T(BCV)
                  J = BC_J_S(BCV)
                  IF (BC_J_N(BCV) - BC_J_S(BCV) + 1 > 0) THEN
                     BC_AREA(BCV) = BC_AREA(BCV) + SUM(DY(BC_J_S(BCV):BC_J_N(BCV))*&
                        X_E(I-1)*DZ(K))
                     J = BC_J_N(BCV) + 1
                  ENDIF
               END DO
            ELSE IF (BC_PLANE(BCV) == 'E') THEN
               I = BC_I_W(BCV)
               DO K = BC_K_B(BCV), BC_K_T(BCV)
                  J = BC_J_S(BCV)
                  IF (BC_J_N(BCV) - BC_J_S(BCV) + 1 > 0) THEN
                     BC_AREA(BCV) = BC_AREA(BCV) + SUM(DY(BC_J_S(BCV):BC_J_N(BCV))*&
                        X_E(I)*DZ(K))
                     J = BC_J_N(BCV) + 1
                  ENDIF
               END DO
            ELSE IF (BC_PLANE(BCV)=='S' .OR. BC_PLANE(BCV)=='N') THEN
               J = BC_J_S(BCV)
               DO K = BC_K_B(BCV), BC_K_T(BCV)
                  I = BC_I_W(BCV)
                  IF (BC_I_E(BCV) - BC_I_W(BCV) + 1 > 0) THEN
                     BC_AREA(BCV) = BC_AREA(BCV) + SUM(DX(BC_I_W(BCV):BC_I_E(BCV))*&
                        X(BC_I_W(BCV):BC_I_E(BCV))*DZ(K))
                     I = BC_I_E(BCV) + 1
                  ENDIF
               END DO
            ELSE IF (BC_PLANE(BCV)=='B' .OR. BC_PLANE(BCV)=='T') THEN
               K = BC_K_B(BCV)
               DO J = BC_J_S(BCV), BC_J_N(BCV)
                  I = BC_I_W(BCV)
                  IF (BC_I_E(BCV) - BC_I_W(BCV) + 1 > 0) THEN
                     BC_AREA(BCV) = BC_AREA(BCV) + SUM(DX(BC_I_W(BCV):BC_I_E(BCV))*&
                        DY(J))
                     I = BC_I_E(BCV) + 1
                  ENDIF
               END DO
            ENDIF
         ENDIF
      END DO

      RETURN
   END SUBROUTINE GET_BC_AREA


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: CG_GET_BC_AREA                                         C
!  Purpose: Compute area of cut-cell boundary surfaces                 C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 15-MAR-13  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
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
   SUBROUTINE CG_GET_BC_AREA

      IMPLICIT NONE

      INTEGER :: BCV,BCID
      INTEGER ::  I, J, K , IJK, IJK2
      INTEGER :: BCT

!-----------------------------------------------

      DO BCV = 1, DIMENSION_BC
         IF (BC_DEFINED(BCV)) THEN

            BC_AREA(BCV) = ZERO
            BC_VOL(BCV) = ZERO

            IF(IS_CG(BC_TYPE_ENUM(BCV))) THEN

! For cut-cell boundaries, add the area of each cut face

               DO IJK = IJKSTART3, IJKEND3
                  I = I_OF(IJK)
                  J = J_OF(IJK)
                  K = K_OF(IJK)
                  IF (.NOT.IS_ON_myPE_OWNS(I,J,K)) CYCLE
                  IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
                  IF(CUT_CELL_AT(IJK)) THEN
                     BCID = BC_ID(IJK)
                     IF(BCID > 0 ) THEN
                        BCT = BC_TYPE_ENUM(BCID)
                        IF(BCID==BCV) BC_AREA(BCV) = BC_AREA(BCV) + Area_CUT(IJK)
                        IF(BCID==BCV) BC_VOL(BCV)  = BC_VOL(BCV)  + VOL(IJK)
                     ENDIF
                  ENDIF
               END DO

            ELSE

! For regular boundaries, add the true area of the faces (they could be truncated,
! for example AYZ(IJK) coulb be less than DY(J)*DZ(K)

            DO K = BC_K_B(BCV), BC_K_T(BCV)
               DO J = BC_J_S(BCV), BC_J_N(BCV)
                  DO I = BC_I_W(BCV), BC_I_E(BCV)

                     IF (.NOT.IS_ON_myPE_OWNS(I,J,K)) CYCLE
                     IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells

                     IJK = FUNIJK(I,J,K)
                     SELECT CASE (TRIM(BC_PLANE(BCV)))
                     CASE ('W')
                        IJK2 = IM_OF(IJK)
                        BC_AREA(BCV) = BC_AREA(BCV) + AYZ(IJK2)
                     CASE ('E')
                        BC_AREA(BCV) = BC_AREA(BCV) + AYZ(IJK)
                     CASE ('S')
                        IJK2 = JM_OF(IJK)
                        BC_AREA(BCV) = BC_AREA(BCV) + AXZ(IJK2)
                     CASE ('N')
                        BC_AREA(BCV) = BC_AREA(BCV) + AXZ(IJK)
                     CASE ('B')
                        IJK2 = KM_OF(IJK)
                        BC_AREA(BCV) = BC_AREA(BCV) + AXY(IJK2)
                     CASE ('T')
                        BC_AREA(BCV) = BC_AREA(BCV) + AXY(IJK)
                     END SELECT
                  END DO
               END DO
            END DO

            ENDIF

          CALL GLOBAL_ALL_SUM(BC_AREA(BCV))
          CALL GLOBAL_ALL_SUM(BC_VOL(BCV))

         ENDIF
      END DO

      RETURN

   END SUBROUTINE CG_GET_BC_AREA

END MODULE GET_BC_AREA_MOD
