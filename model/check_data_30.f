#include "error.inc"

MODULE CHECK_DATA_30_MOD

   use compar, only: DEAD_CELL_AT
   use compar, only: ISTART2, IEND2
   use compar, only: JSTART2, JEND2
   use compar, only: KSTART2, KEND2
   use error_manager, only: err_msg, iVAR, loglevel_error, loglevel_info, log_message
   use fldvar, only: ROP_s, T_G, T_S, X_G, X_S
   use functions, only: FLOW_AT, FUNIJK, WALL_AT
   use mms, only: USE_MMS
   use mpi_utility, only: GLOBAL_ALL_MAX, GLOBAL_ALL_MIN, GLOBAL_ALL_OR, GLOBAL_ALL_SUM
   use open_files_mod, only: open_pe_log, close_pe_log
   use param, only: DIMENSION_M
   use param1, only: ZERO, ONE, UNDEFINED, SMALL_NUMBER
   use physprop, only: C_PG, C_PS, DIF_s, DIF_g
   use physprop, only: K_G, K_S, MU_G, MW_MIX_G, SMAX, NMAX
   use run, only: ENERGY_EQ, SPECIES_EQ, TIME
   use rxns, only: R_GP, R_SP, RoX_GC, RoX_SC, sum_r_g, sum_r_s, r_phase, use_rrates
   use toleranc, only: TMIN, TMAX, TOL_COM
   use visc_g, only: MU_GT, LAMBDA_GT
   use visc_s, only: MU_S, LAMBDA_S

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Module name: CHECK_DATA_30                                          !
!  Author: M. Syamlal                                 Date: 27-OCT-92  !
!                                                                      !
!  Purpose: Check whether the sum of reaction rates is zero and the    !
!  sum of mass fractions is 1.0 and EP_g >= EP_Star.                   !
!           and EP_g >= EP_star. Set miscellaneous constants           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE CHECK_DATA_30

      IMPLICIT NONE

! Local variables:
!---------------------------------------------------------------------//
! Flags to report stats on species equations.
      LOGICAL :: REPORT_SPECIES(0:DIMENSION_M)
!......................................................................!

! Check physical properties in inflow/outflow cells.
      IF (.NOT. USE_MMS) CALL CHECK_FLOW_CELL_PROPS
! Verify physical values for field variables.
      CALL CHECK_PHYSICAL_BOUNDS(REPORT_SPECIES)
! Generate species report if needed.
      IF (ANY(REPORT_SPECIES)) CALL REPORT_SPECIES_STATS(REPORT_SPECIES)
! Check interphase mass transfer.
      IF (USE_RRATES) CALL CHECK_RXN_MASS_BALANCE

      RETURN

   END SUBROUTINE CHECK_DATA_30

!----------------------------------------------------------------------!
!  Module name: CHECK_FLOW_PROPS                                       !
!  Author: M. Syamlal                                 Date: 27-OCT-92  !
!                                                                      !
!  Purpose: Verify that inflow/outflow cells do not contain physical   !
!  properties for specified variables.                                 !
!                                                                      !
!----------------------------------------------------------------------!
   SUBROUTINE CHECK_FLOW_CELL_PROPS

      IMPLICIT NONE

! Local variables:
!---------------------------------------------------------------------//
! Loop counters
      INTEGER :: I, J, K, IJK
      INTEGER :: M, N
! Integer error flag.
      INTEGER :: IER, mype_ier
!......................................................................!
      CHARACTER(:), allocatable:: error_message

      error_message = ""

      IER = 0

      DO K = KSTART2, KEND2
      DO J = JSTART2, JEND2
      DO I = ISTART2, IEND2

         IF (DEAD_CELL_AT(I, J, K)) CYCLE  ! skip dead cells

         IJK = FUNIJK(I, J, K)

         IF (FLOW_AT(IJK)) THEN

! Turbulent viscosity of fluid phase.
            CALL CHECK_IS_ZERO(MU_gt(IJK), 'MU_GT')

! Granular second coefficient of viscosity.
            CALL CHECK_IS_ZERO(LAMBDA_gt(IJK), 'LAMBDA_GT')
! Gas conductivity.
            CALL CHECK_IS_ZERO(K_g(IJK), 'K_G')
! Diffusivity of gas species N.
            DO N = 1, NMAX(0)
               CALL CHECK_IS_ZERO(DIF_g(IJK, N), iVAR('DIF_G', N))
            ENDDO

            DO M = 1, SMAX
! Granular first coefficient of (shear) viscosity.
               CALL CHECK_IS_ZERO(MU_s(IJK, M), iVAR('MU_S', M))
! Granular second coefficient of viscosity.
               CALL CHECK_IS_ZERO(LAMBDA_s(IJK, M), iVAR('LAMBDA_s', M))
! Solids thermal conductivity.
               CALL CHECK_IS_ZERO(K_s(IJK, M), iVAR('K_s', M))
! Diffusivity of solids phase M, species N.
               DO N = 1, NMAX(M)
                  CALL CHECK_IS_ZERO(DIF_s(IJK, M, N), iVAR('DIF_S', M, N))
               ENDDO
            ENDDO
         ENDIF

      ENDDO
      ENDDO
      ENDDO

      CALL GLOBAL_ALL_SUM(IER)

      IF (IER /= 0) THEN
         CALL OPEN_PE_LOG(IER)

         mype_ier = ier
         IF (MYPE_IER /= 0) THEN
            WRITE (ERR_MSG, '(A, A)') 'Unphysical properties detected in flow cells. ', trim(error_message(1:1000))
         ELSE
            WRITE (ERR_MSG, "(A)") "Fatal error on another PE"
         ENDIF
         CALL LOG_ERROR()
         CALL CLOSE_PE_LOG
      ENDIF

      RETURN

   contains

      SUBROUTINE CHECK_IS_ZERO(field_value, label)
         implicit none
         DOUBLE PRECISION, INTENT(IN) :: field_value
         CHARACTER(len=*), INTENT(IN) :: label

         IF (field_value /= ZERO) THEN
            CALL REPORT_ERROR(IER, error_message, I, J, K, field_value, '/=', ZERO, label)
         END IF
      END SUBROUTINE CHECK_IS_ZERO

   END SUBROUTINE CHECK_FLOW_CELL_PROPS

!----------------------------------------------------------------------!
!                                                                      !
!  Module name: CHECK_PHYSICAL_BOUNDS                                  !
!  Author: M. Syamlal                                 Date: 27-OCT-92  !
!                                                                      !
!  Purpose: Verify that fluid cells have physical values for the       !
!  specified variables.                                                !
!                                                                      !
!----------------------------------------------------------------------!
   SUBROUTINE CHECK_PHYSICAL_BOUNDS(REPORT_SPECIES)

      IMPLICIT NONE

      LOGICAL, INTENT(OUT) :: REPORT_SPECIES(0:DIMENSION_M)

! Local variables:
!---------------------------------------------------------------------//
      INTEGER :: I, J, K, IJK
      INTEGER :: M, N
! Integer error flag.
      INTEGER :: IER, mype_ier

      CHARACTER(:), allocatable:: error_message

      error_message = ""

      IER = 0
      REPORT_SPECIES = .FALSE.

      DO K = KSTART2, KEND2
      DO J = JSTART2, JEND2
      DO I = ISTART2, IEND2
         IF (DEAD_CELL_AT(I, J, K)) CYCLE  ! skip dead cells
         IJK = FUNIJK(I, J, K)
         IF (.NOT. WALL_AT(IJK)) THEN

! Gas viscosity must be non-negative.
            CALL CHECK_NOTLESSTHAN(MU_G(IJK), ZERO, 'MU_G')

! Mixture molecular weight must be positive.
            CALL CHECK_GREATERTHAN(MW_MIX_G(IJK), ZERO, 'MW_MIX_G')

! Verify thermodynamic properties when solving energy equations:
            IF (ENERGY_EQ) THEN
! Gas conductivity must be non-negative.
               CALL CHECK_NOTLESSTHAN(K_G(IJK), ZERO, 'K_G')
! Gas phase specific heat must be positive.
               CALL CHECK_GREATERTHAN(C_PG(IJK), ZERO, 'C_PG')
! Verify that the gas phase temperature is within the bounds.
               CALL CHECK_GREATERTHAN(T_G(IJK), TMIN, 'T_G')
               CALL CHECK_LESSTHAN(T_G(IJK), TMAX, 'T_G')
! Diffusivity of gas species N must be non-negative.
               DO N = 1, NMAX(0)
                  CALL CHECK_NOTLESSTHAN(DIF_g(IJK, N), ZERO, iVAR('DIF_G', N))
                  CALL CHECK_NOTLESSTHAN(X_g(IJK, N), ZERO, iVAR('X_G', N))
                  CALL CHECK_NOTGREATERTHAN(X_g(IJK, N), ONE, iVAR('X_G', N))
               ENDDO
            ENDIF

! Verify that the gas phase mass fractons sum to one.
            IF (SPECIES_EQ(0)) THEN
               IF (ABS(ONE - sum(X_G(IJK, 1:NMAX(0)))) > TOL_COM) &
                  REPORT_SPECIES(0) = .TRUE.

! Verify that the rates of formation and consumption adhear to the
! expected coding restraints. (non-negative)
               IF (USE_RRATES) THEN
                  DO N = 1, NMAX(0)
                     CALL CHECK_NOTLESSTHAN(R_GP(IJK, N), ZERO, iVAR('R_GP', N))
                     CALL CHECK_NOTLESSTHAN(ROX_GC(IJK, N), ZERO, iVAR('RoX_GC', N))
                  ENDDO
               ENDIF

            ENDIF

            DO M = 1, SMAX

! Solids viscosity should be non-negative.
               CALL CHECK_NOTLESSTHAN(MU_S(IJK, M), ZERO, iVAR('MU_S', M))

               IF (ENERGY_EQ) THEN
! Thermal conductivity must be non-negative.
                  CALL CHECK_NOTLESSTHAN(K_S(IJK, M), ZERO, iVAR('K_S', M))
! Solids specific heat must be positive.
                  CALL CHECK_GREATERTHAN(C_PS(IJK, M), ZERO, iVAR('C_PS', M))
! Verify that the solids temperature is within the required bounds.
                  CALL CHECK_GREATERTHAN(T_S(IJK, M), TMIN, iVAR('T_S', M))
                  CALL CHECK_LESSTHAN(T_S(IJK, M), TMAX, iVAR('T_S', M))
               ENDIF

               DO N = 1, NMAX(M)
                  CALL CHECK_NOTLESSTHAN(DIF_s(IJK, M, N), ZERO, iVAR('DIF_S', M, N))
               ENDDO

! Sum of solids mass fractions should be one
               IF (SPECIES_EQ(M)) THEN
                  IF (ROP_S(IJK, M) /= ZERO) THEN
                     IF (ABS(ONE - sum(X_S(IJK, M, 1:NMAX(M)))) > TOL_COM) &
                        REPORT_SPECIES(M) = .TRUE.
                  ENDIF

                  DO N = 1, NMAX(M)
                     CALL CHECK_NOTLESSTHAN(X_s(IJK, M, N), ZERO, iVAR('X_S', M, N))
                     CALL CHECK_NOTGREATERTHAN(X_s(IJK, M, N), ONE, iVAR('X_S', M, N))
                  ENDDO
               ENDIF

! Verify that the rates of formation and consumption adhere to the
! expected coding restraints. (non-negative)
               IF (SPECIES_EQ(M) .AND. USE_RRATES) THEN
                  DO N = 1, NMAX(0)
                     CALL CHECK_NOTLESSTHAN(R_SP(IJK, M, N), ZERO, iVAR('R_SP', M, N))
                     CALL CHECK_NOTLESSTHAN(ROX_SC(IJK, M, N), ZERO, iVAR('RoX_SC', M, N))
                  ENDDO
               ENDIF
            ENDDO

         ENDIF ! IF(.NOT.WALL_AT(IJK))
      ENDDO ! DO I = ISTART2, IEND2
      ENDDO ! DO J = JSTART2, JEND2
      ENDDO ! DO K = KSTART2, KEND2

      mype_ier = ier
      CALL GLOBAL_ALL_SUM(IER)

      if (ier == 0) then

         call global_all_or(report_species)
         return

      else

         if (mype_ier /= 0) then
            call log_message(__FILE__, __LINE__, LOGLEVEL_ERROR, &
                 header=.false., footer=.true.)
            call close_pe_log
         endif

         ! This global sum is used as a "gate" to make sure all processes
         ! have written their errors before aborting.
         call global_all_sum(ier)

         write (err_msg,"('Unphysical field variables detected on one or more processes.')")
         call log_error()
      endif

   contains

      SUBROUTINE CHECK_NOTLESSTHAN(field_value, limit_value, label)
         implicit none
         DOUBLE PRECISION, INTENT(IN) :: field_value, limit_value
         CHARACTER(len=*), INTENT(IN) :: label

         IF (field_value < limit_value) CALL REPORT_ERROR &
            (IER, error_message, I, J, K, field_value, '<', limit_value, label)
      END SUBROUTINE CHECK_NOTLESSTHAN

      SUBROUTINE CHECK_NOTGREATERTHAN(field_value, limit_value, label)
         implicit none
         DOUBLE PRECISION, INTENT(IN) :: field_value, limit_value
         CHARACTER(len=*), INTENT(IN) :: label

         IF (field_value > limit_value) CALL REPORT_ERROR &
            (IER, error_message, I, J, K, field_value, '>', limit_value, label)
      END SUBROUTINE CHECK_NOTGREATERTHAN

      SUBROUTINE CHECK_LESSTHAN(field_value, limit_value, label)
         implicit none
         DOUBLE PRECISION, INTENT(IN) :: field_value, limit_value
         CHARACTER(len=*), INTENT(IN) :: label

         IF (field_value >= limit_value) CALL REPORT_ERROR &
            (IER, error_message, I, J, K, field_value, '>=', limit_value, label)
      END SUBROUTINE CHECK_LESSTHAN

      SUBROUTINE CHECK_GREATERTHAN(field_value, limit_value, label)
         implicit none
         DOUBLE PRECISION, INTENT(IN) :: field_value, limit_value
         CHARACTER(len=*), INTENT(IN) :: label

         IF (field_value <= limit_value) CALL REPORT_ERROR &
            (IER, error_message, I, J, K, field_value, '<=', limit_value, label)
      END SUBROUTINE CHECK_GREATERTHAN
   END SUBROUTINE CHECK_PHYSICAL_BOUNDS

!----------------------------------------------------------------------!
!                                                                      !
!  Module name: REPORT_SPECIES_STATS                                   !
!  Author: M. Syamlal                                 Date: 27-OCT-92  !
!                                                                      !
!  Purpose: Collect information on the sums of species mass fractions. !
!  This routine is call as-needed.                                     !
!                                                                      !
!----------------------------------------------------------------------!
   SUBROUTINE REPORT_SPECIES_STATS(REPORT_SPECIES)

      IMPLICIT NONE

      LOGICAL, INTENT(IN) :: REPORT_SPECIES(0:DIMENSION_M)

! Local variables:
!---------------------------------------------------------------------//
      INTEGER :: I, J, K, IJK, L
      INTEGER :: M

      INTEGER :: COUNT(0:DIMENSION_M, 9)
      INTEGER :: MIN_LOC(0:DIMENSION_M)
      INTEGER :: MAX_LOC(0:DIMENSION_M)

      DOUBLE PRECISION :: MIN_VAL(0:DIMENSION_M)
      DOUBLE PRECISION :: MAX_VAL(0:DIMENSION_M)

      DOUBLE PRECISION :: lSUM
!......................................................................!

      WRITE (ERR_MSG, 4000) TIME
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, FOOTER=.FALSE.)

4000  FORMAT('Time = ', G12.5, /'Warning: The sum of mass fractions ', &
             'is not equal to one.')

! Initialize the counters
      COUNT = 0
      MAX_VAL = -UNDEFINED
      MIN_VAL = UNDEFINED

! Collect stats on species across the domain.
      DO K = KSTART2, KEND2
      DO J = JSTART2, JEND2
      DO I = ISTART2, IEND2
         IF (DEAD_CELL_AT(I, J, K)) CYCLE  ! skip dead cells
         IJK = FUNIJK(I, J, K)
         IF (.NOT. WALL_AT(IJK)) THEN

            IF (SPECIES_EQ(0)) THEN
               lSUM = sum(X_G(IJK, 1:NMAX(0)))
               call update_val_loc_count(0, lSum)
            ENDIF

            DO M = 1, SMAX
!  Sum of solids mass fractions should be one
               IF (SPECIES_EQ(M)) THEN
                  lSUM = sum(X_S(IJK, M, 1:NMAX(M)))
                  IF (ROP_S(IJK, M) /= ZERO) THEN
                     call update_val_loc_count(M, lSum)
                  ENDIF
               ENDIF
            ENDDO

         ENDIF ! IF(.NOT.WALL_AT(IJK))
      ENDDO ! DO I = ISTART2, IEND2
      ENDDO ! DO J = JSTART2, JEND2
      ENDDO ! DO K = KSTART2, KEND2

      CALL GLOBAL_ALL_SUM(COUNT)
      CALL GLOBAL_ALL_MAX(MAX_VAL)
      CALL GLOBAL_ALL_MIN(MIN_VAL)

4100  FORMAT(//'Statistics of sum of gas species mass fraction', / &
              1X, 'Minimum sum of X_g=', G12.5, /1X, 'Maximum sum of X_g=', &
              G12.5, 2/, 3x, 'Sum of X_g', T20, 'No of Cells  Distribution')

      IF (REPORT_SPECIES(0)) THEN
         lSUM = SUM(COUNT(0, :))
         WRITE (ERR_MSG, 4100) MIN_VAL(0), MAX_VAL(0)
         CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
         WRITE (ERR_MSG, 4999) (COUNT(0, L), DBLE(COUNT(0, L))/lSUM, L=1, 9)
         CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
      ENDIF

4200  FORMAT(//'Statistics of sum of solids phase ', I2, ' species ', &
              'mass fractions', /1X, 'Minimum sum of X_s=', G12.5, /1X, &
              'Maximum sum of X_s=', G12.5, 2/, 3x, 'Sum of ', A, T20, 'No of ', &
              'Cells', 2x, 'Distribution')

      DO M = 1, SMAX
         IF (REPORT_SPECIES(M)) THEN
            lSUM = SUM(COUNT(M, :))
            WRITE (ERR_MSG, 4200) M, MIN_VAL(M), MAX_VAL(M), &
               trim(iVAR('X_s', M))
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
            WRITE (ERR_MSG, 4999) (COUNT(M, L), DBLE(COUNT(M, L))/lSUM, L=1, 9)
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
         ENDIF
      ENDDO

      WRITE (ERR_MSG, "(/'End of report.')")
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE.)

 4999 FORMAT(                                    &
         1X,'<0.9            ',T20,I9,T33,G12.5,/&
         1X,' 0.9    - 0.99  ',T20,I9,T33,G12.5,/&
         1X,' 0.99   - 0.999 ',T20,I9,T33,G12.5,/&
         1X,' 0.999  - 0.9999',T20,I9,T33,G12.5,/&
         1X,' 0.9999 - 1.0001',T20,I9,T33,G12.5,/&
         1X,' 1.0001 - 1.001 ',T20,I9,T33,G12.5,/&
         1X,' 1.001  - 1.01  ',T20,I9,T33,G12.5,/&
         1X,' 1.01   - 1.1   ',T20,I9,T33,G12.5,/&
         1X,'>1.1            ',T20,I9,T33,G12.5)

      RETURN

   contains

      subroutine update_val_loc_count(MM, Sum)
         INTEGER, INTENT(IN) :: MM
         DOUBLE PRECISION, INTENT(IN) :: Sum

         IF (SUM < MIN_VAL(MM)) THEN
            MIN_VAL(MM) = SUM
            MIN_LOC(MM) = IJK
         ENDIF
         IF (SUM > MAX_VAL(MM)) THEN
            MAX_VAL(MM) = SUM
            MAX_LOC(MM) = IJK
         ENDIF
         IF (SUM < 0.9) THEN
            COUNT(MM, 1) = COUNT(MM, 1) + 1 ! < 0.9
         ELSE IF (SUM < 0.99) THEN
            COUNT(MM, 2) = COUNT(MM, 2) + 1 ! 0.9    - 0.99
         ELSE IF (SUM < 0.999) THEN
            COUNT(MM, 3) = COUNT(MM, 3) + 1 ! 0.99   - 0.999
         ELSE IF (SUM < 0.9999) THEN
            COUNT(MM, 4) = COUNT(MM, 4) + 1 ! 0.999  - 0.9999
         ELSE IF (SUM < 1.0001) THEN
            COUNT(MM, 5) = COUNT(MM, 5) + 1 ! 0.9999 - 1.0001
         ELSE IF (SUM < 1.001) THEN
            COUNT(MM, 6) = COUNT(MM, 6) + 1 ! 1.0001 - 1.001
         ELSE IF (SUM < 1.01) THEN
            COUNT(MM, 7) = COUNT(MM, 7) + 1 ! 1.001  - 1.01
         ELSE IF (SUM < 1.1) THEN
            COUNT(MM, 8) = COUNT(MM, 8) + 1 ! 1.01   - 1.1
         ELSE
            COUNT(MM, 9) = COUNT(MM, 9) + 1 ! > 1.1
         ENDIF

      end subroutine update_val_loc_count

   END SUBROUTINE REPORT_SPECIES_STATS

!----------------------------------------------------------------------!
!                                                                      !
!  Module name: CHECK_RXN_MASS_BALANCE                                 !
!  Author: M. Syamlal                                 Date: 27-OCT-92  !
!                                                                      !
!  Purpose: Verify that the net interphase mass transfer rates sum to  !
!  zero. This check is not needed with updated rates implementation.   !
!                                                                      !
!----------------------------------------------------------------------!
   SUBROUTINE CHECK_RXN_MASS_BALANCE

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
! Loop counters:
      INTEGER :: I, J, K, IJK
      INTEGER :: M, L, LM
! Temp variable for summations.
      DOUBLE PRECISION :: lSUM

      INTEGER :: COUNT(DIMENSION_M + 2, 2)
      INTEGER :: MAX_LOC(DIMENSION_M + 2)
      DOUBLE PRECISION :: MAX_VAL(DIMENSION_M + 2)
!......................................................................!

      COUNT = 0
      MAX_LOC = 0
      MAX_VAL = ZERO

      DO K = KSTART2, KEND2
      DO J = JSTART2, JEND2
      DO I = ISTART2, IEND2
         IF (DEAD_CELL_AT(I, J, K)) CYCLE  ! skip dead cells
         IJK = FUNIJK(I, J, K)
         IF (.NOT. WALL_AT(IJK)) THEN

!---------------------------------------------------------------------//

! The rate of interphase mass transfer must sum to zero over all phases.
            lSUM = SUM_R_G(IJK)
            IF (SMAX > 0) lSUM = lSUM + sum(SUM_R_S(IJK, 1:SMAX))

            IF (ABS(lSUM) > SMALL_NUMBER) THEN
               IF (abs(lSUM) > abs(MAX_VAL(1))) THEN
                  MAX_VAL(1) = lSUM
                  MAX_LOC(1) = IJK
               ENDIF
! Bin the fluid cells: Good/Bad
               IF (ABS(lSUM) > TOL_COM) THEN
                  COUNT(1, 2) = COUNT(1, 2) + 1
               ELSE
                  COUNT(1, 1) = COUNT(1, 1) + 1
               ENDIF
            ENDIF

! Verify that the net rate of production (SUM_R_x) matches the the total
! amount of mass transferred from other phases.
            DO L = 0, SMAX
               IF (L == 0) THEN
                  lSUM = SUM_R_G(IJK)
               ELSE
                  lSUM = SUM_R_S(IJK, L)
               ENDIF
               DO M = 0, SMAX
                  IF (M > L) THEN
                     LM = L + 1 + (M - 1)*M/2
                     lSUM = lSUM - R_PHASE(IJK, LM)
                  ELSE IF (L > M) THEN
                     LM = M + 1 + (L - 1)*L/2
                     lSUM = lSUM + R_PHASE(IJK, LM)
                  ENDIF
               ENDDO

               IF (ABS(lSUM) > SMALL_NUMBER) THEN
                  IF (abs(lSUM) > abs(MAX_VAL(L + 2))) THEN
                     MAX_VAL(L + 2) = lSUM
                     MAX_LOC(L + 2) = IJK
                  ENDIF

! Force an exit if an imbalance exceeds the tolerance.
                  IF (ABS(lSUM) > TOL_COM) then
                     COUNT(L + 2, 2) = COUNT(L + 2, 2) + 1
                  ELSE
! Count the number of cells that do not sum to one, but are below the
! tolerance for force and exit.
                     COUNT(L + 2, 1) = COUNT(L + 2, 1) + 1
                  ENDIF
               ENDIF
            END DO

         ENDIF ! IF(.NOT.WALL_AT(IJK))
      ENDDO ! DO I = ISTART2, IEND2
      ENDDO ! DO J = JSTART2, JEND2
      ENDDO ! DO K = KSTART2, KEND2

      CALL GLOBAL_ALL_SUM(COUNT)

      IF (sum(COUNT(:, 2)) > 0) THEN

         CALL GLOBAL_ALL_MAX(MAX_VAL)

         WRITE (ERR_MSG, 5000)
         CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, FOOTER=.FALSE.)

         IF (COUNT(1, 2) > 0) THEN
            WRITE (ERR_MSG, 5100) COUNT(1, 1), COUNT(1, 2), MAX_VAL(1)
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
         ENDIF

         DO L = 0, SMAX
            IF (COUNT(L + 2, 2) > 0) THEN
               WRITE (ERR_MSG, 5200) L, COUNT(L + 2, 1), COUNT(L + 2, 2), &
                  MAX_VAL(L + 2)
               CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
            ENDIF
         ENDDO

         WRITE (ERR_MSG, "(/'End of report.')")
         CALL LOG_ERROR()
      ENDIF

      RETURN
5000  FORMAT(5X, 'Time = ', G12.5, / &
             'Message: One or more of the following errors detected:', / &
             '  1. Discrepancies in the reaction rates.', / &
             '  2. The rate of production of phases (SUM_R_g or SUM_R_s)', / &
             '     and the interphase mass transfer rates (R_Phase) are', / &
             '     inconsistent (in subroutine RRATES).', /4X, 'I', T14, 'J', &
             T24, 'K', T34, 'M', T45, 'Value')

5100  FORMAT(//'Sum of all the reaction rates is not zero!', /, &
              'Number of cells with discrepancy < error tolerance = ', I5, /, &
              'Number of cells with discrepancy > error tolerance = ', I5, /, &
              'Maximum discrepancy = ', G12.5)

5200  FORMAT(//'Mesage: Production of phase ', I2, ' not equal to ', &
              'total mass transfer', /'from other phases!', / &
              'Number of cells with discrepancy < error tolerance = ', I9, / &
              'Number of cells with discrepancy > error tolerance = ', I9, / &
              'Maximum discrepancy = ', G12.4)

   END SUBROUTINE CHECK_RXN_MASS_BALANCE

!----------------------------------------------------------------------!
!  Subroutine: REPORT_ERROR                                            !
!                                                                      !
!  Purpose: Append line to error_message.                              !
!----------------------------------------------------------------------!
   SUBROUTINE REPORT_ERROR(pIER, error_message, pI, pJ, pK, VAL, RELATION, BND, VAR_FULL)

      INTEGER, INTENT(INOUT) :: pIER
      CHARACTER(:), ALLOCATABLE, INTENT(INOUT) :: error_message
      INTEGER, INTENT(IN) :: pI, pJ, pK
      DOUBLE PRECISION, INTENT(IN) :: BND, VAL
      CHARACTER(LEN=*), INTENT(IN) :: RELATION
      CHARACTER(LEN=*), INTENT(IN) :: VAR_FULL
      CHARACTER(LEN=1000) :: err_line

      integer :: lier
      logical, save :: first_error = .true.

      pIER = 1

      if (first_error) then
         call open_pe_log(lier)
         write (err_msg,3000)
         call log_message(__FILE__, __LINE__, LOGLEVEL_ERROR, &
              header=.true., footer=.false.)
         first_error = .false.
      endif

      write (err_msg, 9000) pi, pj, pk, trim(var_full), val, relation, bnd
      call log_message(__FILE__, __LINE__, LOGLEVEL_ERROR, &
           header=.false., footer=.false.)


3000  FORMAT('Fatal Error: Unphysical field variables detected.',&
           3/5X,'I',6x,'J',6x,'K')

9000  format(3(i6, 1x), 5x, a, ' = ', g12.4, 1x, a, g12.4)

      RETURN
   END SUBROUTINE REPORT_ERROR

END MODULE CHECK_DATA_30_MOD
