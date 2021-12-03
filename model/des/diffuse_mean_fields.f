MODULE DIFFUSE_MEAN_FIELD_MOD

   use adjust_leq_mod, only: adjust_leq
   use compar, only: ijkstart3, ijkend3
   use dif_phi_bc_des_mod, only: dif_phi_bc_des
   use dif_phi_des_mod, only: dif_phi_des
   use dif_phi_source_des_mod, only: dif_phi_source_des
   use error_manager, only: loglevel_info, log_message, err_msg
   use functions, only: fluid_at
   use init_ab_m_mod, only: init_ab_m
   use mpi_init_des, only: ival
   use param, only: dimension_3
   use param1, only: zero
   use particle_filter, only: des_diffuse_width
   use solve_lin_eq_mod, only: solve_lin_eq
   use time_cpu, only: get_cpu_time

! Global Variables:
!---------------------------------------------------------------------//
! Max bound for array sizes.
   use geometry, only: IJKMAX2
! Coefficient matrix and force vector.
   use ambm, only: A_M, B_M
! Method to solve linear system and max iterations
   use leqsol, only: LEQ_METHOD, LEQ_IT
! Preconditioner, sweep method, convergence tolerance
   use leqsol, only: LEQ_PC, LEQ_SWEEP, LEQ_TOL

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: DIFFUSE_MEAN_FIELDS                                     !
!  Author: J.Musser                                   Date: 11-NOV-14  !
!                                                                      !
!  Purpose: Given the field variable PHI (e.g., volume fraction),      !
!  diffuse it across the Eulerian grid such that the Full Width at     !
!  Half Maximum (FWHM) equals the user specified diffusion width.      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE DIFFUSE_MEAN_FIELD(PHI, VNAME)

      IMPLICIT NONE

! Dummy Arguments:
!---------------------------------------------------------------------//
! Variable to diffuse
      DOUBLE PRECISION, INTENT(INOUT) :: PHI(DIMENSION_3)
! Name of variable to diffuse
      CHARACTER(LEN=*), INTENT(IN) :: VNAME

! Local Variables:
!---------------------------------------------------------------------//
! Integer error flag
      INTEGER :: IER
! Linear equation solver method and iterations
      INTEGER :: LEQM, LEQI
! Start, stop and step size of diffusion time march
      DOUBLE PRECISION :: DIF_TIME, DIF_STOP, DIF_DT
! wall time at start
      DOUBLE PRECISION :: WALL_START
! Local flag to print debug messages
      LOGICAL, PARAMETER :: setDBG = .TRUE.
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: DIF
!......................................................................!

      IF(setDBG) THEN
         WRITE(ERR_MSG, "(/3x,'Diffusing Variable: ',A)") VNAME
         CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
         WALL_START = get_cpu_time()
      ENDIF

      ALLOCATE(DIF(DIMENSION_3))

! Populate the diffusion coefficients
      CALL CALC_DIF_DES(DIF, setDBG, IER)

      DIF_STOP = 1.0d0
      DIF_TIME = 0.0d0
      DIF_DT = DIF_STOP/5.0

! Integrate the diffusion equation (time, space)
      DO WHILE(DIF_TIME < DIF_STOP)
! Initialize the coefficient matrix and force vector
         CALL INIT_AB_M (A_M, B_M, IJKMAX2, 0)
! Calculate the coefficients
         CALL DIF_PHI_DES(0, DIF, A_M, B_M)
! Apply zero-flux BC at all walls
         CALL DIF_PHI_BC_DES(PHI, 0, A_M, B_M)
! Collect the center coefficient and force vector
         CALL DIF_PHI_SOURCE_DES(PHI, 0, A_M, B_M, DIF_DT)
! Set the local method and iterations.
         CALL ADJUST_LEQ(0.0d0, LEQ_IT(10), LEQ_METHOD(10), LEQI, LEQM)
! Solve the linear system.
         CALL SOLVE_LIN_EQ (VNAME, 10, PHI, A_M, B_M, 0, LEQI, LEQM, &
            LEQ_SWEEP(10), LEQ_TOL(10), LEQ_PC(10), IER)
! Advance time.
         DIF_TIME = DIF_TIME + DIF_DT
      ENDDO

! Debugging information
      IF(setDBG) THEN
         WRITE(ERR_MSG, 9001) GET_CPU_TIME() - WALL_START
         CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
      ENDIF

      DEALLOCATE(DIF)

9001  FORMAT(5x,'Wall Time: ',g11.4)

      RETURN
   END SUBROUTINE DIFFUSE_MEAN_FIELD

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CALC_DIF_DES                                            !
!  Author: J.Musser                                   Date: 11-NOV-14  !
!                                                                      !
!  Purpose: Calculate the diffusion coefficient for diffusing mean     !
!  fields. Presently the diffusion coefficient is constant.            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE CALC_DIF_DES(DIF, lDBG, IER)

      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(INOUT) :: DIF(DIMENSION_3)
      LOGICAL, INTENT(IN) :: lDBG
      INTEGER, INTENT(INOUT) :: IER

! Fluid Cell indices
      INTEGER :: IJK

      DOUBLE PRECISION :: lDIF

      IER = 0

! The diffusion coefficient is set so that over one second, the
! quantity diffuses such that the Full Width at Half Maximum (FWHM)
! equals what the user specified as the "filter wideth."
      lDIF = ((0.5*DES_DIFFUSE_WIDTH)**2) / &
         (2.0*sqrt(2.0*log(2.0)))

! Store the diffusion coefficient in all fluid cells.
      DO IJK = IJKStart3, IJKEnd3
         DIF(IJK) = ZERO
         IF(FLUID_AT(IJK)) DIF(IJK) = lDIF
      ENDDO

! Information included for debugging.
      IF(lDBG) THEN
         WRITE(ERR_MSG, 9100) iVal(lDIF)
         CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
      ENDIF

9100  FORMAT(/3x,'Diffusion Coefficient: ',A)

      RETURN

   END SUBROUTINE CALC_DIF_DES

END MODULE DIFFUSE_MEAN_FIELD_MOD
