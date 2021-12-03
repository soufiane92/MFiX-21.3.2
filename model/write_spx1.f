#include "error.inc"
#include "version.inc"

MODULE write_spx1_mod

   USE calc_interp_weights_mod, only: calc_interp_weights
   USE cdist, only: bdist_io
   USE comp_mean_fields_mod, only: comp_mean_fields
   USE compar, only: mype, pe_io, root
   USE cutcell, only: dimension_3, re_indexing
   USE discretelement, only: des_continuum_coupled
   USE discretelement, only: discrete_element
   USE discretelement, only: print_des_data
   USE error_manager, only: err_msg, loglevel_error, log_message
   USE fldvar
   USE funits, only: unit_spx
   USE geometry, only: dx, dy, dz, flag, coordinates
   USE geometry, only: imax2, jmax2, kmax2, ijkmax2, ijkmax3
   USE in_binary_512, only: convert_to_io_dp
   USE in_binary_512i, only: convert_to_io_i
   USE mpi_utility, only: gather
   USE open_files_mod, only: open_spx, close_spx
   USE out_bin_r_mod, only: out_bin_r
   USE param1, only: undefined, zero
   USE particles_in_cell_mod, only: particles_in_cell
   USE physprop, only: mmax, nmax
   USE run, only: run_name, run_type, dem_solids, nstep, time
   USE rxns, only: reactionrates, nrr
   USE scalars, only: nscalar
   USE set_increments_mod, only: unshift_dp_array
   USE turb, only: k_epsilon
   USE write_des_data_mod, only: write_des_data
   USE write_error_mod, only: write_error

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: WRITE_SPX1                                              C
!  Purpose: write out the time-dependent restart records (REAL)        C
!                                                                      C
!  Author: P. Nicoletti                               Date: 13-DEC-91  C
!  Reviewer: P. Nicoletti, W. Rogers, M. Syamlal      Date: 24-JAN-92  C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE WRITE_SPX1(L, unit_add)

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! flag whether to write a particular SPx file
      INTEGER, intent(in) :: L
! offset for use in post_mfix
      INTEGER, intent(in) :: unit_add

! Local variables
!---------------------------------------------------------------------//
! loop counters
      INTEGER LC, NN
! Pointer to the next record
      INTEGER NEXT_REC
!  Number of records written each time step
      INTEGER NUM_REC

!---------------------------------------------------------------------//

      CALL OPEN_SPX('OLD')

      SELECT CASE (L)

! ".SP1" FILE         EP_g    [ ROP_g, RO_g  must be calculated ...
!                                        not written out ]
      CASE (1)
         CALL write_time_and_nstep

! Explicitly coupled simulations do not need to rebin particles to
! the fluid grid every time step. However, this implies that the
! fluid cell information and interpolation weights become stale.
         IF (DEM_SOLIDS .AND. .NOT. DES_CONTINUUM_COUPLED) THEN
! Bin particles to fluid grid.
            CALL PARTICLES_IN_CELL
! Calculate interpolation weights
            CALL CALC_INTERP_WEIGHTS
! Calculate mean fields (EPg).
            CALL COMP_MEAN_FIELDS
         ENDIF

         CALL write_spx_array(EP_g)
         CALL write_rec_nums

! The call made in make_arrays captures the initial state of the system
! as the input and RES files for DES runs are read afte the the first
! call to this routine.
         IF (DISCRETE_ELEMENT .AND. PRINT_DES_DATA) THEN
            IF (TIME /= ZERO .OR. TRIM(RUN_TYPE) == 'RESTART_1') &
               CALL WRITE_DES_DATA
         ENDIF

! ".SP2" FILE         P_g , P_star
      CASE (2)
         CALL write_time_and_nstep
         CALL write_spx_array(P_g)
         CALL write_spx_array(P_star)
         CALL write_rec_nums

! ".SP3" FILE         U_g , V_g , W_g
      CASE (3)
         CALL write_time_and_nstep
         CALL write_spx_array(U_g)
         CALL write_spx_array(V_g)
         CALL write_spx_array(W_g)
         CALL write_rec_nums

! ".SP4" FILE         U_s , V_s , W_s
      CASE (4)
         CALL write_time_and_nstep
         DO LC = 1, MMAX
            CALL write_spx_array(U_s(:, LC))
            CALL write_spx_array(V_s(:, LC))
            CALL write_spx_array(W_s(:, LC))
         ENDDO
         CALL write_rec_nums

! ".SP5" FILE         ROP_s
      CASE (5)
         CALL write_time_and_nstep
         DO LC = 1, MMAX
            CALL write_spx_array(ROP_s(:, LC))
         ENDDO
         CALL write_rec_nums

! ".SP6" FILE         T_g  , T_s
      CASE (6)
         CALL write_time_and_nstep
         CALL write_spx_array(T_g)
         DO LC = 1, MMAX
            CALL write_spx_array(T_s(:, LC))
         END DO
         CALL write_rec_nums

! ".SP7" FILE         X_g, X_s
      CASE (7)
         CALL write_time_and_nstep
         DO NN = 1, NMAX(0)
            CALL write_spx_array(X_g(:, nn))
         END DO
         DO LC = 1, MMAX
            DO NN = 1, NMAX(LC)
               CALL write_spx_array(X_s(:, LC, nn))
            ENDDO
         END DO
         CALL write_rec_nums

! ".SP8" FILE         THETA_m
      CASE (8)
         CALL write_time_and_nstep
         DO LC = 1, MMAX
            CALL write_spx_array(THETA_m(:, LC))
         ENDDO
         CALL write_rec_nums

! ".SP9" FILE         Scalar
      CASE (9)
         CALL write_time_and_nstep
         DO LC = 1, Nscalar
            CALL write_spx_array(Scalar(:, LC))
         ENDDO
         CALL write_rec_nums

! Reaction rates
      CASE (10)
         CALL write_time_and_nstep
         DO LC = 1, nRR
            CALL write_spx_array(ReactionRates(:, LC))
         ENDDO
         CALL write_rec_nums

! ".SP11" FILE         turbulence
      CASE (11)
         CALL write_time_and_nstep
         if (K_Epsilon) then
            CALL write_spx_array(K_Turb_G)
            CALL write_spx_array(E_Turb_G)
         end if
         CALL write_rec_nums

      CASE DEFAULT
         WRITE (ERR_MSG, '(A)') 'Unknown SPx file index'
         call log_error()
      END SELECT

      CALL CLOSE_SPX

      RETURN

   CONTAINS

      SUBROUTINE write_time_and_nstep
         INTEGER uspx   ! UNIT_SPX + offset from post_mfix
         uspx = UNIT_SPX + unit_add + L
         if (myPE .eq. PE_IO .or. bDist_IO) then
            READ (uspx, REC=3) NEXT_REC, NUM_REC
            NUM_REC = NEXT_REC
            WRITE (uspx, REC=NEXT_REC) REAL(TIME), NSTEP
            NEXT_REC = NEXT_REC + 1
         end if
      END SUBROUTINE write_time_and_nstep

      SUBROUTINE write_spx_array(some_array)
         double precision, dimension(:), intent(in)  :: some_array
         double precision, dimension(:), allocatable :: TMP_VAR
         INTEGER uspx   ! UNIT_SPX + offset from post_mfix

         uspx = UNIT_SPX + unit_add + L

         if (bDist_IO) then
            IF (RE_INDEXING) THEN
               allocate (TMP_VAR(DIMENSION_3))
               CALL UNSHIFT_DP_ARRAY(some_array, TMP_VAR)
               call OUT_BIN_R(uspx, TMP_VAR, size(TMP_VAR), NEXT_REC)
               deallocate (TMP_VAR)
            ELSE
               call OUT_BIN_R(uspx, some_array, size(some_array), NEXT_REC)
            ENDIF
         else
            call gatherWriteSpx(some_array, uspx, NEXT_REC)
         end if
      END SUBROUTINE write_spx_array

      SUBROUTINE write_rec_nums
         INTEGER uspx   ! UNIT_SPX + offset from post_mfix
         uspx = UNIT_SPX + unit_add + L
         if (myPE .eq. PE_IO .or. bDist_IO) then
            NUM_REC = NEXT_REC - NUM_REC
            WRITE (uspx, REC=3) NEXT_REC, NUM_REC
            if (unit_add == 0) FLUSH(uspx)
         end if
      END SUBROUTINE write_rec_nums

   END SUBROUTINE WRITE_SPX1

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine:                                                         C
!  Purpose:                                                            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   subroutine gatherWriteSpx(VAR, uspxL, NEXT_REC)

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      integer uspxL, NEXT_REC
      double precision, dimension(DIMENSION_3) :: VAR

! Local variables
!---------------------------------------------------------------------//
      double precision, dimension(:), allocatable :: TMP_VAR, array1(:), array2(:)
!---------------------------------------------------------------------//

      allocate (TMP_VAR(DIMENSION_3))

      if (myPE .eq. PE_IO) then
         allocate (array1(ijkmax2))
         allocate (array2(ijkmax3))
      else
         allocate (array1(1))
         allocate (array2(1))
      end if

      IF (RE_INDEXING) THEN
         TMP_VAR = UNDEFINED
         CALL UNSHIFT_DP_ARRAY(VAR, TMP_VAR)
         CALL gather(TMP_VAR, array2, root)
      ELSE
         CALL gather(VAR, array2, root)
      ENDIF

      if (myPE .eq. PE_IO) then
         call convert_to_io_dp(array2, array1, ijkmax2)
         CALL OUT_BIN_R(uspxL, array1, IJKMAX2, NEXT_REC)
      end if

      deallocate (TMP_VAR)
      deallocate (array1)
      deallocate (array2)

   End subroutine gatherWriteSpx


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine:                                                         C
!  Purpose:                                                            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      subroutine copy_d_to_r(darr,rarr,nx,ny,nz)
      implicit none

! Dummy arguments
!---------------------------------------------------------------------//
      integer :: nx, ny, nz
      double precision :: darr(*)
      real :: rarr(nx,ny,*)

! Local variables
!---------------------------------------------------------------------//
      integer :: i, j, k, ijk
!---------------------------------------------------------------------//
      ijk = 0
      do i = 1,nx
           do j = 1,ny
              do k = 1,nz
                 ijk = ijk + 1
                 rarr(i,j,k) = real(darr(ijk))
              end do
           end do
      end do

      return
      end subroutine copy_d_to_r


end MODULE write_spx1_mod
