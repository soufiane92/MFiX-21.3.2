module write_res1_mod

   USE cdist, only: bdist_io
   USE cutcell, only: re_indexing
   USE energy
   USE fldvar
   USE funits, only: unit_res
   USE in_binary_512, only: convert_to_io_dp, in_bin_512
   USE mpi_utility, only: gather
   USE open_files_mod, only: open_file_and_check_error
   USE out_bin_512_mod, only: out_bin_512
   USE param, only: dimension_3
   USE physprop
   USE run, only: run_name, time, dt, nstep, solve_ros
   USE rxns, only: reactionrates, nrr
   USE scalars, only: nscalar
   USE sendrecv
   USE set_increments_mod, only: unshift_dp_array
   USE turb, only: k_epsilon
   USE usr_prop, only: usr_ros

contains

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: WRITE_RES1                                              C
!  Purpose: write out the time-dependent restart records               C
!                                                                      C
!  Author: P. Nicoletti                               Date: 13-DEC-91  C
!  Reviewer: P. Nicoletti, W. Rogers, M. Syamlal      Date: 24-JAN-92  C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE WRITE_RES1

      IMPLICIT NONE

! Local Variables
!---------------------------------------------------------------------//
! allocate arrays for gather/convert_to_io_dp
      double precision, allocatable :: array1(:)
      double precision, allocatable :: array2(:)
! loop counter
      INTEGER :: LC, NN
! pointer to first time-dependent record in restart file
      INTEGER :: NEXT_REC
!---------------------------------------------------------------------//

!      if (myPE.eq.PE_IO .and. .not.distio) then
         allocate (array1(ijkmax2))
         allocate (array2(ijkmax3))
!      else
!         allocate (array1(1))
!         allocate (array2(1))
!      end if

      CALL OPEN_FILE_AND_CHECK_ERROR("OLD", UNIT_RES, '.RES')

      if (myPE.eq.PE_IO .or. bDist_IO) then
         READ (UNIT_RES, REC=3) NEXT_REC
         WRITE (UNIT_RES, REC=NEXT_REC) TIME, DT, NSTEP
         NEXT_REC = NEXT_REC + 1
      end if

       call send_recv(EP_g,2)
       call send_recv(P_g,2)
       call send_recv(P_star,2)
       call send_recv(RO_g,2)
       call send_recv(ROP_g,2)
       call send_recv(X_g,2)
       call send_recv(T_g,2)
       call send_recv(U_g,2)
       call send_recv(V_g,2)
       call send_recv(W_g,2)
       call send_recv(ROP_S,2)
       IF(ANY(SOLVE_ROs) .OR. ANY(USR_ROS)) call send_recv(RO_S,2)
       call send_recv(T_S,2)
       call send_recv(U_S,2)
       call send_recv(V_S,2)
       call send_recv(W_S,2)
       call send_recv(THETA_M,2)
       call send_recv(X_S,2)
       if(NScalar > 0)call send_recv(Scalar,2)
       if(K_Epsilon) THEN
           call send_recv(K_Turb_G,2)
           call send_recv(E_Turb_G,2)
       endif
       call send_recv(GAMA_RG,2)
       call send_recv(T_RG,2)
       call send_recv(GAMA_RS,2)
       call send_recv(T_RS,2)
       if(nRR > 0)call send_recv(ReactionRates,2)

      call gatherWriteRes (EP_g,array2, array1, NEXT_REC)
      call gatherWriteRes (P_g,array2, array1, NEXT_REC)
      call gatherWriteRes (P_star,array2, array1, NEXT_REC)
      call gatherWriteRes (RO_g,array2, array1, NEXT_REC)
      call gatherWriteRes (ROP_g,array2, array1, NEXT_REC)
      call gatherWriteRes (T_g,array2, array1, NEXT_REC)

      DO NN = 1, NMAX(0)
            call gatherWriteRes (X_g(:,nn),array2, array1, NEXT_REC)
      END DO

      call gatherWriteRes (U_g,array2, array1, NEXT_REC)
      call gatherWriteRes (V_g,array2, array1, NEXT_REC)
      call gatherWriteRes (W_g,array2, array1, NEXT_REC)

      DO LC = 1, MMAX

        call gatherWriteRes (ROP_s(:,LC),array2, array1, NEXT_REC)

        IF(ANY(SOLVE_ROs) .OR. ANY(USR_ROS)) &
            call gatherWriteRes (RO_S(:,LC),array2, array1, NEXT_REC)

        call gatherWriteRes (T_s(:,LC),array2, array1, NEXT_REC)
        call gatherWriteRes (U_s(:,LC),array2, array1, NEXT_REC)
        call gatherWriteRes (V_s(:,LC),array2, array1, NEXT_REC)
        call gatherWriteRes (W_s(:,LC),array2, array1, NEXT_REC)
        call gatherWriteRes (THETA_M(:,LC),array2, array1, NEXT_REC)
         DO NN = 1, NMAX(LC)
            call gatherWriteRes (X_s(:,LC,NN),array2, array1, NEXT_REC)
         END DO
      END DO

! Version 1.3
      DO LC = 1, NScalar
            call gatherWriteRes (Scalar(:,LC),array2, array1, NEXT_REC)
      END DO

! Version 1.4 -- write radiation variables in write_res1
      call gatherWriteRes (GAMA_RG,array2, array1, NEXT_REC)
      call gatherWriteRes (T_RG,array2, array1, NEXT_REC)
      DO LC = 1, MMAX
        call gatherWriteRes (GAMA_RS(1,LC),array2, array1, NEXT_REC)
        call gatherWriteRes (T_RS(1,LC),array2, array1, NEXT_REC)
      ENDDO

! Version 1.5
      DO LC = 1, nRR
            call gatherWriteRes (ReactionRates(:,LC),array2, array1, NEXT_REC)
      END DO

! Version 1.6
      if (K_epsilon) then
          call gatherWriteRes (K_turb_G,array2, array1, NEXT_REC)
          call gatherWriteRes (E_turb_G,array2, array1, NEXT_REC)
      endif
!---------------------------------------------------------------------

      if ( (myPE.eq.PE_IO .and. .not.bDist_IO) .or. bDist_IO) then
           FLUSH(UNIT_RES)
        end if

      CLOSE(UNIT_RES)

!      call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here

      deallocate (array1)
      deallocate (array2)
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here

      RETURN
      END SUBROUTINE WRITE_RES1


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine:                                                         C
!  Purpose:                                                            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      subroutine gatherWriteRes(VAR, array2, array1, NEXT_REC)

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      double precision, dimension(ijkmax2) :: array1
      double precision, dimension(ijkmax3) :: array2
      double precision, dimension(DIMENSION_3) :: VAR
      INTEGER :: NEXT_REC

! Local variables
!---------------------------------------------------------------------//
      double precision, dimension(DIMENSION_3) :: TMP_VAR

!---------------------------------------------------------------------//

!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      if (.not.bDist_IO) then

         IF(RE_INDEXING) THEN
            CALL UNSHIFT_DP_ARRAY(VAR,TMP_VAR)
            CALL gather (TMP_VAR,array2,root)
         ELSE
            CALL gather (VAR,array2,root)
         ENDIF
!         call gather (VAR,array2,root)  !//d pnicol

!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         if (myPE.eq.PE_IO) then
            call convert_to_io_dp(array2,array1,ijkmax2)
            CALL OUT_BIN_512 (UNIT_RES, array1, IJKMAX2, NEXT_REC)
         end if

      else

         IF(RE_INDEXING) THEN
            CALL UNSHIFT_DP_ARRAY(VAR,TMP_VAR)
            CALL OUT_BIN_512 (UNIT_RES, TMP_VAR, size(TMP_VAR), NEXT_REC)
         ELSE
            CALL OUT_BIN_512 (UNIT_RES, var, size(var), NEXT_REC)
         ENDIF
!         CALL OUT_BIN_512 (UNIT_RES, var, size(var), NEXT_REC)

      end if

      End subroutine gatherWriteRes


end module write_res1_mod
