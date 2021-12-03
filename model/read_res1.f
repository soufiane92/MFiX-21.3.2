MODULE READ_RES1_MOD

   USE cdist, only: bdist_io, bstart_with_one_res
   USE compar
   USE energy, only: gama_rg, gama_rs, t_rg, t_rs
   USE fldvar
   USE functions
   USE funits, only: unit_res
   USE in_binary_512, only: convert_to_io_dp, convert_from_io_dp, in_bin_512
   USE indices, only: i_of, j_of, k_of
   USE mpi_utility, only: bcast, scatter
   USE open_files_mod, only: open_file_and_check_error
   USE param, only: dimension_3
   USE param1, only: one, zero
   USE param1, only: zero, undefined
   USE physprop, only: mmax, nmax
   USE run, only: dt, dt_fac, nstep, nsteprst, solve_ros, time, run_name
   USE rxns, only: reactionrates, nrr
   USE scalars, only: nscalar
   USE sendrecv, only: ijkmax2, ijkmax3, send_recv
   USE turb, only: k_epsilon
   USE usr_prop, only: usr_ros

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: READ_RES1                                               C
!  Purpose: read in the time-dependent restart records                 C
!                                                                      C
!  Author: P. Nicoletti                               Date: 03-JAN-92  C
!  Reviewer: P. Nicoletti, W. Rogers, M. Syamlal      Date: 24-JAN-92  C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE READ_RES1

      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------//
! loop counter
      INTEGER :: LC
! Local species index
      INTEGER :: NN
! pointer to the next record
      INTEGER :: NEXT_REC
! version id
      CHARACTER(LEN=512) :: VERSION
! version number
      REAL :: VERSION_NUMBER
! Dummy array
      DOUBLE PRECISION :: DT_SAVE
!//PAR_I/O declare global scratch arrays
      double precision, allocatable :: array1(:)
      double precision, allocatable :: array2(:)
!---------------------------------------------------------------------//

      CALL OPEN_FILE_AND_CHECK_ERROR("OLD", UNIT_RES, '.RES')

      if (myPE .eq. PE_IO .or. .not.bStart_with_one_res) then
         allocate (array1(ijkmax2))
         allocate (array2(ijkmax3))
      else
         allocate (array1(1))
         allocate (array2(1))
      end if

!      call MPI_barrier(MPI_COMM_WORLD,mpierr)

! Use DT from data file if DT_FAC is set to 1.0
      IF (DT_FAC == ONE) DT_SAVE = DT


!//PAR_I/O only PE_IO reads the restart file
      if (myPE == PE_IO .or. (bDist_IO .and. .not.bStart_with_one_RES)) then
         READ (UNIT_RES, REC=1) VERSION
         READ (VERSION(6:512), *) VERSION_NUMBER

         READ (UNIT_RES, REC=3) NEXT_REC
         IF (VERSION_NUMBER >= 1.12) THEN
            READ (UNIT_RES, REC=NEXT_REC) TIME, DT, NSTEP
         ELSE
            READ (UNIT_RES, REC=NEXT_REC) TIME, NSTEP
         ENDIF
         NEXT_REC = NEXT_REC + 1
      end if


      if (.not.bDist_IO  .or. bStart_with_one_RES) then
!        call MPI_barrier(MPI_COMM_WORLD,mpierr)

         call bcast(VERSION, PE_IO)        !//PAR_I/O BCAST0c
         call bcast(VERSION_NUMBER, PE_IO) !//PAR_I/O BCAST0r
         call bcast(TIME, PE_IO)           !//PAR_I/O BCAST0d
         call bcast(NSTEP, PE_IO)          !//PAR_I/O BCAST0i
         if (VERSION_NUMBER >= 1.12) call bcast(DT, PE_IO)   !//PAR_I/O BCAST0d
      end if
!      call MPI_barrier(MPI_COMM_WORLD,mpierr)

! Store the timestep counter level at the begin of RESTART run
      NSTEPRST = NSTEP

      call readScatterRes(EP_G,array2, array1, 0, NEXT_REC)
      call readScatterRes(P_G,array2, array1, 0, NEXT_REC)
      call readScatterRes(P_STAR,array2, array1, 1, NEXT_REC)
      call readScatterRes(RO_G,array2, array1, 0, NEXT_REC)
      call readScatterRes(ROP_G,array2, array1, 0, NEXT_REC)
      call readScatterRes(T_G,array2, array1, 1, NEXT_REC)

      IF (VERSION_NUMBER < 1.15) THEN
         call readScatterRes (T_s(:,1),array2, array1, 1, NEXT_REC)
         IF (MMAX >= 2) THEN
            call readScatterRes (T_s(:,2),array2, array1, 1, NEXT_REC)
         ELSE
            if (myPE == PE_IO) &
               CALL IN_BIN_512 (UNIT_RES, array1, IJKMAX2, NEXT_REC)
         ENDIF
      ENDIF

      IF (VERSION_NUMBER >= 1.05) THEN
         DO NN = 1, NMAX(0)
            call readScatterRes (X_g(:,nn),array2, array1, 1, NEXT_REC)
         END DO
      ENDIF

      call readScatterRes(U_G, array2, array1, 0, NEXT_REC)
      call readScatterRes(V_G, array2, array1, 0, NEXT_REC)
      call readScatterRes(W_G, array2, array1, 0, NEXT_REC)

      DO LC = 1, MMAX
         call readScatterRes(ROP_S(:,LC), array2, array1, 0, NEXT_REC)

         IF(ANY(SOLVE_ROs).OR.ANY(USR_ROS)) &
            CALL readScatterRes(RO_S(:,LC), array2, array1, 0, NEXT_REC)

         IF (VERSION_NUMBER >= 1.15) THEN
            call readScatterRes(T_S(:,LC), array2, array1, 1, NEXT_REC)
         END IF
         call readScatterRes(U_S(:,LC), array2, array1, 0, NEXT_REC)
         call readScatterRes(V_S(:,LC), array2, array1, 0, NEXT_REC)
         call readScatterRes(W_S(:,LC), array2, array1, 0, NEXT_REC)

         IF (VERSION_NUMBER >= 1.2) then
            call readScatterRes(THETA_M(:,LC), array2, array1, 1, NEXT_REC)
         end if
         IF (VERSION_NUMBER >= 1.05) THEN
            DO NN = 1, NMAX(LC)
               call readScatterRes(X_S(:,LC,NN), array2, array1, 1, NEXT_REC)
            END DO
         ENDIF
      END DO

      IF (VERSION_NUMBER >= 1.3) THEN
         DO NN = 1, NScalar
            call readScatterRes(Scalar(:,NN), array2, array1, 1, NEXT_REC)
         END DO
      ENDIF

      IF (VERSION_NUMBER >= 1.4) THEN
         call readScatterRes(GAMA_RG, array2, array1, 1, NEXT_REC)

         call readScatterRes(T_RG, array2, array1, 0, NEXT_REC)

         DO LC = 1, MMAX
            call readScatterRes(GAMA_RS(1,LC), array2, array1, 1, NEXT_REC)

            call readScatterRes(T_RS(1,LC), array2, array1, 0, NEXT_REC)

         ENDDO
      ELSE
         GAMA_RG(:)   = ZERO
         T_RG (:)     = ZERO
         GAMA_RS(:,:) = ZERO
         T_RS(:,:)    = ZERO
      ENDIF

      IF (VERSION_NUMBER >= 1.5) THEN
         DO NN = 1, nRR
            call readScatterRes(ReactionRates(:,NN), array2, array1, 1, NEXT_REC)
         END DO
      ENDIF

      IF (VERSION_NUMBER >= 1.6 .AND. K_Epsilon) THEN
         call readScatterRes(K_Turb_G, array2, array1, 1, NEXT_REC)
         call readScatterRes(E_Turb_G, array2, array1, 1, NEXT_REC)
      ENDIF
!------------------------------------------------------------------------

!      call MPI_barrier(MPI_COMM_WORLD,mpierr)
      deallocate( array1 )
      deallocate( array2 )
!      call MPI_barrier(MPI_COMM_WORLD,mpierr)

      if (.not.bDist_IO .or. bStart_with_one_RES) then
         call send_recv(rop_g)
         call send_recv(ro_g)
         call send_recv(rop_s)
         call send_recv(ro_s)
      end if

      CLOSE(UNIT_RES)

      IF (DT_FAC == ONE) DT = DT_SAVE

! We may no longer need PATCH_AFTER_RESTART
!     CALL PATCH_AFTER_RESTART

      RETURN
   END SUBROUTINE READ_RES1


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine:                                                         C
!  Purpose:                                                            C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   subroutine readScatterRes(VAR, array2, array1, init, NEXT_REC)

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      double precision, dimension(DIMENSION_3) :: VAR
      double precision, dimension(ijkmax2) :: array1
      double precision, dimension(ijkmax3) :: array2
      INTEGER :: init  ! define VAR initialization, 0: undefin, 1: zero
      INTEGER :: NEXT_REC
!---------------------------------------------------------------------//

!// Reset global scratch arrays
      if( init==0 ) then
         array1(:) = Undefined
         array2(:) = Undefined
      else
         array1(:) = zero
         array2(:) = zero
      endif

      if (.not.bDist_IO .or. bStart_with_one_RES) then
         if (myPE == PE_IO) then
            CALL IN_BIN_512 (UNIT_RES, array1, IJKMAX2, NEXT_REC)
            CALL convert_from_io_dp(array1, array2, IJKMAX2)
         end if
!        call MPI_barrier(MPI_COMM_WORLD,mpierr)
         call scatter(VAR, array2, PE_IO)
!        call MPI_barrier(MPI_COMM_WORLD,mpierr)
      else
         CALL IN_BIN_512 (UNIT_RES, var, size(var) , NEXT_REC)
      end if

   End subroutine readScatterRes


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: PATCH_AFTER_RESTART                                    C
!  Purpose: Patch new fluid cells after a restart                      C
!           This could occur when restarting with a different          C
!           grid partition when the RESTART file was generated         C
!           prior to the Dec. 4th 2014 bug fix.                        C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 14-APR-15  C
!                                                                      C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE PATCH_AFTER_RESTART

      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------//
! indices
      INTEGER :: I,J,K, IJK, IJKNB
      INTEGER :: M,NN
      INTEGER :: NB
      INTEGER, DIMENSION(6) :: NBCELL
      LOGICAL :: NB_FOUND

!---------------------------------------------------------------------//

      DO IJK = ijkstart3, ijkend3

         IF (FLUID_AT(IJK).AND.EP_G(IJK)==UNDEFINED) THEN

! Detects new fluid cells that used to be blocked cells with undefined
! values. When a fluid cell has undefined void fraction, this means all
! variables need to be patched. Typically, this will be a fairly small
! cut cell that was flagged as BLOCKED cell with a different partition.
! If a valid fluid cell is found next to this undefined cell, all field
! variables will be copied over. If no valid fluid cell is found, the
! code will continue and will likely stop during the check_data_30
! (zero species mass fractions will yield a zero specific heat).
            I = I_OF(IJK)
            J = J_OF(IJK)
            K = K_OF(IJK)

            NBCELL(1) = IM_OF(IJK)
            NBCELL(2) = JM_OF(IJK)
            NBCELL(3) = KM_OF(IJK)
            NBCELL(4) = IP_OF(IJK)
            NBCELL(5) = JP_OF(IJK)
            NBCELL(6) = KP_OF(IJK)

            NB_FOUND = .FALSE.

            DO NB = 1,6

               IJKNB = NBCELL(NB)

               IF(FLUID_AT(IJKNB).AND.EP_G(IJKNB)/=UNDEFINED) THEN
                  NB_FOUND = .TRUE.
                  WRITE (*, 1010) MyPE, I,J,K

                  EP_G(IJK)   = EP_G(IJKNB)
                  P_G(IJK)    = P_G(IJKNB)
                  P_STAR(IJK) = P_STAR(IJKNB)
                  RO_G(IJK)   = RO_G(IJKNB)
                  ROP_G(IJK)  = ROP_G(IJKNB)
                  T_G(IJK)    = T_G(IJKNB)

                  T_s(IJK,1:MMAX) = T_s(IJKNB,1:MMAX)

                  U_s(IJK,1:MMAX) = U_s(IJKNB,1:MMAX)
                  V_s(IJK,1:MMAX) = V_s(IJKNB,1:MMAX)
                  W_s(IJK,1:MMAX) = W_s(IJKNB,1:MMAX)

                  X_g(IJK,1:NMAX(0)) = X_g(IJKNB,1:NMAX(0))

                  U_G(IJK) = U_G(IJKNB)
                  V_G(IJK) = V_G(IJKNB)
                  W_G(IJK) = W_G(IJKNB)

                  ROP_S(IJK,1:MMAX) = ROP_S(IJKNB,1:MMAX)

                  IF(ANY(SOLVE_ROs).OR.ANY(USR_ROS)) RO_S(IJK,1:MMAX) = RO_S(IJKNB,1:MMAX)

                  THETA_M(IJK,1:MMAX) = THETA_M(IJKNB,1:MMAX)

                  DO M = 1,MMAX
                     DO NN = 1, NMAX(M)
                        X_S(IJK,M,NN)= X_S(IJKNB,M,NN)
                     ENDDO
                  ENDDO

                  DO NN = 1, NScalar
                     Scalar(IJK,NN) = Scalar(IJKNB,NN)
                  END DO

                  GAMA_RG(IJK) = GAMA_RG(IJKNB)
                  T_RG(IJK)    = T_RG(IJKNB)

                  GAMA_RS(IJK,1:MMAX) = GAMA_RS(IJKNB,1:MMAX)
                  T_RS(IJK,1:MMAX)    = T_RS(IJKNB,1:MMAX)

                  DO NN = 1, nRR
                     ReactionRates(IJK,NN) = ReactionRates(IJKNB,NN)
                  END DO

                  IF (K_Epsilon) THEN
                     K_Turb_G(IJK) = K_Turb_G(IJKNB)
                     E_Turb_G(IJK) = E_Turb_G(IJKNB)
                  ENDIF

                  EXIT ! Exit as soon as first valid neighbor cell is found
               ENDIF  ! NB is a fluid cell

            ENDDO ! NB Loop

            IF(.NOT.NB_FOUND) WRITE (*, 1020) MyPE, I,J,K   ! NO FLUID CELL AMONG NEIGBHORS

         ENDIF ! New fuid cell

      ENDDO ! IJK loop

1010  FORMAT(1X,'PATCHING NEW FLUID CELL UPON RESTART: MyPE,I,J,K =' ,I6,I6,I6,I6)
1020  FORMAT(1X,'UNABLE TO PATCH NEW FLUID CELL UPON RESTART: MyPE,I,J,K =' ,I6,I6,I6,I6)
   END SUBROUTINE PATCH_AFTER_RESTART

END MODULE READ_RES1_MOD
