#include "error.inc"

MODULE SET_PS_MOD

   use compar, only: pe_io, mype, numpes, dead_cell_at
   use constant, only: GAS_CONST_cal
   use error_manager, only: err_msg, loglevel_error, loglevel_info, log_message
   use functions, only: is_on_mype_owns, funijk, fluid_at
   use geometry, only: do_k, flag, vol
   use mpi_utility, only: global_sum, global_all_sum
   use param1, only: zero, small_number, undefined
   use physprop, only: mmax, nmax, mw_g, database_read, c_pg0, mw_s, c_ps0
   use ps
   use read_database_mod, only: read_database0
   use read_thermochemical, only: calc_CpoR
   use run, only: species_eq, units, energy_eq
   use rxns, only: species_alias_g, species_alias_s

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: CHECK_DATA_09                                          C
!  Purpose: Check point source specifications.                         C
!                                                                      C
!  Author: J. Musser                                  Date: 10-JUN-13  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SET_PS(MFIX_DAT)

      implicit none

      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

      INTEGER :: IJK, I, J, K, M, NN

      INTEGER PSV

      CHARACTER(LEN=64) :: eMsg

      INTEGER :: PS_SIZE

      DOUBLE PRECISION, allocatable :: lData_dp(:)
      DOUBLE PRECISION, allocatable :: gData_dp(:)

      LOGICAL, parameter :: dbg_PS = .FALSE.

      if(.NOT.POINT_SOURCE) return

! DETERMINE WHICH BOUNDARY CONDITION INDICES HAVE VALUES
      L50: do PSV = 1, DIMENSION_PS

         IF(.NOT.PS_DEFINED(PSV)) cycle L50


! Calculate the velocity magnitude and normalize the axial components.
         CALL CALC_PS_VEL_MAG(PS_VEL_MAG_g(PSV), PS_U_g(PSV),          &
            PS_V_g(PSV), PS_W_g(PSV))

         CALL CALC_PS_CpxMFLOW(MFIX_DAT, PS_CpxMFLOW_g(PSV), PS_MASSFLOW_g(PSV), &
            PS_T_g(PSV), PS_X_g(PSV,:), 0, C_PG0, DIM_N_g, MW_g)

         do M=1, MMAX
            CALL CALC_PS_VEL_MAG(PS_VEL_MAG_s(PSV,M), PS_U_s(PSV,M),   &
               PS_V_s(PSV,M), PS_W_s(PSV,M))

            CALL CALC_PS_CpxMFLOW(MFIX_DAT,PS_CpxMFLOW_s(PSV,M),                &
               PS_MASSFLOW_s(PSV,M), PS_T_s(PSV,M), PS_X_s(PSV,M,:), M,&
               C_PS0(M), DIM_N_s, MW_s(M,:))
         enddo


! Calculate the number of cells comprising the point source. This
! information is used to allocate some temp arrays.
!---------------------------------------------------------------------//
         PS_SIZE = (PS_I_E(PSV) - PS_I_W(PSV) + 1) * &
                   (PS_J_N(PSV) - PS_J_S(PSV) + 1)
         if(DO_K) PS_SIZE = PS_SIZE * (PS_K_T(PSV) - PS_K_B(PSV) + 1)

         if(PS_SIZE < 1) then
             eMsg = ''; write(eMsg,"('Invalid PS size: ', I4)")PS_SIZE
             goto 500
         endif


         allocate(lData_dp( 0:numPEs-1)); lData_dp = ZERO
         allocate(gData_dp( 0:numPEs-1)); gData_dp = ZERO


! Calculate the volume of the PointSource cells
!---------------------------------------------------------------------//
! Initialize the loop counter
         PS_VOLUME(PSV) = ZERO

         do k = PS_K_B(PSV), PS_K_T(PSV)
         do j = PS_J_S(PSV), PS_J_N(PSV)
         do i = PS_I_W(PSV), PS_I_E(PSV)

            if(IS_ON_myPE_owns(I, J, K)) then
            IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
               ijk = funijk(i,j,k)

               if(fluid_at(ijk)) &
                  lData_dp(myPE) = lData_dp(myPE) + VOL(IJK)
            endif

         enddo
         enddo
         enddo

! Each process (in DMP) only knows about the volume of the point source
! it sees. Invoke send/recv so that all process can calculate the total
! volume.
         CALL global_all_sum(lData_dp, gData_dp)

         PS_VOLUME(PSV) = sum(gData_dp)
         if(PS_VOLUME(PSV) == ZERO) then
            eMsg = 'No PS_VOLUME == ZERO'
            CALL DEBUG_PS(PSV, PS_SIZE)
            goto 501
         endif

         if(allocated(lData_dp)) deallocate(lData_dp)
         if(allocated(gData_dp)) deallocate(gData_dp)


         IF(dbg_PS) CALL DEBUG_PS(PSV, PS_SIZE)

      enddo L50

      write(err_msg,2000)
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

      do psv = 1, dimension_ps
         if(ps_defined(psv)) then
            write(err_msg,2001) psv, ps_volume(psv)
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
         endif
      enddo

 2000 format(/2x,'Point Source setup information:',/6x,'PS',8x,'Volume')
 2001 format(3x,i3,3x,es15.8)

      return

  500 continue
      write(err_msg,"('PointSource setup Error: ',A)") trim(eMsg)
      call log_error()

  501 continue
      write(err_msg,"('PointSource setup Error: ',I3,2x,A)") myPE, trim(eMsg)
      call log_error()

      RETURN

      contains

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: CALC_PS_CpxMFLOW                                       C
!  Purpose: Calculate velocity magnitude and normalize U/V/W.          C
!                                                                      C
!  Author: J. Musser                                  Date: 10-JUN-13  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CALC_PS_CpxMFLOW(MFIX_DAT, CpxMFLOW, PS_MFLOW, PS_T, PS_X, lM, &
         Cp0, lDIM_N, lMW)

      DOUBLE PRECISION, intent(out) :: CpxMFLOW

      INTEGER, intent(in) :: lM
      INTEGER, intent(in) :: lDIM_N

      DOUBLE PRECISION, intent(in) :: Cp0
      DOUBLE PRECISION, intent(in) :: PS_MFLOW
      DOUBLE PRECISION, intent(in) :: PS_T          ! Temperature
      DOUBLE PRECISION, intent(in) :: PS_X(lDIM_N)  ! Species Mass Frac

      DOUBLE PRECISION, intent(in) :: lMW(lDIM_N)

      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

! If there is no mass flow for this phase, then there is no need to
! calculate a CPxMFLUX. Set it to zero and return.
      if(.NOT.ENERGY_EQ .OR. PS_MFLOW == ZERO) then
         CpxMFLOW = ZERO
         return
      endif

! Calculate the average specific heat.
      if(Cp0 == UNDEFINED) then
         IF(.NOT.database_read) call read_database0(MFIX_DAT)
         CpxMFLOW = ZERO
         do nn = 1, NMAX(lM)
            CpxMFLOW = CpxMFLOW + PS_X(NN) * (GAS_CONST_cal / lMW(NN)) * &
              calc_CpoR(PS_T, lM, NN)
         enddo
      else
         CpxMFLOW = Cp0
      endif

! Unit conversion (if needed)
      if(UNITS == 'SI') CpxMFLOW = 4.183925d3*CpxMFLOW
      CpxMFLOW = CpxMFLOW*PS_MFLOW

      END SUBROUTINE CALC_PS_CpxMFLOW

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: CALC_PS_VEL_MAG                                        C
!  Purpose: Calculate velocity magnitude and normalize U/V/W.          C
!                                                                      C
!  Author: J. Musser                                  Date: 10-JUN-13  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CALC_PS_VEL_MAG(VEL_MAG, lU, lV, lW)

      DOUBLE PRECISION, intent(inout) :: VEL_MAG
      DOUBLE PRECISION, intent(inout) :: lU, lV, lW

! Normalize velocities:
      VEL_MAG = lU**2 + lV**2
      if(DO_K) VEL_MAG = VEL_MAG + lW**2

      VEL_MAG = sqrt(VEL_MAG)

      if(VEL_MAG > small_number) then
         lU = lU/VEL_MAG
         lV = lV/VEL_MAG
         lW = lW/VEL_MAG
      else
         VEL_MAG = ZERO
         lU = ZERO
         lV = ZERO
         lW = ZERO
      endif


      END SUBROUTINE CALC_PS_VEL_MAG


      END SUBROUTINE SET_PS



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: DEBUG_PS                                               C
!                                                                      C
!  Purpose: Write out some information about that point source that    C
!  may be useful in debugging.                                         C
!                                                                      C
!  Author: J. Musser                                  Date: 24-JUN-13  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE DEBUG_PS(lPSV, lPS_SIZE)

      IMPLICIT NONE

! Index of PS source to debug.
      INTEGER, intent(in) :: lPSV
! Number of cells comprising the point source.
      INTEGER, intent(in) :: lPS_SIZE

      INTEGER :: IJK, I, J, K, M, NN

      INTEGER :: lc1, lc2

      INTEGER, allocatable :: lFlags_i(:,:)
      INTEGER, allocatable :: gFlags_i(:,:)

      if(myPE == PE_IO) then
         write(*,"(3/,3x,'Debug Point Source Index: ',I3)") lPSV
         write(*,"(/3x,'Size: ',I4)") lPS_SIZE
      endif

      allocate(lFlags_i(lPS_SIZE,1:2) );   lFlags_i = 0
      allocate(gFlags_i(lPS_SIZE,1:2) );   gFlags_i = 0

      lc1 = 0

      do k = PS_K_B(lPSV), PS_K_T(lPSV)
      do j = PS_J_S(lPSV), PS_J_N(lPSV)
      do i = PS_I_W(lPSV), PS_I_E(lPSV)

         lc1 = lc1 + 1
         if(IS_ON_myPE_owns(I, J, K)) then
         IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
            ijk = funijk(i,j,k)
            if(fluid_at(ijk)) then
               lFlags_i(lc1,1) = myPE
               lFlags_i(lc1,2) = FLAG(IJK)
            endif
         endif
      enddo
      enddo
      enddo

! Collect flag information on root.
      CALL global_sum(lFlags_i, gFlags_i)

! Write some information to the screen.
      if(myPE == PE_IO) then
         write(*,"(/5x,'Location:')")
         write(*,"( 5x,'X:',2(2x,g12.5),' :: ',2(2x,I4))")&
            PS_X_w(lPSV), PS_X_e(lPSV), PS_I_w(lPSV), PS_I_e(lPSV)
         write(*,"( 5x,'Y:',2(2x,g12.5),' :: ',2(2x,I4))")&
            PS_Y_s(lPSV), PS_Y_n(lPSV), PS_J_s(lPSV), PS_J_n(lPSV)
         if(DO_K)write(*,"( 5x,'Z:',2(2x,g12.5),' :: ',2(2x,I4))")&
            PS_Z_b(lPSV), PS_Z_t(lPSV), PS_K_b(lPSV), PS_K_t(lPSV)

         write(*,"(/5x,'Volume: ',g12.5)") PS_VOLUME(lPSV)


         if(PS_MASSFLOW_G(lPSV) > small_number) then
            write(*,"(//5x,'Point Source Gas Phase:')")
            write(*,"(7x,'Mass Flow Rate: ',g12.5)")PS_MASSFLOW_G(lPSV)
            write(*,"(7x,'Velocity Magnitude: ',g12.5)") PS_VEL_MAG_g(lPSV)
            write(*,"(7x,'Normal:')")
            write(*,"(9x,'x-Axis: ',g12.5)")PS_U_g(lPSV)
            write(*,"(9x,'y-Axis: ',g12.5)")PS_V_g(lPSV)
            if(DO_K) write(*,"(9x,'z-Axis: ',g12.5)")PS_W_g(lPSV)
            if(energy_eq) &
               write(*,"(7x,'Temperature: ',g12.5)")PS_T_g(lPSV)
            if(species_eq(0)) then
               write(*,"(7x,'Species Composition:')")
               do nn=1,nmax(0)
                  write(*,"(9x,A,': ',g12.5)") &
                     trim(SPECIES_ALIAS_g(nn)), PS_X_g(lPSV,NN)
               enddo
            endif
         else
            write(*,"(//5x,'No gas phase point source.')")
         endif


         do m=1,mmax
            if(PS_MASSFLOW_S(lPSV,M) > small_number) then
               write(*,"(//5x,'Point Source Solids Phase ',I1,':')")M
               write(*,"(7x,'Mass Flow Rate: ',g12.5)")PS_MASSFLOW_S(lPSV,M)
               write(*,"(7x,'Velocity Magnitude: ',g12.5)") PS_VEL_MAG_s(lPSV,M)
               write(*,"(7x,'Normal:')")
               write(*,"(9x,'x-Axis: ',g12.5)")PS_U_s(lPSV,M)
               write(*,"(9x,'y-Axis: ',g12.5)")PS_V_s(lPSV,M)
               if(DO_K) write(*,"(9x,'z-Axis: ',g12.5)")PS_W_s(lPSV,M)
               if(energy_eq) &
                  write(*,"(7x,'Temperature: ',g12.5)")PS_T_s(lPSV,M)
               if(species_eq(m)) then
                  write(*,"(7x,'Species Composition:')")
                  do nn=1,nmax(m)
                     write(*,"(9x,A,': ',g12.5)") &
                        trim(SPECIES_ALIAS_s(m,nn)), PS_X_s(lPSV,m,NN)
                  enddo
               endif
            else
               write(*,"(//5x,'No solids phase ',I1,' point source.')") m
            endif
         enddo

         write(*,"(//5x,'Point Source Cells:')")
         write(*,"(9x,'IJK',3(6x,A1),3x,'OWNS',3x,'FLAG')") 'I','J','K'

         lc1 = 0
         lc2 = 0
         do k = PS_K_B(lPSV), PS_K_T(lPSV)
         do j = PS_J_S(lPSV), PS_J_N(lPSV)
         do i = PS_I_W(lPSV), PS_I_E(lPSV)
            IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
            lc1 = lc1 + 1
            write(*,"(4x,I8,5(3x,I4))") IJK, I, J, K,  gFlags_i(lc1,:)
            if(gFlags_i(lc1,2) == 1) lc2 = lc2 + 1
         enddo
         enddo
         enddo
         write(*,"('Total fluid cells',I6)") lc2
      endif

      if(allocated(lFlags_i)) deallocate(lFlags_i)
      if(allocated(gFlags_i)) deallocate(gFlags_i)

   END SUBROUTINE DEBUG_PS

END MODULE SET_PS_MOD
