#include "error.inc"

MODULE CALC_EPG_DES_MOD
CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Subroutine: CALC_EPG_DES                                            !
!  Author: R.Garg                                     Date: ??-???-??  !
!                                                                      !
!  Purpose: Calculate the gas phase volume fraction (and in turn the   !
!  gas phase bulk density) from the sum of the solids volume fractions.!
!                                                                      !
!  NOTE: This routine uses a global communication to notify all ranks  !
!  of potential errors. Therefore all ranks can call MFIX_EXIT and     !
!  prevent dead-lock. Communications may be reduced by passing the     !
!  flag back to the caller and combining with other error checks.      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CALC_EPG_DES


      use discretelement, only: valid_fluid_indices,ncells_valid
      use run, only :optflag1

! Global Variables:
!---------------------------------------------------------------------//
! Flag: Discrete and continuum solids co-exist
      use discretelement, only: DES_CONTINUUM_COUPLED
! Number of discrete solids phases
      use discretelement, only: DES_MMAX
! Global ID of particles
      use discretelement, only: iGLOBAL_ID
! Particle positions
      use discretelement, only: DES_POS_NEW
! Number of continuum solids phases
      use physprop, only: MMAX
! Number of particles in indexed fluid cell
      use discretelement, only: PINC
! List of particles in each cell.
      use derived_types, only: PIC
! Gas phae volume fraction, density, and build density
      use fldvar, only: EP_G, RO_G, ROP_G
! Bulk density of continuum solids phases
      use fldvar, only: EP_S
! Volume of scalar grid cell.
      use geometry, only: VOL
! Flag: Status of indexed cell
      use cutcell, only: CUT_CELL_AT
! Flag: Indexed cell contains fluid
      USE functions, only: FLUID_AT
! Fluid grid loop bounds.
      use compar, only: IJKStart3, IJKEnd3
! Flag: Fluid exists at indexed cell
      use functions, only: FLUID_AT
! The I, J, and K values that comprise an IJK
      use indices, only: I_OF, J_OF, K_OF
! Global communication function to sum to all ranks.
      use mpi_utility, only: GLOBAL_ALL_SUM
! Flag for PIC simulation
      use mfix_pic, only: MPPIC
! packing limit
      use constant, only: ep_star
! Undefined
      USE param1, only: undefined

      USE open_files_mod, only: open_pe_log
      USE write_des_data_mod, only: write_des_data

! Global Parameters:
!---------------------------------------------------------------------//
      USE param1, only: ZERO, ONE

      use error_manager

      IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//
! Loop indices
      INTEGER :: IJK, M, LC
! Total solids volume fraction
      DOUBLE PRECISION :: SUM_EPS
! Packed
      DOUBLE PRECISION :: PACKED_EPS
! Integer Error Flag
      INTEGER :: IER

!sitaraman NREL CU optimizations
    integer :: start_index, end_index, do_index

!......................................................................!

! Initialize error flag.
      IER = 0

! Set a max solids volume fraction for MPPIC. The value is arbitrarily
! larger than EP_STAR. However, the value should be large enough so
! that it is rarely used. This was added as a crude work around for
! poor initial conditions that start cells overpacked.
      PACKED_EPS = ONE
      IF(MPPIC) THEN
         PACKED_EPS = 0.9D0
      ELSEIF(EP_STAR/=UNDEFINED) THEN
         PACKED_EPS = ONE - EP_STAR
      ENDIF

if (optflag1.eq.1) then
  start_index = 1
  end_index = ncells_valid
else
  start_index = IJKSTART3
  end_index = IJKEND3
endif

! Calculate gas volume fraction from solids volume fraction:
!---------------------------------------------------------------------//
!$omp parallel do if(ijkend3 .ge. 2000) default(none) reduction(+:IER) &
!$omp shared(IJKSTART3, IJKEND3, DES_CONTINUUM_COUPLED, DES_MMAX, MMAX,&
!$omp        EP_G, RO_G, ROP_G, MPPIC, PACKED_EPS, optflag1, start_index, end_index) &
!$omp private(IJK, SUM_EPs, M) &
!$omp shared(ncells_valid,valid_fluid_indices)

DO do_index = start_index, end_index

  if (optflag1.eq.1) then
    ijk = valid_fluid_indices(do_index)
  else
    ijk = do_index
    IF(.NOT.FLUID_AT(IJK)) CYCLE
  endif

! Initialize EP_g and the accumulator.
         EP_G(IJK) = ONE
         SUM_EPS = ZERO
! Sum the DES solids volume fraction.  If hybrid TFM solids contributions
! will be included
         DO M = 1, DES_MMAX+MMAX
            SUM_EPS = SUM_EPS + EP_S(IJK,M)
         ENDDO

! Calculate the gas phase volume fraction.
         EP_G(IJK) = ONE - min(PACKED_EPS, SUM_EPS)

! Calculate the gas phase bulk density.
         ROP_G(IJK) = RO_G(IJK) * EP_G(IJK)
! Flag an error if gas volume fraction is unphysical.
         IF(DES_CONTINUUM_COUPLED) THEN
            IF(EP_G(IJK) <= ZERO .OR. EP_G(IJK) > ONE) IER = IER + 1
         ENDIF
      ENDDO
!omp end parallel do


      CALL GLOBAL_ALL_SUM(IER)
      IF(IER == 0) RETURN


! Report any errors. Volume fraction errors are fatal.
!---------------------------------------------------------------------//
      CALL OPEN_PE_LOG(IER)

      WRITE(ERR_MSG, 1100)
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, FOOTER=.FALSE.)

 1100 FORMAT('Error 1100: Unphysical gas phase volume fraction ',      &
         'calculated. A .vtp',/'file will be written and the code ',   &
         'will exit. Fluid cell details:')

         DO IJK=IJKSTART3, IJKEND3
            IF(.NOT.FLUID_AT(IJK)) CYCLE
            IF(EP_G(IJK) > ZERO .AND. EP_G(IJK) <= ONE) CYCLE

            WRITE(ERR_MSG,1101) trim(iVal(IJK)), trim(iVal(I_OF(IJK))),&
               trim(iVal(J_OF(IJK))), trim(iVal(K_OF(IJK))),EP_G(IJK), &
               CUT_CELL_AT(IJK), trim(iVal(PINC(IJK))), VOL(IJK)
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

            WRITE(ERR_MSG,1102)
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
            DO LC=1,PINC(IJK)
               M=PIC(IJK)%P(LC)
               WRITE(ERR_MSG,1103) iGlobal_ID(M), trim(iVal(           &
                  DES_POS_NEW(M,1))), trim(iVal(DES_POS_NEW(M,2))),    &
                  trim(iVal(DES_POS_NEW(M,3)))
               CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
            ENDDO
         ENDDO

 1101 FORMAT(/3x,'Fluid Cell IJK: ',A,6x,'I/J/K: (',A,',',A,',',A,')',/&
         T6,'EP_G = ',g11.4,T30,'CUT_CELL_AT = ',L1,/T6,'PINC: ',A,T30,&
         'VOL = ',g11.4)

 1102 FORMAT(/T6,'Global ID',T30,'Position')

 1103 FORMAT(T6,I9,3x,'(',A,', ',A,', ',A,')')

      WRITE(ERR_MSG, 1104)
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE.)
 1104 FORMAT('This is a fatal error. A particle output file (vtp) ',   &
         'will be written',/'to aid debugging.')

      CALL WRITE_DES_DATA
      call log_error()

   END SUBROUTINE CALC_EPG_DES

END MODULE CALC_EPG_DES_MOD
