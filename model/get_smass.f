#include "error.inc"

MODULE GET_SMASS_MOD

   USE check, only: accumulation
   USE discretelement, only: des_mmax, max_pip, pijk, pmass
   USE error_manager, only: err_msg, loglevel_info, log_message
   USE fldvar, only: rop_s
   USE functions, only: is_normal
   USE mfix_pic, only: DES_STAT_WT
   USE mpi_utility, only: global_all_sum
   USE output, only: BREAKDOWN_SOLID_INVENTORY_BY_PHASE
   USE param1, only: zero
   USE physprop, only: mmax
   USE run, only: time, UNITS

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: GET_SMASS(SMASS)                                       C
!  Purpose: Determine the weight of solids in the reactor              C
!                                                                      C
!  Author: M. Syamlal                                 Date: 30-JAN-92  C
!  Reviewer: P. Nicoletti, W. Rogers, S. Venkatesan   Date: 31-JAN-92  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced: IMAX2, JMAX2, KMAX2, MMAX, ROP_s, DX, DY, DZ, C
!                        X, IMIN1, JMIN1. KMIN1                        C
!  Variables modified: I, J, K, M, IJK                                 C
!                                                                      C
!  Local variables:  None                                              C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
      SUBROUTINE GET_SMASS(SMASS)

      IMPLICIT NONE

!                      Total Weight of solids in the reactor
      DOUBLE PRECISION SMASS
!
!                      Solids phase
      INTEGER          M
!
!-----------------------------------------------

      SMASS = ZERO
      DO M = 1, MMAX
         SMASS = SMASS + Accumulation(ROP_s(1, M))
      END DO

      RETURN
      END SUBROUTINE GET_SMASS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: GET_TFM_SMASS                                          C
!  Purpose: Determine the TFM mass of solids in the reactor            C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 09-NOV-17  C
!  Reviewer:                                          Date:            C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
      SUBROUTINE GET_TFM_SMASS

      IMPLICIT NONE

!  Mass of each DEM solids phase
      DOUBLE PRECISION :: TFM_MASS(1:MMAX)
!  Total Mass of all DEM solids phases
      DOUBLE PRECISION :: TOTAL_TFM_MASS

! Loop counters: particles, filter cells, phases
      INTEGER:: M, mLB, mUB
      CHARACTER(LEN=3) :: mass_unit   ! mass unit
!-----------------------------------------------

      TFM_MASS(:) = ZERO

      mLB = 1
      mUB = MMAX

      DO M = mLB,mUB
         TFM_MASS(M) =  Accumulation(ROP_s(1, M))
      END DO

      ! DO M = mLB,mUB
      !    CALL GLOBAL_ALL_SUM(TFM_MASS(M))
      ! ENDDO

      TOTAL_TFM_MASS = SUM(TFM_MASS(:))

      IF(UNITS=='CGS') THEN
         mass_unit = ' g'
      ELSE
         mass_unit = ' kg'
      ENDIF

      WRITE(ERR_MSG, 1300) 'Start'
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
      WRITE(ERR_MSG, 1000) TIME
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

      IF(mUB>mLB.AND.BREAKDOWN_SOLID_INVENTORY_BY_PHASE) THEN
         DO M = mLB,mUB
            WRITE(ERR_MSG, 1100) M,M,TFM_MASS(M),mass_unit
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
         ENDDO
      ENDIF

      WRITE(ERR_MSG, 1200) TOTAL_TFM_MASS, mass_unit
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
      WRITE(ERR_MSG, 1300) '  End'
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

1000  FORMAT('TFM solids mass at time = ',E12.5,' sec:')
1100  FORMAT('For phase ',I3,',   Sm_',I3.3,' = ',E12.5, A)
1200  FORMAT('               Total Sm = ',E12.5, A,/)
1300  FORMAT('==========',1x,A,1x,'of solids inventory report ==========')

      RETURN
      END SUBROUTINE GET_TFM_SMASS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: GET_DEM_SMASS                                          C
!  Purpose: Determine the DEM mass of solids in the reactor            C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 09-NOV-17  C
!  Reviewer:                                          Date:            C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
      SUBROUTINE GET_DEM_SMASS

      IMPLICIT NONE

!  Mass of each DEM solids phase
      DOUBLE PRECISION :: DEM_MASS(MMAX+1:DES_MMAX+MMAX)
!  Total Mass of all DEM solids phases
      DOUBLE PRECISION :: TOTAL_DEM_MASS

! Loop counters: particles, filter cells, phases
      INTEGER:: NP, M, mLB, mUB
      CHARACTER(LEN=3) :: mass_unit   ! mass unit
!-----------------------------------------------

      DEM_MASS(:) = ZERO

      mLB = MMAX+1
      mUB = DES_MMAX+MMAX

      DO NP=1,MAX_PIP
         IF(.NOT.IS_NORMAL(NP)) CYCLE

! Particle phase for data binning.
         M = PIJK(NP,5)

         DEM_MASS(M) = DEM_MASS(M) + PMASS(NP)

      ENDDO

      DO M = mLB,mUB
         CALL GLOBAL_ALL_SUM(DEM_MASS(M))
      ENDDO

      TOTAL_DEM_MASS = SUM(DEM_MASS(:))

      IF(UNITS=='CGS') THEN
         mass_unit = ' g'
      ELSE
         mass_unit = ' kg'
      ENDIF

      WRITE(ERR_MSG, 1300) 'Start'
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
      WRITE(ERR_MSG, 1000) TIME
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

      IF(mUB>mLB.AND.BREAKDOWN_SOLID_INVENTORY_BY_PHASE) THEN
         DO M = mLB,mUB
            WRITE(ERR_MSG, 1100) M,M,DEM_MASS(M),mass_unit
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
         ENDDO
      ENDIF

      WRITE(ERR_MSG, 1200) TOTAL_DEM_MASS, mass_unit
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
      WRITE(ERR_MSG, 1300) '  End'
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

1000  FORMAT('DEM solids mass at time = ',E12.5,' sec:')
1100  FORMAT('For phase ',I3,',   Sm_',I3.3,' = ',E12.5, A)
1200  FORMAT('               Total Sm = ',E12.5, A,/)
1300  FORMAT('==========',1x,A,1x,'of solids inventory report ==========')

      RETURN

   END SUBROUTINE GET_DEM_SMASS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: GET_PIC_SMASS                                          C
!  Purpose: Determine the PIC mass of solids in the reactor            C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 16-JAN-19  C
!  Reviewer:                                          Date:            C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
      SUBROUTINE GET_PIC_SMASS

      IMPLICIT NONE

!  Mass of each PIC solids phase
      DOUBLE PRECISION :: PIC_MASS(MMAX+1:DES_MMAX+MMAX)
!  Total Mass of all PIC solids phases
      DOUBLE PRECISION :: TOTAL_PIC_MASS

! Loop counters: particles, filter cells, phases
      INTEGER:: NP, M, mLB, mUB
      CHARACTER(LEN=3) :: mass_unit   ! mass unit
!-----------------------------------------------

      PIC_MASS(:) = ZERO

      mLB = MMAX+1
      mUB = DES_MMAX+MMAX

      DO NP=1,MAX_PIP
         IF(.NOT.IS_NORMAL(NP)) CYCLE

! Particle phase for data binning.
         M = PIJK(NP,5)

         PIC_MASS(M) = PIC_MASS(M) + PMASS(NP) * DES_STAT_WT(NP)

      ENDDO

      DO M = mLB,mUB
         CALL GLOBAL_ALL_SUM(PIC_MASS(M))
      ENDDO

      TOTAL_PIC_MASS = SUM(PIC_MASS(:))

      IF(UNITS=='CGS') THEN
         mass_unit = ' g'
      ELSE
         mass_unit = ' kg'
      ENDIF

      WRITE(ERR_MSG, 1300) 'Start'
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
      WRITE(ERR_MSG, 1000) TIME
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

      IF(mUB>mLB.AND.BREAKDOWN_SOLID_INVENTORY_BY_PHASE) THEN
         DO M = mLB,mUB
            WRITE(ERR_MSG, 1100) M,M,PIC_MASS(M),mass_unit
            CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
         ENDDO
      ENDIF

      WRITE(ERR_MSG, 1200) TOTAL_PIC_MASS, mass_unit
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
      WRITE(ERR_MSG, 1300) '  End'
      CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

1000  FORMAT('PIC solids mass at time = ',E12.5,' sec:')
1100  FORMAT('For phase ',I3,',   Sm_',I3.3,' = ',E12.5, A)
1200  FORMAT('               Total Sm = ',E12.5, A,/)
1300  FORMAT('==========',1x,A,1x,'of solids inventory report ==========')

      RETURN

   END SUBROUTINE GET_PIC_SMASS

end module get_smass_mod
