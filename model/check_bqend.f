MODULE CHECK_BATCH_QUEUE_END_MOD

   use compar, only: PE_IO
   use error_manager, only: err_msg, loglevel_info, log_message
   use mpi_utility, only: BCAST
   use run, only: BATCH_WALLCLOCK, CHK_BATCHQ_END, TERM_BUFFER, GET_TUNIT
   use time_cpu, only: WALL_START, get_cpu_time

CONTAINS

!----------------------------------------------------------------------!
!                                                                      !
!  Subroutine: CHECK_BATCH_QUEUE_END                                   !
!  Author: A.Gel                                      Date:            !
!                                                                      !
!  Purpose:                                                            !
!                                                                      !
!----------------------------------------------------------------------!
   SUBROUTINE CHECK_BATCH_QUEUE_END(pEXIT_FLAG)

      IMPLICIT NONE

      LOGICAL, INTENT(INOUT) :: pEXIT_FLAG

      pEXIT_FLAG = pEXIT_FLAG .OR. MFIX_STOP_EXISTS()

      IF (CHK_BATCHQ_END) THEN
         pEXIT_FLAG = pEXIT_FLAG .OR. BATCH_WALLTIME_REACHED()
      END IF

      call bcast (pEXIT_FLAG,PE_IO)

   END SUBROUTINE CHECK_BATCH_QUEUE_END


!----------------------------------------------------------------------!
!                                                                      !
!  Subroutine: CHECK_BATCH_QUEUE_END                                   !
!  Author: Mark Meredith                              Date:            !
!                                                                      !
!  Purpose:  Check if MFIX.STOP exists                                 !
!                                                                      !
!----------------------------------------------------------------------!
   LOGICAL FUNCTION MFIX_STOP_EXISTS()

! Logical flags for halt cases.
      LOGICAL :: USER_HALT

      INQUIRE(file="MFIX.STOP", exist=USER_HALT)
      MFIX_STOP_EXISTS = USER_HALT

! Report that the user signaled halt by creating MFIX.STOP
      IF(MFIX_STOP_EXISTS) THEN
         WRITE(ERR_MSG, 1200)
         CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
      ENDIF

1200  FORMAT(2/,19('='),' MFIX STOP SIGNAL DETECTED ',19('='),/'MFIX.',&
         'STOP file detected in run directory. Terminating MFIX.',/,   &
         /19('='),'MFIX STOP SIGNAL DETECTED ',19('='))

   END FUNCTION MFIX_STOP_EXISTS


!----------------------------------------------------------------------!
!                                                                      !
!  Subroutine: BATCH_WALLTIME_REACHED                                  !
!  Author: Mark Meredith                              Date:            !
!                                                                      !
!  Purpose:  Check if Batch walltime limit is reached                  !
!                                                                      !
!----------------------------------------------------------------------!
   LOGICAL FUNCTION BATCH_WALLTIME_REACHED()

      IMPLICIT NONE

! Elapsed wall time, and fancy formatted buffer/batch queue times.
      DOUBLE PRECISION :: WALL_ELAPSED, FANCY_BUFF, FANCY_BATCH, WALL_CURRENT
! Time units for formatted output.
      CHARACTER(LEN=4) :: WT_UNIT, BF_UNIT, BC_UNIT

! Calculate the current elapsed wall time.
      WALL_CURRENT = get_cpu_time()
      WALL_ELAPSED = WALL_CURRENT - WALL_START

! Set flags for wall time exceeded
      BATCH_WALLTIME_REACHED = ((WALL_ELAPSED+TERM_BUFFER) >= BATCH_WALLCLOCK)

! Report that the max user wall time was reached and exit.
      IF(BATCH_WALLTIME_REACHED) THEN
         CALL GET_TUNIT(WALL_CURRENT,WT_UNIT)
         FANCY_BUFF = TERM_BUFFER
         CALL GET_TUNIT(FANCY_BUFF, BF_UNIT)
         FANCY_BATCH = BATCH_WALLCLOCK
         CALL GET_TUNIT(FANCY_BATCH, BC_UNIT)
         WRITE(ERR_MSG, 1100) FANCY_BATCH, BC_UNIT, WALL_CURRENT, WT_UNIT, FANCY_BUFF, BF_UNIT
         CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)
      ENDIF

1100  FORMAT(2/,15('='),' REQUESTED CPU TIME LIMIT REACHED ',('='),/ &
         'Batch Wall Time:',3X,F9.2,1X,A,/ &
         'Elapsed Wall Time: ',F9.2, 1X,A,/ &
         'Term Buffer:',7X,F9.2,1X,A,/ &
         15('='),' REQUESTED CPU TIME LIMIT REACHED ',('='))
   END FUNCTION BATCH_WALLTIME_REACHED


END MODULE CHECK_BATCH_QUEUE_END_MOD
