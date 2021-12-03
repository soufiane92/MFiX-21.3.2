module time_cpu

   use run, only: time, tstop

! cpu time/second
      DOUBLE PRECISION :: CPUos
! old cpu time and time for calculating CPUos
      DOUBLE PRECISION :: CPU_NLOG, TIME_NLOG
! Initial value of CPU time.
      DOUBLE PRECISION :: CPU0
! Time for IO
      DOUBLE PRECISION :: CPU_IO = 0.0d0

! Initial value of CPU time at the begin of MFIX, prior any I/O
      DOUBLE PRECISION :: CPU00
      DOUBLE PRECISION :: WALL0

! Time at start of simulation
      DOUBLE PRECISION :: TIME_START

! Wall time at the beginning
      DOUBLE PRECISION :: WALL_START

! Wall time at the most recent pause
      DOUBLE PRECISION :: WALL_PAUSE_START

! Total wall time spent in PAUSED state
      DOUBLE PRECISION :: WALL_PAUSED = 0.0d0

contains

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Function name: WALL_TIME (CPU)                                      C
!  Purpose: returns wall time since start of the run                   C
!                                                                      C
!  Author: P. Nicoletti                               Date: 10-JAN-92  C
!  Reviewer: P. Nicoletti, W. Rogers, M. Syamlal      Date:            C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced: None                                          C
!  Variables modified: None                                            C
!                                                                      C
!  Local variables: TA, XT                                             C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
   double precision recursive function wall_time()

      implicit none

      INTEGER(KIND=8), SAVE :: COUNT_OLD=0, WRAP=0, COUNT_START=0
      LOGICAL, SAVE :: FIRST_CALL=.true.

      INTEGER(KIND=8)   CLOCK_CYCLE_COUNT, CLOCK_CYCLES_PER_SECOND

!                       max number of cycles; after which count is reset to 0
      INTEGER(KIND=8)   CLOCK_CYCLE_COUNT_MAX

      CALL SYSTEM_CLOCK(CLOCK_CYCLE_COUNT, CLOCK_CYCLES_PER_SECOND, CLOCK_CYCLE_COUNT_MAX)

      IF (FIRST_CALL) THEN
         FIRST_CALL = .false.
         COUNT_START = CLOCK_CYCLE_COUNT
      END IF

      IF(COUNT_OLD .GT. CLOCK_CYCLE_COUNT) THEN
!     This is unlikely. 64-bit INTEGER and 100 MHz CLOCK_CYCLES_PER_SECOND would mean 300 years until WRAP is incremented.
         WRAP = WRAP + 1
      ENDIF
      COUNT_OLD = CLOCK_CYCLE_COUNT

      WALL_TIME = DBLE(CLOCK_CYCLE_COUNT - COUNT_START)/DBLE(CLOCK_CYCLES_PER_SECOND) &
                + DBLE(WRAP) * DBLE(CLOCK_CYCLE_COUNT_MAX)/DBLE(CLOCK_CYCLES_PER_SECOND)
   end function wall_time

   double precision recursive function get_cpu_time()
      double precision :: time
      call cpu_time(time)
      get_cpu_time = time
   end function get_cpu_time

   function remaining_walltime_estimate() result(est_remaining)
      implicit none

      double precision :: est_remaining

      est_remaining = 0.0
      if (TIME > TIME_START) then
         est_remaining = (WALL_TIME() - WALL_START)*(TSTOP-TIME)/(TIME-TIME_START)
      end if
   end function remaining_walltime_estimate

end module time_cpu
