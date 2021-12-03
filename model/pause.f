! -*- f90 -*-

      MODULE PAUSE

         USE EXIT, ONLY : EXIT_FLAG, CHECK_EXIT_FLAG
         USE COMPAR, ONLY : MYPE
         USE MPI_UTILITY
         USE REINIT, ONLY: REINITIALIZE
         USE ERROR_MANAGER, ONLY: REINIT_ERROR
         USE TIME_CPU, ONLY:  WALL_PAUSE_START, WALL_PAUSED, WALL_TIME
         USE RESET_NEW_MOD, ONLY: RESET_NEW
         use, intrinsic:: iso_c_binding, only: c_int

         IMPLICIT NONE

         interface
            subroutine usleep(us) bind (C)
              import c_int
              integer(c_int), value :: us
           end subroutine usleep
        end interface


         LOGICAL :: PAUSE_FLAG = .FALSE.
         LOGICAL :: REINIT_FLAG = .FALSE.
         LOGICAL :: AUTOSTART_FLAG = .FALSE.  ! automatically restart after successful reinit
         INTEGER :: IER = 0

! TODO dynamically allocate this, instead of fixed-length
      CHARACTER(100000):: REINIT_DATA

      CONTAINS
      SUBROUTINE WAIT_WHILE_PAUSED
#ifdef MFIX_INTERACTIVE
         DOUBLE PRECISION :: WALL_PAUSED_PREV

         IF (CHECK_EXIT_FLAG()) RETURN

         WALL_PAUSED_PREV = WALL_PAUSED
         IF (PAUSED_FLAG_BCAST()) THEN
            WALL_PAUSE_START = WALL_TIME()
            IF (MYPE .EQ. 0) THEN
               PRINT *, "Paused"
!           ELSE
!              PRINT *, "Paused", MYPE
            END IF

            DO WHILE (PAUSED_FLAG_BCAST() .AND. .NOT. CHECK_EXIT_FLAG())
               IF (CHECK_REINIT_FLAG()) THEN
                  CALL DO_REINIT
                  REINIT_FLAG = .FALSE.
               ELSE
                  CALL usleep(100000) ! 10 Hz
!                 CALL SLEEP(1) ! 1 Hz
                  WALL_PAUSED = WALL_PAUSED_PREV + WALL_TIME() - WALL_PAUSE_START
               END IF
            END DO

            IF (CHECK_REINIT_FLAG()) THEN
               CALL DO_REINIT
               REINIT_FLAG = .FALSE.
            END IF

            IF (MYPE .EQ. 0) THEN
               PRINT *, "Resuming"
!           ELSE
!              PRINT *, "Resuming", MYPE
            END IF
            WALL_PAUSED = WALL_PAUSED_PREV + WALL_TIME() - WALL_PAUSE_START

         END IF
#endif
      END SUBROUTINE WAIT_WHILE_PAUSED

#ifdef MFIX_INTERACTIVE
      LOGICAL FUNCTION PAUSED_FLAG_BCAST()
         LOGICAL :: LOCAL
         LOCAL = PAUSE_FLAG ! Assuming atomic assignment of boolean
         CALL BCAST(LOCAL)
         PAUSED_FLAG_BCAST = LOCAL
      END FUNCTION PAUSED_FLAG_BCAST


      LOGICAL FUNCTION CHECK_REINIT_FLAG()
         LOGICAL :: LOCAL
         LOCAL = REINIT_FLAG ! Assuming atomic assignment of boolean
         CALL BCAST(LOCAL)
         CHECK_REINIT_FLAG = LOCAL

         IF(CHECK_REINIT_FLAG) CALL BCAST(REINIT_DATA)  ! Really should use mutex for here and SET_REINIT_DATA

      END FUNCTION CHECK_REINIT_FLAG


      SUBROUTINE SET_REINIT_DATA(MFIX_DAT)
         CHARACTER(LEN=*) MFIX_DAT
 !f2py intent(in) MFIX_DAT
         REINIT_DATA = MFIX_DAT
      END SUBROUTINE SET_REINIT_DATA


      SUBROUTINE DO_REINIT

         IF (MYPE .EQ. 0) THEN
            PRINT *, "Reinitializing"
         END IF


         CALL BCAST(REINIT_DATA)
         CALL RESET_NEW(REINIT_DATA) ! we might be paused in the middle of a time step


         CALL REINITIALIZE(REINIT_DATA, IER)

         IF (CHECK_EXIT_FLAG() .OR. REINIT_ERROR()) THEN
            IF (MYPE .EQ. 0) THEN
               PRINT *, "Reinitialization failed"
            END IF
            EXIT_FLAG = .FALSE.  !! allow user to retry. TODO review this
         ELSE
            IF (AUTOSTART_FLAG) THEN
               AUTOSTART_FLAG = .FALSE.
               PAUSE_FLAG = .FALSE.
            END IF
         END IF

      END SUBROUTINE DO_REINIT


#endif
      END MODULE PAUSE
