MODULE DES_PHYSICAL_PROP_MOD
CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Subroutine name: PHYSICAL_PROP                                      !
!                                                                      !
!  Purpose: Calculate physical properties that vary with time.         !
!                                                                      !
!  Author: J.Musser                                   Date: 09-May-11  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE DES_PHYSICAL_PROP

      USE run, ONLY: ENERGY_EQ

      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! Logical indicating to write additional data about the particle for
! debugging purposes.

!......................................................................!

! Specific heat
!-----------------------------------------------------------------------
! This only needs calculated when solving the energy equations.
      IF(ENERGY_EQ) THEN
      ENDIF

      RETURN

   END SUBROUTINE DES_PHYSICAL_PROP

END MODULE DES_PHYSICAL_PROP_MOD
