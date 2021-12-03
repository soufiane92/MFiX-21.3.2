!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR_INIT_NAMELIST                                      C
!  Purpose: initialize user_defined NAMELIST variables                 C
!                                                                      C
!  Author:                                            Date:            C
!  Reviewer:                                          Date:            C
!                                                                      C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced: None                                          C
!  Variables modified:                                                 C
!                                                                      C
!  Local variables:                                                    C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
      SUBROUTINE USR_INIT_NAMELIST

      USE USR

      IMPLICIT NONE

      INCLUDE 'usrnlst.inc'


!<keyword dtype="LOGICAL" category="Run Control" required="false" locked="true">
!  <description>Read a keyframe data file.</description>
!  <arg index="1" id="Keyframe" min="0" max="100"/>
!  <valid value=".TRUE." note="Read keyframe data file."/>
!  <valid value=".FALSE." note="Do not read keyframe data file."/>
      READ_KF(:100) = .FALSE.
!</keyword>

      RETURN
      END SUBROUTINE USR_INIT_NAMELIST
