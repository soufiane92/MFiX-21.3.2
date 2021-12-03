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
      use param1, only: undefined

      IMPLICIT NONE

      INCLUDE 'usrnlst.inc'

!<keyword category="Run Control" required="false" dtype="REAL">
!  <description>
!    Pre-exponential factor for SiH₄ <--> SiH₂ + H₂ (forward rate)
!
!    Unit: kg-mol/m³s
!  </description>
!  <valid value="undefined" note=": value is UNDEFINED if not set in .mfx file."/>
!  <valid value=">0" note=": user specified constant value (Advanced pane in GUI or
!                            manually added to .mfx file)"/>
!  <range min="+0.0"/>
A1F = UNDEFINED
!</keyword>


!<keyword category="Run Control" required="false" dtype="REAL">
!  <description>
!    Pre-exponential factor for SiH₄ <--> SiH₂ + H₂ (reverse rate)
!
!    Unit: kg-mol/m³s
!  </description>
!  <valid value="undefined" note=": value is UNDEFINED if not set in .mfx file."/>
!  <valid value=">0" note=": user specified constant value (Advanced pane in GUI or
!                            manually added to .mfx file)"/>
!  <range min="+0.0"/>
A1R = UNDEFINED
!</keyword>


!<keyword category="Run Control" required="false" dtype="REAL">
!  <description>
!    Pre-exponential factor for Si₂H₆ <--> SiH₄ + SiH₂ (reverse rate)
!
!    Unit: kg-mol/m³s
!  </description>
!  <valid value="undefined" note=": value is UNDEFINED if not set in .mfx file."/>
!  <valid value=">0" note=": user specified constant value (Advanced pane in GUI or
!                            manually added to .mfx file)"/>
!  <range min="+0.0"/>
A2F = UNDEFINED
!</keyword>


!<keyword category="Run Control" required="false" dtype="REAL">
!  <description>
!    Pre-exponential factor for Si₂H₆ <--> SiH₄ + SiH₂ (reverse rate)
!
!    Unit: kg-mol/m³s
!  </description>
!  <valid value="undefined" note=": value is UNDEFINED if not set in .mfx file."/>
!  <valid value=">0" note=": user specified constant value (Advanced pane in GUI or
!                            manually added to .mfx file)"/>
!  <range min="+0.0"/>
A2R = UNDEFINED
!</keyword>

      RETURN
      END SUBROUTINE USR_INIT_NAMELIST
