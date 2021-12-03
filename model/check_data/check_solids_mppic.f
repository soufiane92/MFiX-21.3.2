#include "error.inc"

MODULE CHECK_SOLIDS_MPPIC_MOD

   USE error_manager

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: CHECK_SOLIDS_MPPIC                                      !
!  Purpose:                                                            !
!                                                                      !
!  Author: J.Musser                                   Date: 02-FEB-14  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE CHECK_SOLIDS_MPPIC

      USE param1
      USE geometry
      USE funits
      USE discretelement
      USE constant
      USE physprop
      USE fldvar
      USE toleranc
      USE mfix_pic
      USE cutcell
      USE functions

      USE mpi_utility

      implicit none

!-----------------------------------------------

      IF(MPPIC_COEFF_EN1 == UNDEFINED) THEN
         WRITE(ERR_MSG, 1000) 'MPPIC_COEFF_EN1'
         CALL LOG_ERROR()

      ELSEIF(MPPIC_COEFF_EN1 > ONE .OR.                                &
         MPPIC_COEFF_EN1 < ZERO) THEN
         WRITE(ERR_MSG, 1001) 'MPPIC_COEFF_EN1',                       &
            trim(iVal(MPPIC_COEFF_EN1))
         CALL LOG_ERROR()
      ENDIF

     IF(MPPIC_VELFAC_COEFF > ONE .OR.                                 &
        MPPIC_VELFAC_COEFF < ZERO) THEN
        WRITE(ERR_MSG, 1001) 'MPPIC_VELFAC_COEFF',                    &
           trim(iVal(MPPIC_VELFAC_COEFF))
        CALL LOG_ERROR()
     ENDIF

      IF(MPPIC_COEFF_EN_WALL > ONE .OR.                                &
         MPPIC_COEFF_EN_WALL < ZERO) THEN
         WRITE(ERR_MSG, 1001) 'MPPIC_COEFF_EN_WALL',                   &
            trim(iVal(MPPIC_COEFF_EN_WALL))
         CALL LOG_ERROR()
      ENDIF

      IF(MPPIC_COEFF_ET_WALL > ONE .OR.                                &
         MPPIC_COEFF_ET_WALL < ZERO) THEN
         WRITE(ERR_MSG, 1001) 'MPPIC_COEFF_ET_WALL',                   &
            trim(iVal(MPPIC_COEFF_ET_WALL))
         CALL LOG_ERROR()
      ENDIF

      IF(FRIC_EXP_PIC < 2.0d0 .OR.                                     &
         FRIC_EXP_PIC > 5.0d0) THEN
         WRITE(ERR_MSG, 1001) 'FRIC_EXP_PIC',                          &
            trim(iVal(FRIC_EXP_PIC))
         CALL LOG_ERROR()
      ENDIF

      IF(PSFAC_FRIC_PIC < ZERO) THEN
         WRITE(ERR_MSG, 1001) 'PSFAC_FRC_PIC',                         &
            trim(iVal(PSFAC_FRIC_PIC))
         CALL LOG_ERROR()
      ENDIF

      IF(PIC_CFL/=UNDEFINED.AND.(PIC_CFL <= ZERO .OR.                                          &
         PIC_CFL > ONE)) THEN
         WRITE(ERR_MSG, 1001) 'PIC_CFL',                               &
            trim(iVal(PIC_CFL))
         CALL LOG_ERROR()
      ENDIF

      IF(PIC_CFL_PARCEL_FRACTION <= ZERO .OR.                          &
         PIC_CFL_PARCEL_FRACTION > ONE) THEN
         WRITE(ERR_MSG, 1001) 'PIC_CFL_PARCEL_FRACTION',               &
            trim(iVal(PIC_CFL_PARCEL_FRACTION))
         CALL LOG_ERROR()
      ENDIF


      IF(PIC_CFL_CONTROL /= 'MAX' .AND.                        &
         PIC_CFL_CONTROL /= 'AVG') THEN
         WRITE(ERR_MSG, 1001) 'PIC_CFL_CONTROL',               &
            trim(PIC_CFL_CONTROL)
         CALL LOG_ERROR()
      ENDIF

 1000 FORMAT('Error 1000: Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

 1001 FORMAT('Error 1001: Illegal or unknown input: ',A,' = ',A,/   &
         'Please correct the project settings.')

      RETURN

   END SUBROUTINE CHECK_SOLIDS_MPPIC

END MODULE CHECK_SOLIDS_MPPIC_MOD
