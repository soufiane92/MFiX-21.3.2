#include "error.inc"

MODULE WRITE_HEADER_MOD

   USE error_manager, only: ERR_MSG, LOGLEVEL_STATUS, LOG_MESSAGE
   USE machine, only: ID_NODE, ID_YEAR, ID_MONTH, ID_DAY, ID_HOUR, ID_MINUTE, start_log, end_log
   USE output, only: onemeg
   USE param, only: dimension_3, dimension_m, dimension_n_g, dimension_n_s
   USE run, only: id_version, run_name, project_version

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_HEADER                                           C
!  Purpose: write header information to log output                     C
!                                                                      C
!  Author: M. Syamlal                                 Date: 18-APR-97  C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
   SUBROUTINE WRITE_HEADER

      IMPLICIT NONE

!     Memory required for the run
      DOUBLE PRECISION :: MEMORY

      CALL START_LOG

1005  FORMAT(1X,'MFiX (',A10,') simulation on host: ',A20)
1010  FORMAT(1X,'Run name: ',A20,2X,'Time: ',I2,':',I2.0,20X,'Date: ',I2,'-',I2&
         ,'-',I4)
1011  FORMAT(1X,'Project version: ',A80)
      WRITE(ERR_MSG, *) ' '
      CALL LOG_STATUS()
      WRITE(ERR_MSG, 1005) ID_VERSION, ID_NODE
      CALL LOG_STATUS()
      WRITE(ERR_MSG,1010)RUN_NAME,ID_HOUR,ID_MINUTE,ID_MONTH,ID_DAY,ID_YEAR
      CALL LOG_STATUS()
      WRITE(ERR_MSG,1011) PROJECT_VERSION
      CALL LOG_STATUS()

      MEMORY = 9. + (8.*DIMENSION_3/ONEMEG)*(95. + 32.*DIMENSION_M + 4.*&
         DIMENSION_N_G + 4.*DIMENSION_M*DIMENSION_N_S)
1015  FORMAT(72('_'))
      WRITE(ERR_MSG, '(1X,A,F7.2,A)') 'Memory required: ', MEMORY, ' Mb'
      CALL LOG_STATUS()
      WRITE(ERR_MSG, 1015)
      CALL LOG_STATUS()

      CALL END_LOG

      RETURN

   END SUBROUTINE WRITE_HEADER

END MODULE WRITE_HEADER_MOD
