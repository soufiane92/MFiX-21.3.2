#include "error.inc"

MODULE EVAL_F_MOD

   use define_quadrics_mod, only: get_f_quadric, define_quadrics, reasssign_quadric
   use error_manager
   use get_poly_data_mod, only: eval_poly_fct

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: EVAL_F                                                 C
!  Purpose: Evaluate the function f(x,y,z) defining the boundary       C
!                                                                      C
!  Author: Jeff Dietiker                               Date: 21-FEB-08 C
!  Reviewer:                                           Date:           C
!                                                                      C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced:                                               C
!  Variables modified:                                                 C
!                                                                      C
!  Local variables:                                                    C
!                                                                      C
!  Modified: ##                                        Date: ##-###-## C
!  Purpose: ##                                                         C
!                                                                      C
!  Literature/Document References: ##                                  C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
   SUBROUTINE EVAL_F(METHOD,x1,x2,x3,Q,f,CLIP_FLAG)

      USE compar
      USE cutcell
      USE parallel
      USE param1, only: undefined
      USE quadric
      USE quadric
      USE sendrecv

      IMPLICIT NONE

      DOUBLE PRECISION x1,x2,x3
      DOUBLE PRECISION f
      INTEGER :: Q,Q_ID,BCID
      LOGICAL :: CLIP_FLAG
      CHARACTER (LEN = 7) :: METHOD
      CHARACTER(LEN=9) :: GR

      INTEGER :: GROUP,GS,P

      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: F_G

      ALLOCATE(F_G(N_GROUP,0:DIM_QUADRIC))

      SELECT CASE(METHOD)

         CASE('QUADRIC')

            F_G = - UNDEFINED

            DO GROUP = 1, N_GROUP
               GS = GROUP_SIZE(GROUP)
               GR = TRIM(GROUP_RELATION(GROUP))

               DO P = 1 , GS
                  Q_ID = GROUP_Q(GROUP,P)
                  CALL GET_F_QUADRIC(x1,x2,x3,Q_ID,F_G(GROUP,P),CLIP_FLAG)
               ENDDO
               IF(GR == 'AND') THEN
                  F_G(GROUP,0) = MAXVAL(F_G(GROUP,1:GS))
               ELSEIF(GR == 'OR') THEN
                  F_G(GROUP,0) = MINVAL(F_G(GROUP,1:GS))
               ELSEIF(GR == 'PIECEWISE') THEN
                  CALL REASSSIGN_QUADRIC(x1,x2,x3,GROUP,Q_ID)
!                  CLIP_FLAG=.FALSE.
                  CALL GET_F_QUADRIC(x1,x2,x3,Q_ID,F_G(GROUP,0),CLIP_FLAG)
!                  CLIP_FLAG=.TRUE.
               ENDIF

            ENDDO

            f = F_G(1,0)

            DO GROUP = 2, N_GROUP

               GR = TRIM(RELATION_WITH_PREVIOUS(GROUP))

               IF(GR =='AND') THEN
                  f = DMAX1(f,F_G(GROUP,0))
               ELSEIF(GR =='OR') THEN
                  f = DMIN1(f,F_G(GROUP,0))
               ENDIF

            ENDDO


         CASE('POLYGON')

            CALL EVAL_POLY_FCT(x1,x2,x3,Q,f,CLIP_FLAG,BCID)

         CASE('USR_DEF')

            CALL EVAL_USR_FCT(x1,x2,x3,Q,f,CLIP_FLAG)

!         CASE('STL')

!            CALL EVAL_STL_FCT(x1,x2,x3,Q,f,CLIP_FLAG,BCID)

         CASE DEFAULT

            WRITE(ERR_MSG,*)'ERROR IN SUBROUTINE EVAL_F.'//&
               'UNKNOWN METHOD:',METHOD//&
               'ACCEPTABLE METHODS:'//&
               'QUADRIC'//&
               'POLYGON'//&
               'USR_DEF'
!            WRITE(*,*)'STL'
            call log_error()

      END SELECT

      DEALLOCATE(F_G)

      RETURN

   END SUBROUTINE EVAL_F

END MODULE EVAL_F_MOD
