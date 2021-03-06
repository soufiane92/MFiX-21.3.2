MODULE CALC_DRAG_DES_MOD
CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CALC_DRAG_DES                                           !
!                                                                      !
!  Purpose: This subroutine is called from DES routines. It calls      !
!  functions that calculate the drag force acting on particles. No     !
!  field variables are updated.                                        !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE CALC_DRAG_DES

      use discretelement, only: DES_CONTINUUM_COUPLED
      use discretelement, only: DES_CONTINUUM_HYBRID
      use discretelement, only: DES_EXPLICITLY_COUPLED
      use discretelement, only: DRAG_FC, FC, MAX_PIP
      use drag_gs_des0_mod, only: drag_gs_des0
      use drag_gs_des1_mod, only: drag_gs_des1
      use drag_ss_dem_noninterp_mod, only: drag_ss_dem_noninterp, drag_ss_tfm_noninterp
      use functions, only: IS_NORMAL
      use particle_filter, only: DES_INTERP_GARG
      use particle_filter, only: DES_INTERP_SCHEME_ENUM

      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------//
      INTEGER :: II
!......................................................................!

! Apply the drag force calculated by the gas phase.
      IF(DES_EXPLICITLY_COUPLED) THEN

         IF(DES_CONTINUUM_COUPLED) THEN
!$omp parallel do default(none) private(II) &
!$omp shared(FC, DRAG_FC, MAX_PIP)
            DO II = 1, MAX_PIP
               IF(IS_NORMAL(II)) &
                  FC(II,:) = FC(II,:) + DRAG_FC(II,:)
            ENDDO
!$omp end parallel do
         ENDIF


      ELSE

! Calculate gas-solids drag force on particle
         IF(DES_CONTINUUM_COUPLED) THEN
            SELECT CASE(DES_INTERP_SCHEME_ENUM)
            CASE(DES_INTERP_GARG) ; CALL DRAG_GS_DES0
            CASE DEFAULT; CALL DRAG_GS_DES1
            END SELECT
         ENDIF

! Calculate solids-solids drag force on particle.
         IF(DES_CONTINUUM_HYBRID) THEN
            SELECT CASE(DES_INTERP_SCHEME_ENUM)
            CASE DEFAULT; CALL DRAG_SS_DEM_NONINTERP
            END SELECT
         ENDIF
      ENDIF

      RETURN

   END SUBROUTINE CALC_DRAG_DES

END MODULE CALC_DRAG_DES_MOD
