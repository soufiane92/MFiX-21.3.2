#include "error.inc"

module check_bc_pic_mod

   use error_manager

contains

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
! minimum amount of geometry data.                                     !
!                                                                      !
! Subroutine: CHECK_BC_PIC                                             !
! Author: R. Garg                                     Date: 11-Jun-14  !
!                                                                      !
! Purpose: Determine if BCs are "DEFINED" and that they contain the    !
! minimum amount of geometry data.                                     !
!                                                                      !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
      SUBROUTINE CHECK_BC_PIC(M_TOT)

! Global Variables:
!---------------------------------------------------------------------//

! Simulation dimension (2D/3D)
      USE discretelement, only: DIMN
! User specified: BC geometry
      use bc, only: MASS_INFLOW, MASS_OUTFLOW, P_INFLOW, P_OUTFLOW, CG_MI, CG_PO, OUTFLOW, BC_TYPE_ENUM, BC_EP_S
! PIC model specific BC region specification.
      USE bc, only: BC_PIC_MI_CONST_STATWT
      USE bc, only: BC_X_w, BC_X_e, BC_Y_s, BC_Y_n, BC_Z_b, BC_Z_t

! Solids phase identifier
      use run, only: SOLIDS_MODEL
! Number of PIC inlet/outlet BCs detected.
      use mfix_pic, only: PIC_BCMI
      use mfix_pic, only: PIC_BCMI_MAP

! Global Parameters:
!---------------------------------------------------------------------//
! The max number of BCs.
      use param, only: DIMENSION_BC
! Parameter constants
      use param1, only: ZERO, UNDEFINED


! Use the error manager for posting error messages.
!---------------------------------------------------------------------//
      use error_manager

      IMPLICIT NONE

! Passed Arguments:
!---------------------------------------------------------------------//
! Total number of solids phases.
      INTEGER, INTENT(in) :: M_TOT

! Local Variables:
!---------------------------------------------------------------------//
! loop/variable indices
      INTEGER :: BCV, M, BCV_I, IDIM
      INTEGER :: BCV2, BCV2_I

! Temp logical variables for checking constant statwt specification
      LOGICAL :: CONST_STATWT

      DOUBLE PRECISION :: BC_ORIG(3), BC_END(3), BC2_ORIG(3) , BC2_END(3)
      DOUBLE PRECISION :: BC_MIN, BC_MAX, BC2_MIN, BC2_MAX

      LOGICAL :: SEP_AXIS

! Initialize
      PIC_BCMI = 0

! Loop over all BCs looking for PIC solids inlets/outlets
      DO BCV = 1, DIMENSION_BC

         SELECT CASE (BC_TYPE_ENUM(BCV))

! Determine the number of mass inlets that contain PIC solids.
         CASE (MASS_INFLOW)
            M_LP: DO M=1,M_TOT
               IF(SOLIDS_MODEL(M)=='PIC' .AND.                         &
                  BC_EP_s(BCV,M) > ZERO) THEN
                  PIC_BCMI = PIC_BCMI + 1
                  PIC_BCMI_MAP(PIC_BCMI) = BCV
                  EXIT M_LP
               ENDIF
            ENDDO M_LP

! Count the number of pressure outflows.
         CASE (P_OUTFLOW)

! Flag CG_MI as an error if PIC solids are present.
         CASE (CG_MI)
            DO M=1,M_TOT
               IF(SOLIDS_MODEL(M)=='PIC') THEN
                  IF(BC_EP_s(BCV,M) /= UNDEFINED .AND.                 &
                     BC_EP_s(BCV,M) > ZERO) THEN
                     WRITE(ERR_MSG,1000) trim(iVar('BC_TYPE',BCV)),    &
                        'CG_MI'
                     CALL LOG_ERROR()
                  ENDIF
               ENDIF
            ENDDO

         CASE (CG_PO)
            WRITE(ERR_MSG,1000) trim(iVar('BC_TYPE',BCV)), 'CG_PO'
            CALL LOG_ERROR()

         CASE (MASS_OUTFLOW, OUTFLOW, P_INFLOW)
            WRITE(ERR_MSG,1000) trim(iVar('BC_TYPE',BCV)),             &
               BC_TYPE_ENUM(BCV)
            CALL LOG_ERROR()

         END SELECT

      ENDDO

1000  FORMAT('Error 1000: Unsupported boundary condition specified ',  &
           'with',/'PIC simulation: ',A,' = ',A,/'Please correct the ',&
           'project settings.')

! Loop over all MI BC's for data consistency checks
      DO BCV_I = 1, PIC_BCMI

! Get the user defined BC ID.
         BCV = PIC_BCMI_MAP(BCV_I)

         DO M=1,M_TOT
            IF(SOLIDS_MODEL(M)=='PIC' .AND.                         &
                 BC_EP_s(BCV,M) > ZERO) THEN
               CONST_STATWT = (BC_PIC_MI_CONST_STATWT(BCV, M) .ne. ZERO  )

               IF(.not.CONST_STATWT) then
                  WRITE(ERR_MSG, 1101) BCV, M
                  CALL LOG_ERROR()
               ENDIF


            ENDIF
         ENDDO


1101     FORMAT('Error 1101: In PIC model for BC # ',i5, &
              ' and solid phase # ', i5, /, &
              'A non-zero value not specified for ', &
              'BC_PIC_MI_CONST_STATWT. ', /, &
              'Choose between constant number of parcels per cell or ', &
              'constant statistical weight', /, &
              'See MFIX readme',/'Please correct the data file.')


         BC_ORIG(1) = BC_X_W(BCV)
         BC_ORIG(2) = BC_Y_S(BCV)
         BC_ORIG(3) = BC_Z_B(BCV)
         BC_END(1)  = BC_X_E(BCV)
         BC_END(2)  = BC_Y_N(BCV)
         BC_END(3)  = BC_Z_T(BCV)
         BCVTWOLOOP: DO BCV2_I = BCV_I+1, PIC_BCMI

            ! Get the user defined BC ID.
            BCV2 = PIC_BCMI_MAP(BCV2_I)


            BC2_ORIG(1) = BC_X_W(BCV2)
            BC2_ORIG(2) = BC_Y_S(BCV2)
            BC2_ORIG(3) = BC_Z_B(BCV2)
            BC2_END(1)  = BC_X_E(BCV2)
            BC2_END(2)  = BC_Y_N(BCV2)
            BC2_END(3)  = BC_Z_T(BCV2)

            sep_axis  = .false.
            DO idim = 1, dimn

               bc_min = BC_ORIG(idim)
               bc_max = BC_END(idim)
               bc2_min = BC2_ORIG(idim)
               bc2_max = bc2_END(idim)


               if(bc_min.eq.bc_max.and.bc_min.eq.bc2_min.and.bc_min.eq.bc2_max) cycle
               !if above is true, then the sep_axis will be true (see below) and
               !overlapping bc regions will also be deemed as non-overlapping

               !Check for separating axis. If the separating axis exists, then
               !the BC regions can't overlap.
               !generally equality implies lack of sep_axis, and thus, overlapping
               !However, doing so will flag all BC's as overlapping since
               !BC's have to share common edges. So here the equality is considered
               !as existence of a separating axis, and hence, no overlap
               !equality is also considered as separating axis which is
               if ((bc_min .ge. bc2_max)  .or. (bc_max .le. bc2_min) ) then
                  sep_axis = .true.
                  exit
               endif

            end DO

            if(.not.sep_axis) then
               !implies the BC regions could not find a separating axis and are therefore
               !overlapping

               write(err_msg, 1004) BCV, BCV2
               CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, FOOTER=.FALSE.)

               DO IDIM = 1, DIMN

                  write(err_msg, 1005) 'BC1', IDIM, BC_ORIG(IDIM), BC_END(IDIM)
                  CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

                  write(err_msg, 1005) 'BC2', IDIM, BC2_ORIG(IDIM), BC2_END(IDIM)
                  CALL LOG_MESSAGE(__FILE__, __LINE__, LOGLEVEL_INFO, HEADER=.FALSE., FOOTER=.FALSE.)

               ENDDO
               write(err_msg, 1006)

               CALL LOG_ERROR()

            endif
         end DO BCVTWOLOOP

      ENDDO

1004  FORMAT('Error # 1004 for PIC Solids MI BC:',/5x, &
           'Overlapping MI BC regions with non zero', /, &
           'solids volume fraction  not allowed.', /, &
           'Overlapping BCs are', 2(2x, i4))

1005  FORMAT('Spans of ', A, ' in dir ', I2, /5x, 2(2x, g17.8))

1006  Format('Please correct the data file. Exiting.')

      END SUBROUTINE CHECK_BC_PIC

end module check_bc_pic_mod
