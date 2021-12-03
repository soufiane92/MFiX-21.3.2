!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR0                                                   C
!  Purpose: This routine is called before the time loop starts and is  C
!           user-definable.  The user may insert code in this routine  C
!           or call appropriate user defined subroutines.  This        C
!           can be used for setting constants and checking errors in   C
!           data.  This routine is not called from an IJK loop, hence  C
!           all indices are undefined.                                 C
!                                                                      C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced:                                               C
!  Variables modified:                                                 C
!                                                                      C
!  Local variables:                                                    C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
      SUBROUTINE USR0

      USE USR
      USE discretelement, only:DES_POS_NEW,DES_USR_VAR
      USE GET_STL_DATA_MOD

      IMPLICIT NONE

!-----------------------------------------------
!
!  Include files defining common blocks here
!
!
!  Define local variables here
      double precision, allocatable, dimension(:) :: axisnorm
      integer :: allocstatus
      integer :: i, NN,VV

! Initial particles were read from particle_input.dat.
! Use DES_USR_VAR to color particle based on their original location
! to visualize mixing

      DES_USR_VAR(1,:) = 0.0D0
      WHERE(DES_POS_NEW(:,3)>0.0D0) DES_USR_VAR(1,:) = 1.0D0

! Read and store keyframe data information
      call read_keyframe_data


! Define axis of rotation
      cyl_center(:) = (/0.0, 0.0, 0.0/)  


      CALL GET_STL_DATA_IS

      CALL COPY_INIT_STL_DATA


! Get group ID of the stl files and asign a BC_ID to them
! The BC_ID must be diffferent from existing BCs
      is_1_group_id = GET_GROUP_ID('is_0001.stl')
      is_2_group_id = GET_GROUP_ID('is_0002.stl')

! Not needed       
      CALL ASSIGN_BC_ID_TO_STL_GROUP(is_1_group_id, 11)
      CALL ASSIGN_BC_ID_TO_STL_GROUP(is_2_group_id, 12)


      RETURN
      END SUBROUTINE USR0
