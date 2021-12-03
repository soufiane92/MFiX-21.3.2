#include "error.inc"

MODULE CHECK_GEOMETRY_MOD

   use error_manager
   use shift_dxyz_mod, only: shift_dxyz

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: CHECK_GEOMETRY                                          !
!  Purpose: Check the distributed parallel namelist variables.         !
!                                                                      !
!  Author: P. Nicoletti                               Date: 14-DEC-99  !
!  Reviewer: J.Musser                                 Date: 16-Jan-14  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
   SUBROUTINE CHECK_GEOMETRY(SHIFT)

      use check_axis_mod, only: check_axis, check_mfix_domain
      use check_data_cg, only: get_dxyz_from_control_points

! Global Variables:
!---------------------------------------------------------------------//
! Domain partitions in various directions.
      use geometry, only: DX, XLENGTH
      use geometry, only: DY, YLENGTH
      use geometry, only: DZ, ZLENGTH
      use geometry, only: X_MIN, X_MAX, Y_MIN, Y_MAX, Z_MIN, Z_MAX

      use geometry, only: NO_I, IMIN1, IMAX, IMAX1, IMAX3
      use geometry, only: NO_J, JMIN1, JMAX, JMAX1, JMAX3
      use geometry, only: NO_K, KMIN1, KMAX, KMAX1, KMAX3

      use geometry, only: CYLINDRICAL
      use geometry, only: CYCLIC_X, CYCLIC_X_PD
      use geometry, only: CYCLIC_Y, CYCLIC_Y_PD
      use geometry, only: CYCLIC_Z, CYCLIC_Z_PD

      implicit none

      LOGICAL, intent(IN) :: SHIFT
      LOGICAL, external :: COMPARE

      CALL CHECK_MFIX_DOMAIN(X_MIN, X_MAX, XLENGTH, 'X')
      CALL CHECK_MFIX_DOMAIN(Y_MIN, Y_MAX, YLENGTH, 'Y')
      CALL CHECK_MFIX_DOMAIN(Z_MIN, Z_MAX, ZLENGTH, 'Z')

      CALL GET_DXYZ_FROM_CONTROL_POINTS

      CALL CHECK_AXIS(IMAX, IMAX3, XLENGTH, DX, 'X', 'I', NO_I, SHIFT)
      CALL CHECK_AXIS(JMAX, JMAX3, YLENGTH, DY, 'Y', 'J', NO_J, SHIFT)
      CALL CHECK_AXIS(KMAX, KMAX3, ZLENGTH, DZ, 'Z', 'K', NO_K, SHIFT)

      IF(SHIFT) CALL SHIFT_DXYZ

!  Ensure that the cell sizes across cyclic boundaries are comparable
      IF(CYCLIC_X .OR. CYCLIC_X_PD) THEN
         IF(DX(IMIN1) /= DX(IMAX1)) THEN
            WRITE(ERR_MSG,1100) 'DX(IMIN1)',DX(IMIN1),'DX(IMAX1)',DX(IMAX1)
            CALL LOG_ERROR()
         ENDIF
      ENDIF

      IF(CYCLIC_Y .OR. CYCLIC_Y_PD) THEN
         IF(DY(JMIN1) /= DY(JMAX1)) THEN
            WRITE(ERR_MSG,1100) 'DY(JMIN1)',DY(JMIN1),'DY(JMAX1)',DY(JMAX1)
            CALL LOG_ERROR()
         ENDIF
      ENDIF

      IF(CYCLIC_Z .OR. CYCLIC_Z_PD .OR. CYLINDRICAL) THEN
         IF (DZ(KMIN1) /= DZ(KMAX1)) THEN
            WRITE(ERR_MSG,1100) 'DZ(KMIN1)',DZ(KMIN1),'DZ(KMAX1)',DZ(KMAX1)
            CALL LOG_ERROR()
         ENDIF
      ENDIF

 1100 FORMAT('Error 1100: Cells adjacent to cyclic boundaries must ',  &
         'be of same size:',/2X,A,' = ',G12.5,/2x,A,' = ',G12.5,/      &
         'Please correct the project settings.')

      RETURN

      END SUBROUTINE CHECK_GEOMETRY


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: CHECK_GEOMETRY_DEM                                      !
!  Author: Pradeep Gopalakrishnan                     Date:    Nov-11  !
!                                                                      !
!  Purpose: Checks the des grid input parameters.                      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_GEOMETRY_DEM

! Global Variables:
!---------------------------------------------------------------------//
! Domain partition for DEM background mesh.
      use discretelement, only: DESGRIDSEARCH_IMAX
      use discretelement, only: DESGRIDSEARCH_JMAX
      use discretelement, only: DESGRIDSEARCH_KMAX
! Domain size specified by the user.
      use geometry, only: XLENGTH, YLENGTH, ZLENGTH, NO_K
      use geometry, only: DX, DY, DZ
      use geometry, only: CYCLIC_X, CYCLIC_Y, CYCLIC_Z
! Maximum particle size.
      use discretelement, only: MAX_RADIUS


! Global Parameters:
!---------------------------------------------------------------------//
      use param1, only: UNDEFINED_I

! Use the error manager for posting error messages.
!---------------------------------------------------------------------//
      use error_manager


      implicit none


! Local Variables:
!---------------------------------------------------------------------//
! Maximum particle diameter.
      DOUBLE PRECISION :: MAX_DIAM
! Calculated cell dimension based on particle size
      DOUBLE PRECISION :: WIDTH,WIDTH_X,WIDTH_Y,WIDTH_Z
      DOUBLE PRECISION :: DX_MIN,DY_MIN,DZ_MIN
!......................................................................!

! Calculate the max particle diameter and cell width.
      MAX_DIAM = 2.0d0*MAX_RADIUS
      WIDTH = 3.0d0*(max_diam)

      DX_MIN = minval(dx)
      DY_MIN = minval(dy)
      DZ_MIN = minval(dz)

! The DES grid spacing must be less than or equal to the smallest fluid grid spacing
      WIDTH_X = min(WIDTH,DX_MIN)
      WIDTH_Y = min(WIDTH,DY_MIN)
      WIDTH_Z = min(WIDTH,DZ_MIN)

! Calculate and/or verify the grid in the X-axial direction.
      IF(DESGRIDSEARCH_IMAX == UNDEFINED_I) &
         DESGRIDSEARCH_IMAX = max(int(XLENGTH/WIDTH_X), 1)

      IF((XLENGTH/dble(DESGRIDSEARCH_IMAX)) < MAX_DIAM) THEN
         WRITE(ERR_MSG, 1100) 'X', MAX_DIAM,                           &
            XLENGTH/dble(DESGRIDSEARCH_IMAX)
         CALL LOG_ERROR()
      ELSEIF((XLENGTH/dble(DESGRIDSEARCH_IMAX)) > DX_MIN.AND.CYCLIC_X) THEN
         WRITE(ERR_MSG, 1200) 'X', DX_MIN,                           &
            XLENGTH/dble(DESGRIDSEARCH_IMAX)
         CALL LOG_ERROR()
      ENDIF

! Calculate and/or verify the grid in the Y-axial direction.
      IF(DESGRIDSEARCH_JMAX == UNDEFINED_I) &
         DESGRIDSEARCH_JMAX = max(int(YLENGTH/WIDTH_Y), 1)

      IF((YLENGTH/dble(DESGRIDSEARCH_JMAX)) < MAX_DIAM) THEN
         WRITE(ERR_MSG, 1100) 'Y', MAX_DIAM,                           &
            YLENGTH/dble(DESGRIDSEARCH_JMAX)
         CALL LOG_ERROR()
      ELSEIF((YLENGTH/dble(DESGRIDSEARCH_JMAX)) > DY_MIN.AND.CYCLIC_Y) THEN
         WRITE(ERR_MSG, 1200) 'Y', DY_MIN,                           &
            YLENGTH/dble(DESGRIDSEARCH_JMAX)
         CALL LOG_ERROR()
      ENDIF

! Calculate and/or verify the grid in the Z-axial direction.
      IF(NO_K) DESGRIDSEARCH_KMAX = 1

      IF(DESGRIDSEARCH_KMAX == UNDEFINED_I) &
         DESGRIDSEARCH_KMAX = max(int(ZLENGTH/WIDTH_Z), 1)

      IF((ZLENGTH/dble(DESGRIDSEARCH_KMAX)) < MAX_DIAM) THEN
         WRITE(ERR_MSG, 1100) 'Z', MAX_DIAM,                           &
            ZLENGTH/dble(DESGRIDSEARCH_KMAX)
         CALL LOG_ERROR()
      ELSEIF((ZLENGTH/dble(DESGRIDSEARCH_KMAX)) > DZ_MIN.AND.CYCLIC_Z) THEN
         WRITE(ERR_MSG, 1200) 'Z', DZ_MIN,                           &
            ZLENGTH/dble(DESGRIDSEARCH_KMAX)
         CALL LOG_ERROR()
      ENDIF

      WRITE(ERR_MSG, 2000) DESGRIDSEARCH_IMAX,DESGRIDSEARCH_JMAX,DESGRIDSEARCH_KMAX
      CALL LOG_INFO()

 1100 FORMAT('Error 1100: The des search grid is too fine in the ',A1, &
         '-direction. The',/'maximum particle diameter is larger than',&
         ' the cell width:',/2x,'MAX DIAM:   ',g12.5,/2x,'CELL ',      &
         'WIDTH: ',g12.5,/'Decrease the values for DESGRIDSEARCH in ', &
         'the project settings.')

 1200 FORMAT('Error 1100: The des search grid is too coarse in the ',A1,   &
         '-direction. The',/'des cell width is larger than the smallest',  &
         ' fluid cell width:',/2x,'MIN FLUID CELL WIDTH: ',g12.5,/2x,      &
         'DES CELL WIDTH: ',g12.5,/'Increase the values for',              &
         ' DESGRIDSEARCH in the mfix.dat file.')

 2000 FORMAT('Info: DES grid size:',  /&
         'DESGRIDSEARCH_IMAX = ', I8, /&
         'DESGRIDSEARCH_JMAX = ', I8, /&
         'DESGRIDSEARCH_KMAX = ', I8)

      RETURN
      END SUBROUTINE CHECK_GEOMETRY_DEM

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: CHECK_GEOMETRY_PIC                                      !
!                                                                      !
!  Purpose: Sets des grid input parameters for PIC.                    !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_GEOMETRY_PIC

! Global Variables:
!---------------------------------------------------------------------//
! Domain partition for DEM background mesh.
      use discretelement, only: DESGRIDSEARCH_IMAX
      use discretelement, only: DESGRIDSEARCH_JMAX
      use discretelement, only: DESGRIDSEARCH_KMAX
! Mesh size specified by the user.
      use geometry, only: IMAX, JMAX, KMAX

      use mfix_pic, only: DXYZ_MIN

      use geometry, only: DX, IMIN1, IMAX1
      use geometry, only: DY, JMIN1, JMAX1
      use geometry, only: DZ, KMIN1, KMAX1, DO_K

! Global Parameters:
!---------------------------------------------------------------------//

! Use the error manager for posting error messages.
!---------------------------------------------------------------------//
      use error_manager

      implicit none

!......................................................................!

      ! Get minimum fluid cell dimension
      DXYZ_MIN = min(minval(DX(IMIN1:IMAX1)), minval(DY(JMIN1:JMAX1)))
      IF (DO_K) DXYZ_MIN = min(DXYZ_MIN, minval(DZ(KMIN1:KMAX1)))

      DESGRIDSEARCH_IMAX = IMAX
      DESGRIDSEARCH_JMAX = JMAX
      DESGRIDSEARCH_KMAX = KMAX

      WRITE(ERR_MSG, 2000) DESGRIDSEARCH_IMAX,DESGRIDSEARCH_JMAX,DESGRIDSEARCH_KMAX
      CALL LOG_INFO()

 2000 FORMAT('Info: DES grid size:',  /&
         'DESGRIDSEARCH_IMAX = ', I8, /&
         'DESGRIDSEARCH_JMAX = ', I8, /&
         'DESGRIDSEARCH_KMAX = ', I8)

      RETURN

   END SUBROUTINE CHECK_GEOMETRY_PIC

END MODULE CHECK_GEOMETRY_MOD
