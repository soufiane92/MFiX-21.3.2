#include "error.inc"

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_USR1 (L)                                         C
!  Purpose: Write user-defined output                                  C
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

      SUBROUTINE WRITE_USR1(L)

! Global variables
!--------------------------------------------------------------------
      use output, only: usr_dt, usr_x_w
      use param1, only: undefined
      use usr
      IMPLICIT NONE

! Dummy arguments
!--------------------------------------------------------------------
! index of user region (boundaries/file)
      INTEGER, INTENT(IN) :: L
!--------------------------------------------------------------------

      IF(USR_DT(L) /= UNDEFINED) THEN
! extract/write for any/all regions defined
!         IF (USR_X_W(L) == UNDEFINED) CALL write_customdata2('CustOutput2',L)

      ENDIF

      SELECT CASE(L)
! Compute/print data corresponding to region 1 or usr_dt(1)
! -------------------------------------------------------------------
      CASE(1)
! this is reserved for EQ chem timestep. NOT used for output


! Compute/print data corresponding to region 2 or usr_dt(2)
! -------------------------------------------------------------------
      CASE(2)
! write data here that is not associated with a user defined region
! but does require some usr_dt definition for frequency of output

! or write your custom data here when it is associated with a region
! (assuming header written earlier)
!         IF (USR_X_W(L) /= UNDEFINED) THEN
!            call write_customdata3('CustOutput3',L)
!            call write_customdata1('VolAvg',L)
!         ENDIF
      CASE(3)
         IF (USR_X_W(L) /= UNDEFINED) THEN
         ENDIF
      CASE(4)
         IF (USR_X_W(L) /= UNDEFINED) THEN
         ENDIF

      END SELECT
      RETURN
      END SUBROUTINE WRITE_USR1


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
! Purpose: Calculate a volume average value of the passed variable     C
! at the north face of the indicated region                            C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE GetAvgVar_N(Var, AvgVar, lVEL, AvgVelxVar, L )!, VELl)

      use compar, only: dead_cell_at
      use error_manager
      use fun_avg, only: avg_y
      use functions, only: funijk, north_of, fluid_at
      use functions, only: is_on_mype_owns
      use geometry, only: vol_v
      use mpi_utility, only: global_all_sum
      use output, only: usr_i_w, usr_i_e
      use output, only: usr_j_s, usr_j_n
      use output, only: usr_k_b, usr_k_t
      use param, only: dimension_3
      use param1, only: ZERO

      IMPLICIT NONE

! Dummy arguments
!--------------------------------------------------------------------
! cell-centered interphase momentum coefficient var to be averaged
      DOUBLE PRECISION, INTENT(IN) :: Var(DIMENSION_3)
! volume average of var at north face (i, j+1/2, k)
      DOUBLE PRECISION, INTENT(OUT) ::AvgVar
! velocity at north face (i, j+1/2, k)
      DOUBLE PRECISION, INTENT(IN) :: lVel(DIMENSION_3)
! volume average of var x velocity at north face (i, j+1/2, k)
      DOUBLE PRECISION, INTENT(OUT) :: AvgVelxVar
! index of user region (bounds) to use for extraction
      INTEGER, INTENT(IN) :: L
! velocity of second phase
!      DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: VEll(Dimension_3)

! Local variables
!--------------------------------------------------------------------
! value of variable at north face (i,j+1/2,k)
      DOUBLE PRECISION :: Var_N
! indices
      INTEGER :: IJK, IJKN, I, J, K
! sum of cell volumes (total volume)
      DOUBLE PRECISION :: sum_vol
! sum of the quantity cell volume times var
      DOUBLE PRECISION :: sum_varxvol
! sum of the quantity cell volume times var times vel
      DOUBLE PRECISION :: sum_velxvarxvol
!--------------------------------------------------------------------

!      if (.not.present(vell)) THEN
!         lvel = vel
!      else
!         lvel = vel - vell
!      endif

! initialize
      sum_vol = zero
      sum_varxvol = zero
      sum_velxvarxvol = zero
      avgvar = zero
      avgvelxvar = zero

! calculating sum
      DO K = usr_k_b(L), usr_k_t(L)
         DO J = usr_j_s(L), usr_j_n(L)
            DO I = usr_i_w(L), usr_i_e(L)
               IF(IS_ON_myPE_owns(I,J,K)) THEN
                  IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
                  IJK = FUNIJK(I,J,K)
                  IF (FLUID_AT(IJK)) THEN
!                     IF(.NOT.IP_AT_N(IJK))
                     IJKN = NORTH_OF(IJK)
! evaluate cell-centered quantity at north face (i, j+1/2, k)
                     Var_N = AVG_Y(Var(IJK),VAR(IJKN),J)
                     SUM_vol = sum_vol + VOL_V(IJK)
                     SUM_varxvol = sum_varxvol+&
                        Var_N*VOL_V(IJK)
                     SUM_velxvarxvol = sum_velxvarxvol+&
                        lvel(ijk)*Var_N*VOL_V(IJK)
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      CALL global_all_sum(sum_vol)
      CALL global_all_sum(sum_varxvol)
      CALL global_all_sum(sum_velxvarxvol)

      IF (SUM_vol > ZERO) THEN
         AVGvar = SUM_varXVOL/SUM_vol
         AVGvelxvar = SUM_velxvarXVOL/SUM_vol
      ELSE
         write(err_msg, 1101)  L
         CALL LOG_ERROR()
 1101 FORMAT('Error 1101: Sum of volumes is less than zero in ', &
         'user region L = ',i5)
      ENDIF

      RETURN
      END SUBROUTINE GetAvgVar_N


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!  Subroutine: write_customdata_holder                                 C
!  Purpose: send data on various to relevant data file                 C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE write_customdata(FileN,L)

! Modules
!--------------------------------------------------------------------
      use compar, only: myPE, PE_IO
      use usr, only: index_liq
      use run, only: time
      use param1, only: undefined_c
      use usr, only: index_liq
      IMPLICIT NONE

! Dummy arguments
!--------------------------------------------------------------------
! file name
      CHARACTER (len=*), INTENT(IN) :: FileN
! index of user defined boundary
      INTEGER, INTENT(IN) :: L

! Local variables
! -------------------------------------------------------------------
! file tag name
      CHARACTER(len=3) :: FTAG
      LOGICAL :: lExists
      INTEGER, PARAMETER :: lUnit=484
! local file name
      CHARACTER(LEN=30) :: lFileN
! -------------------------------------------------------------------

! perform some calculations/extract some averages

      IF(myPE /= PE_IO) RETURN

      write(fTag,"('_',I2.2)") L
      write(lFileN,"(A,'.log')") FileN//fTag
      inquire(file=trim(lFileN),exist=lExists)
      IF(lExists) then
         open(lUnit,file=trim(adjustl(lFileN)), &
              status='old', position='append')
      ELSE
         open(lUnit,file=trim(adjustl(lFileN)), &
              status='new')
      ENDIF


! write your data
!      write(lUnit,'(2X,F7.3,X,3(ES12.5,2X))') time, &
!         gradp_va, f_factor, liquidogas
      close(lUnit)


      RETURN
      END SUBROUTINE write_customdata


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
! Purpose: Data collection routine that computes the volume average of C
! a variable over user defined region in the simulation domain.        C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE write_customdata1(FileN,L)

! Modules
!--------------------------------------------------------------------
      use compar, only: ijkstart3, ijkend3

      use compar, only: dead_cell_at
      use compar, only: myPE, PE_IO
      use functions, only: is_on_mype_owns
      use functions, only: funijk, fluid_at
      use geometry, only: vol
      use mpi_utility, only: global_all_sum
      use output, only: usr_i_w, usr_i_e
      use output, only: usr_j_s, usr_j_n, usr_y_s, usr_y_n
      use output, only: usr_k_b, usr_k_t
      use param, only: dimension_3
      use param1, only: ZERO
      use fldvar, only: v_g, v_s, p_g, ep_g, ro_g, ro_s, ep_s
      use usr, only: index_liq, index_sol
      use run, only: time, UNITS

      IMPLICIT NONE

!  Dummy arguments
!--------------------------------------------------------------------
! file name
      CHARACTER (len=*), INTENT(IN) :: FileN
! index of user defined boundary to use for extraction
      INTEGER, INTENT(IN) :: L

! Local variables
!--------------------------------------------------------------------
      DOUBLE PRECISION :: f_factor, gradp_va
      DOUBLE PRECISION :: liquidogas, lrate, grate
      DOUBLE PRECISION :: sum_vol, sum_varxvol, var
      DOUBLE PRECISION :: s_press, n_press, s_pos, n_pos
! volume average variable at north face
      DOUBLE PRECISION :: phi_va, uphi_va
      DOUBLE PRECISION :: ep_liquid(DIMENSION_3)
      INTEGER:: IJK, J, I, K
! file tag name
      CHARACTER(len=3) :: FTAG
      LOGICAL :: lExists
      INTEGER, PARAMETER :: lUnit=485
! local file name
      CHARACTER(LEN=30) :: lFileN,FormatString
!--------------------------------------------------------------------
! calculating avg pressure at south plane
! initialize
      sum_vol = zero
      sum_varxvol=zero
      J = usr_j_s(L)
      DO K = usr_k_b(L), usr_k_t(L)
         DO I = usr_i_w(L), usr_i_e(L)
            IF(IS_ON_myPE_owns(I,J,K)) THEN
               IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
               IJK = FUNIJK(I,J,K)
               IF (FLUID_AT(IJK)) THEN
                  var = p_g(ijk)
                  SUM_vol = sum_vol + VOL(IJK)
                  SUM_varxvol = sum_varxvol+&
                     Var*VOL(IJK)
                ENDIF
            ENDIF
         ENDDO
      ENDDO
      CALL global_all_sum(sum_vol)
      CALL global_all_sum(sum_varxvol)
      IF (SUM_vol > ZERO) THEN
         s_press = sum_varxvol/sum_vol
      ENDIF
      s_pos = usr_y_s(L)

! calculating avg pressure at north plane
! initialize
      sum_vol = zero
      sum_varxvol=zero
      J = usr_j_n(L)
      DO K = usr_k_b(L), usr_k_t(L)
         DO I = usr_i_w(L), usr_i_e(L)
            IF(IS_ON_myPE_owns(I,J,K)) THEN
               IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
               IJK = FUNIJK(I,J,K)
               IF (FLUID_AT(IJK)) THEN
                  var = p_g(ijk)
                  SUM_vol = sum_vol + VOL(IJK)
                  SUM_varxvol = sum_varxvol+&
                     Var*VOL(IJK)
                ENDIF
            ENDIF
         ENDDO
      ENDDO
      CALL global_all_sum(sum_vol)
      CALL global_all_sum(sum_varxvol)
      IF (SUM_vol > ZERO) THEN
         n_press = sum_varxvol/sum_vol
      ENDIF
      n_pos = usr_y_n(L)

! Pressure Gradient, Assume a constant forcing set in mfix.dat:
      IF (UNITS=='SI') THEN
         gradp_va = (s_press-n_press)/(n_pos-s_pos)
      ELSE
         gradp_va = 10.0*(s_press-n_press)/(n_pos-s_pos)
      ENDIF

! F-Factor
      f_factor = 0.0
      IF (UNITS=='SI') THEN
         call GetAvgVar_N(sqrt(ro_g)*ep_g,phi_va,v_g,uphi_va,L)
      ELSE
         call GetAvgVar_N(sqrt(1000.0*ro_g)*ep_g, &
            phi_va,v_g*0.01,uphi_va,L)
      ENDIF
      f_factor = uphi_va

! The Liquid phase volume fraction
      ep_liquid =0.0
      DO IJK=IJKSTART3,IJKEND3
         IF(FLUID_AT(IJK)) THEN
            ep_liquid(ijk) = ep_s(ijk,index_liq)
         ENDIF
      ENDDO

! L/G ratio [L/M^3]
      grate=0.0
      lrate=0.0
      call GetAvgVar_N(ep_g,phi_va,v_g,uphi_va,L)
      grate = uphi_va
      call GetAvgVar_N(ep_liquid,phi_va,v_s(:,index_liq),uphi_va,L)
      lrate = uphi_va
      liquidogas =abs( lrate/grate ) * 1000.0
      if(grate == 0.0) liquidogas = 0.0 !protect against NAN

! Write output files:
      IF(myPE /= PE_IO) RETURN


! Volume Average file
      write(fTag,"('_',I2.2)") L
      write(lFileN,"(A,'.log')") FileN//fTag
      inquire(file=trim(lFileN),exist=lExists)
      IF(lExists) then
         open(lUnit,file=trim(adjustl(lFileN)), &
              status='old', position='append')
      ELSE
         open(lUnit,file=trim(adjustl(lFileN)), &
              status='new')
      ENDIF
      write(lUnit,'(2X,F7.3,1X,3(ES12.5,2X))') time, &
         gradp_va, f_factor, liquidogas
      close(lUnit)

      RETURN
      END SUBROUTINE write_customdata1
