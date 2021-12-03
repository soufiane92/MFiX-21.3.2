      MODULE ur_facs

      use param, only: DIM_EQS

! Underrelaxation factors
      DOUBLE PRECISION :: UR_FAC(DIM_EQS)

! Underrelaxation factors for coefficient update:
!  [0]  every time step (explicit)
!  [1]  every iteration (implicit)
! (0,1) underrelaxed

! Note that these values need to be temporarily set to 1 before the calc_coeff
! call in time_march.  And after the call reset to their original value.

! Underrelaxation factor for gas-solids drag coefficient
      DOUBLE PRECISION :: UR_F_gs

! Underrelaxation factor for conductivity coefficient associated with
! other solid phases for IA theory
      DOUBLE PRECISION :: UR_kth_sml

      END MODULE ur_facs
