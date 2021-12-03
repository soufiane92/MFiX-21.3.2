!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Namelist: INITIAL_CONDITIONS_LOCKED                                 !
!  Author: J.Musser                                   Date: 03-APR-15  !
!                                                                      !
!  Purpose: List MFIX keywords that CANNOT be edited during a run.     !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      NAMELIST / INITIAL_CONDITIONS_LOCKED /&

      IC_DES_FIT_TO_REGION, IC_PIC_CONST_STATWT, &
      IC_DES_LATTICE, IC_DES_SPACING, IC_DES_SPACE_FACTOR_X, &
      IC_DES_SPACE_FACTOR_Y, IC_DES_SPACE_FACTOR_Z , &
      IC_DES_RAND, IC_DES_RAND_FACTOR_X, IC_DES_RAND_FACTOR_Y, &
      IC_DES_RAND_FACTOR_Z, &
      IC_DES_FIT_TO_REGION, &
      IC_DES_SM, IC_DES_NP



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Namelist: INITIAL_CONDITIONS_UNLOCKED                               !
!  Author: J.Musser                                   Date: 03-APR-15  !
!                                                                      !
!  Purpose: List MFIX keywords that may be edited during a run by      !
!  invoking a REFRESH.                                                 !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      NAMELIST / INITIAL_CONDITIONS_UNLOCKED /&

      IC_X_w, IC_X_e, IC_Y_s, IC_Y_n, IC_Z_b, IC_Z_t,&
      IC_I_w, IC_I_e, IC_J_s, IC_J_n, IC_K_b, IC_K_t,&
      IC_EP_g, IC_P_g, IC_ROP_s, IC_T_g, IC_T_s,&
      IC_U_g, IC_U_s, IC_V_g, IC_V_s, IC_W_g, IC_W_s,&
      IC_X_g, IC_X_s, IC_TYPE, IC_P_star, IC_L_scale,&
      IC_GAMA_Rg, IC_T_Rg, IC_GAMA_Rs, IC_T_Rs,&
      IC_Theta_m, IC_Scalar, IC_K_Turb_G, IC_E_Turb_G, IC_EP_s,&
      IC_PSD_type, IC_PSD_mean_dp, IC_PSD_stdev, IC_PSD_max_dp,&
      IC_PSD_min_dp,&
      PART_IN_X_MIN, PART_IN_X_MAX,PART_IN_X_EXCLUDE, &
      PART_IN_Y_MIN, PART_IN_Y_MAX,PART_IN_Y_EXCLUDE, &
      PART_IN_Z_MIN, PART_IN_Z_MAX,PART_IN_Z_EXCLUDE, &
      PART_IN_PHASE, &
      PART_IN_DIAMETER_MIN, PART_IN_DIAMETER_MAX, PART_IN_DIAMETER_EXCLUDE, &
      PART_IN_DENSITY_MIN, PART_IN_DENSITY_MAX, PART_IN_DENSITY_EXCLUDE, &
      PART_IN_U_MIN, PART_IN_U_MAX, PART_IN_U_EXCLUDE, &
      PART_IN_V_MIN, PART_IN_V_MAX, PART_IN_V_EXCLUDE, &
      PART_IN_W_MIN, PART_IN_W_MAX, PART_IN_W_EXCLUDE, &
      PART_IN_TEMP_MIN, PART_IN_TEMP_MAX, PART_IN_TEMP_EXCLUDE, &
      PART_IN_X_S_MIN, PART_IN_X_S_MAX, PART_IN_X_S_EXCLUDE, &
      PART_IN_USR_VAR_MIN, PART_IN_USR_VAR_MAX, PART_IN_USR_VAR_EXCLUDE

