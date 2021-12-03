set(TEST_PATH legacy_tests)

# FIXME:  DT < DT_MIN at TIME 0.14602E-03 out of TSTOP 0.5E-3
add_mfix_test(${TEST_PATH}/tsuji "FAILING" 1)

add_mfix_test(${TEST_PATH}/2Dfluidbed_VDW_high_coh "DMP" 1)
add_mfix_test(${TEST_PATH}/Added_Mass "DMP" 1)
add_mfix_test(${TEST_PATH}/Added_Mass_CartesianGridsCase "DMP" 1)
add_mfix_test(${TEST_PATH}/Added_Mass_LiftForce "DMP" 1)

# FIXME: timeout
add_mfix_test(${TEST_PATH}/FluidBed_DES_3D "FAILING" 4)

add_mfix_test(${TEST_PATH}/HYS_drag_tutorial "DMP" 1)
add_mfix_test(${TEST_PATH}/adiabaticFlame "DMP" 1)
add_mfix_test(${TEST_PATH}/cohesionVdwAsperities "DMP" 1)
add_mfix_test(${TEST_PATH}/cond_conv "DMP" 1)
add_mfix_test(${TEST_PATH}/conduction2 "DMP" 1)
add_mfix_test(${TEST_PATH}/cyclic-shear "DMP" 1)
add_mfix_test(${TEST_PATH}/dmp_test "DMP" 4)
add_mfix_test(${TEST_PATH}/dmp_test_cyclic "DMP" 4)
add_mfix_test(${TEST_PATH}/granular_shear "DMP" 1)
add_mfix_test(${TEST_PATH}/ozone "DMP" 1)
add_mfix_test(${TEST_PATH}/packed_bed "DMP" 1)
add_mfix_test(${TEST_PATH}/periodic-flow-fixed-mass-flux "DMP" 1)
add_mfix_test(${TEST_PATH}/rotary-drum "DMP" 1)
add_mfix_test(${TEST_PATH}/segregation "DMP" 1)
add_mfix_test(${TEST_PATH}/settling-tank "DMP" 1)
add_mfix_test(${TEST_PATH}/simple-2phase "DMP" 1)
add_mfix_test(${TEST_PATH}/solid-body-rotation "DMP" 1)
add_mfix_test(${TEST_PATH}/ssfGHD "DMP" 1)
add_mfix_test(${TEST_PATH}/thermo "DMP" 1)

# FIXME: File "AUTOTEST/POST_epgAxial.dat" differs
add_mfix_test(${TEST_PATH}/fluidBed1SIConstVisc  "FAILING" 1)

add_mfix_test(${TEST_PATH}/chi-scheme "SERIAL" 1)
add_mfix_test(${TEST_PATH}/conduction "SERIAL" 1)
add_mfix_test(${TEST_PATH}/developed_pipe_flow "SERIAL" 1)
add_mfix_test(${TEST_PATH}/developed_pipe_flow1 "SERIAL" 1)
add_mfix_test(${TEST_PATH}/developed_slit_flow "SERIAL" 1)
add_mfix_test(${TEST_PATH}/dqmom "SERIAL" 1)
add_mfix_test(${TEST_PATH}/driven_cavity_cohesion_VDW "SERIAL" 1)
add_mfix_test(${TEST_PATH}/drying "SERIAL" 1)
add_mfix_test(${TEST_PATH}/fluidBed1SI "SERIAL" 1)
add_mfix_test(${TEST_PATH}/phase_change "SERIAL" 1)
add_mfix_test(${TEST_PATH}/turbulent_pipe_flow "SERIAL" 1)

# FIXME: File "POST_ROP_s_1.dat" differs
add_mfix_test(${TEST_PATH}/hopper "FAILING" 1)

# POST_EP_g_1.dat differs
add_mfix_test(${TEST_PATH}/fluidBed1_cn "FAILING" 1)
