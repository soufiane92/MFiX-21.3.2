set(TEST_PATH legacy_tests/dem-tests)

add_mfix_test(${TEST_PATH}/circle-advection "DMP" 1)
add_mfix_test(${TEST_PATH}/contact-conduction "DMP" 1)
add_mfix_test(${TEST_PATH}/convection "DMP" 1)
add_mfix_test(${TEST_PATH}/homogeneous-cooling-system "DMP" 1)
add_mfix_test(${TEST_PATH}/particle-fluid-particle-conduction "DMP" 1)
add_mfix_test(${TEST_PATH}/particle-vortex "DMP" 1)
add_mfix_test(${TEST_PATH}/poly_dem_inlet_outlet "DMP" 1)
add_mfix_test(${TEST_PATH}/radiation "DMP" 1)
add_mfix_test(${TEST_PATH}/reactive-chemistry "DMP" 1)
add_mfix_test(${TEST_PATH}/restart "DMP" 1)
add_mfix_test(${TEST_PATH}/sphere-advection "DMP" 1)

add_mfix_test(${TEST_PATH}/condensation "SERIAL" 1)
add_mfix_test(${TEST_PATH}/evaporation "SERIAL" 1)

add_mfix_test(${TEST_PATH}/restart2 "FAILING" 1)
