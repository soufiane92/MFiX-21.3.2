set(TEST_PATH legacy_tutorials)

add_mfix_test(${TEST_PATH}/DEM_Wall_HT "FAILING" 4)

add_mfix_test(${TEST_PATH}/fluidBed1_dmp_test "FAILING" 4)

add_mfix_test(${TEST_PATH}/3dCfb "SERIAL" 1)
add_mfix_test(${TEST_PATH}/3dFluidbed "SERIAL" 1)
add_mfix_test(${TEST_PATH}/FluidBed2D "SERIAL" 1)
add_mfix_test(${TEST_PATH}/Fully_Developed_With_AhmadiModel "SERIAL" 1)
add_mfix_test(${TEST_PATH}/Fully_Developed_With_SimoninModel "SERIAL" 1)
add_mfix_test(${TEST_PATH}/Gidaspow_fbed_With_SimoninModel "SERIAL" 1)
add_mfix_test(${TEST_PATH}/LiquidFluidBed "SERIAL" 1)

# FIXME: timeout
add_mfix_test(${TEST_PATH}/Solvent_Absorption "FAILING" 1)

add_mfix_test(${TEST_PATH}/SpoutedBedCombustor "SERIAL" 1)
add_mfix_test(${TEST_PATH}/channel "SERIAL" 1)
add_mfix_test(${TEST_PATH}/fluidBed1 "SERIAL" 1)
add_mfix_test(${TEST_PATH}/fluidBed2 "SERIAL" 1)
add_mfix_test(${TEST_PATH}/hourglass "SERIAL" 1)
add_mfix_test(${TEST_PATH}/point_source_spiral "SERIAL" 1)
add_mfix_test(${TEST_PATH}/reactor1a "SERIAL" 1)
add_mfix_test(${TEST_PATH}/reactor1b "SERIAL" 1)
add_mfix_test(${TEST_PATH}/silane_pyrolysis "SERIAL" 1)

add_mfix_test(${TEST_PATH}/usr_drag "FAILING" 1)

add_mfix_test(${TEST_PATH}/variable_density "SERIAL" 1)
add_mfix_test(${TEST_PATH}/vtk_regions "SERIAL" 1)

add_mfix_test(${TEST_PATH}/IATheory "FAILING" 1)
