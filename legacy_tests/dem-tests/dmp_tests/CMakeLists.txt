set(TEST_PATH legacy_tests/dem-tests/dmp_tests)

add_mfix_test(${TEST_PATH}/dmp_restart "FAILING" 1)
add_mfix_test(${TEST_PATH}/freely-falling-particle "FAILING" 8)
