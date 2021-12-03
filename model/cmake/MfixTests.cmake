function(setup_tests)
  check_numdiff()

  # Run "make build_test" to build all tests
  set(BUILD_TEST build_test)
  add_custom_target(${BUILD_TEST})

  set(SUBDIRS
    benchmarks/dem
    benchmarks/tfm/ParallelBenchmarkCases
    legacy_tests
    legacy_tests/dem-tests
    legacy_tests/dem-tests/dmp_tests
    legacy_tutorials
    legacy_tutorials/Cartesian_grid_tutorials
    legacy_tutorials/DEM_CG_TUTORIALS
    legacy_tutorials/QMOM_TUTORIALS
    tests/dem
    tests/fluid
    tests/mms
    tutorials
    )

  foreach( SUBDIR ${SUBDIRS} )
    add_subdirectory( ${SUBDIR} )
  endforeach()
endfunction()


function(check_numdiff)
  find_program(NUMDIFF numdiff)
  if(NOT NUMDIFF)
    message(FATAL_ERROR "
    numdiff not found! Unable to build with CTests, \
    the MFiX ctests test suite requires the numdiff command.
    Install numdiff to your PATH.
    ")
  endif()
endfunction()


function(add_mfix_test TEST_PATH LABEL MPI_RANK)
  string(REPLACE "/" "__" TEST_NAME ${TEST_PATH})
  set(TEST_SOURCE_DIR ${PROJECT_SOURCE_DIR}/${TEST_PATH})
  set(TEST_BINARY_DIR ${CMAKE_BINARY_DIR}/${TEST_PATH})
  set(TEST_BINARY_EXE mfixsolver_${TEST_NAME})
  setup_test_dir(${TEST_BINARY_DIR} ${TEST_SOURCE_DIR})
  make_ctest_target(${TEST_SOURCE_DIR} ${TEST_BINARY_EXE})

  find_project_file(${TEST_BINARY_DIR}) #  sets PROJECT_FILE
  if(NOT PROJECT_FILE)
    message( FATAL_ERROR "\
    error in MFiX CMake scripts: add_mfix_test() on:  ${TEST_PATH} \
    but no project file was found in that directory. " )
  endif()

  if(MPI_RANK EQUAL 1)
    set(MPIRANK "")
  else()
    set(MPIRANK MPIRANK=${MPI_RANK})
  endif()

  if(LABEL STREQUAL "TUTORIAL")
    find_package(Python3 COMPONENTS Interpreter NumPy)
    execute_process(
      COMMAND ${Python3_EXECUTABLE} -m "mfixgui.vtk_widgets.geometry_engine" ${PROJECT_FILE}
      RESULT_VARIABLE EXIT_STATUS)
    if(NOT EXIT_STATUS EQUAL 0)
      message(FATAL_ERROR "Cannot generate *.stl for ${PROJECT_FILE}")
    endif()

    set(TSTOP TSTOP="0.0001")
  else()
    set(TSTOP TSTOP="")
  endif()

  set(TEST_RUN run_${TEST_NAME})
  add_custom_target(${TEST_RUN}
    COMMAND ${CMAKE_COMMAND} -E env ${MPIRANK} ${TSTOP}
    ${PROJECT_SOURCE_DIR}/model/cmake/runtests.sh
    ${CMAKE_CURRENT_BINARY_DIR}/${TEST_BINARY_EXE}
    ${CMAKE_BINARY_DIR}/postmfix

    DEPENDS ${TEST_BINARY_EXE}
    WORKING_DIRECTORY ${TEST_BINARY_DIR})

  add_test(NAME ${TEST_NAME}
    COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR} --target ${TEST_RUN}
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
  set_tests_properties(${TEST_NAME} PROPERTIES LABELS ${LABEL})
endfunction()


function(make_ctest_target UDF_DIR TEST_TARGET)
  make_mfixsolver_target(${UDF_DIR} ${TEST_TARGET} udf_${TEST_NAME})
  add_dependencies(${BUILD_TEST} ${TEST_TARGET})
  add_dependencies(${TEST_TARGET} postmfix)
  set_target_properties(${TEST_TARGET} PROPERTIES EXCLUDE_FROM_ALL "1")
endfunction()


function(setup_test_dir TEST_BINARY_DIR TEST_SOURCE_DIR)
  file(MAKE_DIRECTORY ${TEST_BINARY_DIR})

  file(GLOB EXTRA_FILES
    ${TEST_SOURCE_DIR}/*.dat
    ${TEST_SOURCE_DIR}/*.f
    ${TEST_SOURCE_DIR}/*.f95
    ${TEST_SOURCE_DIR}/*.inp
    ${TEST_SOURCE_DIR}/*.mfx
    ${TEST_SOURCE_DIR}/*.msh
    ${TEST_SOURCE_DIR}/*.sh
    ${TEST_SOURCE_DIR}/*.stl
    ${TEST_SOURCE_DIR}/*.txt
    ${TEST_SOURCE_DIR}/AUTOTEST)

  foreach(EXTRA_FILE ${EXTRA_FILES})
    get_filename_component(ABS_EXTRA_FILE ${EXTRA_FILE} REALPATH)
    file(COPY ${ABS_EXTRA_FILE} DESTINATION ${TEST_BINARY_DIR})
  endforeach()

endfunction()
