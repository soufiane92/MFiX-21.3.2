function(add_udf_target UDF_DIR UDF_TARGET)
  setup_udf_sources()
  add_library(${UDF_TARGET} OBJECT ${UDF_SOURCES})
  set_source_files_properties(${UDF_SOURCES} PROPERTIES Fortran_FORMAT "FREE")
  setup_udf_target(${UDF_TARGET})
endfunction()


function(setup_udf_sources)
  file(GLOB UDF_SOURCES
    ${UDF_DIR}/*.f
    ${UDF_DIR}/*.f90
    ${UDF_DIR}/GhdTheory/*.f
    ${UDF_DIR}/GhdTheory/*.f90
    ${UDF_DIR}/cartesian_grid/*.f
    ${UDF_DIR}/cartesian_grid/*.f90
    ${UDF_DIR}/check_data/*.f
    ${UDF_DIR}/check_data/*.f90
    ${UDF_DIR}/chem/*.f
    ${UDF_DIR}/chem/*.f90
    ${UDF_DIR}/des/*.f
    ${UDF_DIR}/des/*.f90
    ${UDF_DIR}/des/pic/*.f
    ${UDF_DIR}/des/pic/*.f90
    ${UDF_DIR}/dmp_modules/*.f
    ${UDF_DIR}/dmp_modules/*.f90
    ${UDF_DIR}/dqmom/*.f
    ${UDF_DIR}/dqmom/*.f90
    ${UDF_DIR}/monitors/*.f
    ${UDF_DIR}/monitors/*.f90
    ${UDF_DIR}/qmomk/*.f
    ${UDF_DIR}/qmomk/*.f90
    ${UDF_DIR}/thermochemical/*.f
    ${UDF_DIR}/thermochemical/*.f90
    )

  set_source_files_properties(
    ${UDF_SOURCES} PROPERTIES
    Fortran_FORMAT "FREE"
    )

  # UDF usrnlst.inc requires recompiling usr_read_namelist.f
  # (object file from mfixcore would not work)
  if(EXISTS ${UDF_DIR}/usrnlst.inc)
    set(UDF_SOURCES
      ${UDF_SOURCES}
      ${PROJECT_SOURCE_DIR}/model/usr_read_namelist.f
      )
  endif()

  if((EXISTS ${UDF_DIR}/usr_rates.f) OR
      (EXISTS ${UDF_DIR}/usr_rates_des.f))
    set(SPECIES_INC ${UDF_DIR}/species.inc)
    generate_species_inc(${UDF_DIR} ${SPECIES_INC})
    set(UDF_SOURCES
      ${UDF_SOURCES} ${SPECIES_INC})
  endif()


  set(UDF_SOURCES ${UDF_SOURCES}
    PARENT_SCOPE)

endfunction()


function(generate_species_inc UDF_DIR SPECIES_INC)

  set(RXN_PREPROC_PY ${PROJECT_SOURCE_DIR}/model/cmake/rxn_preproc.py)
  find_package(PythonInterp REQUIRED) #  sets PYTHON_EXECUTABLE
  find_project_file("${UDF_DIR}") #  sets PROJECT_FILE

  if(NOT PROJECT_FILE)
    message( FATAL_ERROR "\
    usr_rates.f or usr_rates_des.f exists, \
    but no project file: mfix.dat nor *.mfx" )
  endif()

  set(RXN_PREPROC_DIR ${CMAKE_CURRENT_BINARY_DIR})
  if(TEST_BINARY_DIR)
    set(RXN_PREPROC_DIR ${TEST_BINARY_DIR})
  endif()

  add_custom_command(OUTPUT ${SPECIES_INC}
    COMMAND ${CMAKE_COMMAND} -E env RUN_DIR=${UDF_DIR}
    ${PYTHON_EXECUTABLE} ${RXN_PREPROC_PY} ${PROJECT_FILE}
    WORKING_DIRECTORY ${RXN_PREPROC_DIR}
    DEPENDS ${RXN_PREPROC_DIR}
    COMMENT "Generating species.inc")

endfunction()


function(find_project_file RUN_DIR)
  file(GLOB mfxs ${RUN_DIR}/*.mfx)

  if(EXISTS ${RUN_DIR}/mfix.dat)
    set(PROJECT_FILE ${RUN_DIR}/mfix.dat)
  elseif(EXISTS ${mfxs})
    get_filename_component(PROJECT_FILE ${mfxs} REALPATH)
  else()
    set(PROJECT_FILE "")
  endif()
  set(PROJECT_FILE ${PROJECT_FILE}
    PARENT_SCOPE)
endfunction()


function(setup_udf_target UDF_TARGET)
  set_target_properties(${UDF_TARGET} PROPERTIES EXCLUDE_FROM_ALL "1")
  set_property(TARGET ${UDF_TARGET} PROPERTY POSITION_INDEPENDENT_CODE ON)

  # for UDF .mod files and species.inc
  if(TEST_BINARY_DIR)
    target_include_directories(${UDF_TARGET} PUBLIC ${TEST_BINARY_DIR})
    set_target_properties(${UDF_TARGET} PROPERTIES
      Fortran_MODULE_DIRECTORY ${TEST_BINARY_DIR})
  endif()

  # for species.inc
  target_include_directories(${UDF_TARGET} PUBLIC ${CMAKE_BINARY_DIR})

  # for usrnlst.inc
  if(EXISTS ${UDF_DIR}/usrnlst.inc)
    target_compile_definitions(${UDF_TARGET} PUBLIC USR_NAMELIST)
    target_include_directories(${UDF_TARGET} PUBLIC ${UDF_DIR})
  endif()

  # Inherit compilation options from mfixcore
  add_dependencies(${UDF_TARGET} mfixcore)

  target_compile_definitions(${UDF_TARGET} PUBLIC
    $<TARGET_PROPERTY:mfixcore,INTERFACE_COMPILE_DEFINITIONS>)

  target_compile_options(${UDF_TARGET} PUBLIC
    $<TARGET_PROPERTY:mfixcore,INTERFACE_COMPILE_OPTIONS>)

  target_include_directories(${UDF_TARGET} PUBLIC
    $<TARGET_PROPERTY:mfixcore,INTERFACE_INCLUDE_DIRECTORIES>)

  if(ENABLE_COVERAGE)
    target_compile_options(${UDF_TARGET} PUBLIC ${COVERAGE_COMPILER_FLAGS})
  endif()
endfunction()
