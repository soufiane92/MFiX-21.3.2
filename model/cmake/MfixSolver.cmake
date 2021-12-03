include(MfixUdf)


function(make_mfixsolver_target UDF_DIR SOLVER_TARGET UDF_TARGET)
  # On Windows change slashes: \ to /
  file(TO_CMAKE_PATH ${UDF_DIR} UDF_DIR)

  setup_udf_sources()  # Defines UDF_SOURCES
  setup_solver_sources()
  add_executable(${SOLVER_TARGET} ${SOLVER_SOURCES})
  setup_solver_target()
endfunction()


function(setup_solver_sources)
  set(SOLVER_SOURCES ${PROJECT_SOURCE_DIR}/model/mfix.f
    PARENT_SCOPE)

  if(UDF_SOURCES)
    message(STATUS "\nFound UDFs:\n${UDF_SOURCES}")
    add_udf_target(${UDF_DIR} ${UDF_TARGET})
    set(SOLVER_SOURCES
      ${SOLVER_SOURCES}
      $<TARGET_OBJECTS:${UDF_TARGET}>
      PARENT_SCOPE)
  endif()
endfunction()


function(setup_solver_target)
  set_target_properties(${SOLVER_TARGET} PROPERTIES Fortran_FORMAT "FREE")
  set_target_properties(${SOLVER_TARGET} PROPERTIES LINKER_LANGUAGE Fortran)

  target_link_libraries(${SOLVER_TARGET} mfixcore)

  target_compile_definitions(${SOLVER_TARGET} PUBLIC
    $<TARGET_PROPERTY:mfixcore,INTERFACE_COMPILE_DEFINITIONS>)

  target_compile_options(${SOLVER_TARGET} PUBLIC
    $<TARGET_PROPERTY:mfixcore,INTERFACE_COMPILE_OPTIONS>)

  target_include_directories(${SOLVER_TARGET} PUBLIC
    $<TARGET_PROPERTY:mfixcore,INTERFACE_INCLUDE_DIRECTORIES>)

  if(UDF_SOURCES)
    add_dependencies(${SOLVER_TARGET} ${UDF_TARGET})
  endif()

  if(WIN32 OR MINGW)
    target_link_libraries(${SOLVER_TARGET}
      advapi32 gcc_eh shell32 userenv ws2_32 wsock32)
  endif()
endfunction()
