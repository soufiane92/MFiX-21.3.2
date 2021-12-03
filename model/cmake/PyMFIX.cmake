cmake_minimum_required(VERSION 3.14)

if(NOT "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  message(FATAL_ERROR "ENABLE_PYMFIX is only supported for GNU Fortran compiler")
endif()

find_package(Python3 COMPONENTS Interpreter NumPy)

if(NOT Python3_FOUND)
  message(FATAL_ERROR "Unable to find Python 3 with NumPy for building pymfix")
endif()

###########################
## Extension module file suffix (for example .cpython-36m-x86_64-linux-gnu.so)
###########################
if(NOT F2PY_SUFFIX)
  execute_process(COMMAND "${Python3_EXECUTABLE}" -c
    "import sysconfig; print(sysconfig.get_config_var('EXT_SUFFIX') or sysconfig.get_config_var('SO'))"
    OUTPUT_VARIABLE PYTHON_EXT_SUFFIX
    RESULT_VARIABLE FOUND_PYTHON_EXTENSION)
  string(STRIP "${PYTHON_EXT_SUFFIX}" PYTHON_EXT_SUFFIX)
  if(NOT FOUND_PYTHON_EXTENSION EQUAL 0)
    set(F2PY_SUFFIX "" CACHE STRING "Suffix added by F2Py to the module name to get the output file name.")
    message(FATAL_ERROR "Unable to determine file extension of compiled Python modules - specify it with F2PY_SUFFIX")
  endif()
  set(F2PY_SUFFIX "${PYTHON_EXT_SUFFIX}" CACHE STRING INTERNAL)
endif()


###########################
## Find the path to the f2py src directory
###########################

execute_process(COMMAND "${Python3_EXECUTABLE}" -c "import numpy.f2py; import os.path; print(os.path.join(os.path.dirname(numpy.f2py.__file__),'src'), end='')"
  OUTPUT_VARIABLE F2PY_SRC
  RESULT_VARIABLE FOUND_F2PY_SRC)
file(TO_CMAKE_PATH "${F2PY_SRC}" F2PY_SRC)

###########################
## Copy wrapped mfix sources to .f90 extension
###########################

set(WRAPPED_SOURCES
  dmp_modules/compar_mod.f
  dmp_modules/parallel_mpi_mod.f
  exit.f
  iterate.f
  machine_mod.f
  main.f
  param_mod.f
  pause.f
  residual_pub_mod.f
  run_mod.f
  time_cpu_mod.f
  )

foreach(SOURCE ${WRAPPED_SOURCES})
  file(COPY
    ${PROJECT_SOURCE_DIR}/model/${SOURCE}
    DESTINATION
    ${PROJECT_BINARY_DIR}/pymfix)
endforeach()

file(GLOB WRAPPED_F_SRCS
  ${PROJECT_BINARY_DIR}/pymfix/*.f
)

foreach(SOURCE ${WRAPPED_F_SRCS})
  file(RENAME
    ${SOURCE}
    ${SOURCE}90)
endforeach()

file(GLOB WRAPPED_F90_SRCS
  ${PROJECT_BINARY_DIR}/pymfix/*.f90
)

###########################
## Generate wrapper sources from wrapped sources
###########################

set(WRAPPER_DIR ${CMAKE_BINARY_DIR}/f2pywrappers)
set(C_WRAPPER ${WRAPPER_DIR}/mfixsolvermodule.c)
set(F90_WRAPPER ${WRAPPER_DIR}/mfixsolver-f2pywrappers2.f90)
set(F_WRAPPER ${WRAPPER_DIR}/mfixsolver-f2pywrappers.f)

add_custom_command(OUTPUT ${C_WRAPPER} ${F_WRAPPER} ${F90_WRAPPER}
  COMMAND ${CMAKE_COMMAND} -E env "${Python3_EXECUTABLE}" -m numpy.f2py
  -m mfixsolver
  --quiet
  --lower
  --build-dir ${WRAPPER_DIR}
  ${WRAPPED_F90_SRCS}

  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
  DEPENDS ${CMAKE_BINARY_DIR}
  COMMENT "Generating f2py")

###########################
## Add mfixsolver Python extension module shared library
###########################

if(TARGET udfs)
  set( UDF_OBJS $<TARGET_OBJECTS:udfs> )
else()
  set( UDF_OBJS )
endif()

add_library(mfixsolver_ext SHARED
  ${C_WRAPPER} ${F_WRAPPER} ${F90_WRAPPER}
  ${WRAPPED_F90_SRCS}
  ${F2PY_SRC}/fortranobject.c
  ${UDF_OBJS}
  )

# disable adding prefix "lib" (mfixsolver.so not libmfixsolver.so)
set_target_properties(mfixsolver_ext PROPERTIES
  PREFIX ""
  OUTPUT_NAME "mfixsolver"
  SUFFIX "${F2PY_SUFFIX}"
  )

set_source_files_properties(${F90_WRAPPER} PROPERTIES Fortran_FORMAT "FREE")
set_source_files_properties(${F_WRAPPER} PROPERTIES Fortran_FORMAT "FIXED")

set_source_files_properties(
  ${F90_SRCS}
  ${F_WRAPPER} ${F90_WRAPPER}
  PROPERTIES
  COMPILE_FLAGS "-fno-second-underscore"
  )

## Link extension with mfixcore and use mfixcore compiler flags
target_link_libraries( mfixsolver_ext mfixcore )

if(APPLE)
  target_link_libraries(mfixsolver_ext "-undefined dynamic_lookup")
endif()

target_compile_definitions(mfixsolver_ext
  PUBLIC $<TARGET_PROPERTY:mfixcore,INTERFACE_COMPILE_DEFINITIONS>)
target_compile_options(mfixsolver_ext
  PUBLIC $<TARGET_PROPERTY:mfixcore,INTERFACE_COMPILE_OPTIONS>
  PUBLIC "-w"
  )
target_include_directories(mfixsolver_ext
  PUBLIC $<TARGET_PROPERTY:mfixcore,INTERFACE_INCLUDE_DIRECTORIES>)

## Compile and link extension with Python
target_include_directories(mfixsolver_ext PUBLIC ${Python3_NumPy_INCLUDE_DIRS})
target_include_directories(mfixsolver_ext PUBLIC ${Python3_INCLUDE_DIRS}
  ${F2PY_SRC}
)

target_link_libraries(mfixsolver_ext ${Python3_LIBRARIES})

if(ENABLE_MPI)
  set(LD_PRELOAD_MPI
    "LD_PRELOAD=${MPI_mpi_LIBRARY}"
    )
  set(LD_LIBRARY_PATH
    "LD_LIBRARY_PATH=\${LD_LIBRARY_PATH:+\$LD_LIBRARY_PATH:}\"${Python3_LIBRARIES}\""
    )
endif()

if(EXISTS ${CMAKE_SOURCE_DIR}/pyproject.toml)
  set(MFIXGUI_PATH ${CMAKE_SOURCE_DIR})
endif()

set(SOLVER_PATH ${CMAKE_INSTALL_PREFIX})

if(WIN32)
  set(MFIXSOLVER_SCRIPT "mfixsolver.bat")
  file(WRITE ${CMAKE_BINARY_DIR}/${MFIXSOLVER_SCRIPT}
"@echo on

setlocal
set PYTHONPATH=${SOLVER_PATH};${MFIXGUI_PATH};%PYTHONPATH%
call \"${Python3_EXECUTABLE}\" -m mfixgui.pymfix %*
endlocal")

else()
  set(MFIXSOLVER_SCRIPT "mfixsolver.sh")
  file(WRITE ${CMAKE_BINARY_DIR}/${MFIXSOLVER_SCRIPT}
"#!/bin/sh

env ${LD_PRELOAD_MPI} ${LD_LIBRARY_PATH} PYTHONPATH=\"${SOLVER_PATH}\":\"${MFIXGUI_PATH}\":\${PYTHONPATH:+:\$PYTHONPATH} ${Python3_EXECUTABLE} -m mfixgui.pymfix \"\$@\"
")

endif()

install(TARGETS mfixsolver_ext DESTINATION "${SOLVER_PATH}")

if(NOT "$ENV{CONDA_BUILD}")
  install(
    PROGRAMS ${CMAKE_BINARY_DIR}/${MFIXSOLVER_SCRIPT}
    DESTINATION ${CMAKE_INSTALL_PREFIX})
endif()
