cmake_minimum_required(VERSION 3.4)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/model/cmake)

include(MfixSolver)


##################################
# Set options and define Project
##################################

include(MfixOptions)

set_mfix_build_type()

check_mfix_options()

set_languages()

project(MFiX
  LANGUAGES ${languages}
  )

# Load Code Coverage module
if(ENABLE_COVERAGE)
  include(CodeCoverage)
endif()

############################################################
# target: mfixcore library, defined in model/CMakeLists.txt
############################################################

add_subdirectory( model )

######################################################
# target:  mfixsolver executable
######################################################

# build_mfixsolver sets UDF_DIF path
if(NOT UDF_DIR)
  # if not build_mfixsolver, use current directory:
  set(UDF_DIR ${CMAKE_CURRENT_BINARY_DIR})
endif()

make_mfixsolver_target(${UDF_DIR} "mfixsolver" "udfs")

######################################################
# target:  postmfix executable
######################################################
if(ENABLE_POSTMFIX)
  add_subdirectory( post_mfix )
endif()

######################################################
# target: install
######################################################
if(ENABLE_PYMFIX)
  # mfixsolver extension module for pymfix solver
  include(PyMFIX)

else()
  # batch solver
  install(TARGETS mfixsolver DESTINATION ${CMAKE_INSTALL_PREFIX})

endif()

######################################################
# Add targets: coverage, clean_coverage
######################################################

if(ENABLE_COVERAGE)
  target_compile_options(mfixcore PUBLIC ${COVERAGE_COMPILER_FLAGS})
  target_compile_options(mfixsolver PUBLIC ${COVERAGE_COMPILER_FLAGS})
endif()

######################################################
# Add CTests:
######################################################

if(ENABLE_CTEST)
  include(MfixTests)
  enable_testing()
  setup_tests()
endif()
