# remove OpenMP flags from environment (only wanted if ENABLE_OpenMP not set)
# remove -ftree-vectorize (causes failure in leq_bicgs.f)
foreach(flag -fopenmp -ftree-vectorize)
  string(REPLACE ${flag} "" tmp "$ENV{DEBUG_FFLAGS}")
  set(ENV{DEBUG_FFLAGS} ${tmp})
  string(REPLACE ${flag} "" tmp "$ENV{DEBUG_FORTRANFLAGS}")
  set(ENV{DEBUG_FORTRANFLAGS} ${tmp})
  string(REPLACE ${flag} "" tmp "$ENV{FFLAGS}")
  set(ENV{FFLAGS} ${tmp})
  string(REPLACE ${flag} "" tmp "$ENV{FORTRANFLAGS}")
  set(ENV{FORTRANFLAGS} ${tmp})
endforeach()

option(ENABLE_MPI "Build with MPI (dmp) support" OFF)
option(ENABLE_OpenMP "Build with OpenMP (smp) support" OFF)
option(ENABLE_COVERAGE "Build with gcov/lcov code coverage" OFF)
option(ENABLE_CTEST "Enable building MFiX CTest Suite" OFF)
if(DEFINED ENV{AR} AND NOT WIN32)
  set(CMAKE_AR "$ENV{AR}" CACHE FILEPATH "Archiver")
endif()

######################################################
# Set CMAKE_BUILD_TYPE (if undefined)
######################################################

function(set_mfix_build_type)

  # Default for build type
  set(default_build_type "RelWithDebInfo")
  if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_Fortran_FLAGS)
    message(STATUS "Setting build type to '${default_build_type}' as none was specified.")
    set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE
      STRING "Choose the type of build." FORCE)
    # Set the possible values of build type for cmake-gui
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
  endif()

endfunction()

######################################################
# Set languages (Fortran, C)
######################################################

function(set_languages)

  if(ENABLE_PYMFIX)
    set(languages C Fortran PARENT_SCOPE)
  else()
    set(languages Fortran PARENT_SCOPE)
  endif()

endfunction()

######################################################
# Check options; print build settings summary
######################################################

function(check_mfix_options)

  # ctest targets require postmfix to run
  if(ENABLE_CTEST)
    set(ENABLE_POSTMFIX ON PARENT_SCOPE)
  endif()

  if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
    # require at least gcc 4.6
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 4.6)
      message(FATAL_ERROR
        "GFortran version ${CMAKE_Fortran_COMPILER_VERSION} found. \
        MFiX requires GFortran 4.6 or later.")
    endif()
  endif()

  message(STATUS "MFIX build settings summary: ")
  message(STATUS "   Build type        = ${CMAKE_BUILD_TYPE}")
  message(STATUS "   CMake version     = ${CMAKE_VERSION}")
  message(STATUS "   Fortran compiler  = ${CMAKE_Fortran_COMPILER}")
  if(CMAKE_BUILD_TYPE)
    string(TOUPPER ${CMAKE_BUILD_TYPE} BUILDTYPE)
    message(STATUS "   Fortran flags     = ${CMAKE_Fortran_FLAGS_${BUILDTYPE}}")
  else()
    message(STATUS "   Fortran flags     = ${CMAKE_Fortran_FLAGS}")
  endif()
  message(STATUS "   ENABLE_MPI        = ${ENABLE_MPI}")
  message(STATUS "   ENABLE_OpenMP     = ${ENABLE_OpenMP}")
  message(STATUS "   ENABLE_CTEST      = ${ENABLE_CTEST}")
  message(STATUS "   ENABLE_COVERAGE   = ${ENABLE_COVERAGE}")

endfunction()


######################################################
# Find MFIX_VERSION (from GUI, tarball, or Git)
######################################################

function(set_mfix_version)
  set( VERSION_FILE ${CMAKE_CURRENT_SOURCE_DIR}/.tarball-version )
  if(VERSION)
    # running from build_mfixsolver (either source or package)
    set(MFIX_VERSION ${VERSION})

  elseif(EXISTS ${VERSION_FILE})
    # running cmake directly from source tarball
    file(READ ${VERSION_FILE} VERSION_FILE_VERSION)
    set(MFIX_VERSION ${VERSION_FILE_VERSION})

  elseif(EXISTS ${PROJECT_SOURCE_DIR}/.git)
    # running cmake directly from a git repo
    find_package(Git)
    if(Git_FOUND)
      execute_process(COMMAND ${GIT_EXECUTABLE} describe --abbrev=0
        OUTPUT_VARIABLE GIT_REPO_VERSION
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        )
      string(STRIP "${GIT_REPO_VERSION}" GIT_REPO_VERSION)
      set(MFIX_VERSION "${GIT_REPO_VERSION}")

    else()
      message(WARNING
        "Directory .git but git is not installed, unknown CMake version")
      set(MFIX_VERSION 0.0)
    endif()

  else()
    message(WARNING "Unknown CMake version")
    set(MFIX_VERSION 0.0)

  endif()

  # Quote MFIX_VERSION string, to be used in run_mod.f
  set(MFIX_VERSION "\"${MFIX_VERSION}\"" PARENT_SCOPE)

endfunction()
