CMAKE_MINIMUM_REQUIRED(VERSION 3.15.0)
# Default compiler gfortran, otherwise cmake Fortran package will set it to f95
# You can override the compiler with cmake -DFTOBJ_COMPILER=
#set(FTOBJ_AR "/usr/bin/gcc-ar" CACHE STRING "Static archive command")
#set(FTOBJ_RANLIB "/usr/bin/gcc-ranlib" CACHE STRING "ranlib used to create Static archive")
INCLUDE(CMakePackageConfigHelpers)
SET(FFLAGS "-cpp -O -std=f2018 -pedantic -Werror -fimplicit-none -Wall -Wextra -Wcharacter-truncation -Wimplicit-interface -Wimplicit-procedure -Wno-compare-reals -Wno-unused-dummy-argument -Wno-intrinsic-shadow -Wno-implicit-interface -Wno-implicit-procedure" CACHE STRING "Fortran compiler flags")

MESSAGE("-- FTOBJ -------------  cmake START -------------------")
MESSAGE("-- FFLAGS:               ${FFLAGS}")
MESSAGE("-- CMAKE_INSTALL_PREFIX: ${CMAKE_INSTALL_PREFIX}")
MESSAGE("-- FTOBJ ----------------------------------------------")

SET(CMAKE_VERBOSE_MAKEFILE ON)

SET(CMAKE_Fortran_COMPILER_INIT ${FC})
SET(CMAKE_Fortran_FLAGS "${FFLAGS} ${CMAKE_Fortran_FLAGS}")

PROJECT(ftobj Fortran)
SET(VERSION 0.0.0)

IF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    MESSAGE(FATAL_ERROR "Fortran compiler does not support F90")
ENDIF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

ADD_SUBDIRECTORY(${CMAKE_SOURCE_DIR}/Source)

configure_package_config_file(FTObjectLibraryConfig.cmake.in
	                      ${CMAKE_CURRENT_BINARY_DIR}/FTObjectLibraryConfig.cmake 
			      INSTALL_DESTINATION ${CMAKE_INSTALL_PREFIX}
			      PATH_VARS CMAKE_INSTALL_PREFIX)
INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/FTObjectLibraryConfig.cmake
        DESTINATION ${CMAKE_INSTALL_PREFIX})	

message("-- FTOBJ -------------  cmake DONE --------------------")
