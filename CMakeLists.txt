CMAKE_MINIMUM_REQUIRED(VERSION 3.0.2)
# Default compiler gfortran, otherwise cmake Fortran package will set it to f95
# You can override the compiler with cmake -DFTOBJ_COMPILER=
set(FTOBJ_COMPILER "/usr/bin/gfortran" CACHE STRING "Fortran Compiler to build ftobj")
set(FTOBJ_AR "/usr/bin/gcc-ar" CACHE STRING "Static archive command")
set(FTOBJ_RANLIB "/usr/bin/gcc-ranlib" CACHE STRING "ranlib used to create Static archive")
set(FTOBJ_COMPILER_FLAGS "-cpp" CACHE STRING "Compiler flags to build ftobj")
message("-- FTOBJ -------------  cmake START -------------------")
message("-- FTOBJ_COMPILER:       ${FTOBJ_COMPILER}")
message("-- FTOBJ_AR:             ${FTOBJ_AR}")
message("-- FTOBJ_RANLIB:         ${FTOBJ_RANLIB}")
message("-- FTOBJ_COMPILER_FLAGS: ${FTOBJ_COMPILER_FLAGS}")
message("-- FTOBJ ----------------------------------------------")

set(CMAKE_Fortran_COMPILER_INIT ${FTOBJ_COMPILER})
set(CMAKE_VERBOSE_MAKEFILE ON)


PROJECT(ftobj Fortran C)
SET(CMAKE_BUILD_TYPE ${FTOBJ_BUILD_TYPE})
SET(VERSION 0.0.0)
SET(CMAKE_AR ${FTOBJ_AR})
SET(CMAKE_RANLIB ${FTOBJ_RANLIB})
SET(CMAKE_INSTALL_PREFIX ${FTOBJ_INSTALL_DIR})
message("-- CMAKE_INSTALL_PREFIX:${CMAKE_INSTALL_PREFIX}")

INCLUDE(FortranCInterface)
FortranCInterface_VERIFY()
IF(NOT FortranCInterface_VERIFIED_C)
	MESSAGE(FATAL_ERROR "Fortran compiler must support C Interface")
ENDIF(NOT FortranCInterface_VERIFIED_C)
	
IF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    MESSAGE(FATAL_ERROR "Fortran compiler does not support F90")
ENDIF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

ADD_SUBDIRECTORY(${CMAKE_SOURCE_DIR}/Source)

message("-- FTOBJ -------------  cmake DONE --------------------")
