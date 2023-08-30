
########################################################
###            DEFINE PROJECT DESCRIPTION            ###
########################################################

cmake_minimum_required(VERSION 3.15)

# set the project name and version
project(flogger LANGUAGES Fortran)
SET(VERSION 0.0.0)

########################################################
###           DEFINE THE FILES AND FOLDERS           ###
########################################################

# add the install targets and module files
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/run)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/mod)
set(CMAKE_Fortran_FLAGS "-cpp -g")

# set variables for files
set(lib_src
    ${PROJECT_SOURCE_DIR}/src/ansi_styling.f90
    ${PROJECT_SOURCE_DIR}/src/formatter.f90
    ${PROJECT_SOURCE_DIR}/src/flogger.f90
)
set(main_src
    ${PROJECT_SOURCE_DIR}/src/flogger_test.f90
)

# configure the libraries
add_library(flogger ${lib_src})

# configure the executable
add_executable(floggertest ${main_src})
target_link_libraries(floggertest flogger)