# "Flogger" is simple and fast logging library for Modern Fortran applications.
#  https://github.com/arifyunando/flogger
# 
#  MIT License
# 
#  Copyright (c) 2023 Arif Y. Sunanhadikusuma
# 
#  Permission is hereby granted, free of charge, to any person obtaining a copy
#  of this software and associated documentation files (the "Software"), to deal
#  in the Software without restriction, including without limitation the rights
#  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
# 
#  The above copyright notice and this permission notice shall be included in all
#  copies or substantial portions of the Software.
# 
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#  SOFTWARE.

################################################################################
###                        DEFINE PROJECT DESCRIPTION                        ###
################################################################################

cmake_minimum_required(VERSION 3.15)

# set the project name and version
project(flogger LANGUAGES Fortran)
SET(VERSION 0.0.0)

################################################################################
###                       DEFINE THE FILES AND FOLDERS                       ###
################################################################################

# add the install targets and module files
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/mod)
set(CMAKE_Fortran_FLAGS "-cpp -g")

# set variables for files
set(lib_src
    ${PROJECT_SOURCE_DIR}/src/flogger_ansistyling.f90
    ${PROJECT_SOURCE_DIR}/src/flogger_formatter.f90
    ${PROJECT_SOURCE_DIR}/src/flogger.f90
)

set(test_flogger_src
    ${PROJECT_SOURCE_DIR}/test/test_flogger.f90
)
set(test_formatter_src
    ${PROJECT_SOURCE_DIR}/test/test_formatter.f90
)

# configure the libraries
add_library(flogger ${lib_src})

# configure the tests executable
add_executable(floggertest ${test_flogger_src})
target_link_libraries(floggertest flogger)

add_executable(formattertest ${test_formatter_src})
target_link_libraries(formattertest flogger)