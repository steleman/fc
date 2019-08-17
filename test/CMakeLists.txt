# Copyright (c) 2019, Compiler Tree Technologies Pvt Ltd.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice, this
#    list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
add_subdirectory(utils)

file(GLOB BASIC_FILES 
      basic/*.f90
)

file(GLOB DECL_FILES 
      decl/scalar/*.f90
      decl/arrays/*.f90
      decl/scalar_init/*.f90
      decl/strings/*.f90
      decl/modules/*.f90
      decl/functions/*.f90
      decl/pointer/*.f90
)

file(GLOB EXPR_FILES 
      expr/scalar/*.f90
      expr/arrays/*.f90
      expr/loops/*.f90
      stmts/stop/*.f90
      stmts/reads/*.f90
      stmts/open/*.f90
      stmts/close/*.f90
      stmts/allocate/*.f90
      stmts/comment/*.f90
      stmts/where/*.f90
      stmts/nullify/*.f90
      subroutine/*.f90
)

file(GLOB STMT_FILES
      stmts/assignments/scalar/*.f90
      stmts/assignments/arrays/*.f90
      stmts/prints/scalar/*.f90
      stmts/prints/arrays/*.f90
      stmts/prints/strings/*.f90
      stmts/write/scalar/*.f90
      stmts/write/arrays/*.f90
      stmts/write/strings/*.f90
      stmts/ifstmt/*.f90
      stmts/select/*.f90
      fileio/*.f90
      modules/*.f90
      derived_type/*.f90
)

file(GLOB CONSTPROP_FILES
  constprop/*.f90
)

file(GLOB OUT_TEST_FILES
  output_tests/*.f90
  output_tests/*.f # Fortran 77 tests
)

file(GLOB IN_TEST_FILES
  stmts/reads/*.f90
  input_tests/*.f90
)

file (GLOB EXPR_TEST_FILES
  expr_tests/*.f90
)

file (GLOB CMD_ARG_TEST_FILES
  cmd_arg_tests/*.f90
)

find_program(BASH bash)
function(runTest TEST_FILES TEST_NAME)
foreach(file ${TEST_FILES}) 
    get_filename_component(testname ${file} NAME)
    string( REPLACE ".f90" "" name ${testname})
    add_test(NAME ${name}
             COMMAND ${BASH} ${CMAKE_SOURCE_DIR}/test/test.sh ${CMAKE_BINARY_DIR}/bin/fc ${file} ${testname}
#             CONFIGURATIONS ${TEST_NAME}
             WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test
      )
endforeach(file)
endfunction()

function(runOutputTest TEST_FILES TEST_NAME)
foreach(file ${TEST_FILES}) 
    get_filename_component(testname ${file} NAME)
    string( REPLACE ".f90" "" name ${testname})
    set(name2 "out.${name}")
    add_test(NAME ${name2}
             COMMAND ${BASH} ${CMAKE_SOURCE_DIR}/test/output_test.sh ${CMAKE_BINARY_DIR}/bin/fc ${file} ${testname} ${CMAKE_BINARY_DIR}/bin/compare_out_file
#             CONFIGURATIONS ${TEST_NAME}
             WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test
      )
endforeach(file)
endfunction()

function(runInputTest TEST_FILES TEST_NAME)
foreach(file ${TEST_FILES})
    get_filename_component(testname ${file} NAME)
    string( REPLACE ".f90" "" name ${testname})
    set(name2 "in.${name}")
    add_test(NAME ${name2}
             COMMAND ${BASH} ${CMAKE_SOURCE_DIR}/test/input_test.sh ${CMAKE_BINARY_DIR}/bin/fc ${file} ${testname} ${CMAKE_BINARY_DIR}/bin/compare_out_file
#             CONFIGURATIONS ${TEST_NAME}
             WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test
      )
endforeach(file)
endfunction()

function(runCmdArgsTest TEST_FILES TEST_NAME)
foreach(file ${TEST_FILES})
    get_filename_component(testname ${file} NAME)
    string( REPLACE ".f90" "" name ${testname})
    set(name2 "cmdarg.${name}")
    add_test(NAME ${name2}
             COMMAND ${BASH} ${CMAKE_SOURCE_DIR}/test/cmd_arg_test.sh ${CMAKE_BINARY_DIR}/bin/fc ${file} ${testname} ${CMAKE_BINARY_DIR}/bin/compare_out_file
#             CONFIGURATIONS ${TEST_NAME}
             WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test
      )
endforeach(file)
endfunction()

runTest("${BASIC_FILES}" "basic")
runTest("${DECL_FILES}" "decl")
runTest("${EXPR_FILES}" "expr")
runTest("${STMT_FILES}" "stmts")
runTest("${CONSTPROP_FILES}" "constprop")

runOutputTest("${EXPR_TEST_FILES}" "expr_tests")
runOutputTest("${STMT_FILES}" "out_stmts")
runOutputTest("${OUT_TEST_FILES}" "run_complex")
runInputTest("${IN_TEST_FILES}" "run_input")
runCmdArgsTest("${CMD_ARG_TEST_FILES}" "run_cmdargs")
