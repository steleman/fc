find_program(PYTHON python3)

# Usage of fort-test-gen script: run.py <reference-fc> <testing-fc> <diff-too> <failed-dir>
add_custom_target(test-gen
  COMMAND ${PYTHON} ${CMAKE_CURRENT_SOURCE_DIR}/run.py
    gfortran ${CMAKE_BINARY_DIR}/bin/fc ${CMAKE_BINARY_DIR}/bin/compare_out_file
    ${CMAKE_BINARY_DIR}/failed

  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
