add_library(FC SHARED
    fc_runtime.c
)

set_source_files_properties(fc_runtime.c PROPERTIES COMPILE_FLAGS "-O2 -Wno-unused-result")

install(TARGETS FC DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
