add_compile_options(-O3)
add_executable(compare_out_file
    compare_out_file.cpp
)

target_link_libraries(compare_out_file
  LLVMSupport
)
