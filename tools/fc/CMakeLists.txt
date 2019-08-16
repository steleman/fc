add_executable(fc
    fc.cpp
)

target_link_libraries(fc
    FCCommon
    FCLex
    FCParse
    FCParseTree
    FCSema
    FCCodeGen
    LLVMCore
    LLVMSupport
    LLVMBitWriter
    LLVMipo
    LLVMCodeGen
    LLVMMC
    LLVMX86Desc
    LLVMTarget
    LLVMX86Info
    LLVMAnalysis
)
install(TARGETS fc DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
