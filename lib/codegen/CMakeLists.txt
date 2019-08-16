add_library(FCCodeGen
    CodeGen.cpp
    CGExpr.cpp
    CGStmt.cpp
    CGASTHelper.cpp
    RuntimeHelper.cpp
    CGDebugInfo.cpp
)

target_link_libraries(FCCodeGen
 FCCommon
 FCParseTree
 LLVMX86CodeGen
 LLVMX86AsmParser
 LLVMX86AsmPrinter
 LLVMCodeGen
 )

install(TARGETS FCCodeGen DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
