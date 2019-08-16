add_library(FCParseTree
    ASTContext.cpp
    ASTPass.cpp
    ParseTree.cpp
    ParseTreeDumper.cpp
    ParseTreeBuilder.cpp
    SymbolTable.cpp
    Type.cpp
)
target_link_libraries(FCParseTree
 FCCommon)

install(TARGETS FCParseTree DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
