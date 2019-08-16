add_library(FCSema
    BinaryOpHandler.cpp
    Intrinsics.cpp
    ModFileHandler.cpp
    ModFileDumperPass.cpp
    FormatHandler.cpp
    Sema.cpp
    SymbolResolverPass.cpp
    TypeUpdaterPass.cpp
    UndeclaredErrorPrinter.cpp
    UseStmtHandlerPass.cpp
    ConstPropPass.cpp
    StmtTypeUpdaterPass.cpp
    ExpansionUtils.cpp
    ArraySectionExpander.cpp
    ArraySectionReplacer.cpp
    ConstructExpander.cpp
    ArrBoundsIntrinExpanderPass.cpp
    IntrinsicsExpanderPass.cpp
    ExprTypeUpdaterPass.cpp
    FunctionTypeUpdaterPass.cpp
    ParameterConstPropPass.cpp
    DeclEliminatorPass.cpp
    PartRefSymResolverPass.cpp
)

target_link_libraries(FCSema
 FCCommon
 FCParseTree
)

install(TARGETS FCSema DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
