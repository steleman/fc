add_library(FCLex 
    Lexer.cpp
    TokenKinds.cpp
) 

install(TARGETS FCLex DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)