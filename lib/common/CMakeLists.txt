add_library(FCCommon 
    Debug.cpp
    Diagnostics.cpp
) 

install(TARGETS FCCommon DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
