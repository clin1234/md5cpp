
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DDEBUG")


# Build rules for md2html command line utility

include_directories("${PROJECT_SOURCE_DIR}/src")
add_executable(md2html cmdline.cpp cmdline.h md2html.cpp)
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
    target_compile_options(md2html PRIVATE -Wall -Wextra -pedantic)
    target_compile_features(md2html PRIVATE cxx_std_20)
endif()
target_link_libraries(md2html md4c-html)

# Install rules

install(
    TARGETS md2html
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
install(FILES "md2html.1" DESTINATION "${CMAKE_INSTALL_MANDIR}/man1")
