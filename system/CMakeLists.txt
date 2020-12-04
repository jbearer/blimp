# Module library.
file(GLOB MODULE_SRC "module/*.c")
add_library(blimpmodule ${MODULE_SRC})
target_compile_options(blimpmodule PRIVATE -Wall -Wextra -Werror -ansi -std=c99)
target_include_directories(blimpmodule PUBLIC include)
target_link_libraries(blimpmodule blimp)
target_link_libraries(blimpmodule ${CMAKE_DL_LIBS})

# System libraries.
function(system_lib NAME)
    add_library(${NAME} SHARED src/${NAME}.c src/system.c)
    target_compile_options(${NAME} PRIVATE -Wall -Wextra -Werror -ansi -std=c99)
    target_include_directories(${NAME} PRIVATE src/)
    target_link_libraries(${NAME} blimpmodule)
endfunction()

system_lib(io)
system_lib(debug)
system_lib(types)

# Build API documentation.
if (DOXYGEN_FOUND)
    doxygen_add_docs(system-docs include)
endif()

