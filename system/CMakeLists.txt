# Module libarary.
file(GLOB MODULE_SRC "module/*.c")
add_library(blimpmodule ${MODULE_SRC})
target_compile_options(blimpmodule PRIVATE -Wall -Wextra -Werror -ansi -std=c99)
target_include_directories(blimpmodule PUBLIC include)
target_link_libraries(blimpmodule blimp)
target_link_libraries(blimpmodule ${CMAKE_DL_LIBS})


# system.io
add_library(io SHARED src/io.c)
target_compile_options(io PRIVATE -Wall -Wextra -Werror -ansi -std=c99)
target_link_libraries(io blimpmodule)

# Build API documentation.
if (DOXYGEN_FOUND)
    doxygen_add_docs(system-docs include)
endif()

