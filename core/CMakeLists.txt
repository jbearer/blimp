# Find pthread.
find_package(Threads REQUIRED)

# Build the core library.
file(GLOB LIB_BLIMP_SRC "src/*.c")
add_library(blimp SHARED ${LIB_BLIMP_SRC})
target_include_directories(blimp PUBLIC include)
target_compile_options(blimp PRIVATE -Wall -Wextra -Werror -ansi -std=c99)
target_link_libraries(blimp Threads::Threads)

# Build libblimp API documentation.
find_package(Doxygen)
if (DOXYGEN_FOUND)
    set(DOXYGEN_PREDEFINED "DOXYGEN=1")
    doxygen_add_docs(libblimp-docs include/blimp.h)
endif()
