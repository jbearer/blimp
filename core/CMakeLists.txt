# Find pthread.
find_package(Threads REQUIRED)

# Build the core library.
file(GLOB LIB_BLIMP_SRC "src/*.c")
add_library(blimp SHARED ${LIB_BLIMP_SRC})
target_include_directories(blimp PUBLIC include)
target_link_libraries(blimp Threads::Threads)
target_link_libraries(blimp blimputils)

# Build libblimp API documentation.
if (DOXYGEN_FOUND)
    doxygen_add_docs(libblimp-docs include/blimp.h)
endif()
