file(GLOB LIB_BLIMP_UTILS_SRC "src/*.c")
add_library(blimputils STATIC ${LIB_BLIMP_UTILS_SRC})
target_include_directories(blimputils PUBLIC include)
target_include_directories(blimputils PRIVATE ../core/include)
target_compile_options(blimputils PRIVATE -fPIC)
target_link_libraries(blimputils m)
