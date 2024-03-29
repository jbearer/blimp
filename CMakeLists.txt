cmake_minimum_required(VERSION 3.0.0)
cmake_policy(SET CMP0054 NEW)

project(bl:mp)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

find_package(Doxygen)
if (DOXYGEN_FOUND)
    set(DOXYGEN_PREDEFINED "DOXYGEN=1")
endif()

add_compile_options(-Wall -Wextra -Werror -ansi -std=c11)

# Platform specific options
if (USE_POSIX08)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_POSIX_C_SOURCE=200809L")
endif()

enable_testing()
add_subdirectory(utils)
add_subdirectory(core)
add_subdirectory(system)
add_subdirectory(blimp)
add_subdirectory(docs)
add_subdirectory(test)
