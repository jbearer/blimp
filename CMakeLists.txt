cmake_minimum_required(VERSION 3.0.0)

project(bl:mp)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

enable_testing()
add_subdirectory(core)
add_subdirectory(blimp)
add_subdirectory(docs)
add_subdirectory(test)
