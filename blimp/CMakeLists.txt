file(GLOB BLIMP_SRC "src/*.c")

add_library(blimprepl src/command.c src/debug.c src/readline.c src/interrupt.c)
target_include_directories(blimprepl PUBLIC src)
target_link_libraries(blimprepl blimp)

add_executable(blimp-interpreter ${BLIMP_SRC})
set_target_properties(blimp-interpreter PROPERTIES OUTPUT_NAME blimp)
target_link_libraries(blimp-interpreter blimp)
target_link_libraries(blimp-interpreter blimpmodule)
target_compile_options(blimp-interpreter PRIVATE -no-pie)
target_compile_options(blimp-interpreter PRIVATE -DEXTENSIONS_PATH="${CMAKE_BINARY_DIR}")
target_compile_options(blimp-interpreter PRIVATE -DPRELUDE_PATH="${CMAKE_SOURCE_DIR}/prelude")

find_package(Readline)
if (Readline_FOUND)
    message("-- Found Readline headers: ${Readline_INCLUDE_DIR}")
    message("-- Found Readline libararies: ${Readline_LIBRARY}")
    target_include_directories(
        blimprepl PRIVATE ${Readline_INCLUDE_DIR})
    target_include_directories(
        blimp-interpreter PRIVATE ${Readline_INCLUDE_DIR})
    target_link_libraries(blimprepl ${Readline_LIBRARY})
    target_link_libraries(blimp-interpreter ${Readline_LIBRARY})
    target_compile_options(blimprepl PRIVATE -DHAVE_READLINE=1)
    target_compile_options(blimp-interpreter PRIVATE -DHAVE_READLINE=1)
else()
    message(WARNING "Unable to find Readline")
endif()
