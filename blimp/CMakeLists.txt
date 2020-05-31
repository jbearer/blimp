file(GLOB BLIMP_SRC "src/*.c")

add_executable(blimp-interpreter ${BLIMP_SRC})
set_target_properties(blimp-interpreter PROPERTIES OUTPUT_NAME blimp)
target_link_libraries(blimp-interpreter blimp)
target_link_libraries(blimp-interpreter blimpmodule)
target_compile_options(blimp-interpreter PRIVATE -Wall -Wextra -Werror -ansi -std=c99)

find_package(Readline)
if (Readline_FOUND)
    message("-- Found Readline headers: ${Readline_INCLUDE_DIR}")
    message("-- Found Readline libararies: ${Readline_LIBRARY}")
    target_include_directories(
        blimp-interpreter PRIVATE ${Readline_INCLUDE_DIR})
    target_link_libraries(blimp-interpreter ${Readline_LIBRARY})
    target_compile_options(blimp-interpreter PRIVATE -DHAVE_READLINE=1)
else()
    message(WARNING "Unable to find Readline")
endif()
