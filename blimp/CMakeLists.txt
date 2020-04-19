add_executable(blimp-interpreter src/blimp.c)
set_target_properties(blimp-interpreter PROPERTIES OUTPUT_NAME blimp)
target_link_libraries(blimp-interpreter blimp)
target_compile_options(blimp-interpreter PRIVATE -Wall -Wextra -Werror -ansi -std=c99)
