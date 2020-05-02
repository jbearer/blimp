file(GLOB TEST_BLIMP_SRC "src/*.c")
add_executable(blimp-test ${TEST_BLIMP_SRC})
target_compile_options(blimp-test PRIVATE
    -Wall -Wextra -Werror -std=c99
    -DSEMANTICS_PATH="${CMAKE_CURRENT_SOURCE_DIR}/src/semantics.rkt"
    -DTEST_DIRECTORY="${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(blimp-test PRIVATE src)
target_link_libraries(blimp-test blimp)

find_program(RACKET racket)

set(BLIMP_TEST_FLAGS --verbose=test)

if (RACKET)
    message("-- Found Racket: ${RACKET}")
else()
    message(WARNING "Unable to find Racket: semantics tests will not run")
    set(BLIMP_TEST_FLAGS ${BLIMP_TEST_FLAGS} --skip-racket)
endif()

add_test(
    NAME test-blimp
    COMMAND blimp-test ${BLIMP_TEST_FLAGS}
)
