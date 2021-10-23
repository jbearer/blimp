file(GLOB TEST_BLIMP_SRC "src/*.c")
add_executable(blimp-test ${TEST_BLIMP_SRC})
target_compile_options(blimp-test PRIVATE
    -Wall -Wextra -Werror -std=c99
    -DSEMANTICS_PATH="${CMAKE_CURRENT_SOURCE_DIR}/src/semantics.rkt"
    -DTEST_DIRECTORY="${CMAKE_CURRENT_SOURCE_DIR}"
    -DEXTENSIONS_PATH="${CMAKE_BINARY_DIR}/"
    -DPRELUDE_PATH="${CMAKE_SOURCE_DIR}/prelude")
target_include_directories(blimp-test PRIVATE src)
target_link_libraries(blimp-test blimp)
target_link_libraries(blimp-test blimpmodule)
target_link_libraries(blimp-test blimprepl)
target_link_libraries(blimp-test m)

# Binary modules for system tests
function(test_module NAME)
    add_library(${NAME} SHARED system/${NAME}.c)
    target_compile_options(${NAME} PRIVATE -Wall -Wextra -Werror -ansi -std=c99)
    target_link_libraries(${NAME} blimpmodule)
endfunction()
test_module(binary_module)
test_module(ambiguous_module)
test_module(invalid_extension)

if (NOT TEST_PERF_FACTOR)
    set(TEST_PERF_FACTOR 1)
endif()
target_compile_options(blimp-test PRIVATE -DDEFAULT_PERF_FACTOR=${TEST_PERF_FACTOR})

if(COVERAGE AND CMAKE_COMPILER_IS_GNUCXX)
    include(CodeCoverage)
    append_coverage_compiler_flags()
    setup_target_for_coverage_lcov(
        NAME blimp-coverage
        EXECUTABLE test/blimp-test --disable-racket
        DEPENDENCIES blimp-test
        BASE_DIRECTORY ${PROJECT_BINARY_DIR}
        EXCLUDE CMakeCCompilerID.c
    )
endif()

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
