add_test(
    NAME test-semantics
    COMMAND racket semantics-test.rkt
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
