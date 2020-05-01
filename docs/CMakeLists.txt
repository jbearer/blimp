find_program(RACO raco)
if (RACO)
    message("-- Found raco: ${RACO}")
    add_test(
        NAME test-semantics
        COMMAND ${RACO} test semantics-test.rkt
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
else()
    message(WARNING "Unable to find raco: documentation tests will not run")
endif()
