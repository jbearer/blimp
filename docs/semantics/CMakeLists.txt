find_program(COQC coqc)
find_program(COQDOC coqdoc)
if (COQC AND COQDOC)
    message("-- Found coqc: ${COQC}")
    message("-- Found coqdoc: ${COQDOC}")

    set(COQ_RECORD_UPDATE_SRC ${CMAKE_SOURCE_DIR}/submodules/coq-record-update/src)
    set(COQ_RECORD_UPDATE_BIN ${CMAKE_CURRENT_BINARY_DIR}/coq-record-update)
    set(COQ_FLAGS -R ${COQ_RECORD_UPDATE_BIN} RecordUpdate -R ${CMAKE_CURRENT_BINARY_DIR}/Blimp Blimp)

    add_custom_command(OUTPUT ${COQ_RECORD_UPDATE_BIN}
        COMMAND ${CMAKE_COMMAND} -E make_directory coq-record-update
        COMMAND ${COQC} ${COQ_FLAGS} -o ${COQ_RECORD_UPDATE_BIN}/RecordSet.vo ${COQ_RECORD_UPDATE_SRC}/RecordSet.v
        COMMAND ${COQC} ${COQ_FLAGS} -o ${COQ_RECORD_UPDATE_BIN}/RecordUpdate.vo ${COQ_RECORD_UPDATE_SRC}/RecordUpdate.v
        WORKING_DIRECTORY .
        DEPENDS ${COQ_RECORD_UPDATE_SRC}/RecordSet.v ${COQ_RECORD_UPDATE_SRC}/RecordUpdate.v
    )
    add_custom_target(coq-record-update ALL DEPENDS ${COQ_RECORD_UPDATE_BIN})

    function(add_semantics_module FILE)
        cmake_parse_arguments(PARSE_ARGV 1 "" "" "" "DEPENDS")
        add_custom_command(OUTPUT ${FILE}.v ${FILE}.vo ${FILE}.glob
            BYPRODUCTS ${FILE}.vok ${FILE}.vos
            COMMAND ${CMAKE_COMMAND} -E make_directory Blimp
            COMMAND ${COQC} ${COQ_FLAGS} -o ${FILE}.vo ${CMAKE_CURRENT_SOURCE_DIR}/${FILE}.v
            # Copy the source file into the build directory next to the globalization file, so
            # coqdoc knows where to find the glob file.
            COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${FILE}.v ${FILE}.v
            WORKING_DIRECTORY .
            MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${FILE}.v
            DEPENDS ${_DEPENDS}
        )
    endfunction()

    add_semantics_module(Blimp/Notation DEPENDS coq-record-update)
    add_semantics_module(Blimp/Nat)
    add_semantics_module(Blimp/Semantics DEPENDS coq-record-update Blimp/Notation.vo)
    add_semantics_module(Blimp/Properties DEPENDS coq-record-update Blimp/Nat.vo Blimp/Notation.vo Blimp/Semantics.vo)
    add_semantics_module(Blimp/Examples DEPENDS coq-record-update Blimp/Notation.vo Blimp/Semantics.vo Blimp/Properties.vo)
    add_semantics_module(Blimp DEPENDS Blimp/Semantics.vo Blimp/Properties.vo Blimp/Examples.vo)

    add_custom_command(OUTPUT Blimp.html Blimp.Nat.html Blimp.Notation.html Blimp.Semantics.html Blimp.Properties.html Blimp.Examples.html index.html coqdoc.css
        COMMAND ${COQDOC} ${COQ_FLAGS} Blimp.v Blimp/Nat.v Blimp/Notation.v Blimp/Semantics.v Blimp/Properties.v Blimp/Examples.v
        WORKING_DIRECTORY .
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/Blimp/Nat.v Blimp/Nat.glob
                ${CMAKE_CURRENT_SOURCE_DIR}/Blimp/Notation.v Blimp/Notation.glob
                ${CMAKE_CURRENT_SOURCE_DIR}/Blimp/Semantics.v Blimp/Semantics.glob
                ${CMAKE_CURRENT_SOURCE_DIR}/Blimp/Properties.v Blimp/Properties.glob
                ${CMAKE_CURRENT_SOURCE_DIR}/Blimp/Examples.v Blimp/Examples.glob
                ${CMAKE_CURRENT_SOURCE_DIR}/Blimp.v Blimp.glob
    )

    add_custom_target(semantics-docs DEPENDS index.html)
    add_custom_target(semantics ALL DEPENDS Blimp/Semantics.vo Blimp/Properties.vo Blimp/Examples.vo semantics-docs)

    find_program(COQIDE coqide)
    if (COQIDE)
        message("-- Found coqide: ${COQIDE}")
        add_custom_target(semantics-ide
            COMMAND ${COQIDE} ${COQ_FLAGS}
                ${CMAKE_CURRENT_SOURCE_DIR}/Blimp/Nat.v
                ${CMAKE_CURRENT_SOURCE_DIR}/Blimp/Notation.v
                ${CMAKE_CURRENT_SOURCE_DIR}/Blimp/Semantics.v
                ${CMAKE_CURRENT_SOURCE_DIR}/Blimp/Properties.v
                ${CMAKE_CURRENT_SOURCE_DIR}/Blimp/Examples.v
        )
    endif()
endif()
