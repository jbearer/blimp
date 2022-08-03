# Documentation for the bl:mp language

## Semantics

[semantics](semantics) contains a formal specification of bl:mp semantics, written in Coq. When working
with this library, you will need to have `coqc`, `coqdoc`, and `coqide` installed. Once installed,
you can run `make semantics` to check all of the proofs and produce a compiled Coq library in
`$BUILD_DIR/docs/semantics`, as well as a formatted, annotated semantics document in HTML form (in
`$BUILD_DIR/docs/semantics/Blimp.html`). The pseudo-target `make semantics-ide` will open the Coq
source files in `coqide` with all of the necessary dependencies loaded.
