# the bl:mp test suite

This sub-project contains a number of `bl:mp` programs which have been annotated to document their expected behavior. These programs can be used to sanity-check the [semantics](../docs/semantics.rkt), or to test the [interpreter](../blimp/README.md) by comparing against the semantics, the annotations, or both. This sub-project builds a standalone executable which runs the test suite and produces a report.

The sub-project contains both the source code for the test runner (located in `src`) and a variety of `bl:mp` test programs which make up the test suite (located in the other subdirectories).

## test files
The `bl:mp` test runner, `blimp-test`, interprets "`bl:mp` test" files, denoted with the `.blt` extension. These files are almost normal `bl:mp` programs. The only difference is that they are designed to run in a patched `bl:mp` interpreter which has two additional primitive methods:

 Receiver |     Message     |   Body   | Description
:--------:|:---------------:|:--------:|---------------------------------------
|`symbol` |    `!expect`    | `symbol` | Causes a test failure if the receiver and the body do not evaluate to the same symbol.
|   _     | `!expect_error` |     _    | Causes a test failure unless the body of the receiver (which must be a block) fails to evaluate.

The semantics of this extended `bl:mp` language are documented in `test-semantics.rkt`, which extends the `bl:mp-machine` semantics from `docs/semantics.rkt` to interpret these two additional primitives.

## test suites
A `bl:mp` test suite is a hierarchical division of test files. The hierarchy has three levels:
* suite: a collection of groups
* group: a collection of individual tests. The `blimp-test` program automatically creates a group for each subdirectory of this project which contains at least one `.blt` file.
* test: a single test, consisting of a `.blt` file which can be checked against the Racket semantics as well as the `bl:mp` interpreter.

The `blimp-test` runner can automatically discover tests in immediate subdirectories of this project. Any subdirectory which contains at least one `.blt` file will become a test group, and all `.blt` files in that directory will become tests.

## test options
The `blimp-test` runner has a number of command line arguments which can be used to control how the tests are executed. For example, `--filter` controls which tests are run, `--skip-racket` controls whether to run the tests through the Racket semantic model, and so on. Use `blimp-test --help` for a complete list.

Individual tests can also override the options passed via the command line using a special pragma: if the first line of a `.blt` file starts with `#:`, the test runner will interpret the remainder of that line as options which should override the global options just for that test. For example, to disable the Racket semantics for a particular test, add the line `#: --skip-racket` at the start of the test file. Or, to disable a test, you can add a filter that you know does not match the name of the test. For example: `#: -f"skipped: pending feature XYZ"`.
