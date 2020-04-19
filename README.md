# bl:mp
_A minimalist, block-oriented, imperative language.

## Project Structure
The `bl:mp` project is divided into several sections:

* [docs](docs/README.md)
Documentation on the `bl:mp` language -- what it is, and why it is. This will include an informal description of the language and some brief tutorials on how to use it, as well as a semi-formal semantics, which is considered the "official" `bl:mp` specification.

* [core](core/README.md)
An implementation of the `bl:mp` core language semantics as defined in [the spec](docs/semantics.rkt). This sub-project builds a library `libblimp` which other projects can link to embed and manipulate a `bl:mp` interpreter at a high level (parsing and executing programs) or a low level (directly manipulating interpreter state).

* [prelude](prelude/README.md)
A pure `bl:mp` library which implements some useful convenience features.

* [system](system/README.md)
A plugin library that implements some `bl:mp` primitives that are not part of the core language specification, but which are necessary to write real-world programs, such as I/O functions and interfaces to system libraries. This sub-project builds a `bl:mp` extension which can be loaded into the [interpreter](blimp/README.md).

* [blimp](blimp/README.md)
A complete `bl:mp` interpreter. This sub-project builds a standalone executable which links to [`libblimp`](core/README.md) and uses it to drive an interpreter.

* [test](test/README.md)
A multi-purpose test suite. This sub-project contains a number of `bl:mp` programs which have been annotated to document their expected behavior. These programs can be used to sanity-check the [semantics](docs/semantics.rkt), or to test the [interpreter](blimp/README.md) by comparing against the semantics, the annotations, or both. This sub-project builds a standalone executable which runs the test suite and produces a report.

* editors
Support for `bl:mp` in various editors. Currently only Sublime is supported.

## Building

To build all projects:
```
mkdir bld-debug
cd bld-debug
cmake -DCMAKE_BUILD_TYPE=Debug ..
make
```
