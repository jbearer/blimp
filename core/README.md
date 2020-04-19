# bl:mp core

An implementation of the `bl:mp` core language semantics as defined in [the spec](docs/semantics.rkt). This sub-project builds a library `libblimp` which other projects can link to embed and manipulate a `bl:mp` interpreter at a high level (parsing and executing programs) or a low level (directly manipulating interpreter state).

## Structure

Headers are in `include/` and source files are in `src/`. 

The header `include/blimp.h` defines the public API for interacting with `bl:mp`. It is the only header meant to be included in applications that link against `libblimp`. As such, every declaration in that file is intended as part of the public API, unless otherwise documented, and every name introduced into the global namespace is qualified with `Blimp` in some way to avoid naming conflicts.

The headers in `include/internal/` define an extended API which is only to be used by the implementation of `libblimp` (the files in `src/`). These headers serve several purposes:
* They define structs which were left opaque in `blimp.h`
* They declare function prototypes and macros for additional, private interfaces
* They declare shorter names for many of the common Blimp types and functions. For example, `typedef BlimpStatus Status`.

These headers are included in the `src/` files and from each other. They should not be included from anywhere else, especially not from any public header in `include/` or any header included from a public header in `include/`. In addition, to avoid naming conflicts, all non-static functions defined in `src/` must be qualified with `Blimp` in a manner similar to the public functions, even if the functions in question are not declared in `blimp.h`.

## Building

The library `libblimp` will be built automatically by the top level `CMakeLists.txt`. API documentation is not built automatically, but can be built manually by building the target `libblimp-docs`, as long as Doxygen is installed.
