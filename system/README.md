# `system`
A plugin library that implements some `bl:mp` primitives that are not part of the core language specification, but which are necessary to write real-world programs, such as I/O functions and interfaces to system libraries. This sub-project builds several libraries:
* `libblimpmodule`: a static library which implements some module functionality, such as importing `bl:mp` libraries and extensions. It is included in thes `system` project because it is technically an extension to the core language, but it is built as a separate library because while `libblimpsys` can be loaded dynamically once you have `libblimpmodule`, `libblimpmodule` is necessary to do anything at all.
* `libio`: a `bl:mp` extension which can be loaded into the [interpreter](blimp/README.md) using `import system.io`.
* `libio`: a `bl:mp` extension which provides debugging facilities.
