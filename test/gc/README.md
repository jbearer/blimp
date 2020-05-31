# Garbage Collection Tests and Benchmarks

This directory contains tests for `bl:mp`'s three forms of garbage collection:
* Reference counting (`refcount_*.blt`)
* Enhanced reference counting (`erc_*.blt`)
* Tracing garbage collection (`tracing_*.blt`)

In addition to checking that the various forms of garbage collection actually clean up the types of garbage that they are supposed to be able to clean up, the tests are also annotated with `--blimp-timeout`s, so they serve as performance benchmarks. For this reason, the reference counting and ERC tests are duplicated as tracing tests as well (using `-fno-gc-refcount`, `-fno-gc-cycle-detection`, and `-fno-gc-tracing` to control which form of garbage collection is used in each version of the test) so that we can compare the performance of the tracing collector to that of reference counting and ERC.

Note that we don't care too much about comparing the performance of vanilla reference counting to ERC: there isn't really an interesting tradeoff there, because each algorithm can collect kinds of garbage that the other can't; they are complementary. Tracing can collect everything that either reference counting or ERC can collect, so we really need to check that each form of reference counting is faster than tracing on the relevant types of garbage, otherwise there's no point in supporting the added complexity of the reference counting algorithms.
