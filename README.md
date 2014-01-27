Scala implementation of incremental lambda calculus
===================================================

This code can be compiled using SBT. Development should follow the style guide
available at http://docs.scala-lang.org/style/.


Important files
===============

* `src/main/scala/ilc/examples/MapReduce.scala` is the MapReduce
  library in the object language. `lambda` constructs
  object-level lambda-abstractions, `!` constructs
  left-associative applications. `TermBuilder` and
  `PolymorphicTerm` are boilerplates to reduce type
  annotation.

* `src/main/scala/ilc/examples/HistogramExample.scala` is the
  Histogram program written in terms of MapReduce.

* `src/main/scala/ilc/feature/base/Derivation.scala` contains the
  trait `Derivation`, the superclass of all erased change
  structures. The erased change operators are `updateTerm` and
  `diffTerm`, and the derivatives of primitives are to hide in
  the open method `derive`.

* `src/test/scala/ilc/examples/bench/HistogramBenchmark.scala`
  contains benchmark and verification classes of the case study
  and clues about input and change generation.
