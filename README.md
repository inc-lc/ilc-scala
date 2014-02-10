Scala implementation of incremental lambda calculus
===================================================

This code can be compiled using SBT. Development should follow the style guide
available at http://docs.scala-lang.org/style/.

How to run the evaluation from the paper
========================================

Start sbt **inside this folder** with
```
$ sbt
```
and give the command
```
test-only ilc.examples.bench.BenchSuite
```

This will fetch any needed dependencies, compile everything and start running
tests. Warning: The test execution will proceed for quite a while (around 10
minutes here, more on slower machines) without producing any output, as if
it had hang. Please be patient; you can observe the progress using the
pre-installed JVisualVM.

Running benchmarks with bigger datasets will require more time and (no less
than) 4G of RAM - because of this, it is better done on a different (64bit)
host.

You need to replace
```
  val value = true
```
with
```
  val value = false
```
in `scala-prototype/src/test/scala/ilc/examples/bench/FastBenchmarksFlag.scala`
(this will affect the actual parameters set in
`scala-prototype/src/test/scala/ilc/examples/bench/HistogramBenchmark.scala` and
`scala-prototype/src/test/scala/ilc/examples/ExampleToBenchmark.scala`).

Cross reference
===============


#### Object language syntax

* [src/test/scala/ilc/howTo/WriteLambdaTerms.scala](src/test/scala/ilc/howTo/WriteLambdaTerms.scala)

  An executable guide to the embedding of simply typed lambda terms in Scala.
  With current directory in `scala-prototype`, the following command in `sbt`
  prompt would execute all statements in the body of the class `WriteLambdaTerms`:

      test-only ilc.howTo.WriteLambdaTerms


#### Case study

* [src/main/scala/ilc/examples/MapReduce.scala](src/main/scala/ilc/examples/MapReduce.scala)

  The MapReduce
  library in the object language. `lambda` constructs
  object-level lambda-abstractions, `!` constructs
  left-associative applications. `TermBuilder` and
  `PolymorphicTerm` are boilerplates to reduce type
  annotation.

* [src/main/scala/ilc/examples/HistogramExample.scala](src/main/scala/ilc/examples/HistogramExample.scala)

  The Histogram program written in terms of MapReduce.

* [src/main/scala/ilc/feature/base/Derivation.scala](src/main/scala/ilc/feature/base/Derivation.scala)

  The
  trait `Derivation`, the superclass of all erased change
  structures. The erased change operators are `updateTerm` and
  `diffTerm`, and the derivatives of primitives are to hide in
  the open method `derive`.

* [src/test/scala/ilc/examples/bench/HistogramBenchmark.scala](src/test/scala/ilc/examples/bench/HistogramBenchmark.scala)

  Benchmark and verification classes of the case study,
  with clues about input and change generation.


#### Figure 5

* [src/main/scala/ilc/examples/HistogramExample.scala](src/main/scala/ilc/examples/HistogramExample.scala)

  Definitions of term `histogram` (here `program`), `histogramMap` (here `userMap`) and `histogramReduce` (here `userReduce`).

* [src/main/scala/ilc/feature/integers/Syntax.scala](src/main/scala/ilc/feature/integers/Syntax.scala)

  The term `additiveGroupOnIntegers` is defined here in the trait `SyntaxSugar`.


* [src/main/scala/ilc/examples/MapReduce.scala](src/main/scala/ilc/examples/MapReduce.scala)

  Definitions of `mapReduce`, `mapPerKey`, `groupByKey` and `reducePerKey`



#### Figure 6

* [src/main/scala/ilc/feature/abelianGroups/Library.scala](src/main/scala/ilc/feature/abelianGroups/Library.scala)

  Interface `Group`, with fields `merge`, `inverse`, `zero`
  (here `AbelianGroup` with `binOp`, `inv`, `neutral`).
  Method types are a bit different to implement lazy evaluation in Scala (§4.3).

* [src/main/scala/ilc/feature/bags/Library.scala](src/main/scala/ilc/feature/bags/Library.scala)

  Interface `Bag` with primitives `groupOnBags` (here `case class FreeAbelianGroup`) and
  `foldBag` (here `bagFoldGroup`).

* [src/main/scala/ilc/feature/abelianMaps/Library.scala](src/main/scala/ilc/feature/abelianMaps/Library.scala)

  Interface `Map` with primitives `groupOnMaps` (here `liftGroup`) and `foldMap` (here `foldByHom`).
