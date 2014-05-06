Scala implementation of incremental lambda calculus
===================================================

This code can be compiled using [SBT](http://www.scala-sbt.org/). Development
should follow the style guide available at http://docs.scala-lang.org/style/.

How to load inside Eclipse
==========================

Run `sbt eclipse` to generate the project files. This will download all required
libraries, including their sources for easier navigation.

Customizing the Eclipse project (advanced)
------------------------------------------

The generated output hardcodes paths on the specific machines, so try to not
commit the files. If you need to change the project, try to do so by changing
the build definition itself, including the sbteclipse setup
[as documented by them](https://github.com/typesafehub/sbteclipse/wiki/Using-sbteclipse).

How to run the evaluation from the paper
========================================

Start sbt **inside this folder** with
```
$ sbt
```
and give the commands
```
project clients
test-only longRunning.BenchSuite
```

This will fetch any needed dependencies, compile everything and start running
tests. See [below](#example-test-output) for some example test output. Moreover,
this will generate raw datasets in CSV format under the `testOutput` subfolder,
with extension DSV. See the README of the 'graphs' artifacts for details on the
content.

Warning: The test execution will proceed for quite a while (around 10
minutes here, more on slower machines) without producing any output, as if it
had hang, before finally outputting the test results. This happens **before** the line:

```
[info] - ilc.examples.HistogramGenerated (derivative, surgical change).Test-0 measurements:
```

Please be patient; you can observe the progress using the VisualVM
profiling/monitoring tool (inside the virtual machine, this is pre-installed ---
see icon on the left-hand side of the screen). Warning: VisualVM can do
profiling, so it can invalidate the performance results of the benchmarks themselves.

Running benchmarks with bigger datasets will require more time and (no less
than) 4G of RAM - because of this, it is better done on a different (64bit)
host. To increase the dataset size, you need to replace
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

Example test output
===================

```
> test-only ilc.examples.bench.BenchSuite
[info] Compiling 111 Scala sources to /Users/pgiarrusso/Documents/Research/Sorgenti/ilc/scala-prototype/target/scala-2.10/classes...
[warn] there were 1 inliner warning(s); re-run with -Yinline-warnings for details
[warn] one warning found
[info] Generating examples into:
[info] /Users/pgiarrusso/Documents/Research/Sorgenti/ilc/scala-prototype/target/scala-2.10/src_managed/test
[info] Generating GroupBy
[info] Generating MapSucc
[info] Generating SumValues
[info] Generating Histogram
[info] Generating MapSuccBags
[info] Generating MapSuccBase
[info] Generating BagUnion
[info] - DummyGenerated.scala
[info] - GroupByGenerated.scala
[info] - MapSuccGenerated.scala
[info] - SumValuesGenerated.scala
[info] - HistogramGenerated.scala
[info] - MapSuccBagsGenerated.scala
[info] - MapSuccBaseGenerated.scala
[info] - BagUnionGenerated.scala
[info] - ilc.examples.HistogramGenerated (derivative, surgical change).Test-0 measurements:
[info] ::Benchmark ilc.examples.HistogramGenerated (derivative, surgical change)::
[info] cores: 4
[info] hostname: pc12281.mathematik.uni-marburg.de
[info] jvm-name: Java HotSpot(TM) 64-Bit Server VM
[info] jvm-vendor: Oracle Corporation
[info] jvm-version: 24.51-b03
[info] os-arch: x86_64
[info] os-name: Mac OS X
[info] Parameters(n -> 1, change -> random changes): 0.41252777777777777
[info] Parameters(n -> 2, change -> random changes): 0.4160833333333334
[info] Parameters(n -> 4, change -> random changes): 0.2881666666666666
[info] Parameters(n -> 8, change -> random changes): 0.17052777777777772
[info] Parameters(n -> 16, change -> random changes): 0.12133333333333332
[info] Parameters(n -> 32, change -> random changes): 0.1375833333333333
[info] Parameters(n -> 64, change -> random changes): 0.11105555555555556
[info] Parameters(n -> 128, change -> random changes): 0.11627777777777776
[info] Parameters(n -> 256, change -> random changes): 0.13955555555555554
[info] Parameters(n -> 512, change -> random changes): 0.11416666666666668
[info]
[info] - ilc.examples.HistogramGenerated (recomputation).Test-1 measurements:
[info] ::Benchmark ilc.examples.HistogramGenerated (recomputation)::
[info] cores: 4
[info] hostname: pc12281.mathematik.uni-marburg.de
[info] jvm-name: Java HotSpot(TM) 64-Bit Server VM
[info] jvm-vendor: Oracle Corporation
[info] jvm-version: 24.51-b03
[info] os-arch: x86_64
[info] os-name: Mac OS X
[info] Parameters(n -> 1, change -> random changes): 4.712833333333333
[info] Parameters(n -> 2, change -> random changes): 7.604833333333334
[info] Parameters(n -> 4, change -> random changes): 8.933777777777777
[info] Parameters(n -> 8, change -> random changes): 16.02652777777778
[info] Parameters(n -> 16, change -> random changes): 28.632916666666674
[info] Parameters(n -> 32, change -> random changes): 53.185166666666674
[info] Parameters(n -> 64, change -> random changes): 97.8111111111111
[info] Parameters(n -> 128, change -> random changes): 195.64419444444448
[info] Parameters(n -> 256, change -> random changes): 411.4718888888889
[info] Parameters(n -> 512, change -> random changes): 787.4827222222225
[info]
[info]
[info] :::Summary of regression test results - Accepter():::
[info] Test group: ilc.examples.HistogramGenerated (derivative, surgical change)
[info] - ilc.examples.HistogramGenerated (derivative, surgical change).Test-0 measurements:
[info]
[info] Test group: ilc.examples.HistogramGenerated (recomputation)
[info] - ilc.examples.HistogramGenerated (recomputation).Test-1 measurements:
[info]
[info]  Summary: 2 tests passed, 0 tests failed.
[info] Passed: : Total 2, Failed 0, Errors 0, Passed 2, Skipped 0
[success] Total time: 612 s, completed Feb 10, 2014 7:10:09 PM
```


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
