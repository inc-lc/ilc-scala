Scala implementation of incremental lambda calculus
===================================================

This code allows you to write programs in a small DSL and *automatically incrementalize* them, that is, produce their *derivatives*. A derivative of a program is a function which takes changes to the program input and maps them directly to changes to the program output, without reexecuting the original program. For big inputs and small changes, this can be much can be more efficient than rerunning the program from scratch.

If you are not an academic, beware this is a research prototype, and further research is needed before this is more generally useful.

To understand more what this is all about, see <http://inc-lc.github.io/> and the paper(s) there listed. This work was published at PLDI '14.

Building
========

This code can be compiled using [SBT](http://www.scala-sbt.org/). Development
should follow the style guide available at http://docs.scala-lang.org/style/.

[![Build Status](https://travis-ci.org/inc-lc/ilc-scala.svg?branch=master)](https://travis-ci.org/inc-lc/ilc-scala)

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

How to run the evaluation from the PLDI'14 paper
========================================

For the original version of the evaluation (as submitted to the AEC), switch
to the `pldi14-aec` tag and follow instructions there. For the current version, read on.

Start sbt **inside this folder** with
```
$ sbt
```
and give the commands
```
project bigClients
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


Cross reference
===============


#### Object language syntax

* [src/test/scala/ilc/howTo/WriteLambdaTerms.scala](src/test/scala/ilc/howTo/WriteLambdaTerms.scala)

  An executable guide to the embedding of simply typed lambda terms in Scala.
  With current directory in `scala-prototype`, the following command in `sbt`
  prompt would execute all statements in the body of the class `WriteLambdaTerms`:

      test-only ilc.howTo.WriteLambdaTerms


#### Case study

* [clients/src/main/scala/ilc/examples/MapReduce.scala](clients/src/main/scala/ilc/examples/MapReduce.scala)

  The MapReduce
  library in the object language. `lambda` constructs
  object-level lambda-abstractions, `!` constructs
  left-associative applications. `TermBuilder` and
  `PolymorphicTerm` are boilerplates to reduce type
  annotation.

* [clients/src/main/scala/ilc/examples/HistogramExample.scala](clients/src/main/scala/ilc/examples/HistogramExample.scala)

  The Histogram program written in terms of MapReduce.

* [src/main/scala/ilc/feature/base/Derivation.scala](src/main/scala/ilc/feature/base/Derivation.scala)

  The
  trait `Derivation`, the superclass of all erased change
  structures. The erased change operators are `updateTerm` and
  `diffTerm`, and the derivatives of primitives are to hide in
  the open method `derive`.

* [bigClients/src/test/scala/longRunning/HistogramBenchmark.scala](bigClients/src/test/scala/longRunning/HistogramBenchmark.scala)

  Benchmark and verification classes of the case study,
  with clues about input and change generation.


#### Figure 5

* [clients/src/main/scala/ilc/examples/HistogramExample.scala](clients/src/main/scala/ilc/examples/HistogramExample.scala)

  Definitions of term `histogram` (here `program`), `histogramMap` (here `userMap`) and `histogramReduce` (here `userReduce`).

* [src/main/scala/ilc/feature/integers/Syntax.scala](src/main/scala/ilc/feature/integers/Syntax.scala)

  The term `additiveGroupOnIntegers` is defined here in the trait `SyntaxSugar`.


* [clients/src/main/scala/ilc/examples/MapReduce.scala](clients/src/main/scala/ilc/examples/MapReduce.scala)

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


Example test output
===================

This is an example output from the first run of SBT on a fresh checkout (as of 7 June 2014).
```
$ sbt
[info] Loading global plugins from /Users/pgiarrusso/.sbt/0.13/plugins
[info] Updating {file:/Users/pgiarrusso/.sbt/0.13/plugins/}global-plugins...
[info] Resolving org.fusesource.jansi#jansi;1.4 ...
[info] Done updating.
[info] Loading project definition from /Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/project
[info] Updating {file:/Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/project/}ilc-scala-build...
[info] Resolving org.fusesource.jansi#jansi;1.4 ...
[info] Done updating.
[info] Compiling 1 Scala source to /Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/project/target/scala-2.10/sbt-0.13/classes...
[info] Set current project to ilc (in build file:/Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/)
> project bigClients
[info] Set current project to bigClients (in build file:/Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/)
> test-only longRunning.BenchSuite
[info] Updating {file:/Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/}ilc...
[info] Resolving jline#jline;2.11 ...
[info] Done updating.
[info] Updating {file:/Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/}clients...
[info] Resolving jline#jline;2.11 ...
[info] Done updating.
[info] Updating {file:/Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/}bigClients...
[info] Resolving jline#jline;2.11 ...
[info] Done updating.
[info] Compiling 118 Scala sources to /Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/target/scala-2.11/classes...
[info] Compiling 36 Scala sources to /Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/target/scala-2.11/test-classes...
[warn] /Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/src/test/scala/ilc/examples/ExampleToBenchmark.scala:228: Selecting value benchData from class ExampleToBenchmark, which extends scala.DelayedInit, is likely to yield an uninitialized value
[warn] abstract class ReplacementChangeBenchmark(override val benchData: BenchData with ReplacementChangeData) extends ExampleToBenchmark(benchData) {
[warn]                                                        ^
[warn] one warning found
[info] Compiling 9 Scala sources to /Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/clients/target/scala-2.11/classes...
[info] Generating examples into:
[info] /Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/clients/target/scala-2.11/src_managed/test
[info] Generator started
[info] Generating MapSuccGeneratedProgBase.scala
[info] Generating MapSuccGeneratedUtilBase.scala
[info] Generating MapSuccGeneratedDerivBase.scala
[info] Generating MapSuccGenerated.scala
[info] Generating MapSuccGenerated.txt
[info] Generating BagUnionGeneratedProgBase.scala
[info] Generating BagUnionGeneratedUtilBase.scala
[info] Generating BagUnionGeneratedDerivBase.scala
[info] Generating BagUnionGenerated.scala
[info] Generating BagUnionGenerated.txt
[info] Generating MapSuccBaseGeneratedProgBase.scala
[info] Generating MapSuccBaseGeneratedUtilBase.scala
[info] Generating MapSuccBaseGeneratedDerivBase.scala
[info] Generating MapSuccBaseGenerated.scala
[info] Generating MapSuccBaseGenerated.txt
[info] Generating MapSuccBagsGeneratedProgBase.scala
[info] Generating MapSuccBagsGeneratedUtilBase.scala
[info] Generating MapSuccBagsGeneratedDerivBase.scala
[info] Generating MapSuccBagsGenerated.scala
[info] Generating MapSuccBagsGenerated.txt
[info] Generating SumValuesGeneratedProgBase.scala
[info] Generating SumValuesGeneratedUtilBase.scala
[info] Generating SumValuesGeneratedDerivBase.scala
[info] Generating SumValuesGenerated.scala
[info] Generating SumValuesGenerated.txt
[info] Generating GroupByGeneratedProgBase.scala
[info] Generating GroupByGeneratedUtilBase.scala
[info] Generating GroupByGeneratedDerivBase.scala
[info] Generating GroupByGenerated.scala
[info] Generating GroupByGenerated.txt
[info] - DummyGenerated.scala
[info] - MapSuccGeneratedProgBase.scala
[info] - MapSuccGeneratedUtilBase.scala
[info] - MapSuccGeneratedDerivBase.scala
[info] - MapSuccGenerated.scala
[info] - MapSuccGenerated.txt
[info] - BagUnionGeneratedProgBase.scala
[info] - BagUnionGeneratedUtilBase.scala
[info] - BagUnionGeneratedDerivBase.scala
[info] - BagUnionGenerated.scala
[info] - BagUnionGenerated.txt
[info] - MapSuccBaseGeneratedProgBase.scala
[info] - MapSuccBaseGeneratedUtilBase.scala
[info] - MapSuccBaseGeneratedDerivBase.scala
[info] - MapSuccBaseGenerated.scala
[info] - MapSuccBaseGenerated.txt
[info] - MapSuccBagsGeneratedProgBase.scala
[info] - MapSuccBagsGeneratedUtilBase.scala
[info] - MapSuccBagsGeneratedDerivBase.scala
[info] - MapSuccBagsGenerated.scala
[info] - MapSuccBagsGenerated.txt
[info] - SumValuesGeneratedProgBase.scala
[info] - SumValuesGeneratedUtilBase.scala
[info] - SumValuesGeneratedDerivBase.scala
[info] - SumValuesGenerated.scala
[info] - SumValuesGenerated.txt
[info] - GroupByGeneratedProgBase.scala
[info] - GroupByGeneratedUtilBase.scala
[info] - GroupByGeneratedDerivBase.scala
[info] - GroupByGenerated.scala
[info] - GroupByGenerated.txt
[info] Compiling 44 Scala sources to /Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/clients/target/scala-2.11/test-classes...
[info] Compiling 1 Scala source to /Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/bigClients/target/scala-2.11/classes...
[info] Generating examples into:
[info] /Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/bigClients/target/scala-2.11/src_managed/test
[info] Generator started
[info] Generating HistogramGeneratedProgBase.scala
[info] Generating HistogramGeneratedUtilBase.scala
[info] Generating HistogramGeneratedDerivBase.scala
[info] Generating HistogramGenerated.scala
[info] Generating HistogramGenerated.txt
[info] - DummyGenerated.scala
[info] - HistogramGeneratedProgBase.scala
[info] - HistogramGeneratedUtilBase.scala
[info] - HistogramGeneratedDerivBase.scala
[info] - HistogramGenerated.scala
[info] - HistogramGenerated.txt
[info] Compiling 10 Scala sources to /Users/pgiarrusso/Documents/Research/Sorgenti/ilc-scala/bigClients/target/scala-2.11/test-classes...
[info] - ilc.examples.HistogramGenerated (derivative, surgical change).Test-0 measurements:
[info]   - at n -> 1, change -> random changes: passed
[info]     (mean = 2.24 ms, ci = <2.17 ms, 2.31 ms>, significance = 0.01)
[info]   - at n -> 2, change -> random changes: passed
[info]     (mean = 2.53 ms, ci = <2.34 ms, 2.72 ms>, significance = 0.01)
[info]   - at n -> 4, change -> random changes: passed
[info]     (mean = 2.08 ms, ci = <2.03 ms, 2.12 ms>, significance = 0.01)
[info]   - at n -> 8, change -> random changes: passed
[info]     (mean = 2.47 ms, ci = <2.32 ms, 2.62 ms>, significance = 0.01)
[info]   - at n -> 16, change -> random changes: passed
[info]     (mean = 1.83 ms, ci = <1.81 ms, 1.85 ms>, significance = 0.01)
[info]   - at n -> 32, change -> random changes: passed
[info]     (mean = 2.19 ms, ci = <1.97 ms, 2.41 ms>, significance = 0.01)
[info]   - at n -> 64, change -> random changes: passed
[info]     (mean = 2.16 ms, ci = <2.03 ms, 2.29 ms>, significance = 0.01)
[info]   - at n -> 128, change -> random changes: passed
[info]     (mean = 2.11 ms, ci = <2.08 ms, 2.15 ms>, significance = 0.01)
[info]   - at n -> 256, change -> random changes: passed
[info]     (mean = 2.09 ms, ci = <2.05 ms, 2.13 ms>, significance = 0.01)
[info]   - at n -> 512, change -> random changes: passed
[info]     (mean = 1.96 ms, ci = <1.93 ms, 1.99 ms>, significance = 0.01)
[info] ::Benchmark ilc.examples.HistogramGenerated (derivative, surgical change)::
[info] cores: 4
[info] hostname: BlueVelvet.local
[info] jvm-name: Java HotSpot(TM) 64-Bit Server VM
[info] jvm-vendor: Oracle Corporation
[info] jvm-version: 24.51-b03
[info] os-arch: x86_64
[info] os-name: Mac OS X
[info] Parameters(n -> 1, change -> random changes): 2.036
[info] Parameters(n -> 2, change -> random changes): 1.956
[info] Parameters(n -> 4, change -> random changes): 1.807
[info] Parameters(n -> 8, change -> random changes): 1.864
[info] Parameters(n -> 16, change -> random changes): 1.757
[info] Parameters(n -> 32, change -> random changes): 1.748
[info] Parameters(n -> 64, change -> random changes): 1.874
[info] Parameters(n -> 128, change -> random changes): 1.977
[info] Parameters(n -> 256, change -> random changes): 1.938
[info] Parameters(n -> 512, change -> random changes): 1.851
[info]
[info] - ilc.examples.HistogramGenerated (normalized derivative, surgical change).Test-1 measurements:
[info]   - at n -> 1, change -> random changes: passed
[info]     (mean = 1.63 ms, ci = <1.59 ms, 1.67 ms>, significance = 0.01)
[info]   - at n -> 2, change -> random changes: passed
[info]     (mean = 1.19 ms, ci = <1.18 ms, 1.20 ms>, significance = 0.01)
[info]   - at n -> 4, change -> random changes: passed
[info]     (mean = 1.12 ms, ci = <1.10 ms, 1.13 ms>, significance = 0.01)
[info]   - at n -> 8, change -> random changes: passed
[info]     (mean = 1.11 ms, ci = <1.10 ms, 1.12 ms>, significance = 0.01)
[info]   - at n -> 16, change -> random changes: passed
[info]     (mean = 1.19 ms, ci = <1.18 ms, 1.20 ms>, significance = 0.01)
[info]   - at n -> 32, change -> random changes: passed
[info]     (mean = 1.11 ms, ci = <1.09 ms, 1.12 ms>, significance = 0.01)
[info]   - at n -> 64, change -> random changes: passed
[info]     (mean = 1.48 ms, ci = <1.36 ms, 1.61 ms>, significance = 0.01)
[info]   - at n -> 128, change -> random changes: passed
[info]     (mean = 1.22 ms, ci = <1.20 ms, 1.24 ms>, significance = 0.01)
[info]   - at n -> 256, change -> random changes: passed
[info]     (mean = 1.14 ms, ci = <1.13 ms, 1.16 ms>, significance = 0.01)
[info]   - at n -> 512, change -> random changes: passed
[info]     (mean = 1.31 ms, ci = <1.21 ms, 1.42 ms>, significance = 0.01)
[info] ::Benchmark ilc.examples.HistogramGenerated (normalized derivative, surgical change)::
[info] cores: 4
[info] hostname: BlueVelvet.local
[info] jvm-name: Java HotSpot(TM) 64-Bit Server VM
[info] jvm-vendor: Oracle Corporation
[info] jvm-version: 24.51-b03
[info] os-arch: x86_64
[info] os-name: Mac OS X
[info] Parameters(n -> 1, change -> random changes): 1.481
[info] Parameters(n -> 2, change -> random changes): 1.167
[info] Parameters(n -> 4, change -> random changes): 1.09
[info] Parameters(n -> 8, change -> random changes): 1.087
[info] Parameters(n -> 16, change -> random changes): 1.167
[info] Parameters(n -> 32, change -> random changes): 1.086
[info] Parameters(n -> 64, change -> random changes): 1.206
[info] Parameters(n -> 128, change -> random changes): 1.197
[info] Parameters(n -> 256, change -> random changes): 1.07
[info] Parameters(n -> 512, change -> random changes): 1.06
[info]
[info] - ilc.examples.HistogramGenerated (recomputation).Test-2 measurements:
[info]   - at n -> 1, change -> random changes: passed
[info]     (mean = 2.89 ms, ci = <2.87 ms, 2.91 ms>, significance = 0.01)
[info]   - at n -> 2, change -> random changes: passed
[info]     (mean = 6.65 ms, ci = <5.98 ms, 7.33 ms>, significance = 0.01)
[info]   - at n -> 4, change -> random changes: passed
[info]     (mean = 11.78 ms, ci = <10.32 ms, 13.24 ms>, significance = 0.01)
[info]   - at n -> 8, change -> random changes: passed
[info]     (mean = 20.53 ms, ci = <17.75 ms, 23.32 ms>, significance = 0.01)
[info]   - at n -> 16, change -> random changes: passed
[info]     (mean = 28.56 ms, ci = <28.04 ms, 29.09 ms>, significance = 0.01)
[info]   - at n -> 32, change -> random changes: passed
[info]     (mean = 61.04 ms, ci = <58.20 ms, 63.88 ms>, significance = 0.01)
[info]   - at n -> 64, change -> random changes: passed
[info]     (mean = 98.74 ms, ci = <97.69 ms, 99.78 ms>, significance = 0.01)
[info]   - at n -> 128, change -> random changes: passed
[info]     (mean = 194.67 ms, ci = <193.49 ms, 195.86 ms>, significance = 0.01)
[info]   - at n -> 256, change -> random changes: passed
[info]     (mean = 410.63 ms, ci = <399.49 ms, 421.78 ms>, significance = 0.01)
[info]   - at n -> 512, change -> random changes: passed
[info]     (mean = 828.06 ms, ci = <805.02 ms, 851.09 ms>, significance = 0.01)
[info] ::Benchmark ilc.examples.HistogramGenerated (recomputation)::
[info] cores: 4
[info] hostname: BlueVelvet.local
[info] jvm-name: Java HotSpot(TM) 64-Bit Server VM
[info] jvm-vendor: Oracle Corporation
[info] jvm-version: 24.51-b03
[info] os-arch: x86_64
[info] os-name: Mac OS X
[info] Parameters(n -> 1, change -> random changes): 2.786
[info] Parameters(n -> 2, change -> random changes): 5.012
[info] Parameters(n -> 4, change -> random changes): 8.423
[info] Parameters(n -> 8, change -> random changes): 14.741
[info] Parameters(n -> 16, change -> random changes): 27.234
[info] Parameters(n -> 32, change -> random changes): 51.907
[info] Parameters(n -> 64, change -> random changes): 97.071
[info] Parameters(n -> 128, change -> random changes): 191.751
[info] Parameters(n -> 256, change -> random changes): 380.414
[info] Parameters(n -> 512, change -> random changes): 760.621
[info]
[info]
[info] :::Summary of regression test results - Accepter():::
[info] Test group: ilc.examples.HistogramGenerated (derivative, surgical change)
[info] - ilc.examples.HistogramGenerated (derivative, surgical change).Test-0 measurements:
[info]   - at n -> 1, change -> random changes: passed
[info]     (mean = 2.24 ms, ci = <2.06 ms, 2.41 ms>, significance = 1.0E-10)
[info]   - at n -> 2, change -> random changes: passed
[info]     (mean = 2.53 ms, ci = <2.05 ms, 3.00 ms>, significance = 1.0E-10)
[info]   - at n -> 4, change -> random changes: passed
[info]     (mean = 2.08 ms, ci = <1.96 ms, 2.20 ms>, significance = 1.0E-10)
[info]   - at n -> 8, change -> random changes: passed
[info]     (mean = 2.47 ms, ci = <2.10 ms, 2.85 ms>, significance = 1.0E-10)
[info]   - at n -> 16, change -> random changes: passed
[info]     (mean = 1.83 ms, ci = <1.77 ms, 1.89 ms>, significance = 1.0E-10)
[info]   - at n -> 32, change -> random changes: passed
[info]     (mean = 2.19 ms, ci = <1.65 ms, 2.73 ms>, significance = 1.0E-10)
[info]   - at n -> 64, change -> random changes: passed
[info]     (mean = 2.16 ms, ci = <1.84 ms, 2.48 ms>, significance = 1.0E-10)
[info]   - at n -> 128, change -> random changes: passed
[info]     (mean = 2.11 ms, ci = <2.02 ms, 2.21 ms>, significance = 1.0E-10)
[info]   - at n -> 256, change -> random changes: passed
[info]     (mean = 2.09 ms, ci = <1.99 ms, 2.19 ms>, significance = 1.0E-10)
[info]   - at n -> 512, change -> random changes: passed
[info]     (mean = 1.96 ms, ci = <1.88 ms, 2.04 ms>, significance = 1.0E-10)
[info]
[info] Test group: ilc.examples.HistogramGenerated (normalized derivative, surgical change)
[info] - ilc.examples.HistogramGenerated (normalized derivative, surgical change).Test-1 measurements:
[info]   - at n -> 1, change -> random changes: passed
[info]     (mean = 1.63 ms, ci = <1.53 ms, 1.74 ms>, significance = 1.0E-10)
[info]   - at n -> 2, change -> random changes: passed
[info]     (mean = 1.19 ms, ci = <1.16 ms, 1.22 ms>, significance = 1.0E-10)
[info]   - at n -> 4, change -> random changes: passed
[info]     (mean = 1.12 ms, ci = <1.08 ms, 1.15 ms>, significance = 1.0E-10)
[info]   - at n -> 8, change -> random changes: passed
[info]     (mean = 1.11 ms, ci = <1.09 ms, 1.14 ms>, significance = 1.0E-10)
[info]   - at n -> 16, change -> random changes: passed
[info]     (mean = 1.19 ms, ci = <1.17 ms, 1.21 ms>, significance = 1.0E-10)
[info]   - at n -> 32, change -> random changes: passed
[info]     (mean = 1.11 ms, ci = <1.07 ms, 1.15 ms>, significance = 1.0E-10)
[info]   - at n -> 64, change -> random changes: passed
[info]     (mean = 1.48 ms, ci = <1.17 ms, 1.80 ms>, significance = 1.0E-10)
[info]   - at n -> 128, change -> random changes: passed
[info]     (mean = 1.22 ms, ci = <1.18 ms, 1.26 ms>, significance = 1.0E-10)
[info]   - at n -> 256, change -> random changes: passed
[info]     (mean = 1.14 ms, ci = <1.11 ms, 1.18 ms>, significance = 1.0E-10)
[info]   - at n -> 512, change -> random changes: passed
[info]     (mean = 1.31 ms, ci = <1.06 ms, 1.57 ms>, significance = 1.0E-10)
[info]
[info] Test group: ilc.examples.HistogramGenerated (recomputation)
[info] - ilc.examples.HistogramGenerated (recomputation).Test-2 measurements:
[info]   - at n -> 1, change -> random changes: passed
[info]     (mean = 2.89 ms, ci = <2.85 ms, 2.93 ms>, significance = 1.0E-10)
[info]   - at n -> 2, change -> random changes: passed
[info]     (mean = 6.65 ms, ci = <4.96 ms, 8.34 ms>, significance = 1.0E-10)
[info]   - at n -> 4, change -> random changes: passed
[info]     (mean = 11.78 ms, ci = <8.11 ms, 15.45 ms>, significance = 1.0E-10)
[info]   - at n -> 8, change -> random changes: passed
[info]     (mean = 20.53 ms, ci = <13.54 ms, 27.53 ms>, significance = 1.0E-10)
[info]   - at n -> 16, change -> random changes: passed
[info]     (mean = 28.56 ms, ci = <27.24 ms, 29.88 ms>, significance = 1.0E-10)
[info]   - at n -> 32, change -> random changes: passed
[info]     (mean = 61.04 ms, ci = <53.91 ms, 68.16 ms>, significance = 1.0E-10)
[info]   - at n -> 64, change -> random changes: passed
[info]     (mean = 98.74 ms, ci = <96.10 ms, 101.37 ms>, significance = 1.0E-10)
[info]   - at n -> 128, change -> random changes: passed
[info]     (mean = 194.67 ms, ci = <191.70 ms, 197.65 ms>, significance = 1.0E-10)
[info]   - at n -> 256, change -> random changes: passed
[info]     (mean = 410.63 ms, ci = <382.64 ms, 438.62 ms>, significance = 1.0E-10)
[info]   - at n -> 512, change -> random changes: passed
[info]     (mean = 828.06 ms, ci = <770.23 ms, 885.89 ms>, significance = 1.0E-10)
[info]
[info]  Summary: 3 tests passed, 0 tests failed.
[info] ScalaTest
[info] Run completed in 2 minutes, 15 seconds.
[info] Total number of tests run: 0
[info] Suites: completed 0, aborted 0
[info] Tests: succeeded 0, failed 0, canceled 0, ignored 0, pending 0
[info] No tests were executed.
[info] Passed: Total 3, Failed 0, Errors 0, Passed 3
[success] Total time: 355 s, completed Jun 7, 2014 12:02:01 PM
>
```
