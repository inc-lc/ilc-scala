package ilc
package examples

import org.scalameter.{reporting, execution, Aggregator, Context}
import org.scalameter.api._
import longRunning.FastBenchmarksFlag
import Executor.Measurer

trait ReplacementChangeData extends BenchData {
  def replacementChange(newInput: Data): Change
}

/**
  * A more customizable version of ScalaMeter's PerformanceTest.Regression.
  */
trait RegressionTesting extends PerformanceTest {
  import reporting._

  def warmer = Executor.Warmer.Default()
  def aggregator: Aggregator = Aggregator.average
  def measurer: Measurer = new Measurer.IgnoringGC with Measurer.PeriodicReinstantiation with Measurer.OutlierElimination with Measurer.RelativeNoise
  def regressionTester: RegressionReporter.Tester = RegressionReporter.Tester.OverlapIntervals()
  def historian: RegressionReporter.Historian = RegressionReporter.Historian.ExponentialBackoff()
  def regressionReporter =
    new RegressionReporter(regressionTester, historian)

  protected def separateExecutor = new execution.SeparateJvmsExecutor(warmer, aggregator, measurer)
  protected def localExecutor = new execution.LocalExecutor(warmer, aggregator, measurer)
  def buildExecutor: Executor = separateExecutor
  def reporters: Seq[Reporter]

  /* The methods below are part of the interface to the superclass.
   *
   * They and their overrides must be lazy.
   *
   * Unlike done by ScalaMeter's implementation, I believe they should be lazy
   * values, since they are called multiple times, possibly for performance, but
   * also to avoid potential bugs (unless the implementation is completely
   * stateless).
   */
  //@transient is needed for lazy values which aren't serializable, to avoid
  //java.io.NotSerializableException.
  @transient
  override lazy val executor: Executor = buildExecutor

  override lazy val reporter: Reporter =
    Reporter.Composite(reporters: _*)

  override lazy val persistor: Persistor = new SerializationPersistor
}

/**
  * Our benchmarking settings.
  */
trait BaseBenchmark extends RegressionTesting with Serializable {
  import Executor.Measurer

  override def aggregator = QuickAndDirty.choose(Aggregator.min, super.aggregator)
  override def regressionTester = RegressionReporter.Tester.Accepter()

  //Removes Measurer.RelativeNoise from the
  //above default, since we're not really doing regression testing, hence we
  //don't need to add noise to results.
  //
  //Measurer.PeriodicReinstantiation is in because it can change, by
  //reinstantiation, the layout in which data is allocated, limiting the effects
  //of bad allocation patterns.
  override def measurer: Measurer =
    new Measurer.IgnoringGC
      //Comment it out, since it makes things so slow.
      // with Measurer.PeriodicReinstantiation

      //OutlierElimination is not described by the paper we're citing. However,
      //it behaves well given enough initial warmup.
      with Measurer.OutlierElimination

  override def reporters = baseReporters ++ QuickAndDirty.choose(Seq.empty, expensiveReporters)

  def baseReporters =
    Seq(
      regressionReporter,           // First, update history
      LoggingReporter())

  def expensiveReporters =
    Seq(
      DsvReporter(delimiter=';'),   // Then, use the updated history
      HtmlReporter(true)            // Ditto
      /*ChartReporter(ChartFactory.XYLine())*/
    )

  override def buildExecutor: Executor = QuickAndDirty.choose(
      localExecutor,
      separateExecutor)

  //Don't save QuickAndDirty results.
  override lazy val persistor = QuickAndDirty.choose(Persistor.None, new SerializationPersistor)

  def minWarmupRuns: Int = 20
  def maxWarmupRuns: Int = 100

  def memorySizeMB: Int = FastBenchmarksFlag.choose(1024, 4096)
  /* You need to use this explicitly when defining each test */
  def testConfig =
    Context(
      reports.regression.significance -> 0.01) ++ //Confidence level = 99 %
    QuickAndDirty.choose(Context.empty,
      Context(
        exec.jvmflags -> s"-Xmx${memorySizeMB}m -Xms${memorySizeMB}m -XX:CompileThreshold=100",
        exec.minWarmupRuns -> minWarmupRuns,
        exec.warmupCovThreshold -> 0.05,
        exec.reinstantiation.fullGC -> true,
        exec.maxWarmupRuns -> maxWarmupRuns))

  //To make verification fast.
  def verificationConfig =
    Context(
      exec.minWarmupRuns -> 1,
      exec.maxWarmupRuns -> 1,
      exec.warmupCovThreshold -> 1.0,
      exec.benchRuns -> 1,
      exec.independentSamples -> 1,
      exec.reinstantiation.fullGC -> false,
      verbose -> false)
}

/**
  * Create a benchmark from an ExampleGenerated, input and changes.
  *
  * Note: this class inherits (indirectly) from DelayedInit, hence
  * initialization order is rather different here. Hence, prefer defining
  * helpers inside BenchData if at all possible.
  *
  * This class is marked abstract to prevent ScalaMeter from trying to run it.
  */
abstract class ExampleToBenchmark(val benchData: BenchData) extends BaseBenchmark {
  import benchData._
  import example._

  def verifyCorrectness(desc: String, derivative: (=> InputType) => (=> DeltaInputType) => DeltaOutputType) =
    performance of
    s"${className} (${desc}, verification)" in {
      using(inputsOutputsChanges) config verificationConfig in {
        case Datapack(oldInput, newInput, change, oldOutput) => {
          val newOutput = program(newInput)
          val derivedChange = derivative(oldInput)(change)
          if (newOutput != updateOutput(derivedChange)(oldOutput)) {
            println(s"change = $change")
            println(s"oldInput = $oldInput")
            println(s"newInput = $newInput")
            println(s"oldOutput = $oldOutput")
            println(s"newOutput = $newOutput")
            println(s"derivedChange = ${derivedChange}")
            sys error (s"failed ${this.getClass.getName}")
          }
        }
      }
    }

  def iters: Int = 1

  def testSurgical(desc: String, derivative: (=> InputType) => (=> DeltaInputType) => DeltaOutputType) =
    performance of
    s"${className} (${desc}, surgical change)" in {
      using(inputsOutputsChanges) config testConfig in {
        case Datapack(oldInput, newInput, change, oldOutput) => {
          // we compute the result change with the derivative,
          // then apply it to the old value.
          for (i <- 1 to iters)
            updateOutput(derivative(oldInput)(change))(oldOutput)
        }
      }
    }

  def testSurgical() {
    for ((desc, derivative) <- derivatives) {
      testSurgical(desc, derivative)
    }
  }

  def verifyCorrectness() {
    for ((desc, derivative) <- derivatives) {
      verifyCorrectness(desc, derivative)
    }
  }

  def testRecomputation() =
    performance of s"${className} (recomputation)" in {
      using(inputsOutputsChanges) config testConfig in {
        case Datapack(oldInput, newInput, change, oldOutput) => {
          program(newInput)
        }
      }
    }

  // We can't just define the test inline, since they would not be inherited by
  // subclasses, due to https://github.com/axel22/scalameter/issue/32. But that
  // issue contains a fix, so this should hopefully get better soon.
  def sharedTests() = {
    testSurgical()
    testRecomputation()
  }
}

abstract class BenchmarkVerification(benchData: BenchData)
extends ExampleToBenchmark(benchData) {
  override def reporters = Seq(LoggingReporter())
  override def measurer: Measurer = new Measurer.Default
  override def buildExecutor = localExecutor

  verifyCorrectness()
}

abstract class NonReplacementChangeBenchmark(benchData: BenchData) extends ExampleToBenchmark(benchData) {
  sharedTests()
}

abstract class OnlyDerivativeBenchmark(benchData: BenchData) extends ExampleToBenchmark(benchData) {
  override def minWarmupRuns = 10000
  override def maxWarmupRuns = 10000

  testSurgical()
}

abstract class OnlyRecomputationBenchmark(benchData: BenchData) extends ExampleToBenchmark(benchData) {
  testRecomputation()
}

// This must be a class because one can't define tests in a trait.
abstract class ReplacementChangeBenchmark(override val benchData: BenchData with ReplacementChangeData) extends ExampleToBenchmark(benchData) {
  import benchData._
  import example._

  testSurgical()

  performance of
  s"${className} (derivative, replacement change)" in {
    using(inputsOutputsChanges) config testConfig in {
      case Datapack(oldInput, newInput, change, oldOutput) => {
        updateOutput(derivative(oldInput)(replacementChange(newInput)))(oldOutput)
      }
    }
  }

  testRecomputation()
}
