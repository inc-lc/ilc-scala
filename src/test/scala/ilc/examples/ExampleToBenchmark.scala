package ilc
package examples

import org.scalameter.api._

trait ReplacementChangeData extends BenchData {
  def replacementChange(newInput: Data): Change
}

import org.scalameter.{reporting, api, execution, Aggregator}
import org.scalameter.api._

/**
  * A more customizable version of ScalaMeter's PerformanceTest.Regression.
  */
trait RegressionTesting extends PerformanceTest {
  import Executor.Measurer
  import reporting._

  def warmer = Executor.Warmer.Default()
  def aggregator: Aggregator = Aggregator.complete(Aggregator.average)
  def measurer: Measurer = new Measurer.IgnoringGC with Measurer.PeriodicReinstantiation with Measurer.OutlierElimination with Measurer.RelativeNoise
  def regressionTester: RegressionReporter.Tester = RegressionReporter.Tester.OverlapIntervals()
  def historian: RegressionReporter.Historian = RegressionReporter.Historian.ExponentialBackoff()
  def regressionReporter =
    new RegressionReporter(regressionTester, historian)

  def buildExecutor: Executor = new execution.SeparateJvmsExecutor(warmer, aggregator, measurer)
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
  override def aggregator = QuickAndDirty.choose(Aggregator.min, super.aggregator)
  override def regressionTester = RegressionReporter.Tester.Accepter()

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
      new execution.LocalExecutor(warmer, aggregator, measurer),
      super.buildExecutor)

  //Don't save QuickAndDirty results.
  override lazy val persistor = QuickAndDirty.choose(Persistor.None, new SerializationPersistor)
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

  def verifyCorrectness() =
    performance of
    s"${className} (verification)" in {
      using(inputsOutputsChanges) in {
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

  def testSurgical() =
    performance of
    s"${className} (derivative, surgical change)" in {
      using(inputsOutputsChanges) in {
        case Datapack(oldInput, newInput, change, oldOutput) => {
          // we compute the result change with the derivative,
          // then apply it to the old value.
          updateOutput(derivative(oldInput)(change))(oldOutput)
        }
      }
    }

  def testRecomputation() =
    performance of s"${className} (recomputation)" in {
      using(inputsOutputsChanges) in {
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
  verifyCorrectness()
}

abstract class NonReplacementChangeBenchmark(benchData: BenchData) extends ExampleToBenchmark(benchData) {
  sharedTests()
}

// This must be a class because one can't define tests in a trait.
abstract class ReplacementChangeBenchmark(override val benchData: BenchData with ReplacementChangeData) extends ExampleToBenchmark(benchData) {
  import benchData._
  import example._

  testSurgical()

  performance of
  s"${className} (derivative, replacement change)" in {
    using(inputsOutputsChanges) in {
      case Datapack(oldInput, newInput, change, oldOutput) => {
        updateOutput(derivative(oldInput)(replacementChange(newInput)))(oldOutput)
      }
    }
  }

  testRecomputation()
}
