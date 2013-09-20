package ilc
package examples

import scala.collection.breakOut
import org.scalameter.api._

import ilc.examples.MapSuccBinary._

object MapSuccBenchmark
extends PerformanceTest.Quickbenchmark
{
  performance of
  "ilc.examples.MapSuccBinary (derivative, surgical change)" in {
    using(inputsOutputsChanges) in {
      case Datapack(oldInput, newInput, change, oldOutput) => {
        // we compute the result change with the derivative,
        // then apply it to the old value.
        updateOutput(derivative(oldInput)(change))(oldOutput)
      }
    }
  }

  performance of
  "ilc.examples.MapSuccBinary (derivative, replacement change)" in {
    using(inputsOutputsChanges) in {
      case Datapack(oldInput, newInput, change, oldOutput) => {
        updateOutput(derivative(oldInput)(Right(newInput)))(oldOutput)
      }
    }
  }

  performance of "ilc.examples.MapSuccBinary (recomputation)" in {
    using(inputsOutputsChanges) in {
      case Datapack(oldInput, newInput, change, oldOutput) => {
        program(newInput)
      }
    }
  }

  // collection sizes
  lazy val base = 1000
  lazy val last = 5000
  lazy val step = 1000
  lazy val sizes: Gen[Int] = Gen.range("n")(base, last, step)

  // consider leaving out the output.
  def inputOfSize(n: Int): Data =
    (1 to n).map(i => (i, i))(breakOut)

  lazy val changeDescriptions: Gen[String] = Gen.enumeration("change")(
    "no change",
    "replace 1 by n + 1",
    "add n + 2",
    "remove 2",
    "remove n"
  )

  type Data = InputType // which is equal to OutputType
  type Change = DeltaInputType // which is equal to DeltaOutputType

  def lookupChange(n: Int, description: String): Change = description match {
    case "no change" =>
      Left(Map.empty)

    case "replace 1 by n + 1" =>
      Left(Map(1 -> Right(n + 1)))

    case "add n + 2" =>
      Left(Map(n + 2 -> Left(Some(n + 2))))

    case "remove 2" =>
      Left(Map(2 -> Left(None)))

    case "remove n" =>
      Left(Map(n -> Left(None)))
  }

  case class Datapack(
    oldInput: Data,
    newInput: Data,
    change: Change,
    oldOutput: Data)

  lazy val inputsOutputsChanges: Gen[Datapack] = for {
    n <- sizes
    description <- changeDescriptions
  } yield {
    val oldInput = inputOfSize(n)
    val change = lookupChange(n, description)
    val newInput = updateInput(change)(oldInput)
    Datapack(oldInput, newInput, change, program(oldInput))
  }
}
