package ilc
package examples

import scala.collection.breakOut
import org.scalameter.api._
/*
import ilc.examples.MapSuccBinary._

object MapSuccBenchmark
extends PerformanceTest.Quickbenchmark
   with MapSuccTypes
{
  // collection sizes
  val base = 10000
  val last = 50000
  val step = 10000
  val sizes: Gen[Int] = Gen.range("n")(base, last, step)

  // consider leaving out the output.
  def inputOfSize(n: Int): Data =
    (1 to n).map(i => (i, i))(breakOut)

  val changeDescriptions: Gen[String] = Gen.enumeration("change")(
    "no change",
    "replace 1 by n + 1",
    "add n + 2",
    "remove 2",
    "remove n"
  )

  def lookupChange(n: Int, description: String): Change = description match {
    case "no change" =>
      Left(Map.empty)

    case "replace 1 by n + 1" =>
      Left(Map(1 -> Right(Right(n + 1))))

    case "add n + 2" =>
      Left(Map(n + 2 -> Left(Right(n + 2))))

    case "remove 2" =>
      Left(Map(2 -> Left(Left(()))))

    case "remove n" =>
      Left(Map(n -> Left(Left(()))))
  }

  case class Datapack(
    oldInput: Data,
    newInput: Data,
    change: Change,
    oldOutput: Data)

  val inputsOutputsChanges: Gen[Datapack] = for {
    n <- sizes
    description <- changeDescriptions
  } yield {
    val oldInput = inputOfSize(n)
    val change = lookupChange(n, description)
    val newInput = applyChange(change, oldInput)
    Datapack(oldInput, newInput, change, program(oldInput))
  }

  // Real tests has to be at the bottom.
  // otherwise we get NullPointerException
  // at org.scalameter.execution.LocalExecutor$$anonfun$run$1.apply
  //    (LocalExecutor.scala:38)
  performance of
  "ilc.examples.MapSuccBinary (derivative, surgical change)" in {
    using(inputsOutputsChanges) in {
      case Datapack(oldInput, newInput, change, oldOutput) => {
        // we compute the result change with the derivative,
        // then apply it to the old value.
        applyChange(derivative(oldInput)(change), oldOutput)
      }
    }
  }

  performance of
  "ilc.examples.MapSuccBinary (derivative, replacement change)" in {
    using(inputsOutputsChanges) in {
      case Datapack(oldInput, newInput, change, oldOutput) => {
        applyChange(derivative(oldInput)(Right(newInput)), oldOutput)
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
}*/
