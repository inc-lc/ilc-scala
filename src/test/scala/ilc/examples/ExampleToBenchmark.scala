package ilc
package examples

import org.scalameter.api._

/**
  * Create a benchmark from an ExampleGenerated, input and changes.
  */
abstract class ExampleToBenchmark extends PerformanceTest.Quickbenchmark {
  /**
    * Subclass obligation: ExampleGenerated instance containing the generated code.
    */
  val example: ExampleGenerated

  import example._

  type Data = InputType
  type Change = DeltaInputType

  /**
    * Subclass obligation: inputs given the size.
    */
  def inputOfSize(n: Int): Data
  /**
    * Subclass obligation: list of descriptions of changes.
    */
  def changeDescriptions: Gen[String]
  /**
    * Subclass obligation: map input size and change description to actual change.
    * (XXX: should use an enum instead of a description for the key).
    */
  def lookupChange(n: Int, description: String): Change

  case class Datapack(
    oldInput: Data,
    newInput: Data,
    change: Change,
    oldOutput: OutputType)

  // collection sizes
  lazy val base = 1000
  lazy val last = 5000
  lazy val step = 1000
  lazy val sizes: Gen[Int] = Gen.range("n")(base, last, step)

  lazy val inputsOutputsChanges: Gen[Datapack] = for {
    n <- sizes
    description <- changeDescriptions
  } yield {
    val oldInput = inputOfSize(n)
    val change = lookupChange(n, description)
    val newInput = updateInput(change)(oldInput)
    Datapack(oldInput, newInput, change, program(oldInput))
  }

  def replacementChange(newInput: Data): Change

  def className: String = example.getClass.getName.stripSuffix("$")

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

  performance of
  s"${className} (derivative, replacement change)" in {
    using(inputsOutputsChanges) in {
      case Datapack(oldInput, newInput, change, oldOutput) => {
        updateOutput(derivative(oldInput)(replacementChange(newInput)))(oldOutput)
      }
    }
  }

  performance of s"${className} (recomputation)" in {
    using(inputsOutputsChanges) in {
      case Datapack(oldInput, newInput, change, oldOutput) => {
        program(newInput)
      }
    }
  }
}
