package ilc
package examples

import scala.collection.breakOut
import org.scalameter.api.Gen

/**
  * A template for benchmarking derivatives of generated programs transforming
  * Map[Int, Int] into Map[Int, Int].
  */
class MapIntIntBenchData(val example: ExampleGenerated {
  type InputType = Map[Int, Int]
  type OutputType = Map[Int, Int]
  type DeltaInputType = Either[Map[Int, Either[Option[Int], Int]], Map[Int, Int]]
  type DeltaOutputType = Either[Map[Int, Either[Option[Int], Int]], Map[Int, Int]]
}) extends BenchData with ReplacementChangeData
{
  import example._

  def replacementChange(newInput: Data): Change = Right(newInput)

  // consider leaving out the output.
  def inputOfSize(n: Int): Data =
    (1 to n).map(i => (i -> i))(breakOut)

  lazy val changeDescriptions: Gen[String] = Gen.enumeration("change")(
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
      Left(Map(1 -> Right(n + 1)))

    case "add n + 2" =>
      Left(Map(n + 2 -> Left(Some(n + 2))))

    case "remove 2" =>
      Left(Map(2 -> Left(None)))

    case "remove n" =>
      Left(Map(n -> Left(None)))
  }
}

import ilc.feature.bags.Library._

import collection.immutable.HashMap

class BagIntBenchData(val example: ExampleGenerated {
  type InputType = Bag[Int]
  type OutputType = Bag[Int]
  type DeltaInputType = Bag[Int]
  type DeltaOutputType = Bag[Int]
}) extends BenchData
{
  import example._

  // consider leaving out the output.
  def inputOfSize(n: Int): Data =
    (1 to n).map(i => (i -> 1))(breakOut)

  lazy val changeDescriptions: Gen[String] = Gen.enumeration("change")(
    "no change",
    "replace 1 by n + 1",
    "add n + 2",
    "remove 2",
    "remove n"
  )

  def add(e: Int) =
    bagSingleton(e)

  def remove(e: Int) =
    bagNegate(add(e))

  def replace(a: Int, b: Int) =
    bagUnion(remove(a))(add(b))

  def lookupChange(n: Int, description: String): Change = description match {
    case "no change" =>
      bagEmpty

    case "replace 1 by n + 1" =>
      replace(1, n + 1)

    case "add n + 2" =>
      add(n + 2)

    case "remove 2" =>
      remove(2)

    case "remove n" =>
      remove(n)
  }
}

import ilc.feature.abelianGroups.Library._
import ilc.feature.bags.BagChanges

class AbelianBagIntBenchData(val example: ExampleGenerated {
  type InputType = Bag[Int]
  type OutputType = Bag[Int]
  type DeltaInputType = DeltaOutputType
  type DeltaOutputType =
    Either[(AbelianGroup[Bag[Int]], Bag[Int]), Bag[Int]]
}) extends BenchData with BagChanges
{
  import example._

  // copied from BagIntBenchData
  def inputOfSize(n: Int): Data =
    (1 to n).map(i => (i -> 1))(breakOut)

  lazy val changeDescriptions: Gen[String] =
    Gen.enumeration("change")(changesToBagsOfIntegers.keySet.toSeq: _*)

  def lookupChange(n: Int, description: String): Change =
    changesToBagsOfIntegers(description)(n)
}
