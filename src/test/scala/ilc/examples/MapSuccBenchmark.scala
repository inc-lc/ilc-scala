package ilc
package examples

import scala.collection.breakOut
import org.scalameter.api.Gen

object MapSuccBenchmark extends ExampleToBenchmark
{
  val example = ilc.examples.MapSuccBinary
  import example._

  def replacementChange(newInput: Data): Change = Right(newInput)

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
