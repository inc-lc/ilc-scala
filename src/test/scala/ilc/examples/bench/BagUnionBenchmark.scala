package ilc
package examples
package bench

import org.scalameter.api._
import ilc.feature.abelianGroups.Library._
import ilc.feature.bags.Library._
import ilc.feature.bags.BagChanges

/**
  * Benchmark bag union.
  */
class BagUnionBenchmark extends NonReplacementChangeBenchmark(
  new BagPairBenchData(BagUnionGenerated) {
    override def base = 5000
    override def last = 25000
    override def step = 5000
  })

object BagUnionVerification extends BenchmarkVerification(
  new BagPairBenchData(BagUnionGenerated) {
    override def base = 5
    override def last = 15
    override def step = 5
  })

class BagPairBenchData(val example: ExampleGenerated {
  type InputType = (OutputType, OutputType)
  type DeltaInputType = (DeltaOutputType, DeltaOutputType)

  type OutputType = Bag[Int]
  type DeltaOutputType =
    Either[(AbelianGroup[Bag[Int]], Bag[Int]), Bag[Int]]
}) extends BenchData with BagChanges
{
  import example._

  type BagType = OutputType
  type DeltaBagType = ChangeToBags[Int]

  // consider leaving out the output.
  def inputOfSize(n: Int): Data = {
    def mkBag(first: Int, last: Int): BagType =
      (first to last).map(i => (i -> 1))(collection.breakOut)
    (mkBag(1, n), mkBag(-n + n/2, -1 + n/2))
  }

  def changes: Map[String, Int => DeltaBagType] = changesToBagsOfIntegers

  def pack(description1: String, description2: String): String =
    s"($description1, $description2)"

  def unpack(jointDescription: String): (String, String) = {
    val Array(x, y) = jointDescription.split("(, )|[()]").tail
    (x, y)
  }

  lazy val changeDescriptions: Gen[String] = {
    val descriptions = changes.keySet.toSeq
    val jointDescriptions: Seq[String] = for {
        change1 <- descriptions
        change2 <- descriptions
      } yield pack(change1, change2)
    Gen.enumeration("change")(jointDescriptions: _*)
  }

  def lookupChange(jointDescription: String, inputSize: Int, input: InputType, output: OutputType): Change = {
    val (description1, description2) = unpack(jointDescription)
    ( changes(description1)(input._1.size),
      changes(description2)(input._2.size) )
  }
}
