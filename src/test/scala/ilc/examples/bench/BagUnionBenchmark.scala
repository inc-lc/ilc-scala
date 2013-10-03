package ilc
package examples
package bench

import org.scalameter.api._
import ilc.feature.abelianGroups.Library._
import ilc.feature.bags.Library._

/**
  * Benchmark bag union.
  */
object BagUnionBenchmark extends NonReplacementChangeBenchmark(
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
}) extends BenchData
{
  import example._

  type BagType = OutputType
  type DeltaBagType = DeltaOutputType

  // consider leaving out the output.
  def inputOfSize(n: Int): Data = {
    def mkBag(first: Int, last: Int): BagType =
      (first to last).map(i => (i -> 1))(collection.breakOut)
    (mkBag(1, n), mkBag(-n + n/2, -1 + n/2))
  }

  private def mkChange(element: BagType): DeltaBagType =
    Left((FreeAbelianGroup.apply[Int], element))

  def add(e: Int) =
    mkChange(bagSingleton(e))

  def remove(e: Int) =
    mkChange(bagNegate(bagSingleton(e)))

  def replace(a: Int, b: Int) =
    mkChange(bagUnion(bagNegate(bagSingleton(a)))(bagSingleton(b)))

  def changes: Map[String, Int => DeltaBagType] =
    Map("no change"          -> (n => mkChange(bagEmpty)),
        "replace 1 by n + 1" -> (n => replace(1, n + 1)),
        "add n + 2"          -> (n => add(n + 2)),
        "remove 2"           -> (n => remove(2)),
        "remove n"           -> (n => remove(n)))

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

  def lookupChange(n: Int, jointDescription: String): Change = {
    val (description1, description2) = unpack(jointDescription)
    (changes(description1)(n), changes(description2)(n))
  }
}
