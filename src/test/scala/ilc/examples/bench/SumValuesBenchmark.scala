package ilc
package examples
package bench

import org.scalameter.api._
import ilc.feature.abelianGroups.Library._
import ilc.feature.abelianMaps.Library._
import ilc.feature.abelianMaps.MapChanges

class SumValuesBenchmark extends NonReplacementChangeBenchmark(
  new AdditiveMapBenchData(SumValuesGenerated) {
    override def base = 5000
    override def last = 25000
    override def step = 5000
  })

object SumValuesVerification extends BenchmarkVerification(
  new AdditiveMapBenchData(SumValuesGenerated) {
    override def base = 5
    override def last = 25
    override def step = 5
  })

// Input: Map[Int, Int] with LiftedMapGroup(additiveIntegerGroup)
class AdditiveMapBenchData(val example: ExampleGenerated {
  type InputType =
    AbelianMap[Int, Int]

  type DeltaInputType =
    Either[
      (AbelianGroup[AbelianMap[Int, Int]], AbelianMap[Int, Int]),
      AbelianMap[Int, Int]
    ]

  type OutputType =
    (Int, AbelianGroup[Int])

  type DeltaOutputType =
    (Either[(AbelianGroup[Int], Int), Int], AbelianGroup[Int])
}) extends BenchData with MapChanges
{
  import example._

  private def mkIdentityMap(n: Int) =
    AbelianMap((1 to n) map {i => (i, i)}: _*)

  def inputOfSize(n: Int): Data = mkIdentityMap(n)

  lazy val changeDescriptions: Gen[String] =
    Gen.enumeration("change")(changesToMapsBetweenIntegers.keySet.toSeq: _*)

  def lookupChange(description: String,
                   input: InputType,
                   output: OutputType): Change =
    changesToMapsBetweenIntegers(description)(output._2)(input)
}
