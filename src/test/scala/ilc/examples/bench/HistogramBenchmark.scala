package ilc
package examples
package bench

import scala.util.Random
import org.scalameter.api.Gen
import ilc.feature.abelianMaps.Library._
import ilc.feature.abelianGroups.Library._
import ilc.feature.bags.Library._
import ilc.feature.bags.BagChanges

object HistogramBenchmark extends NonReplacementChangeBenchmark(
  new PyramidBenchData(HistogramGenerated) {
    override def base = 50
    override def last = 250
    override def step = 50
  })

object HistogramVerification extends BenchmarkVerification(
  new PyramidBenchData(HistogramGenerated) {
    override def base = 5
    override def last = 5
    override def step = 5
  })


class PyramidBenchData(val example: ExampleGenerated {
  type InputType = AbelianMap[Int, Bag[Int]]
  type DeltaInputType =
    Either[(AbelianGroup[InputType], InputType), InputType]

  type OutputType = AbelianMap[Int, Int]
  type DeltaOutputType =
    Either[(AbelianGroup[OutputType], OutputType), OutputType]
})
extends BenchData with BagChanges
{
  import example._

  val numberOfChanges = 10

  def changeDescriptions: org.scalameter.api.Gen[String] =
    Gen.enumeration("change")(changesToBagsOfIntegers.keySet.toSeq: _*)

  def inputOfSize(n: Int): InputType = {
    var bag: Bag[Int] = Bag()
    AbelianMap(
      (1 to n) map { i =>
        // breaks abstraction barrier.
        // needs abstraction in library for generated code?
        bag = bag.updated(i, 1)
        i -> bag
      }: _*)
  }

  /** apply the change to a random element in input */
  def lookupChange(desc: String, input: InputType, output: OutputType):
      DeltaInputType = {
    val n = input.size
    val luckyContestant = Random.nextInt(n) + 1
    Left((
      LiftedMapGroup[Int, Bag[Int]](FreeAbelianGroup()),
      AbelianMap(luckyContestant ->
        changesToBagsOfIntegers(desc)(n).fold[Bag[Int]](_._2,
          _ => sys error "I want group-based bag changes" +
                         " but got a replacement"))
    ))
  }
}
