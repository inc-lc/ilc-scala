package ilc
package examples
package bench

import scala.util.Random
import org.scalameter.api.Gen
import ilc.feature.abelianMaps.Library._
import ilc.feature.abelianGroups.Library._
import ilc.feature.bags.Library._

object HistogramBenchmark extends NonReplacementChangeBenchmark(
  new BootBenchData(HistogramGenerated) {
    override def base = 5000
    override def last = 25000
    override def step = 5000
  })

object HistogramVerification extends BenchmarkVerification(
  new BootBenchData(HistogramGenerated) {
    override def base = 5
    override def last = 5
    override def step = 5
  })


/** Given `n`, create a hash trie of bags with `n` random numbers
  * in total.
  *
  * The second to the last bag gets the same amount of numbers,
  * the first bag gets the rest, which is is usually more.
  * If we make a bar chart of the size of bags agains the orginal
  * numbers of bags, then it looks like a boot facing right.
  */
class BootBenchData(val example: ExampleGenerated {
  type InputType = AbelianMap[Int, Bag[Int]]
  type DeltaInputType =
    Either[(AbelianGroup[InputType], InputType), InputType]

  type OutputType = AbelianMap[Int, Int]
  type DeltaOutputType =
    Either[(AbelianGroup[OutputType], OutputType), OutputType]
})
extends BenchData
   // a hack to use MapChanges: depends on that both bags and
   // abelian maps are HashMap when generated.
   with feature.abelianMaps.MapChanges
{
  import example._

  def rand(from: Int, to: Int) = from + Random.nextInt(to - from + 1)

  def rand1(n: Int): Int = rand(1, n)

  def randomBag(size: Int, ceiling: Int): Bag[Int] =
    Bag(Seq.fill(size)(rand1(ceiling)): _*)

  def changeDescriptions: org.scalameter.api.Gen[String] =
    Gen.enumeration("change")(changesToMapsBetweenIntegers.keySet.toSeq: _*)

  def inputOfSize(n: Int): InputType = {
    val numberPerBag = rand(2, n)
    val numberOfBags = n / numberPerBag
    val firstBagSize = n - numberPerBag * (numberOfBags - 1)
    val randMax = math.max(math.sqrt(n.toDouble).toInt, 10)
    AbelianMap(
      (2 to numberOfBags).map(_ -> randomBag(numberPerBag, randMax)).toSeq: _*
    ).updated(1, randomBag(firstBagSize, randMax))
  }

  /** apply the change to a random element in input */
  def lookupChange(desc: String, input: InputType, output: OutputType):
      DeltaInputType = {
    val n = input.size
    val chosenKey = rand1(input.size)
    val chosenBag = input(chosenKey)
    Left((
      LiftedMapGroup[Int, Bag[Int]](FreeAbelianGroup()),
      AbelianMap(chosenKey ->
        // maps between integers happen to be bags of integers, too
        changesToMapsBetweenIntegers(desc)(
          // the index of this group is unused and can be anything
          // but the group itself is important, because lifting
          // the additive group on integers yields an abelian
          // group on maps that is identical to the free abelian
          // group of integers, i. e., bags.
          IndexedGroup.apply[Int](42, x => y => x + y, -_, 0)
        )(chosenBag).fold[Bag[Int]](_._2,
          _ => sys error "I want group-based bag changes" +
                         " but got a replacement"))
    ))
  }
}
