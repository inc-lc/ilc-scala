package ilc
package examples
package bench

import scala.util.Random
import org.scalameter.api.Gen
import ilc.feature.abelianMaps.Library._
import ilc.feature.abelianGroups.Library._
import ilc.feature.bags.Library._

class HistogramBenchmark extends OnlyDerivativeBenchmark(
  new RandomAbelianMapBenchData(HistogramGenerated) {
    override def base = 10 //Should be around the break-even point.
    override def last = 1000 * 1000
    override def step = 10
    override def sizes: Gen[Int] = Gen.exponential("n")(base, last, step)
  }) {
  override def iters: Int = 10
}

class HistogramRecomputeBenchmark extends OnlyRecomputationBenchmark(
  new RandomAbelianMapBenchData(HistogramGenerated) {
    override def base = 10
    override def last = 100
    override def step = 10
    override def sizes: Gen[Int] = Gen.exponential("n")(base, last, step)
  })

object HistogramVerification extends BenchmarkVerification(
  new RandomAbelianMapBenchData(HistogramGenerated) {
    override def base = 25
    override def last = 25
    override def step = 5
  })


class RandomAbelianMapBenchData(val example: ExampleGenerated {
  type InputType = AbelianMap[Int, Bag[Int]]
  type DeltaInputType =
    Either[(AbelianGroup[InputType], InputType), InputType]

  type OutputType = AbelianMap[Int, Int]
  type DeltaOutputType =
    Either[(AbelianGroup[OutputType], OutputType), OutputType]
})
extends BenchData
{
  import example._

  /**
    * Returns a pseudorandom, uniformly distributed int value between from
    * (inclusive) and to (inclusive).
    */
  def rand(from: Int, to: Int) = from + Random.nextInt(to - from + 1)

  def rand1(n: Int): Int = rand(1, n)

  def randomSeq(size: Int, ceiling: Int): Seq[Int] = {
    val actualCeiling = Math.max(1, ceiling)
    Seq.fill(size)(rand1(actualCeiling))
  }

  /** Put `elements` randomly in a `numberOfGroups`. */
  def randomGroups(elements: Seq[Int], numberOfGroups: Int):
      Seq[List[Int]] =
    {
      val groups: Array[List[Int]] = Array.fill(numberOfGroups)(Nil)
      val maxGroupIndex = numberOfGroups - 1
      elements foreach { element =>
        val i = rand(0, maxGroupIndex)
        groups(i) = element :: groups(i)
      }
      groups
    }

  def changeDescriptions: Gen[String] =
    Gen.enumeration("change")("up to 10 random changes")

  /** Given `n`, create a hash trie of bags with `n` random numbers
    * in total. The size of the trie is random between 1 and n.
    * Random numbers between 1 and n/expectedCollisions are generated
    * and put into a random bag.
    */
  def inputOfSize(n: Int): InputType =
    mkAbelianMap(n, getUpperBound(n))

  // expectedCollisions should be a small constant.
  // this way, the heap memory consumption of input stays linear in n.
  // Reason: repeated elements in a bag are represented only once in
  // memory. They should still count toward input size, because
  // repeated elements means repeated function calls during a fold.
  val expectedCollisions = 10
  def getUpperBound(n: Int): Int = n / expectedCollisions

  def mkAbelianMap(size: Int, upperBound: Int): InputType = {
    val trieSize = rand(1, size)
    val elements = randomSeq(size, upperBound)
    val bags     = randomGroups(elements, trieSize) map { group =>
      Bag(group: _*)
    }
    val keys     = randomSubset(trieSize, size)
    AbelianMap((0 until trieSize) map {i => (keys(i), bags(i))}: _*)
  }

  // Robert Floyd's sampling algorithm
  def randomSubset(subsetSize: Int, totalSize: Int): Seq[Int] = {
    val result: collection.mutable.Set[Int] =
      collection.mutable.Set.empty
    (totalSize - subsetSize until totalSize) foreach { i =>
      val element = rand(0, i)
      result += (if (result contains element) i else element)
    }
    result.toSeq
  }

  /** changes have random size between 1 and 10 */
  def changeSize = rand(1, 10)

  def halfChance: Boolean = if (rand(0, 1) == 0) true else false

  /** Generate a random abelian map to bags of integers as change.
    * The bags may have negative multiplicities.
    */
  def lookupChange(desc: String,
                   inputSize: Int,
                   input: InputType,
                   output: OutputType):
      DeltaInputType =
  {
    val randomMap = mkAbelianMap(changeSize, getUpperBound(inputSize))
    val changeGroupElement: InputType = randomMap map { case (key, bag) =>
      val bagChange: Bag[Int] = bag map { case (element, multiplicity) =>
        val keyExists = input contains key
        val modifiedMultiplicity: Int =
          if (halfChance)
            - multiplicity
          else
            multiplicity
        (element, modifiedMultiplicity)
      }
      (key, bagChange)
    }
    Left((
      LiftedMapGroup[Int, Bag[Int]](FreeAbelianGroup()),
      changeGroupElement))
  }
}
