package longRunning

import scala.util.Random
import org.scalameter.api.Gen
import ilc.feature.abelianMaps.Library._
import ilc.feature.abelianGroups.Library._
import ilc.feature.bags.Library._
import ilc.examples.BenchData
import ilc.examples.BenchmarkVerification
import ilc.examples.ExampleGenerated
import ilc.examples.HistogramGenerated
import ilc.examples.OnlyDerivativeBenchmark
import ilc.examples.OnlyRecomputationBenchmark

class HistogramBenchmark extends OnlyDerivativeBenchmark(
  new WordCountBenchData(HistogramGenerated) {
    override def base = 1 //Should be around the break-even point.
    override def last = FastBenchmarksFlag.choose(512, 4096)
    override def step = 2
    override def sizes: Gen[Int] = Gen.exponential("n")(base, last, step)
  }) {
  override def iters: Int = 10
}

class HistogramRecomputeBenchmark extends OnlyRecomputationBenchmark(
  new WordCountBenchData(HistogramGenerated) {
    override def base = 1
    override def last = FastBenchmarksFlag.choose(512, 4096)
    override def step = 2
    override def sizes: Gen[Int] =
      Gen.exponential("n")(base, last, step)
      // exponential doesn't support base = last (it produces n = 0 during
      // warmup). Great! Use single instead:
      //Gen.single("n")(last)
  })

object HistogramVerification extends BenchmarkVerification(
  new WordCountBenchData(HistogramGenerated) {
    override def base = 2
    override def last = 2
    override def step = 10
  })


class WordCountBenchData(val example: ExampleGenerated {
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

  val vocabularySize = 1000

  /** word limit in UCLA personal statement for undergrad admission */
  val expectedDocumentSize = 1000

  /**
    * Returns a pseudorandom, uniformly distributed int value between from
    * (inclusive) and to (inclusive).
    */
  def rand(from: Int, to: Int) = from + Random.nextInt(to - from + 1)

  def rand1(n: Int): Int = rand(1, n)

  def randomWord: Int = rand1(vocabularySize)

  def randomWords(wordCount: Int): Seq[Int] =
    Seq.fill(wordCount)(randomWord)

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
    Gen.enumeration("change")("random changes")

  /** Given `n`, create a hash trie of `n` documents with
    *
    *   n * expectedDocumentSize
    *
    * words in total. Words are put into documents at random
    * in the manner of the balls-and-bins situation.
    */
  def inputOfSize(n: Int): InputType = {
    val numberOfDocuments = n
    val words = randomWords(getTotalWords(n))
    val documents = randomGroups(words, numberOfDocuments) map { group =>
      Bag(group: _*)
    }
    AbelianMap((0 until numberOfDocuments) map {i => (i, documents(i))}: _*)
  }

  def getTotalWords(n: Int): Int = n * expectedDocumentSize

  def halfChance: Boolean = if (rand(0, 1) == 0) true else false

  /** Generate a random insertion or a random deletion. */
  def lookupChange(desc: String,
                   n: Int,
                   input: InputType,
                   output: OutputType):
      DeltaInputType =
  {
    val totalWords = getTotalWords(n)
    val changeGroupElement: InputType =
      if (halfChance)
        generateInsertion(input, totalWords)
      else
        generateDeletion(input, totalWords)
    Left((
      LiftedMapGroup[Int, Bag[Int]](FreeAbelianGroup()),
      changeGroupElement))
  }

  sealed trait CountingWords
  case class FoundTheWord(word: Int) extends CountingWords
  case class CountedWholeDocument(wordCount: Int) extends CountingWords

  def ithWord(document: Bag[Int], i: Int): CountingWords = {
    var wordCount = 0
    val iterator = document.iterator
    while (iterator.hasNext) {
      val (word, multiplicity) = iterator.next
      wordCount += multiplicity
      if (wordCount > i)
        return FoundTheWord(word)
    }
    CountedWholeDocument(wordCount)
  }

  def ithWordInDoc(input: InputType, i: Int): (Int, Int) = {
    def loop(i: Int, iterator: Iterator[(Int, Bag[Int])]): (Int, Int) = {
      val (docID, document) = iterator.next
      ithWord(document, i) match {
        case FoundTheWord(word) =>
          (word, docID)
        case CountedWholeDocument(wordCount) =>
          loop(i - wordCount, iterator)
      }
    }
    loop(i, input.iterator)
  }

  def generateDeletion(input: InputType, totalWords: Int): InputType = {
    val (word, docID) = ithWordInDoc(input, rand(0, totalWords - 1))
    AbelianMap(docID -> bagNegate(Bag(word)))
  }

  def generateInsertion(input: InputType, totalWords: Int): InputType = {
    val numberOfDocuments = input.size
    val insertedWord = randomWord
    val locationOfInsertion = rand(- numberOfDocuments, totalWords - 1)
    // negative locations represent space between documents.
    // if a word is inserted there, then a new document is created.
    if (locationOfInsertion < 0) {
      val newDocID = numberOfDocuments
      assert(! (input contains newDocID))
      AbelianMap(newDocID -> Bag(insertedWord))
    }
    else {
      val (word, docID) = ithWordInDoc(input, locationOfInsertion)
      // triggers scala 2.10.2 bug in merging HashMaps (Bags):
      // if multiplicity is 1, sometimes the collision handler
      // is invoked on null.
      AbelianMap(docID -> Bag(insertedWord))
    }
  }
}
