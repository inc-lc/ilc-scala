package ilc
package examples
package handwritten


import util._
import collection.{mutable, immutable}
import org.scalameter.api._

/*
 * Warning: does not implement any actual collection interface.
 * I still aim for this to support for-comprehensions.
 */
case class Bag[A](contents: immutable.Map[A, Int] = immutable.HashMap()) {
  import feature.bags.{Library => BagLib}

  private def mapCommon[B](f: A => B) = {
     for {
      (el, count) <- contents.toSeq //.toSeq is just to change the result type. Alternatively, just use breakOut.
    } yield (f(el), count)
  }

  /**
   * Maps f over the bag content.
   * Precondition: f is injective. Otherwise, resort to map.
   */
  def mapInjective[B](f: A => B): Bag[B] = {
    new Bag(mapCommon(f).toMap)
  }

  /**
   * This implements the expected map interface, sufficient for for-comprehension (though not CanBuildFrom-compliant).
   * By far not the most efficient implementation possible.
   * Untested.
   */
  def map[B](f: A => B): Bag[B] = {
    val internalMap = mapCommon(f).
      //F might not be injective
      map { case (el, count) => immutable.HashMap((el, count)) }.
      fold(BagLib.bagEmpty[B])(BagLib.bagUnionInt _)
    new Bag(internalMap)
  }

  def flatten[B](implicit p: A <:< Bag[B]): Bag[B] = {
    new Bag(for {
      (outerEl, count) <- contents
      innerBag = p(outerEl)
      (el, count2) <- innerBag.contents
    } yield (el, count * count2))
  }

  /**
   * This implements the expected flatMap interface (though not CanBuildFrom-compliant).
   * By far not the most efficient implementation possible.
   * Untested.
   */
  def flatMap[B](f: A => Bag[B]): Bag[B] = {
    map(f).flatten
  }

  //Complexity: quadratic?
  def --(toDrop: Bag[A]): Bag[A] = {
    new Bag(toDrop.contents.foldLeft(contents) {
      case (newContents, (el, count)) =>
        val newCount = newContents.getOrElse(el, 0) - count
        if (newCount != 0)
          newContents.updated(el, newCount)
        else
          newContents - el
    })
  }

  def ++(toAdd: Bag[A]): Bag[A] = {
    new Bag(toAdd.contents.foldLeft(contents) {
      case (newContents, (el, count)) =>
        val newCount = newContents.getOrElse(el, 0) + count
        if (newCount != 0)
          newContents.updated(el, newCount)
        else
          newContents - el
    })
  }
}

object Bag {
  import feature.bags.{Library => BagLib}

  def apply[T](t: T*): Bag[T] =
    new Bag(t.map(BagLib.bagSingletonInt).fold(BagLib.bagEmpty)(BagLib.bagUnionInt _))
}

//import scala.collection.Bag
//import scala.collection.immutable.{HashBag=>Bag}

object BagUtils {
  //implicit def config[T] = Bag.configuration.compact[T]
  implicit class FlattenOps[T](b: Bag[Bag[T]]) {
    def flattenB: Bag[T] = {
      b.flatten
      /*
      //Alternative implementation, but quadratic because of
      //https://github.com/nicolasstucki/multisets/issues/7
      //b.fold[Bag[T]](Bag.empty)(_ union _)
      val builder = Bag.newBuilder[T]
      for (baglet <- b)
        builder ++= baglet
      builder.result
      */
    }
  }
}
import BagUtils._

object MemoUtils {
  import feature.memoize.Library._

  def getIdentityMap[Key, Value]: mutable.Map[Key, Value] = MemoizeObjMap()

  def memoedF[I, O](cache: mutable.Map[I, O])(f: I => O): I => O =
    x => cache.getOrElseUpdate(x, f(x))

  def memo[A, B](f: A => B): A => B = {
    val cache = getIdentityMap[A, B]
    memoedF(cache)(f)
  }
}

class NestedLoop1(val M: Int = 1000, val N: Int = 1000) extends Serializable {
  val coll1Init = List[Int](0 to M - 1: _*)
  val coll2Init = List[Int](1 to N: _*)
  val coll1Upd = List[Int](1 to M: _*)

  val bag1Init = Bag[Int](0 to M - 1: _*)
  val bag2Init = Bag[Int](1 to N: _*)
  val bag1Upd = Bag[Int](1 to M: _*)

  //On lists first. Bag[T] is a Map[T, Int], so it's less clear how to write this on maps.
  def nestedLoop1(coll1: List[Int], coll2: List[Int]): List[Int] =
    for {
      i <- coll1
      j <- coll2
    } yield (i * N + j)


  //Yes!
  def nestedLoopBags1(coll1: Bag[Int], coll2: Bag[Int]): Bag[Int] =
    for {
      i <- coll1
      j <- coll2
    } yield (i * N + j)

  def nestedLoop2(coll1: List[Int], coll2: List[Int]): List[Int] = {
    val g = (i: Int) => (j: Int) => i * N + j
    val f = (i: Int) => coll2.map(g(i))
    coll1.flatMap(f)
  }

  def nestedLoopBags2(coll1: Bag[Int], coll2: Bag[Int]): Bag[Int] = {
    val g = (i: Int) => (j: Int) => i * N + j
    val f = (i: Int) => coll2.map(g(i))
    coll1.flatMap(f)
  }

  def nestedLoop3(coll1: List[Int], coll2: List[Int]): List[Int] = {
    val g = (i: Int) => (j: Int) => i * N + j
    val f = (i: Int) => coll2.map(g(i))
    coll1.map(f).flatten
  }

  def nestedLoopBags3(coll1: Bag[Int], coll2: Bag[Int]): Bag[Int] = {
    val g = (i: Int) => (j: Int) => i * N + j
    val f = (i: Int) => coll2.map(g(i))
    coll1.map(f).flattenB
    //coll1.flatMap(f)
  }

  import MemoUtils._

  //Also return the cache.
  def nestedLoop3Memo(coll1: List[Int], coll2: List[Int]) /*: (List[Int], Any)*/ = {
    val g = (i: Int) => (j: Int) => i * N + j
    val f = (i: Int) => coll2.map(g(i))
    //memo(coll1 => coll1.map(f))(coll1).flatten
    //coll1.map(memoInt(f)).flatten
    val cache = getIdentityMap[Int, List[Int]]
    val ret = (coll1.map(x => cache.getOrElseUpdate(x, f(x))).flatten, cache)

    Util.assertType[List[Int]](ret._1)
    ret
  }

  //Also return the cache.
  def nestedLoopBags3Memo(coll1: Bag[Int], coll2: Bag[Int]) /*: (List[Int], Any)*/ = {
    val g = (i: Int) => (j: Int) => i * N + j
    val f = (i: Int) => coll2.map(g(i))
    //memo(coll1 => coll1.map(f))(coll1).flatten
    //coll1.map(memoInt(f)).flatten
    val cache = getIdentityMap[Int, Bag[Int]]
    val ret = (coll1.map(x => cache.getOrElseUpdate(x, f(x))).flattenB, cache)

    Util.assertType[Bag[Int]](ret._1)
    ret
  }

  /*
   * This code kind-of implements what I described in
   * https://github.com/Blaisorblade/ilc-ldiff/issues/14#issuecomment-99446136
   * Some mismatches:
   * - it doesn't use derivatives for f, since they aren't easily available here; I only use recomputation.
   * - Updating the output collection is too slow with this representation.
   *   But should I really update the collection?
   *   For now, setting `getRightResult = false` skips building the updated result, and just computes the pieces for benchmarking purposes.
   *
   * Other notes:
   * - This uses as collections List[Int] and ignores the order of elements. I should instead take Bag[Int], I guess.
   */

  def nestedLoop3Incr(coll1: List[Int], coll2: List[Int])(oldOut: List[Int], cache: mutable.Map[Int, List[Int]])(removedFromColl1: Int, addedToCol1: Int, getRightResult: Boolean): Any /*List[Int]*/ = {
    val g = (i: Int) => (j: Int) => i * N + j
    val f = (i: Int) => coll2.map(g(i))

    val toDrop = cache.getOrElse(removedFromColl1, List())
    val toAdd = //Correct result, but too slow with these data structures.
      memoedF(cache)(f)(addedToCol1)

    if (getRightResult)
      oldOut.filterNot(toDrop.contains) ++ toAdd
    else
      //The result here is *not* good. But at least, return all pieces, to prevent optimizations from getting in the way of microbenchmarking.
      (toAdd, toDrop)
  }

  def nestedLoopBags3Incr(coll1: Bag[Int], coll2: Bag[Int])(oldOut: Bag[Int], cache: mutable.Map[Int, Bag[Int]])(removedFromColl1: Int, addedToCol1: Int) /*List[Int]*/ = {
    val g = (i: Int) => (j: Int) => i * N + j
    val f = (i: Int) => coll2.map(g(i))

    val toDrop = cache.getOrElse(removedFromColl1, Bag[Int]())
    val toAdd = //Correct result, but too slow with these data structures.
      memoedF(cache)(f)(addedToCol1)

    oldOut -- toDrop ++ toAdd
  }
}

class NestedLoop1BenchInput(M: Int, N: Int) extends NestedLoop1(M, N) {
  val (res1, res1Cache) = nestedLoop3Memo(coll1Init, coll2Init)
  val (resBag1, resBag1Cache) = nestedLoopBags3Memo(bag1Init, bag2Init)
}

trait MyBenchmarkingSetup extends BaseBenchmark {
  override def reporters = Seq(LoggingReporter())
  override def memorySizeMB: Int = 4096

  private val realBench = false
  def myBenchConfig =
    if (realBench)
      //Config. for real measurements.
      testConfig
    else
      //To make tests fast, while still having lots of memory
      Context(
        //reports.regression.significance -> 0.01, //Confidence level = 99 %
        exec.jvmflags -> s"-Xmx${memorySizeMB}m -Xms${memorySizeMB}m -XX:CompileThreshold=100"
      ) ++ verificationConfig
}


class NestedLoop1Bench extends MyBenchmarkingSetup {
  val sizes = Gen.enumeration("m")(100, 500, 1000, 5000, 10000)
  val inputs = for {
    i <- sizes
    j <- Gen.exponential("n")(1, 25, 5)
    n = new NestedLoop1BenchInput(i, j)
  } yield (i, n)

  performance of "nestedLoop1" in {
    using(inputs) config myBenchConfig in { case (i, n) =>
      import n._
      nestedLoop1(coll1Init, coll2Init)
    }
  }

  performance of "nestedLoop3" in {
    using(inputs) config myBenchConfig in { case (i, n) =>
      import n._
      nestedLoop3(coll1Init, coll2Init)
    }
  }

  performance of "nestedLoop3Memo" in {
    using(inputs) config myBenchConfig in { case (i, n) =>
      import n._
      nestedLoop3Memo(coll1Init, coll2Init)
    }
  }

  performance of "nestedLoop3Incr" in {
    using(inputs) config myBenchConfig in { case (i, n) =>
      import n._
      nestedLoop3Incr(coll1Init, coll2Init)(res1, res1Cache)(0, N, true)
    }
  }

  //Cheat a lot, to get closer to the performance we'd hope with proper data structures.
  performance of "nestedLoop3IncrSimulated" in {
    using(inputs) config myBenchConfig in { case (i, n) =>
      import n._
      nestedLoop3Incr(coll1Init, coll2Init)(res1, res1Cache)(0, N, false)
    }
  }

  performance of "nestedLoopBags1" in {
    using(inputs) config myBenchConfig in { case (i, n) =>
      import n._
      nestedLoopBags1(bag1Init, bag2Init)
    }
  }

  performance of "nestedLoopBags3" in {
    using(inputs) config myBenchConfig in { case (i, n) =>
      import n._
      nestedLoopBags3(bag1Init, bag2Init)
    }
  }

  performance of "nestedLoopBags3Memo" in {
    using(inputs) config myBenchConfig in { case (i, n) =>
      import n._
      nestedLoopBags3Memo(bag1Init, bag2Init)
    }
  }

  performance of "nestedLoopBags3Incr" in {
    using(inputs) config myBenchConfig in { case (i, n) =>
      import n._
      nestedLoopBags3Incr(bag1Init, bag2Init)(resBag1, resBag1Cache)(0, N)
    }
  }
}

import org.scalatest._
class NestedLoop1Test extends FlatSpec {
  val n = new NestedLoop1(3, 4)
  import n._

  "nestedLoop1" should "be equivalent to nestedLoop3 and incremental variants" in {
    val (res1, res1Cache) = nestedLoop3Memo(coll1Init, coll2Init)
    val nl1 = nestedLoop1(coll1Init, coll2Init)
    assert(nl1 == nestedLoop3(coll1Init, coll2Init))
    assert(nl1 == res1)
    assert(nestedLoop3Incr(coll1Init, coll2Init)(res1, res1Cache)(0, M, true) == nestedLoop1(coll1Upd, coll2Init))
  }

  "nestedLoopBags1" should "be equivalent to nestedLoopBags3 and incremental variants" in {
    val (resBag1, resBag1Cache) = nestedLoopBags3Memo(bag1Init, bag2Init)
    val nl1 = nestedLoopBags1(bag1Init, bag2Init)
    assert(nl1 == nestedLoopBags3(bag1Init, bag2Init))
    assert(nl1 == resBag1)
    assert(nestedLoopBags3Incr(bag1Init, bag2Init)(resBag1, resBag1Cache)(0, M) == nestedLoopBags1(bag1Upd, bag2Init))
  }
}
