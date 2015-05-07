package ilc
package examples
package handwritten


import util._
import feature.bags.Library._

import collection.{mutable, immutable}
import org.scalameter.api._

class NestedLoop1 extends Serializable {
  val N = 1000
  val coll1Init = List[Int](0 to N - 1: _*)
  val coll2Init = List[Int](1 to N: _*)

  //On lists first. Bag[T] is a Map[T, Int], so it's less clear how to write this on maps.
  def nestedLoop1(coll1: List[Int], coll2: List[Int]): List[Int] =
    for {
      i <- coll1
      j <- coll2
    } yield (i * N + j)

  def nestedLoop2(coll1: List[Int], coll2: List[Int]): List[Int] = {
    val g = (i: Int) => (j: Int) => i * N + j
    val f = (i: Int) => coll2.map(g(i))
    coll1.flatMap(f)
  }

  def nestedLoop3(coll1: List[Int], coll2: List[Int]): List[Int] = {
    val g = (i: Int) => (j: Int) => i * N + j
    val f = (i: Int) => coll2.map(g(i))
    coll1.map(f).flatten
  }

  def getIdentityMap[Key, Value]: mutable.Map[Key, Value] = {
    import java.util.IdentityHashMap
    import scala.collection.JavaConverters._
    new IdentityHashMap[Key, Value]().asScala
  }

  def memoedF[I, O](cache: mutable.Map[I, O])(f: I => O): I => O =
    x => cache.getOrElseUpdate(x, f(x))

  def memo[A, B](f: A => B): A => B = {
    val cache = getIdentityMap[A, B]
    memoedF(cache)(f)
  }

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

    val toDrop = cache.get(removedFromColl1)
    val toAdd = //Correct result, but too slow with these data structures.
      memoedF(cache)(f)(addedToCol1)

    if (getRightResult)
      oldOut.filterNot(toDrop.contains) ++ toAdd
    else
      //The result here is *not* good. But at least, return all pieces, to prevent optimizations from getting in the way of microbenchmarking.
      (toAdd, toDrop)
  }
}

trait MyBenchmarkingSetup extends BaseBenchmark {
  override def reporters = Seq(LoggingReporter())
  //Config. for real measurements.
  //def myBenchConfig = testConfig
  //To make tests fast, while still having lots of memory
  def myBenchConfig =
    Context(
      //reports.regression.significance -> 0.01, //Confidence level = 99 %
      exec.jvmflags -> s"-Xmx${memorySizeMB}m -Xms${memorySizeMB}m -XX:CompileThreshold=100"
    ) ++ verificationConfig
}


class NestedLoop1Bench extends MyBenchmarkingSetup {
  val n = new NestedLoop1()
  import n._
  val (res1, res1Cache) = nestedLoop3Memo(coll1Init, coll2Init)

  performance of "nestedLoop1" in {
    using(Gen.unit("dummy")) config myBenchConfig in { _ =>
      nestedLoop1(coll1Init, coll2Init)
    }
  }

  performance of "nestedLoop3" in {
    using(Gen.unit("dummy")) config myBenchConfig in { _ =>
      nestedLoop3(coll1Init, coll2Init)
    }
  }

  performance of "nestedLoop3Memo" in {
    using(Gen.unit("dummy")) config myBenchConfig in { _ =>
      nestedLoop3Memo(coll1Init, coll2Init)
    }
  }

  performance of "nestedLoop3Incr" in {
    using(Gen.unit("dummy")) config myBenchConfig in { _ =>
      nestedLoop3Incr(coll1Init, coll2Init)(res1, res1Cache)(0, N, true)
    }
  }

  //Cheat a lot, to get closer to the performance we'd hope with proper data structures.
  performance of "nestedLoop3IncrSimulated" in {
    using(Gen.unit("dummy")) config myBenchConfig in { _ =>
      nestedLoop3Incr(coll1Init, coll2Init)(res1, res1Cache)(0, N, false)
    }
  }
}
