package ilc
package examples
package handwritten

import feature.bags.Library._
import org.scalameter.api._

class NestedLoop1 extends Serializable {
  val N = 10
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
}

class NestedLoop1Bench extends BaseBenchmark {
  val n = new NestedLoop1()
  import n._

  performance of "nestedLoop1" in {
    using(Gen.unit("dummy")) config testConfig in { _ =>
      nestedLoop1(coll1Init, coll2Init)
    }
  }
}
