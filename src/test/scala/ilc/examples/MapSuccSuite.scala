package ilc
package examples

/**
 * Test that the term constructors in MapSucc are defined correctly.
 */

import org.scalatest.FunSuite
import ilc.language.Bacchus
import ilc.language.bacchus.Subjects._
import ilc.language.bacchus.Tools

class MapSuccSuite
extends FunSuite
   with Tools
   with MapSucc
{
  val ex = new MapSuccExample
  import ex._
  import ex.calculus._

  test("succ increments naturals") {
    Range(0, 20) foreach { i =>
      assert(eval(succ(i)).toNat === i + 1)
    }
  }

  test("dsucc increments replacement naturals") {
    ((1 to 20), (100 to 120)).zipped foreach { (i, j) =>
      assert(eval(dsucc(i)(Right(j))).toSum ===
        scala.Right(Value.Nat(j + 1)))
    }
  }

  test("program increments map values") {
    assert(eval(program(twiceMap1234)).toMap ===
      ValueMap(1 -> 3, 2 -> 5, 3 -> 7, 4 -> 9))
  }

  test("the derivative of the program is correct") {
    val oldMap = eval(twiceMap1234)
    val newMap = eval(twiceMap1256)
    val refinement = bacchusDiff(newMap, oldMap)
    val replacement = Value.diff(newMap, oldMap)
    val f = eval(program)
    val df = eval(derivative)
    assert(Value.apply(df(oldMap)(replacement), f(oldMap)) === f(newMap))
    assert(Value.apply(df(oldMap)(refinement), f(oldMap)) === f(newMap))
  }
}
