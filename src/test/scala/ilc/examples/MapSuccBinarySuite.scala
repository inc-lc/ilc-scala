package ilc
package examples

import org.scalatest.FunSuite
/*
import MapSuccBinary._

class MapSuccBinarySuite
extends FunSuite
   with MapSuccTypes
{
  val n = 100

  test("the compiled derivative is correct") {
    val old: Data = (1 to n).map(i => i -> i)(collection.breakOut)
    val res = program(old)
    changes foreach { change =>
      assert(
        applyChange(derivative(old)(change), res) ===
          program(applyChange(change, old)))
    }
  }

  val changes: List[Change] = {
    List(
      Left(Map.empty),
      Left(Map(1 -> Right(Right(n + 1)))),
      Left(Map(n + 2 -> Left(Right(n + 2)))),
      Left(Map(2 -> Left(Left(())))),
      Left(Map(n -> Left(Left(())))))
  }
}*/
