package ilc
package examples

import org.scalatest.FunSuite
import MapSuccGenerated._

class MapSuccGeneratedSuite
extends FunSuite
{
  val n = 100

  test("the compiled derivative is correct") {
    import collection.immutable.TreeMap // for sorted print-out
    val old: InputType = (1 to n).map(i => i -> i)(collection.breakOut)
    val res = program(old)
    changes foreach { change =>
      assert(
        TreeMap(updateOutput(derivative(old)(change))(res).toSeq: _*) ===
          TreeMap(program(updateInput(change)(old)).toSeq: _*))
    }
  }

  val changes: List[DeltaInputType] = {
    List(
      Left(Map.empty),
      Left(Map(1 -> Right(n + 1))),
      Left(Map(n + 2 -> Left(Some(n + 2)))),
      Left(Map(2 -> Left(None))),
      Left(Map(n -> Left(None))))
  }
}
