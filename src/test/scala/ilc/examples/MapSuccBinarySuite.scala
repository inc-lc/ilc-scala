package ilc
package examples

import org.scalatest.FunSuite
import ilc.feature.bags.BagChanges
import MapSuccGenerated._

class MapSuccGeneratedSuite
extends FunSuite
   with BagChanges
{
  val n = 100

  val oldInput: InputType = (1 to n).map(i => i -> 1)(collection.breakOut)
  val oldOutput = program(oldInput)

  test("the compiled derivative increments collections") {
    val expected: OutputType = for {
      (element, multiplicity) <- oldInput
    } yield (element + 1, multiplicity)
    assert(oldOutput === expected)
  }

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

  val changes: Iterable[ChangeToBags[Int]] =
    changesToBagsOfIntegers.map(_ _2 n)

}
