package ilc
package examples

/**
 * Test that the term constructors in MapSucc are defined correctly.
 */

import org.scalatest.FunSuite

class MapSuccSuite extends FunSuite {
  import ilc.language.bacchus._

  val example = new MapSuccExample
      with Subjects with Evaluation with FineGrainedDifference

  import example._

  test("program increments map values") {
    assert(eval(program ! oldMap) ===
      MapValue(1 -> 3, 2 -> 5, 3 -> 7, 4 -> 9))
  }

  test("the derivative of the program is correct") {
    val refinement = derivative ! oldMap ! fineGrainedDiff(newMap, oldMap)
    val replacement = derivative ! oldMap ! mkMapReplacement(newMap)
    val oldOutput = program ! oldMap
    val newOutputValue = eval(program ! newMap)
    assert(eval(ChangeUpdate ! refinement  ! oldOutput) === newOutputValue)
    assert(eval(ChangeUpdate ! replacement ! oldOutput) === newOutputValue)
  }
}
