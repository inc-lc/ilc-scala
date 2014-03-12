package ilc
package examples

import org.scalatest.FunSuite
import org.scalatest.Matchers
import ilc.examples.HistogramGenerated._
import ilc.feature.abelianMaps.Library._
import ilc.feature.bags.Library._

class HistogramGeneratedSuite
extends FunSuite
   with Matchers
{
  val input: InputType =
    AbelianMap(
      1 -> Bag(1, 2, 3, 4, 5),
      2 -> Bag(   2, 3, 4   ),
      3 -> Bag(      3      )
    )

  test("Histogram is correct on a toy example") {
    program(input) should be(AbelianMap(
      1 -> 1,
      2 -> 2,
      3 -> 3,
      4 -> 2,
      5 -> 1
    ))
  }
}
