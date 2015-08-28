package ilc
package language
package bacchus

import org.scalatest.FunSuite
import org.scalatest.Matchers

class BacchusPrettySuite
extends FunSuite
   with Matchers
{
  object Lang extends Evaluation with feature.base.Pretty
  import Lang._

  test("values have short descriptions") {
    NatValue(9).toString should be("9")
    MapValue(1 -> 4).toString should be("Map(1 -> 4)")
    SumValue(Left(5)).toString should be("Inj1(5)")
    SumValue(Right(2)).toString should be("Inj2(2)")
    MaybeValue(None).toString should be("Nope")
    MaybeValue(Some(5)).toString should be("Just(5)")
  }
}
