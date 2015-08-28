package ilc
package language
package bacchus

import feature._
import org.scalatest.FunSuite
import org.scalatest.Matchers

class TypeCheckingSuite extends FunSuite with Matchers {
  object Lang extends Bacchus
  import Lang._

  test("Base type inference gives accurate messages") {
    intercept[base.TypeError] {
      (FoldNat ! LiteralInt(1) ! lambda(NatType) { x => PlusInt ! x ! x }): Term
    }.getMessage() should be ("Type error: PlusInt has type Z -> Z -> Z but should have type N -> N -> _")
  }
}