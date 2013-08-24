package ilc
package feature.functions

import org.scalatest.FunSuite

class StabilitySuite extends FunSuite {
  import feature.Functions._

  val omega: Subterm = {
    val x = Var("x")
    Subterm refl (x ->: x(x))(x ->: x(x))
  }

  val (stable, stableArg) = StabilityAttr(omega.term).split

  test("omega's bound variables are stable, stable, unstable, unstable") {
    val vars = omega.children.
      flatMap(_.children).flatMap(_.children).toList
    assert(vars.map(x => stable(x)) === List(true, true, false, false))
  }

  test("omega's operator receives one stable argument") {
    val List(operator, operand) = omega.children.toList
    assert(Range(0,4).map(i => stableArg(operator, i)).toList ===
           List(true, false, false, false))
    assert(Range(0,4).map(i => stableArg(operand, i)).toList ===
           List(false, false, false, false))
  }
}

