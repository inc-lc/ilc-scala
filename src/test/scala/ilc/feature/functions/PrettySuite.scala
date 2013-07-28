package ilc
package feature.functions

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class PrettySuite extends FunSuite with ShouldMatchers {
  object Language extends Syntax

  import Language._

  val id = Abs("x", Var(0))

  test("The identity function prints as λx. x") {
    pretty(id) should be ("λx. x")
  }

  test("Left-associative application print without extra parentheses") {
    pretty(App(App(id, id), id)) should be ("(λx. x) (λx. x) (λx. x)")
  }

  test("Variables are disambiguated with indices.") {
    pretty(Abs("x", Abs("x", Abs("x", Abs("x", Var(2)))))) should be ("λx. λx₁. λx₂. λx₃. x₁")
  }
}
