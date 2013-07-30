package ilc
package feature.functions

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class PrettySuite extends FunSuite with ShouldMatchers {
  import feature.Functions._

  val id = Abs("x", Var("x"))

  test("The identity function prints as λx. x") {
    pretty(id) should be ("λx. x")
  }

  test("Left-associative application print without extra parentheses") {
    pretty(App(App(id, id), id)) should be ("(λx. x) (λx. x) (λx. x)")
  }

  test("Variables are disambiguated with indices.") {
    val List(x1, x2, x3) = uniqueNames(Set("x"), "x", "x", "x")
    pretty(Abs("x", Abs(x1, Abs(x2, Abs(x3, Var(x1)))))) should be ("λx. λx₁. λx₂. λx₃. x₁")
  }
}
