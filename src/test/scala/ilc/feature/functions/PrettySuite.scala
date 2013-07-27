package ilc
package feature.functions

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class PrettySuite extends FunSuite with ShouldMatchers {
  object Language extends Syntax {
    def deriveConst(constant : Constant) : Term = null
  }

  import Language._

  val id = Abs("x", Var(0))

  test("The identity function prints as λx. x") {
    id.toString should be ("λx. x")
  }

  test("Left-associative application print without extra parentheses") {
    App(App(id, id), id).toString should be ("(λx. x) (λx. x) (λx. x)")
  }

  test("Variables are disambiguated with indices.") {
    Abs("x", Abs("x", Abs("x", Abs("x", Var(2))))).toString should be ("λx. λx₁. λx₂. λx₃. x₁")
  }
}
