package ilc
package feature
package functions

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class PrettySuite
extends FunSuite
   with ShouldMatchers
   with Pretty
{
  case object Bot extends Type

  val id = lambda("x") { x => x }
  val id3 = id ! id%(Bot =>: Bot) ! id%Bot

  test("The identity function prints λx. x") {
    val printout = pretty(id%Bot)
    printout should be ("λx. x")
    info(printout)
  }

  test("Left-associative application print without extra parentheses") {
    val printout = pretty(id3)
    printout should be ("(λx. x) (λx. x) (λx. x)")
    info(printout)
  }

  test("Variables are disambiguated with indices.") {
    val shadowed = lambda("x", "x", "x", "x") {
      case Seq(x, x1, x2, x3) => x1
    }
    val printout = pretty(shadowed%(Bot, Bot, Bot, Bot))
    val expected = "λx. λx_1. λx_2. λx_3. x_1"
    printout should be (expected)
  }
}
