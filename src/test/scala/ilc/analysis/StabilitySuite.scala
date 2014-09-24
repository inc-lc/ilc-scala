package ilc
package analysis

import org.scalatest.FunSuite
import ilc.feature._

class StabilitySuite
extends FunSuite
   with Stability
   with functions.Pretty
   with naturals.Syntax
{
  val app = lambda("f", "x") { case Seq(f, x) => f ! x }

  val appType = (ℕ =>: ℕ) =>: ℕ =>: ℕ

  // app2 = (λ f x → f x) (λ f x → f x)
  val app2: Subtree =
    Subtree.ofRoot((app ofType appType =>: appType) ! (app ofType appType))

  val variables = app2.children flatMap
    (_.children) flatMap (_.children) flatMap (_.children)

  val Seq(f1, x1, f2, x2) = variables

  //x should be an application node.
  def testNavigation(x: Subtree) {
    assert(goLeft(goRight(x.children(0))) == x.children(0))
    assert(goRight(goLeft(x.children(1))) == x.children(1))
  }

  test("goLeft and goRight are partial inverses") {
    testNavigation(app2)
    testNavigation(app2.children(0).children(0).children(0))
    testNavigation(app2.children(1).children(0).children(0))
    //testNavigation(app2.children(1).children(0))
  }

  test("app2's bound variables are stable, unstable, unstable, unstable") {
    assert(variables.map(_.isStable) === Seq(true, false, false, false))
  }

  test("app2's bound variables have no stable argument") {
    assert(variables.map(_.hasStableArgument(0)) ===
      Seq(false, false, false, false))
  }

  test("app2's operator has a stable argument") {
    val Seq(operator, operand) = app2.children
    assert(Range(0, 4).map(i => operator.hasStableArgument(i)) ===
      Seq(true, false, false, false))
    assert(Range(0, 4).map(i => operand.hasStableArgument(i)) ===
      Seq(false, false, false, false))
  }

  test("free variables are unstable") {
    assert(Subtree.ofRoot(Var("x", ℕ)).isStable === false)
  }
}
