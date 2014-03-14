package ilc
package feature
package functions

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.FlatSpec

class BetaReductionSuite
extends FlatSpec
   with Matchers
   with BetaReduction with naturals.Syntax {

  val x = Var("x", NatType)
  val y = Var("y", NatType)
  val z = Var("z", NatType)
  val f = Var("f", NatType =>: NatType)

  val simpleApp = App(lambdaTerm(x) { x }, 1)
  val simpleAppAsLet = Let(x, 1, x)

  val nestedApp = lambdaTerm(y) { App(lambdaTerm(x) { x }, y) }

  val appInLet = Let(f, lambdaTerm(z) { z }, lambdaTerm(y) { Plus ! (f ! y) ! (lambdaTerm(x) { x } ! y) })

  val deadLets = Let(x, 1, Let(y, 2, 3))

  val deadLets2 = Let(x, 1, Let(y, x, 3))

  "letBetaReduceRule" should "turn redexes into lets" in {
    letBetaReduceRuleTotal(simpleApp) should be (simpleAppAsLet)
  }

  it should "not turn redexes into lets inside nested terms" in {
    letBetaReduceRuleTotal(nestedApp) should be (nestedApp)
  }

  "letBetaReduceOneStep" should "turn redexes into lets" in {
    letBetaReduceOneStep(simpleApp) should be (simpleAppAsLet)
  }

  it should "turn redexes into lets inside nested terms" in {
    letBetaReduceOneStep(nestedApp) should be (lambdaTerm(y) { Let(x, y, x) })
  }

  it should "reduce terms inside lets" in {
    letBetaReduceOneStep(appInLet) should be (Let(f, lambdaTerm(z) { z }, lambdaTerm(y) {Plus ! (f ! y) ! Let(x, y, x) }))
  }

  "dce" should "remove dead variables" in {
    dceOneStep(deadLets) should be (3: Term)
    dceOneStep(appInLet) should be (appInLet)
    val v = Let(x, 1, Let(y, x, y))
    dceOneStep(v) should be (v)
    dceOneStep(Let(x, 1, Let(y, x, x))) should be (Let(x, 1, x))
    dceOneStep(deadLets2) should be (3: Term)
  }
}
