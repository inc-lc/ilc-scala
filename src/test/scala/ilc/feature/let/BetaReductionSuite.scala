package ilc
package feature
package let

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import ilc.util.EvalGenerated

class BetaReductionSuite
extends FlatSpec
   with Matchers
   with BetaReduction with naturals.ImplicitSyntaxSugar with ToScala with ProgramSize with naturals.ToScala with EvalGenerated {

  val x = Var("x", NatType)
  val y = Var("y", NatType)
  val z = Var("z", NatType)
  val f = Var("f", NatType =>: NatType)

  val simpleApp = App(lambdaTerm(x) { x }, 1)
  val simpleAppAsLet = Let(x, 1, x)

  val nestedApp = lambdaTerm(y) { App(lambdaTerm(x) { x }, y) }

  val appInLet = Let(f, lambdaTerm(z) { z }, lambdaTerm(y) { PlusNat ! (f ! y) ! (lambdaTerm(x) { x } ! y) })

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
    letBetaReduceOneStep(appInLet) should be (Let(f, lambdaTerm(z) { z }, lambdaTerm(y) {PlusNat ! (f ! y) ! Let(x, y, x) }))
  }

  "dce" should "remove dead variables" in {
    dceOneStep(deadLets) should be (3: Term)
    dceOneStep(appInLet) should be (appInLet)
    val v = Let(x, 1, Let(y, x, y))
    dceOneStep(v) should be (v)
    dceOneStep(Let(x, 1, Let(y, x, x))) should be (Let(x, 1, x))
    dceOneStep(deadLets2) should be (3: Term)
  }

  val normalAppInLet = normalizeEverywhereOnce(appInLet)

  "normalizeEverywhereOnce" should "work" in {
    println(normalAppInLet)
  }

  "toScala" should "pretty-print working programs even with lets" in {
    val exprGen = letBetaReduceOneStep(normalizeEverywhereOnce(normalAppInLet ! 2))
    println(toScala(exprGen))
    evalGenerated(exprGen) should be (4)
  }

  "size" should "return correct results on simple programs" in {
    termSize(deadLets) should be (7)
    termSize(appInLet) should be (18)
  }

  val t =
    lambda(Var("param", NatType)) { param =>
      lambda(Var("expensive", NatType)) { expensive =>
        lambda(NatType) { _ =>
          expensive }
      } }
  val u = t ! 1 ! (PlusNat ! 42 ! 84)
  val duplicating = lambda(Var("f", NatType =>: NatType), Var("expensive", NatType)) {
    case Seq(arg, expensive) => PlusNat ! (arg ! expensive) ! (arg ! expensive)
  } ! u ! (PlusNat ! 42 ! 84)
  pretty(duplicating)
  pretty(normalize(duplicating))
  /*
  val t =
    lambda(Var("param", NatType)) { param =>
      lambda(Var("expensive", NatType)) { expensive =>
        lambda(NatType) { _ => expensive }
      } ! (PlusNat ! param ! param) }
  val u = t ! 1
  val duplicating = lambda(Var("f", NatType =>: NatType)) { arg => PlusNat ! (arg ! z) ! (arg ! z) } ! u
   */
}
