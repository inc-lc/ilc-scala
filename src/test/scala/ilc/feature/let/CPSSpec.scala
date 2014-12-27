package ilc
package feature
package let

import org.scalatest._

trait CPSTestingHelper extends Instantiations {
  val bacchusSystem = buildBacchusWithLetSystem(true, true, true)
  import bacchusSystem._

  def testCPS(t: Term, callToCps: Boolean = true) = {
    val typ = t.getType
    verboseShowTerm(t, "source")

    println(s"Type from CBPV CPS transformation: ${cbvTypeToCPS(typ)}")
    println()

    println("Untyped CPS transformation plus type inference")

    val untypedCPS = asTerm(toCPSU(t)) //XXX without asTerm, inference will be repeated (and give equivalent but different results).
    val cpsInferredType = untypedCPS.getType

    verboseShowTerm(untypedCPS, "untyped CPS")
    println()

    println("Typed CPS transformation without type inference")

    val cpsTau = cpsTransformType(typ)
    println(s"Expected CPS result type: ${cpsTau}")
    println(s"Unifying result of type inference with expected type: ${unification(Set(Constraint(cpsInferredType, cpsTau, "")))}")

    val typedCPS = toCPS(t)
    verboseShowTerm(typedCPS, "typed CPS")
    //XXX how do I reuse this inside and outside tests elegantly (that is, without the kludge of abstracting over assert)?
    assert(cpsTau == cpsTransformType(typ))
  }

  val examples: List[Term] =
    List(
      'x ->: 'x,
      'f ->: 'x ->: 'f('x),
      'f ->: 'x ->: 'y ->: 'f('y)('x),
      //From Plotkin's paper; they show that eta-equivalence is not preserved.
      //I needed to correct the second one, assuming there's a typo and
      //eta-expansion is intended.
      'y ->: 'x ->: 'x('y('x)),
      'y ->: 'x ->: 'x('y('z ->: 'x('z)))) map (asTerm(_))
  val tst0 = examples(0)
  val tst1 = examples(1)
  val tst2 = examples(2)
  val tstA = examples(3)
  val tstB = examples(4) //eta-expanded tstA
}

class CPSSpec extends FlatSpec with CPSTestingHelper {
  import bacchusSystem._

  "cps" should "work on all examples" in {
    for (example <- examples)
      testCPS(example)
  }
}