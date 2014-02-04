package ilc.feature.inference

import org.scalatest._
import scala.language.implicitConversions

class IntegerSuite
extends FlatSpec
   with Matchers
   with Integers
{
  "Type inference with integers" should "infer the correct type for literal integers" in {
    val i42 = UTerm(LiteralInt(42))
    val (tterm, subst) = collectConstraints(i42)
    assert(tterm === TTerm(LiteralInt(42), IntType))
    assert(subst === emptyConstraintSet)
  }

  it should "not break type inference without integers" in {
    val id: UntypedTerm = UAbs(UVar("x"), UVar("x"))
    val (typedTerm, constraints) = collectConstraints(UApp(id, id), List())
    val solved = unification(constraints)
    val finalTerm = substitute(typedTerm, solved)
    assert(finalTerm.getType === Arrow(TypeVariable(2), TypeVariable(2)))
  }


}

