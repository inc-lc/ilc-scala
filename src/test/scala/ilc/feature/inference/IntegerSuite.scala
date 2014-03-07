package ilc.feature.inference

import org.scalatest._
import scala.language.implicitConversions

class IntegerSuite
extends FlatSpec
   with ilc.feature.integers.Syntax
   with Pretty
   with Matchers
{
  "Type inference with integers" should "infer the correct type for literal integers" in {
    val i42 = UMonomorphicConstant(LiteralInt(42))
    val (tterm, subst) = collectConstraints(i42)
    assert(tterm === TMonomorphicConstant(LiteralInt(42)))
    assert(subst === emptyConstraintSet)
  }

  it should "not break type inference without integers" in {
    val id: UntypedTerm = UAbs(UVar("x"), UVar("x"))
    val (typedTerm, constraints) = collectConstraints(UApp(id, id), List())
    val solved = unification(constraints)
    val finalTerm = substitute(typedTerm, solved)
    assert(finalTerm.getType === =>:(TypeVariable(2), TypeVariable(2)))
  }
}

