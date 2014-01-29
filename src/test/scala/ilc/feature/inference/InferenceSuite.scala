package ilc.feature.inference

import org.scalatest._
import scala.language.implicitConversions

class InferenceSuite
extends FlatSpec
   with Inference
{
  "Unification" should "produce no substitutions for equal constraints" in {
    val s: Set[Constraint] = Set((TypeVariable(1), TypeVariable(1)))
    assert(unification(s) === Map())
  }

  it should "map 1 to 2 for a constraint that 1 should be equal to 2" in {
    val s: Set[Constraint] = Set((TypeVariable(1), TypeVariable(2)))
    assert(unification(s) === Map((TypeVariable(1), TypeVariable(2))))
  }

  it should "map 2 to 1 for a constraint that 2 should be equal to 1" in {
    val s: Set[Constraint] = Set((TypeVariable(2), TypeVariable(1)))
    assert(unification(s) === Map((TypeVariable(2), TypeVariable(1))))
  }

  "Type inference" should "infer α -> α for (id id)" in {
    val id: UntypedTerm = UAbs(UVar("x"), UVar("x"))
    val (typedTerm, constraints) = collectConstraints(UApp(id, id), List())
    println(typedTerm)
    val solved = unification(constraints)
    val finalTerm = substitute(typedTerm, solved)
    finalTerm match {
      case TApp(t1, t2, typ) => println(t1.getType)
    }

    println(finalTerm)
    assert(finalTerm.getType === Arrow(TypeVariable(2), TypeVariable(2)))
  }
}
