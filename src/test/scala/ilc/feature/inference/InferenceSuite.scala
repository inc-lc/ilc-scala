package ilc.feature.inference

import org.scalatest._
import scala.language.implicitConversions

class InferenceSuite
extends FlatSpec
   with Matchers
   with Inference
{
  val (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9) = (TypeVariable(0), TypeVariable(1), TypeVariable(2), TypeVariable(3), TypeVariable(4), TypeVariable(5), TypeVariable(6), TypeVariable(7), TypeVariable(8), TypeVariable(9))

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

  it should "be symmetric" in {
    assert(unification(Set((t1, Arrow(t2, t3)))) === unification(Set((Arrow(t2, t3), t1))))
  }

  it should "pass the Wand example" in {

    val ex = Set[(InferredType, InferredType)](
      (t0, Arrow(t1, t2)),
      (t2, Arrow(t3, t4)),
      (t4, Arrow(t5, t6)),
      (t1, Arrow(t8, Arrow(t7, t6))),
      (t8, t5),
      (Arrow(t9, t7), t3),
      (t9, t5))
    val subst = unification(ex)
    println(subst)
    println(substitute(t0, subst))
    // TODO decide on something, see below
    fail()
    /* We get:
    Substitutions: Map(TypeVariable(0) -> Arrow(TypeVariable(1),Arrow(Arrow(TypeVariable(9),TypeVariable(7)),Arrow(TypeVariable(5),TypeVariable(6)))),
                       TypeVariable(4) -> Arrow(TypeVariable(5),TypeVariable(6)),
                       TypeVariable(1) -> Arrow(TypeVariable(5),Arrow(TypeVariable(7),TypeVariable(6))),
                       TypeVariable(3) -> Arrow(TypeVariable(9),TypeVariable(7)),
                       TypeVariable(8) -> TypeVariable(5),
                       TypeVariable(9) -> TypeVariable(5),
                       TypeVariable(2) -> Arrow(Arrow(TypeVariable(9),TypeVariable(7)),Arrow(TypeVariable(5),TypeVariable(6))))

      Substitutions applied to t0: Arrow(TypeVariable(1),Arrow(Arrow(TypeVariable(9),TypeVariable(7)),Arrow(TypeVariable(5),TypeVariable(6))))

      Expected: doSubst s t0 @?= (t5 =:> t7 =:> t6) =:> (t5 =:> t7) =:> (t5 =:> t6)

      It looks like we are almost correct, but missing some substitutions.
      (t1 == t5 => t7 => t6, and t9 == t5)
      Question is: is this a problem?
      Probably yes, because more arrows are more specific.
      Is it a problem in practice?
      We only use unification on constraints collected from the type checker. Maybe the example violates some implicit invariant. Though we should probably check for that.
      Where does it come from, and how to fix it?
      Something to do with ordering, perhaps? Should we use a list of constraints instead of a set?
     */
  }

  it should "fail for inputs where no substitution is possible" in {
    a [UnificationFailure] should be thrownBy {
      unification(Set((t0, Arrow(t1, t2)), (t0, t1)))
    }
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

