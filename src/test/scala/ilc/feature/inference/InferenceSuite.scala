package ilc.feature.inference

import org.scalatest._
import ilc.feature._

class InferenceSuite
extends FlatSpec
   with Inference
   with SyntaxSugar
   with LetSyntaxSugar
   with LetInference
   with LetRecInference
   with base.Pretty

// Stuff for testing old inference
with InferenceTestHelper
with bags.Syntax
with integers.ImplicitSyntaxSugar
{
  val (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9) = (TypeVariable(0), TypeVariable(1), TypeVariable(2), TypeVariable(3), TypeVariable(4), TypeVariable(5), TypeVariable(6), TypeVariable(7), TypeVariable(8), TypeVariable(9))

  "Unification" should "produce no substitutions for equal constraints" in {
    val s: Set[Constraint] = Set(Constraint(TypeVariable(1), TypeVariable(1)))
    assert(unification(s) === Map())
  }

  it should "map 1 to 2 for a constraint that 1 should be equal to 2" in {
    val s: Set[Constraint] = Set(Constraint(TypeVariable(1), TypeVariable(2)))
    assert(unification(s) === Map((TypeVariable(1), TypeVariable(2))))
  }

  it should "map 2 to 1 for a constraint that 2 should be equal to 1" in {
    val s: Set[Constraint] = Set(Constraint(TypeVariable(2), TypeVariable(1)))
    assert(unification(s) === Map((TypeVariable(2), TypeVariable(1))))
  }

  it should "be symmetric" in {
    assert(unification(Set(Constraint(t1, =>:(t2, t3)))) === unification(Set(Constraint(=>:(t2, t3), t1))))
  }

  it should "pass the Wand example" in {
    val ex = Set(
      (t0, =>:(t1, t2)),
      (t2, =>:(t3, t4)),
      (t4, =>:(t5, t6)),
      (t1, =>:(t8, =>:(t7, t6))),
      (t8, t5),
      (=>:(t9, t7), t3),
      (t9, t5)) map Function.tupled(Constraint(_, _))
    val subst = unification(ex)
    assert(substituteInType(subst)(t0) === ((t5 =>: t7 =>: t6) =>: (t5 =>: t7) =>: (t5 =>: t6)))
  }

  it should "fail for inputs where no substitution is possible" in {
    intercept[UnificationFailure] {
      unification(Set(Constraint(t0, =>:(t1, t2)), Constraint(t0, t1)))
    }
  }

  "Type inference" should "infer α -> α for (id id)" in {
    val id: UntypedTerm = UAbs("x", None, UVar("x"))
    val (typedTerm, constraints) = collectConstraints(UApp(id, id))
    val solved = unification(constraints)
    val finalTerm = substitute(solved, typedTerm)
    assert(dropSourceInfo(finalTerm.getType) === (TypeVariable(2) =>: TypeVariable(2)))
  }

  it should "work on open terms" in {
    val vU: UntypedTerm = UVar("x")
    val vT: Term = vU
    assert(dropSourceInfo(vT.getType) === TypeVariable(4))
  }
  it should "assign consistent types for repeated variables" in {
    val vU: UntypedTerm = UApp(UVar("x"), UVar("x"))
    intercept[Throwable] {
      typecheck(vU)
    }
  }

  it should "work on LetRec" in {
    val vU = ULetRec(List(("fac", 'x ->: 'fac('fac('x)))), "", 'fac(1: Term))
    typecheck(vU)
  }

  /*
   * When inferring principal typings, the thing I keep forgetting is dropping
   * identifiers from contexts, and it shows up only when it tries to unify
   * types for different bound variables sharing a name.
   * Hence the two tests below.
   */
  it should "not relate identifiers bounds in different Lets" in {
    val vU =
      letS(
      'h := 'x ->: letS('f := 'x ->: 'x)('f(1: Term)),
      'i := 'x ->: letS('f := 'x ->: 'x)('f(EmptyBag)))('h)
    typecheck(vU)
  }

  it should "not relate identifiers bounds in different LetRec" in {
    val vU =
      letS(
      'g := 'x ->: ULetRec(List(("f", 'x ->: 'f('f('x)))), "", 'f(1: Term)),
      'h := 'x ->: ULetRec(List(("f", 'x ->: 'f('f('x)))), "", 'f(EmptyBag)))('g)
    typecheck(vU)
  }
}

