package ilc.feature.inference

import org.scalatest._
import scala.language.implicitConversions
import ilc.examples.Example
import ilc.feature._

class InferenceSuite
extends FlatSpec
   with Matchers
   with Inference

// Stuff for testing old inference
with Pretty
with bags.SyntaxSugar

// context-sensitive derivations
with bags.AbelianDerivation
with integers.AbelianDerivation

// code generation
with bags.ToScala
with booleans.ToScala
with functions.ToScala
with integers.ToScala
with products.ToScala
with sums.ToScala
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
    assert(unification(Set((t1, =>:(t2, t3)))) === unification(Set((=>:(t2, t3), t1))))
  }

  it should "pass the Wand example" in {
    val ex = Set[(Type, Type)](
      (t0, =>:(t1, t2)),
      (t2, =>:(t3, t4)),
      (t4, =>:(t5, t6)),
      (t1, =>:(t8, =>:(t7, t6))),
      (t8, t5),
      (=>:(t9, t7), t3),
      (t9, t5))
    val subst = unification(ex)
    assert(substitute(subst)(t0) === ((t5 =>: t7 =>: t6) =>: (t5 =>: t7) =>: (t5 =>: t6)))
  }

  it should "fail for inputs where no substitution is possible" in {
    a [UnificationFailure] should be thrownBy {
      unification(Set((t0, =>:(t1, t2)), (t0, t1)))
    }
  }

  "Type inference" should "infer α -> α for (id id)" in {
//    val id: UntypedTerm = UAbs("x", None, UVar("x"))
//    val (typedTerm, constraints) = collectConstraints(UApp(id, id), List())
//    val solved = unification(constraints)
//    val finalTerm = substitute(typedTerm, solved)
//    assert(finalTerm.getType === =>:(TypeVariable(2), TypeVariable(2)))
  }

  it should "produce the same results as the old inference" in {
    val programOld: Term = map ! (PlusInt ! LiteralInt(1))
    // Oh ffs. map is not a polymorphic constant but a polymorphic term.
    // I'm a bit lost now...
    val programNew: UntypedTerm = UPolymorphicConstant(map).apply(((PlusInt)(LiteralInt(1))))
    assert(programOld === untypedTermToTerm(programNew))
  }
}

