package ilc.feature.inference

import org.scalatest._
import scala.language.implicitConversions
import ilc.feature._

import shapeless._
import poly._

class FeaturesSuite
  extends FlatSpec
  with Inference
  with PrettySyntax

  with bags.Syntax
  with integers.Syntax
  with maps.Syntax
{
  val (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9) = (TypeVariable(0), TypeVariable(1), TypeVariable(2), TypeVariable(3), TypeVariable(4), TypeVariable(5), TypeVariable(6), TypeVariable(7), TypeVariable(8), TypeVariable(9))

  object dropSourceInfoBase extends ->((t: TypeVariable) => t copy (uterm = None))
  def dropSourceInfo = everywhere(dropSourceInfoBase)
  def collectConstraintsForTest(term: UntypedTerm): (TypedTerm, Set[Constraint]) = dropSourceInfo(collectConstraints(term))

  "Empty map" should "have type schema (Map k v)" in {
    val empty = UPolymorphicConstant(EmptyMap)
    assert(collectConstraints(empty) === ((TPolymorphicConstant(EmptyMap, MapType(t1, t2), Seq(t1, t2)), emptyConstraintSet)))
  }

  "Update" should "have type k → v → Map k v → Map k v" in {
    val update = UPolymorphicConstant(Update)
    assert(collectConstraints(update) === ((TPolymorphicConstant(Update, t3 =>: t4 =>: MapType(t3, t4) =>: MapType(t3, t4), Seq(t3, t4)), emptyConstraintSet)))
  }

  it should "partially applied to one integer should have type a -> Map Int a -> Map Int a" in {
    val i42 = UMonomorphicConstant(LiteralInt(42))
    val update = UPolymorphicConstant(Update)
    val (tt, c) = collectConstraints(UApp(update, i42))
    val solved = unification(c)
    val finalTerm = substitute(tt, solved)
    assert(finalTerm.getType === (t6 =>: MapType(IntType, t6) =>: MapType(IntType, t6)))
  }

  it should "partially applied to two integers should have type Map Int Int -> Map Int Int" in {
    val i42 = UMonomorphicConstant(LiteralInt(42))
    val i43 = UMonomorphicConstant(LiteralInt(43))
    val update = UPolymorphicConstant(Update)
    val (tt, c) = collectConstraints(UApp(UApp(update, i42), i43))
    val solved = unification(c)
    val finalTerm = substitute(tt, solved)
    assert(finalTerm.getType === (MapType(IntType, IntType) =>: MapType(IntType, IntType)))
  }

  it should "have the type Map Int Int when applied to two integers and the empty map" in {
    val i42 = LiteralInt(42)
    val i43 = LiteralInt(43)
    val empty = EmptyMap
    val update = Update
    val (tt, c) = collectConstraints(UApp(UApp(UApp(update, i42), i43), empty))
    val solved = unification(c)
    val finalTerm = substitute(tt, solved)
    assert(finalTerm.getType === MapType(IntType, IntType))
  }

  "Type inference with integers" should "infer the correct type for literal integers" in {
    val i42 = UMonomorphicConstant(LiteralInt(42))
    val (tterm, subst) = collectConstraints(i42)
    assert(tterm === TMonomorphicConstant(LiteralInt(42)))
    assert(subst === emptyConstraintSet)
  }

  "Ascription" should "make the polymorphic identity function monomorphic" in {
    val id: UntypedTerm = TypeAscription('x ->: 'x, IntType =>: IntType)
    val (typedTerm, constraints) = collectConstraints(id, List())
    val solved = unification(constraints)
    val finalTerm = substitute(typedTerm, solved)
    assert(finalTerm.getType === =>:(IntType, IntType))
  }

  it should "also work when only applied to the body" in {
    val id: UntypedTerm = 'x ->: TypeAscription('x, IntType)
    val (typedTerm, constraints) = collectConstraints(id, List())
    val solved = unification(constraints)
    val finalTerm = substitute(typedTerm, solved)
    assert(finalTerm.getType === =>:(IntType, IntType))
  }

  "Type annotation" should "result in the same type" in {
    val id: UntypedTerm = 'x % IntType ->: 'x
    val (typedTerm, constraints) = collectConstraints(id, List())
    val solved = unification(constraints)
    val finalTerm = substitute(typedTerm, solved)
    assert(finalTerm.getType === =>:(IntType, IntType))
  }

  "Type inference for bags" should "produce the same results as the old inference (see MapSuccExample)" in {
    // PolymorphicConstant.apply interferes with the implicit conversion to UntypedTerm and its apply.
    val foldGroup: UntypedTerm = FoldGroup
    val singleton: UntypedTerm = Singleton

    // flatMap : (v → Bag u) → Bag v → Bag u
    val flatMap: UntypedTerm = foldGroup(FreeAbelianGroup)
    //println(inferType(flatMap).getType)

    // map : (a -> b) -> Bag a -> Bag b
    val map: UntypedTerm = 'f ->: flatMap('x ->: singleton('f('x)))
    //println(inferType(map).getType)

    // succ : ℤ → ℤ
    val succ: UntypedTerm = (PlusInt)(LiteralInt(1))
    //println(inferType(succ).getType)

    // program : Bag ℤ -> Bag ℤ
    val program: UntypedTerm = map(succ)
    //println(inferType(program).getType)

    assert(inferType(program).getType === (BagType(IntType) =>: BagType(IntType)))
  }
}
