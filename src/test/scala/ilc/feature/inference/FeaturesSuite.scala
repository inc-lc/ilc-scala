package ilc.feature.inference

import org.scalatest._
import scala.language.implicitConversions
import ilc.feature._

import shapeless._
import poly._

// XXX cleanup needed. It's not clear that using shapeless bought us anything.
trait InferenceTestHelper
  extends Inference
  with PrettySyntax
  with base.Pretty
{

  def onTypes[T](transformer: Type => Type): T => T = {
    //The pattern matching cannot distinguish this.Type from (something else).Type.
    //Won't be a problem as long as you don't mix different Types in the same tree.
    case subType: Type @unchecked => transformer(subType).asInstanceOf[T]
    case notType: Product => mapSubtrees(transformer)(notType).asInstanceOf[T]
    case v: Traversable[u] => (v map onTypes(transformer)).asInstanceOf[T]
    case notProduct => notProduct
  }

  /**
   * Take a transformer and a term, and apply transformer to each subterm of term.
   * @param transformer
   */
  def mapSubtrees[T <: Product](transformer: Type => Type): T => T =
    typ => {
      val subTypes = typ.productIterator.toList map onTypes(transformer)
      reflectiveCopy(typ, subTypes: _*)
    }

  /**
   * Apply transformer to a Type bottom-up: transformer is applied to each leave,
   * then the parent node is rebuilt with the transformed leaves, then the
   * transformer is applied to the newly constructed nodes, and so forth.
   * The traversal algorithm is the same as a fold.
   *
   * If you want to implement a rewrite system, this might not be enough — you
   * might need to implement fix-point iteration, if a single rule needs to be
   * applied more than once in the same position. Since in my experience most
   * rules must be applied at most once, this is left to the rules themselves.
   *
   * Beta-reduction is a typical example of a rule needing fixpoint iteration.
   */
  def traverse[T <: Product](transformer: Type => Type): T => T =
    typ =>
      onTypes(transformer)(mapSubtrees(traverse(transformer))(typ))

  //object dropSourceInfoBase extends ->((t: TypeVariable) => t copy (uterm = None))
  //val dropSourceInfoType = (t: Type) => everywhere(dropSourceInfoBase)(t)
  object dropSourceInfoBase extends ->((t: TypeVariable) => t copy (uterm = None))
  def dropSourceInfoT[T <: Product] = traverse[T] {
    case t: TypeVariable => t copy (uterm = None)
    case t => t
  }

  private def debugDropSourceInfoT[T <: Product](nm: String)(input: T): T = {
    val output = dropSourceInfoT(input)
    //println(s"$nm $input $output")
    output
  }

  object dropSourceInfoPolyFun extends Poly1 /*->(dropSourceInfoType)*/ {
    //implicit def default[T] = at[T] { t => dropSourceInfoE(t) }
    implicit def atProduct[T <: Product] = at[T] { debugDropSourceInfoT("atProduct") }

    /*implicit def caseTypeV = at[TypeVariable] { t => println("caseTypeV " + t); dropSourceInfoBase(t) }
    implicit def caseSet[T](implicit st: Case.Aux[T, T]) = at[Set[T]] { set => println("caseSet " + set); everywhere(dropSourceInfo)(set.toList).toSet }
    implicit def caseVector[T](implicit st: Case.Aux[T, T]) = at[Vector[T]] { vector => println("caseVector " + vector); everywhere(dropSourceInfo)(vector.toList).toVector }


    implicit def caseT2 = at[TPolymorphicConstant] { t => println("caseT2 " + t); dropSourceInfoT(t) }
    implicit def caseT3 = at[TypedTerm] { debugDropSourceInfoT("caseT3") }
    implicit def caseType = at[Type] { debugDropSourceInfoT("caseType") }*/
  }
  //object dropSourceInfo extends ->(dropSourceInfoType)
  val dropSourceInfo = everywhere(dropSourceInfoPolyFun)
}

class FeaturesSuite
  extends FlatSpec
  with InferenceTestHelper

  with bags.Syntax
  with integers.Syntax
  with maps.Syntax
{
  val (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9) = (TypeVariable(0), TypeVariable(1), TypeVariable(2), TypeVariable(3), TypeVariable(4), TypeVariable(5), TypeVariable(6), TypeVariable(7), TypeVariable(8), TypeVariable(9))

  "dropSourceInfoE" should "work" in {
    val tv = TypeVariable(1, Some('x ->: 'x))
    val t = tv: Type
    assert(dropSourceInfoT(tv) === TypeVariable(1, None))
    assert(dropSourceInfoT(t) === TypeVariable(1, None))
    assert(dropSourceInfo((t, 2)) === ((TypeVariable(1, None), 2)))
    assert(dropSourceInfo((List(t), 2)) === ((List(TypeVariable(1, None)), 2)))
  }

  it should "work more" in {
    val tv = TypeVariable(1, Some('x ->: 'x))
    assert(dropSourceInfo(TPolymorphicConstant(EmptyMap, MapType(tv, tv), List(tv))) === (TPolymorphicConstant(EmptyMap, MapType(t1, t1), List(t1))))
  }
  //Does not work:
  //def collectConstraintsForTestForTest(term: UntypedTerm): (TypedTerm, Set[Constraint]) = everywhere(dropSourceInfo: Poly1)(collectConstraintsForTest(term))
  //Remember: the type annotation affects the bidirectional type checking algorithm.

  def collectConstraintsForTest(term: UntypedTerm) = {
    //val (term2, constraints) = collectConstraints(term)
    //val (term3, constraints_) = dropSourceInfoE((term2, constraints.toList))
    //(term3, constraints_.toSet)
    dropSourceInfo(collectConstraints(term))
  }

  "Empty map" should "have type schema (Map k v)" in {
    val empty = UPolymorphicConstant(EmptyMap)
    assert(collectConstraintsForTest(empty) === ((TPolymorphicConstant(EmptyMap, MapType(t1, t2), Seq(t1, t2)), emptyConstraintSet)))
  }

  "Update" should "have type k → v → Map k v → Map k v" in {
    val update = UPolymorphicConstant(Update)
    assert(collectConstraintsForTest(update) === ((TPolymorphicConstant(Update, t3 =>: t4 =>: MapType(t3, t4) =>: MapType(t3, t4), Seq(t3, t4)), emptyConstraintSet)))
  }

  it should "partially applied to one integer should have type a -> Map Int a -> Map Int a" in {
    val i42 = UMonomorphicConstant(LiteralInt(42))
    val update = UPolymorphicConstant(Update)
    val (tt, c) = collectConstraints(UApp(update, i42))
    val solved = unification(c)
    val finalTerm = substitute(solved, tt)
    assert(dropSourceInfoT(finalTerm.getType) === (t6 =>: MapType(IntType, t6) =>: MapType(IntType, t6)))
  }

  it should "partially applied to two integers should have type Map Int Int -> Map Int Int" in {
    val i42 = UMonomorphicConstant(LiteralInt(42))
    val i43 = UMonomorphicConstant(LiteralInt(43))
    val update = UPolymorphicConstant(Update)
    val (tt, c) = collectConstraints(UApp(UApp(update, i42), i43))
    val solved = unification(c)
    val finalTerm = substitute(solved, tt)
    assert(finalTerm.getType === (MapType(IntType, IntType) =>: MapType(IntType, IntType)))
  }

  it should "have the type Map Int Int when applied to two integers and the empty map" in {
    val i42 = LiteralInt(42)
    val i43 = LiteralInt(43)
    val empty = EmptyMap
    val update = Update
    val (tt, c) = collectConstraints(UApp(UApp(UApp(update, i42), i43), empty))
    val solved = unification(c)
    val finalTerm = substitute(solved, tt)
    assert(finalTerm.getType === MapType(IntType, IntType))
  }

  "Type inference with integers" should "infer the correct type for literal integers" in {
    val i42 = UMonomorphicConstant(LiteralInt(42))
    val (tterm, subst) = collectConstraintsForTest(i42)
    assert(tterm === TMonomorphicConstant(LiteralInt(42)))
    assert(subst === emptyConstraintSet)
  }

  "Ascription" should "make the polymorphic identity function monomorphic" in {
    val id: UntypedTerm = TypeAscription('x ->: 'x, IntType =>: IntType)
    val (typedTerm, constraints) = collectConstraints(id)
    val solved = unification(constraints)
    val finalTerm = substitute(solved, typedTerm)
    assert(finalTerm.getType === (IntType =>: IntType))
  }

  it should "also work when only applied to the body" in {
    val id: UntypedTerm = 'x ->: TypeAscription('x, IntType)
    val (typedTerm, constraints) = collectConstraints(id)
    val solved = unification(constraints)
    val finalTerm = substitute(solved, typedTerm)
    assert(finalTerm.getType === (IntType =>: IntType))
  }

  "Type annotation" should "result in the same type" in {
    val id: UntypedTerm = 'x % IntType ->: 'x
    val (typedTerm, constraints) = collectConstraintsForTest(id)
    val solved = unification(constraints)
    val finalTerm = substitute(solved, typedTerm)
    assert(finalTerm.getType === (IntType =>: IntType))
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
