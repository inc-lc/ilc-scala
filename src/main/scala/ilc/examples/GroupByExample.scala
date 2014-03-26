package ilc
package examples

import feature._

trait GroupBy
extends abelianMaps.AbelianDerivation
   with bags.StdLib
   with abelianMaps.ToScala
   with bags.AbelianDerivation
   with bags.ToScala
{
  /** `groupBy` operation on bags:
    *
    *   groupByGen : ∀t k v. (t → k) → (t → v) → Bag t → Map k (Bag v)
    *   groupByGen =
    *     Λt k v.  λf : t → k.  λg : t → v.
    *       foldGroup (liftGroup FreeAbelianGroup)
    *                 (λe : t. singletonMap (f e) (singleton (g e)))
    *
    * where
    *
    *   singletonMap : k → v → Map k v
    *      singleton : v → Bag v
    *
    * The type of primitives such as `foldGroup` can be looked up in
    *   src/main/scala/ilc/feature/bags/Syntax.scala             and
    *   src/main/scala/ilc/feature/abelianMaps/Syntax.scala
    */
  val groupByGen: UntypedTerm =
    'f ->: 'g ->:
      foldGroup(
        liftGroup(FreeAbelianGroup),
        'e ->: singletonMap('f('e), singleton('g('e))))

  private val singletonMap: UntypedTerm = SingletonMap
  private val liftGroup: UntypedTerm = LiftGroup
}

/**
  * Exemplify using GroupBy. This primitive is important because it is useful for indexing.
  */
class GroupByExample
extends Example
   with MapReduce
   with GroupBy
   with integers.SyntaxSugar
   with integers.AbelianDerivation
   with integers.ToScala
{
  val ℤ = IntType

  /** An ascription of `groupByGen`, instantiating it to a simply
    * typed term.
    *
    * program =
    *   groupByGen : ((ℤ × ℤ) → ℤ) → ((ℤ × ℤ) → ℤ) →
    *                  Bag (ℤ × ℤ) → Map ℤ (Bag ℤ)
    */
  val untypedProgram: UntypedTerm =
    groupByGen ofType
      ((ProductType(ℤ, ℤ) =>: ℤ) =>: (ProductType(ℤ, ℤ) =>: ℤ)
        =>: BagType(ProductType(ℤ, ℤ)) =>: MapType(ℤ, BagType(ℤ)))

  val program: Term = untypedTermToTerm(untypedProgram)
}
