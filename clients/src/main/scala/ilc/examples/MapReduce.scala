package ilc
package examples

import feature._

trait MapReduce
extends products.Derivation

   with abelianMaps.AbelianDerivation
   with abelianMaps.ToScala

   with products.Syntax
   with bags.AbelianDerivation
   with bags.ToScala

   with products.ToScala

   with booleans.ToScala
   with sums.ToScala
   with GroupBy
{
  /** {{{
    * mapPerKey : ∀ {k₁ v₁ k₂ v₂} →
    *   AbelianGroup v₁ →
    *   (k₁ → v₁ → Bag (k₂ × v₂)) →
    *   Map k₁ v₁ → Bag (k₂ × v₂)
    * }}}
    *
    * Limitation: userMap partially applied to every key
    * must be a homomorphism. (Visible in fig. 5 & 6, mentioned
    * briefly in §4.4, the last paragraph on page 7).
    */
  val mapPerKey: UntypedTerm =
    'v1Group ->: 'userMap ->: FoldByHom('v1Group, FreeAbelianGroup, 'userMap)

  /** {{{
    * group-by-key : ∀ {k₂ v₂} →
    *   Bag (k₂ × v₂) → Map k₂ (Bag v₂)
    * }}}
    */
  val groupByKey: UntypedTerm =
    FoldGroup(
      LiftGroup(FreeAbelianGroup),
      'k2v2Pair ->: SingletonMap(Proj1('k2v2Pair), Singleton(Proj2('k2v2Pair)))
    )

  /** Output of userReduce is v₃, but it corresponds to (Maybe v₃).
    * The hidden `Nothing` constructor is the neutral element
    * specified by the first argument, the abelian group on v₃.
    *
    * {{{
    * reduce-per-key : ∀ {k₂ v₂ v₃} →
    *   AbelianGroup v₃ →
    *   (k₂ → Bag v₂ → v₃) →
    *   Map k₂ (Bag v₂) →
    *   Map k₂ v₃
    * }}}
    */
  val reducePerKey: UntypedTerm =
    'v3Group ->: 'userReduce ->:
      FoldByHom(
        FreeAbelianGroup,
        LiftGroup('v3Group),
        'key ->: 'bag ->: SingletonMap('key, 'userReduce('key, 'bag)))

  /** {{{
    * mapReduce : ∀ {k₁ v₁ k₂ v₂} →
    *   AbelianGroup v₁ →
    *   AbelianGroup v₃ →
    *   (k₁ → v₁ → Bag (k₂ × v₂)) →
    *   (k₂ → Bag v₂ → v₃) →
    *   Map k₁ v₁ →
    *   Map k₂ v₃
    * }}}
    */
  val mapReduce: UntypedTerm =
    'v1Group ->: 'v3Group ->: 'userMap ->: 'userReduce ->:
      (reducePerKey('v3Group, 'userReduce) composeWith groupByKey composeWith mapPerKey('v1Group, 'userMap))
}
