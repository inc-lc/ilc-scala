package ilc
package examples

import feature._

trait MapReduce
extends products.Derivation

   with abelianMaps.AbelianDerivation
   with abelianMaps.ToScala

   with bags.SyntaxSugar
   with bags.AbelianDerivation
   with bags.ToScala

   with products.ToScala
   with booleans.ToScala
   with sums.ToScala
   with GroupBy
{
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
  val mapReduce: TermBuilder = new PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term =
      argumentTypes.tail.take(2) match {
        case Seq(AbelianGroupType(v3),
                 k1 =>: v1 =>: BagType(ProductType(k2, v2))) =>
          val userMapType =
            k1 =>: v1 =>: BagType(ProductType(k2, v2))
          val userReduceType =
            k2 =>: BagType(v2) =>: v3
          lambda(Var("v1Group", AbelianGroupType(v1)),
                 Var("v3Group", AbelianGroupType(v3)),
                 Var("userMap", userMapType),
                 Var("userReduce", userReduceType)) {
            case Seq(v1Group, v3Group, userMap, userReduce) =>
              (reducePerKey ! v3Group ! userReduce) composeWith
                (groupByKey composeWith
                  (mapPerKey ! v1Group ! userMap))
          }
      }
  }

  /** {{{
    * mapPerKey : ∀ {k₁ v₁ k₂ v₂} →
    *   AbelianGroup v₁ →
    *   (k₁ → v₁ → Bag (k₂ × v₂)) →
    *   Map k₁ v₁ → Bag (k₂ × v₂)
    * }}}
    *
    * Great limitation: userMap partially applied to every key
    * must be a homomorphism. If v₁ isn't a collection type,
    * such homomorphisms ain't easy to construct.
    */
  val mapPerKey: TermBuilder = new PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term =
      argumentTypes.tail.head match {
        case userMapType @ k1 =>: v1 =>: BagType(ProductType(k2, v2)) =>
          lambda(Var("v1Group", AbelianGroupType(v1)),
                 Var("userMap", userMapType)) {
            case Seq(v1Group, userMap) =>
              FoldByHom !
                v1Group !
                FreeAbelianGroup(ProductType(k2, v2)) !
                userMap
          }
      }
  }

  /** {{{
    * group-by-key : ∀ {k₂ v₂} →
    *   Bag (k₂ × v₂) → Map k₂ (Bag v₂)
    * }}}
    */
  val groupByKey: TermBuilder = new PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term =
      argumentTypes.head match {
        case BagType(ProductType(k2, v2)) =>
          //This alternative implementation compiles and generates code correctly as well, but I did not benchmark it yet.
          //groupByGen ! Proj1(k2, v2) ! Proj2(k2, v2)

          FoldGroup !
            (LiftGroup(k2) ! FreeAbelianGroup(v2)) !
            lambda(Var("k2v2Pair", ProductType(k2, v2))) { k2v2Pair =>
                SingletonMap !
                  (Proj1 ! k2v2Pair) !
                  (Singleton ! (Proj2 ! k2v2Pair))
            }
      }
  }

  /** Output of userReduce is v₃, but it is actually (Maybe v₃).
    * The hidden `Nothing` constructor is the neutral element
    * specified by the first argument, the abelian group on v₃.
    *
    * XXX It would be perfect if we could use (Maybe v₃) as
    * the result type of userReduce, but I ran out of time to
    * implement abelian group support for maybe. (cai 06.10.13)
    *
    * {{{
    * reduce-per-key : ∀ {k₂ v₂ v₃} →
    *   AbelianGroup v₃ →
    *   (k₂ → Bag v₂ → v₃) →
    *   Map k₂ (Bag v₂) →
    *   Map k₂ v₃
    * }}}
    */
  val reducePerKey: TermBuilder = new PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term =
      argumentTypes.tail.head match {
        case userReduceType @ k2 =>: BagType(v2) =>: v3 =>
          lambda(Var("v3Group", AbelianGroupType(v3)),
                 Var("userReduce", userReduceType)) {
            case Seq(v3Group, userReduce) =>
              FoldByHom !
                FreeAbelianGroup(v2) !
                (LiftGroup(k2) ! v3Group) !
                lambda(Var("key", k2),
                       Var("bag", BagType(v2))) { case Seq(key, bag) =>
                    SingletonMap ! key ! (userReduce ! key ! bag)
                }
          }
      }
  }
}
