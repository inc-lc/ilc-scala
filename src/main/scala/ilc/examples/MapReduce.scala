package ilc
package examples

import feature._

trait MapReduce
extends products.Derivation

   with abelianMaps.AbelianDerivation
   with abelianMaps.ToScala

   with products.StdLib
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

  // Here we encounter the Scala type `TermBuilder`.
  // A `TermBuilder` object represents a simply typed
  // lambda term with zero or more type variables left
  // to be instantiated. We chose to expose the library
  // functions `mapReduce`, `groupBy` etc as `TermBuilder`
  // objects so that they can be used to build MapReduce
  // operations on collections with arbitrary element
  // types.

//  val OLDmapReduce: TermBuilder = new PolymorphicTerm {
//    def specialize(argumentTypes: Type*): Term =
//      argumentTypes.tail.take(2) match {
//        case Seq(AbelianGroupType(v3),
//                 k1 =>: v1 =>: BagType(ProductType(k2, v2))) =>
//          val userMapType =
//            k1 =>: v1 =>: BagType(ProductType(k2, v2))
//          val userReduceType =
//            k2 =>: BagType(v2) =>: v3
//          lambda(Var("v1Group", AbelianGroupType(v1)),
//                 Var("v3Group", AbelianGroupType(v3)),
//                 Var("userMap", userMapType),
//                 Var("userReduce", userReduceType)) {
//            case Seq(v1Group, v3Group, userMap, userReduce) =>
//              (reducePerKey ! v3Group ! userReduce) composeWith
//                (groupByKey composeWith
//                  (mapPerKey ! v1Group ! userMap))
//          }
//      }
//  }

  val mapReduce: UntypedTerm =
    'v1Group ->: 'v3Group ->: 'userMap ->: 'userReduce ->:
      reducePerKey('v3Group, 'userReduce) composeWith (groupByKey composeWith mapPerKey('v1Group, 'userMap))


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
//  val OLDmapPerKey: TermBuilder = new PolymorphicTerm {
//    def specialize(argumentTypes: Type*): Term =
//      argumentTypes.tail.head match {
//        case userMapType @ k1 =>: v1 =>: BagType(ProductType(k2, v2)) =>
//          lambda(Var("v1Group", AbelianGroupType(v1)),
//                 Var("userMap", userMapType)) {
//            case Seq(v1Group, userMap) =>
//              FoldByHom !
//                v1Group !
//                FreeAbelianGroup(ProductType(k2, v2)) !
//                userMap
//          }
//      }
//  }

  val mapPerKey: UntypedTerm =
    'v1Group ->: 'userMap ->: foldByHom('v1Group, freeAbelianGroup, 'userMap)

  /** {{{
    * group-by-key : ∀ {k₂ v₂} →
    *   Bag (k₂ × v₂) → Map k₂ (Bag v₂)
    * }}}
    */
//  val OLDgroupByKey: TermBuilder = new PolymorphicTerm {
//    def specialize(argumentTypes: Type*): Term =
//      argumentTypes.head match {
//        case BagType(ProductType(k2, v2)) =>
//          //This alternative implementation compiles and generates code correctly as well, but I did not benchmark it yet.
//          //groupByGen ! Proj1(k2, v2) ! Proj2(k2, v2)
//
//          FoldGroup !
//            (LiftGroup(k2) ! FreeAbelianGroup(v2)) !
//            lambda(Var("k2v2Pair", ProductType(k2, v2))) { k2v2Pair =>
//                SingletonMap !
//                  (Proj1 ! k2v2Pair) !
//                  (Singleton ! (Proj2 ! k2v2Pair))
//            }
//      }
//  }

  val groupByKey: UntypedTerm =
    foldGroup(
      liftGroup(freeAbelianGroup),
      'k2v2Pair ->: singletonMap(proj1('k2v2Pair), singleton(proj2('k2v2Pair)))
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
//  val OLDreducePerKey: TermBuilder = new PolymorphicTerm {
//    def specialize(argumentTypes: Type*): Term =
//      argumentTypes.tail.head match {
//        case userReduceType @ k2 =>: BagType(v2) =>: v3 =>
//          lambda(Var("v3Group", AbelianGroupType(v3)),
//                 Var("userReduce", userReduceType)) {
//            case Seq(v3Group, userReduce) =>
//              FoldByHom !
//                FreeAbelianGroup(v2) !
//                (LiftGroup(k2) ! v3Group) !
//                lambda(Var("key", k2),
//                       Var("bag", BagType(v2))) { case Seq(key, bag) =>
//                    SingletonMap ! key ! (userReduce ! key ! bag)
//                }
//          }
//      }
//  }

  val reducePerKey: UntypedTerm =
    'v3Group ->: 'userReduce ->:
      foldByHom(
        freeAbelianGroup,
        liftGroup('v3Group),
        'key ->: 'bag ->: singletonMap('key, 'userReduce('key, 'bag)))

  // TODO This type constructor apply method is really annoying (ilc.feature.base.TypeConstructor.TypeConstructor.apply)
  // We could possibly rename it so that the PolymorphicConstant implicit conversion makes this kind of aliasing unnecessary.
  private val foldByHom: UntypedTerm = FoldByHom
  private val singletonMap: UntypedTerm = SingletonMap
  private val liftGroup: UntypedTerm = LiftGroup
}
