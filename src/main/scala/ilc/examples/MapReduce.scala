package ilc
package examples

import feature._
import language.bacchus

class MapReduceExample
extends Example
// We probably don't want to mix in Bacchus,
// because naturals and integers don't play well together (yet)

   with bacchus.Syntax
   with bacchus.Prelude
   with bacchus.ToScala
   with bacchus.BasicDerivation

   with booleans.SyntaxSugar
   with groups.SyntaxSugar
{
  // dummy program TODO: replace me!
  def program = lambda(NatType) {x => x}
/*
  type BTerm = TermBuilder
  def bagGroup(k: Type): BTerm =
    groupBuildTerm ! Union(k) ! Negate(k) ! EmptyBag(k)

  def let(n: Name, value: TermBuilder)(body: Name => TermBuilder): TermBuilder =
    lambda(n)(body) ! value

  //map-values : ∀ {k a b} → Group b → (a → b) → Map k a → Map k b

  def mapValues(keyType: Type): PolymorphicTerm =
    new PolymorphicTerm {
      def specialize(argumentTypes: Type*): Term =
        argumentTypes take 2 match {
          case Seq(gt @ GroupType(elType), fType @ srcType =>: dstType)
              if elType == dstType =>

            lambda(
              Var("gv", gt),
              Var("f", fType),
              Var("m", MapType(keyType, srcType))) { case Seq(gv, f, m) =>
                GroupUnfold(elType, MapType(keyType, elType)) ! gv !
                  lambda(
                    Var("gb", gt),
                    Var("op", binOpType(elType)),
                    Var("inv", invType(elType)),
                    Var("e", elType)) { case Seq(gb, op, inv, e) =>
                      FoldGroupMap ! (liftedValueGroup(keyType) ! gb) !
                        lambda("key", "value") { case Seq(key, value) =>
                          let("res", f ! value) { res =>
                            ifTerm(Eq ! res ! e, EmptyMap, SingletonMap ! key ! res)
                          }}}}}}

  //lifted-value-group = ∀ {k v} → Group v → Group (Map k v)
  def liftedValueGroup(keyType: Type): PolymorphicTerm =
    new PolymorphicTerm {
      def specialize(argumentTypes: Type*): Term =
        argumentTypes.head match {
          case gt @ GroupType(elType) =>

            lambda(Var("gv", gt)) { gv =>
              GroupUnfold(elType, MapType(keyType, elType)) ! gv !
                lambda(
                  Var("gv", gt),
                  Var("op", binOpType(elType)),
                  Var("inv", invType(elType)),
                  Var("e", elType)) { case Seq(gv, op, inv, e) =>
                  groupBuildTerm ! {
                    Combine(keyType, elType) ! {
                      lambda(Var("u", elType), Var("v", elType)) { case Seq(u, v) =>
                        let("res", op ! u ! v) { res =>
                          ifTerm((Eq ! res ! e), Nope(elType), Just ! res)
                        }}}} !
                    (mapValues(keyType) ! gv ! inv) !
                    EmptyMap
                }}}}

  def liftedBagGroup(k: Type, v: Type): BTerm =
    liftedValueGroup(k) ! bagGroup(v)
  def integerGroup: BTerm =
    groupBuildTerm ! Plus ! Negate ! Nat(0)

  val StringType = IntType

  def wordCountGroup: BTerm =
    liftedValueGroup(StringType) ! integerGroup

  def program: Term =
    liftedBagGroup(ℕ, ℕ)
    //liftedValueGroup(ℕ)
    //mapValues
    //bagGroup(ℕ) */
}
