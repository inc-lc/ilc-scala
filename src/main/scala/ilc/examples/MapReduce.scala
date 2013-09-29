package ilc
package examples

import feature._
import language.bacchus

class MapReduceExample
extends Example
   with bacchus.Syntax
   with bacchus.Prelude
   with bacchus.ToScala
   with bacchus.BasicDerivation

   with integers.Syntax
   with booleans.SyntaxSugar
   with groups.SyntaxSugar
   with mapReduce.Syntax
{
  type BTerm = TermBuilder
  def bagGroup(k: Type): BTerm =
    groupBuildTerm ! Union(k) ! Negate(k) ! EmptyBag(k)

  def let(n: Name, value: TermBuilder)(body: Name => TermBuilder): TermBuilder =
    lambda(n)(body) ! value

  def mapValues(keyType: Type): BTerm =
    lambda("gv") { gv =>
      GroupUnfold ! gv ! {
        lambda("gb", "op", "inv", "e") { case Seq(gb, op, inv, e) =>
          lambda("f") { f =>
            FoldGroupMap ! (liftedValueGroup(keyType) ! gb) ! {
              lambda("key", "value") { case Seq(key, value) =>
                let("res", f ! value) { res =>
                  ifTerm(Eq ! res ! e, EmptyMap, SingletonMap ! key ! res)
                }}}}}}}

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
                    Combine ! {
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
    //bagGroup(ℕ)
}
