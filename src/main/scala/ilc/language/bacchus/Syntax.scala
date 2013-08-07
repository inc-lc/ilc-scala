package ilc
package language.bacchus

/**
 * Bacchus, the calculus after Atlas
 * - base types = nat, sum, map
 * - all base types have intro/elim forms
 * - all base-type changes can be replacement values
 * - replacement values beget replacement values
 *   (ilc/replacement-pair.md §2 approach 1)
 */

import scala.language.implicitConversions

trait Syntax
extends feature.Functions
   with feature.DiffAndApply {
  sealed trait Constant

  // type-indexed terms that has to be simulated in scala
  // cannot be derived
  //
  // diff : τ → τ → Δτ
  // apply : Δτ → τ → τ
  //
  case object Diff extends Constant
  case object Apply extends Constant
  val diffTerm = Const(Diff)
  val applyTerm = Const(Apply)

  // the inhabitant of the unit type
  // also used as placeholder
  case object Individualist extends Constant

  // intro/elim forms of nats
  //
  //   foldNat : r → (r → r) → Nat → r
  //
  case class Nat(n: Int) extends Constant { require(n >= 0) }
  case object FoldNat extends Constant
  case object Plus extends Constant with NestingBinaryOperator

  // intro/elim forms of maps (of base-type values)
  //
  //   empty : Map k v
  //   update : k → v → Map k v → Map k v
  //   lookup : k → Map k v → v
  //   fold : (k → a → b → b) → b → Map k a → b
  //
  case object Empty extends Constant
  case object Update extends Constant
  case object Lookup extends Constant
  case object Fold extends Constant

  // intro/elim forms of sum types
  // (sums of functions are forbidden)
  //
  //   either : (a → c) → (b → c) → a ⊎ b → c
  //
  case object Left extends Constant
  case object Right extends Constant
  case object Either extends Constant

  // product types are encoded in terms of maps.
  // it is probably useful to have them later on,
  // for the exclusion of ill-formed products and
  // better incremental behavior.
  //
  //   uncurry : (a → b → c) → a × b → c
  //
  def pair(s: Term, t: Term): Term = Update(s)(t)(Empty)
  def pairTerm: Term = Lambda("x", "y") ->: pair("x", "y")
  def uncurry(f: Term, p: Term): Term = {
    val List(x, y, dontcare) = uniqueVars(f, "x", "y", "_")
    Fold(Lambda(x, y, dontcare) ->: f(x)(y))(Individualist)(p)
  }
  def uncurryTerm: Term = Lambda("f", "p") ->: uncurry("f", "p")

  // booleans are encoded as sums
  val tt = Left(Individualist)
  val ff = Right(Individualist)
  val ifThenElse = Lambda("cond", "then", "else") ->:
    Either("_" ->: "then")("_" ->: "else")("cond")

  // implicit conversions
  implicit def natToConst(n: Int): Constant = Nat(n)
  implicit def natToTerm(n: Int): Term = Const(natToConst(n))

  // easy construction of map literals (copied from Atlas)
  def Map(assoc: (Term, Term)*): Term =
    updatesFrom(Empty, assoc: _*)

  def fromList(base: Term, assoc: List[(Term, Term)]): Term =
    assoc match {
      case Nil => base
      case (k, v) :: assoc =>
        Update(k)(v)(fromList(base, assoc))
    }

  // shorthand for chain updates
  def updatesFrom(base: Term, assoc: (Term, Term)*): Term =
    fromList(base, assoc.toList)

  // mapValues: (a -> b) -> Map k a -> Map k b
  val mapValues: Term = Lambda("f", "map") ->:
    Fold(Lambda("key", "value", "accumulator") ->:
           Update("key")(Var("f")("value"))("accumulator"))(
         Empty)("map")

  // shorthands for pattern matching

  def case2(matchAgainst: Term, caseLeft: Term, caseRight: Term): Term =
    Either(caseLeft)(caseRight)(matchAgainst)

  def case4(sample1: Term, sample2: Term,
            caseLL: Term, caseLR: Term,
            caseRL: Term, caseRR: Term): Term =
    case2(sample2,
      case2(sample1, caseLL, caseRL),
      case2(sample1, caseLR, caseRR))
}
