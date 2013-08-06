package ilc
package language.bacchus

/**
 * Bacchus, the calculus after Atlas
 * - base types = nat, sum, map
 * - all base types have intro/elim forms
 * - all base-type changes can be replacement pairs
 * - replacement pairs beget replacement pairs
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

  // implicit conversions
  implicit def natToConst(n: Int): Constant = Nat(n)
  implicit def natToTerm(n: Int): Term = Const(natToConst(n))

  def deriveConst(c: Constant): Term = c match {
    case Diff | Apply => sys.error("cannot derive " ++ c.toString)

    // ΔUnit = Unit
    case Individualist => Individualist

    // changes to natural numbers are replacement pairs,
    // put in a sum so that the replacement-pair-part of
    // every base-type change is an injection with Right.
    //
    // ΔNat = Unit ⊎ (Nat × Nat)
    //         nil    replace
    case Nat(n) => Individualist

    case FoldNat => Diff(FoldNat)(FoldNat)

    // Δ (Map κ τ) = Map κ ((Unit ⊎ τ) ⊎ Δτ) ⊎ (Map κ τ × Map κ τ)
    //                      del  ins  modify     replace
    case Empty => Left(Empty)

    // recompute everything for now.
    // consider potential improvements even here.
    // https://github.com/ps-mr/ilc/commit/053a8229f200d6087fe331c6ea7bccc2ba92b81d#commitcomment-3781686

    case Update => Diff(Update)(Update)
    case Lookup => Diff(Lookup)(Lookup)
    case Fold => Diff(Fold)(Fold)

    // Δ (σ ⊎ τ) = (Δσ ⊎ Δτ) ⊎ ((σ ⊎ τ) ⊎ (σ ⊎ τ))
    //              modify      replace
    case Left => Lambda("x", "Δx") ->: Left(Left("Δx"))
    case Right => Lambda("x", "Δx") ->: Left(Right("Δx"))
    case Either => Diff(Either)(Either)
  }
}
