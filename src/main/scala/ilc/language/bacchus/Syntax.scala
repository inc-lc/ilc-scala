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
import feature._

trait Syntax
extends feature.Functions
   with feature.DiffAndApply with naturals.Syntax with unit.Syntax with sum.Syntax with sum.SyntaxSugar with maps.Syntax with maps.SyntaxSugar {
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

  // product types are encoded in terms of maps.
  // it is probably useful to have them later on,
  // for the exclusion of ill-formed products and
  // better incremental behavior.
  //
  //   uncurry : (a → b → c) → a × b → c
  //
  def pair(s: Term, t: Term): Term = Update(s)(t)(EmptyMap)
  def pairTerm: Term = Lambda("x", "y") ->: pair("x", "y")
  def uncurry(f: Term, p: Term): Term = {
    val List(x, y, dontcare) = uniqueVars(f, "x", "y", "_")
    Fold(Lambda(x, y, dontcare) ->: f(x)(y))(UnitTerm)(p)
  }
  def uncurryTerm: Term = Lambda("f", "p") ->: uncurry("f", "p")
}
