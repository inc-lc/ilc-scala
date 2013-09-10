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
   with feature.changePrimitives.Syntax
   with naturals.ToScala
   with unit.ToScala
   with sums.ToScala
   with sums.SyntaxSugar
   with maps.ToScala
   with maps.SyntaxSugar
   with nilChange.Syntax
{
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
