package ilc
package feature
package integers

/** Δℤ = (AbelianGroup ℤ × ℤ) ⊎ ℤ
  *
  * The change to an integer is an abelian group element or a
  * replacement.
  */
trait AbelianDerivation
extends abelianGroups.AbelianDerivation
   with Syntax
   with functions.ContextSensitiveDerivation
{
  override def isAbelianType(tau: Type): Boolean = tau match {
    case IntType => true
    case _       => super.isAbelianType(tau)
  }

  override def deriveSubtree(s: Subtree): Term = s.toTerm match {
    case term @ LiteralInt(i) =>
      replacementChange ! term

    // PlusInt and NegateInt can use the slow derivative. It operates on
    // integers, so calling `updateTerm` and `diffTerm` is okay.

    case _ =>
      super.deriveSubtree(s)
  }
}
