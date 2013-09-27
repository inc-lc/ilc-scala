package ilc
package feature
package base

trait ContextSensitiveDerivation
extends Derivation
   with Context
{
  override def derive(t: Term): Term =
    deriveSubterm(Location.ofRoot(t))

  // subclass should override this one instead
  def deriveSubterm(subterm: Subterm): Term =
    // default to 
    super.derive(subterm.toTerm)
}
