package ilc
package feature
package base

trait ContextSensitiveDerivation
extends Derivation
   with Context
{
  final override def derive(t: Term): Term =
    deriveSubtree(Subtree.ofRoot(t))

  // subclass should override this one instead
  def deriveSubtree(subtree: Subtree): Term =
    // default to 
    super.derive(subtree.toTerm)
}
