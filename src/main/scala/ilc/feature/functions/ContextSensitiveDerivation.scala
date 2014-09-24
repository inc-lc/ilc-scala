
package ilc
package feature
package functions

/** Derivation with access to parent terms to facilitate static
  * analysis
  *
  * CAUTION
  *
  * base.ContextSensitiveDerivation must be mixed in after all
  * context-free Derivation traits, because the `derive` method
  * of a ContextSensitiveDerivation trait is final. The final tag
  * is there to ensure that the `derive` called from within a
  * calculus with ContextSensitiveDerivation is the one defined
  * in base.ContextSensitiveDerivation for sure, who delegates
  * its duties to `deriveSubtree` immediately.
  *
  * The base deriveSubtree will then default to context-free derivation
  * in the superclass - ignoring code of context-free derivation traits
  * in subtraits.
  */

trait ContextSensitiveDerivation
extends Derivation
   with functions.Context
   with base.ContextSensitiveDerivation
{
  override def deriveSubtree(s: Subtree): Term = s.toTerm match {
    case Abs(x, _) => {
      val Seq(body) = s.children
      lambdaTerm(x, DVar(x)) { deriveSubtree(body) }
    }

    case App(_, _) => {
      val Seq(operator, operand) = s.children
      deriveSubtree(operator) ! operand.toTerm ! deriveSubtree(operand)
    }

    case _ =>
      super.deriveSubtree(s)
  }
}
