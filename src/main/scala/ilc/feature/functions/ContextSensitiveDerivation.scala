
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
  * its duties to `deriveSubterm` immediately.
  */

trait ContextSensitiveDerivation
extends Derivation
   with functions.Context
   with base.ContextSensitiveDerivation
{
  override def deriveSubterm(s: Subterm): Term = s.toTerm match {
    case Abs(x, _) => {
      val Seq(body) = s.children
      lambdaTerm(x, DVar(x)) { deriveSubterm(body) }
    }

    case App(_, _) => {
      val Seq(operator, operand) = s.children
      deriveSubterm(operator) ! operand.toTerm ! deriveSubterm(operand)
    }

    case _ =>
      super.deriveSubterm(s)
  }
}
