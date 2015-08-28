package ilc
package feature
package naturals

import org.kiama.output._

trait Pretty extends Syntax with base.Pretty {
  override def toPrettyExpression(tau: Type): PrettyExpression = tau match {
    case NatType =>
      PrettyNullaryExpression(text(UnicodeOutput.choose("ℕ", "N")))

    case _ =>
      super.toPrettyExpression(tau)
  }
}
