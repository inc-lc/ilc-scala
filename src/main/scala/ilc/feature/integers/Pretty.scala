package ilc
package feature
package integers

import org.kiama.output._

trait Pretty extends Syntax with base.Pretty {
  override def toPrettyExpression(tau: Type): PrettyExpression = tau match {
    case IntType =>
      PrettyNullaryExpression(text(UnicodeOutput.choose("ℤ", "Z")))

    case _ =>
      super.toPrettyExpression(tau)
  }
}
