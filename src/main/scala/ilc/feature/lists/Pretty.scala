package ilc
package feature
package lists

import org.kiama.output._

trait Pretty extends Syntax with base.Pretty {
  override def toPrettyExpression(tau: Type): PrettyExpression = tau match {
    case ListType(elemType) =>
      PrettyNullaryExpression {
        text("[") <> toDoc(elemType) <> text("]")
      }

    case _ =>
      super.toPrettyExpression(tau)
  }
}
