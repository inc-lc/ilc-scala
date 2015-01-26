package ilc
package feature
package lists

trait Pretty extends Syntax with base.Pretty {
  override def toPrettyExpression(tau: Type): PrettyExpression = tau match {
    case ListType(elemType) =>
      PrettyNullaryExpression {
        text("[") <> toDoc(tau) <> text("]")
      }

    case _ =>
      super.toPrettyExpression(tau)
  }
}
