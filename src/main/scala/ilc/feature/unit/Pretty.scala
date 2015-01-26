package ilc
package feature
package unit

trait Pretty extends Syntax with base.Pretty {
  override def toPrettyExpression(tau: Type): PrettyExpression = tau match {
    case UnitType =>
      PrettyNullaryExpression(text(UnicodeOutput.choose("𝟙", "1")))

    case _ =>
      super.toPrettyExpression(tau)
  }
}
