package ilc
package feature
package naturals

trait Pretty extends Syntax with base.Pretty {
  override def toPrettyExpression(tau: Type): PrettyExpression = tau match {
    case NatType =>
      PrettyNullaryExpression(text(UnicodeOutput.choose("ℕ", "N")))

    case _ =>
      super.toPrettyExpression(tau)
  }
}
