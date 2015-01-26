package ilc
package feature
package integers

trait Pretty extends Syntax with base.Pretty {
  override def toPrettyExpression(tau: Type): PrettyExpression = tau match {
    case IntType =>
      PrettyNullaryExpression(text(UnicodeOutput.choose("ℤ", "Z")))

    case _ =>
      super.toPrettyExpression(tau)
  }
}
