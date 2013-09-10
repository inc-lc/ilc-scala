package ilc
package feature
package unit

trait ToScala extends base.ToScala with Syntax with Typing {
  override def toScala(c: Constant): String = c match {
    case UnitTerm =>
      "()"

    case _ =>
      super.toScala(c)
  }

  override def toScala(tau: Type): String = tau match {
    case UnitType =>
      "Unit"

    case _ =>
      super.toScala(tau)
  }
}
