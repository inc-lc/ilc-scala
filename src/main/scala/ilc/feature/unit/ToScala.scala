package ilc
package feature
package unit

trait ToScala extends base.ToScala with Syntax {
  override def toUntypedScala(t: Term): String = t match {
    case UnitTerm =>
      "()"

    case _ =>
      super.toUntypedScala(t)
  }

  override def toScala(tau: Type): String = tau match {
    case UnitType =>
      "Unit"

    case _ =>
      super.toScala(tau)
  }
}
