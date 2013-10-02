package ilc
package feature
package unit

trait ToScala extends base.ToScala with Syntax {
  override def toScala(t: Term): String = t match {
    case UnitTerm =>
      s"((): ${toScala(UnitType)})"

    case _ =>
      super.toScala(t)
  }

  override def toScala(tau: Type): String = tau match {
    case UnitType =>
      "Unit"

    case _ =>
      super.toScala(tau)
  }
}
