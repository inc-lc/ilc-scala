package ilc
package feature
package unit

trait Derivation extends base.Derivation with Syntax {
  override def deltaType(tau: Type): Type = tau match {
    case UnitType => UnitType
    case _        => super.deltaType(tau)
  }

  override def updateTerm(tau: Type): Term = tau match {
    case UnitType =>
      lambda(UnitType, UnitType) { case Seq(_, _) => UnitTerm }

    case _ =>
      super.updateTerm(tau)
  }

  override def diffTerm(tau: Type): Term = tau match {
    case UnitType =>
      lambda(UnitType, UnitType) { case Seq(_, _) => UnitTerm }

    case _ =>
      super.diffTerm(tau)
  }

  override def derive(t: Term): Term = t match {
    case UnitTerm => UnitTerm
    case _        => super.derive(t)
  }
}
