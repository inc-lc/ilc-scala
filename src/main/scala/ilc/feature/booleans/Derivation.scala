package ilc
package feature
package booleans

/** change to a Boolean = replacement value */

trait Derivation extends unit.Derivation with Syntax {
  override def deltaType(tau: Type): Type = tau match {
    case BooleanType => BooleanType
    case _           => super.deltaType(tau)
  }

  override def updateTerm(tau: Type): Term = tau match {
    case BooleanType =>
      lambda(BooleanType, BooleanType) { case Seq(db, b) => db }

    case _ =>
      super.updateTerm(tau)
  }

  override def diffTerm(tau: Type): Term = tau match {
    case BooleanType =>
      lambda(BooleanType, BooleanType) { case Seq(bNew, bOld) => bNew }

    case _ =>
      super.diffTerm(tau)
  }

  override def derive(t: Term): Term = t match {
    case True  => True
    case False => False
    // recompute on IfThenElse
    case _     => super.derive(t)
  }
}
