package ilc
package feature
package abelianGroups

/** The change to a group is a replacement.
  *
  *   Δ Abelian(e) = Abelian(e)
  */

trait Derivation extends base.Derivation with Syntax {
  override def deltaType(tau: Type): Type = tau match {
    case AbelianGroupType(_) =>
      tau

    case _ =>
      super.deltaType(tau)
  }

  override def updateTerm(tau: Type): Term = tau match {
    case _G @ AbelianGroupType(_) =>
      lambda(_G, _G) { case Seq(dG, _G) => dG }

    case _ =>
      super.updateTerm(tau)
  }

  override def diffTerm(tau: Type): Term = tau match {
    case _G @ AbelianGroupType(_) =>
      lambda(_G, _G) { case Seq(_Gnew, _Gold) => _Gnew }

    case _ =>
      super.diffTerm(tau)
  }

  // do not override derive, do replacement in all cases
}
