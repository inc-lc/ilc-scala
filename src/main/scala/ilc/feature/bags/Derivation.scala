package ilc
package feature
package bags

trait Derivation
extends base.Derivation
   with Syntax
   with functions.Syntax
   with maybe.Syntax
   with sums.Syntax
{
  override def deltaType(tau: Type): Type = tau match {
    case BagType(v) =>
      BagType(v) //Simple derivative type.

    case _ =>
      super.deltaType(tau)
  }

  override def updateTerm(tau: Type): Term = tau match {
    case BagType(valueType) =>
      lambda(deltaType(tau), tau) { case Seq(deltaBag, oldBag) =>
        Union ! oldBag ! deltaBag
      }
    case _ =>
      super.updateTerm(tau)
  }

  override def diffTerm(tau: Type): Term = tau match {
    case BagType(valueType) =>
      lambda(tau, tau) { case Seq(newBag, oldBag) =>
        Union ! newBag ! (Negate ! oldBag)
      }

    case _ =>
      super.diffTerm(tau)
  }
  //TODO: non-naive derivatives!
}
