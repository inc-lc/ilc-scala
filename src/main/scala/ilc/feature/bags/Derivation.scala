package ilc
package feature
package bags

/** {{{
  * Δ(Bag a) = Bag a
  * }}}
  */

trait Derivation
extends base.Derivation
   with Syntax
   with functions.Syntax
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

  object BagSurgery {
    sealed trait Change
    case class DELETE(value: Term) extends Change
    case class INSERT(value: Term) extends Change
    case class MODIFY(value: Term, deltaValue: Term) extends Change
  }

  //Copied from ReplacementValuesDerivation.scala
  def mkSurgicalBagChange(valueType: Type)
    (changes: BagSurgery.Change*): Term =
  {
    import BagSurgery._
    if (changes.isEmpty)
      EmptyBag(valueType)
    else
      ???
  }

  //TODO: non-naive derivatives!
  override def derive(t: Term): Term =
    t match {
      case EmptyBag(valueType) =>
        mkSurgicalBagChange(valueType)()
      case _ =>
        super.derive(t)
    }
}
