package ilc
package feature
package maybe

/** Δ(Maybe a) = Δa ⊎ Maybe a */
trait ReplacementValues
extends base.Derivation
   with Syntax
   with sums.Syntax
{
  override def deltaType(tau: Type): Type = tau match {
    case MaybeType(contentType) =>
      SumType(deltaType(contentType), MaybeType(contentType))

    case _ =>
      super.deltaType(tau)
  }

  private[this] def replace(contentType: Type) =
    Inj2(deltaType(contentType))

  override def updateTerm(tau: Type): Term = tau match {
    case MaybeType(contentType) =>
      lambda(deltaType(tau), tau) { case Seq(deltaMaybe, oldMaybe) =>
        Either !
          lambda(deltaType(contentType)) { dx =>
            oldMaybe >>= lambda { x =>
              Just ! (updateTerm(contentType) ! dx ! x)
            }
          } !
          lambda(tau) { replacement => replacement } !
          deltaMaybe
      }

    case _ =>
      super.updateTerm(tau)
  }

  override def diffTerm(tau: Type): Term = tau match {
    case MaybeType(contentType) =>
      lambda(tau, tau) { case Seq(theNew, theOld) =>
        replace(contentType) ! theNew
      }

    case _ =>
      super.diffTerm(tau)
  }

  override def derive(t: Term): Term = t match {
    case Nope(contentType) =>
      replace(contentType) ! t

    case Just(contentType) =>
      lambda(contentType, deltaType(contentType)) {
        case Seq(x, dx) =>
          Inj1(t.getType) ! dx
      }

    // Maybe has slow derivative
    case _ =>
      super.derive(t)
  }
}
