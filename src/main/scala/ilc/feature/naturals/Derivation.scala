package ilc
package feature
package naturals

/** ΔNat = Nat
  * (The change to a natural number is a replacement value)
  */
trait ReplacementValues
extends base.Derivation
   with Syntax
   with functions.Derivation // for syntactic sugar
{
  override def deltaType(tau: Type): Type = tau match {
    case NatType => NatType
    case _ => super.deltaType(tau)
  }

  override def updateTerm(tau: Type): Term = tau match {
    case NatType =>
      lambda(NatType, NatType) { case Seq(dx, x) => dx }

    case _ =>
      super.updateTerm(tau)
  }

  override def diffTerm(tau: Type): Term = tau match {
    case NatType =>
      lambda(NatType, NatType) { case Seq(m, n) => m }

    case _ =>
      super.diffTerm(tau)
  }

  override def derive(t: Term): Term = t match {
    case Nat(n) =>
      Nat(n)

    case Plus =>
      lambdaDelta(t) { case Seq(x, dx, y, dy) => Plus ! dx ! dy }

    // case FoldNat => ... // we will use the default FoldNat ⊖ FoldNat
    case _ =>
      super.derive(t)
  }
}
