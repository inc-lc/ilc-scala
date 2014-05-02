package ilc
package feature
package naturals

trait ToScala extends base.ToScala with Syntax {
  private[this] val natType: String =
    toScala(NatType)

  override def toUntypedScala(t: Term): String = t match {
    case Nat(n) =>
      n.toString

    case PlusNat =>
      scalaFunction("x", "y")("x + y")

    case FoldNat(r) =>
      scalaFunction("z", "f", "n") {
        s"Range.inclusive(1, n).foldLeft[${toScala(r)}](z)((x, _) => f(x))"
      }

    case _ =>
      super.toUntypedScala(t)
  }

  override def toScala(tau: Type): String = tau match {
    case NatType =>
      "Int"

    case _ =>
      super.toScala(tau)
  }
}
