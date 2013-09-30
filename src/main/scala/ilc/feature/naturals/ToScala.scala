package ilc
package feature
package naturals

trait ToScala extends base.ToScala with Syntax {
  private[this] val natType: String =
    toScala(NatType)

  override def toScala(t: Term): String = t match {
    case Nat(n) =>
      n.toString

    case Plus => {
      s"((x: $natType) => (y: $natType) => x + y)"
    }

    case FoldNat(rType) => {
      val r = toScala(rType)
      val fType = toScala(rType =>: rType)
      val body = s"Range.inclusive(1, n).foldLeft[$r](z)" ++
        s"((x: $r, _: $natType) => f(x))"
      s"((z: $r) => (f: $fType) => (n: $natType) => $body)"
    }

    case _ =>
      super.toScala(t)
  }

  override def toScala(tau: Type): String = tau match {
    case NatType =>
      "Int"

    case _ =>
      super.toScala(tau)
  }
}
