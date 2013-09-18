package ilc
package feature
package naturals

trait ToScala extends base.ToScala with TypedSyntax {
  private[this] val natType: String =
    toScala(NatType)

  // polymorphic constants go here
  override def toScala(t: Term): String = t match {
    case TypedFoldNat(rType) => {
      val r = toScala(rType)
      val body = s"Range.inclusive(1, n).foldLeft[$r](z)" ++
        s"((x: $r, _: $natType) => f(x))"
      s"((z: $r) => (f: $r => $r) => (n: $natType) => $body)"
    }

    case _ =>
      super.toScala(t)
  }

  // nonpolymorphic constants go here
  override def toScala(c: Constant): String = c match {
    case Nat(n) =>
      n.toString

    case Plus => {
      s"((x: $natType) => (y: $natType) => x + y)"
    }
  }

  override def toScala(tau: Type): String = tau match {
    case NatType =>
      "Int"

    case _ =>
      super.toScala(tau)
  }
}
