package ilc
package feature
package sums

trait ToScala extends base.ToScala with Syntax {
  private[this] def sumType(a: Type, b: Type): String =
    toScala(SumType(a, b))

  override def toUntypedScala(t: Term): String = t match {
    case Either(aType, bType, cType) => {
      val c = toScala(cType)
      scalaFunction("f", "g", "s")(s"s.fold[$c](f, g)")
    }

    case Inj1(aType, bType) =>
      s"(Left.apply _)"

    case Inj2(aType, bType) =>
      s"(Right.apply _)"

    case _ =>
      super.toUntypedScala(t)
  }

  override def toScala(tau: Type): String = tau match {
    case SumType(leftType, rightType) =>
      "Either[%s, %s]".format(toScala(leftType), toScala(rightType))

    case _ =>
      super.toScala(tau)
  }
}
