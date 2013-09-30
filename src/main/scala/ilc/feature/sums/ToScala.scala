package ilc
package feature
package sums

trait ToScala extends base.ToScala with Syntax {
  private[this] def sumType(a: Type, b: Type): String =
    toScala(SumType(a, b))

  override def toScala(t: Term): String = t match {
    case Either(aType, bType, cType) => {
      val List(a, b, c) = List(aType, bType, cType) map toScala
      val either_a_b = sumType(aType, bType)
      val body = s"s.fold[$c](f, g)"
      // CAUTION: function types must be handled delicately!
      // NEVER, EVER writes an => as the type of something!
      // Reason: (Int, Int) => Int is NOT a function with domains
      // in a product type.
      val fType = toScala(aType =>: cType)
      val gType = toScala(bType =>: cType)
      s"((f: $fType) => (g: $gType) => (s: $either_a_b) => $body)"
    }

    case Inj1(aType, bType) => {
      val (a, b) = (toScala(aType), toScala(bType))
      s"((x: $a) => Left.apply[$a, $b](x))"
    }

    case Inj2(aType, bType) => {
      val (a, b) = (toScala(aType), toScala(bType))
      s"((y: $b) => Right.apply[$a, $b](y))"
    }

    case _ =>
      super.toScala(t)
  }

  override def toScala(tau: Type): String = tau match {
    case SumType(leftType, rightType) =>
      "Either[%s, %s]".format(toScala(leftType), toScala(rightType))

    case _ =>
      super.toScala(tau)
  }
}
