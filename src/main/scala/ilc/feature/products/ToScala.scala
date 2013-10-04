package ilc
package feature
package products

trait ToScala extends base.ToScala with Syntax {
  private[this] def productType(a: Type, b: Type): String =
    toScala(ProductType(a, b))

  override def toUntypedScala(t: Term): String = t match {
    case Pair(aType, bType) =>
      scalaFunction("pairElem1", "pairElem2")("(pairElem1, pairElem2)")

    case Proj1(a, b) =>
      "(_._1)"

    case Proj2(a, b) =>
      "(_._2)"

    case _ =>
      super.toUntypedScala(t)
  }

  override def toScala(tau: Type): String = tau match {
    case ProductType(leftType, rightType) =>
      s"(${toScala(leftType)}, ${toScala(rightType)})"

    case _ =>
      super.toScala(tau)
  }
}
