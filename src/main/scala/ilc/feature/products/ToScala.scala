package ilc
package feature
package products

trait ToScala extends base.ToScala with Syntax {
  private[this] def productType(a: Type, b: Type): String =
    toScala(ProductType(a, b))

  override def toScala(t: Term): String = t match {
    case Pair(aType, bType) => {
      val (a, b) = (toScala(aType), toScala(bType))
      s"((pairElem1: $a) => (pairElem2: $b) => (pairElem1, pairElem2))"
    }

    case Proj1(a, b) =>
      s"((pOfProj1: ${productType(a, b)}) => pOfProj1._1)"

    case Proj2(a, b) =>
      s"((pOfProj2: ${productType(a, b)}) => pOfProj2._2)"

    case _ =>
      super.toScala(t)
  }

  override def toScala(tau: Type): String = tau match {
    case ProductType(leftType, rightType) =>
      s"(${toScala(leftType)}, ${toScala(rightType)})"

    case _ =>
      super.toScala(tau)
  }
}
