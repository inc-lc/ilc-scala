package ilc
package feature
package products

import ilc.util.ExtractorTrait

trait Types extends base.Types with ExtractorTrait {
  case class ProductType(leftType: Type, rightType: Type) extends Type {
    override def toString = s"($leftType) x ($rightType)"
    override def traverse(f: Type => Type) = copy(f(leftType), f(rightType))
  }

  /** As in Agda, tuples are nested toward the right.
    * If tuples are nested toward the left,
    * then proj(i) is ill-defined.
    */

  def tupleType(elementTypes: Type*): Type =
    if (elementTypes.size == 1)
      elementTypes.head
    else
      ProductType(elementTypes.head, tupleType(elementTypes.tail: _*))

  def tupleTypeExtractor(n: Int): Extractor[Type, Seq[Type]] =
    new Extractor[Type, Seq[Type]] {
      def unapply(t: Type): Option[Seq[Type]] = t match {
        case ProductType(leftType, rightType) =>
          if (n == 2)
            Some(List(leftType, rightType))
          else if (n > 2)
            tupleTypeExtractor(n - 1).unapply(rightType).
              fold[Option[Seq[Type]]](None)(seq => Some(leftType +: seq))
          else
            sys error s"${n}-tuples are not supported"
      }
    }
}
