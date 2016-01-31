package ilc
package feature
package sums

trait Reification extends base.Reification with Evaluation {
  override def reify(value: Value, valueType: Type): Term =
    (value, valueType) match {
      case (SumValue(Left(content)), SumType(leftType, rightType)) =>
        Inj1.tapply(rightType) ! reify(content, leftType)

      case (SumValue(Right(content)), SumType(leftType, rightType)) =>
        Inj2.tapply(leftType) ! reify(content, rightType)

      case _ =>
        super.reify(value, valueType)
    }
}
