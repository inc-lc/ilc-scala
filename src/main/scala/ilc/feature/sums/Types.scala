package ilc
package feature
package sums

trait Types extends base.Types {
  case class SumType(leftType: Type, rightType: Type) extends Type
}
