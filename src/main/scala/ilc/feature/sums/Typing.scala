package ilc
package feature
package sums

trait Typing extends base.Typing {
  this: Syntax =>
  case class SumType(leftType: Type, rightType: Type) extends Type
}
