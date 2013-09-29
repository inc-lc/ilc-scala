package ilc
package feature
package products

trait Types extends base.Types {
  case class ProductType(leftType: Type, rightType: Type) extends Type
}
