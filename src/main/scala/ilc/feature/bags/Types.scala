package ilc
package feature
package bags

trait Types extends base.Types {
  case class BagType(valType: Type) extends Type
}
