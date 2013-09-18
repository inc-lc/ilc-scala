package ilc
package feature
package maps

trait Types extends base.Types {
  case class MapType(keyType: Type, valType: Type) extends Type
}
