package ilc
package feature
package maps

trait Types extends base.Types {
  case class MapType(keyType: Type, valType: Type) extends Type {
    override def traverse(f: Type => Type): Type = copy(f(keyType), f(valType))
  }
}
