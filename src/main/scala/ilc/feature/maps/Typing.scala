package ilc
package feature
package maps

trait Typing extends base.Typing {
  this: Syntax =>

  case class MapType(keyType: Type, valType: Type) extends Type

  // there is nothing else here: all maps constants are polymorphic.
}
