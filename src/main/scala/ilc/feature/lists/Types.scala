package ilc
package feature
package lists

trait Types extends base.Types {
  case class ListType(elemType: Type) extends Type {
    override def toString = s"[$elemType]"
    override def traverse(f: Type => Type): Type = copy(f(elemType))
  }
}
