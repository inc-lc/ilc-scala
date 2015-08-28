package ilc
package feature
package bintrees

trait Types extends base.Types {
  case class BinTreeType(elemType: Type) extends Type {
    override def traverse(f: Type => Type): Type = copy(f(elemType))
  }
}
