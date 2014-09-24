package ilc
package feature
package maybe

trait Types extends base.Types {
  case class MaybeType(contentType: Type) extends Type {
    override def traverse(f: Type => Type): Type = copy(f(contentType))
  }
}
