package ilc
package feature
package abelianGroups

trait Types extends functions.Types {
  case class AbelianGroupType(elType: Type) extends Type {
    override def traverse(f: Type => Type): Type = copy(f(elType))
  }

  def binOpType(e: Type) = e =>: e =>: e
  def invType(e: Type) = e =>: e
}
