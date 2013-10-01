package ilc
package feature
package abelianGroups

trait Types extends functions.Types {
  case class AbelianGroupType(elType: Type) extends Type

  def binOpType(e: Type) = e =>: e =>: e
  def invType(e: Type) = e =>: e
}
