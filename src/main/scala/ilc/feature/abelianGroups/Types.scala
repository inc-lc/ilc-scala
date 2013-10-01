package ilc
package feature
package groups

trait Types extends functions.Types {
  case class GroupType(elType: Type) extends Type

  def binOpType(e: Type) = e =>: e =>: e
  def invType(e: Type) = e =>: e
}
