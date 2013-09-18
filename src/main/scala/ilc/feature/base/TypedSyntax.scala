package ilc
package feature
package base

trait TypedSyntax extends Syntax with Typing {
  case class TypedConst(c: Constant, tau: Type) extends Term
}
