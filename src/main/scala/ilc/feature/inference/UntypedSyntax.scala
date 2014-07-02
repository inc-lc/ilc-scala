package ilc
package feature
package inference

trait UntypedSyntax {
  this: base.Syntax with functions.Syntax =>

  trait UntypedTerm

  case class UVar(getName: String) extends UntypedTerm
  case class UAbs(argumentName: String, typeAnnotation: Option[Type], body: UntypedTerm) extends UntypedTerm
  case class UApp(operator: UntypedTerm, operand: UntypedTerm) extends UntypedTerm
  case class UMonomorphicConstant(term: Term) extends UntypedTerm
  case class UPolymorphicConstant(term: PolymorphicConstant) extends UntypedTerm
  case class TypeAscription(term: UntypedTerm, typ: Type) extends UntypedTerm
}
