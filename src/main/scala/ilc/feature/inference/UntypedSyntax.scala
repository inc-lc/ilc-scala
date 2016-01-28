package ilc
package feature
package inference

trait UntypedSyntax {
  this: base.Syntax =>

  trait UntypedTerm

  case class UVar(getName: Name) extends UntypedTerm
  case class UAbs(argumentName: Name, typeAnnotation: Option[Type], body: UntypedTerm) extends UntypedTerm
  case class UApp(operator: UntypedTerm, operand: UntypedTerm) extends UntypedTerm
  case class UMonomorphicConstant(term: Term) extends UntypedTerm
  case class UPolymorphicConstant(term: PolymorphicConstant) extends UntypedTerm
  case class TypeAscription(term: UntypedTerm, typ: Type) extends UntypedTerm
}

trait LetUntypedSyntax extends UntypedSyntax {
  this: base.Syntax with functions.Syntax =>

  case class ULet(variable: Name, exp: UntypedTerm, body: UntypedTerm) extends UntypedTerm
}

trait LetRecUntypedSyntax extends UntypedSyntax {
  this: base.Syntax with functions.Syntax =>

  case class ULetRec(pairs: List[(Name, UntypedTerm)], bodyName: Name, body: UntypedTerm) extends UntypedTerm
}
