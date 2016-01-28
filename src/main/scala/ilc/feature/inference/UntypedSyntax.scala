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

  def freeVars(t: UntypedTerm): Set[Name] = t match {
    case UVar(n)                 => Set(n)
    case UAbs(n, _, body)        => freeVars(body) - n
    case UApp(f, arg)            => freeVars(f) ++ freeVars(arg)
    case TypeAscription(t, _)    => freeVars(t)
    case UMonomorphicConstant(_) => Set.empty
    case UPolymorphicConstant(_) => Set.empty
  }
}

trait LetUntypedSyntax extends UntypedSyntax {
  this: base.Syntax with functions.Syntax =>

  case class ULet(variable: Name, exp: UntypedTerm, body: UntypedTerm) extends UntypedTerm
  override def freeVars(t: UntypedTerm) = t match {
    case ULet(v, exp, body) =>
      // Parens are important here: if there's shadowing, v can be free on the
      // LHS.
      freeVars(exp) ++ (freeVars(body) - v)
    case _ =>
      super.freeVars(t)
  }
}

trait LetRecUntypedSyntax extends UntypedSyntax {
  this: base.Syntax with functions.Syntax =>

  case class ULetRec(pairs: List[(Name, UntypedTerm)], bodyName: Name, body: UntypedTerm) extends UntypedTerm
  override def freeVars(t: UntypedTerm) = t match {
    case ULetRec(pairs, _, body) =>
      freeVars(body) ++ (pairs flatMap (x => freeVars(x._2))) -- (pairs map (_._1))
    case _ =>
      super.freeVars(t)
  }
}
