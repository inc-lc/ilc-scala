package ilc.feature.inference

trait Integers
extends Inference
   with ilc.feature.integers.Syntax
{
  override def collectConstraints(term: UntypedTerm, context: Context): (TypedTerm, Set[Constraint]) = term match {
    case UTerm(t@LiteralInt(_)) => (TTerm(t, IntType), emptyConstraintSet)
    case _ => super.collectConstraints(term, context)
  }

  override def substitute(typ: InferredType, substitutions: Map[TypeVariable, InferredType]): InferredType = typ match {
    case IntType => typ
    case _ => super.substitute(typ, substitutions)
  }

  override def substitute(term: TypedTerm, substitutions: Map[TypeVariable, InferredType]): TypedTerm = term match {
    case TTerm(LiteralInt(_), IntType) => term
    case _ => super.substitute(term, substitutions)
  }

  /* I think this is everything that needs to be overridden to extend type inference.

    Observations:
    - Maybe overloading substitute for different stuff is not so clever. It is not immediately clear that only two of
      the substitute methods need to be overwritten.
    - Implement stuff for TTerm and UTerm once and require that they are primitive enough?
      - no constraints
      - always return the same inner term as a TTerm, with its getType as the type
        (maybe change TTerm then to just call getTerm? or extend Term to TypedTerm?)
      - always false for occurs
      - identity substitution and no recursion in both cases


    How do Ints work?
    Shouldn't these have two and one children, respectively?

  case object PlusInt extends Term {
    override def getType: Type = IntType =>: IntType =>: IntType
  }

  case object NegateInt extends Term {
    override def getType: Type = IntType =>: IntType
  }

    I'm consufed...
    Has this to do with "prettier" syntax? With the TermBuilder and !-chaining and this kind of stuff?

    */
}
