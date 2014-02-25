package ilc.feature.inference

trait Integers
extends Inference
   with ilc.feature.integers.Syntax
{
  override def collectConstraints(term: UntypedTerm, context: Context): (TypedTerm, Set[Constraint]) = term match {
    case UTerm(t@PlusInt) => (TTerm(t, IntType =>: IntType =>: IntType), emptyConstraintSet)
    case UTerm(t@LiteralInt(_)) => (TTerm(t, IntType), emptyConstraintSet)
    case _ => super.collectConstraints(term, context)
  }

  override def substitute(term: TypedTerm, substitutions: Map[TypeVariable, Type]): TypedTerm = term match {
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
      - identity substitution and no recursion in both cases
    */
}
