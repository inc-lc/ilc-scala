package ilc
package feature
package sums

trait Syntax extends base.Syntax {
  // intro/elim forms of sum types
  // (sums of functions are forbidden)
  //
  //   either : (a → c) → (b → c) → a ⊎ b → c
  //
  case object Left extends Constant
  case object Right extends Constant
  case object Either extends Constant
}

trait SyntaxSugar extends Syntax with functions.Syntax with unit.Syntax {
  // booleans are encoded as sums
  val tt = Left(UnitTerm)
  val ff = Right(UnitTerm)
  val ifThenElse = Lambda("cond", "then", "else") ->:
    Either("_" ->: "then")("_" ->: "else")("cond")

  // shorthands for pattern matching

  def case2(matchAgainst: Term, caseLeft: Term, caseRight: Term): Term =
    Either(caseLeft)(caseRight)(matchAgainst)

  def case4(sample1: Term, sample2: Term,
            caseLL: Term, caseLR: Term,
            caseRL: Term, caseRR: Term): Term =
    case2(sample2,
      case2(sample1, caseLL, caseRL),
      case2(sample1, caseLR, caseRR))
}
