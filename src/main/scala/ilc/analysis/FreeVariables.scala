package ilc
package analysis

import ilc.feature.functions

trait FreeVariables
extends functions.Context
{
  implicit class FreeVariablesOfSubtrees(subtree: Subtree) {
    def freeVariables: Set[Var] = subtree.toTerm.freeVariables
  }

  def termFreeVariables(term: Term): Set[Var] = term match {
    case variable: Var =>
      Set(variable)

    case Abs(variable, body) =>
      body.freeVariables - variable

    case App(operator, operand) =>
      operator.freeVariables ++ operand.freeVariables

    case _ =>
      Set.empty
  }

  implicit class FreeVariablesOfTerms(term: Term) {
    def freeVariables: Set[Var] = termFreeVariables(term)
  }
}
