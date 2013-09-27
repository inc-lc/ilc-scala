package ilc
package analysis

import ilc.feature.functions

trait FreeVariables
extends functions.Context
{
  implicit class FreeVariablesOfSubterms(subterm: Subterm) {
    def freeVariables: Set[Variable] = subterm.toTerm.freeVariables
  }

  implicit class FreeVariablesOfTerms(term: Term) {
    def freeVariables: Set[Variable] = term match {
      case variable: Variable =>
        Set(variable)

      case Abs(variable, body) =>
        body.freeVariables - variable

      case App(operator, operand) =>
        operator.freeVariables ++ operand.freeVariables

      case _ =>
        Set.empty
    }
  }
}
