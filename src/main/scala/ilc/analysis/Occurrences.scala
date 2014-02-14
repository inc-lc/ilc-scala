package ilc
package analysis

import ilc.feature.functions

trait Occurrences extends functions.Syntax {
  implicit class Occurrences(val term: Term) {
    //Occurrences under lambdas should count more.
    def occurrencesOf(soughtV: Variable): Int = term match {
      case variable: Variable =>
        if (variable == soughtV) 1 else 0
      case Abs(variable, body) if variable == soughtV =>
        0
      case Abs(variable, body) =>
        body.occurrencesOf(soughtV)
      case App(operator, operand) =>
        operator.occurrencesOf(soughtV) + operand.occurrencesOf(soughtV)
      case _ => 0
    }
  }
}
