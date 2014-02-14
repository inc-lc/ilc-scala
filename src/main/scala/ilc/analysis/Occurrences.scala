package ilc
package analysis

import ilc.feature.functions

trait Occurrences extends functions.Syntax {
  case class UsageCount private(n: Option[Int]) {
    def +(that: UsageCount): UsageCount = {
      UsageCount build (for {
        a <- this.n
        b <- that.n
      } yield a + b)
    }
  }

  object UsageCount {
    val zero = new UsageCount(Some(0))
    val one = new UsageCount(Some(1))
    val more = new UsageCount(None)
    private def build(n: Option[Int]) = {
      n match {
        case Some(0) => zero
        case Some(1) => one
        case _ => more
      }
    }
  }

  implicit class Occurrences(val term: Term) {
    import UsageCount._
    //Occurrences under lambdas should count more.
    def occurrencesOf(soughtV: Variable): UsageCount = term match {
      case variable: Variable =>
        if (variable == soughtV) one else zero
      case Abs(variable, body) if variable == soughtV =>
        zero
      case Abs(variable, body) =>
        body.occurrencesOf(soughtV)
      case App(operator, operand) =>
        operator.occurrencesOf(soughtV) + operand.occurrencesOf(soughtV)
      case _ => zero
    }
  }
}
