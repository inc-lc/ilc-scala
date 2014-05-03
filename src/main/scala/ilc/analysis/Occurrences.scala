package ilc
package analysis

import ilc.feature._

trait Occurrences extends functions.Syntax with let.Syntax {
  case class UsageCount private (n: Option[Int]) {
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
    def occurrencesOf(soughtV: Var): UsageCount = term match {
      case variable: Var =>
        if (variable == soughtV) one else zero
      case Abs(variable, body) if variable == soughtV =>
        zero
      case Abs(variable, body) =>
        body.occurrencesOf(soughtV) match {
          case `zero` => zero
          //Rationale: if the variable is used once under lambda,
          //since this lambda might be reused, the argument might end up being
          //reused more often.
          //This is discussed in the "Once upon a type" paper on the GHC
          //optimizer; they propose a different solution, but ultimately they
          //scrapped the whole line of research and switched to a different
          //approach.
          case other =>
            more
        }
      case App(operator, operand) =>
        operator.occurrencesOf(soughtV) + operand.occurrencesOf(soughtV)
      case Let(v, exp, body) =>
        exp.occurrencesOf(soughtV) + {
          if (v == soughtV)
            zero
          else
            //In the body of a lambda, a single variable occurrence translates to many occurrences, because the lambda might be an argument to another function.
            //Since a let can be desugared to lambda, it seems that its body should be handled the same way.
            //However, a let corresponds to a lambda which is not an argument to another function, but which is immediately applied. In this case, we can propagate the occurrence count of the body without transforming it.
            //In other words, we can easily increase the precision of the analysis (though maybe we could do the same without Let by handling lambdas nested in App nodes).
            body.occurrencesOf(soughtV)
        }
      case _ => zero
    }
  }
}
