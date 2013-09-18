package ilc
package feature.sums

import scala.language.implicitConversions

//EvaluationBase isolates the parts of the evaluation not depending on functions. However,
//since we have higher-order constants, most of evalConst needs functions.
//Hence, currently all of evalConst requires functions anyway - we could have
//even more granularity, but this seems already too much.

trait EvaluationBase extends feature.base.Evaluation with Syntax {
  type ValueSum = Either[Value, Value]

  implicit class SumOps(value: Value) {
    def toSum: ValueSum =
      value match {
        case SumValue(s) => s
        case _ => value die "toSum"
      }
  }

  case class SumValue(toSum: ValueSum) extends Value

  object Inj1Value {
    def apply(v: Value): SumValue = SumValue(Left(v))
    def unapply(s: SumValue): Option[Value] = s.toSum match {
      case Left(v) => Some(v)
      case _ => None
    }
  }

  object Inj2Value {
    def apply(v: Value): SumValue = SumValue(Right(v))
    def unapply(s: SumValue): Option[Value] = s.toSum match {
      case Right(v) => Some(v)
      case _ => None
    }
  }
}

trait Evaluation extends feature.functions.Evaluation with EvaluationBase {
  override def coreEval(t: Term, env: Env): Value = t match {
    case Inj1(_, _) =>
      (x: Value) => Inj1Value(x)

    case Inj2(_, _) =>
      (y: Value) => Inj2Value(y)

    case Either(_, _, _) =>
      (f: Value) => (g: Value) => (s: Value) =>
        s.toSum.fold(x => f(x), y => g(y))

    case _ =>
      super.coreEval(t, env)
  }
}
