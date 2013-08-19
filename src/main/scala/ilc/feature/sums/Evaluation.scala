package ilc
package feature.sums

import scala.language.implicitConversions

//EvaluationBase isolates the parts of the evaluation not depending on functions. However,
//since we have higher-order constants, most of evalConst needs functions.
//Hence, currently all of evalConst requires functions anyway - we could have
//even more granularity, but this seems already too much.

trait EvaluationBase extends feature.base.Evaluation {
  this: Syntax =>

  type ValueSum = Either[Value, Value]

  implicit class SumOps(value: Value) {
    def toSum: ValueSum =
      value match {
        case Value.Sum(s) => s
        case _ => die("toSum")
      }
  }

  trait SumValues {
    case class Sum(toSum: ValueSum) extends Value

    object Left {
      def apply(v: Value): Sum = Sum(scala.Left(v))
      def unapply(s: Sum): Option[Value] = s.toSum match {
        case scala.Left(v) => Some(v)
        case _ => None
      }
    }

    object Right {
      def apply(v: Value): Sum = Sum(scala.Right(v))
      def unapply(s: Sum): Option[Value] = s.toSum match {
        case scala.Right(v) => Some(v)
        case _ => None
      }
    }
  }

  val Value: SumValues
}

trait Evaluation extends feature.functions.Evaluation with EvaluationBase {
  this: Syntax =>

  override val Value: SumValues with FunValues

  override def evalConst(c: Constant): Value = c match {
    case Left =>
      (x: Value) => Value.Left(x)

    case Right =>
      (y: Value) => Value.Right(y)

    case Either =>
      (f: Value) => (g: Value) => (s: Value) =>
        s.toSum.fold(x => f(x), y => g(y))

    case _ =>
      super.evalConst(c)
  }
}
