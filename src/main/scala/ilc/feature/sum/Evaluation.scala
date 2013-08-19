package ilc
package feature.sum

import scala.language.implicitConversions

trait Evaluation extends feature.base.Evaluation {
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
