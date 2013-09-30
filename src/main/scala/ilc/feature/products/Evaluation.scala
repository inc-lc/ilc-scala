package ilc
package feature
package products

trait Evaluation
extends functions.Evaluation
   with Syntax
{
  case class PairValue(pair: (Value, Value)) extends Value {
    override def toString = pair.toString
  }

  implicit class PairValueOps(value: Value) {
    def toPair: (Value, Value) = value match {
      case PairValue(pair) => pair
      case _ => typeErrorNotTheSame("Value.toPair", "PairValue", value)
    }
  }

  override def coreEval(t: Term, env: Env) = t match {
    case Pair(leftType, rightType) =>
      (left: Value) => (right: Value) => PairValue((left, right))

    case Proj1(leftType, rightType) =>
      (pair: Value) => pair.toPair._1

    case Proj2(leftType, rightType) =>
      (pair: Value) => pair.toPair._2

    case _ =>
      super.coreEval(t, env)
  }
}
