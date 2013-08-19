package ilc
package feature.naturals

import scala.language.implicitConversions

// EvaluationBase is the part of Evaluation which does not depend on functions.
// See comments in feature.sum.EvaluationBase for more info.
trait EvaluationBase extends feature.base.Evaluation {
  this: Syntax =>

  trait NatValues {
    case class Nat(toNat: Int) extends Value {
      require(toNat >= 0)
    }
  }
  val Value: NatValues

  implicit def liftNat(n: Int): Value = Value.Nat(n)
  implicit class NatOps(value: Value) {
    def toNat: Int =
      value match {
        case Value.Nat(n) => n
        case _ => die("toNat")
      }
  }
}

trait Evaluation extends EvaluationBase {
  this: feature.functions.Evaluation with Syntax =>

  override def evalConst(c: Constant): Value = c match {
    case Nat(n) =>
      n

    case FoldNat =>
      (z: Value) => (f: Value) => (n: Value) => {
        def loop(i: Int): Value =
          if (i == 0)
            z
          else
            f(loop(i - 1))
        loop(n.toNat)
      }

    case Plus =>
      (x: Value) => (y: Value) => x.toNat + y.toNat

    case _ =>
      super.evalConst(c)
  }
}
