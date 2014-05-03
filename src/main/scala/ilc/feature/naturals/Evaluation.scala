package ilc
package feature
package naturals

import scala.language.implicitConversions

// EvaluationBase is the part of Evaluation which does not depend on functions.
// See comments in feature.sum.EvaluationBase for more info.
trait EvaluationBase extends feature.base.Evaluation with Syntax {

  case class NatValue(toNat: Int) extends Value {
    require(toNat >= 0)

    override def toString = toNat.toString
  }

  implicit def liftNatValue(n: Int): Value = NatValue(n)
  implicit class NatOps(value: Value) {
    def toNat: Int =
      value match {
        case NatValue(n) => n
        case _ => value die "toNat"
      }
  }
}

trait Evaluation
extends EvaluationBase
   with functions.Evaluation
{
  override def coreEval(t: Term, env: Env): Value = t match {
    case Nat(n) =>
      n

    case FoldNat(_) =>
      (z: Value) => (f: Value) => (n: Value) => {
        def loop(i: Int): Value =
          if (i == 0)
            z
          else
            f(loop(i - 1))
        loop(n.toNat)
      }

    case PlusNat =>
      (x: Value) => (y: Value) => x.toNat + y.toNat

    case _ =>
      super.coreEval(t, env)
  }
}
