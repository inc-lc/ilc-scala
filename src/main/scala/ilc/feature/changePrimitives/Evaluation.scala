package ilc
package feature
package changePrimitives

trait Evaluation extends feature.functions.Evaluation {
  this: Syntax =>

  val Value: ChangePrimitiveValues with FunValues

  trait ChangePrimitiveValues {
    def diff(u: Value, v: Value): Value =
      throw new DiffError(u, v)
    def apply(dv: Value, v: Value): Value =
      throw new ApplyError(dv, v)
  }

  override def evalConst(c: Constant): Value = c match {
    case Diff =>
      (u: Value) => (v: Value) => Value.diff(u, v)

    case Apply =>
      (dv: Value) => (v: Value) => Value.apply(dv, v)

    case _ =>
      super.evalConst(c)
  }

  class DiffError(u: Value, v: Value)
  extends Exception("Unable to compute:\ndiff(" ++
    u.toString ++ ", " ++ v.toString ++ ")")

  class ApplyError(dv: Value, v: Value)
  extends Exception("Unable to compute:\napply(" ++
    dv.toString ++ ", " ++ v.toString ++ ")")
}
