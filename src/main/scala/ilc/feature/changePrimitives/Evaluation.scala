package ilc
package feature
package changePrimitives

trait Evaluation extends feature.functions.Evaluation {
  this: Syntax =>

  val Value: ChangePrimitiveValues with FunValues

  trait ChangePrimitiveValues {
    def diff(u: Value, v: Value): Value =
      diffMatchError(u, v)
    def apply(dv: Value, v: Value): Value =
      applyMatchError(dv, v)
  }

  override def evalConst(c: Constant): Value = c match {
    case Diff =>
      (u: Value) => (v: Value) => Value.diff(u, v)

    case Apply =>
      (dv: Value) => (v: Value) => Value.apply(dv, v)

    case _ =>
      super.evalConst(c)
  }

  private def diffMatchError(u: Value, v: Value): Nothing =
    throw new MatchError("diff(" ++
      u.toString ++ ", " ++ v.toString ++ ")")

  private def applyMatchError(dv: Value, v: Value): Nothing =
    throw new MatchError("apply(" ++
      dv.toString ++ ", " ++ v.toString ++ ")")
}
