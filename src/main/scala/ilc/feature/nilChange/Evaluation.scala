package ilc
package feature
package nilChange

/**
 * A constant to designate the lack of change
 */

trait Evaluation extends changePrimitives.Evaluation {
  this: Syntax with changePrimitives.Syntax =>

  trait NilChangeValues extends ChangePrimitiveValues {
    case object NilChange extends Value

    override def apply(dv: Value, v: Value): Value = dv match {
      case NilChange => v
      case _ => super.apply(dv, v)
    }
  }

  val Value: NilChangeValues with FunValues

  override def evalConst(c: Constant): Value = c match {
    case NilChange =>
      Value.NilChange

    case _ =>
      super.evalConst(c)
  }
}
