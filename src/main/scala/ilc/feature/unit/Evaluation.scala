package ilc
package feature.unit

import scala.language.implicitConversions
import scala.collection.immutable

trait Evaluation extends feature.base.Evaluation {
  this: Syntax =>

  trait UnitValues {
    // the inhabitant of unit type has no computation content
    case object UnitValue extends Value
  }
  val Value: UnitValues

  override def evalConst(c: Constant): Value = c match {
    case UnitTerm =>
      Value.UnitValue
    case _ =>
      super.evalConst(c)
  }
}
