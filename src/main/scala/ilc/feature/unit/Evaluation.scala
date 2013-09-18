package ilc
package feature.unit

import scala.language.implicitConversions
import scala.collection.immutable

trait Evaluation extends feature.base.Evaluation {
  this: Syntax =>

  // the inhabitant of unit type has no computation content
  case object UnitValue extends Value

  override def coreEval(t: Term, env: Env): Value = t match {
    case UnitTerm => UnitValue
    case _ => super.coreEval(t, env)
  }
}
