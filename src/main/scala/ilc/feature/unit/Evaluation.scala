package ilc
package feature.unit

import scala.language.implicitConversions
import scala.collection.immutable

trait Evaluation extends feature.base.Evaluation {
  trait UnitValues {
    // the inhabitant of unit type has no computation content
    case object UnitValue extends Value
  }
}
