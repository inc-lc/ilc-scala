package ilc
package feature.maps

import scala.language.implicitConversions
import scala.collection.immutable

trait Evaluation extends feature.base.Evaluation {
  type ValueMap = immutable.Map[Value, Value]
  def ValueMap(assoc: (Value, Value)*): ValueMap =
    immutable.Map.apply[Value, Value](assoc: _*)

  implicit class MapOps(value: Value) {
    def toMap: ValueMap =
      value match {
        case Value.Map(m) => m
        case _ => die("toMap")
      }
  }

  implicit def liftMap(m: ValueMap): Value = Value.Map(m)

  // Basic Map values
  trait MapValues {
    case class Map(toMap: ValueMap) extends Value

    object Map {
      def apply(assoc: (Value, Value)*): Map =
        Map(immutable.Map(assoc: _*))
    }
  }
  val Value: MapValues
}
