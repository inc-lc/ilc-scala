package ilc
package feature.maps

import scala.language.implicitConversions
import scala.collection.immutable

trait EvaluationBase extends feature.base.Evaluation {
  type ValueMap = immutable.Map[Value, Value]
  def ValueMap(assoc: (Value, Value)*): ValueMap =
    immutable.Map.apply[Value, Value](assoc: _*)

  implicit class MapOps(value: Value) {
    def toMap: ValueMap =
      value match {
        case Value.Map(m) => m
        case _ => die(value, "toMap")
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

trait Evaluation extends EvaluationBase {
  this: feature.functions.Evaluation with Syntax =>

  override def evalConst(c: Constant): Value = c match {
    case EmptyMap =>
      ValueMap()

    case Update =>
      (k: Value) => (v: Value) => (m: Value) =>
        m.toMap.updated(k, v)

    case Lookup =>
      (k: Value) => (m: Value) => m.toMap.withDefault(
        k => throw new
          java.util.NoSuchElementException(
            "key " ++ k.toString ++
            " not found in " ++ m.toMap.toString)
      )(k)

    case Fold =>
      (f: Value) => (z: Value) => (map: Value) =>
        map.toMap.foldRight(z)((p, b) => f(p._1)(p._2)(b))

    case _ =>
      super.evalConst(c)
  }
}
