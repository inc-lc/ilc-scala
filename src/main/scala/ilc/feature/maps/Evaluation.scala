package ilc
package feature.maps

import scala.language.implicitConversions
import scala.collection.immutable

// Map evaluation - what only depends on base.Evaluation
trait EvaluationBase extends feature.base.Evaluation with Syntax {
  // ValueMap is a map between values.
  // MapValue is the result of evaluating an expression of MapType.

  type ValueMap = immutable.Map[Value, Value]

  case class MapValue(toMap: ValueMap) extends Value

  implicit def liftMapValue(m: ValueMap): Value = MapValue(m)

  object MapValue {
    def apply[K <% Value, V <% Value](assoc: (K, V)*): MapValue = {
      val assocValues: Seq[(Value, Value)] = assoc map {
        case (key, value) => {
          val keyValue: Value = key
          val valValue: Value = value
          (keyValue, valValue)
        }
      }
      MapValue(immutable.Map.apply(assocValues: _*))
    }
  }

  def ValueMap(assoc: (Value, Value)*): ValueMap =
    immutable.Map.apply[Value, Value](assoc: _*)

  implicit class MapOps(value: Value) {
    def toMap: ValueMap =
      value match {
        case MapValue(m) => m
        case _ => value die "toMap"
      }
  }
}

trait Evaluation extends EvaluationBase with feature.maybe.Evaluation {
  override def coreEval(t: Term, env: Env): Value = t match {
    case EmptyMap(_, _) =>
      ValueMap()

    case Update(_, _) =>
      (k: Value) => (v: Value) => (m: Value) =>
        m.toMap.updated(k, v)

    case Lookup(_, _) =>
      (k: Value) => (m: Value) => MaybeValue(m.toMap.get(k))

    case Delete(_, _) =>
      (k: Value) => (m: Value) => MapValue(m.toMap - k)

    case Fold(_, _, _) =>
      (f: Value) => (z: Value) => (map: Value) =>
        map.toMap.foldRight(z)((p, b) => f(p._1)(p._2)(b))

    case _ =>
      super.coreEval(t, env)
  }
}
