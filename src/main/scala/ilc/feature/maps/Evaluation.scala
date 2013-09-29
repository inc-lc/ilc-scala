package ilc
package feature.maps

import scala.language.implicitConversions
import scala.collection.immutable

// Map evaluation - what only depends on base.Evaluation
trait EvaluationBase extends feature.base.Evaluation with Syntax {
  // ValueMap is a map between values.
  // MapValue is the result of evaluating an expression of MapType.

  private[this] type ValueMap = immutable.Map[Value, Value]

  case class MapValue(toMap: ValueMap) extends Value {
    override def toString = toMap.toString
  }

  object MapValue {
    def apply[K <% Value, V <% Value](assoc: (K, V)*): MapValue = {
      val assocValues: Seq[(Value, Value)] = assoc map {
        case (key, value) =>
          //The type ascription invokes implicit conversions.
          (key: Value, value: Value)
      }
      MapValue(immutable.Map.apply(assocValues: _*))
    }
  }

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
      MapValue()

    case Update(_, _) =>
      (k: Value) => (v: Value) => (m: Value) =>
        MapValue(m.toMap.updated(k, v))

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
