package ilc
package feature.maps

import scala.language.implicitConversions
import scala.collection.immutable

trait EvaluationBase extends feature.base.Evaluation
   with feature.maybe.SumUnitEncoded
{
  type ValueMap = immutable.Map[Value, Value]
  def ValueMap(assoc: (Value, Value)*): ValueMap =
    immutable.Map.apply[Value, Value](assoc: _*)

  implicit class MapOps(value: Value) {
    def toMap: ValueMap =
      value match {
        case Value.Map(m) => m
        case _ => value die "toMap"
      }
  }

  implicit def liftMap(m: ValueMap): Value = Value.Map(m)

  // Basic Map values
  trait MapValues extends MaybeValues {
    case class Map(toMap: ValueMap) extends Value

    object Map {
      def apply(assoc: (Value, Value)*): Map =
        Map(immutable.Map(assoc: _*))
    }
  }

  // the object `Value` needs deep mixin composition.
  //
  // the object `Value` serves as a name space so that we can write
  // `Value.Map`, and it will be distinct from the syntax tree node
  // `Map` inherted from the `Syntax` trait.
  //
  // Question: Is there any way to support namespaces for inner-class-
  // declarations without doing deep mixin composition?

  val Value: MapValues with MaybeValues
}

trait Evaluation extends EvaluationBase {
  this: Syntax =>

  override def evalConst(c: Constant): Value = c match {
    case EmptyMap =>
      ValueMap()

    case Update =>
      (k: Value) => (v: Value) => (m: Value) =>
        m.toMap.updated(k, v)

    case Lookup =>
      (k: Value) => (m: Value) => lowerMaybe(m.toMap.get(k))

    case Fold =>
      (f: Value) => (z: Value) => (map: Value) =>
        map.toMap.foldRight(z)((p, b) => f(p._1)(p._2)(b))

    case _ =>
      super.evalConst(c)
  }
}
