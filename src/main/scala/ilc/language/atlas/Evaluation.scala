/**
 * Atlas's evaluator
 */

package ilc
package language.atlas

import scala.language.implicitConversions
import scala.collection.immutable
import ilc.feature.functions

trait Evaluation
extends functions.Evaluation { self: language.atlas.Syntax =>

  type ValueMap = immutable.Map[Value, Value]

  // SPECIALIZE VALUE TO ATLAS

  // boilerplate for adding methods to Value trait
  implicit def toAtlas(value: Value): AtlasValue = value match {
    case av: AtlasValue => av
    case _ => AtlasValue.default
  }

  class AtlasValue extends Value {
    def toBool: Boolean = die("toBool")
    def toInt: Int = die("toInt")
    def toMap: ValueMap = die("toMap")
    def isNeutral: Boolean = die("isNeutral")
  }

  object AtlasValue {
    val default = new AtlasValue
  }

  implicit def liftNum(i: Int): Value = Value.Num(i)
  implicit def liftBool(b: Boolean): Value = Value.Bool(b)
  implicit def liftMap(m: ValueMap): Value = Value.Map(m)

  // boilerplate for extending value declarations
  override val Value = AtlasValueDeclarations
  object AtlasValueDeclarations extends ValueDeclarations {

    // the unique neutral element in the dynamic type system
    case object Neutral extends AtlasValue {
      override def toBool = false
      override def toInt = 0
      override def isNeutral = true

      // there should only be two instances where any ValueMap
      // is created from a Value: in Neutral.toMap, and in
      // NonemptyMap.toMap. If both places ensure that Neutral
      // is the default value, then it is guaranteed that all
      // ValueMaps at runtime have Neutral as the default value.
      override val toMap =
        immutable.Map.empty[Value, Value].withDefaultValue(this)
    }

    case object True extends AtlasValue {
      override def toBool = true
      override def isNeutral = toBool == false
    }

    object Bool {
      def apply(b: Boolean): Value = if (b) True else Neutral
    }

    case class Nonzero(override val toInt: Int) extends AtlasValue {
      override def isNeutral = false

      override def toString = "Num(" ++ toInt.toString ++ ")"

      // sanity check
      if (toInt == 0)
        sys error "nonzero zero detected"
    }

    object Num {
      def apply(i: Int): Value =
        if (i != 0)
          new Nonzero(i)
        else
          Neutral
    }

    class NonemptyMap(_m: ValueMap) extends AtlasValue {
      override val toMap = _m.withDefaultValue(Neutral)
      override def isNeutral = false

      override def toString = "Map(" ++ toMap.toString ++ ")"
      override def hashCode = toMap.hashCode
      override def equals(that: Any) =
        if (that.isInstanceOf[NonemptyMap])
          this.toMap == that.asInstanceOf[NonemptyMap].toMap
        else
          false

      // sanity check
      if (toMap.isEmpty)
        sys error "empty nonempty-map detected"
    }

    object Map {
      def apply(m: ValueMap): Value = {
        if (m.isEmpty)
          Neutral
        else
          new NonemptyMap(m)
      }
    }
  }

  def evalConst(c: Constant): Value = c match {
    case True =>
      true

    case False =>
      false

    case Xor =>
      (x: Value) => (y: Value) =>
        (x.toBool && ! y.toBool) || (! x.toBool && y.toBool)

    case Num(i) =>
      i

    case Plus =>
      (x: Value) => (y: Value) => x.toInt + y.toInt

    case Negate =>
      (x: Value) => - x.toInt

    // TODO: make the following cases independent of type params.
    case _: Empty =>
      Value.Neutral

    case _: Update =>
      (key: Value) => (value: Value) => (map: Value) =>
        if (value.isNeutral)
          map.toMap - key
        else
          map.toMap.updated(key, value)

    // Lookup and Zip are evaluated correctly only if all
    // ValueMaps at runtime as Value.Neutral as default value.

    case _: Lookup =>
      (key: Value) => (map: Value) => map.toMap(key)

    case _: Zip =>
      (f: Value) => (m1: Value) => (m2: Value) => {
        val (map1, map2) = (m1.toMap, m2.toMap)
        val keySet: Set[Value] = map1.keySet ++ map2.keySet
        // grossResult is defined so as to specify the type
        // of the result of keySet.map. otherwise the inferred
        // return value of the anonymous function is not ValueMap
        // and has no implicit conversion to Value.
        val grossResult: ValueMap = keySet.map({
          key => key -> f(key)(map1(key))(map2(key))
        })(collection.breakOut)
        grossResult filter {
          p => ! p._2.isNeutral
        }
      }

    case _: Fold =>
      (f: Value) => (z: Value) => (map: Value) =>
        map.toMap.foldRight(z)((p, b) => f(p._1)(p._2)(b))
  }
}
