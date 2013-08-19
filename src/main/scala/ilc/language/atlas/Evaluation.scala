/**
 * Atlas's evaluator
 */

package ilc
package language.atlas

import scala.language.implicitConversions
import scala.collection.immutable
import ilc.feature._

trait Evaluation
extends functions.Evaluation with changePrimitives.Evaluation { self: language.atlas.Syntax =>

  type ValueMap = immutable.Map[Value, Value]
  def ValueMap(assoc: (Value, Value)*): ValueMap =
    immutable.Map.apply[Value, Value](assoc: _*)

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
  object AtlasValueDeclarations extends FunValues with ChangePrimitiveValues {

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
      override def isNeutral = false
    }

    object Bool {
      def apply(b: Boolean): Value =
        if (b) True else Neutral

      def unapply(v: Value): Option[Boolean] = v match {
        case Neutral => Some(Neutral.toBool)
        case True => Some(True.toBool)
        case _ => None
      }
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

      def unapply(v: Value): Option[Int] = v match {
        case Neutral => Some(Neutral.toInt)
        case nonzero: Nonzero => Some(nonzero.toInt)
        case _ => None
      }
    }

    object NonNilNumericChange {
      def unapply(v: Value): Option[(Int, Int)] = v match {
        case NonemptyMap(m) if (m.size == 1) => m.head match {
          case (Num(base), Num(summand)) => Some(base -> summand)
          case _ => None
        }
        case _ => None
      }
    }

    class NonemptyMap(_m: ValueMap) extends AtlasValue {
      override val toMap = _m.withDefaultValue(Neutral)
      override def isNeutral = false

      override def toString = "Map(" ++ toMap.toString ++ ")"
      override def hashCode = toMap.hashCode
      override def equals(that: Any) = that match {
        case thatMap: NonemptyMap =>
          this.toMap == thatMap.toMap
        case _ =>
          false
      }

      // sanity check
      if (toMap.isEmpty)
        sys error "empty nonempty-map detected"
    }

    object NonemptyMap {
      def unapply(v: Value): Option[ValueMap] = v match {
        case m: NonemptyMap => Some(m.toMap)
        case _ => None
      }
    }

    object Map {
      def apply(m: ValueMap): Value = {
        val theEffectiveMap = m filter { p => ! p._2.isNeutral }
        if (theEffectiveMap.isEmpty)
          Neutral
        else
          new NonemptyMap(theEffectiveMap)
      }

      def unapply(v: Value): Option[ValueMap] = v match {
        case Neutral => Some(Neutral.toMap)
        case nonempty: NonemptyMap => Some(nonempty.toMap)
        case _ => None
      }
    }

    // helper functions of evalConst
    // or: object functions lifted to the meta level

    def xor(x: Boolean, y: Boolean): Boolean =
      (x && ! y) || (! x && y)

    def zip(f: Value, m1: Value, m2: Value): Value = {
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

    def diff(u: Value, v: Value): Value = (u, v) match {
      case (Bool(x), Bool(y)) =>
        xor(x, y)
      case (Num(n1), Num(n2)) =>
        ValueMap(n2 -> (n1 - n2))
      case (Map(m1), Map(m2)) =>
        zip((_: Value) => (v1: Value) => (v2: Value) => diff(v1, v2),
            m1, m2)
      case (Function(f), Function(g)) =>
        (x: Value) => (dx: Value) => diff(f(apply(dx, x)), g(x))
    }

    def apply(dv: Value, v: Value): Value =  (dv, v) match {
      case (Bool(dx), Bool(x)) =>
        xor(x, dx)
      case (Neutral, Num(n)) => n
      case (NonNilNumericChange(base, summand), Num(n)) =>
        if (base == n)
          n + summand
        else
          sys.error("trying to apply invalid change (" ++
            base.toString ++ " -> (+) " ++ summand.toString ++
            ") to " ++ v.toString)
      case (Map(dm), Map(m)) =>
        zip((_: Value) => (dv: Value) => (v: Value) => apply(dv, v),
            dm, m)
      case (Function(df), Function(f)) =>
        (x: Value) => {
          val nilChange = x match {
            case _: AtlasValue => Neutral
            case _ => diff(x, x)
          }
          apply(df(x)(nilChange),  f(x))
        }
    }
  }

  override def evalConst(c: Constant): Value = c match {
    case Diff =>
      (u: Value) => (v: Value) => Value.diff(u, v)

    case Apply =>
      (dv: Value) => (v: Value) => Value.apply(dv, v)

    case True =>
      true

    case False =>
      false

    case Xor =>
      (x: Value) => (y: Value) => Value.xor(x.toBool, y.toBool)

    case Num(i) =>
      i

    case Plus =>
      (x: Value) => (y: Value) => x.toInt + y.toInt

    case Negate =>
      (x: Value) => - x.toInt

    // TODO: make the following cases independent of type params.
    case Empty =>
      Value.Neutral

    case Update =>
      (key: Value) => (value: Value) => (map: Value) =>
        if (value.isNeutral)
          map.toMap - key
        else
          map.toMap.updated(key, value)

    // Lookup and Zip are evaluated correctly only if all
    // ValueMaps at runtime as Value.Neutral as default value.

    case Lookup =>
      (key: Value) => (map: Value) => map.toMap(key)

    case Zip =>
      (f: Value) => (m1: Value) => (m2: Value) =>
        Value.zip(f, m1, m2)

    case Fold =>
      (f: Value) => (z: Value) => (map: Value) =>
        map.toMap.foldRight(z)((p, b) => f(p._1)(p._2)(b))

    case _ =>
      super.evalConst(c)
  }
}
