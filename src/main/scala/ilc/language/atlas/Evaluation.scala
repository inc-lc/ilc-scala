/**
 * Atlas's evaluator
 */

package ilc
package language.atlas

import scala.language.implicitConversions

import java.lang.IllegalArgumentException
import javax.management.modelmbean.InvalidTargetObjectTypeException

import collection.immutable

trait Evaluation { self: Syntax =>

  type Env = immutable.Map[String, Value]

  type ValueMap = immutable.Map[Value, Value]

  // VALUE

  sealed trait Value {
    def toBool: Boolean = die("toBool")
    def toInt: Int = die("toInt")
    def toMap: ValueMap = die("toMap")
    def apply(argument: Value): Value = die("apply") // "toFunction"

    def isNeutral: Boolean = die("isNeutral")

    def die(from: String): Nothing =
      throw new
        InvalidTargetObjectTypeException(this.getClass.getName ++
          "." ++ from)
  }

  // wrapping values in a module to avoid name space pollution
  object Value {

    // the unique neutral element in the dynamic type system
    case object Neutral extends Value {
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

    case object True extends Value {
      override def toBool = true
      override def isNeutral = toBool == false
    }

    object Bool {
      def apply(b: Boolean): Value = if (b) True else Neutral
    }

    case class Nonzero(override val toInt: Int) extends Value {
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

    class NonemptyMap(_m: ValueMap) extends Value {
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

    case class Function(operator: Value => Value) extends Value {
      override def apply(operand: Value): Value = operator(operand)
    }

    implicit def liftNum(i: Int): Value = Num(i)
    implicit def liftBool(b: Boolean): Value = Bool(b)
    implicit def liftMap(m: ValueMap): Value = Map(m)

    implicit def liftFunction[T <% Value](f: Value => T): Value =
      Function(x => f(x))
      // `f` is written pointfully so that implicit conversion
      // on the return value of `f` may kick in to make the
      // whole expression well-typed

  implicit def liftPair[S, T]
    (p: (S, T))
    (implicit impS: S => Value, impT: T => Value): (Value, Value) =
      (impS(p._1), impT(p._2))
  }

  def eval(t: Term): Value = try {
    evalWithEnv(t, immutable.Map.empty)
  } catch { case err: IllegalArgumentException =>
    throw new java.lang.
      IllegalArgumentException(err.getMessage() ++
        "\n in the term\n    " ++ t.toString)
  }

  def evalWithEnv(t: Term, env: Env): Value = try {
    t match {
      case Abs(x, t) =>
        (arg: Value) => evalWithEnv(t, env.updated(x, arg))
      case App(s, t) =>
        evalWithEnv(s, env)(evalWithEnv(t, env))
      case Var(name) =>
        env(name) // NoSuchElementException = free var
      case Const(c) =>
        evalConst(c)
    }
  } catch { case err: InvalidTargetObjectTypeException =>
    throw new
      IllegalArgumentException(err.getMessage() ++
        " when evaluating:\n    " ++ t.toString)
  }

  def evalConst(c: Constant): Value = c match {
    case True =>
      true

    case False =>
      false

    case Xor =>
      (x: Value) => (y: Value) =>
        Value.liftBool((x.toBool && ! y.toBool) ||
                       (! x.toBool && y.toBool))

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
