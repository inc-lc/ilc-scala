package ilc
package feature.functions

/**
 * Evaluating untyped lambda calculus
 * extensible by constants and primitives
 */

import scala.language.implicitConversions
import scala.collection.immutable
import java.lang.IllegalArgumentException
import javax.management.modelmbean.InvalidTargetObjectTypeException

trait Evaluation { self: Syntax =>

  def evalConst(c: Constant): Value

  type Env = immutable.Map[String, Value]

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

  trait Value {
    // "toFunction"
    def apply(argument: Value): Value = die("apply")

    def die(from: String): Nothing =
      throw new
        InvalidTargetObjectTypeException(this.getClass.getName ++
          "." ++ from)
  }

  val Value = new ValueDeclarations

  class ValueDeclarations {
    case class Function(operator: Value => Value) extends Value {
      override def apply(operand: Value): Value = operator(operand)
    }
  }

  implicit def liftFunction[T <% Value](f: Value => T): Value =
    Value.Function(x => f(x))
    // `f` is written pointfully so that implicit conversion
    // on the return value of `f` may kick in to make the
    // whole expression well-typed

  implicit def liftValuePair[S, T]
    (p: (S, T))
    (implicit impS: S => Value, impT: T => Value): (Value, Value) =
      (impS(p._1), impT(p._2))
}
