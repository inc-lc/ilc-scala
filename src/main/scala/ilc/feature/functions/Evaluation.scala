package ilc
package feature
package functions

/**
 * Evaluating untyped lambda calculus
 * extensible by constants and primitives
 */

import scala.collection.immutable
import scala.language.implicitConversions

trait Evaluation extends base.Evaluation with Syntax {
  override def evalWithEnv(t: Term, env: Env): Value =
    t match {
      case Abs(x, t) =>
        (arg: Value) => evalWithEnv(t, env.updated(x, arg))
      case App(s, t) =>
        evalWithEnv(s, env)(evalWithEnv(t, env))
      case Var(name) =>
        env(name) // NoSuchElementException = free var
      case _ =>
        super.evalWithEnv(t, env)
    }

  val Value: FunValues

  trait FunValues {
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
