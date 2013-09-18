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
  override def coreEval(t: Term, env: Env): Value =
    t match {
      case Abs(x, t) =>
        (arg: Value) => wrapEval(t, env.updated(x.getName, arg))
      case App(s, t) =>
        wrapEval(s, env)(wrapEval(t, env))
      case _ =>
        super.coreEval(t, env)
    }

  case class FunctionValue(operator: Value => Value) extends Value

  implicit class FunctionValueOps(value: Value) {
    // "toFunction"
    def apply(arg: Value): Value =
      value match {
        case FunctionValue(f) => f(arg)
        case _ => value die("apply", arg)
      }
  }

  implicit def liftFunctionValue[T <% Value](f: Value => T): Value =
    FunctionValue(x => f(x))
    // `f` is written pointfully so that implicit conversion
    // on the return value of `f` may kick in to make the
    // whole expression well-typed
}
