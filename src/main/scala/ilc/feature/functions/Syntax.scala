package ilc
package feature
package functions

/**
 * Untyped lambda calculi with abstraction and application
 * extensible by constants and primitives
 */

import scala.language.implicitConversions
import scala.collection.GenTraversable
import ilc.util.UnionType._

trait Syntax extends base.Syntax {
  // easy way to build up nested addition &co.
  trait NestingBinaryOperator { self: Constant =>
    def apply(lhs: Term, rhs: Term, others: Term*): Term =
      if (others.isEmpty)
        Const(this)(lhs)(rhs)
      else
        apply(apply(lhs, rhs), others.head, others.tail: _*)
  }

  // SYNTAX

  // begin: syntactic sugar for term constructors

  trait Parameter {
    def attachBody(body: Term): Abs
  }

  implicit class StringParameter(s0: String) extends Parameter {
    def attachBody(body: Term): Abs = Abs(s0, body)
  }

  implicit class VarParameter(x0: Var) extends Parameter {
    def attachBody(body: Term): Abs = Abs(x0.name, body)
  }

  implicit class TermOps(t0: Term) {
    // easy way to build up nested applications
    def apply(t: Term): App = App(t0, t)

    // easy way to build up nested abstractions
    def ->:(parameter: Parameter): Abs =
      parameter attachBody t0

    def ->:(parameterList: GenTraversable[Parameter]): Term =
      if (parameterList.isEmpty)
        t0
      else
        parameterList.head ->: parameterList.tail ->: t0
  }

  object Lambda {

    // Usage:
    //
    // Lambda("x", "y", "z") ->: Plus("x", "y", "z")
    //
    // -OR-
    //
    // val (x, y, z) = uniqueVars(namesToAvoid, "x", "y", "z")
    // Lambda(x, y, z) ->: Plus(x, y, z)

    def apply[T](parameters: T*)
      (implicit view: T => Parameter): GenTraversable[Parameter] =
      parameters map view
  }

  // end: syntatic sugar for term constructors

  case class Var(name: String) extends Term
  case class App(operator: Term, operand: Term) extends Term
  case class Abs(name: String, body: Term) extends Term

  // implicit conversion to stop writing `Var`
  implicit def liftVariable(name: String): Term = Var(name)

  //Chain implicit conversions.
  implicit def constToTermOps(c: Constant): TermOps = TermOps(c)
  implicit def stringToTermOps(s: String): TermOps = TermOps(s)

  implicit def liftTermPair[S, T]
    (p: (S, T))
    (implicit impS: S => Term, impT: T => Term): (Term, Term) =
      (impS(p._1), impT(p._2))
}
