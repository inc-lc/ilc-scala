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

  implicit class TermOps(t0: Term) {
    // easy way to build up nested applications
    def apply(t: Term): App = App(t0, t)

    // easy way to build up nested abstractions
    def ->:(name: String): Abs = Abs(name, t0)
    def ->:(variable: Var): Abs = variable.name ->: t0
    def ->:(parameterList: GenTraversable[String]): Term =
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

    def apply[Name: Or[String, Var]#Type](names: Name*): GenTraversable[String] =
      names map {
        _ match {
          case name: String => name
          case variable: Var => variable.name
        }
      }
  }

  case class Var(name: String) extends Term
  case class App(operator: Term, operand: Term) extends Term
  case class Abs(name: String, body: Term) extends Term

  // implicit conversion to stop writing `Var`
  implicit def liftVariable(name: String): Term = Var(name)

  //Chain implicit conversions.
  implicit def constToTermOps(c: Constant): TermOps = TermOps(c)
  implicit def stringToTermOps(name: String): TermOps = TermOps(name)

  implicit def liftTermPair[S, T]
    (p: (S, T))
    (implicit impS: S => Term, impT: T => Term): (Term, Term) =
      (impS(p._1), impT(p._2))
}
