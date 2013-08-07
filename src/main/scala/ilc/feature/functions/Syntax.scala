package ilc
package feature.functions

/**
 * Untyped lambda calculi with abstraction and application
 * extensible by constants and primitives
 */

import scala.language.implicitConversions
import scala.collection.GenTraversable
import ilc.util.UnionType._

trait Syntax {

  // SUBCLASS OBLIGATIONS

  type Constant

  // easy way to build up nested addition &co.
  trait NestingBinaryOperator { self: Constant =>
    def apply(lhs: Term, rhs: Term, others: Term*): Term =
      if (others.isEmpty)
        Const(this)(lhs)(rhs)
      else
        apply(apply(lhs, rhs), others.head, others.tail: _*)
  }

  // SYNTAX

  sealed abstract trait Term {
    // easy way to build up nested applications
    def apply(t: Term): App = App(this, t)

    // easy way to build up nested abstractions
    def ->:(name: String): Abs = Abs(name, this)
    def ->:(variable: Var): Abs = variable.name ->: this
    def ->:(parameterList: GenTraversable[String]): Term =
      if (parameterList.isEmpty)
        this
      else
        parameterList.head ->: parameterList.tail ->: this
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

  case class Const(c: Constant) extends Term

  // implicit conversion to stop writing `Const`
  implicit def liftConstant(c: Constant): Term = Const(c)

  // implicit conversion to stop writing `Var`
  implicit def liftVariable(name: String): Term = Var(name)

  implicit def liftTermPair[S, T]
    (p: (S, T))
    (implicit impS: S => Term, impT: T => Term): (Term, Term) =
      (impS(p._1), impT(p._2))
}
