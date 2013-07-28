package ilc
package feature.functions

/**
 * Untyped lambda calculi with abstraction and application
 * extensible by constants and primitives
 */

import scala.language.implicitConversions

trait Syntax {

  // SUBCLASS OBLIGATIONS

  type Constant

  // SYNTAX

  // Terms are parametric in the set `Constant` of constants.

  sealed abstract trait Term

  case class Var(index: Int) extends Term
  case class App(operator: Term, operand: Term) extends Term
  case class Abs(name: String, body: Term) extends Term
  // The first argument of abstraction serves as documentation
  // alone. Variables are de-Bruijn indices.

  case class Const(c: Constant) extends Term

  // implicit conversion to stop writing `Const`
  implicit def liftConstant(c: Constant): Term = Const(c)

  // WEAKENING

  def weaken(adjustIndex: Int => Int, t: Term): Term = t match {
    case c: Const => c
    case Var(i: Int) => Var(adjustIndex(i))
    case App(s1, s2) =>
      new App(weaken(adjustIndex, s1), weaken(adjustIndex, s2))
    case Abs(x, s) => new Abs(x,
      weaken(i => if (i <= 0) i else 1 + adjustIndex(i - 1), s))
  }
}
