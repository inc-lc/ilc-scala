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

  sealed abstract trait Term

  case class Var(name: String) extends Term
  case class App(operator: Term, operand: Term) extends Term
  case class Abs(name: String, body: Term) extends Term

  case class Const(c: Constant) extends Term

  // implicit conversion to stop writing `Const`
  implicit def liftConstant(c: Constant): Term = Const(c)
}
