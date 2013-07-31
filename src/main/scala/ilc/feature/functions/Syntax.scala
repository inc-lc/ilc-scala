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

  sealed abstract trait Term {
    // easy way to build up nested applications
    def apply(t: Term): App = App(this, t)

    // easy way to build up nested abstractions
    def ->:(name: String): Abs = Abs(name, this)
    def ->:(variable: Var): Abs = variable.name ->: this
    def ->:(parameterList: ParameterList): Term = parameterList match {
      case EmptyParameterList =>
        this
      case NameBinding(name, otherBindings) =>
        name ->: otherBindings ->: this
    }
  }

  // helpers to syntactic sugar of abstraction
  // basically a list of strings. unfortunately List[String]
  // is a sealed class and can't be inherited.
  sealed trait ParameterList
  case object EmptyParameterList extends ParameterList
  case class NameBinding(name: String,
                         otherBindings: ParameterList)
  extends ParameterList
  object Lambda {
    // unsafe argument type signature Any* is needed to work around
    // scala's runtime type erasure
    def apply(stuff: Any*): ParameterList =
      if (stuff.isEmpty)
        EmptyParameterList
      else
        NameBinding(stuff.head match {
                      case name: String  => name
                      case variable: Var => variable.name
                    },
                    apply(stuff.tail: _*))
  }

  case class Var(name: String) extends Term
  case class App(operator: Term, operand: Term) extends Term
  case class Abs(name: String, body: Term) extends Term

  case class Const(c: Constant) extends Term

  // implicit conversion to stop writing `Const`
  implicit def liftConstant(c: Constant): Term = Const(c)
}
