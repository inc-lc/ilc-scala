package ilc
package feature
package functions

import scala.collection.GenTraversable
import ilc.util.UnionType._

trait TypedSyntax extends Syntax with Typing {

  // Usage:
  //
  // Lambda("x" :: NatType, "y" :: NatType) ->: Plus("x", "y")
  //
  // -OR-
  //
  // val List(x, y) = uniqueVars(namesToAvoid, "x", "y")
  // Lambda(x :: NatType, y :: NatType) ->: Plus(x, y)

  case class TypedParameter(name: String, theType: Type)
  extends Parameter {
    def attachBody(body: Term): Abs =
      TypedAbs(name, theType, body)
  }

  implicit class TypeOps(tau0: Type) {
    def :: [T: Or[String, Var]#Type](x: T): TypedParameter = {
      val name = x match {
        case s: String => s
        case y: Var => y.name
      }
      TypedParameter(name, tau0)
    }
  }

  // TypedAbs should be a subclass of Abs in order to take
  // advantage of all infrastructures made for Abs.
  // It would be good to be a case class, but case-to-case
  // inheritance is prohibited.
  class TypedAbs(name: String, val argumentType: Type, body: Term)
  extends Abs(name, body)

  object TypedAbs {
    def apply(name: String, argumentType: Type, body: Term): TypedAbs =
      new TypedAbs(name, argumentType, body)

    def unapply(t: TypedAbs): Option[(String, Type, Term)] =
      Some((t.name, t.argumentType, t.body))
  }
}

