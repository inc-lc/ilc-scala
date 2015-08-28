package ilc
package feature
package base

import ilc.feature.inference.Reflection

trait Types extends PrettyTypes {
  // We want subclasses of Type to be case classes.
  // If subclass is not a case class/case object, then they
  // fail to instantiate at compile time (a good thing) with
  // an obscure error message (a necessary evil).
  //
  // In unification we use the product iterator to map over all fields and we expect those to be types themselves.
  // If we ever want non-type fields we need to adapt ilc.feature.inference.Inference.unification
  trait Type extends Product with PrettyPrintable {
    def traverse(f: Type => Type): Type =
      this
  }

  // ERROR THROWERS
  def typeErrorWrongType(term: String, actual: Type, expected: Type): Nothing =
    throw TypeError(s"$term has type ${pretty(actual)} but should have type ${pretty(expected)}")

  def typeErrorNotTheSame(context: String, expected: Any, actual: Any) =
    throw TypeError(s"expected $expected instead of $actual in $context")

  def typeErrorNotInContext(name : Any) =
    throw TypeError(s"identifier ${name} not found in typing context")
}

case class TypeError(msg: String)
extends Exception(s"Type error: $msg")

case class IDontKnow(what: String)
extends Exception(s"I don't know $what")
