package ilc
package feature
package base

import ilc.feature.inference.Reflection

trait Types {
  // We want subclasses of Type to be case classes.
  // If subclass is not a case class/case object, then they
  // fail to instantiate at compile time (a good thing) with
  // an obscure error message (a necessary evil).
  trait Type extends Product

  // ERROR THROWERS

  def typeErrorNotTheSame(context: String, expected: Any, actual: Any) =
    throw TypeError(s"expected $expected instead of $actual in $context")

  def typeErrorNotDefined(name : Any) =
    throw TypeError("undefined identifier " + name)
}

case class TypeError(msg: String)
extends Exception(s"Type error: $msg")

case class IDontKnow(what: String)
extends Exception(s"I don't know $what")
