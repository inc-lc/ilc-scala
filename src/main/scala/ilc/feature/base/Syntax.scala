package ilc
package feature.base

import scala.language.implicitConversions

trait Syntax {
  ////////////////////////////////
  // Subclass obligations start //
  ////////////////////////////////
  // These traits can be extended by subclasses.
  trait Term

  trait Constant

  //////////////////////////////
  // Subclass obligations end //
  //////////////////////////////

  case class Const(c: Constant) extends Term

  // implicit conversion to stop writing `Const`
  implicit def liftConstant(c: Constant): Term = Const(c)
}
