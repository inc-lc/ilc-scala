package ilc
package feature.naturals

import scala.language.implicitConversions

trait Syntax extends feature.functions.Syntax {
  // intro/elim forms of nats
  //
  //   foldNat : r → (r → r) → Nat → r
  //
  case class Nat(n: Int) extends Constant { require(n >= 0) }
  case object FoldNat extends Constant
  case object Plus extends Constant with NestingBinaryOperator

  // implicit conversions
  implicit def natToConst(n: Int): Constant = Nat(n)
  implicit def natToTerm(n: Int): Term = Const(natToConst(n))
}
