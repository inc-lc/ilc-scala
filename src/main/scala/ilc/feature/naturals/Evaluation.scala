package ilc
package feature.naturals

import scala.language.implicitConversions

trait Evaluation extends feature.base.Evaluation {
  trait NatValues {
    case class Nat(toNat: Int) extends Value {
      require(toNat >= 0)
    }
  }
  val Value: NatValues

  implicit def liftNat(n: Int): Value = Value.Nat(n)
  implicit class NatOps(value: Value) {
    def toNat: Int =
      value match {
        case Value.Nat(n) => n
        case _ => die("toNat")
      }
  }
}
