package ilc
package feature
package naturals

import scala.language.implicitConversions

trait Syntax extends base.Syntax with Types {
  // intro/elim forms of nats
  //
  //   foldNat : r → (r → r) → ℕ → r

  case object FoldNat extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("r") { r =>
      r =>: (r =>: r) =>: NatType =>: r
    }
  }

  case class Nat(n: Int) extends Term {
    require(n >= 0)
    override lazy val getType: Type = NatType
    override def toString = n.toString
  }

  case object PlusNat extends Term {
    override lazy val getType: Type =
      NatType =>: NatType =>: NatType
  }
}

trait ImplicitSyntaxSugar extends Syntax {
  implicit def natToTerm(n: Int): Term = Nat(n)
}
