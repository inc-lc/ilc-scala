package ilc
package language
package bacchus

import feature._

trait Subjects
extends bacchus.Syntax with bacchus.Prelude with ChangingTerms
   with naturals.ImplicitSyntaxSugar
{
  // shorthand for types
  implicit class MapTypeOps(sigma: Type) {
    def ↦ (tau: Type): Type = MapType(sigma, tau)
  }

  val powerOfTwo = FoldNat ! 1 ! lambda(ℕ) {x => PlusNat ! x ! x}

  val getSize: Term =
    Fold ! (lambda(ℕ, ℕ) { case Seq(_, _) => PlusNat ! 1 }) ! 0

  val enumerate: Term = lambda(ℕ) { n =>
    FoldNat ! EmptyMap.tapply(ℕ, ℕ) !
      lambda(ℕ ↦ ℕ) { theMap =>
        lambda {m => Update ! m ! m ! theMap} ! (getSize ! theMap)
      } ! (PlusNat ! n ! 1)
  }

  val sum: Term =
    Fold ! lambda(ℕ, ℕ) { case Seq(k, v) => PlusNat ! v } ! 0

  val natPairs = {
    val lhs = Array(914, 649, 869, 432, 795, 761, 1, 3, 5, 7, 0)
    val rhs = Array(904, 772, 178, 470, 484, 889, 2, 4, 6, 7, 0)
      (lhs, rhs).zipped.toList
  }

  // various possibilities for keys in a changing map
  val oldMap = mapLiteral(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
  val newMap = mapLiteral(1 -> 200, 2 -> 4, 5 -> 10, 6 -> 12)
  val keyInOld = Nat(4)
  val keyInNew = Nat(5)
  val keyInBoth = Nat(1)
  val keyInNone = Nat(9)
  val keyCases = List(keyInOld, keyInNew, keyInBoth, keyInNone)
}
