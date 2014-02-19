package ilc
package examples

import feature._

class HistogramExample
extends Example
   with MapReduce
   with integers.SyntaxSugar
   with integers.AbelianDerivation
   with integers.ToScala
{
  val ℤ = IntType

  // userMap : ℤ → Bag ℤ → Bag (ℤ × ℤ)
  // userMap = λ ignoredDocumentID : ℤ.
  //   foldGroup freeAbelianGroup
  //     (λ number : ℤ. singleton (pair number 1))
  val userMap = lambda(ℤ) { ignoredDocumentID =>
    FoldGroup ! FreeAbelianGroup(ProductType(ℤ, ℤ)) !
      lambda(ℤ) { number => Singleton ! (Pair ! number ! LiteralInt(1)) }
  }

  // userReduce : ℤ → Bag ℤ → ℤ
  // userReduce = λ ignoredKey : ℤ.
  //   foldGroup additiveGroupOnIntegers (λx : ℤ. x)
  val userReduce = lambda(ℤ) { ignoredKey =>
    FoldGroup ! additiveGroupOnIntegers ! lambda(ℤ) { x => x }
  }

  // program : Map ℤ (Bag ℤ) → Map ℤ ℤ
  // program =
  //   mapReduce
  //     freeAbelianGroup
  //     additiveGroupOnIntegers
  //     userMap
  //     userReduce
  val program: Term =
    (mapReduce !
      FreeAbelianGroup(ℤ) !
      additiveGroupOnIntegers !
      userMap !
      userReduce) ofType
    MapType(ℤ, BagType(ℤ)) =>: MapType(ℤ, ℤ)
}
