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

  val userMap = lambda(ℤ) { ignoredDocumentID =>
    FoldGroup ! FreeAbelianGroup(ProductType(ℤ, ℤ)) !
      lambda(ℤ) { number => Singleton ! (Pair ! number ! LiteralInt(1)) }
  }

  val userReduce = lambda(ℤ) { ignoredKey =>
    FoldGroup ! additiveGroupOnIntegers ! lambda(ℤ) { x => x }
  }

  val program: Term =
    (mapReduce !
      FreeAbelianGroup(ℤ) !
      additiveGroupOnIntegers !
      userMap !
      userReduce) ofType
    MapType(ℤ, BagType(ℤ)) =>: MapType(ℤ, ℤ)
}
