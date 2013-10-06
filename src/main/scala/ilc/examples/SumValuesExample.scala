package ilc
package examples

import feature._

class SumValuesExample
extends Example

   with products.Derivation
   with abelianMaps.AbelianDerivation
   with integers.AbelianDerivation

   with functions.SyntaxSugar
   with integers.SyntaxSugar
   with products.SyntaxSugar

   with abelianMaps.ToScala
   with booleans.ToScala
   with functions.ToScala
   with integers.ToScala
   with products.ToScala
   with sums.ToScala
{
  private val ℤ = IntType

  val program: Term =
    let_x_=(additiveGroupOnIntegers) { _G_+ =>
      lambda(MapType(ℤ, ℤ)) { inputMap =>
        Pair ! (FoldByHom ! _G_+ ! _G_+ ! snd%(ℤ, ℤ) ! inputMap) ! _G_+
      }
    }
}
