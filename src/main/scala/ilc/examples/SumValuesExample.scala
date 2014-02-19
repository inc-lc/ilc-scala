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

  // snd : ℤ → ℤ → ℤ
  // snd = λx : ℤ. λy : ℤ. y
  //
  // (In Scala code, we provide the argument type
  // annotations by the % operator. Using the % operator
  // to save notation overhead can cause subtle type errors
  // and is not recommended.)
  //
  // program : Map ℤ ℤ → (ℤ, AbelianGroup ℤ)
  // program =
  //   let G+ = additiveGroupOnIntegers : AbelianGroup ℤ
  //    in λ inputMap : Map ℤ ℤ.
  //         pair (foldByHom G+ G+ snd inputMap) G+
  val program: Term =
    let_x_=(additiveGroupOnIntegers) { _G_+ =>
      lambda(MapType(ℤ, ℤ)) { inputMap =>
        Pair ! (FoldByHom ! _G_+ ! _G_+ ! snd%(ℤ, ℤ) ! inputMap) ! _G_+
      }
    }
}
