package ilc
package examples

import feature._

class SumValuesExample
extends Example

   with products.Derivation
   with abelianMaps.AbelianDerivation
   with integers.AbelianDerivation

   with functions.Syntax
   with integers.SyntaxSugar
   with products.StdLib

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
  val G: UntypedTerm = additiveGroupOnIntegers
  val foldByHom: UntypedTerm = FoldByHom
  // Need to annotate the first argument. It is not used, so type inference leaves it open, but code generation does not cope with type variables.
  val second: UntypedTerm = 'first % ℤ ->: 'second ->: 'second

  val program: Term = untypedTermToTerm(
    'inputMap ->: pair(foldByHom(G, G, second, 'inputMap), G)
  )
}
