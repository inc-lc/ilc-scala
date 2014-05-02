package ilc
package examples

import feature._

// program = uncurry union
// input   = (Bag(1, 2, ..., 1997), Bag(-1, -2, ..., -1997)
// output  = Bag(-1997, -1996, ..., 1997)
class BagUnionExample
extends Example
   with functions.Pretty
   with inference.PrettySyntax

   with bags.StdLib
   with products.StdLib

   // context-free derivations
   with products.Derivation

   // context-sensitive derivations
   with bags.AbelianDerivation
   with integers.AbelianDerivation

   // code generation
   with bags.ToScala
   with booleans.ToScala
   with functions.ToScala
   with integers.ToScala
   with products.ToScala
   with sums.ToScala
{
  val inputType = ProductType(BagType(IntType), BagType(IntType))

  //   λ pair : inputType.
  //      union (proj₁ pair) (proj₂ pair)
  def untypedProgram: UntypedTerm =
    'pair % inputType ->: union(proj1('pair), proj2('pair))

  def program: Term = untypedTermToTerm(untypedProgram)

}
