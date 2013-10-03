package ilc
package examples

import feature._

class MapSuccExample
extends Example
   with bags.SyntaxSugar

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
  def program: Term = map ! (PlusInt ! LiteralInt(1))
}
