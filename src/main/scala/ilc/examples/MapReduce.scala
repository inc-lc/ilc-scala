package ilc
package examples

import feature._

trait MapReduce
extends abelianMaps.AbelianDerivation
   with abelianMaps.ToScala

   with bags.SyntaxSugar
   with bags.AbelianDerivation
   with bags.ToScala

   with booleans.ToScala
   with products.ToScala
   with sums.ToScala
