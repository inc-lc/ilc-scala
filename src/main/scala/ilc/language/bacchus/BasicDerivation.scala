package ilc
package language
package bacchus

import feature._

trait BasicDerivation
extends functions.Derivation
   with bags.Derivation
   with maps.ReplacementValuesDerivation
   with maybe.ReplacementValuesDerivation
   with naturals.ReplacementValuesDerivation
   with sums.ReplacementValuesDerivation
   with abelianGroups.Derivation
   with products.Derivation
   with booleans.Derivation
   with integers.AbelianDerivation
