package ilc
package language
package bacchus

import feature._

trait Syntax
extends functions.Pretty
   with bags.SyntaxSugar
   with maps.SyntaxSugar
   with maybe.Syntax
   with naturals.Syntax
   with sums.SyntaxSugar
   with equality.Syntax
   with abelianGroups.Syntax
   with products.Syntax
   with booleans.SyntaxSugar
