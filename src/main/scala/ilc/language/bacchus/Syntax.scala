package ilc
package language
package bacchus

import feature._

trait Syntax
extends functions.Pretty
   with maps.SyntaxSugar
   with maybe.Syntax
   with naturals.Syntax
   with sums.SyntaxSugar
