package ilc
package language
package gcc

import feature._

trait Syntax
extends functions.Syntax
   with let.Syntax
   with maybe.Syntax
   with integers.SyntaxSugar
   with sums.SyntaxSugar
   with equality.Syntax
   with products.Syntax
   with booleans.SyntaxSugar
