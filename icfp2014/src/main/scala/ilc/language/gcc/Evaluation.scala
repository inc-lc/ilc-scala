package ilc
package language
package gcc

import feature._

trait Evaluation
extends functions.Evaluation
   with let.Evaluation
   with maybe.Evaluation
   with integers.Evaluation
   with sums.Evaluation
   with products.Evaluation
   with booleans.Evaluation
