package ilc
package language

/**
 * The calculus Bacchus in one convenient object
 */

import feature.functions
import bacchus._

object Bacchus
extends Syntax
   with Evaluation
   with Derivation
   with functions.Pretty
