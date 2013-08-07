package ilc
package language

/**
 * The calculus Atlas in one convenient object
 */

import feature.functions
import atlas._

object Atlas
extends Syntax
   with Evaluation
   with Derivation
   with functions.Pretty
