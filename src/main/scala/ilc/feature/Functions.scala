package ilc
package feature

/**
 * First-class functions.
 */
trait Functions
extends functions.Syntax
   with functions.Evaluation
   with functions.Derivation
   /* Calculus/feature-independent infrastructure below this line. */
   with functions.Pretty
   with functions.UniqueNames
   with functions.Attribution

object Functions extends Functions {
  def deriveConst(constant: Constant) = ???
  val Value = new FunValues {}
}
