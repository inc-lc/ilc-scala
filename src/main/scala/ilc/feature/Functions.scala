package ilc
package feature

/**
 * First-class functions.
 */
trait Functions
extends functions.Syntax
   with functions.Pretty
   with functions.Derivation
   with functions.UniqueNames
   with functions.Evaluation
   with functions.Attribution

object Functions extends Functions {
  def deriveConst(constant: Constant) = ???
}
