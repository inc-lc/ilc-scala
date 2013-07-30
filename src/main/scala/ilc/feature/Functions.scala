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

object Functions extends Functions {
  type Constant = Nothing

  def deriveConst(constant : Constant) = ???
}
