package ilc
package feature

/**
 * First-class functions.
 */
trait Functions
extends functions.TypedSyntax
   with functions.Typing
   with functions.Evaluation
   with functions.Derivation
   with functions.ToScala
   /* Calculus/feature-independent infrastructure below this line. */
   with functions.Pretty
   with functions.UniqueNames
   with functions.Attribution
   with functions.Stability

object Functions extends Functions {
  def deriveConst(constant: Constant) = ???
  val Value = new FunValues {}
}
