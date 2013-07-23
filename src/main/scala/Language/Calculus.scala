/**
 * Features useful to all calculi
 *
 * - Specialize the parametric lambda calculus to the calculus's
 *   set of constants
 */

package Language

import language.implicitConversions
import Syntax.Lambda

trait Calculus {

  // SUBCLASS OBLIGATIONS

  type Constant
  def deriveConst(c: Constant): Syntax.Term

  // SUBCLASS RIGHTS

  // specialize the abstract syntax of lambda terms and their
  // visitorsto the current calculus
  object Syntax extends Lambda[Constant]
  import Syntax._
  // Cai 23.07.13 [
  //
  //   We want all names in Syntax to be available to subclasses.
  //   The import above is useless in this regard, because
  //   subclasses do not inherit imported names. I see 2 methods
  //   to deal with the situation.
  //
  //   1. Write `import Syntax._` in the calculus Atlas, and write
  //
  //          import Language.Atlas._
  //          import Language.Atlas.Syntax._
  //
  //      if one wants to bring everything pertaining to Atlas in
  //      scope.
  //
  //   2. Duplicate interface declaration of Syntax here.
  //  
  //   Method 1 seems the lesser evil. Is there another way?
  // ]

  // convenient incremental transformation
  val derive: Term => Term = mkDerive(deriveConst)

  // implicit conversion to stop writing `Const`
  implicit def liftConstant(c: Constant): Term = Const(c)
}
