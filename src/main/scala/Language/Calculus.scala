/**
 * Features useful to all calculi
 *
 * - Specialize the parametric lambda calculus to the calculus's
 *   set of constants
 */

package Language

import language.implicitConversions
import Syntax.Lambda

trait Calculus extends Syntax.Lambda {

  // SUBCLASS OBLIGATIONS

  type Constant
  def deriveConst(c: Constant): Term

  // SUBCLASS RIGHTS

  // convenient incremental transformation
  val derive: Term => Term = mkDerive(deriveConst)

  // implicit conversion to stop writing `Const`
  implicit def liftConstant(c: Constant): Term = Const(c)
}
