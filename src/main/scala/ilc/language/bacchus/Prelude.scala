package ilc
package language
package bacchus

import feature._

/**
  * This trait includes generally useful functions for the Bacchus object
  * language. When Bacchus abstraction is insufficient, we resort to meta-level
  * abstraction (see for instance const).
  *
  * Many function are inspired (to some extent) from Haskell ones.
  */
trait Prelude extends bacchus.Syntax
{
  def succ: Term = PlusInt ! LiteralInt(1)
}
