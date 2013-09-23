package ilc
package language
package bacchus

/**
  * This trait includes generally useful functions for the Bacchus object
  * language. When Bacchus abstraction is insufficient, we resort to meta-level
  * abstraction (see for instance const).
  *
  * Many function are inspired (to some extent) from Haskell ones.
  */
trait Prelude extends bacchus.Syntax
{
  //Same as in Haskell's
  def const(t: Type)(k: Term) = lambda(t) (ignored => k)
  def succ: Term = Plus ! 1

  // alias
  val ℕ = NatType
}
