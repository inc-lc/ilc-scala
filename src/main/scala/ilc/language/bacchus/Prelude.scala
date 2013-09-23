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
  /** Usage: `(const ! firstArg) % ignoredType`.
    *
    * Examples:
    * {{{
    * // if given a curried argument, `const` needs only 1 type argument
    * val constSucc: Term = (const ! succ) % ℕ
    * }}}
    */
  val const: TermBuilder =
    lambda { x => lambda("ignored") { ignored => x } }

  def succ: Term = Plus ! 1

  // alias
  val ℕ = NatType
}
