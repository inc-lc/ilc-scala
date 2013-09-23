package ilc
package examples

import language.bacchus

// Example 3 variant: mapping over the values of a map
// program = map succ
// input = Map(1 -> 1, 2 -> 2, 3 -> 3)
// program input = Map(1 -> 2, 2 -> 3, 3 -> 4)
//
// The names of examples can be used elsewhere to access
// a particular example. Rename wisely.
class MapSuccBaseExample
extends Example
   with bacchus.Syntax // for syntactic sugars
   with bacchus.Prelude
   with bacchus.ToScala
   with bacchus.BasicDerivation
{
  def program: Term = mapWithKey ! constSucc

  // constSucc = λ k (x: ℕ) → x + 1
  def constSucc: Term = (const ! succ) % ℕ
}
