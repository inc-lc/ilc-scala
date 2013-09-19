package ilc
package language
package bacchus

import org.scalatest.FunSuite
import ilc.feature._

class BacchusReificationSuite
extends FunSuite
   with Syntax // for mapLiteral
   with Subjects // for type shorthands
   with Evaluation
   with naturals.Reification
   with maps.Reification
   with maybe.Reification
   with sums.Reification
{
  test("can reify natural numbers") {
    val n: Value = 42
    assert(eval(reify(n, ℕ)) === n)
  }

  test("can reify maps") {
    val valueType = (ℕ ↦ ℕ) ↦ (ℕ ↦ ℕ)
    val t = mapLiteral(
      EmptyMap(ℕ, ℕ) -> mapLiteral(0 -> 100),
      mapLiteral(1 -> 2, 3 -> 4) -> mapLiteral(5 -> 6, 7 -> 8),
      mapLiteral(20 -> 30) -> EmptyMap(ℕ, ℕ))
    val value = eval(t)
    assert(eval(reify(value, valueType)) === value)
  }
}
