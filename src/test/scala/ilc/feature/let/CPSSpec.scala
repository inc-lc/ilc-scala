package ilc
package feature
package let

import org.scalatest._

class CPSSpec extends FlatSpec with Instantiations {
  val bacchusSystem = buildBacchusWithLetSystem(true, true, true)
  import bacchusSystem._

  "cps" should "work" in {
    val tst1 = asTerm('f ->: 'x ->: 'f('x))
    println(pretty(tst1))
    println(tst1.getType)
    println(toCPST(tst1.getType))
    println(cbvTypeToCPS(tst1.getType))
    println(pretty(toCPS(tst1)))
  }
}