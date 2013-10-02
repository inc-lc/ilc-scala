package ilc
package feature
package integers

import org.scalatest.FunSuite
import ilc.util.EvalGenerated
import ilc.language.bacchus
import scala.language.implicitConversions

class IntegerSuite
extends FunSuite
   with AbelianDerivation
   with Evaluation
   with ToScala
   with bacchus.Evaluation
   with bacchus.BasicDerivation
   with bacchus.ToScala
   with EvalGenerated
   with functions.Pretty
{
  private val ℤ = IntType
  implicit def intToTerm(i: Int): Term = LiteralInt(i)

  def expectToGet(i: Int)(t: => Term) {
    assert(eval(t) === IntValue(i))
    try { assert(evalGenerated(t) === i) }
    catch { case e: Throwable =>
      info(e.getMessage)
      info(pretty(t))
      info(toScala(t))
      fail
    }
  }

  test("2 + 2 =  4") { expectToGet( 4) { PlusInt ! 2 ! 2 } }
  test("2 - 4 = -2") { expectToGet(-2) { PlusInt ! 2 ! (NegateInt ! 4) } }

  test("i ⊕ (j ⊝ i) = j") {
    val (i, j) = (4, 2)
    expectToGet(j) { ChangeUpdate ! (Diff ! j ! i) ! i }
  }

  test("-4 ⊕ (+ 20) = 16") {
    val plus20: Term = Inj1(ℤ) !
      (Pair ! (AbelianGroup ! PlusInt ! NegateInt ! 0) ! 20)
    assert(plus20.getType === deltaType(IntType))
    expectToGet(16) { ChangeUpdate ! plus20 ! -4 }
  }
}
