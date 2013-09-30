package ilc
package feature
package integers

import org.scalatest.FunSuite
import ilc.util.EvalScala

class IntegerSuite
extends FunSuite
   with AbelianDerivation
   with Evaluation
   with functions.Evaluation
   with products.Evaluation
   with sums.Evaluation
   with ToScala
   with functions.ToScala
   with products.ToScala
   with sums.ToScala
   with EvalScala
   with functions.Pretty
{
  private val ℤ = IntType

  def expectToGet(i: Int)(t: => Term) {
    assert(eval(t) === IntValue(i))
    try { assert(evalScala(toScala(t)) === i) }
    catch { case e: Throwable =>
      info(e.getMessage)
      info(pretty(t))
      info(toScala(t))
      fail
    }
  }

  test("2 + 2 =  4") { expectToGet( 4) { Plus ! 2 ! 2 } }
  test("2 - 4 = -2") { expectToGet(-2) { Minus ! 2 ! 4} }

  test("i ⊕ (j ⊝ i) = j") {
    val (i, j) = (4, 2)
    expectToGet(j) { ChangeUpdate ! (Diff ! j ! i) ! i }
  }

  test("-4 ⊕ (+ 20) = 16") {
    val plus20: Term = Inj1(ℤ) !
      (Pair ! (abelian ! Plus ! (Minus ! 0) ! 0) ! 20)
    assert(plus20.getType === deltaType(IntType))
    expectToGet(16) { ChangeUpdate ! plus20 ! -4 }
  }
}
