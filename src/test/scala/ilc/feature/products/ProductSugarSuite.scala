package ilc
package feature
package products

import org.scalatest.FunSuite

class ProductSugarSuite
extends FunSuite
   with SyntaxSugar
   with Evaluation
   with naturals.Evaluation
   with functions.Evaluation
   with functions.Pretty
{
  val quadruple: Term = tuple(4) ! 1 ! 2 ! 3 ! 4

  test("tuple(n) creates pairs nested toward the right") {
    val desugaredQuadruple: Term = Pair ! 1 ! (Pair ! 2 ! (Pair ! 3 ! 4))
    assert(eval(quadruple) === eval(desugaredQuadruple))
  }

  test("projections extract elements") {
    (1 to 4) foreach { i =>
      assert(eval(project(i) ! quadruple).toNat === i)
    }
  }
}
