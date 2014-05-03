package ilc
package feature
package products

import org.scalatest.FunSuite

class ProductSugarSuite
extends FunSuite
   with SyntaxSugar
   with Evaluation
   with integers.Evaluation
   with integers.ImplicitSyntaxSugar
   with functions.Evaluation
   with functions.Pretty
{
  val quadruple: Term = tuple(4) ! 1 ! 2 ! 3 ! 4

  test("tuple(n) creates a term of tupleType(n)") {
    assert(quadruple.getType == tupleType(ℤ, ℤ, ℤ, ℤ))
  }

  test("tuple(n) creates pairs nested toward the right") {
    val desugaredQuadruple: Term = Pair ! 1 ! (Pair ! 2 ! (Pair ! 3 ! 4))
    assert(eval(quadruple) === eval(desugaredQuadruple))
  }

  test("projections extract elements") {
    (1 to 4) foreach { i =>
      assert(eval(project(i) ! quadruple).toInt === i)
    }
  }
}
