package ilc
package feature
package products

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import ilc.util.EvalScala

class ProductToScalaSuite
extends FunSuite
   with ShouldMatchers
   with ToScala
   with naturals.ToScala
   with functions.ToScala
   with EvalScala
   with SyntaxSugar
{
  def run(t: Term): Any = evalScala(toScala(t))

  val quintuple: Term = tuple(5) ! 1 ! 2 ! 3 ! 4 ! 5

  test("can construct tuples") {
    run(quintuple) should be((1, (2, (3, (4, 5)))))
  }

  test("can deconstruct tuples") {
    (1 to 5) foreach { i =>
      run(project(i) ! quintuple) should be(i)
    }
  }
}
