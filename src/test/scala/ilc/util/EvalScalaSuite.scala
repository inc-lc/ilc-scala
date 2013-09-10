package ilc
package util

import org.scalatest.FunSuite
import ilc.util.process.FunProcess

class EvalScalaSuite
extends FunSuite
   with EvalScala
{
  test("can evaluate integer arithmetic") {
    assert(evalScala("(1 + 2) * (3 + 4)") === 21)
  }

  test("can evaluate ilc constructs") {
    assert(evalScala("""ilc.feature.Functions.Var("hello")""") ===
      ilc.feature.Functions.Var("hello"))
  }

  test("can evaluate Scala lambdas") {
    assert(evalScala("((x: Int) => 3 * x + 1997)(5)") === 3 * 5 + 1997)
  }
}
