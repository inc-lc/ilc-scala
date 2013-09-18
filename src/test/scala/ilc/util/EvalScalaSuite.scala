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
    assert(evalScala("""ilc.Examples.scalaMeterDummyCode""") ===
      ilc.Examples.scalaMeterDummyCode)
  }

  test("can evaluate Scala lambdas") {
    assert(evalScala("((x: Int) => 3 * x + 1997)(5)") === 3 * 5 + 1997)
  }
}
