package ilc
package util

import org.scalatest.FunSuite
import ilc.util.process.FunProcess

class EvalScalaSuite
extends FunSuite
   with EvalScala
{
  test("can evaluate ilc constructs") {
    assert(evalScala("""ilc.Examples.scalaMeterDummyCode""") ===
      ilc.Examples.scalaMeterDummyCode)
  }
}
