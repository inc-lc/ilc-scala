package ilc
package language
package gcc

import org.scalatest.FunSuite
import org.scalatest.Matchers
import GCC._

class GCCPrimitivesSuite
extends FunSuite
   with Matchers
   with Evaluation
{

  test("Booleans") {

    typecheck {
      lam('a % bool, 'b % bool) {
        ('a or 'b) and 'a
      }
    }



  }
}
