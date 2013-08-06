package ilc
package language.atlas

/**
 * Test tools for Atlas
 */

import collection.immutable
import org.scalatest.FunSuite
import org.scalatest.exceptions.TestFailedException
import ilc.feature.functions.DerivationTools
import ilc.language.Atlas._

trait Tools
extends DerivationTools { self: FunSuite =>
  val calculus = ilc.language.Atlas

  // TRUTH-TABLE-BASED TESTING TOOLS FOR BOOLEANS

  // Usage example:
  //
  //   truthTable(trueTerm,    "1")
  //   truthTable(falseTerm,   "0")
  //   truthTable(andTerm,     "0001")
  //   truthTable(orTerm,      "0111")
  //   truthTable(xorTerm,     "0110")
  //   truthTable(parity4Term, "0101 0101 0101 0101")
  //   truthTable(isPrimeTerm, /* of big-endian binary numbers */
  //                         """0011 0101 0001 0100
  //                            0101 0001 0000 0101""")

  def truthTable(t: Term, spec: String) {
    try {
      truthTableVal(eval(t), spec)
    } catch {
      case err: TestFailedException =>
        val msg =
          "truth table mismatch when testing:\n    " ++
          t.toString ++
          "\n  where " ++ err.getMessage()
        throw new TestFailedException(msg,
                    err.getCause(),
                    err.failedCodeStackDepth)
    }
  }

  def truthTableVal(v: Value, spec: String) {
      val results = (spec.replaceAll("""(?m)\s+""", "") map {
        (x: Char) => if (x == '0') false else true
      }).toList
      testTruthTable(v, results)
  }

  def testTruthTable(f: Value, spec: List[Boolean]) {
    val size = spec.size
    size match {
      case 0 => sys error "wrong truth table specification"
      case 1 => try {
        assert(f.toBool === spec.head)
      } catch {
        case err: TestFailedException =>
          val msg = " didn't map to " ++ (if (spec.head) "1" else "0")
          throw new TestFailedException(msg, err,
                      err.failedCodeStackDepth)
      }
      case _ => {
        try {
          testTruthTable(f(false),
                         spec.slice(0, size / 2))
        } catch {
          case err: TestFailedException =>
            throw nextErr(err, false, err.getMessage())
        }
        try {
          testTruthTable(f(true),
                         spec.slice(size / 2, size))
        } catch {
          case err: TestFailedException =>
            throw nextErr(err, true, err.getMessage())
        }
      }
    }
  }

  def nextErr(err: TestFailedException,
              lastArg: Boolean,
              initMsg: String): TestFailedException = {
    val msg = (if (lastArg) "1" else "0") ++ initMsg
    val cause = err.getCause()
    val stackDepth = err.failedCodeStackDepth
    new TestFailedException(msg, cause, stackDepth)
  }

  // ASSOC-LIST-BASED TESTING TOOLS FOR MAPS

  def assertMap(t: Term, assoc: (Value, Value)*) {
    assertMapVal(eval(t).toMap, assoc: _*)
  }

  def assertMapVal(t: ValueMap, assoc: (Value, Value)*) {
    assert(t === immutable.Map(assoc: _*))
  }
}
