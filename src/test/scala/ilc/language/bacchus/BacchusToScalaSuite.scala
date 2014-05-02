package ilc
package language.bacchus

/**
 * Tests for Bacchus.toScala
 */

import org.scalatest.FunSuite
import ilc.util.EvalScala

class BacchusToScalaSuite
extends FunSuite
   with ToScala
   with Subjects
   with EvalScala
{
  def run(t: Term): Any = {
    val code = toScala(t)
    try { evalScala(code) } catch {
      // convert stack trace to string
      // http://stackoverflow.com/a/1149712
      case err: Throwable =>
        // err.getMessage()
        val writer = new java.io.StringWriter()
        err.printStackTrace(new java.io.PrintWriter(writer))
        // take first 10 lines of a stacktrace
        // the wasteful computation here should be acceptable
        // unless the thrown object is a StackOverflowError.
        val stacktrace = writer.toString.lines.take(10).mkString("\n")
        val msg = s"When evaluating\n  $code\nwe encountered:\n$stacktrace"
        fail(msg, err)
    }
  }

  test("Nat, Abs, App") {
    val code = lambda(ℕ) { x => 1997 } ! 42
    assert(run(code) === 1997)
  }

  test("EmptyMap, Update, Lookup, Delete, Fold, Plus") {
    assert(run(oldMap) === Map(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8))
    assert(run(Lookup ! 3 ! oldMap) === Some(6))
    assert(run(Lookup ! 9 ! oldMap) === None)
    assert(run(Delete ! 1 ! oldMap) === Map(2 -> 4, 3 -> 6, 4 -> 8))
    assert(run(sum ! oldMap) === 20)
  }

  test("Inj1, Inj2, Either, FoldNat") {
    def times: Term = lambda(ℕ, ℕ) { case Seq(m, n) =>
      FoldNat ! 0 ! lambda(ℕ) { PlusNat ! _ ! m } ! n
    }
    assert(run(Either ! (times ! 10) ! (times ! 20) ! (Inj1(ℕ) ! 4)) === 40)
    assert(run(Either ! (times ! 10) ! (times ! 20) ! (Inj2(ℕ) ! 4)) === 80)
  }
}
