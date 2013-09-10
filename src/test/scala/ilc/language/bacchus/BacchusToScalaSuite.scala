package ilc
package language.bacchus

/**
 * Tests for Bacchus.toScala
 */

import org.scalatest.FunSuite
import ilc.util.EvalScala
import ilc.language.Bacchus._
import ilc.language.bacchus.Subjects._

class BacchusToScalaSuite
extends FunSuite
   with Tools
   with EvalScala
{
  // access point to the Map data structure
  val mapColl = scala.collection.immutable.Map
  // encoding Maybe monad by Either in the domain of Scala objects
  def just[T](x: T): Either[Unit, T] = scala.Right(x)
  val nope: Either[Unit, Nothing] = scala.Left(())

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

  test("UnitTerm, Nat, Abs, App") {
    val code = TypedAbs("_", UnitType, Nat(1997))(UnitTerm)
    assert(run(code) === 1997)
  }

  test("EmptyMap, Update, Lookup, Fold, Plus") {
    assert(run(typed1234) === mapColl(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8))
    assert(run(lookupNN(3)(typed1234)) === just(6))
    assert(run(lookupNN(9)(typed1234)) === nope)
    assert(run(foldNNN(plus3)(2000)(typed1234)) === 2030)
  }

  test("Left, Right, Either, FoldNat") {
    def times(m: Term, n: Term) =
      foldNatN(0)(TypedAbs("y", NatType, Plus("y", m)))(n)
    val decuple = TypedAbs("x", NatType, times("x", 10))
    val icosuple = TypedAbs("x", NatType, times("x", 20))
    assert(run(eitherNNN(decuple)(icosuple)(leftNN(5))) === 50)
    assert(run(eitherNNN(decuple)(icosuple)(rightNN(5))) === 100)
  }
}
