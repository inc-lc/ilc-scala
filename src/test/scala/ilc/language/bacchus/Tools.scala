package ilc
package language.bacchus

/**
 * Test tools for Bacchus
 */

import collection.immutable
import org.scalatest.FunSuite
import ilc.feature.functions.DerivationTools
import ilc.language.Bacchus

trait Tools
extends DerivationTools { self: FunSuite =>
  val calculus = Bacchus
  import calculus._

  // ASSOC-LIST-BASED TESTING TOOLS FOR MAPS
  //
  // code duplication with ilc.language.atlas.Tools.
  // TODO: come up with suitable abstraction
  // on both the syntactic and the semantic domains
  // for re-use across calculi, then see if these
  // testing tools can be unified.

  def assertMap(t: Term, assoc: (Value, Value)*) {
    assertMapVal(eval(t).toMap, assoc: _*)
  }

  def assertMapVal(t: ValueMap, assoc: (Value, Value)*) {
    assert(t === immutable.Map(assoc: _*))
  }

  // TEST OPTIMIZED DERIVATION
  def assertCorrectPlus(f: Term, input: List[(Term, Term)]) {
    assertCorrect(f, input, defaultDiff, derivePlus)
    assertCorrect(f, input, fineGrainedDiff, derivePlus)
  }

  // DERIVATION TEST WITH FINE-GRAINED CHANGES
  def assertCorrectTwice(f: Term, input: List[(Term, Term)]) {
    assertCorrect(f, input)
    assertCorrect(f, input, fineGrainedDiff)
  }

  def fineGrainedDiff(x: Term, y: Term): Term =
    (eval(x), eval(y)) match {
      case (_: Value.Function, _) =>
        Diff(x)(y)
      case (_, _: Value.Function) =>
        Diff(x)(y)
      case (vx, vy) =>
        reify(bacchusDiff(vx, vy))
    }

  def bacchusDiff(x: Value,
                  y: Value): Value = (x, y) match {
    case (Value.Left(lx), Value.Left(ly)) =>
      Value.Left(Value.Left(bacchusDiff(lx, ly)))
    case (Value.Right(lx), Value.Right(ly)) =>
      Value.Right(Value.Right(bacchusDiff(lx, ly)))
    case (Value.Map(m1), Value.Map(m2)) => {
      val toDelete = m2.keySet -- m1.keySet
      val toInsert = m1 -- m2.keySet
      val toUpdate = (m1.keySet & m2.keySet) filter {
        key => m1(key) != m2(key)
      }
      Value.Left(
        toInsert.map {
          case (key, value) => key -> Value.Left(Value.Right(value))
        } ++ toDelete.map {
          key => key -> Value.Left(Value.Left(Value.UnitValue))
        } ++ toUpdate.map {
          key => key -> Value.Right(bacchusDiff(m1(key), m2(key)))
        })
    }
    case (Value.Function(f1), Value.Function(f2)) =>
      // XXX The right code should be this, but it's untested, so let's just
      // fail instead.
      //
      // This is not commented out so that it gets maintained,
      // since it's most probably the code we want anyway.
      Value.Function(x => Value.Function(dx => bacchusDiff(f1(x)(dx), f2(x)(dx))))
      // Fail instead:
      ???
    case _ => Value.diff(x, y)
  }

  def reify(x: Value): Term = x match {
    case Value.UnitValue => UnitTerm
    case Value.Nat(n) => Nat(n)
    case Value.Map(m) => Map(m.map({case (key, value) =>
      reify(key) -> reify(value)}).toList: _*)
    case Value.Left(x) => Left(reify(x))
    case Value.Right(y) => Right(reify(y))

    case Value.Function(f) =>
      ???
      // XXX We're using no environment here, and we can't.
      //Lambda("x") ->: reify(f(eval("x")))
  }
}
