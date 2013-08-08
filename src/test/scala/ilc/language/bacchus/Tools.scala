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
      case (vx: BacchusValue, vy: BacchusValue) =>
        reify(bacchusDiff(vx, vy))
      case _ =>
        Diff(x)(y)
    }

  def bacchusDiff(x: BacchusValue,
                  y: BacchusValue): BacchusValue = (x, y) match {
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
          key => key -> Value.Left(Value.Left(Value.Individualist))
        } ++ toUpdate.map {
          key => key -> Value.Right(bacchusDiff(m1(key), m2(key)))
        })
    }
    case _ => Value.diff(x, y) match {
      case bv: BacchusValue => bv
      case _ => sys.error("the difference between " ++
        "two Bacchus-values is not a Bacchus-value?!")
    }
  }

  def reify(x: BacchusValue): Term = x match {
    case Value.Individualist => Individualist
    case Value.Nat(n) => Nat(n)
    case Value.Map(m) => Map(m.map({case (key, value) =>
      reify(key) -> reify(value)}).toList: _*)
    case Value.Left(x) => Left(reify(x))
    case Value.Right(y) => Right(reify(y))
  }
}
