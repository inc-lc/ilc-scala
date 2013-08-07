package ilc
package language.bacchus

/**
 * Tests for Bacchus
 */

import org.scalatest.FunSuite
import ilc.language.Bacchus._
import ilc.language.bacchus.Subjects._

// TODO: think about how to reuse tests across calculi

class BacchusSuite
extends FunSuite
   with Tools {

  // this test is copied from Atlas
  // with subjects in Bacchus
  // TODO: remove code duplication
  test("pair encoding preserves information") {
    val termList: List[(Term, Term)] =
      List((0, Individualist),
           (Lookup(2)(twiceMap1234), primeMap10),
           (twiceMap1234, Lookup(4)(primeMap10)),
           (twiceMap1256, Empty))
    val fst = "x" ->: "y" ->: "x"
    val snd = "x" ->: "y" ->: "y"
    def mkProj(proj: Term): (Term, Term) => Value =
      (s, t) => eval(uncurry(proj, pair(s, t)))
    val proj1 = mkProj(fst)
    val proj2 = mkProj(snd)
    termList.foreach {
      terms => {
        val (t1, t2) = terms
        assert(proj1(t1, t2) === eval(t1))
        assert(proj2(t1, t2) === eval(t2))
      }
    }
  }

  test("adding two numbers yields their sum") {
    natsAndNats.foreach { p =>
      assert(eval(Plus(p._1, p._2)).toNat === p._1 + p._2)
    }
  }

  test("Empty is empty map") {
    assertMap(Empty, Nil: _*)
  }

  test("updating with nonexistent key is insertion") {
    assertMap(twiceMap1234, 1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)
  }

  test("updating with existing key changes the value") {
    assertMap(updatesFrom(twiceMap1234, 2 -> 99, 3 -> 75),
              1 -> 2, 2 -> 99, 3 -> 75, 4 -> 8)
  }

  test("looking up an existing key returns the associated value") {
    Range.inclusive(1, 4) foreach { i =>
      assert(eval(Lookup(i)(twiceMap1234)).toNat === 2 * i)
    }
  }

  test("folding with Plus over a map yields the sum of its values") {
    assert(eval(sum(twiceMap1234)).toNat === 20)
    assert(eval(sum(twiceMap1256)).toNat === 28)
  }

  test("[s ⊕ (t ⊝ s) == t] holds for sums, numbers and maps") {
    def applyDiff(s: Term, t: Term): Value = eval(Apply(Diff(t)(s))(s))
    List.apply[(Term, Term)](
      (ff, ff),
      (ff, tt),
      (tt, ff),
      (tt, tt),
      (392, 1522),
      (sum(twiceMap1234), sum(twiceMap1256)),
      (twiceMap1234, twiceMap1256),
      (primeMap10, oddityMap1234),
      (Map(twiceMap1234 -> oddityMap1234,
           twiceMap1256 -> primeMap10,
           Map(99 -> 217) -> Map(2012 -> tt)),
       Map(twiceMap1234 -> primeMap10,
           twiceMap1256 -> primeMap10,
           Empty -> primeMap10))
    ).foreach { duo =>
      assert(applyDiff(duo._1, duo._2) === eval(duo._2))
    }
  }

  test("pattern matching behaves as expected") {
    assert(eval(case2(Left(5), idFun, constFun(6))).toNat === 5)
    assert(eval(case2(Right(6), constFun(5), idFun)).toNat === 6)
    val (tt, ff) = (Left(0), Right(1))
    List(ff -> ff, ff -> tt, tt -> ff, tt -> tt) foreach {
      case (bool1, bool2) => {
        val code =
          ifThenElse(bool1)(
            ifThenElse(bool2)(0)(1))(
            ifThenElse(bool2)(2)(3))
        def const2(x: Term) = {
          val y = uniqueName(x, "_")
          Lambda(y, y) ->: x
        }
        assert(eval(case4(bool1, bool2,
          const2(0), const2(1), const2(2), const2(3))) ===
          eval(code))
        assert(eval(case4(bool1, bool2, fst, fst, fst, fst)) ===
               eval(case2(bool1, idFun, idFun)))
        assert(eval(case4(bool1, bool2, snd, snd, snd, snd)) ===
               eval(case2(bool2, idFun, idFun)))
      }
    }
  }

  test("foldNat is a for-loop") {
    assert(eval(FoldNat(0)(Const(Plus)(10))(100)).toNat === 1000)
  }

  test("the derivative of Plus is correct") {
    (natsAndNats, natsAndNats.reverse).zipped.foreach { (p1, p2) =>
      val ((x, xNew), (yNew, y)) = (p1, p2)
      assertCorrect(Plus, List((x, xNew), (y, yNew)))
    }
  }

  // For derivatives, you want to test beyond replacement pairs.
  // have something that computes fine-grained change on
  // values and have something else that converts it back to a
  // term.
}
