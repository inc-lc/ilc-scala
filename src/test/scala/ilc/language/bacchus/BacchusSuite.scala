package ilc
package language.bacchus

/**
 * Tests for Bacchus
 */

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

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
      List((0, UnitTerm),
           (Lookup(2)(twiceMap1234), primeMap10),
           (twiceMap1234, Lookup(4)(primeMap10)),
           (twiceMap1256, EmptyMap))
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

  test("EmptyMap is empty map") {
    assertMap(EmptyMap, Nil: _*)
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
      assert(eval(Lookup(i)(twiceMap1234)) === Value.Just(2 * i))
    }
  }

  test("looking up a nonexistent key returns Nothing") {
    Range.inclusive(5, 8) foreach { i =>
      assert(eval(Lookup(i)(twiceMap1234)) === Value.Nothing())
    }
  }

  test("folding with Plus over a map yields the sum of its values") {
    assert(eval(sum(twiceMap1234)).toNat === 2 + 4 + 6 + 8)
    assert(eval(sum(twiceMap1256)).toNat === 200 + 4 + 10 + 12)
  }

  test("diff and apply works on functions") {
    val f = Const(Plus)(25)
    val g = Const(Plus)(100)
    assert(eval(Apply(Diff(g)(f))(f)(Nat(20))).toNat === 120)
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
           EmptyMap -> primeMap10))
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

  test("the derivative of constants are nil changes of themselves") {
    def assertNil(t: Term): Unit = {
      assert(eval(Apply(derive(t))(t)) === eval(t))
      assert(eval(Apply(derivePlus(t))(t)) === eval(t))
    }
    List.apply[Term](
      UnitTerm, Nat(5), EmptyMap, Left(EmptyMap), Right(UnitTerm)
    ).foreach(assertNil)
  }

  test("the derivative of Plus is correct") {
    (natsAndNats, natsAndNats.reverse).zipped.foreach { (p1, p2) =>
      val ((x, xNew), (yNew, y)) = (p1, p2)
      val args: List[(Term, Term)] = List((x, xNew), (y, yNew))
      assertCorrectTwice(Plus, args)
      assertCorrectPlus(Plus, args)
    }
  }

  test("the derivative of FoldNat is correct") {
    val args2: List[(Term, Term)] = List(
      5 -> 1997,
      Const(Plus)(25) -> Const(Plus)(100))
    val args3: List[(Term, Term)] = args2 ++
      List.apply[(Term, Term)](40 -> 5)

    assertCorrectTwice(FoldNat, args3)
    // optimized foldNat should not be triggered,
    // because the number of iteration changes
    assertCorrectPlus(FoldNat, args3)

    val foldNatWithFixedIterations: Term =
      Lambda("z", "f") ->: FoldNat("z")("f")(25)

    assertCorrectTwice(foldNatWithFixedIterations, args2)
    assertCorrectPlus(foldNatWithFixedIterations, args2)
  }

  test("the derivative of Fold is correct") {
    // constant f z
    assertCorrectTwice(Fold(constFun(Plus))(0),
      List(twiceMap1234 -> twiceMap1256))
    // constant f
    assertCorrectTwice(Fold(constFun(Plus)),
      List(0 -> 100, twiceMap1234 -> twiceMap1256))
    // changing everything
    assertCorrectTwice(Fold,
      List(constFun(Plus) ->
             (Lambda("_", "x", "y") ->: Plus(2000, "x", "y")),
           0 -> 100,
           twiceMap1234 -> twiceMap1256))
  }
}

// XXX: This test is for functionality of base, and only needs functions and
// integers for the actual terms, but doesn't belong to tests for either
// feature.functions or feature.integers. Where should I put this?

class EvalSuite extends FunSuite with ShouldMatchers {
  val t1 = Nat(1)(idFun)
  val subt2 = Nat(1)(idFun)
  val t2 = (Lambda("x") ->: subt2)(1)

  def evalGivesDynamicTypeError(t: Term): UnexpectedTypeException =
    //Note: Scalatest's 'should produce' also returns the exception!
    evaluating { eval(t) } should produce [UnexpectedTypeException]

  test("Type errors cause exceptions") {
    evalGivesDynamicTypeError(t1)
  }

  def getErrorInfo(ute: UnexpectedTypeException): OuterTypeExceptionInfo =
    // it would be nice to have a more compact way to write that, say:
    // evaluating {ute.data} should have type[OuterTypeExceptionInfo]
    ute.data match {
      case otei: OuterTypeExceptionInfo => otei
      case _   : InnerTypeExceptionInfo => fail()
    }

  test("Exceptions for type errors report the offending subterm") {
    val thrown1 = getErrorInfo(evalGivesDynamicTypeError(t1))
    thrown1.term should be (Some(t1))
    thrown1.subterm should be (t1)
    val thrown2 = getErrorInfo(evalGivesDynamicTypeError(t2))
    thrown2.term should be (Some(t2))
    thrown2.subterm should be (subt2)
  }
}
