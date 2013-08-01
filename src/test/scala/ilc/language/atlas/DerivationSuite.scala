package ilc
package language.atlas

/**
 * Tests for Atlas
 */

import collection.immutable
import org.scalatest.FunSuite
import org.scalatest.exceptions.TestFailedException
import ilc.language.Atlas._

class DerivationSuite extends FunSuite {

  // USABLE TERM CONSTRUCTORS

  // λf. λx. f x
  val appFun = "f" ->: "x" ->: Var("f")("x")

  // λ_. t
  def constFun(t: Term) = uniqueName(t, "_") ->: t

  // λx. x
  val idFun = "x" ->: "x"

  def sum(t: Term): Term =
    Fold(constFun(Plus))(0)(t)

  val negMap1234: Term = Map(1 -> -1, 2 -> -2, 3 -> -3, 4 -> -4)

  val negMap1256: Term = Map(1 -> -1, 2 -> -2, 5 -> -5, 6 -> -6)

  val idMap2367: Term = Map(2 -> 2, 3 -> 3, 6 -> 6, 7 -> 7)

  val map1368: Term = Map(1 -> 10, 3 -> 3, 6 -> 60, 8 -> 80)

  val oddityMap1234: Term =
    Map(1 -> True, 2 -> False, 3 -> True, 4 -> False)

  val primeMap10: Term =
    Map(2 -> True, 3 -> True, 4 -> False,  5 -> True, 6 -> False,
        7 -> True, 8 -> False, 9 -> False, 10 -> False)

  val nilTerm = Empty

  // TEST CANDIDATES

  val terms = List.apply[Term](
    True,
    False,
    Lookup(2)(oddityMap1234),
    1984,
    sum(negMap1256),
    negMap1234,
    primeMap10,
    Map(negMap1234 -> oddityMap1234, negMap1256 -> primeMap10))

  val intsAndInts = {
    val lhs = Array(914, 649, 869, 432, 795, 761, 1, 3, 5)
    val rhs = Array(904, 772, 178, 470, 484, 889, 2, 4, 6)
    (lhs, rhs).zipped.toList
  }

  // TESTING ABSTRACTION AND APPLICATION

  test("the constant function is λ_. t") {
    truthTable(constFun(False), "00")
    truthTable(constFun(True ), "11")
  }

  test("the identity function is " ++ idFun.toString) {
    truthTable(idFun, "01")
  }

  test("Haskell's ($) is " ++ appFun.toString) {
    truthTable(appFun(constFun(False)), "00")
    truthTable(appFun(constFun(True)), "11")
    truthTable(appFun(idFun), "01")
  }

  // TESTING ENCODINGS

  test("pair encoding preserves information") {
    val termList: List[(Term, Term)] =
      List((0, False),
           (Lookup(2)(oddityMap1234), primeMap10),
           (negMap1234, Lookup(4)(primeMap10)),
           (negMap1256, primeMap10))
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

  test("zip4 traverses the union of all maps") {
    val plus4 = Lambda("k", "a", "b", "c", "d") ->:
      Plus("a", "b", "c", "d")
    def t = zip4(plus4,
                 Map(1 -> 2), Map(3 -> 4), Map(5 -> 6), Map(7 -> 8))
    assertMap(t, 1 -> 2, 3 -> 4, 5 -> 6, 7 -> 8)
  }

  // TESTING CONSTANTS

  test("False is false") {
    truthTable(False, "0")
  }

  test("True is true") {
    truthTable(True, "1")
  }

  test("Xor is exclusive-OR") {
    truthTable(Xor, "0110")
  }

  test("adding two numbers yields their sum") {
    intsAndInts.foreach { p =>
      assert(eval(Plus(p._1, p._2)).toInt === p._1 + p._2)
    }
  }

  test("Empty is empty map") {
    assertMap(Empty, Nil: _*)
  }

  test("neutral elements are false, 0, emptyMap") {
    assert(Value.Neutral.toBool === false)
    assert(Value.Neutral.toInt === 0)
    assert(Value.Neutral.toMap === ValueMap())
  }

  test("updating with nonexistent key is insertion") {
    assertMap(negMap1234, 1 -> -1, 2 -> -2, 3 -> -3, 4 -> -4)
  }

  test("updating with neutral element is deletion") {
    assertMap(updatesFrom(negMap1234, 1 -> 0, 3 -> 0, 5 -> 0),
              2 -> -2, 4 -> -4)
    assertMap(updatesFrom(oddityMap1234, 1 -> False),
              3 -> true)
  }

  test("updating with existing key changes the value") {
    assertMap(updatesFrom(negMap1234, 2 -> 99, 3 -> 75),
              1 -> -1, 2 -> 99, 3 -> 75, 4 -> -4)
  }

  test("looking up an existing key returns the associated value") {
    Range.inclusive(1, 4) foreach { i =>
      assert(eval(Lookup(i)(negMap1234)).toInt === -i)
    }
  }

  test("looking up a nonexistent key returns the neutral element") {
    Range.inclusive(5, 9) foreach { i =>
      assert(eval(Lookup(i)(negMap1234)) === Value.Neutral)
    }
  }

  test("zipping two maps traverses the union of their keys") {
    assertMap(Zip(constFun(Plus))(negMap1234)(negMap1256),
              1 -> (- 1 - 1), 2 -> (- 2 - 2),
              3 -> -3, 4 -> -4, 5 -> -5, 6 -> -6)
  }

  test("folding with Plus over a map yields the sum of its values") {
    assert(eval(sum(negMap1234)).toInt === -10)
    assert(eval(sum(negMap1256)).toInt === -14)
  }

  // TESTING DIFF, APPLY, NIL

  test("nil-terms denote nil changes") {
    def applyNil(t: Term): Value = eval(Apply(nilTerm)(t))

    terms.foreach { t =>
      assert(applyNil(t) === eval(t))
    }
  }

  test("difference with self is the nil change") {
    terms.foreach { t =>
      assert(eval(Diff(t)(t)) === Value.Neutral)
    }
  }

  test("[s ⊕ (t ⊝ s) == t] holds for Booleans, numbers and maps") {
    def applyDiff(s: Term, t: Term): Value = eval(Apply(Diff(t)(s))(s))
    List.apply[(Term, Term)](
      (False, False),
      (False, True ),
      (True , False),
      (True , True ),
      (392, -1522),
      (sum(negMap1234), sum(negMap1256)),
      (negMap1234, negMap1256),
      (primeMap10, oddityMap1234),
      (Map(negMap1234 -> oddityMap1234,
           negMap1256 -> primeMap10,
           Map(99 -> 217) -> Map(2012 -> True)),
       Map(negMap1234 -> primeMap10,
           negMap1256 -> primeMap10,
           Empty -> primeMap10))
    ).foreach { duo =>
      assert(applyDiff(duo._1, duo._2) === eval(duo._2))
    }
  }

  // TESTING DERIVATIVES OF CONSTANTS

  test("the derivative of base-type constants is the nil change") {
    assert(eval(derive(True)) === Value.Neutral)
    assert(eval(derive(False)) === Value.Neutral)
    assert(eval(derive(Num(12341))) === Value.Neutral)
    assert(eval(derive(Empty)) === Value.Neutral)
  }


  test("the derivatives of Xor and Plus are correct") {

    def isOdd(i: Int): Term = if (i % 2 == 1) True else False
    Range.inclusive(0, 16).foreach { i =>
      assertCorrect(Xor,
        (isOdd(i / 8), isOdd(i / 4)) ::
        (isOdd(i / 2), isOdd(i)) :: Nil)
    }

    (intsAndInts, intsAndInts.reverse).zipped.foreach { (p1, p2) =>
      val ((x, xNew), (yNew, y)) = (p1, p2)
      assertCorrect(Plus, List((x, xNew), (y, yNew)))
    }
  }

  test("the derivative of Negate is correct") {
    intsAndInts.foreach { p =>
      assertCorrect(Negate, List(p))
    }
  }

  test("the derivative of Update is correct") {
    (intsAndInts, intsAndInts.reverse).zipped.foreach { (p, q) =>
      assertCorrect(Update,
        List(p, // old/new keys
             q, // old/new values
             (negMap1234, negMap1256)))
    }
  }

  test("the derivative of Lookup is correct") {
    // A key can be in 4 states:
    // 1. in both the old and the new maps,
    // 2. outside the old map and in the new map,
    // 3. in the old map and outside the new map,
    // 4. outside both the old and the new maps.
    // This test tests all combinations of the states
    // of the old key and of the new key.
    (List(1,1,1,1,3,3,3,3,5,5,5,5,7,7,7,7),
     List(1,3,5,7,1,3,5,7,1,3,5,7,1,3,5,7)).zipped.foreach {
      (i, j) =>
        assertCorrect(Lookup,
          List((i, j), // old/new keys
               (negMap1256, negMap1234)))
    }
  }

  test("the derivative of Zip is correct") {
    // λx. zip (λ_. λy. λz. x + y + z)
    val t = "x" ->:
      Zip(Lambda("_", "y", "z") ->: Plus("x", "y", "z"))
    assertCorrect(t,
      List((100, 1000),
           (negMap1234, negMap1256),
           (idMap2367, map1368)))
    // this is a good test case: result change contains all of
    // insertion, deletion and update.
  }

  // TESTING DERIVATIVES OF ABSTRACTION AND APPLICATION

  test("the derivative of identity is snd") {
    truthTable(derive(idFun), "0101")
  }

  test("the derivative of the constant function does not react") {
    truthTable(("y" ->: constFun("y"))(False), "00")
  }

  test("the derivative of [λf. λx. f x] computes [Δf x Δx]") {
    truthTable(derive(appFun(Xor(True))), "0101")
  }

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

  // TESTING TOOLS FOR DERIVATIVES

  // assert the correctness of the derivative of t
  // according to
  //
  //     f x ⊕ derive(f) x (y ⊝ x) == f y
  //
  def assertCorrect(f: Term,
                    input: List[(Term, Term)]) {
    val reversedInput = input.reverse
    val theOld    = assembleOld(f, reversedInput)
    val theNew    = assembleNew(f, reversedInput)
    val theChange = assembleChange(derive(f), reversedInput)
    try {
      assert(eval(Apply(theChange)(theOld)) === eval(theNew))
    } catch { case err: TestFailedException =>
      val msg = "wrong derivative:" ++
        "\n     old = " ++ eval(theOld).toString ++
        "\n     new = " ++ eval(theNew).toString ++
        "\n  change = " ++ eval(theChange).toString ++
        "\n  result " ++ err.getMessage() ++
        "\n   old-t = " ++ theOld.toString ++
        "\n   new-t = " ++ theNew.toString
      throw new TestFailedException(msg, err, err.failedCodeStackDepth)
    }
  }

  def assembleChange(dt: Term,
                     reversedInput: List[(Term, Term)]): Term =
    reversedInput match {
      case Nil => dt
      case (z, zNew) :: theRest => {
        val dz = Diff(zNew)(z)
        assembleChange(dt, theRest)(z)(dz)
      }
    }

  val assembleOld = assembleAccordingTo(_._1)
  val assembleNew = assembleAccordingTo(_._2)

  def assembleAccordingTo(chooser: ((Term, Term)) => Term):
        (Term, List[(Term, Term)]) => Term = {
    def assemble(t: Term,
                 reversedInput: List[(Term, Term)]): Term =
      reversedInput match {
        case Nil => t
        case duo :: theRest =>
          assemble(t, theRest)(chooser(duo))
      }
    assemble
  }

}
