/**
 * Tests for Atlas
 */

// Right now, tests for evaluation of lambda terms are also
// included, reason being the evaluation framework is based on
// Syntax.Lambda, which will probably be replaced by a more
// reasonable language descriptor in the branch scala-mixin
// soon, when tests here will have to be ported as well.

import org.scalatest.FunSuite
import org.scalatest.exceptions.TestFailedException
import Language.Atlas._
import Evaluation.Atlas._
 
class AtlasTest extends FunSuite {

  // SHORTHANDS

  private type map[K, V] = collection.immutable.Map[K, V]
  private val map = collection.immutable.Map
  private val emptyMap = map.empty[Any, Any]

  def lookup(key: Term, m: Term) =
    eval(App(App(Lookup(Number, Number), key), m))

  def foldNumMap(f: Term, z: Term, m: Term): Any =
    eval(fold(Number, Number, Number, f, z, m))

  def diff(tau: Type, s: Term, t: Term): Term =
    App(App(diffTerm(tau), s), t)

  def apply(tau: Type, dt: Term, t: Term): Term =
    App(App(applyTerm(tau), dt), t)

  // USABLE TERM CONSTRUCTORS

  // λf. λx. f x
  val appFun = Abs("f", Abs("x", App(Var(1), Var(0))))

  // λ_. t
  def constFun(t: Term) = Abs("_", weaken(_ + 1, t))

  // λx. x
  val idFun = Abs("x", Var(0))

  def sum(t: Term): Term =
    fold(Number, Number, Number, constFun(Plus), 0, t)

  val negMap1234: Term =
    mapLit(Number, Number, 1 -> -1, 2 -> -2, 3 -> -3, 4 -> -4)

  val negMap1256: Term =
    mapLit(Number, Number, 1 -> -1, 2 -> -2, 5 -> -5, 6 -> -6)

  val oddityMap1234: Term =
    mapLit(Number, Bool, 1 -> True, 2 -> False, 3 -> True, 4 -> False)

  val primeMap10: Term =
    mapLit(Number, Bool,
      2 -> True,  3 -> True, 4 -> False, 5 -> True,
      6 -> False, 7 -> True, 8 -> False, 9 -> False, 10 -> False)

  // TEST CANDIDATES

  val typesAndTerms = List.apply[(Type, Term)](
    Bool -> True,
    Bool -> False,
    Bool -> App(App(Lookup(Number, Bool), 2), oddityMap1234),
    Number -> 1984,
    Number -> sum(negMap1256),
    Map(Number, Number) -> negMap1234,
    Map(Number, Bool) -> primeMap10,
    Map(Map(Number, Number), Map(Number, Bool)) ->
      mapLit(Map(Number, Number), Map(Number, Bool),
             negMap1234 -> oddityMap1234,
             negMap1256 -> primeMap10)
  )

  val intsAndInts = {
    val lhs = Array(914, 649, 869, 432, 795, 761)
    val rhs = Array(904, 772, 178, 470, 484, 889)
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
    def app(t: Term) = App(appFun, t)
    truthTable(app(constFun(False)), "00")
    truthTable(app(constFun(True)), "11")
    truthTable(app(idFun), "01")
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
      assert(eval(App(App(Plus, p._1), p._2)) === p._1 + p._2)
    }
  }

  test("Empty is empty map") {
    // areEqual(t: Term, assoc: (Any, Any)*)
    // when no other argument is given,
    // areEqual(t) tests if t evaluates to the empty map.
    areEqual(Empty(Bool, Bool))
    // test that type parameters to Emtpy don't influence anything
    areEqual(Empty(Map(Bool, Map(Bool, Bool)),
                   Map(Map(Bool, Bool), Bool)))
  }

  test("neutral elements are false, 0, emptyMap") {
    assert(neutral(Bool) === false)
    assert(neutral(Number) === 0)
    assert(neutral(Map(Number, Bool)) === emptyMap)
    assert(neutral(Map(Map(Bool, Map(Bool, Bool)),
                       Map(Map(Bool, Bool), Bool))) === emptyMap)
  }

  test("updating with nonexistent key is insertion") {
    areEqual(negMap1234, 1 -> -1, 2 -> -2, 3 -> -3, 4 -> -4)
  }

  test("updating with neutral element is deletion") {
    areEqual(updatesFrom(Number, Number, negMap1234,
                         1 -> 0, 3 -> 0, 5 -> 0),
             2 -> -2, 4 -> -4)
    areEqual(updatesFrom(Number, Bool, oddityMap1234, 1 -> False),
             3 -> true)
  }

  test("updating with existing key changes the value") {
    areEqual(updatesFrom(Number, Number, negMap1234, 2 -> 99, 3 -> 75),
             1 -> -1, 2 -> 99, 3 -> 75, 4 -> -4)
  }

  test("looking up an existing key returns the associated value") {
    Range.inclusive(1, 4) foreach { i =>
      assert(lookup(i, negMap1234) === -i)
    }
  }

  test("looking up a nonexistent key returns the neutral element") {
    Range.inclusive(5, 9) foreach { i =>
      assert(lookup(i, negMap1234) === neutral(Number))
    }
  }

  test("zipping two maps traverses the union of their keys") {
    areEqual(zip(Number, Number, Number,
                 constFun(Plus), negMap1234, negMap1256),
             1 -> (- 1 - 1), 2 -> (- 2 - 2),
             3 -> -3, 4 -> -4, 5 -> -5, 6 -> -6)
  }

  test("folding with Plus over a map yields the sum of its values") {
    assert(eval(sum(negMap1234)) === -10)
    assert(eval(sum(negMap1256)) === -14)
  }

  // TESTING DIFF, APPLY, NIL

  test("nil-terms denote nil changes") {
    def applyNil(tau: Type, t: Term): Any =
      eval(apply(tau, nilTerm(tau), t))

    typesAndTerms.foreach { p =>
      assert(applyNil(p._1, p._2) === eval(p._2))
    }
  }

  test("difference with self is the nil change") {
    typesAndTerms.foreach { p =>
      assert(eval(diff(p._1, p._2, p._2)) === eval(nilTerm(p._1)))
    }
  }

  test("[s ⊕ (t ⊝ s) == t] holds for Booleans, numbers and maps") {
    def applyDiff(tau: Type, s: Term, t: Term): Any =
      eval(apply(tau, diff(tau, t, s), s))
    List.apply[(Type, Term, Term)](
      (Bool, False, False),
      (Bool, False, True ),
      (Bool, True , False),
      (Bool, True , True ),
      (Number, 392, -1522),
      (Number, sum(negMap1234), sum(negMap1256)),
      (Map(Number, Number), negMap1234, negMap1256),
      (Map(Number, Bool), primeMap10, oddityMap1234),
      (Map(Map(Number, Number), Map(Number, Bool)),
        mapLit(Map(Number, Number), Map(Number, Bool),
               negMap1234 -> oddityMap1234,
               negMap1256 -> primeMap10,
               mapLit(Number, Number, 99 -> 217) ->
                 mapLit(Number, Bool, 2012 -> True)),
        mapLit(Map(Number, Number), Map(Number, Bool),
               negMap1234 -> primeMap10,
               negMap1256 -> primeMap10,
               Empty(Number, Number) -> primeMap10))
    ).foreach { trio =>
      assert(applyDiff(trio._1, trio._2, trio._3) === eval(trio._3))
    }
  }

  // TESTING DERIVATIVES OF CONSTANTS

  test("the derivative of base-type constants is the nil change") {
    assert(eval(derive(True)) === eval(nilTerm(Bool)))
    assert(eval(derive(False)) === eval(nilTerm(Bool)))
    assert(eval(derive(Num(12341))) === eval(nilTerm(Number)))
    assert(eval(derive(Empty(Bool, Number))) ===
           eval(nilTerm(Map(Bool, Number))))
  }


  test("the derivatives of Xor and Plus are versions of themselves") {

    //                   x = 0000 0000 1111 1111
    //                  dx = 0000 1111 0000 1111
    //                   y = 0011 0011 0011 0011
    //                  dy = 0101 0101 0101 0101
    //                xNew = 0000 1111 1111 0000
    //                yNew = 0110 0110 0110 0110
    //             x xor y = 0011 0011 1100 1100
    //       xNew xor yNew = 0110 1001 1001 0110
    //              change = 0101 1010 0101 1010
    truthTable(derive(Xor), "0101 1010 0101 1010")

    (intsAndInts, intsAndInts.reverse).zipped.foreach { (p1, p2) =>
      val ((x, xNew), (yNew, y)) = (p1, p2)
      val dx = diff(Number, xNew, x)
      val dy = diff(Number, yNew, y)
      val result = apply(Number,
                     App(App(App(App(derive(Plus), x), dx), y), dy),
                     App(App(Plus, x), y))
      assert(eval(result) === xNew + yNew)
    }
  }

  // TESTING DERIVATIVES OF ABSTRACTION AND APPLICATION

  test("the derivative of identity is snd") {
    truthTable(derive(idFun), "0101")
  }

  test("the derivative of the constant function does not react") {
    truthTable(App(Abs("y", constFun(Var(0))), False), "00")
  }

  test("the derivative of [λf. λx. f x] computes [Δf x Δx]") {
    truthTable(derive(App(appFun, App(Xor, True))), "0101")
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

  def truthTableVal(v: Any, spec: String) {
      val results = (spec.replaceAll("""(?m)\s+""", "") map {
        (x: Char) => if (x == '0') false else true
      }).toList
      testTruthTable(v, results)
  }

  def testTruthTable(f: Any, spec: List[Boolean]) {
    val size = spec.size
    size match {
      case 0 => sys error "wrong truth table specification"
      case 1 => try {
        assert(f === spec.head)
      } catch {
        case err: TestFailedException =>
          val msg = " didn't map to " ++ (if (spec.head) "1" else "0")
          throw new TestFailedException(msg, err,
                      err.failedCodeStackDepth)
      }
      case _ => {
        try {
          testTruthTable(f.asInstanceOf[Any => Any](false),
                         spec.slice(0, size / 2))
        } catch {
          case err: TestFailedException =>
            throw nextErr(err, false, err.getMessage())
        }
        try {
          testTruthTable(f.asInstanceOf[Any => Any](true),
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

  def areEqual(t: Term, assoc: (Any, Any)*) {
    areEqualMaps(eval(t), assoc: _*)
  }

  def areEqualMaps(t: Any, assoc: (Any, Any)*) {
    assert(t === map(assoc: _*))
  }

}