/**
 * Tests for Atlas
 */

// Right now, tests for evaluation of lambda terms are also
// included, reason being the evaluation framework is based on
// Syntax.Lambda, which will probably be replaced by a more
// reasonable language descriptor in the branch scala-mixin
// soon, when tests here will have to be ported as well.

import org.scalatest.FunSuite
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

  // USABLE TERM CONSTRUCTORS

  // λf. λx. f x
  val appFun = Abs("f", Abs("x", App(Var(1), Var(0))))

  // λ_. t
  def constFun(t: Term) = Abs("_", t)

  // λx. x
  val idFun = Abs("x", Var(0))

  val negMap1234: Term =
    mapLit(Number, Number, 1 -> -1, 2 -> -2, 3 -> -3, 4 -> -4)

  val negMap1256: Term =
    mapLit(Number, Number, 1 -> -1, 2 -> -2, 5 -> -5, 6 -> -6)

  val oddityMap1234: Term =
    mapLit(Number, Bool, 1 -> True, 2 -> False, 3 -> True, 4 -> False)

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
    val lhs = Array(914, 649, 869, 432, 795, 761)
    val rhs = Array(904, 772, 178, 470, 484, 889)
    (lhs, rhs).zipped.foreach { (x, y) =>
      assert(x + y === eval(App(App(Plus, x), y)))
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
      assert(-i === lookup(i, negMap1234))
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
    def sum(m: Term) = foldNumMap(constFun(Plus), 0, m)
    assert(sum(negMap1234) === -10)
    assert(sum(negMap1256) === -14)
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
    truthTableVal(eval(t), spec)
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
      case 1 => assert(f === spec.head)
      case _ => {
        testTruthTable(f.asInstanceOf[Any => Any](false),
                       spec.slice(0, size / 2))
        testTruthTable(f.asInstanceOf[Any => Any](true),
                       spec.slice(size / 2, size))
      }
    }
  }

  // ASSOC-LIST-BASED TESTING TOOLS FOR MAPS

  def areEqual(t: Term, assoc: (Any, Any)*) {
    areEqualMaps(eval(t), assoc: _*)
  }

  def areEqualMaps(t: Any, assoc: (Any, Any)*) {
    assert(t === map(assoc: _*))
  }

}
