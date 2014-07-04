package ilc
package language
package bacchus

import org.scalatest.FunSuite

class BacchusEvaluationSuite
extends FunSuite
   with Subjects
   with Pretty
   with Evaluation
{
  // `expect` is taken and deprecated
  def expectToGet(result: Value)(term: => Term) {
    val prettyTerm = pretty(term)
    test(s"$result = $prettyTerm") {
      assert(eval(term) === result)
    }
  }

  // Plus
  expectToGet(10) {
    PlusNat ! (PlusNat ! 1 ! 2) ! (PlusNat ! 3 ! 4)
  }

  // Plus FoldNat
  expectToGet(1024) { powerOfTwo ! 10 }

  // Plus Inj1 Inj2 Either
  expectToGet(123) {
    def add(n: Int) = lambda(ℕ, ℕ) {
      case Seq(x, y) => PlusNat ! (PlusNat ! x ! y) ! n
    }
    case4(Inj2(ℕ) ! 100, Inj1(ℕ) ! 20,
      add(1), add(2), add(3), add(4))
  }

  // Maybe Nope
  expectToGet(0) { Maybe ! 0 ! lambda(ℕ) {x => x} ! Nope(ℕ) }

  // Maybe Just
  expectToGet(5) { Maybe ! 0 ! lambda(ℕ) {x => x} ! (Just ! 5) }

  // Maybe Nope EmptyMap Lookup
  expectToGet(MaybeValue(None)) { Lookup ! 5 ! EmptyMap(ℕ, ℕ) }

  // Maybe Just EmptyMap Lookup Update
  expectToGet(MaybeValue(Some(5))) {
    Lookup ! 4 !
      (Update ! 3 ! 6 ! (Update ! 4 ! 5 ! EmptyMap(ℕ, ℕ)))
  }

  // Plus EmptyMap Update Fold
  expectToGet(3) {
    getSize !
      (Update ! 3 ! 6 !
        (Update ! 4 ! 5 !
          (Update ! 5 ! 4 ! EmptyMap(ℕ, ℕ))))
  }

  // 500500 = 1 + 2 + ... + 1000
  // (in O(n^2) time, with n = 1000 here)
  expectToGet(500500) {
    sum ! (enumerate ! 1000)
  }
}
