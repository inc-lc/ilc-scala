package ilc
package feature
package maybe

import org.scalatest.FunSuite

class DeltaMaybeSuite
extends FunSuite
   with ReplacementValuesDerivation
   with Evaluation
   with naturals.ReplacementValuesDerivation
   with naturals.Evaluation
   with sums.Evaluation
{

  val toNope = Inj2(ℕ) ! Nope(ℕ)

  val to42 = Inj2(ℕ) ! (Just ! 42)

  def toJust(i: Int) = Inj1(MaybeType(ℕ)) ! i

  test("updateTerm behaves as expected") {
    assert(eval(ChangeUpdate ! toNope ! (Just ! 5)) ===
      MaybeValue(None))

    assert(eval(ChangeUpdate ! to42 ! Nope(ℕ)) ===
      MaybeValue(Some(42)))

    assert(eval(ChangeUpdate ! toJust(42) ! (Just ! 5)) ===
      MaybeValue(Some(42)))

    assert(eval(ChangeUpdate ! toJust(42) ! Nope(ℕ)) ===
      MaybeValue(None))
  }

  test("diffTerm produces replacements") {
    assert(eval(Diff ! Nope(ℕ) ! (Just ! 5)) ===
      SumValue(Right(MaybeValue(None))))
    assert(eval(Diff ! (Just ! 5) ! Nope(ℕ)) ===
      SumValue(Right(MaybeValue(Some(5)))))
  }
}
