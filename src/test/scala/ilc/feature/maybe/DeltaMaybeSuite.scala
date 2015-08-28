package ilc
package feature
package maybe

import org.scalatest.FunSuite

class DeltaMaybeSuite
extends FunSuite
   with ReplacementValuesDerivation
   with Evaluation
   with integers.AbelianDerivation
   with integers.Evaluation
   with integers.ImplicitSyntaxSugar
   with sums.Evaluation
   with base.Pretty
{
  //XXX abstract more?
  val toNope = Inj2(deltaType(ℤ)) ! Nope(ℤ)

  val to42 = Inj2(deltaType(ℤ)) ! (Just ! 42)

  def toJust(i: Int) = Inj1(MaybeType(ℤ)) ! (Inj2(groupBasedChangeType(ℤ)) ! i)

  test("updateTerm behaves as expected") {
    assert(eval(ChangeUpdate ! toNope ! (Just ! 5)) ===
      MaybeValue(None))

    assert(eval(ChangeUpdate ! to42 ! Nope(ℤ)) ===
      MaybeValue(Some(42)))

    assert(eval(ChangeUpdate ! toJust(42) ! (Just ! 5)) ===
      MaybeValue(Some(42)))

    assert(eval(ChangeUpdate ! toJust(42) ! Nope(ℤ)) ===
      MaybeValue(None))
  }

  test("diffTerm produces replacements") {
    assert(eval(Diff ! Nope(ℤ) ! (Just ! 5)) ===
      SumValue(Right(MaybeValue(None))))
    assert(eval(Diff ! (Just ! 5) ! Nope(ℤ)) ===
      SumValue(Right(MaybeValue(Some(5)))))
  }
}
