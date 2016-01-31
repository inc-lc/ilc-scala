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
  val toNope = Inj2.tapply(deltaType(ℤ)) ! Nope.tapply(ℤ)

  val to42 = Inj2.tapply(deltaType(ℤ)) ! (Just ! 42)

  def toJust(i: Int) = Inj1.tapply(MaybeType(ℤ)) ! (Inj2.tapply(groupBasedChangeType(ℤ)) ! i)

  test("updateTerm behaves as expected") {
    assert(eval(ChangeUpdate ! toNope ! (Just ! 5)) ===
      MaybeValue(None))

    assert(eval(ChangeUpdate ! to42 ! Nope.tapply(ℤ)) ===
      MaybeValue(Some(42)))

    assert(eval(ChangeUpdate ! toJust(42) ! (Just ! 5)) ===
      MaybeValue(Some(42)))

    assert(eval(ChangeUpdate ! toJust(42) ! Nope.tapply(ℤ)) ===
      MaybeValue(None))
  }

  test("diffTerm produces replacements") {
    assert(eval(Diff ! Nope.tapply(ℤ) ! (Just ! 5)) ===
      SumValue(Right(MaybeValue(None))))
    assert(eval(Diff ! (Just ! 5) ! Nope.tapply(ℤ)) ===
      SumValue(Right(MaybeValue(Some(5)))))
  }
}
