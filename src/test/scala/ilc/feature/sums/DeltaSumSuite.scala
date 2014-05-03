package ilc
package feature
package sums

import org.scalatest.FunSuite

class DeltaSumSuite
extends FunSuite
   with ReplacementValuesDerivation
   with Evaluation
   with naturals.ReplacementValuesDerivation
   with naturals.Evaluation
   with naturals.ImplicitSyntaxSugar
{
  implicit class SumTypeOps(sigma: Type) {
    def ⊎ (tau: Type): Type =
      SumType(sigma, tau)
  }

  val oldSum = Inj1(ℕ) ! 5
  val swapped = Inj2(ℕ) ! 7
  val operated = Inj1(ℕ) ! 9

  val replace = Inj2(ℕ ⊎ ℕ)

  val replacement = replace ! swapped
  val surgery = Inj1(ℕ ⊎ ℕ) ! (Inj1(ℕ) ! 9)

  test("updateTerm behaves as expected") {
    assert(eval(ChangeUpdate ! replacement ! oldSum) === eval(swapped))
    assert(eval(ChangeUpdate ! surgery ! oldSum) === eval(operated))
  }

  test("diffTerm produces replacement") {
    assert(eval(Diff ! swapped ! oldSum) === eval(replace ! swapped))
    assert(eval(Diff ! operated ! oldSum) === eval(replace ! operated))
  }
}
