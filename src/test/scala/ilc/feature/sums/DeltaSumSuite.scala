package ilc
package feature
package sums

import org.scalatest.FunSuite

class DeltaSumSuite
extends FunSuite
   with ReplacementValuesDerivation
   with Evaluation
   with integers.AbelianDerivation
   with integers.Evaluation
   with integers.ImplicitSyntaxSugar
   with base.Pretty
{
  implicit class SumTypeOps(sigma: Type) {
    def ⊎ (tau: Type): Type =
      SumType(sigma, tau)
  }

  //XXX abstract more
  val oldSum = Inj1(ℤ) ! 5
  val swapped = Inj2(ℤ) ! 7
  val operated = Inj1(ℤ) ! 9

  val replace = Inj2(deltaType(ℤ) ⊎ deltaType(ℤ))

  val replacement = replace ! swapped
  val surgery = Inj1(ℤ ⊎ ℤ) ! (Inj1(deltaType(ℤ)) ! (replacementChange ! 9))

  test("updateTerm behaves as expected") {
    assert(eval(ChangeUpdate ! replacement ! oldSum) === eval(swapped))
    assert(eval(ChangeUpdate ! surgery ! oldSum) === eval(operated))
  }

  test("diffTerm produces replacement") {
    assert(eval(Diff ! swapped ! oldSum) === eval(replace ! swapped))
    assert(eval(Diff ! operated ! oldSum) === eval(replace ! operated))
  }
}
