package ilc
package feature
package booleans

import org.scalatest.FunSuite
import ilc.util.EvalScala

trait BooleanSuiteBase
extends FunSuite
   with EvalScala
   with SyntaxSugar
   with Evaluation
   with Derivation
   with ToScala
   with functions.Derivation
   with functions.Evaluation
   with functions.ToScala
   with base.Pretty
{
  val booleans = Seq(False, True)

  def expectToGet(b: Boolean)(t: => Term): Unit

  test("True, False, IfThenElse works as intended") {
    expectToGet(true )(True )
    expectToGet(false)(False)
    expectToGet(true )(ifThenElse(True , True, False))
    expectToGet(false)(ifThenElse(False, True, False))
  }

  test("b₀ ⊕ (b₁ ⊝ b₀) == b₁") {
    for {
      b0 <- booleans
      b1 <- booleans
    } expectToGet(eval(b1).toBoolean) {
      ChangeUpdate ! (Diff ! b1 ! b0) ! b0
    }
  }

  test("Derivatives are correct (takes minutes to run all 256 cases for BooleanSuiteCompile)") {
    val t: Term = lambda(BooleanType, BooleanType, BooleanType) {
      case Seq(condition, thenBranch, elseBranch) =>
        ifThenElse(condition, thenBranch, elseBranch)
    }
    for {
      old0  <- booleans
      new0  <- booleans
      old1b <- booleans
      new1b <- booleans
      old2b <- booleans
      new2b <- booleans
    } {
      val curriedIf = IfThenElse.tapply(BooleanType)
      val Seq(old1, new1, old2, new2) = Seq(old1b, new1b, old2b, new2b) map {
        // written in a pointful style to trigger implicit conversion
        // from Term to TermBuilder
        term => mkIfThenElseBranch(term)
      }
      val (del1, del2) = (Diff ! new1 ! old1, Diff ! new2 ! old2)
      val newResult =
        if (eval(new0).toBoolean)
          eval(new1b).toBoolean
        else
          eval(new2b).toBoolean
      expectToGet(newResult) {
        ChangeUpdate !
          (derive(curriedIf) ! old0 ! new0 ! old1 ! del1 ! old2 ! del2) !
          (curriedIf ! old0 ! old1 ! old2)
      }
      expectToGet(newResult) {
        ChangeUpdate !
          (derive(curriedIf ! new0) ! old1 ! del1 ! old2 ! del2) !
          (curriedIf ! new0 ! old1 ! old2)
      }
      expectToGet(newResult) {
        ChangeUpdate !
          (derive(curriedIf ! new0 ! new1) ! old2 ! del2) !
          (curriedIf ! new0 ! new1 ! old2)
      }
      expectToGet(newResult) {
        ChangeUpdate !
          (derive(curriedIf ! new0 ! new1 ! new2)) !
          (curriedIf ! new0 ! new1 ! new2)
      }
    }
  }
}

class BooleanSuiteInterp extends BooleanSuiteBase {
  def expectToGet(b: Boolean)(t: => Term) {
    assert(eval(t) === BooleanValue(b))
  }
}
