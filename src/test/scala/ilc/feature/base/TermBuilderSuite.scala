package ilc
package feature
package base

import org.scalatest.FunSuite

class TermBuilderSuite
extends FunSuite
   with base.Syntax
   with functions.Syntax // for term application operator !
{
  case object Bot extends Type { override def toString = "⊥" }
  case object Top extends Type { override def toString = "⊤" }
  case object TT extends Term { def getType: Type = Top }

  test("SpecializedTerm detects mismatched argument type") {
    val typeError = intercept[TypeError] {
      SpecializedTerm(Var("x", Top =>: Top)) specialize Bot
    }
    info(typeError.getMessage)
  }

  // id2 : ∀T. (T → T) → (T → T)
  object id2 extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("T") { T =>
      (T =>: T) =>: (T =>: T)
    }
  }

  test("Polymorphic constants detect mismatched argument type") {
    val typeError = intercept[TypeError] {
      val t: Term = id2 ! TT
    }
    info(typeError.getMessage)
  }
}
