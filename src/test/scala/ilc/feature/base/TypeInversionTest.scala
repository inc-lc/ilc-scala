package ilc
package feature
package base

import org.scalatest.FunSuite

class TypeInversionTest
extends FunSuite
   with functions.Syntax // for application
{
  case object Bot extends Type
  case class T1(inner: Type) extends Type
  case class T2(inner: Type) extends Type
  object OperatorT1 extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("inner") { inner =>
      T1(inner) =>: inner
    }
  }

  object OperandT2 extends Term {
    override def getType: Type = T2(Bot)
  }

  test("can distinguish type constructors of the same arity") {
    val error = intercept[TypeError] {
      val illTypedTerm = (OperatorT1 ! OperandT2).toTerm
    }
    info(error.getMessage)
  }

  case object T3 extends Type { override def toString = "T3" }
  object OperatorT3 extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("r") { r =>
      T3 =>: r =>: r
    }
  }

  test("leaves of an abstract type tree can be non-case objects") {
    OperatorT3 ofType T3 =>: Bot =>: Bot
  }
}
