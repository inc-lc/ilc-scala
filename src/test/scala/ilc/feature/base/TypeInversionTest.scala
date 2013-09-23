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
}
