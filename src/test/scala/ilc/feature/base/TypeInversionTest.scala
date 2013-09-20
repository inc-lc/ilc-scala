package ilc
package feature
package base

import org.scalatest.FunSuite

class TypeInversionTest
extends FunSuite
   with Syntax
{
  case object Bot extends Type
  case class T1(inner: Type) extends Type
  case class T2(inner: Type) extends Type
  object OpT1 extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("inner") { inner =>
      T1(inner)
    }
  }

  test("can distinguish type constructors of the same arity") {
    val error = intercept[TypeError] {
      OpT1 of T2(Bot)
    }
    info(error.getMessage)
  }
}
