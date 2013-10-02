package ilc
package feature
package booleans

import scala.language.implicitConversions

trait Syntax extends unit.Syntax with Types with functions.Types {
  case object True  extends Term { def getType = BooleanType }
  case object False extends Term { def getType = BooleanType }

  case object IfThenElse extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("resultType") { r =>
      val rCBN = UnitType =>: r
      BooleanType =>: rCBN =>: rCBN =>: r
    }
  }

  implicit def booleanToTerm(b0: Boolean): Term =
    if (b0) True else False
}
