package ilc
package feature
package booleans

import scala.language.implicitConversions

trait Syntax extends unit.Syntax with Types with functions.Types {
  case object True  extends Term { lazy val getType = BooleanType }
  case object False extends Term { lazy val getType = BooleanType }

  case object IfThenElse extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("resultType") { r =>
      val rCBN = UnitType =>: r
      BooleanType =>: rCBN =>: rCBN =>: r
    }
  }

  implicit def booleanToTerm(b0: Boolean): Term =
    if (b0) True else False
}

trait SyntaxSugar extends Syntax with functions.Syntax {
  def ifThenElse(condition : TermBuilder,
                 thenBranch: TermBuilder,
                 elseBranch: TermBuilder): TermBuilder =
    IfThenElse ! condition !
      mkIfThenElseBranch(thenBranch) !
      mkIfThenElseBranch(elseBranch)

  def mkIfThenElseBranch(t: TermBuilder): TermBuilder =
    lambda(Var("unit", UnitType)) { unit => t }

  def andTerm: Term =
    lambda(Var("cond1", BooleanType), Var("cond2", BooleanType)) {
      case Seq(cond1, cond2) =>
        ifThenElse(cond1, cond2, False)
    }
}
