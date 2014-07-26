package ilc
package language
package gcc

import feature._
import scala.language.implicitConversions

trait LetRecSyntax extends functions.Syntax {
  case class LetRec(variable: Var, exp: Term, body: Term) extends Term {
    override lazy val getType = {
      assert (variable.getType == exp.getType || !TypeChecking.value)
      body.getType
    }
  }

  case class LetRecStar(pairs: List[(Var, Term)], bodyName: Name, body: Term) extends Term {
    override lazy val getType = {
      assert ((pairs forall { case (variable, exp) => variable.getType == exp.getType }) || !TypeChecking.value)
      body.getType
    }
  }
}

trait GCCIntSyntax
extends base.Syntax
   with integers.Types
   with functions.Types {
  case class LiteralInt(i: Int) extends Term {
    override lazy val getType: Type = IntType
  }

  class IntOp extends Term {
    override lazy val getType: Type = IntType =>: IntType =>: IntType
  }

  case object Plus extends IntOp
  case object Minus extends IntOp
  case object Mult extends IntOp
  case object Div extends IntOp

  implicit def intToTerm(n: Int): Term = LiteralInt(n)
}

trait Syntax
extends functions.Syntax
   with let.Syntax
   with maybe.Syntax
   with GCCIntSyntax
   with sums.SyntaxSugar
   with equality.Syntax
   with products.Syntax
   with booleans.SyntaxSugar
   with LetRecSyntax
