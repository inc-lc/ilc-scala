package ilc
package language
package gcc

import feature._

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

trait Syntax
extends functions.Syntax
   with let.Syntax
   with maybe.Syntax
   with integers.ImplicitSyntaxSugar
   with sums.SyntaxSugar
   with equality.Syntax
   with products.Syntax
   with booleans.SyntaxSugar
   with LetRecSyntax
