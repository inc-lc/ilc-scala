package ilc
package feature
package functions

trait LetRecSyntax extends Syntax {
  case class LetRec(pairs: List[(Var, Term)], bodyName: Name, body: Term) extends Term {
    override lazy val getType = {
      assert ((pairs forall { case (variable, exp) => variable.getType == exp.getType }) || !TypeChecking.value)
      body.getType
    }
  }
}
