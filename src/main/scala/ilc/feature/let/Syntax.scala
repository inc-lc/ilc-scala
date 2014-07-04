package ilc
package feature
package let

trait Syntax extends functions.Syntax {
  case class Let(variable: Var, exp: Term, body: Term) extends Term {
    override lazy val getType = {
      //assert (variable.getType == exp.getType)
      body.getType
    }
  }
}
