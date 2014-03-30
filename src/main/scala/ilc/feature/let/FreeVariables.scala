package ilc
package feature
package let

trait FreeVariables extends analysis.FreeVariables with Syntax {
  override def termFreeVariables(term: Term): Set[Var] = term match {
    case Let(v, exp, body) =>
      //If v is free in exp, it is indeed free in the overall let!
      body.freeVariables - v ++ exp.freeVariables
    case _ =>
      super.termFreeVariables(term)
  }
}
