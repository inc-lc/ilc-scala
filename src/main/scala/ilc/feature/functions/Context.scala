package ilc
package feature
package functions

trait Context extends base.Context with Syntax {
  override def getChildren(term: Term): Seq[Subtree] =
    term match {
      case Abs(variable, body) =>
        Seq(Subtree(body, AbsBodyContext(Top, variable)))

      case App(operator, operand) =>
        Seq(Subtree(operator, AppOperatorContext(Top, operand)),
            Subtree(operand, AppOperandContext(Top, operator)))

      case _ =>
        super.getChildren(term)
    }

  case class AbsBodyContext(parent: Context, variable: Var)
  extends Context
  {
    // boilerplate clone of this.copy()
    def updateParent(newParent: Context): Context =
      this.copy(newParent, variable)

    def instantiate(body: Term): Term = Abs(variable, body)
  }

  case class AppOperatorContext(parent: Context, operand: Term)
  extends Context
  {
    def updateParent(newParent: Context): Context =
      this.copy(newParent, operand)

    def instantiate(operator: Term): Term = App(operator, operand)
  }

  case class AppOperandContext(parent: Context, operator: Term)
  extends Context
  {
    def updateParent(newParent: Context): Context =
      this.copy(newParent, operator)

    def instantiate(operand: Term): Term = App(operator, operand)

    override def holePosition: Int = 1
  }
}
