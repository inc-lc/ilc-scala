package ilc
package feature
package booleans

trait Evaluation extends functions.Evaluation with Syntax {
  case class BooleanValue(toBoolean: Boolean) extends Value {
    override def toString = toBoolean.toString
  }

  override def coreEval(t: Term, env: Env): Value = t match {
    case True =>
      BooleanValue(true)

    case False =>
      BooleanValue(false)

    // call-by-value.
    // sound by purity.
    case IfThenElse(_) =>
      (condition: Value) => (thenBranch: Value) => (elseBranch: Value) =>
        condition match {
          case BooleanValue(satisfied) =>
            if (satisfied) thenBranch else elseBranch
        }

    case _ =>
      super.coreEval(t, env)
  }
}
