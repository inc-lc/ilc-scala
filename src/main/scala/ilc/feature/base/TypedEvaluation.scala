package ilc
package feature
package base

trait TypedEvaluation extends TypedSyntax with Evaluation {
  override def coreEval(t: Term, env: Env): Value = t match {
    case TypedConst(c, cType) =>
      evalConst(c)

    case _ =>
      super.coreEval(t, env)
  }
}
