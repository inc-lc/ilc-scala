package ilc
package feature
package let

trait Evaluation extends Syntax with functions.Evaluation {
  override def coreEval(t: Term, env: Env): Value =
    t match {
      case Let(x, exp, body) =>
        wrapEval(body, env.updated(x.getName, wrapEval(exp, env)))
      case _ =>
        super.coreEval(t, env)
    }
}