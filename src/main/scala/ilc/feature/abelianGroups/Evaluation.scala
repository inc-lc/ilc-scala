package ilc
package feature
package abelianGroups

/** hacked evaluation to pass tests */

trait Evaluation
extends products.Evaluation
   with functions.Evaluation
   with booleans.Evaluation
   with Syntax
   with products.SyntaxSugar
   with integers.Syntax
{
  var index = 0

  override def coreEval(t: Term, env: Env): Value = t match {
    case AbelianGroup(_)
       | GetBinOp(_) | GetInv(_) | GetNeutral(_)  =>
      coreEval(transform(t), env)

    case AreEqualGroups(e) =>
      (g1: Value) => (g2: Value) =>
        coreEval(g1.toPair._1 == g2.toPair._1, env)

    case _ =>
      super.coreEval(t, env)
  }

  private def transform(t: Term): Term = {
    // annotate a term with the argument types of `t`
    def fixType(x: TermBuilder): Term =
      x % (getArgumentTypes(t.getType).map(transformType): _*)
    t match {
      case AbelianGroup(e) => {
        index += 1
        fixType(tuple(4) ! LiteralInt(index))
      }

      case GetBinOp(_)   => fixType(project(2))
      case GetInv(_)     => fixType(project(3))
      case GetNeutral(_) => fixType(project(4))
    }
  }

  private def transformType(tau: Type): Type = tau match {
    case AbelianGroupType(e) =>
      tupleType(IntType, binOpType(e), invType(e), e)

    case _ =>
      tau
  }
}
