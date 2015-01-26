package ilc
package feature
package bintrees

trait Pretty extends Syntax with base.Pretty {
  override def toPrettyExpression(tau: Type): PrettyExpression = tau match {
    case BinTreeType(elemType) =>
      PrettyNullaryExpression {
        text("<#") <+> toDoc(tau) <+> text("#>")
      }

    case _ =>
      super.toPrettyExpression(tau)
  }
}
