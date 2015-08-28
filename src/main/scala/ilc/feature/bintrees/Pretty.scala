package ilc
package feature
package bintrees

import org.kiama.output._

trait Pretty extends Syntax with base.Pretty {
  override def toPrettyExpression(tau: Type): PrettyExpression = tau match {
    case BinTreeType(elemType) =>
      PrettyNullaryExpression {
        text("<#") <+> toDoc(elemType) <+> text("#>")
      }

    case _ =>
      super.toPrettyExpression(tau)
  }
}
