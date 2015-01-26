package ilc
package feature
package sums

/**
 * Pretty printing for first-class functions.
 */

trait Pretty extends base.Pretty with Syntax {
  override def operatorPrecedence(tau: Type): Int = tau match {
    case SumType(leftType, rightType) =>
      6 // like + in Haskell

    case _ =>
      super.operatorPrecedence(tau)
  }

  override def toPrettyExpression(tau: Type): PrettyExpression = tau match {
    case SumType(leftType, rightType) =>
      new PrettyBinaryExpression {
        def priority = operatorPrecedence(tau)
        def fixity   = Infix(LeftAssoc)

        def left     = toPrettyExpression(leftType)
        def right    = toPrettyExpression(rightType)

        def op       = "+"
      }

    case _ =>
      super.toPrettyExpression(tau)
  }
}
