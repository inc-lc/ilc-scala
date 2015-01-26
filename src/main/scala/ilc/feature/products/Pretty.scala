package ilc
package feature
package products

trait Pretty extends base.Pretty with Syntax {
  override def operatorPrecedence(tau: Type): Int = tau match {
    case ProductType(leftType, rightType) =>
      7 // like * in Haskell

    case _ =>
      super.operatorPrecedence(tau)
  }

  override def toPrettyExpression(tau: Type): PrettyExpression = tau match {
    case ProductType(leftType, rightType) =>
      new PrettyBinaryExpression {
        def priority = operatorPrecedence(tau)
        def fixity   = Infix(LeftAssoc)

        def left     = toPrettyExpression(leftType)
        def right    = toPrettyExpression(rightType)

        def op       = UnicodeOutput.choose("×", "*")
      }

    case _ =>
      super.toPrettyExpression(tau)
  }
}
