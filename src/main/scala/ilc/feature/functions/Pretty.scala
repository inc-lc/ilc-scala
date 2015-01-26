package ilc
package feature
package functions

/**
 * Pretty printing for first-class functions.
 */

import org.kiama.output._

trait Pretty extends base.Pretty with Syntax {
  override def operatorPrecedence(t: Term): Int = t match {
    case Abs(_, _) =>
      10 // looser than Haskell's default operator (9)

    case App(_, _) =>
      0  // tighter than every positive precedence

    case _ =>
      super.operatorPrecedence(t)
  }

  override def toPrettyExpression(t: Term): PrettyExpression = t match {
    case Abs(Var(x, xType), body) =>
      new PrettyEnclosingExpression {
        def priority = operatorPrecedence(t)
        def fixity   = Prefix

        def op       = text("λ") <> text(x.toString) <> text(".")
        def exp      = toPrettyExpression(body)
      }

    case App(operator, operand) =>
      new PrettyJuxtaposedExpression {
        // application binds tighter than everything
        def priority = operatorPrecedence(t)
        def fixity   = Infix(LeftAssoc)

        def left  = toPrettyExpression(operator)
        def right = toPrettyExpression(operand)
      }

    case _ =>
      super.toPrettyExpression(t)
  }
}
