package ilc
package feature
package functions

/**
 * Pretty printing for first-class functions.
 */

import org.kiama.output._

trait Pretty extends base.Pretty with Syntax {
  override def operatorPrecedence(tau: Type): Int = tau match {
    case domain =>: range =>
      5
      // like in ILC's Agda code base: Parametric.Syntax.Type:
      // infixr 5 _⇒_
      //
      // Ref.
      // http://www.informatik.uni-marburg.de/~pgiarrusso/ILC/AEC/agda/Parametric.Syntax.Type.html

    case _ =>
      super.operatorPrecedence(tau)
  }

  override def operatorPrecedence(t: Term): Int = t match {
    case Abs(_, _) =>
      0 // looser than everything

    case App(_, _) =>
      10 // tighter than Haskell's default operator precedence (9)

    case _ =>
      super.operatorPrecedence(t)
  }

  override def toPrettyExpression(tau: Type): PrettyExpression = tau match {
    case domain =>: range =>
      new PrettyBinaryExpression {
        def priority = operatorPrecedence(tau)
        def fixity   = Infix(RightAssoc)

        def left     = toPrettyExpression(domain)
        def right    = toPrettyExpression(range)

        def op       = UnicodeOutput.choose("→", "->")
      }

    case _ =>
      super.toPrettyExpression(tau)
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
