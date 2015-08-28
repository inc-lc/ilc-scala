package ilc
package feature
package let

import org.kiama.output._

trait Pretty extends functions.Pretty with Syntax {
  override def operatorPrecedence(t: Term): Int = t match {
    case Let(variable, exp, body) =>
      // let-expressions binds as loosely as lambda expressions
      super.operatorPrecedence(Abs(variable, body))

    case _ =>
      super.operatorPrecedence(t)
  }

  override def toPrettyExpression(t: Term): PrettyExpression =
    t match {
      case Let(x, xdef, body) =>
        new PrettyEnclosingExpression {
          def priority = operatorPrecedence(t)
          def fixity   = Prefix

          def op =
            group(
              text("let") <>
                nest(line <>
                  text(x.getName.toString) <+> text("=") <>
                    nest(line <> toDoc(xdef))
                ) <> line <>
                text("in"))

          def exp = toPrettyExpression(body)
        }

      case _ =>
        super.toPrettyExpression(t)
    }
}
