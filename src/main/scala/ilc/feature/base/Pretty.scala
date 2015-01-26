/** Pretty-printing infrastructure
  *
  * For each feature declaring a new Term constructor,
  * it should extend this trait.
  *
  * Subclass are obligated to override:
  *
  * - toPrettyExpresion:
  *     convert declared term constructors
  *     to an operator with arity, fixity and precedence
  */

package ilc
package feature
package base

import org.kiama.output

trait Pretty extends Syntax with PrettyPrinterInterfaceFromKiama {
  /** Things that are never parenthesized */
  case class PrettyNullaryExpression(toDoc: Doc)
      extends PrettyExpression

  /** `ParenPrettyPrinter.toParenDoc`
    * extended to handle PrettyNullaryExpression
    */
  def toParenDoc(e: PrettyExpression): Doc = e match {
    case PrettyNullaryExpression(doc) => doc
    case _                            => ParenPrettyPrinter.toParenDoc(e)
  }

  /** @return org.kiama.output.PrettyExpression
    *         representing the given term
    *
    * By default, render term `t` by calling `t.toString`.
    */
  def toPrettyExpression(t: Term): PrettyExpression = t match {
    case Var(name, tpe) =>
      PrettyNullaryExpression(text(name.toString))

    case unknownTerm =>
      PrettyNullaryExpression(text(unknownTerm.toString))
  }

  /** support pretty printing on terms */
  def pretty(t: Term): Layout =
    pretty(t, defaultWidth)

  def pretty(t: Term, width: Width): Layout =
    ParenPrettyPrinter.pretty(toParenDoc(toPrettyExpression(t)), width)
}

trait PrettyPrinterInterfaceFromKiama {
  // we cannot inherit from ParenPrettyPrinter because its
  // interface conflicts with scalatest.Matchers.

  object ParenPrettyPrinter extends output.ParenPrettyPrinter

  // aliases such that subclasses need not be aware of Kiama
  import org.kiama.output
  trait PrettyUnaryExpression  extends output.PrettyUnaryExpression
  trait PrettyBinaryExpression extends output.PrettyBinaryExpression

  type PrettyExpression = output.PrettyExpression
  val  LeftAssoc        = output.LeftAssoc
  val  RightAssoc       = output.RightAssoc
  val  NonAssoc         = output.NonAssoc
  val  Prefix           = output.Prefix
  val  Postfix          = output.Postfix
  val  Infix            = output.Infix

  type Doc    = ParenPrettyPrinter.Doc
  type Width  = ParenPrettyPrinter.Width
  type Layout = ParenPrettyPrinter.Layout

  def text(s: String) = ParenPrettyPrinter.text(s)
  def defaultWidth    = ParenPrettyPrinter.defaultWidth
}
