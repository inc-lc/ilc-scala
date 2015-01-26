/** Pretty-printing infrastructure
  *
  * For each feature declaring a new Term constructor,
  * it should extend this trait.
  *
  * Subclass are obligated to override:
  *
  * - operatorPrecedence
  *     assign to each term constructor an integer
  *     signifying how loosely the term constructor binds
  *
  * - toPrettyExpresion:
  *     convert declared term constructors
  *     to an operator with arity, fixity and precedence
  *
  * If a new term constructor's case is not overridden in
  * operatorPrecedence, then future subclasses won't be
  * able to look up that constructor's precedence.
  *
  * If a new term constructor's case is not overridden in
  * toPrettyExpression, then that constructor will be
  * printed by calling `toString`.
  */

package ilc
package feature
package base

import org.kiama.output


trait Pretty extends Syntax with PrettyPrinterInterfaceFromKiama {
  /** Juxtaposition */
  trait PrettyJuxtaposedExpression extends output.PrettyBinaryExpression {
    def left : output.PrettyExpression
    def right: output.PrettyExpression

    override def op = ""
  }

  /** Things that are never parenthesized */
  case class PrettyNullaryExpression(toDoc: Doc)
      extends PrettyExpression

  /** mixfix operator with prefix/postfix keyword */
  abstract class PrettyEnclosingExpression extends output.PrettyOperatorExpression {
    def op: Doc
    def exp: PrettyExpression
  }

  /** look up the operator precedence of a term */
  def operatorPrecedence(t: Term): Int = t match {
    case Var(_, _) =>
      Int.MinValue // variables are never parenthesized
  }

  /** `ParenPrettyPrinter.toParenDoc`
    * extended to handle PrettyNullaryExpression
    */
  override def toParenDoc(e: PrettyExpression): Doc = e match {
    case PrettyNullaryExpression(doc) =>
      doc

    case u: PrettyEnclosingExpression =>
      val ed = u.exp match {
        case e: output.PrettyOperatorExpression =>
          val assoc = u.fixity match {
            case Prefix  => RightAssoc
            case Postfix => LeftAssoc
          }
          bracket(e, u, assoc)

        case e =>
          toParenDoc(e)
      }
      if (u.fixity == Prefix)
        group(u.op <> nest(line <> ed))
      else
        group(ed <> nest(line <> u.op))

    // override default implementation of binary operation
    case b: output.PrettyBinaryExpression =>
      val ld = b.left match {
        case l: output.PrettyOperatorExpression =>
          bracket(l, b, LeftAssoc)

        case l =>
          toParenDoc(l)
      }

      val rd = b.right match {
        case r: output.PrettyOperatorExpression =>
          bracket(r, b, RightAssoc)

        case r =>
          toParenDoc(r)
      }

      if (b.op.nonEmpty)
        group(ld <+> text(b.op) <> nest(line <> rd))
      else
        group(ld <> nest(line <> rd))

    case _ =>
      super.toParenDoc(e)
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

  def toDoc(t: Term): Doc =
    toParenDoc(toPrettyExpression(t))

  /** support pretty printing on terms */
  def pretty(t: Term): Layout =
    pretty(t, defaultWidth)

  def pretty(t: Term, width: Width): Layout =
    ParenPrettyPrinter.pretty(toDoc(t), width)
}

trait PrettyPrinterInterfaceFromKiama {
  // we cannot inherit from ParenPrettyPrinter because its
  // interface conflicts with scalatest.Matchers. They both
  // define `empty` of different types.
  //
  // using Ruby's aliasing pattern to restore dynamic dispatch
  // so that `toParenDoc` stays an open method.

  object ParenPrettyPrinter extends output.ParenPrettyPrinter {
    override def toParenDoc(e: PrettyExpression): Doc =
      PrettyPrinterInterfaceFromKiama.this.toParenDoc(e)

    private[PrettyPrinterInterfaceFromKiama]
    def defaultToParenDoc(e: PrettyExpression): Doc =
      super.toParenDoc(e)

    // make parenthesizing super simple
    override def noparens(
      inner: output.PrettyOperatorExpression,
      outer: output.PrettyOperatorExpression,
      side : output.Side):
        Boolean =
      if (matchingAssoc(outer.fixity, side))
        inner.priority <= outer.priority
      else
        inner.priority <  outer.priority

  def matchingAssoc(fixity: output.Fixity, side: output.Side): Boolean =
    fixity match {
      case Infix(LeftAssoc)  => side == LeftAssoc
      case Infix(RightAssoc) => side == RightAssoc
      case Prefix            => side == RightAssoc
      case Postfix           => side == LeftAssoc
      case _                 => false
    }

    // override defaults here
    override val defaultIndent = 2
  }

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

  def bracket(
    inner: output.PrettyOperatorExpression,
    outer: output.PrettyOperatorExpression,
    side : output.Side): Doc
                      = ParenPrettyPrinter.bracket(inner, outer, side)
  def group(d: Doc)   = ParenPrettyPrinter.group(d)
  def nest(d: Doc, i: Int = defaultIndent)
                      = ParenPrettyPrinter.nest(d, i)
  def line: Doc       = ParenPrettyPrinter.line
  def text(s: String) = ParenPrettyPrinter.text(s)

  def defaultWidth    = ParenPrettyPrinter.defaultWidth
  def defaultIndent   = ParenPrettyPrinter.defaultIndent

  def toParenDoc(e: PrettyExpression): Doc =
    ParenPrettyPrinter.defaultToParenDoc(e)
}
