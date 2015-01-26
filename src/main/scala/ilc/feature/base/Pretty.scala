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


/** Unary expressions with space beind */
trait SpacedPrettyUnaryExpression extends output.PrettyUnaryExpression

/** Juxtaposition */
trait PrettyJuxtaposedExpression extends output.PrettyBinaryExpression {
  def left : output.PrettyExpression
  def right: output.PrettyExpression

  override def op = ""
}

trait Pretty extends Syntax with PrettyPrinterInterfaceFromKiama {
  /** Things that are never parenthesized */
  case class PrettyNullaryExpression(toDoc: Doc)
      extends PrettyExpression

  /** `ParenPrettyPrinter.toParenDoc`
    * extended to handle PrettyNullaryExpression
    */
  override def toParenDoc(e: PrettyExpression): Doc = e match {
    case PrettyNullaryExpression(doc) =>
      doc

    case u: SpacedPrettyUnaryExpression =>
      import ParenPrettyPrinter.{bracket, group, indent, line}
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
        group(text(u.op) <> line <> indent(ed))
      else
        group(ed <> line <> indent(text(u.op)))

    // override default implementation of binary operation
    case b: output.PrettyBinaryExpression =>
      import ParenPrettyPrinter.{bracket, group, indent, line}

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
        group(ld <+> text(b.op) <> line <> indent(rd))
      else
        group(ld <> line <> indent(rd))

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

  /** support pretty printing on terms */
  def pretty(t: Term): Layout =
    pretty(t, defaultWidth)

  def pretty(t: Term, width: Width): Layout =
    ParenPrettyPrinter.pretty(toParenDoc(toPrettyExpression(t)), width)
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

    // override defaults here:
    override val defaultIndent = 2
  }

  // aliases such that subclasses need not be aware of Kiama
  type SpacedPrettyUnaryExpression = ilc.feature.base.SpacedPrettyUnaryExpression
  type PrettyJuxtaposedExpression  = ilc.feature.base.PrettyJuxtaposedExpression

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

  def toParenDoc(e: PrettyExpression): Doc =
    ParenPrettyPrinter.defaultToParenDoc(e)
}
