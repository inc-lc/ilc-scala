/** Pretty-printing infrastructure using Kiama
  *
  * For each feature declaring a new Term constructor,
  * it should extend this trait.
  *
  * Subclass are obligated to override:
  *
  * - operatorPrecedence, for terms and types:
  *     assign to each term constructor an integer
  *     signifying how tightly the term constructor binds
  *
  * - toPrettyExpresion, for terms and types:
  *     convert declared term constructors
  *     to an operator with arity, fixity and precedence
  *
  * If a new term constructor's case is not overridden in
  * operatorPrecedence, then that constructor will be
  * considered atomic and bind the tightest.
  *
  * If a new term constructor's case is not overridden in
  * toPrettyExpression, then that constructor will be
  * printed by calling `toString`.
  *
  * CAUTION: due to interface conflict between Kiama and
  * Scalatest, trait Pretty cannot be a subclass of
  * kiama.output.ParenPrettyPrinter. We have to delegate
  * instead.
  *
  * Subclasses may import from ParenPrettyPrinter.
  * However, directly imported methods won't perform
  * dynamic dispatch. If dynamic dispatch/open method
  * is desired, then one must define an alias in
  * trait PrettyPrinterInterfaceFromKiama, similar to
  * `bracket`, `line`, `nest` etc.
  */

package ilc
package feature
package base

import org.kiama.output
import output._

trait PrettyPrintable {
  def prettyPrintDefault: String = toString
}

trait Pretty extends Syntax with ParenPrettyPrinter {
  /** Juxtaposition */
  trait PrettyJuxtaposedExpression extends PrettyBinaryExpression {
    def left : PrettyExpression
    def right: PrettyExpression

    override def op = ""
  }

  /** Things that are never parenthesized */
  case class PrettyNullaryExpression(toDoc: Doc)
      extends PrettyExpression

  /** mixfix operator with prefix/postfix keyword */
  abstract class PrettyEnclosingExpression extends PrettyOperatorExpression {
    def op: Doc
    def exp: PrettyExpression
  }

  def operatorPrecedence(tau: Type): Int =
    Int.MaxValue // by default, do not parenthesize types

  /** look up the operator precedence of a term */
  def operatorPrecedence(t: Term): Int = t match {
    case Var(_, _) =>
      Int.MaxValue // variables are never parenthesized
  }

  /** @return org.kiama.output.PrettyExpression
    *         representing the given type
    *
    * By default, render type `tau` by calling `tau.toString`
    */
  def toPrettyExpression(tau: Type): PrettyExpression =
    PrettyNullaryExpression(text(tau.prettyPrintDefault))

  /** @return org.kiama.output.PrettyExpression
    *         representing the given term
    *
    * By default, render term `t` by calling `t.toString`.
    */
  def toPrettyExpression(t: Term): PrettyExpression = t match {
    case Var(name, tpe) =>
      PrettyNullaryExpression(text(name.toString))

    case unknownTerm =>
      PrettyNullaryExpression(text(unknownTerm.prettyPrintDefault))
  }

  /** `ParenPrettyPrinter.toParenDoc`
    * extended to handle PrettyNullaryExpression
    */
  override def toParenDoc(e: PrettyExpression): Doc = e match {
    case PrettyNullaryExpression(doc) =>
      doc

    case u: PrettyEnclosingExpression =>
      val ed = u.exp match {
        case e: PrettyOperatorExpression =>
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
    case b: PrettyBinaryExpression =>
      val ld = b.left match {
        case l: PrettyOperatorExpression =>
          bracket(l, b, LeftAssoc)

        case l =>
          toParenDoc(l)
      }

      val rd = b.right match {
        case r: PrettyOperatorExpression =>
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

  def toDoc(t: Term): Doc =
    toParenDoc(toPrettyExpression(t))

  /** support pretty printing on terms */
  def pretty(t: Term): Layout =
    pretty(t, defaultWidth)

  def pretty(t: Term, width: Width): Layout =
    pretty(toDoc(t), width)

  def toDoc(t: Type): Doc =
    toParenDoc(toPrettyExpression(t))

  /** support pretty printing on terms */
  def pretty(t: Type): Layout =
    pretty(t, defaultWidth)

  def pretty(t: Type, width: Width): Layout =
    pretty(toDoc(t), width)

  // make parenthesizing super simple
  override def noparens(inner: PrettyOperatorExpression,
                        outer: PrettyOperatorExpression,
                        side: Side): Boolean = {
    if (matchingAssoc(outer.fixity, side))
      inner.priority >= outer.priority
    else
      inner.priority > outer.priority
  }

  def matchingAssoc(fixity: Fixity, side: Side): Boolean =
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