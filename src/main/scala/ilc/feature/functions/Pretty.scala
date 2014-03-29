package ilc
package feature.functions

/**
 * Pretty printing for first-class functions.
 */

trait Pretty extends Syntax with util.IndentUtils {
  //Local-only addition to IndentUtils
  protected def closeParenNoIndent(delim: String) = { indentLess(); delim }

  /**
   * Pretty print an expression according to a template.
   *
   * @param inner
   *   priority of the language construct to be printed
   * @param outer
   *   priority of the context to be printed in
   * @param format
   *   format string, use @code{%s} as placeholder
   * @param args
   *   arguments to be put into the placeholders
   */
  def template(inner : Priority, outer : Priority, text : => String) = {
    if (bindsStronger(inner, outer))
      text
     else
       openParen("(") + text + closeParenNoIndent(")")
       //Alternative: have body on separate lines as the parens
       //openParen("(") + indent() + text + closeParen(")")
  }

  /**
   * Print a closed term to human-readable syntax.
   *
   * @param t
   *   the term to print
   */
  def pretty(t: Term): String = {
    //Set the initial indent
    setIndentDepth(4)
    pretty(t, outermostPriority)
  }

  /**
   * Print a term to human-readable syntax.
   *
   * @param t
   *   the term to print
   * @param priority
   *   the priority of the context this term is printed in
   */
  def pretty(t : Term, priority : Priority) : String = t match {
    case variable: Var =>
      variable.getName.toString

    case App(operator, operand) =>
      template(priorityOfApp, priority,
        s"${pretty(operator, priorityOfApp + 1)}$indent${pretty(operand, priorityOfApp)}")

    case Abs(variable, body) =>
      template(priorityOfAbs, priority,
        s"λ${variable.getName.toString}.$indent${pretty(body, priorityOfAbs + 1)}")

    // other operations would throw "unknown term" error here.
    // the pretty printer defaults to calling `toString`.
    case _ =>
      t.toString
  }

  // parentheses handling
  //
  // - subterms of λ are not parenthesized
  // - nested applications are not parenthesized
  // - constants are not parenthesized
  // - variables are not parenthesized
  // - all other subterms are parenthesized

  /**
   * The priority of an operator or language construct, for
   * deciding whether to add parentheses when pretty printing.
   *
   * Smaller numbers mean stronger binding. For example, the
   * priority of multiplication should be a number less than
   * the priority of addition.
   */
  type Priority = Int

  /**
   * Priority of application.
   */
  val priorityOfApp : Priority = 1

  /**
   * Priority of abstraction.
   */
  val priorityOfAbs : Priority = 2

  /**
   * Priority of the outermost context.
   *
   * This priority is used when a term occurs in a context
   * that is not a term itself.
   */
  val outermostPriority : Priority = 3

  private def bindsStronger(a: Priority, b: Priority): Boolean = a < b
}
