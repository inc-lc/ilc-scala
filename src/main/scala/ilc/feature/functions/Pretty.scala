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
   * @param text
   *   by-name argument with content to print.
   */
  def template(inner : Priority, outer : Priority, text : => String) = {
    if (bindsStronger(inner, outer))
      text
     else
       "(" + text + ")"
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
        //Increase indentation and break line before each argument.
        s"${pretty(operator, priorityOfApp + 1)}${deeper(s"$indent${pretty(operand, priorityOfApp)}")}")

    case Abs(variable, body) =>
      //Break lines before the body.

      //How much should the body be indented, for perfect visual alignment? That's tricky.
      val outputStartsWithParen = !bindsStronger(priorityOfAbs, priority)
      val isBodyNestedAbs = body match {
        case Abs(_, _) => true
        case _ => false
      }
      val increaseInnerIndent =
        //Nested abstractions should align with the first one.
        (if (isBodyNestedAbs) 0 else indentDiff) +
        //If the output starts with a parenthesis, indent the content by one column.
        (if (outputStartsWithParen) 1 else 0)

      template(priorityOfAbs, priority,
        s"λ${variable.getName.toString}.${deeper(s"$indent${pretty(body, priorityOfAbs + 1)}", increaseInnerIndent)}")

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
