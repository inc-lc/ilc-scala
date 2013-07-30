package ilc
package feature.functions

/**
 * Pretty printing for first-class functions.
 */

trait Pretty extends Syntax {
  object pretty extends (Term => String) {
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
    def template(inner : Priority, outer : Priority, format : String, args : String*) = {
      val text = format.format(args : _*)
      if (inner < outer)
        text
       else
         "(" + text + ")"
    }

    /**
     * Print a closed term to human-readable syntax.
     *
     * @param t
     *   the term to print
     */
    def apply(t : Term) : String =
      apply(t, outermostPriority)

    /**
     * Print a term to human-readable syntax.
     *
     * @param t
     *   the term to print
     * @param priority
     *   the priority of the context this term is printed in
     */
    def apply(t : Term, priority : Priority) : String = t match {
      case Const(constant) =>
        constant.toString

      case Var(name) =>
        name

      case App(operator, operand) =>
        template(priorityOfApp, priority, "%s %s",
                 apply(operator, priorityOfApp + 1),
                 apply(operand, priorityOfApp))

      case Abs(name, body) => {
        template(priorityOfAbs, priority, "λ%s. %s",
                 name,
                 apply(body, priorityOfAbs + 1))
      }
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
  }
}
