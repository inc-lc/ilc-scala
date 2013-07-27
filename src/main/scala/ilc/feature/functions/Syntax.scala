package ilc
package feature.functions

/**
 * Untyped lambda calculi with abstraction and application
 * extensible by constants and primitives
 */

import scala.language.implicitConversions

trait Syntax {

  // SUBCLASS OBLIGATIONS

  type Constant

  // SYNTAX

  // Terms are parametric in the set `Constant` of constants.

  sealed abstract trait Term {
    override def toString = (new Pretty)(this)
  }

  case class Var(index: Int) extends Term
  case class App(operator: Term, operand: Term) extends Term
  case class Abs(name: String, body: Term) extends Term
  // The first argument of abstraction serves as documentation
  // alone. Variables are de-Bruijn indices.

  case class Const(c: Constant) extends Term

  // implicit conversion to stop writing `Const`
  implicit def liftConstant(c: Constant): Term = Const(c)

  // WEAKENING

  def weaken(adjustIndex: Int => Int, t: Term): Term = t match {
    case c: Const => c
    case Var(i: Int) => Var(adjustIndex(i))
    case App(s1, s2) =>
      new App(weaken(adjustIndex, s1), weaken(adjustIndex, s2))
    case Abs(x, s) => new Abs(x,
      weaken(i => if (i <= 0) i else 1 + adjustIndex(i - 1), s))
  }

  // PRETTY PRINTING

  // scala> import Language.Atlas._
  // import Language.Atlas._
  //
  // scala> val id = Abs("x", Var(0))
  // id: Language.Atlas.Syntax.Abs = λx. x
  //
  // scala> val y = App(App(id, id), id)
  // y: Language.Atlas.Syntax.app = (λx. x) (λx. x) (λx. x)
  //
  // scala> val z = Abs("x", Abs("x", Abs("x", Abs("x", Var(2)))))
  // z: Language.Atlas.Syntax.Abs = λx. λx₁. λx₂. λx₃. x₁
  //
  // The pretty-printing visitor Pretty is in this file because
  // it depends on Visitor, which depends on Term, which depends
  // on Pretty.

  class Pretty {
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
      apply(t, outermostPriority, List())

    /**
     * Print a term to human-readable syntax.
     *
     * @param t
     *   the term to print
     * @param priority
     *   the priority of the context this term is printed in
     * @param names
     *   the names of bound variables
     */
    def apply(t : Term, priority : Priority, names : List[String]) : String = t match {
      case Const(constant) =>
        constant.toString

      case Var(index) =>
        names.lift(index) getOrElse ("#" + index)

      case App(operator, operand) =>
        template(priorityOfApp, priority, "%s %s",
                 apply(operator, priorityOfApp + 1, names),
                 apply(operand, priorityOfApp, names))

      case Abs(name, body) => {
        val unique = uniqueName(name, names)
        template(priorityOfAbs, priority, "λ%s. %s",
                 unique,
                 apply(body, priorityOfAbs + 1, unique :: names))
      }
    }

    // variable disambiguation

    val subscript = "₀₁₂₃₄₅₆₇₈₉".toCharArray

    def toSubscript(s: String): String = {
      s.map({ (char: Char) =>
        if (char.isDigit) subscript(char - '0') else char
      }).mkString
    }

    def uniqueName(x: String, nameStack : List[String]) = {
      val freq = nameStack.count(_.matches("^" ++ x ++ "[₀-₉]*$"))
      if (freq == 0)
        x
      else
        x ++ toSubscript(freq.toString)
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
