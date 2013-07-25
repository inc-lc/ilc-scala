package Syntax

/**
 * Untyped lambda calculi with abstraction and application
 * extensible by constants and primitives
 */

import language.implicitConversions

trait Lambda {

  // SUBCLASS OBLIGATIONS

  type Constant
  def deriveConst(c: Constant): Term

  // SYNTAX

  // Terms are parametric in the set `Constant` of constants.

  sealed abstract trait Term {
    override def toString = (new Pretty)(this)
  }

  case class Var(i: Int) extends Term
  case class App(s: Term, t: Term) extends Term
  case class Abs(x: String, t: Term) extends Term
  // The first argument of abstraction serves as documentation
  // alone. Variables are de-Bruijn indices.

  case class Const(c: Constant) extends Term

  // implicit conversion to stop writing `Const`
  implicit def liftConstant(c: Constant): Term = Const(c)


  // DERIVATION

  // String transformation
  def delta(x: String): String = "Δ" ++ x

  // Derivation follows Agda module
  // Syntax.Derive.Canon-Popl14

  def derive(t: Term): Term = t match {
    case Const(c)  => deriveConst(c)
    case Var(i)    => Var(2 * i)
    case Abs(x, t) => Abs(x, Abs(delta(x), derive(t)))
    case App(s, t) => App(App(derive(s), t), derive(t))
  }

  // VISITOR/FOLDING

  trait Visitor[T] {

    def Const(c: Constant): T
    def Var(i: Int): T
    def App(me: App, s: T, t: T): T
    def Abs(me: Abs, x: String, t: T): T

    // folding

    def apply(t: Term): T = t match {
      case Const(c)     => Const(c)
      case Var(i)       => Var(i)
      case me@App(s, t) => App(me, this(s), this(t))
      case me@Abs(x, t) => { bind(x) ; unbind(me, this(t)) }
    }

    // variable name resolution

    def resolveName(i: Int) =
      if (i < nameStack.length)
        nameStack(i)
      else {
        // come up with a default name for free variables:
        // 1. convert de-Bruijn index to negated de-Bruijn level
        // 2. de-Bruijn level can be used to disambiguate names,
        //    because all occurrences of a variable have the
        //    same level.
        val negLevel = i - nameStack.length
        "FV#" ++ negLevel.toString
      }

    // stack of names: private to Visitor

    protected[this] def bind(x: String) { pushName(x) }
    protected[this] def unbind(me: Abs, t: T): T = { Abs(me, popName(), t) }

    protected[this] var nameStack = List.empty[String]

    protected[this] def pushName(x: String) {
      nameStack = x :: nameStack
    }

    protected[this] def popName(): String = {
      val topName = nameStack.head
      nameStack = nameStack.tail
      topName
    }
  }

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

  class Pretty extends Visitor[String] {

    def Const(c: Constant) = c.toString

    def Var(i: Int) = resolveName(i)

    def App(me: App, s: String, t: String) =
      parenthesizeWisely(me, s, LeftChild) ++ " " ++
        parenthesizeWisely(me, t, RightChild)

    def Abs(me: Abs, x: String, t: String) =
      "λ" ++ x ++ ". " ++ parenthesizeWisely(me, t)

    // variable disambiguation

    val subscript = "₀₁₂₃₄₅₆₇₈₉".toCharArray

    def toSubscript(s: String): String = {
      s.map({ (char: Char) =>
        if (char.isDigit) subscript(char - '0') else char
      }).mkString
    }

    override def pushName(x: String) {
      val freq = nameStack.count(_.matches("^" ++ x ++ "[₀-₉]*$"))
      super.pushName(if (freq == 0)
                       x
                     else
                       x ++ toSubscript(freq.toString))
    }

    // parentheses handling
    //
    // - subterms of λ are not parenthesized
    // - nested applications are not parenthesized
    // - constants are not parenthesized
    // - variables are not parenthesized
    // - all other subterms are parenthesized

    sealed trait Laterality
    case object LeftChild  extends Laterality
    case object RightChild extends Laterality

    def shouldParenthesize(me: Term, child: Laterality): Boolean = {
      me match {
        case Abs(x, t) => false
        case App(s, t) => shouldParenthesizeApp(child, s, t)
        case _ =>
          sys error "Constants and variables should not have subterms"
      }
    }

    def shouldParenthesizeApp(child: Laterality,
                              s: Term,
                              t: Term): Boolean = {
      (if (child == LeftChild) s else t) match {
          case Const(c)  => false
          case Var(i)    => false
          case App(s, t) => !(child == LeftChild)
          case Abs(x, t) => true
      }
    }

    def parenthesizeWisely(me: Term,
                           subterm: String,
                           child: Laterality = RightChild): String = {
      if (shouldParenthesize(me, child))
        "(" ++ subterm ++ ")"
      else
        subterm
    }
  }

}
