import language.implicitConversions

/**
 * Untyped lambda calculi with abstraction and application
 * extensible by constants and primitives
 */

object Lambda {

  // SYNTAX

  // Terms are parametric in the set C of constants.

  case class Var[C](i: Int) extends Term[C]
  case class App[C](s: Term[C], t: Term[C]) extends Term[C]
  case class Abs[C](x: String, t: Term[C]) extends Term[C]
  // The first argument of abstraction serves as documentation
  // alone. Variables are de-Bruijn indices.

  case class Const[C](c: C) extends Term[C] {
    override def toString = c.toString
  }

  // DERIVATION

  // String transformation
  def delta(x: String): String = "Δ" ++ x

  // Derivation follows Agda module
  // Syntax.Derive.Canon-Popl14

  def mkDerive[C](deriveConst: C => Term[C],
                  t: Term[C]): Term[C] = {
    def derive(t: Term[C]) = mkDerive(deriveConst, t)
    t match {
      case Const(c)  => deriveConst(c)
      case Var(i)    => Var(2 * i)
      case Abs(x, t) => Abs(x, Abs(delta(x), derive(t)))
      case App(s, t) => App(App(derive(s), t), derive(t))
    }
  }

  // VISITOR/FOLDING

  trait Visitor[C, T] {
    def Const(c: C): T
    def Var(i: Int): T
    def App(me: App[C], s: T, t: T): T
    def Abs(me: Abs[C], x: String, t: T): T

    // folding

    def apply(t: Term[C]): T = t match {
      case Const(c)     => Const(c)
      case Var(i)       => Var(i)
      case me@App(s, t) => App(me, this(s), this(t))
      case me@Abs(x, t) => { bind(x) ; unbind(me, this(t)) }
    }

    // variable name resolution

    def resolveName(i: Int) = nameStack(i)

    // bind/unbind should be private to Term[C], but
    // Term[C] is not an enclosing class. What to do?
    def bind(x: String) { pushName(x) }
    def unbind(me: Abs[C], t: T): T = { Abs(me, popName(), t) }

    // stack of names: private to Visitor

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

  sealed abstract trait Term[C] {
    override def toString = (new Pretty)(this)
  }

  // PRETTY PRINTING

  // scala> import Lambda._ ; val id = Abs("x", Var(0))
  // import Lambda._
  // id: Lambda.Abs[Nothing] = λx. x

  // scala> val y = App[Nothing](App[Nothing](id, id), id)
  // y: Lambda.App[Nothing] = (λx. x) (λx. x) (λx. x)

  // scala> val z = Abs("x", Abs("x", Abs("x", Abs("x", Var(2)))))
  // z: Lambda.Abs[Nothing] = λx. λx₁. λx₂. λx₃. x₁

  class Pretty[C] extends Visitor[C, String] {

    def Const(c: C) = c.toString

    def Var(i: Int) = resolveName(i)

    def App(me: App[C], s: String, t: String) =
      parenthesizeWisely(me, s, LeftChild) ++ " " ++
        parenthesizeWisely(me, t, RightChild)

    def Abs(me: Abs[C], x: String, t: String) =
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
      nameStack =
        (if (freq == 0) x
         else x ++ toSubscript(freq.toString)) :: nameStack
    }

    // parentheses handling
    //
    // - subterms of λ are not parenthesized
    // - nested applications are not parenthesized
    // - constants are not parenthesized
    // - variables are not parenthesized
    // - all other subterms are parenthesized

    sealed trait Laterality
    case object LeftChild extends Laterality
    case object RightChild extends Laterality

    def shouldParenthesize(me: Term[C], child: Laterality): Boolean = {
      me match {
        case Abs(x, t) => false
        case App(s, t) => shouldParenthesizeApp(child, s, t)
        case _ =>
          sys error "Constants and variables should not have subterms"
      }
    }

    def shouldParenthesizeApp(child: Laterality,
                              s: Term[C],
                              t: Term[C]): Boolean = {
      (if (child == LeftChild) s else t) match {
          case Const(c)  => false
          case Var(i)    => false
          case App(s, t) => !(child == LeftChild)
          case Abs(x, t) => true
      }
    }

    def parenthesizeWisely(me: Term[C],
                           subterm: String,
                           child: Laterality = RightChild): String = {
      if (shouldParenthesize(me, child))
        "(" ++ subterm ++ ")"
      else
        subterm
    }
  }
}
