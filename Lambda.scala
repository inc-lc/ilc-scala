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
    def App(s: T, t: T): T
    def Abs(x: String, t: T): T

    // variable name resolution

    def resolveName(i: Int) = nameStack(i)

    // bind/unbind should be private to Term[C], but
    // Term[C] is not an enclosing class. What to do?
    def bind(x: String) { pushName(x) }
    def unbind(t: T): T = { Abs(popName(), t) }

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
    def accept[T](v: Visitor[C, T]): T = this match {
      case Const(c)  => v.Const(c)
      case Var(i)    => v.Var(i)
      case App(s, t) => v.App(s.accept(v), t.accept(v))
      case Abs(x, t) => { v.bind(x) ; v.unbind(t.accept(v)) }
    }

    override def toString = {
      accept(new Pretty(accept(logger)))
    }
  }

  // LOGGING TRAVERSAL SEQUENCE

  def logger[C] = new Visitor[C, List[Term[C]]] {
    type Log = List[Term[C]]

    def Const(c: C)            = push(new Const(c))
    def Var(i: Int)            = push(new Var(i))
    def App(s: Log, t: Log)    = push(new App(s.head, t.head))
    def Abs(x: String, t: Log) = push(new Abs(x, t.head))

    var log: Log = Nil
    def push(t: Term[C]): Log = {
      log = t :: log
      log
    }
  }

  // PRETTY PRINTING

  // scala> import Lambda._ ; val id = Abs("x", Var(0))
  // import Lambda._
  // id: Lambda.Abs[Nothing] = λx. x

  // scala> val y = App[Nothing](App[Nothing](id, id), id)
  // y: Lambda.App[Nothing] = (λx. x) (λx. x) (λx. x)

  // scala> val z = Abs("x", Abs("x", Abs("x", Abs("x", Var(2)))))
  // z: Lambda.Abs[Nothing] = λx. λx₁. λx₂. λx₃. x₁

  class Pretty[C](log: List[Term[C]]) extends Visitor[C, String] {

    def Const(c: C) = {
      proceed()
      c.toString
    }

    def Var(i: Int) = {
      proceed()
      resolveName(i)
    }

    def App(s: String, t: String) = {
      proceed()
      parenthesizeWisely(s, LeftChild) ++ " " ++
        parenthesizeWisely(t, RightChild)
    }

    def Abs(x: String, t: String) = {
      proceed()
      "λ" ++ x ++ ". " ++ parenthesizeWisely(t)
    }

    // variable disambiguation

    val subscripts = "₀₁₂₃₄₅₆₇₈₉"

    def toSubscript(s: String): String = {
      val arr: Array[Char] = s.toCharArray.map({ char =>
        if (char.isDigit) ('₀' + (char - '0')).toChar else char
      })
      arr.mkString
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

    var upcoming = new Var[C](0) :: log.reverse
    def proceed() { upcoming = upcoming.tail }

    sealed trait Laterality
    case object LeftChild extends Laterality
    case object RightChild extends Laterality

    def myself = upcoming.head      

    def shouldParenthesize(child: Laterality): Boolean = {
      myself match {
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

    def parenthesizeWisely(subterm: String,
                           child: Laterality = RightChild): String = {
      if (shouldParenthesize(child))
        "(" ++ subterm ++ ")"
      else
        subterm
    }
  }
}
