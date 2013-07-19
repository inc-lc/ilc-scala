import language.implicitConversions

// Put everything into an object, since Scala does not have top-level definition
// (except at the REPL).
object ILC {

  // SYNTAX

  abstract class Exp

  // Well-typed programs should not go Wrong.
  case object Wrong extends Exp

  case class Var(x: Symbol) extends Exp {
    override def toString = x.name
  }

  case class Lam(x: Symbol, e: Exp) extends Exp {
    override def toString = "\\"+x.name+"."+e.toString
  }

  case class App(e1: Exp, e2: Exp) extends Exp {
    override def toString = e1 match {
      case _: Lam => "(" + e1.toString + ") " + e2.toString
      case _ => e1.toString + " " +e2.toString
    }
  }

  case class Deriv(e: Exp) extends Exp

  case class Num(n: Int) extends Exp {
    override def toString = n.toString
  }

  case class Add(e1: Exp, e2: Exp) extends Exp {
    override def toString = e1.toString + " + " + e2.toString
  }

  case class Pair(e1: Exp, e2: Exp) extends Exp {
    override def toString = "(" + e1.toString + "," + e2.toString + ")"
  }

  case class Proj1(e: Exp) extends Exp {
    override def toString = e.toString + "._1"
  }

  case class Proj2(e: Exp) extends Exp {
    override def toString = e.toString + "._2"
  }

  case class Left(e: Exp) extends Exp

  case class Right(e: Exp) extends Exp

  case class Case(e: Exp, x: Symbol, left: Exp, right: Exp) extends Exp {
    override def toString =
      "case " + e.toString + " of Left(" + x.name + ") => " +
      left.toString + "; Right(" + x.name + ") => " + right.toString
  }

  case object True extends Exp

  case object False extends Exp

  case class If(cond: Exp, thenBranch: Exp, elseBranch: Exp) extends Exp

  implicit def num2exp(n: Int) = Num(n)

  implicit def id2exp(s: Symbol) = Var(s)

  // NAME MANIPULATION

  def xtodx(x: Symbol) : Symbol = Symbol("d" + x.name)

  var varcount = 0
  def freshName : Symbol = {
    varcount += 1
    Symbol("x" + varcount.toString)
  }

  // FOLDING

  case class ExpVisitor[B] (
    tVar   : Symbol => B,
    tApp   : (B, B) => B,
    tLam   : (Symbol, B) => B,
    tDoe   : B => B,
    tNum   : Int => B,
    tAdd   : (B, B) => B,
    tPair  : (B, B) => B,
    tProj1 : B => B,
    tProj2 : B => B,
    tLeft  : B => B,
    tRight : B => B,
    tCase  : (B, Symbol, B, B) => B,
    tTrue  : B,
    tFalse : B,
    tIf    : (B, B, B) => B
  )

  // foldExp : forall B. ExpVisitor[B] -> Exp -> B
  object foldExp {
    def apply[B](v: ExpVisitor[B]): Exp => B = new Function1[Exp, B] {
      def apply(e: Exp): B = e match {
        case Var(x)                     => v.tVar(x)
        case App(e1, e2)                => v.tApp(this(e1), this(e2))
        case Lam(x, e1)                 => v.tLam(x, this(e1))
        case Deriv(e0)                  => v.tDoe(this(e0))
        case Num(n)                     => v.tNum(n)
        case Add(e1, e2)                => v.tAdd(this(e1), this(e2))
        case Pair(e1, e2)               => v.tPair(this(e1), this(e2))
        case Proj1(e0)                  => v.tProj1(this(e0))
        case Proj2(e0)                  => v.tProj2(this(e0))
        case Left(e0)                   => v.tLeft(this(e0))
        case Right(e0)                  => v.tRight(this(e0))
        case Case(cond, x, left, right) => v.tCase(this(cond), x, this(left), this(right))
        case True                       => v.tTrue
        case False                      => v.tFalse
        case If(cond, thenBr, elseBr)   => v.tIf(this(cond), this(thenBr), this(elseBr))
        case Wrong                      => sys error "visiting Wrong"
      }
    }
  }

  // SYNTACTIC SUGAR

  val etrue = True
  val efalse = False
  def ifthenelse(c: Exp, t: Exp, e: Exp) = Case(c, freshName, t, e)
  def and(e1: Exp, e2: Exp) = Case(e1, freshName, e2, efalse)
  def or(e1: Exp, e2: Exp) = Case(e1, freshName, etrue, e2)
  def let(x: Symbol, e1: Exp, e2: Exp) = App(Lam(x, e2), e1)

  // FREE VARIABLES

  def freevars(e: Exp): Set[Symbol] = foldExp(ExpVisitor[Set[Symbol]](
    Set(_),
    _ ++ _,
    (x, s) => s - x,
    identity,
    _ => Set(),
    _ ++ _,
    _ ++ _,
    identity,
    identity,
    identity,
    identity,
    (fvCond, x, fvLeft, fvRight) => fvCond ++ ((fvLeft ++ fvRight) - x),
    Set(),
    Set(),
    _ ++ _ ++ _
  ))(e)

  def eIsOpen(e: Exp): Exp =
    if(freevars(e).size == 0)
      efalse
    else
      etrue

  // LIFTING OPERATIONS
  // (to account for boolean isNil tags)

  // this code is so schematic that it could mostly be generated
  // e.g. as a datatype-generic algorithm
  //
  // the "Case" part could presumably be optimized a little more,
  // though

  def map0(e: Exp, f: Exp => Exp, booleanTag: Exp) : Exp = {
    Pair(f(Proj1(e)),
         booleanTag)
  }

  def map1(e: Exp, f: Exp => Exp) : Exp = {
    val x = freshName
    let(x, e,
      Pair(f(Proj1(x)),
           Proj2(x)))
  }

  def map2(e1: Exp, e2: Exp, f: (Exp,Exp) => Exp) : Exp = {
    val x = freshName
    val y = freshName
    let(x, e1,
      let(y, e2,
        Pair(f(Proj1(x), Proj1(y)),
             or(Proj2(x),Proj2(y)))))
  }

  def map3(e1: Exp, e2: Exp, e3: Exp, f: (Exp,Exp,Exp) => Exp) : Exp = {
    val x = freshName
    val y = freshName
    val z = freshName
    let(x, e1,
      let(y, e2,
        let(z, e3,
          Pair(f(Proj1(x), Proj1(y), Proj1(z)),
               or(or(Proj2(x), Proj2(y)), Proj2(z))))))
  }

  // SYMBOLIC DERIVATION

  /*
  derive :: Exp T -> Exp delta(T)

  where
  delta(Int) = (Int, Bool)
  delta(T1 -> T2) = (T1 -> delta(t1) -> delta(T2), Bool)
  delta( (X,Y) ) = ((delta(X) + delta(Y)) + (delta(X), delta(Y)), Bool)
  delta( X + Y ) = ((delta(X) + delta(Y)) + X + Y, Bool)

  The second component in this pair always denotes whether the change is not nil
  For delta(sum/product type), if second component is False then first component
  can be Wrong, i. e., a nil-change of sum/product should never be examined
  */

  def derive(e: Exp) : Exp = e match {
    case Wrong => sys error "deriving Wrong"

    case Num(n: Int) => Pair(Num(n), efalse)

    case Var(x: Symbol) => Var(xtodx(x))

    case App(e1,e2) =>
      map2(derive(e1), derive(e2), { (de1, de2) => App(App(de1,e2), derive(e2)) })

    case Lam(x,e) =>
      map0(derive(e), de => Lam(x, Lam(xtodx(x), de)), eIsOpen(Lam(x, e)))

      // Remark: We should *never* set the Boolean tag according to the Boolean
      // tag of the body, which evaluates at runtime to the Boolean tag of the
      // return value. A nil change to a function may return a non-nil change
      // if its second argument is non-nil, and a non-nil change to a function
      // may return a nil-change on some arguments.
      //
      // The Boolean tag of the derivative of a function is not used when applied.
      // It is used only when the function is an argument of something, say a
      // Pair constructor. The whole picture is not clear to me yet. (Cai 08.04.13)

    case Deriv(e) => sys error "I don't know for now; depends on case Pair, Proj* etc"

      //  t : T    delta(T) = (T', Bool)
      //  ------------------------------
      //    Deriv(t) : (T', Bool)

    case Add(e1,e2) =>
      map2(derive(e1), derive(e2), {
        case (de1, de2) => Add(de1, de2)
      })

      // Rationale: If change to integers is replacement, then change to a sum is sum
      // of replacements of summands. See also case Num.

    case Pair(e1,e2) => {
      // delta( (X,Y) ) = ((delta(X) + delta(Y)) + (delta(X), delta(Y)), Bool)
      //
      // The derivative of a pair tests if either component sees a nil change
      // and takes care not to report it.

      val de1 = freshName
      val de2 = freshName
      let(de1, derive(e1),
        let(de2, derive(e2),
          ifthenelse(Proj2(de1),
            ifthenelse(Proj2(de2),
              /* TT */ Pair(Right(Pair(Proj1(de1), Proj1(de2))), etrue),
              /* TF */ Pair(Left(Left(Proj1(de1))), etrue)
            ),
            ifthenelse(Proj2(de2),
              /* FT */ Pair(Left(Right(Proj1(de2))), etrue),
              /* FF */ Pair(Wrong, efalse)
            )
          )
        )
      )
    }

    case Proj1(e) => {
      // delta( (X,Y) ) = ((delta(X) + delta(Y)) + (delta(X), delta(Y)), Bool)
      //
      // The derivative of a projection generates nil-change
      // on the fly. It points out that we are using Boolean tags
      // to avoid storage and computation of derivatives: Instead
      // of computing them all on creation of a change to a pair,
      // we put it off to when it is used. This creates a problem
      // currently: How to get the ASTs of components of a pair
      // given the AST of a pair?

      val de = freshName
      let(de, derive(e),
        ifthenelse( Proj2(de),
          sys error "non-nil: extract appropriate change from first component of de",
          sys error "nil: where should I get my derivative?!"
        )
      )
    }

    case Proj2(e) => map1(derive(e), de => Proj2(de))
    // TODO 26.03.13
    // - Verify type-correctness of deriving injections

    case Left(e) => map1(derive(e), de => Left(de))
    case Right(e) => map1(derive(e), de => Right(de))
    case Case(e,x,l,r) =>
      Pair(
        Case(e, x,
          Case(Proj1(derive(e)), xtodx(x),
            App(App(Proj1(derive(l)), x), xtodx(x)),
            Wrong),
          Case(Proj1(derive(e)), xtodx(x),
            Wrong,
            App(App(Proj1(derive(r)), x), xtodx(x)))),
        eIsOpen(Case(e, x, l, r)))

      // Caution: the rule below produces open terms from closed terms.
      //
      // map3(derive(e), derive(l), derive(r), { case (de,dl,dr) => Case(de,x,dl,dr) })
      //
      // Example:
      //   e         = Case(Left(0), x, x, x)
      //   derive(e) = Case(Left((0, efalse)), x, dx, dx)
      //
      // Example motivates additionally the examination of the derivation of injections.
  }

  // VALUES

  sealed abstract class Value
  case class NumV(n: Int) extends Value
  case class ClosureV(f: Lam, e: Map[Symbol, Value]) extends Value
  case class PairV(x: Value, y: Value) extends Value
  case class LeftV(x: Value) extends Value
  case class RightV(x: Value) extends Value

  // EVALUATION

  /*
   * ignore eval for now
   *
  def eval(e: Exp, env: Map[Symbol, Value]) : Value = e match {
    case Var(x) => env(x)

    case App(e1,e2) =>
      eval(e1, env) match {
        case ClosureV(f,env2) =>
          eval(f.e, Map(f.x -> eval(e2, env)))
	case other =>
          sys.error("Closure expected, found: " + other.toString)
					}
    case l@Lam(x, e) => ClosureV(l, env)

    case Num(n) => NumV(n)

    case Add(x, y) => (eval(x, env), eval(y, env)) match {
      case (NumV(a), NumV(b)) => NumV(a + b)
      case other => sys.error("Numbers expected, found: " + other.toString)
    }

    case Pair(e1, e2) => PairV(eval(e1, env), eval(e2, env))

    case Proj1(e) => eval(e, env) match {
      case PairV(e1, _) => e1;
      case other => sys.error("Pair expected, found: " + other.toString)
    }

    case Proj2(e) => eval(e,env) match {
      case PairV(_, e2) => e2;
      case other => sys.error("Pair expected, found: " + other.toString)
    }

    case Deriv(e) => eval(derive(e), env)

    case Left(e) => LeftV(eval(e, env))

    case Right(e) => RightV(eval(e, env))

    case Case(e, x, l, r) => eval(e,env) match {
      case LeftV(v) => eval(l, env + (x -> v))
      case RightV(v) => eval(r, env + (x -> v))
      case other => sys.error("Sum expected, found: " + other.toString)
    }
  }
  */

  // EXAMPLES

  val f = Lam('x, Pair(Add(Proj2('x), 1), Add(Proj1('x), -1)))
}
