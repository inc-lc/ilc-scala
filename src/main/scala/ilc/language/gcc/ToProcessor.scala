package ilc
package language
package gcc

import scala.language.postfixOps
import feature._
import base.FreshGen
import collection._

trait BasicDefinitions {
  outer: Syntax =>

  trait Instr extends Product {
    def showArgs(forHaskell: Boolean) = productIterator.toList mkString " "
    def show(forHaskell: Boolean = false) = {
      def instrName = this.getClass().getSimpleName().stripSuffix("$")
      val args = showArgs(forHaskell)
      val sep = if (args.nonEmpty) " " else ""
      s"$instrName$sep$args"
    }

    //Is this a primitive which consumes its arguments from the stack?
    def isPrimFun: Boolean = false
  }

  trait PrimInstr extends Instr {
    override def isPrimFun = true
  }
  type Block = List[Instr]

  case class Frame(vars: List[Var])
  val emptyFrame = Frame(Nil)

  def validateTopVar(v: Var): Unit
}

trait TopLevel {
  outer: Syntax with BasicDefinitions =>

  val topLevel: mutable.ArrayBuffer[(Var, Block)] = mutable.ArrayBuffer.empty
  def addTopLevelBinding(v: Var, b: Block): Unit = {
    topLevel += ((v, b))
  }

  def reset(): Unit = {
    topLevel.clear()
  }

  def topNames = topLevel map (_._1) toList
  def topBlocks = topLevel map (_._2) toList
  def validateTopVar(v: Var): Unit = assert(topNames contains v)
}

trait Instructions {
  outer: Syntax with BasicDefinitions =>

  def showComment(forHaskell: Boolean, _comment: Option[String]): String =
    _comment map { comment =>
      if (forHaskell)
        ""
      else
        "\t\t; " + comment
    } getOrElse ""

  case class AP(n: Int) extends Instr
  case class DeBrujinIdx
    ( n: Int //Number of binders (frames) to go down
    , i: Int //Number of variable in the binder
    , v: Var //Readable name
    )

  case class LD(idx: DeBrujinIdx) extends Instr {
    override def showArgs(forHaskell: Boolean) = s"${idx.n} ${idx.i}${showComment(forHaskell, Some(s"var ${idx.v}"))}"
  }
  case class DUM(n: Int) extends Instr
  case class LDC(n: Int) extends Instr
  case class RAP(n: Int) extends Instr
  case object RTN extends Instr

  /**
   * A list of symbolic labels as part of an instruction, which can optionally have been resolved to numeric labels.
   * We keep the original labels for comments.
   */
  case class ResolvableLabels(targetVars: List[Var], targetPositions: Option[List[Int]]) {
    //XXX distinguish top labels (code pointers) from the top stack frame (which is a normal one).
    targetVars foreach validateTopVar

    def comment =
      targetPositions map { _ =>
        (for {
          v <- targetVars
        } yield s"var $v") mkString ", "
      }

    def show(forHaskell: Boolean): String =
      targetPositions match {
        case Some(positions) =>
         (positions mkString " ") + showComment(forHaskell, comment)
        case _ =>
          assert(!forHaskell)
          targetVars map (_.getName.toString) mkString " "
      }
  }
  case class LDF(label: ResolvableLabels) extends Instr {
    override def showArgs(forHaskell: Boolean) = label show forHaskell
  }
  object LDF {
    def apply(v: Var): LDF = LDF(ResolvableLabels(List(v), None))
  }

  //Integer instructions
  case object ADD extends PrimInstr
  case object SUB extends PrimInstr
  case object MUL extends PrimInstr
  case object DIV extends PrimInstr

  //Boolean instructions
  case object CEQ extends PrimInstr
  case object CGT extends PrimInstr
  case object CGTE extends PrimInstr

  case object JOIN extends Instr
  case class SEL(targets: ResolvableLabels) extends Instr {
    override def showArgs(forHaskell: Boolean) = targets show forHaskell
  }
  object SEL {
    def apply(thn: Var, els: Var): SEL = SEL(ResolvableLabels(List(thn, els), None))
  }

  // Pairs
  case object CONS extends PrimInstr
  case object CAR extends PrimInstr
  case object CDR extends PrimInstr
  case object ATOM extends PrimInstr
}

trait ToProcessor extends BasicDefinitions with TopLevel with Instructions {
  outer: Syntax =>
  val freshener = new FreshGen {
    val syntax: outer.type = outer
  }

  def varInFrame(v: Var)(l: Frame): Option[Int] = {
    //Variables must only be looked up by name, not by type.
    //That's only ever important for the initVars
    val idx = l.vars map (_.getName) indexOf v.getName
    if (idx == -1)
      None
    else
      Some(idx)
    //if (v == l.v) Some(0) else None
  }

  def toIdx(v: Var, frames: List[Frame]): DeBrujinIdx = {
    val results = frames.map(varInFrame(v)).zipWithIndex.collect ({
      case (Some(idx), idxFrame) =>
        DeBrujinIdx(idxFrame, idx, v)
    })
//    if (results.length != 1)
//      println(s"$results - $v - $frames")
//    assert (results.length == 1) //We don't expect shadowing, do we?
    results.head //Finds the left-most (that is, innermost) binding (in case we do support it)
  }

  def toClosure(bodyVar: Var, isFresh: Boolean, body: Term, frames: List[Frame]) = {
    //Freshening again would create confusion for human readers between fun_N1 and fun_N2 referring to the same function
    val fresherVar = if (isFresh) bodyVar else freshener.fresh(bodyVar)
    //Add compiled `body` with given name to the list of top-level procedures.
    addTopLevelBinding(fresherVar, toProc(body, frames) ++ List(RTN))
    //Create the closure here.
    List(LDF(fresherVar))
  }

  def toProc(t: Term, frames: List[Frame], suggestedFunName: Option[Var] = None): Block = t match {
    //Primitives.
    //Integers
    case LiteralInt(n) => List(LDC(n))

    //Integer Ops
    case Plus  => List(ADD)
    case Minus => List(SUB)
    case Mult  => List(MUL)
    case Div   => List(DIV)
    case Eq    => List(CEQ)
    case Gt    => List(CGT)
    case Gte   => List(CGTE)

    //Booleans
    case True  => toProc(LiteralInt(1), frames, suggestedFunName)
    case False => toProc(LiteralInt(0), frames, suggestedFunName)

    case App(Not, b) =>
      toProc(App(App(Minus, 1), b), frames)

    case App(App(App(IfThenElse(t), cond), Abs(_, trueBlock)), Abs(_, falseBlock)) => {

      val trueLabel: Var = freshener.fresh("if_t", UnitType =>: t)
      val falseLabel: Var = freshener.fresh("if_f", UnitType =>: t)

      addTopLevelBinding(trueLabel, toProc(trueBlock, frames) ++ List(JOIN))
      addTopLevelBinding(falseLabel, toProc(falseBlock, frames) ++ List(JOIN))

      toProc(cond, frames, suggestedFunName) ++ List(SEL(trueLabel, falseLabel))
    }

    //Pairs
    case Pair(car, cdr) => List(CONS)
      // val left = toProc(car, frames, suggestedFunName)
      // val right = toProc(cdr, frames, suggestedFunName)

    case Proj1(a, b) => List(CAR)
    case Proj2(a, b) => List(CDR)

    case Empty(elemT) => toProc(LiteralInt(0), frames, suggestedFunName)
    case Cons(elemT) => List(CONS)
    case Head(elemT) => List(CAR)
    case Tail(elemT) => List(CDR)
    case IsEmpty(elemT) => List(ATOM)

    //Core: lambda-calculus with letrec*.
    /* TODOs:
     * - Add more primitives
     * - Test non-top-level LetRecStar
     */
    case App(operator, operand) =>
      //Adapted from A-normalization.
      def collectApps(t: Term, acc: List[Term]): List[Term] = t match {
        case App(s, t) => collectApps(s, t :: acc)
        case _ =>
          //The function must be last!
          acc ++ (t :: Nil)
      }
      //Special handling for nested applications
      //Note that this is very syntactic, and that's bad: if the user already inserted a binding for a partial application, as in:
      // val r1 = f arg1 arg2
      // val r2 = r1 arg3
      //we don't want to inline r1 into r2, because that might lead to work duplication.
      //XXX: This works if programs are in eta-long normal form.
      val subNodes =
        collectApps(operator, operand :: Nil)
      val fun = subNodes.last
      val operands = subNodes.init
      def descend(v: Term) = toProc(v, frames)

      val arity = operands.length
      val operandsCode = operands flatMap descend
      val funCode = descend(fun)
      val isPrim = funCode.length == 1 && funCode.head.isPrimFun
      val extraApply =
        if (isPrim)
          Nil
        else
          List(AP(arity))
      operandsCode ++ funCode ++ extraApply
      //toProc(operand, frames) ++ toProc(operator, frames)

    //Maybe the current "top-level" handling should instead be used just for letrec*?
    case abs@Abs(_, _) =>
      def collectAbs(acc: List[Var], t: Term): (List[Var], Term) = t match {
        case Abs(v, body) =>
          collectAbs(v :: acc, body)
        case _ =>
          (acc.reverse, t)
      }
      val (variables, body) = collectAbs(Nil, abs)
      val funType = variables.map(_.getType).foldRight(body.getType)(_ =>: _)

      //We have to lift the body to the top.
      //But we don't do lambda-lifting because we still expect to find the variables in the containing frame.
      val (v, isFresh) = suggestedFunName map ((_, false)) getOrElse ((freshener.fresh("fun", funType), true))
      toClosure(v, isFresh, body, Frame(variables) :: frames)

    case LetRec(bindings, bodyName, body) =>
      val frame = Frame(bindings map (_._1))
      //Allow recursion by binding the names before compiling them.
      val newFrames = frame :: frames
      val labels = bindings flatMap {
        case (v, exp) =>
          toProc(exp, newFrames, Some(v))
      }
      val frameSize = frame.vars.length
      val bodyVar = Var(bodyName, UnitType)
      List(DUM(frameSize)) ++ labels ++ toClosure(bodyVar, false, body, newFrames) ++ List(RAP(frameSize))

    case v @ Var(name, _) =>
      List(LD(toIdx(v, frames)))

    case _ =>
      ???
  }

  val forGame = true
  override def initVars =
    if (forGame)
      List(Var("initWorld", freshTypeVariable('initWorld)), Var("ghosts", freshTypeVariable('ghosts)))
    else
      Nil
  def baseFrames: List[Frame] =
    if (forGame)
      List(Frame(initVars))
    else
      Nil

  def toProcBase(t: Term) = toProc(t, baseFrames) ++ List(RTN)
}

trait ToProcessorFrontend extends ToProcessor {
  outer: Syntax =>

  def showTraversable[T, U](trav: Traversable[(T, U)]) =
    "\n" + ((for {
      (el, n) <- trav
    } yield s"$n: $el") mkString "\n")
  //def show(b: Block) = showTraversable(b.zipWithIndex)

  case class CompiledProgram(code: Block, labelPositions: Map[Var, Int]) {
    def showNumbered = showTraversable(code map (_ show()) zipWithIndex)
    def showLabels = showTraversable(labelPositions)

    def showResolved(forHaskell: Boolean = false) =
      resolveSymbolic(code, labelPositions) map (_ show forHaskell)
    /**
     * Convert to a form which can be Read in Haskell with a "natural" definition of instructions
     * (the one used elsewhere in this repo).
     */
    def toHaskell =
      showResolved(true) mkString ("[", ",\n", "]")
    /**
     * Convert to form acceptable for input in website.
     */
    def toRaw =
      showResolved(false) mkString "\n"
  }

  def toProg(t: Term): CompiledProgram = {
    reset()
    val main = toProcBase(t)
    val blocks = main :: topBlocks
    val blockSizes = (blocks.map (_.length) .scanLeft(0)(_ + _)).tail
    val labelSizes = Map(topNames zip blockSizes: _*)
    CompiledProgram(blocks.flatten, labelSizes)
  }

  def showProg(t: Term) = {
    val cp@CompiledProgram(prog, labels) = toProg(t)
    s"${cp.showNumbered}\n${cp.showLabels}\n${cp.toHaskell}"
  }

  def resolveLabelSymbolic(l: ResolvableLabels, labelSizes: Map[Var, Int]): ResolvableLabels =
    l match {
      case ResolvableLabels(vars, None) =>
        ResolvableLabels(vars, Some(vars map labelSizes))
  }

  def resolveSymbolic(instrs: Block, labelSizes: Map[Var, Int]) = {
    instrs map {
      case LDF(rl) => LDF(resolveLabelSymbolic(rl, labelSizes))
      case SEL(rl) => SEL(resolveLabelSymbolic(rl, labelSizes))
      case op => op
    }
  }
}
