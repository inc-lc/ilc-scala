package ilc
package language
package gcc

import scala.language.postfixOps
import feature._
import let.FreshGen
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

  case class AP(n: Int) extends Instr
  case class DeBrujinIdx
    ( n: Int //Number of binders (frames) to go down
    , i: Int //Number of variable in the binder
    , v: Var //Readable name
    )

  case class LD(idx: DeBrujinIdx) extends Instr {
    override def showArgs(forHaskell: Boolean) = s"${idx.n} ${idx.i}${if (forHaskell) "" else s"\t\t; var ${idx.v}"}"
  }
  case class DUM(n: Int) extends Instr
  case class LDC(n: Int) extends Instr
  case class RAP(n: Int) extends Instr
  case object RTN extends Instr

  case class LDF(target: Either[Var, Int]) extends Instr {
    //XXX distinguish top labels (code pointers) from the top stack frame (which is a normal one).
    target match {
      case Left(v) => validateTopVar(v)
      case _ =>
    }

    override def showArgs(forHaskell: Boolean) =
      target match {
      case Left(v) =>
        assert(!forHaskell)
        v.getName.toString
      case Right(i) =>
        assert(forHaskell)
        i.toString
    }
  }

  object LDF {
    def apply(v: Var): LDF = LDF(Left(v))
    def apply(i: Int): LDF = LDF(Right(i))
  }

  //Integer instructions
  case object ADD extends PrimInstr
  case object SUB extends PrimInstr
  case object MUL extends PrimInstr
  case object DIV extends PrimInstr

}

trait ToProcessor extends BasicDefinitions with TopLevel with Instructions {
  outer: Syntax =>
  val freshener = new FreshGen {
    val syntax: outer.type = outer
  }

  def varInFrame(v: Var)(l: Frame): Option[Int] = {
    val idx = l.vars indexOf v
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
    assert (results.length == 1) //We don't expect shadowing, do we?
    results.head //Finds the left-most (that is, innermost) binding (in case we do support it)
  }

  def toClosure(bodyVar: Var, body: Term, frames: List[Frame]) = {
    //Add compiled `body` with given name to the list of top-level procedures.
    addTopLevelBinding(bodyVar, toProc(body, frames) ++ List(RTN))
    //Create the closure here.
    List(LDF(bodyVar))
  }

  def toProc(t: Term, frames: List[Frame], suggestedFunName: Option[Var] = None): Block = t match {
    //Primitives.
    //Integers
    case LiteralInt(n) =>
      List(LDC(n))
    case Plus =>
      List(ADD)
    case Minus =>
      List(SUB)
    case Mult =>
      List(MUL)
    case Div =>
      List(DIV)

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
    case Abs(variable, body) =>
      //We have to lift the body to the top.
      //But we don't do lambda-lifting because we still expect to find the variables in the containing frame.
      val v: Var = suggestedFunName getOrElse freshener.fresh("fun", variable.getType =>: body.getType)
      toClosure(v, body, Frame(List(variable)) :: frames)
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
      List(DUM(frameSize)) ++ labels ++ toClosure(bodyVar, body, newFrames) ++ List(RAP(frameSize))

    case v @ Var(name, _) =>
      List(LD(toIdx(v, frames)))

    case _ =>
      ???
  }

  def toProcBase(t: Term) = toProc(t, Nil) ++ List(RTN)
  def toProg(t: Term): (Block, Map[Var, Int]) = {
    reset()
    val main = toProcBase(t)
    val blocks = main :: topBlocks
    val blockSizes = (blocks.map (_.length) .scanLeft(0)(_ + _)).tail
    val labelSizes = Map(topNames zip blockSizes: _*)
    (blocks.flatten, labelSizes)
  }

  def showTraversable[T, U](trav: Traversable[(T, U)]) =
    "\n" + ((for {
      (el, n) <- trav
    } yield s"$n: $el") mkString "\n")
  //def show(b: Block) = showTraversable(b.zipWithIndex)

  def showProg(t: Term) = {
    val (prog, labels) = toProg(t)
    s"${showTraversable(prog map (_ show()) zipWithIndex)}\n${showTraversable(labels)}\n${toHaskell(prog, labels)}"
  }

  def resolveSymbolic(instrs: Block, labelSizes: Map[Var, Int]) = {
    instrs map {
      case LDF(Left(v)) => LDF(labelSizes(v))
      case op => op
    }
  }

  /**
   * Convert to a form which can be Read in Haskell with a "natural" definition of instructions
   * (the one used elsewhere in this repo).
   */
  def toHaskell(block: Block, labelSizes: Map[Var, Int]) = {
    resolveSymbolic(block, labelSizes) map (_ show true) mkString ("[", ",\n", "]")
  }
}
