package ilc
package language
package gcc

import scala.language.postfixOps
import feature._
import let.FreshGen
import collection._

trait BasicDefinitions {
  outer: Syntax =>

  trait Instr
  type Block = List[Instr]

  case class Frame(vars: List[Var])
  val emptyFrame = Frame(Nil)
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
}

trait Instructions {
  outer: Syntax with BasicDefinitions =>

  case class AP(n: Int) extends Instr
  case class DeBrujinIdx
    ( n: Int //Number of binders (frames) to go down
    , i: Int //Number of variable in the binder
    , v: Var //Readable name
    )

  case class LD(idx: DeBrujinIdx) extends Instr
  case class DUM(n: Int) extends Instr
  case class LDC(n: Int) extends Instr
  case class RAP(n: Int) extends Instr
  case object ADD extends Instr
  case object RTN extends Instr
}

trait ToProcessor extends BasicDefinitions with TopLevel with Instructions {
  outer: Syntax =>
  val freshener = new FreshGen {
    val syntax: outer.type = outer
  }
  //XXX distinguish top labels (code pointers) from the top stack frame (which is a normal one).
  case class LDF(n: Var) extends Instr {
    assert(topNames contains n)
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
    case App(App(PlusInt, a), b) =>
      toProc(b, frames) ++
      toProc(a, frames) ++
      List(ADD)

    //Core: lambda-calculus with letrec*.
    /* TODOs:
     * - Add more primitives
     * - Test non-top-level LetRecStar
     */
    case App(f, arg) =>
      val arity = 1 //XXX generalize!
      toProc(arg, frames) ++ toProc(f, frames) ++ List(AP(arity))

    //Maybe the current "top-level" handling should instead be used just for letrec*?
    case Abs(variable, body) =>
      //We have to lift the body to the top.
      //But we don't do lambda-lifting because we still expect to find the variables in the containing frame.
      val v: Var = suggestedFunName getOrElse freshener.fresh("fun", variable.getType =>: body.getType)
      toClosure(v, body, Frame(List(variable)) :: frames)
    case LetRecStar(bindings, bodyName, body) =>
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
  def toProg(t: Term) = {
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
  def show(b: Block) = showTraversable(b.zipWithIndex)
  def showProg(t: Term) = {
    val (prog, labels) = toProg(t)
    s"${show(prog)}\n${showTraversable(labels)}"
  }
}
