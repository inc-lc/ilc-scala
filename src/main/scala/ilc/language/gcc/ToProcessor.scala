package ilc
package language
package gcc

import feature._
import let.FreshGen
import collection._

trait BasicDefinitions {
  outer: Syntax =>

  trait Instr
  type Block = List[Instr]

  //case class Frame(v: Var)
  case class Frame(vars: List[Var])
  val emptyFrame = Frame(Nil)
}

trait TopLevel {
  outer: Syntax with BasicDefinitions =>

  def checkTopConsistent(): Unit =
    assert(topBlocks.length == topNames.length)

  val topNames: mutable.ArrayBuffer[Var] = mutable.ArrayBuffer.empty
  def addTopLevelName(v: Var): Unit = {
    checkTopConsistent()
    topNames += v
  }

  val topBlocks: mutable.ArrayBuffer[Block] = mutable.ArrayBuffer.empty
  def addTopLevelBlock(b: Block): Unit = {
    topBlocks += b
    checkTopConsistent()
  }

  def reset() {
    topBlocks.clear()
    topNames.clear()
  }
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
    //"Allow recursion by adding the name before compiling the body." XXX
    // Doesn't make sense, because the name is not part of the program.
    // That would make sense for letrec* though.
    addTopLevelName(bodyVar)
    //Add compiledBody with a fresh name to the environment.
    addTopLevelBlock(toProc(body, frames) ++ List(RTN))
    //Create the closure here.
    List(LDF(bodyVar))
  }

  def toProc(t: Term, frames: List[Frame], suggestedFunName: Option[Var] = None): Block = t match {
    //Integers
    case LiteralInt(n) =>
      List(LDC(n))
    case App(App(PlusInt, a), b) =>
      toProc(b, frames) ++
      toProc(a, frames) ++
      List(ADD)

    //XXX: - Add more cases
    //XXX: - Add Let, and especially LetRecs. We need to support a LetRec* at the top-level, but probably we can just as well support it everywhere
    //by collecting the definitions, so that we don't need to use mutation for the top-level. (They don't really do that either).
    //XXX: - test this!!!
    case App(f, arg) =>
      val arity = 1 //XXX generalize!
      toProc(arg, frames) ++ toProc(f, frames) ++ List(AP(arity))

    //Maybe the current "top-level" handling should instead be used just for letrec*?
    case Abs(variable, body) =>
      //We have to lift the body to the top.
      //But we don't do lambda-lifting because we still expect to find the
      val v: Var = suggestedFunName getOrElse freshener.fresh("fun", variable.getType =>: body.getType)
      toClosure(v, body, Frame(List(variable)) :: frames)
    case LetRecStar(bindings, bodyName, body) =>
      //XXX handle better the last case.
      val frame = Frame(bindings map (_._1))
      val newFrames = frame :: frames
      val labels = bindings flatMap {
        case (v, exp) =>
          toProc(exp, newFrames, Some(v))
      }
      val frameSize = frame.vars.length
      val bodyVar = Var(bodyName, UnitType)
      List(DUM(frameSize)) ++ labels ++ toClosure(bodyVar, body, newFrames) ++ List(RAP(frameSize))
      //XXX turn body into special-cased last abstraction. And add separate name for it.
    case v @ Var(name, _) =>
      List(LD(toIdx(v, frames)))

    case _ =>
      ???
  }
  def toProcBase(t: Term) = toProc(t, Nil) ++ List(RTN)
  def toProg(t: Term) = {
    reset()
    val main = toProcBase(t)
    val blocks = main :: topBlocks.toList
    val labelSizes = Map(topNames.toList zip (blocks.map (_.length) .scanLeft(0)(_ + _)).tail: _*)
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
