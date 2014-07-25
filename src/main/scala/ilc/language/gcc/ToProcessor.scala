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

  // The top-level is special!
  // It needs the

  //case class TopLevel(names: List[Var], blocks: List[Block])
  //def emptyTopLevel = TopLevel(Nil, Nil)
  //case class Program(t: TopLevel)

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

  def topFrame(): Frame = Frame(topNames.toList)
}

trait Instructions {
  outer: Syntax with BasicDefinitions =>

  case class AP(n: Int) extends Instr
  case class DeBrujinIdx
    ( n: Int //Number of binders to go down
    , i: Int //Number of variable in the binder
    , v: Var //Readable name
    )

  case class LD(idx: DeBrujinIdx) extends Instr
}

trait ToProcessor extends BasicDefinitions with TopLevel with Instructions {
  outer: Syntax =>
  val freshener = new FreshGen {
    val syntax: outer.type = outer
  }
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
    val adjFrames = frames ++ List(topFrame())
    val results = adjFrames.map(varInFrame(v)).zipWithIndex.collect {
      case (Some(idx), idx2) =>
        DeBrujinIdx(idx, idx2, v)
    }
    assert (results.length == 1) //We don't expect shadowing, do we?
    results.head //Finds the left-most (that is, innermost) binding (in case we do support it)
  }

  def toProc(t: Term, frames: List[Frame]): Block = t match {
    //XXX: - Add more cases
    //XXX: - Add Let, and especially LetRecs. We need to support a LetRec* at the top-level, but probably we can just as well support it everywhere
    //by collecting the definitions, so that we don't need to use mutation for the top-level. (They don't really do that either).
    //XXX: - test this!!!
    case App(f, arg) =>
      val arity = 1 //XXX generalize!
      toProc(f, frames) ++ toProc(arg, frames) ++ List(AP(arity))

    //Maybe the current "top-level" handling should instead be used just for letrec*?
    case Abs(variable, body) =>
      //We have to lift the body to the top.
      //But we don't do lambda-lifting because we still expect to find the
      val v: Var = freshener.fresh("fun", variable.getType =>: body.getType)
      //"Allow recursion by adding the name before compiling the body." XXX
      // Doesn't make sense, because the name is not part of the program.
      // That would make sense for letrec* though.
      addTopLevelName(v)
      val compiledBody = toProc(body, Frame(List(variable)) :: frames)
      //Add compiledBody with a fresh name to the environment.
      addTopLevelBlock(compiledBody)
      //XXX That's for accessing globals, not for creating closures.
      //val idx = toIdx(v, frames)
      //List(LD(idx))
      //Create the closure here.
      List(LDF(v))
    case v @ Var(name, _) =>
      List(LD(toIdx(v, frames)))
    case _ =>
      ???
  }
  def toProcBase(t: Term) = toProc(t, Nil)
}