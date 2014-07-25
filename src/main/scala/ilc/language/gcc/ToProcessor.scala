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
}

trait TopLevel {
  outer: Syntax with BasicDefinitions =>

  // The top-level is special!
  // It needs the

  case class TopLevel(names: List[Var], blocks: List[Block])
  def emptyTopLevel = TopLevel(Nil, Nil)
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
}

trait Instructions {
  outer: Syntax with BasicDefinitions =>

  case class LDF() extends Instr
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

  //case class Frame(v: Var)
  case class Frame(vars: List[Var])

  def varInFrame(v: Var)(l: Frame): Option[Int] = {
    val idx = l.vars indexOf v
    if (idx == -1)
      None
    else
      Some(idx)
    //if (v == l.v) Some(0) else None
  }

  def toIdx(v: Var, l: List[Frame]): DeBrujinIdx = {
    l.map(varInFrame(v)).zipWithIndex.collect {
      case (Some(idx), idx2) =>
        DeBrujinIdx(idx, idx2, v)
    }.head
  }

  def toProc(t: Term, vars: List[Frame]): List[Instr] = t match {
    case App(f, arg) =>
      toProc(f, vars) ++ toProc(arg, vars) ++ List(AP(1))
    case Abs(variable, body) =>
      val v: Var = freshener.fresh("fun", variable.getType =>: body.getType)
      //this.add

      //val newTop = TopLevel(???)
      //Allow recursion
      val compiledBody = toProc(body, Frame(List(variable)) :: vars)
      //Add compiledBody with a fresh name to the environment.
      val idx = ???
      List(LD(idx))
    case v @ Var(name, _) =>
      List(LD(toIdx(v, vars)))
    case _ =>
      ???
  }
  def toProcBase(t: Term) = toProc(t, Nil)
}