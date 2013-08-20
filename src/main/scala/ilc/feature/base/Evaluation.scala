package ilc
package feature.base

import scala.collection.immutable

trait Evaluation extends Syntax {
  evalTrait =>
  ////////////////////////////////
  // Subclass obligations start //
  ////////////////////////////////

  def evalConst(c: Constant): Value = { die(c, "evalConst") }

  //Core of the evaluation function, to be extended by subclasses.
  //Recursive calls should be done only through wrapEval.
  def coreEval(t: Term, env: Env): Value =
    t match {
      case Const(c) =>
        evalConst(c)
    }

  //////////////////////////////
  // Subclass obligations end //
  //////////////////////////////

  //Record the evaluated subterm
  def wrapEval(t: Term, env: Env): Value =
    try {
      coreEval(t, env)
    } catch { case ute @ UnexpectedTypeException(InnerTypeExceptionInfo(msg), _) =>
      throw new UnexpectedTypeException(OuterTypeExceptionInfo(msg, t), ute)
    }

  // Data is not put directly in the exception, but in nested case classes,
  // because exceptions seem to implement toString in a fixed way (which makes
  // sense) - or they just don't use toString but getMessage in the output, more
  // likely.
  case class UnexpectedTypeException(data: TypeExceptionInfo, cause: Throwable = null)
    extends Exception(data.toString, cause)

  trait TypeExceptionInfo
  case class InnerTypeExceptionInfo(message: String) extends TypeExceptionInfo
  case class OuterTypeExceptionInfo(message: String, subterm: Term, term: Option[Term] = None) extends TypeExceptionInfo {
    override def toString = {
      val optTermStr = term map ("\n>> Term: " + _.toString) getOrElse ""
      s"${message}\n>> Subterm: ${subterm}${optTermStr}"
    }
  }

  trait Value {
    // "toFunction"
    def apply(argument: Value): Value = die("apply", argument)
    def die(from: String, arg: Any = ""): Nothing =
      evalTrait.die(this, from, arg)
  }

  type Env = immutable.Map[String, Value]

  def eval(t: Term): Value = try {
    wrapEval(t, immutable.Map.empty)
  } catch { case UnexpectedTypeException(info: OuterTypeExceptionInfo, cause) =>
    throw UnexpectedTypeException(info.copy(term = Some(t)), cause)
  }

  def die(value: Any, from: String, arg: Any = ""): Nothing =
    throw UnexpectedTypeException(
      InnerTypeExceptionInfo(value.toString ++
        "." ++ from ++
        (if (arg.toString == "")
          ""
        else
          "(" ++ arg.toString ++ ")")))
}
