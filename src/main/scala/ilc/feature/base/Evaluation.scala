package ilc
package feature
package base

import scala.collection.immutable

trait Evaluation extends Syntax with functions.Pretty {
  type Env = immutable.Map[Name, Value]

  //Core of the evaluation function, to be extended by subclasses.
  //Recursive calls should be done only through wrapEval.
  def coreEval(t: Term, env: Env): Value =
    t match {
      case variable: Var => env(variable.getName)
      case _ => sys error s"cannot evaluate $t"
    }

  /** Evaluate a Term, returning a Value or throwing an exception (for instance,
    * an UnexpectedTypeException).
    */
  def eval(t: Term): Value =
    try {
      wrapEval(t, immutable.Map.empty)
    } catch { case UnexpectedTypeException(info: OuterTypeExceptionInfo, cause) =>
        throw UnexpectedTypeException(info.copy(term = Some(t)), cause)
    }

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
      val subtermStr = pretty(subterm)
      val optTermStr = term map { t =>
        "\n>> Term: " + pretty(t).toString
      } getOrElse ""
      s"${message}\n>> Subterm: ${subtermStr}${optTermStr}"
    }
  }

  trait Value {
    def die(from: String, arg: Any = ""): Nothing =
    throw UnexpectedTypeException(
      InnerTypeExceptionInfo(this.toString ++
        "." ++ from ++
        (if (arg.toString == "")
          ""
        else
          "(" ++ arg.toString ++ ")")))
  }
}
