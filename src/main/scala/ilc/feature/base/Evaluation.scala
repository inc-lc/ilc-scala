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
  def coreEval(t: Term, env: Env): Value =
    t match {
      case Const(c) =>
        evalConst(c)
    }

  //////////////////////////////
  // Subclass obligations end //
  //////////////////////////////

  trait Value {
    // "toFunction"
    def apply(argument: Value): Value = die("apply", argument)
    def die(from: String, arg: Any = ""): Nothing =
        evalTrait.die(this, from, arg)
  }

  type Env = immutable.Map[String, Value]

  class InvalidTargetObjectTypeException(message: String) extends Exception(message)

  def eval(t: Term): Value = try {
    coreEval(t, immutable.Map.empty)
  } catch { case err: InvalidTargetObjectTypeException =>
    throw new
      IllegalArgumentException(err.getMessage() ++
        "\n in the term\n    " ++ t.toString)
  }


  def die(value: Any, from: String, arg: Any = ""): Nothing =
    throw new
      InnerUnexpectedTypeException(value.toString ++
        "." ++ from ++
        (if (arg.toString == "")
          ""
        else
          "(" ++ arg.toString ++ ")"))
}
