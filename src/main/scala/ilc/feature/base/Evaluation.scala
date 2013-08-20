package ilc
package feature.base

import scala.collection.immutable

trait Evaluation extends Syntax {
  ////////////////////////////////
  // Subclass obligations start //
  ////////////////////////////////

  def evalConst(c: Constant): Value = { die("evalConst") }

  def evalWithEnv(t: Term, env: Env): Value =
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
  }

  type Env = immutable.Map[String, Value]

  class InvalidTargetObjectTypeException(message: String) extends Exception(message)

  def eval(t: Term): Value = try {
    evalWithEnv(t, immutable.Map.empty)
  } catch { case err: InvalidTargetObjectTypeException =>
    throw new
      IllegalArgumentException(err.getMessage() ++
        "\n in the term\n    " ++ t.toString)
  }


  def die(from: String, arg: Any = ""): Nothing =
    throw new
      InvalidTargetObjectTypeException(this.toString ++
        "." ++ from ++
        (if (arg.toString == "")
          ""
        else
          "(" ++ arg.toString ++ ")"))
}
