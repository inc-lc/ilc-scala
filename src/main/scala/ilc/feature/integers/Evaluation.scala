package ilc
package feature
package integers

trait Evaluation extends functions.Evaluation with Syntax
{
  case class IntValue(i: Int) extends Value {
    override def toString = i.toString
  }

  implicit class intValueOps(value: Value) {
    def toInt: Int = value match {
      case IntValue(i) =>
        i

      case _ =>
        typeErrorNotTheSame("Value.toInt", "IntValue", value)
    }
  }

  override def coreEval(t: Term, env: Env): Value = t match {
    case LiteralInt(i) =>
      IntValue(i)

    case PlusInt =>
      (i: Value) => (j: Value) => IntValue(i.toInt + j.toInt)

    case MinusInt =>
      (i: Value) => (j: Value) => IntValue(i.toInt - j.toInt)

    case _ =>
      super.coreEval(t, env)
  }
}
