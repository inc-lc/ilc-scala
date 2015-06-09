package ilc
package feature
package integers

trait ToScala extends base.ToScala with Syntax {
  private def int = "Int"

  override def toScala(tau: Type): String = tau match {
    case IntType => int
    case _       => super.toScala(tau)
  }

  override def isScalaPrimitive(tau: Type) = tau match {
    case IntType => true
    case _ => super.isScalaPrimitive(tau)
  }

  override def toUntypedScala(t: Term): String = t match {
    case LiteralInt(i) => i.toString
    case PlusInt   => scalaFunction("i", "j")("i + j")
    case NegateInt => "(- _)"
    case _       => super.toUntypedScala(t)
  }
}
