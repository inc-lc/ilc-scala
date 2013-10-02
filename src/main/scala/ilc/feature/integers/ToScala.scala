package ilc
package feature
package integers

trait ToScala extends base.ToScala with Syntax {
  private def int = "Int"

  override def toScala(tau: Type): String = tau match {
    case IntType => int
    case _       => super.toScala(tau)
  }

  override def toScala(t: Term): String = t match {
    case LiteralInt(i) => s"($i: $int)"
    case PlusInt   => s"((i: $int) => (j: $int) => i + j)"
    case NegateInt => s"((i: $int) => - i)"
    case _       => super.toScala(t)
  }
}
