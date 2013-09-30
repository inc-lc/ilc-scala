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
    case Literal(i) => s"($i: $int)"
    case Plus       => s"((i: $int) => (j: $int) => i + j)"
    case Minus      => s"((i: $int) => (j: $int) => i - j)"
    case _          => super.toScala(t)
  }
}
