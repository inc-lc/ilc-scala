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
    case ILit(i) => s"($i: $int)"
    case IPlus   => s"((i: $int) => (j: $int) => i + j)"
    case IMinus  => s"((i: $int) => (j: $int) => i - j)"
    case _       => super.toScala(t)
  }
}
