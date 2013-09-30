package ilc
package feature
package base

trait ToScala extends Syntax with functions.Types {
  // term constructors & polymorphic constants (TypedConst)
  def toScala(t: Term): String = t match {
    case variable: Variable =>
      variable.getName.toString

    case _ =>
      sys error s"Unknown term $t"
  }

  // types
  def toScala(tau: Type): String = tau match {
    case sigma0 =>: sigma1 => {
      s"((${toScala(sigma0)}) => ${toScala(sigma1)})"
    }

    case _ =>
      sys error s"Unknown type $tau"
  }
}
