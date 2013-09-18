package ilc
package feature
package base

trait ToScala extends TypedSyntax {
  // term constructors & polymorphic constants (TypedConst)
  def toScala(t: Term): String = t match {
    case Const(c) =>
      toScala(c)

    case _ =>
      sys error s"Unknown term $t"
  }

  // nonpolymorphic constants
  def toScala(c: Constant): String =
    sys error s"Unknown constant $c"


  // types
  def toScala(tau: Type): String = tau match {
    case sigma0 =>: sigma1 =>
      "(%s => %s)".format(toScala(sigma0), toScala(sigma1))

    case _ =>
      sys error s"Unknown type $tau"
  }
}
