package ilc
package feature
package functions

import scala.language.postfixOps

trait ToScala extends base.ToScala with Syntax {
  override def toUntypedScala(t: Term): String = t match {
    case App(f, x) =>
      "%s(%s)".format(toScala(f), toScala(x))

    case Abs(variable, bodyTerm) =>
      scalaFunction(variable.getName.toString)(toScala(bodyTerm))

    case _ =>
      super.toUntypedScala(t)
  }

  override def toScala(tau: Type): String = tau match {
    case sigma0 =>: sigma1 => {
      s"((=>${toScala(sigma0)}) => ${toScala(sigma1)})"
    }

    case _ =>
      super.toScala(tau)
  }
}
