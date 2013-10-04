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
}
