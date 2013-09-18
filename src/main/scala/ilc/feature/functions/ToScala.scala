package ilc
package feature
package functions

import scala.language.postfixOps

trait ToScala extends base.ToScala with Syntax {
  override def toScala(t: Term): String = t match {
    case App(f, x) =>
      "%s(%s)".format(toScala(f), toScala(x))

    case Abs(variable, bodyTerm) => {
      val x = variable.getName.toString
      val xType = toScala(variable.getType)
      val body = toScala(bodyTerm)
      s"(($x: $xType) => $body)"
    }

    case _ =>
      super.toScala(t)
  }
}
